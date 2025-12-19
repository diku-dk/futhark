{-# LANGUAGE Strict #-}

-- | This module performs the stripmining part
--     of a high-level schedule
module Futhark.Optimise.Fusion.HLsched.AdjustResult
  ( manifestResult
  )
where

import Control.Monad
import Data.Sequence qualified as Sq
import Data.Map.Strict qualified as M
import Data.Maybe
--import Futhark.Analysis.HORep.SOAC qualified as H
import Futhark.Construct
import Futhark.IR.SOACS
import Futhark.Optimise.Fusion.HLsched.Env
import Futhark.Optimise.Fusion.HLsched.SchedUtils
--import Futhark.Util.Pretty hiding (line, sep, (</>))
import Futhark.Analysis.PrimExp.Convert

import Debug.Trace

pe0 :: PrimExp VName
pe0 = ValueExp $ IntValue $ Int64Value 0

-- | Manifests the original result by using a map-nest with accumulators.
--   Arguments:
--     1. the environment
--     2. the schedule
--     3. the pattern of the original soac
--   Result:
--     1. the pattern of the rescheduled soac
--     2. the scratch statements allocating the result
--     3. the with-acc statement that uses accumulators
--           to "copy" the result in the original shape
--   ToDo:
--     1. treat the case of an accumulated reduction:
--          - initialize with replicate of neutral elem instead of scratch
--          - accumulate with operator instead of simple assignment
--     2. The implementation assumes that all dimensions of the result
--          array are mapped; please verify that this is so!
manifestResult ::
    (LocalScope SOACS m, MonadFreshNames m) =>
    Env -> HLSched -> Pat Type ->
    m (PatElem Type, Stms SOACS, Stm SOACS)
manifestResult env sched pat_orig
  | [patel_orig] <- patElems pat_orig,
    (nm_orig, tp_orig) <- (patElemName patel_orig, patElemDec patel_orig),
    iota_inv <- M.fromList $ map (\(nm,(w,pe)) -> (pe, (nm,w))) $ M.toList $ iotas env,
    (iot_nms, iot_ws) <- unzip $ mapMaybe (`M.lookup` iota_inv) (dimlens sched),
    length iot_nms == length (dimlens sched) = do
  let tp_i64 = Prim $ IntType Int64
  ind_pars <- mapM (\ _ -> newParam "i" tp_i64) $ iot_nms
  -- 1. compute the multi-dim index used to update the original array:
  let (updt_inds0, fst_ind, _, _) =
        foldl ff ([], pe0, pe0, -1) $
          filter ((/= pe0) . fst . snd . snd) $
            zip (map paramName ind_pars) $
              zip (dimlens sched) $
                zip (strides sched) (origids sched)
      updt_inds = fst_ind : updt_inds0
  -- 2. make the init statement: currently only scratch is implemented!
  let iot_tps = map (\ w -> arrayOfShape tp_i64 (Shape [w])) iot_ws
      iot_pars= zipWith (Param mempty) iot_nms iot_tps 
      tup4s_orig = zip (strides sched) $ zip3 ind_pars iot_pars iot_ws -- dimlens sched
      tup4s_schd = map (\i -> tup4s_orig !! i) $ invPerm $ sigma sched
      (_, tup3s) = unzip $ filter (\(stride,_) -> not (stride == pe0)) tup4s_schd
  -- ^ tup3s contains triplets (i_param, iota_nm, size_pexp) in the permuted order
      elem_tp  = elemType $ patElemDec patel_orig
      orig_shp = arrayDims $ patElemDec patel_orig
      schd_shp = Shape $ map (\(_,_,w) -> w) tup3s
      schd_tp  = arrayOfShape (Prim elem_tp) schd_shp
  schd_par <- newParam ((baseString nm_orig) ++ "_sched") schd_tp
  let schd_patel = PatElem (paramName schd_par) (paramDec schd_par)
  scope <- askScope
  (nm_init, init_stms) <-
    runBuilder $ localScope scope $ do
      letExp (baseString (patElemName patel_orig)) $ BasicOp $ Scratch elem_tp orig_shp
  -- will need replicate in the case of an accumulated reduction!!!
  -- rep_stm <- letExp "lala" $ Replicate shp
  withacc_stm <- mkWithAccStm (nm_orig, nm_init, tp_orig) schd_par tup3s updt_inds
  trace ("\n\n\nUpdated-Indices:" ++ prettyString updt_inds ++ "\n!!!!!!!!!!\n\n\n") $
    pure (schd_patel, init_stms, withacc_stm)
  where
    leaf i = LeafExp i (IntType Int64)
    ff (ind_lst, ind_acc, s_acc, d_acc) (i, (n, (_s, d)))
      | d_acc == -1= ( [], leaf i, n, d )
      | d_acc /= d = (ind_acc : ind_lst, leaf i, n, d)
      | otherwise  = (ind_lst, addPes (mulPes (leaf i) s_acc) ind_acc, mulPes s_acc n, d_acc)
manifestResult _env _sched _pat_orig =
  error "In manifestResult: something went wrong"
  
  
mkWithAccStm ::
  (HasScope SOACS m, MonadFreshNames m) =>
  (VName, VName, Type) -> Param Type ->
  [(Param Type, Param Type, SubExp)] ->
  [PrimExp VName] -> m (Stm SOACS)
mkWithAccStm (nm_orig, nm_init, tp_orig) param_schd tup3s updt_ind = do
  let shp_orig= Shape $ arrayDims tp_orig
  cert_param <- newParam "acc_cert_p" $ Prim Unit
  acc_param  <-
    newParam (baseString nm_orig ++ "_acc") $
      Acc (paramName cert_param) shp_orig [Prim $ elemType tp_orig] NoUniqueness
  acc_bdy <- mkWithAccBdy updt_ind param_schd acc_param tup3s []
  let withacc_lam =
            Lambda
              { lambdaParams = [cert_param, acc_param],
                lambdaReturnType = [paramDec acc_param],
                lambdaBody = acc_bdy
              }
      withacc_inps = [(shp_orig, [nm_init], Nothing)]
      withacc_pat = Pat [PatElem nm_orig tp_orig]
  scope <- askScope
  (_,stms) <- runBuilder $ localScope scope $ do
     letBind withacc_pat $ WithAcc withacc_inps withacc_lam
  case stms of
    (stm Sq.:<| _) -> pure stm
    _ -> error "Unreachable case reached!"


mkWithAccBdy ::
  (HasScope SOACS m, MonadFreshNames m) =>
  [PrimExp VName] -> Param Type -> Param Type ->
  [(Param Type, Param Type, SubExp)] -> [VName] -> m (Body SOACS)
mkWithAccBdy updt_ind schd_param acc_param tup3s is
  -- recursive case:
  | (i_par, iot_par, w_se) : others <- tup3s = do
  acc_param' <- newParam (baseString (paramName acc_param)) (paramType acc_param)
  scope <- askScope
  runBodyBuilder $ localScope (scope <> scopeOfLParams [i_par, iot_par, schd_param, acc_param, acc_param']) $ do
    map_lam_bdy <-
      mkWithAccBdy updt_ind schd_param acc_param' others (paramName i_par : is)
    let map_lam = Lambda [i_par, acc_param'] [paramDec acc_param] map_lam_bdy
        map_soac = Screma w_se [paramName iot_par, paramName acc_param] $ ScremaForm map_lam [] []
    res_nms <- letTupExp "acc_res" $ Op map_soac
    pure $ map (subExpRes . Var) res_nms
  -- base case:
  | [] <- tup3s = do
  let nm_acc = paramName acc_param
  scope <- askScope
  runBodyBuilder $ localScope (scope <> scopeOfLParams [schd_param, acc_param]) $ do
    -- generate the vars holding the indices
    ind_ses <- forM (zip updt_ind [0..length updt_ind-1]) $ \ (ind_pe, k) ->
      letSubExp ("i_"++ show k) =<< toExp ind_pe
    -- get the value from the nested array
    v_se <- letSubExp "elem" $ BasicOp $ Index (paramName schd_param) $ Slice $ map (DimFix . Var) $ reverse is
    r_nm <- letExp (baseString nm_acc) $ BasicOp $ UpdateAcc Safe nm_acc ind_ses [v_se]
    pure [subExpRes $ Var r_nm]

