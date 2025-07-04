{-# LANGUAGE Strict #-}

-- | This module performs the stripmining part
--     of a high-level schedule
module Futhark.Optimise.Fusion.HLsched.Stripmine
  ( applyStripmining
  )
where

--import Data.List qualified as L
import Control.Monad
--import Data.Graph.Inductive.Graph qualified as G
import Data.Map.Strict qualified as M
import Data.Set qualified as S
--import Data.Maybe
import Futhark.Analysis.HORep.SOAC qualified as H
import Futhark.Construct
import Futhark.IR.SOACS hiding (SOAC (..))
import Futhark.IR.SOACS qualified as F
--import Futhark.Optimise.Fusion.GraphRep
--import Futhark.Tools
--import Futhark.Transform.Rename
--import Futhark.Transform.Substitute
--import Futhark.Analysis.PrimExp
import Futhark.Optimise.Fusion.HLsched.Utils
--import Futhark.Util.Pretty hiding (line, sep, (</>))
import Futhark.Analysis.PrimExp.Convert

import Debug.Trace

-- | ToDos:
--   1. need to treat SOAC's inputs that are iota arrays
--   2. need to generate the code for the iota inputs to various soacs
applyStripmining ::
    (HasScope SOACS m, MonadFreshNames m) =>
    Env -> HLSched -> H.SOAC SOACS -> m (Maybe (H.SOAC SOACS))
applyStripmining env sched (H.Screma mm inps (H.ScremaForm map_lam [] []))
  | Just (sched', sched'') <- validStripForDim env 0 mm sched,
    not (any hasTransform inps) = do
  --
  let inp_arr_nm_tps = map extractNmTp inps
  mres_lam <- stripmineLambda env sched'' map_lam inp_arr_nm_tps mm
  case mres_lam of
    Just (_prologue_stms1, env1, lam') -> do
      mres_nest <- buildMapNest (0,inp_arr_nm_tps,mm) env1 (dimlens sched') pe0 lam'
      case mres_nest of
        Just (prologue_stms2, _env2, soac@(F.Screma m' [nm] form'@(F.ScremaForm _ [] []))) -> do
        -- ^ by construction the resulting soac has one iota-array arg
          let tp = Array i64ptp (Shape [m']) NoUniqueness
              hinp = H.Input H.noTransforms nm tp
          trace ("stripmined soac: \n" ++ prettyString soac ++
                 "\n prologues-stms:\n"++ prettyString prologue_stms2 ) $
           pure $ Just $ H.Screma m' [hinp] form'
        _ -> pure Nothing
    _ -> pure Nothing
  where
    hasTransform (H.Input transf _ _) = not $ H.nullTransforms transf
    extractNmTp (H.Input _ nm tp) = (nm,tp)
applyStripmining _ _ _ = pure Nothing

--------------------------------------------------------------------------
--------------------------------------------------------------------------

validStripForDim :: Env -> Int -> SubExp -> HLSched -> Maybe (HLSched, HLSched)
validStripForDim env k m sched
  | q <- countSame $ origids sched,
    (sched', sched'') <- splitAtSched q sched, 
    validSize (dimlens sched') =
  Just (sched', sched'')
  where
    countSame [] = 0
    countSame (i:ctarr) =
      if k == i
      then 1 + countSame ctarr
      else 0
    validSize dims
      | Just nms <- M.lookup (peFromSeI64 m) (appTilesFwd env),
        penms <- S.fromList $ map (`LeafExp` i64ptp) $ namesToList nms,
        length dims == S.size penms,
        penms == S.fromList dims = True
    validSize _ = False
validStripForDim _ _ _ _ = Nothing

----------------------------------------------------------------------------
----------------------------------------------------------------------------

-- | ToDos: needs to be improved, for example to:
--   1. return an expression, not a SOAC, otherwise loops cannot
--      be stripmined; question is how do I get the types in recuCase???
--   2. populate the environment, for example bind the PrimExp
--      of i_flat
--   3. not here, at the very beginning of the pass, need to
--      track all dependencies of the original SOAC and populate
--      the symbol table with bindings for:
--        (a) PrimExp of integral scalar variables
--        (b) transformation on arrays, e.g., transpose
buildMapNest :: (HasScope SOACS m, MonadFreshNames m) =>
    (Int, [(VName,Type)], SubExp) ->
    Env -> [PrimExp VName] ->
    PrimExp VName -> Lambda SOACS ->
    m (Maybe (Stms SOACS, Env, F.SOAC SOACS))
buildMapNest (k, inp_nm_tps, mm) env (n:ns) cur_ind lam = do
  i_p <- newParam ("i"++show k) $ Prim i64ptp
  let i_pe = LeafExp (paramName i_p) i64ptp
      i_mul_ns = mulPes i_pe $ foldl mulPes pe1 ns
      cur_ind' = addPes cur_ind i_mul_ns
  -- build the iota for this screma
  scope <- askScope
  iota_info <-
    runBuilder $ localScope scope $ do
      w <- letSubExp ("w"++show k) =<< toExp n
      iotnm <- letExp "iota" $ BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
      pure (w, iotnm)
  if null ns
  then baseCase scope i_p cur_ind' iota_info
  else recuCase scope i_p cur_ind' iota_info
  where
    baseCase scope i_p cur_ind' ((w,iotnm), prologue_stms) = do
      new_lam <-
        runLambdaBuilder [i_p] $ localScope scope $ do
          let i_flat_pe = minPes cur_ind' $ BinOpExp (Sub Int64 OverflowUndef) (peFromSeI64 mm) pe1 
          i_flat <- letSubExp ("i"++show k++"flat") =<< toExp i_flat_pe
          forM_ (zip (lambdaParams lam) inp_nm_tps) $ \ (lp, (inp_nm, _inp_tp)) -> do
            letBind (Pat [PatElem (paramName lp) (paramDec lp)]) $
                      BasicOp $ Index inp_nm (Slice [DimFix i_flat])
          addStms $ bodyStms $ lambdaBody lam
          pure $ bodyResult $ lambdaBody lam
      let soac = F.Screma w [iotnm] $ ScremaForm new_lam [] []
      pure $ Just (prologue_stms, env, soac)
    --
    recuCase scope i_p cur_ind' ((w,iotnm), prologue_stms) = do
      rec_res <- buildMapNest (k, inp_nm_tps, mm) env ns cur_ind' lam
      case rec_res of
        Just (prev_stms, prev_env, prev_soac) -> do
          new_lam <-
            runLambdaBuilder [i_p] $ localScope scope $ do
              prev_soac_res <- letTupExp' ("map"++show k++"strip") $ Op prev_soac
              pure $ map subExpRes prev_soac_res
          let soac = F.Screma w [iotnm] $ ScremaForm new_lam [] []
          pure $ Just (prologue_stms <> prev_stms, prev_env, soac)
        -- end ^
        _ -> pure Nothing
buildMapNest _ _ [] _ _ =
  error "buildMapNest: unreachable case reached!"

----------------------------------------------------------------------------
----------------------------------------------------------------------------

stripmineLambda :: (HasScope SOACS m, MonadFreshNames m) =>
    Env -> HLSched -> Lambda SOACS -> [(VName, Type)] ->
    SubExp -> m (Maybe (Stms SOACS, Env, Lambda SOACS))
stripmineLambda env _sched lam _inp_nm_tps _m =
  pure $ Just (mempty, env, lam)

--------------------------------------------------------------------------
--- Helpers
--------------------------------------------------------------------------

pe0 :: PrimExp VName
pe0 = ValueExp $ IntValue $ Int64Value 0

pe1 :: PrimExp VName
pe1 = ValueExp $ IntValue $ Int64Value 1

i64ptp :: PrimType
i64ptp = IntType Int64

peFromSeI64 :: SubExp -> PrimExp VName
peFromSeI64 (Constant pv) = ValueExp pv
peFromSeI64 (Var vnm) = LeafExp vnm i64ptp

addPes :: PrimExp VName -> PrimExp VName -> PrimExp VName
addPes e1 e2 | e1 == pe0 = e2
addPes e1 e2 | e2 == pe0 = e1
addPes e1 e2 = BinOpExp (Add Int64 OverflowUndef) e1 e2

mulPes :: PrimExp VName -> PrimExp VName -> PrimExp VName
mulPes e1 e2 | e1 == pe1 = e2
mulPes e1 e2 | e2 == pe1 = e1
mulPes e1 e2 = BinOpExp (Mul Int64 OverflowUndef) e1 e2 

minPes :: PrimExp VName -> PrimExp VName -> PrimExp VName
minPes e1 e2 | e1 == e2 = e1
minPes e1 e2 = BinOpExp (SMin Int64) e1 e2

--------------------------------------------------------------------------
--- GARBAGE CODE
--------------------------------------------------------------------------

{--
applyStripmining env sched (H.Screma m inps form) = do
  soac <- stripmineRec 0 env sched $ F.Screma m (map selVName inps) form
  case soac of
    Just (F.Screma sz [iot] form') -> do
      let tp = Array i64ptp (Shape [sz]) NoUniqueness
          hinps = H.Input H.noTransforms iot tp
      pure $ Just $ H.Screma sz [hinps] form'
    _ -> pure Nothing
  where
    selVName (H.Input _ vnm _) = vnm
--}


{--
addNewMap :: (HasScope SOACS m, MonadFreshNames m) => Env -> PrimExp VName -> m (VName, SubExp, Env)
addNewMap len_pe = do
  scope <- askScope
  ((w,iotnm), stms) <-
    runBuilder $ localScope scope $ do
      w <- letSubExp "sz" =<< toExp len_pe
      iotnm <- letExp "iota" . BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
      pure (w, iotnm)
  let iotas' = M.insert iotanm (len_pe, w, stms) $ iotas env
  pure ( iotnm, w, Env { iotas = iotas' } )
--}


{--
stripmineRec ::
    (HasScope SOACS m, MonadFreshNames m) =>
    Int -> Env -> HLSched -> F.SOAC SOACS -> m (Maybe (F.SOAC SOACS))
stripmineRec k env sched soac@(F.Screma m inps form)
  | q <- countSame $ origids sched,
    (sched', sched'') <- splitAtSched q sched, 
    validSize (dimlens sched') = do
  case form of
    -- 1. the common case of map stripmining
    H.ScremaForm map_lam [] [] -> do
      
      env' <- foldM addNewIotaToEnv env (dimlens sched')
      pure $ Just soac
    _ -> error "Stripmining anything other than map is not implemented yet!"
  where
    peFromSe (Constant pv) = ValueExp pv
    peFromSe (Var vnm) = LeafExp vnm i64ptp
    countSame [] = 0
    countSame (i:ctarr) =
      if k == i
      then 1 + countSame ctarr
      else 0
    validSize dims
      | Just nms <- M.lookup (peFromSe m) (appTilesFwd env),
        penms <- S.fromList $ map (`LeafExp` i64ptp) $ namesToList nms,
        length dims == S.size penms,
        penms == S.fromList dims = True
    validSize _ = False
--
stripmineRec _ _ _ _ = pure Nothing

addNewIotaToEnv :: (HasScope SOACS m, MonadFreshNames m) => Env -> PrimExp VName -> m (VName, SubExp, Env)
addNewIotaToEnv len_pe = do
  scope <- askScope
  ((w,iotnm), stms) <-
    runBuilder $ localScope scope $ do
      w <- letSubExp "sz" =<< toExp len_pe
      iotnm <- letExp "iota" . BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
      pure (w, iotnm)
  let iotas' = M.insert iotanm (len_pe, w, stms) $ iotas env
  pure ( iotnm, w, Env { iotas = iotas' } )
--}

{--
buildMapNest (k, inp_nm_tps, mm) env [n] cur_ind lam = do
  i_p <- newParam ("i"++show k) $ Prim i64ptp
  let i_pe = LeafExp (paramName i_p) i64ptp
      cur_ind' = addPes cur_ind i_pe
  scope <- askScope
  ((w,iotnm), prologue_stms) <-
    runBuilder $ localScope scope $ do
      w <- letSubExp ("w"++show k) =<< toExp n
      iotnm <- letExp "iota" $ BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
      pure (w, iotnm)
      -- 1. build a body that calls the screma
  lam_bdy <-
    runBodyBuilder $ localScope (scope <> scopeOfLParams [i_p]) $ do
      let i_flat_pe = minPes cur_ind' $ BinOpExp (Sub Int64 OverflowUndef) (peFromSeI64 mm) pe1 
      i_flat <- letSubExp ("i"++show k++"flat") =<< toExp i_flat_pe
      forM_ (zip (lambdaParams lam) inp_nm_tps) $ \ (lp, (inp_nm, _inp_tp)) -> do
        letBind (Pat [PatElem (paramName lp) (paramDec lp)]) $
                  BasicOp $ Index inp_nm (Slice [DimFix i_flat])
      addStms $ bodyStms $ lambdaBody lam
      pure $ bodyResult $ lambdaBody lam
  -- build the lambda and the result screma
  let new_lam = Lambda [i_p] (lambdaReturnType lam) lam_bdy
      soac = F.Screma w [iotnm] $ ScremaForm new_lam [] []
  pure $ Just (prologue_stms, env, soac)
--
buildMapNest (k, inp_nm_tps, mm) env (n:m:ms) cur_ind lam = do
  let ns = m:ms
  i_p <- newParam ("i"++show k) $ Prim i64ptp
  let i_pe = LeafExp (paramName i_p) i64ptp
      i_mul_ns = mulPes i_pe $ foldl mulPes pe1 ns
      cur_ind' = addPes cur_ind i_mul_ns
  rec_res <- buildMapNest (k, inp_nm_tps, mm) env ns cur_ind' lam
  case rec_res of
    Just (prev_stms, prev_env, prev_soac@(F.Screma prev_w _ (ScremaForm prev_lam _ _))) -> do
      scope <- askScope
      -- 0. build the iota for this screma
      ((w,iotnm), prologue_stms) <-
        runBuilder $ localScope scope $ do
          w <- letSubExp ("w"++show k) =<< toExp n
          iotnm <- letExp "iota" $ BasicOp $ Iota w (intConst Int64 0) (intConst Int64 1) Int64
          pure (w, iotnm)
      -- 1. build a body that calls the screma
      lam_bdy <-
        runBodyBuilder $ localScope (scope <> scopeOfLParams [i_p]) $ do
          prev_soac_res <- letTupExp' ("map"++show k++"strip") $ Op prev_soac
          pure $ map subExpRes prev_soac_res
      -- 2. build the resulting screma
      let lam_rts = map (`arrayOfRow` prev_w) $ lambdaReturnType prev_lam
          new_lam = Lambda [i_p] lam_rts lam_bdy
          soac = F.Screma w [iotnm] $ ScremaForm new_lam [] []
      pure $ Just (prologue_stms <> prev_stms, prev_env, soac)
    -- end ^
    _ -> pure Nothing
--
-- there is still a lot of common code, clean it up please!!!
--
buildMapNest _ _ [] _ _ =
  error "buildMapNest: unreachable case reached!"
--}

{--
          -- 1. build a body that calls the screma
          lam_bdy <-
            runBodyBuilder $ localScope (scope <> scopeOfLParams [i_p]) $ do
              prev_soac_res <- letTupExp' ("map"++show k++"strip") $ Op prev_soac
              lam_res_tps <- mapM getTypeSE prev_soac_res
              trace ("\nDEBUG: \n"++prettyString lam_res_tps) $
                pure $ map subExpRes prev_soac_res
          -- 2. build the lambda and the resulting screma
          -- lam_rts <- localScope scope $ do
          --              mapM getTypeSE $ map resSubExp $ bodyResult lam_bdy
          let lam_rts = map (`arrayOfRow` prev_w) $ lambdaReturnType prev_lam
              new_lam = Lambda [i_p] lam_rts lam_bdy
              soac = F.Screma w [iotnm] $ ScremaForm new_lam [] []
          pure $ Just (prologue_stms <> prev_stms, prev_env, soac)
--}

{--
      -- 1. adjust the lambda body
      lam_bdy <-
        runBodyBuilder $ localScope (scope <> scopeOfLParams [i_p]) $ do
          let i_flat_pe = minPes cur_ind' $ BinOpExp (Sub Int64 OverflowUndef) (peFromSeI64 mm) pe1 
          i_flat <- letSubExp ("i"++show k++"flat") =<< toExp i_flat_pe
          forM_ (zip (lambdaParams lam) inp_nm_tps) $ \ (lp, (inp_nm, _inp_tp)) -> do
            letBind (Pat [PatElem (paramName lp) (paramDec lp)]) $
                      BasicOp $ Index inp_nm (Slice [DimFix i_flat])
          addStms $ bodyStms $ lambdaBody lam
          pure $ bodyResult $ lambdaBody lam
      -- 2. build the lambda and the result screma
      let new_lam = Lambda [i_p] (lambdaReturnType lam) lam_bdy
          soac = F.Screma w [iotnm] $ ScremaForm new_lam [] []
      pure $ Just (prologue_stms, env, soac)
--}


{--
    getTypeSE (Constant pval) = pure (Prim (primValueType pval))
    getTypeSE (Var nm) = lookupType nm     
--}
