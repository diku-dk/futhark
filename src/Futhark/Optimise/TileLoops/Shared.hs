{-# LANGUAGE FlexibleContexts #-}

module Futhark.Optimise.TileLoops.Shared
  ( TileM,
    scratch,
    index,
    update,
    update',
    rebindLambda,
    forLoop',
    forLoop,
    segMap1D,
    segMap2D,
    segMap3D,
    segScatter2D,
    VarianceTable,
    varianceInStms,
    isTileableRedomap,
    changeEnv,
    kkLoopBody,
  )
where

import Control.Monad.Reader
import Control.Monad.State
import Data.List (foldl', zip4)
import qualified Data.Map as M
import Futhark.IR.GPU
import qualified Futhark.IR.Mem.IxFun as IxFun
import qualified Futhark.IR.SeqMem as ExpMem
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Transform.Rename

type TileM = ReaderT (Scope GPU) (State VNameSource)

scratch :: MonadBuilder m => String -> PrimType -> [SubExp] -> m VName
scratch se_name t shape = letExp se_name $ BasicOp $ Scratch t shape

-- index an array with indices given in outer_indices; any inner
-- dims of arr not indexed by outer_indices are sliced entirely
index :: MonadBuilder m => String -> VName -> [VName] -> m VName
index se_desc arr outer_indices = do
  arr_t <- lookupType arr
  let shape = arrayShape arr_t
      inner_dims = shapeDims $ stripDims (length outer_indices) shape
      untouched d = DimSlice (intConst Int64 0) d (intConst Int64 1)
      inner_slices = map untouched inner_dims
      slice = Slice $ map (DimFix . Var) outer_indices ++ inner_slices
  letExp se_desc $ BasicOp $ Index arr slice

update :: MonadBuilder m => String -> VName -> [VName] -> VName -> m VName
update se_desc arr indices new_elem = update' se_desc arr indices (Var new_elem)

update' :: MonadBuilder m => String -> VName -> [VName] -> SubExp -> m VName
update' se_desc arr indices new_elem =
  letExp se_desc $ BasicOp $ Update Unsafe arr (Slice $ map (DimFix . Var) indices) new_elem

-- given a lambda "lam", a list "new_params" of new
-- parameters which should be applied to the lambda,
-- and a VName "res_name" which the lambda result should
-- be bound to:
--   creates Stms corresponding to binding of new_params,
--   lambda body, and binding of lambda result to res_name.
rebindLambda ::
  Lambda GPU ->
  [VName] ->
  [VName] ->
  Stms GPU
rebindLambda lam new_params res_names =
  stmsFromList
    ( zipWith
        (\ident new_param -> mkLet [ident] $ BasicOp $ SubExp $ Var new_param)
        idents
        new_params
    )
    <> bodyStms lam_body
    <> stmsFromList res_cpy_stms
  where
    (lam_params, lam_body, lam_ret_type : _) =
      (lambdaParams lam, lambdaBody lam, lambdaReturnType lam)
    idents =
      map
        (\param -> Ident (paramName param) (paramDec param))
        lam_params
    res_cpy_stms =
      zipWith
        ( \res_name (SubExpRes cs lam_res) ->
            certify cs $ mkLet [Ident res_name lam_ret_type] $ BasicOp $ SubExp lam_res
        )
        res_names
        lam_ress
    lam_ress = bodyResult lam_body

forLoop' ::
  SubExp -> -- loop var
  [VName] -> -- loop inits
  ( VName ->
    [VName] -> -- (loop var -> loop inits -> loop body)
    Builder GPU (Body GPU)
  ) ->
  Builder GPU [VName]
forLoop' i_bound merge body = do
  i <- newVName "i" -- could give this as arg to the function
  let loop_form = ForLoop i Int64 i_bound []

  merge_ts <- mapM lookupType merge
  loop_inits <- mapM (\merge_t -> newParam "merge" $ toDecl merge_t Unique) merge_ts

  loop_body <-
    runBodyBuilder . inScopeOf loop_form . localScope (scopeOfFParams loop_inits) $
      body i $ map paramName loop_inits

  letTupExp "loop" $
    DoLoop (zip loop_inits $ map Var merge) loop_form loop_body

forLoop ::
  SubExp ->
  [VName] ->
  (VName -> [VName] -> Builder GPU (Body GPU)) ->
  Builder GPU VName
forLoop i_bound merge body = do
  res_list <- forLoop' i_bound merge body
  return $ head res_list

segMap1D ::
  String ->
  SegLevel ->
  ResultManifest ->
  (VName -> Builder GPU Result) ->
  Builder GPU [VName]
segMap1D desc lvl manifest f = do
  ltid <- newVName "ltid"
  ltid_flat <- newVName "ltid_flat"
  let space = SegSpace ltid_flat [(ltid, unCount $ segGroupSize lvl)]

  ((ts, res), stms) <- localScope (scopeOfSegSpace space) . runBuilder $ do
    res <- f ltid
    ts <- mapM subExpResType res
    return (ts, res)
  Body _ stms' res' <- renameBody $ mkBody stms res

  let ret (SubExpRes cs se) = Returns manifest cs se
  letTupExp desc $
    Op . SegOp $
      SegMap lvl space ts $ KernelBody () stms' $ map ret res'

segMap2D ::
  String -> -- desc
  SegLevel -> -- lvl
  ResultManifest -> -- manifest
  (SubExp, SubExp) -> -- (dim_x, dim_y)
  ( (VName, VName) -> -- f
    Builder GPU Result
  ) ->
  Builder GPU [VName]
segMap2D desc lvl manifest (dim_y, dim_x) f = do
  ltid_xx <- newVName "ltid_x"
  ltid_flat <- newVName "ltid_flat"
  ltid_yy <- newVName "ltid_y"
  let segspace = SegSpace ltid_flat [(ltid_yy, dim_y), (ltid_xx, dim_x)]

  ((ts, res), stms) <- localScope (scopeOfSegSpace segspace) . runBuilder $ do
    res <- f (ltid_yy, ltid_xx)
    ts <- mapM subExpResType res
    return (ts, res)

  let ret (SubExpRes cs se) = Returns manifest cs se
  letTupExp desc <=< renameExp $
    Op . SegOp $
      SegMap lvl segspace ts $ KernelBody () stms $ map ret res

segMap3D ::
  String -> -- desc
  SegLevel -> -- lvl
  ResultManifest -> -- manifest
  (SubExp, SubExp, SubExp) -> -- (dim_z, dim_y, dim_x)
  ( (VName, VName, VName) -> -- f
    Builder GPU Result
  ) ->
  Builder GPU [VName]
segMap3D desc lvl manifest (dim_z, dim_y, dim_x) f = do
  ltid_x <- newVName "ltid_x"
  ltid_flat <- newVName "ltid_flat"
  ltid_y <- newVName "ltid_y"
  ltid_z <- newVName "ltid_z"
  let segspace = SegSpace ltid_flat [(ltid_z, dim_z), (ltid_y, dim_y), (ltid_x, dim_x)]

  ((ts, res), stms) <- localScope (scopeOfSegSpace segspace) . runBuilder $ do
    res <- f (ltid_z, ltid_y, ltid_x)
    ts <- mapM subExpResType res
    return (ts, res)

  let ret (SubExpRes cs se) = Returns manifest cs se
  letTupExp desc <=< renameExp $
    Op . SegOp $
      SegMap lvl segspace ts $ KernelBody () stms $ map ret res

segScatter2D ::
  String -> -- desc
  SubExp -> -- arr_size
  VName ->
  SegLevel -> -- lvl
  (SubExp, SubExp) -> -- (dim_y, dim_x)
  ((VName, VName) -> Builder GPU (SubExp, SubExp)) -> -- f
  Builder GPU [VName]
segScatter2D desc arr_size updt_arr lvl (dim_x, dim_y) f = do
  ltid_x <- newVName "ltid_x"
  ltid_y <- newVName "ltid_y"
  ltid_flat <- newVName "ltid_flat"
  let segspace = SegSpace ltid_flat [(ltid_x, dim_x), (ltid_y, dim_y)]

  ((t_v, res_v, res_i), stms) <- runBuilder $ do
    (res_v, res_i) <- f (ltid_x, ltid_y)
    t_v <- subExpType res_v
    return (t_v, res_v, res_i)

  let ret = WriteReturns mempty (Shape [arr_size]) updt_arr [(Slice [DimFix res_i], res_v)]
  let body = KernelBody () stms [ret]

  letTupExp desc <=< renameExp $ Op $ SegOp $ SegMap lvl segspace [t_v] body

-- | The variance table keeps a mapping from a variable name
-- (something produced by a 'Stm') to the kernel thread indices
-- that name depends on.  If a variable is not present in this table,
-- that means it is bound outside the kernel (and so can be considered
-- invariant to all dimensions).
type VarianceTable = M.Map VName Names

isTileableRedomap ::
  Stm GPU ->
  Maybe
    ( SubExp,
      [VName],
      (Commutativity, Lambda GPU, [SubExp], Lambda GPU)
    )
isTileableRedomap stm
  | Op (OtherOp (Screma w arrs form)) <- stmExp stm,
    Just (reds, map_lam) <- isRedomapSOAC form,
    Reduce red_comm red_lam red_nes <- singleReduce reds,
    all (primType . rowType . paramType) $ lambdaParams red_lam,
    all (primType . rowType . paramType) $ lambdaParams map_lam,
    lambdaReturnType map_lam == lambdaReturnType red_lam, -- No mapout arrays.
    not (null arrs),
    all primType $ lambdaReturnType map_lam,
    all (primType . paramType) $ lambdaParams map_lam =
    Just (w, arrs, (red_comm, red_lam, red_nes, map_lam))
  | otherwise =
    Nothing

defVarianceInStm :: VarianceTable -> Stm GPU -> VarianceTable
defVarianceInStm variance stm =
  foldl' add variance $ patNames $ stmPat stm
  where
    add variance' v = M.insert v binding_variance variance'
    look variance' v = oneName v <> M.findWithDefault mempty v variance'
    binding_variance = mconcat $ map (look variance) $ namesToList (freeIn stm)

-- just in case you need the Screma being treated differently than
-- by default; previously Cosmin had to enhance it when dealing with stream.
varianceInStm :: VarianceTable -> Stm GPU -> VarianceTable
varianceInStm v0 stm@(Let _ _ (Op (OtherOp Screma {})))
  | Just (_, arrs, (_, red_lam, red_nes, map_lam)) <- isTileableRedomap stm =
    let v = defVarianceInStm v0 stm
        red_ps = lambdaParams red_lam
        map_ps = lambdaParams map_lam
        card_red = length red_nes
        acc_lam_f = take (card_red `quot` 2) red_ps
        arr_lam_f = drop (card_red `quot` 2) red_ps
        stm_lam = bodyStms (lambdaBody map_lam) <> bodyStms (lambdaBody red_lam)

        f vacc (v_a, v_fm, v_fr_acc, v_fr_var) =
          let vrc = oneName v_a <> M.findWithDefault mempty v_a vacc
              vacc' = M.insert v_fm vrc vacc
              vrc' = oneName v_fm <> vrc
           in M.insert v_fr_acc (oneName v_fr_var <> vrc') $ M.insert v_fr_var vrc' vacc'

        v' =
          foldl' f v $
            zip4 arrs (map paramName map_ps) (map paramName acc_lam_f) (map paramName arr_lam_f)
     in varianceInStms v' stm_lam
varianceInStm v0 stm = defVarianceInStm v0 stm

varianceInStms :: VarianceTable -> Stms GPU -> VarianceTable
varianceInStms = foldl' varianceInStm

----------------
---- Helpers for building the environment that binds array variable names to their index functions
----------------

type IxFun = IxFun.IxFun (TPrimExp Int64 VName)

type IxFnEnv = M.Map VName IxFun

type WithEnv = M.Map VName (Lambda GPU, [SubExp])

type Env = (WithEnv, IxFnEnv)

changeEnv :: Env -> VName -> Exp GPU -> TileM Env
changeEnv (with_env, ixfn_env) y e = do
  with_env' <- changeWithEnv with_env e
  ixfn_env' <- changeIxFnEnv ixfn_env y e
  return (with_env', ixfn_env')

changeWithEnv :: WithEnv -> Exp GPU -> TileM WithEnv
changeWithEnv with_env (WithAcc accum_decs inner_lam) = do
  let bindings = map mapfun accum_decs
      par_tps = take (length bindings) $ map paramName $ lambdaParams inner_lam
      with_env' = M.union with_env $ M.fromList $ zip par_tps bindings
  return with_env'
  where
    mapfun (_, _, Nothing) = error "What the hack is an accumulator without operator?"
    mapfun (shp, _, Just (lam_inds, ne)) =
      let len_inds = length $ shapeDims shp
          lam_op = lam_inds {lambdaParams = drop len_inds $ lambdaParams lam_inds}
       in (lam_op, ne)
changeWithEnv with_env _ = return with_env

composeIxfuns :: IxFnEnv -> VName -> VName -> (IxFun -> IxFun) -> TileM IxFnEnv
composeIxfuns env y x ixf_fun =
  case M.lookup x env of
    Just ixf -> return $ M.insert y (ixf_fun ixf) env
    Nothing -> do
      tp <- lookupType x
      case tp of
        Array _ptp shp _u -> do
          let shp' = map ExpMem.pe64 (shapeDims shp)
          return $ M.insert y (ixf_fun $ IxFun.iota shp') env
        _ -> return env

changeIxFnEnv :: IxFnEnv -> VName -> Exp GPU -> TileM IxFnEnv
changeIxFnEnv env y (BasicOp (Reshape shp_chg x)) =
  composeIxfuns env y x (`IxFun.reshape` map (fmap ExpMem.pe64) shp_chg)
changeIxFnEnv env y (BasicOp (Rearrange perm x)) =
  composeIxfuns env y x (`IxFun.permute` perm)
changeIxFnEnv env y (BasicOp (Rotate rs x)) =
  composeIxfuns env y x (`IxFun.rotate` fmap ExpMem.pe64 rs)
changeIxFnEnv env y (BasicOp (Index x slc)) =
  composeIxfuns env y x (`IxFun.slice` (Slice $ map (fmap ExpMem.pe64) $ unSlice slc))
changeIxFnEnv env _ _ = return env

--------
--- Main helper function for Register-and-Block Tiling
--------
kkLoopBody ::
  ( (SubExp, SubExp, SubExp, SubExp, SubExp, SubExp, SubExp, SubExp),
    SegLevel,
    [Int],
    (VName, SubExp, VName, SubExp, SubExp),
    (SubExp, SubExp),
    (VName, VName),
    (Stm GPU, VName, Stm GPU, VName),
    (Lambda GPU, Lambda GPU)
  ) ->
  VName ->
  (VName, VName, VName) ->
  Bool ->
  Builder GPU [VName]
kkLoopBody
  ( (rx, ry, tx, ty, tk, tk_div_tx, tk_div_ty, tx_rx),
    segthd_lvl,
    var_dims,
    (gtid_x, width_B, gtid_y, height_A, common_dim),
    (a_loc_sz, b_loc_sz),
    (iii, jjj),
    (load_A, inp_A, load_B, inp_B),
    (map_lam, red_lam)
    )
  kk0
  (thd_res_merge, a_loc_init', b_loc_init')
  epilogue = do
    let [map_t1, map_t2] = map (elemType . paramDec) $ lambdaParams map_lam
    kk <- letExp "kk" =<< toExp (le64 kk0 * pe64 tk)
    a_loc <- forLoop ry [a_loc_init'] $ \i0 [a_loc_merge] -> do
      loop_a_loc <- forLoop tk_div_tx [a_loc_merge] $ \k0 [a_loc_merge'] -> do
        scatter_a_loc <- segScatter2D "A_glb2loc" a_loc_sz a_loc_merge' segthd_lvl (ty, tx) $
          \(thd_y, thd_x) -> do
            k <- letExp "k" =<< toExp (le64 thd_x + le64 k0 * pe64 tx)
            i <- letExp "i" =<< toExp (le64 thd_y + le64 i0 * pe64 ty)

            letBindNames [gtid_y] =<< toExp (le64 iii + le64 i)
            a_col_idx <- letExp "A_col_idx" =<< toExp (le64 kk + le64 k)

            a_elem <-
              letSubExp "A_elem"
                =<< eIf
                  ( toExp $
                      le64 gtid_y .<. pe64 height_A
                        .&&. if epilogue
                          then le64 a_col_idx .<. pe64 common_dim
                          else true
                  )
                  ( do
                      addStm load_A
                      res <- index "A_elem" inp_A [a_col_idx]
                      resultBodyM [Var res]
                  )
                  (eBody [eBlank $ Prim map_t1])
            a_loc_ind <-
              letSubExp "a_loc_ind"
                =<< eIf
                  (toExp $ le64 k .<. pe64 tk)
                  ( toExp (le64 k + le64 i * pe64 tk)
                      >>= letTupExp' "loc_fi"
                      >>= resultBodyM
                  )
                  (eBody [pure $ BasicOp $ SubExp $ intConst Int64 (-1)])
            return (a_elem, a_loc_ind)
        resultBodyM $ map Var scatter_a_loc
      resultBodyM [Var loop_a_loc]

    -- copy B from global to shared memory
    b_loc <- forLoop tk_div_ty [b_loc_init'] $ \k0 [b_loc_merge] -> do
      loop_b_loc <- forLoop rx [b_loc_merge] $ \j0 [b_loc_merge'] -> do
        scatter_b_loc <- segScatter2D
          "B_glb2loc"
          b_loc_sz
          b_loc_merge'
          segthd_lvl
          (ty, tx)
          $ \(thd_y, thd_x) -> do
            k <- letExp "k" =<< toExp (le64 thd_y + le64 k0 * pe64 ty)
            j <- letExp "j" =<< toExp (le64 thd_x + le64 j0 * pe64 tx)

            letBindNames [gtid_x] =<< toExp (le64 jjj + le64 j)
            b_row_idx <- letExp "B_row_idx" =<< toExp (le64 kk + le64 k)

            b_elem <-
              letSubExp "B_elem"
                =<< eIf
                  ( toExp $
                      le64 gtid_x .<. pe64 width_B
                        .&&. if epilogue
                          then le64 b_row_idx .<. pe64 common_dim
                          else true
                  )
                  ( do
                      addStm load_B
                      res <- index "B_elem" inp_B [b_row_idx]
                      resultBodyM [Var res]
                  )
                  (eBody [eBlank $ Prim map_t2])

            b_loc_ind <-
              letSubExp "b_loc_ind"
                =<< eIf
                  (toExp $ le64 k .<. pe64 tk)
                  ( toExp (le64 j + le64 k * pe64 tx_rx)
                      >>= letTupExp' "loc_fi"
                      >>= resultBodyM
                  )
                  (eBody [pure $ BasicOp $ SubExp $ intConst Int64 (-1)])
            return (b_elem, b_loc_ind)
        resultBodyM $ map Var scatter_b_loc
      resultBodyM [Var loop_b_loc]

    -- inner loop updating this thread's accumulator (loop k in mmm_kernels).
    thd_acc <- forLoop tk [thd_res_merge] $ \k [acc_merge] ->
      resultBodyM =<< letTupExp' "foo"
        =<< eIf
          ( toExp $
              if epilogue
                then
                  le64 kk + le64 k
                    .<. pe64 common_dim
                else true -- if in prologue, always compute redomap.
          )
          ( do
              reg_mem <- segMap2D "reg_mem" segthd_lvl ResultPrivate (ty, tx) $
                \(ltid_y, ltid_x) -> do
                  asss_init <- scratch "asss_init" map_t1 [ry]
                  bsss_init <- scratch "bsss_init" map_t2 [rx]

                  asss <- forLoop ry [asss_init] $ \i [asss_merge] -> do
                    a_loc_ind <-
                      letExp "a_loc_ind"
                        =<< toExp
                          ( le64 k
                              + (le64 ltid_y * pe64 ry + le64 i) * pe64 tk
                          )

                    asss <-
                      index "A_loc_elem" a_loc [a_loc_ind]
                        >>= update "asss" asss_merge [i]
                    resultBodyM [Var asss]

                  bsss <- forLoop rx [bsss_init] $ \j [bsss_merge] -> do
                    b_loc_ind <-
                      letExp "b_loc_ind"
                        =<< toExp
                          ( le64 j
                              + le64 k * pe64 tx_rx
                              + le64 ltid_x * pe64 rx
                          )

                    bsss <-
                      index "B_loc_elem" b_loc [b_loc_ind]
                        >>= update "bsss" bsss_merge [j]
                    resultBodyM [Var bsss]
                  return $ varsRes [asss, bsss]

              let [asss, bsss] = reg_mem

              -- the actual redomap.
              redomap_res <- segMap2D "redomap_res" segthd_lvl ResultPrivate (ty, tx) $
                \(ltid_y, ltid_x) -> do
                  as <- index "as" asss [ltid_y, ltid_x]
                  bs <- index "bs" bsss [ltid_y, ltid_x]
                  css_init <- index "css_init" acc_merge [ltid_y, ltid_x]

                  css <- forLoop ry [css_init] $ \i [css_merge] -> do
                    css <- forLoop rx [css_merge] $ \j [css_merge'] ->
                      resultBodyM =<< letTupExp' "foo"
                        =<< eIf
                          ( toExp $
                              le64 iii + le64 i + pe64 ry * le64 ltid_y
                                .<. pe64 height_A
                                  .&&. le64 jjj + le64 j + pe64 rx * le64 ltid_x
                                .<. pe64 width_B
                          )
                          ( do
                              a <- index "a" as [i]
                              b <- index "b" bs [j]
                              c <- index "c" css_merge' [i, j]

                              map_res <- newVName "map_res"
                              map_lam' <- renameLambda map_lam
                              red_lam' <- renameLambda red_lam

                              -- the inputs to map are supposed to be permutted with the
                              -- inverted permutation, so as to reach the original position;
                              -- it just so happens that the inverse of [a,b] is [b,a]
                              let map_inp_reg = if var_dims == [0, 1] then [a, b] else [b, a]

                              addStms $
                                rebindLambda map_lam' map_inp_reg [map_res]
                                  <> rebindLambda red_lam' [c, map_res] [c]

                              css <- update "css" css_merge' [i, j] c

                              resultBodyM [Var css]
                          )
                          (resultBodyM [Var css_merge'])
                    resultBodyM [Var css]
                  return [varRes css]

              resultBodyM $ map Var redomap_res
          )
          (resultBodyM [Var acc_merge])
    return [thd_acc, a_loc, b_loc]
