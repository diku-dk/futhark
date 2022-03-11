{-# LANGUAGE FlexibleContexts #-}

module Futhark.Optimise.TileLoops.Shared
  ( TileM,
    Env,
    index,
    update,
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
    TileKind (..),
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

-- | Are we working with full or partial tiles?
data TileKind = TilePartial | TileFull

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

update :: MonadBuilder m => String -> VName -> [VName] -> SubExp -> m VName
update se_desc arr indices new_elem =
  letExp se_desc $ BasicOp $ Update Unsafe arr (Slice $ map (DimFix . Var) indices) new_elem

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
    Op . SegOp $ SegMap lvl space ts $ KernelBody () stms' $ map ret res'

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
  ltid_yy <- newVName "ltid_y"
  ltid_flat <- newVName "ltid_flat"
  let segspace = SegSpace ltid_flat [(ltid_yy, dim_y), (ltid_xx, dim_x)]

  ((ts, res), stms) <- localScope (scopeOfSegSpace segspace) . runBuilder $ do
    res <- f (ltid_yy, ltid_xx)
    ts <- mapM subExpResType res
    return (ts, res)

  let ret (SubExpRes cs se) = Returns manifest cs se
  letTupExp desc <=< renameExp $
    Op . SegOp $ SegMap lvl segspace ts $ KernelBody () stms $ map ret res

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
  ltid_flat <- newVName "ltid_flat"
  ltid_z <- newVName "ltid_z"
  ltid_y <- newVName "ltid_y"
  ltid_x <- newVName "ltid_x"
  let segspace = SegSpace ltid_flat [(ltid_z, dim_z), (ltid_y, dim_y), (ltid_x, dim_x)]

  ((ts, res), stms) <- localScope (scopeOfSegSpace segspace) . runBuilder $ do
    res <- f (ltid_z, ltid_y, ltid_x)
    ts <- mapM subExpResType res
    return (ts, res)

  let ret (SubExpRes cs se) = Returns manifest cs se
  letTupExp desc <=< renameExp $
    Op . SegOp $ SegMap lvl segspace ts $ KernelBody () stms $ map ret res

segScatter2D ::
  String ->
  SubExp ->
  VName ->
  SegLevel -> -- lvl
  [SubExp] -> -- dims of sequential loop on top
  (SubExp, SubExp) -> -- (dim_y, dim_x)
  ([VName] -> (VName, VName) -> Builder GPU (SubExp, SubExp)) -> -- f
  Builder GPU VName
segScatter2D desc arr_size updt_arr lvl seq_dims (dim_x, dim_y) f = do
  ltid_flat <- newVName "ltid_flat"
  ltid_y <- newVName "ltid_y"
  ltid_x <- newVName "ltid_x"

  seq_is <- replicateM (length seq_dims) (newVName "ltid_seq")
  let seq_space = zip seq_is seq_dims

  let segspace = SegSpace ltid_flat $ seq_space ++ [(ltid_y, dim_y), (ltid_x, dim_x)]
      lvl' =
        SegThread
          (segNumGroups lvl)
          (segGroupSize lvl)
          (SegNoVirtFull (SegSeqDims [0 .. length seq_dims -1]))

  ((t_v, res_v, res_i), stms) <- runBuilder $ do
    (res_v, res_i) <-
      localScope (scopeOfSegSpace segspace) $
        f seq_is (ltid_y, ltid_x)
    t_v <- subExpType res_v
    return (t_v, res_v, res_i)

  let ret = WriteReturns mempty (Shape [arr_size]) updt_arr [(Slice [DimFix res_i], res_v)]
  let body = KernelBody () stms [ret]

  letExp desc <=< renameExp $ Op $ SegOp $ SegMap lvl' segspace [t_v] body

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

-- | Map from array variable names to their corresponding index functions.
--   The info is not guaranteed to be exact, e.g., we assume ifs and loops
--   return arrays layed out in normalized (row-major) form in memory.
--   We only record aliasing statements, such as transposition, slice, etc.
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
changeIxFnEnv env y (BasicOp (Manifest perm x)) = do
  tp <- lookupType x
  case tp of
    Array _ptp shp _u -> do
      let shp' = map ExpMem.pe64 (shapeDims shp)
      let ixfn = IxFun.permute (IxFun.iota shp') perm
      return $ M.insert y ixfn env
    _ -> error "In TileLoops/Shared.hs, changeIxFnEnv: manifest applied to a non-array!"
changeIxFnEnv env y (BasicOp (Rearrange perm x)) =
  composeIxfuns env y x (`IxFun.permute` perm)
changeIxFnEnv env y (BasicOp (Rotate rs x)) =
  composeIxfuns env y x (`IxFun.rotate` fmap ExpMem.pe64 rs)
changeIxFnEnv env y (BasicOp (Index x slc)) =
  composeIxfuns env y x (`IxFun.slice` (Slice $ map (fmap ExpMem.pe64) $ unSlice slc))
changeIxFnEnv env y (BasicOp (Opaque _ (Var x))) =
  composeIxfuns env y x id
changeIxFnEnv env _ _ = return env
