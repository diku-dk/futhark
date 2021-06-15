module Futhark.Optimise.TileLoops.Shared
  ( TileM,
    segMap2D,
    segMap3D,
    segScatter2D,
    VarianceTable,
    varianceInStms,
    isTileableRedomap,
  )
where

import Control.Monad.Reader
import Control.Monad.State
import Data.List (foldl', zip4)
import qualified Data.Map as M
import Futhark.IR.GPU
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Transform.Rename

type TileM = ReaderT (Scope GPU) (State VNameSource)

segMap2D ::
  String -> -- desc
  SegLevel -> -- lvl
  ResultManifest -> -- manifest
  (SubExp, SubExp) -> -- (dim_x, dim_y)
  ( (VName, VName) -> -- f
    Binder GPU [SubExp]
  ) ->
  Binder GPU [VName]
segMap2D desc lvl manifest (dim_y, dim_x) f = do
  ltid_xx <- newVName "ltid_x"
  ltid_flat <- newVName "ltid_flat"
  ltid_yy <- newVName "ltid_y"
  let segspace = SegSpace ltid_flat [(ltid_yy, dim_y), (ltid_xx, dim_x)]

  ((ts, res), stms) <- runBinder $ do
    res <- f (ltid_yy, ltid_xx)
    ts <- mapM subExpType res
    return (ts, res)

  letTupExp desc <=< renameExp $
    Op $
      SegOp $
        SegMap lvl segspace ts $ KernelBody () stms $ map (Returns manifest) res

segMap3D ::
  String -> -- desc
  SegLevel -> -- lvl
  ResultManifest -> -- manifest
  (SubExp, SubExp, SubExp) -> -- (dim_z, dim_y, dim_x)
  ( (VName, VName, VName) -> -- f
    Binder GPU [SubExp]
  ) ->
  Binder GPU [VName]
segMap3D desc lvl manifest (dim_z, dim_y, dim_x) f = do
  ltid_x <- newVName "ltid_x"
  ltid_flat <- newVName "ltid_flat"
  ltid_y <- newVName "ltid_y"
  ltid_z <- newVName "ltid_z"
  let segspace = SegSpace ltid_flat [(ltid_z, dim_z), (ltid_y, dim_y), (ltid_x, dim_x)]

  ((ts, res), stms) <- runBinder $ do
    res <- f (ltid_z, ltid_y, ltid_x)
    ts <- mapM subExpType res
    return (ts, res)

  letTupExp desc <=< renameExp $
    Op $
      SegOp $
        SegMap lvl segspace ts $ KernelBody () stms $ map (Returns manifest) res

segScatter2D ::
  String -> -- desc
  SubExp -> -- arr_size
  VName ->
  SegLevel -> -- lvl
  (SubExp, SubExp) -> -- (dim_y, dim_x)
  ((VName, VName) -> Binder GPU (SubExp, SubExp)) -> -- f
  Binder GPU [VName]
segScatter2D desc arr_size updt_arr lvl (dim_x, dim_y) f = do
  ltid_x <- newVName "ltid_x"
  ltid_y <- newVName "ltid_y"
  ltid_flat <- newVName "ltid_flat"
  let segspace = SegSpace ltid_flat [(ltid_x, dim_x), (ltid_y, dim_y)]

  ((t_v, res_v, res_i), stms) <- runBinder $ do
    (res_v, res_i) <- f (ltid_x, ltid_y)
    t_v <- subExpType res_v
    return (t_v, res_v, res_i)

  let ret = WriteReturns (Shape [arr_size]) updt_arr [([DimFix res_i], res_v)]
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
defVarianceInStm variance bnd =
  foldl' add variance $ patternNames $ stmPattern bnd
  where
    add variance' v = M.insert v binding_variance variance'
    look variance' v = oneName v <> M.findWithDefault mempty v variance'
    binding_variance = mconcat $ map (look variance) $ namesToList (freeIn bnd)

-- just in case you need the Screma being treated differently than
-- by default; previously Cosmin had to enhance it when dealing with stream.
varianceInStm :: VarianceTable -> Stm GPU -> VarianceTable
varianceInStm v0 bnd@(Let _ _ (Op (OtherOp Screma {})))
  | Just (_, arrs, (_, red_lam, red_nes, map_lam)) <- isTileableRedomap bnd =
    let v = defVarianceInStm v0 bnd
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
varianceInStm v0 bnd = defVarianceInStm v0 bnd

varianceInStms :: VarianceTable -> Stms GPU -> VarianceTable
varianceInStms = foldl' varianceInStm
