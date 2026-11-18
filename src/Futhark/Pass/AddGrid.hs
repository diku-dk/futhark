-- | Add grid information to SegOps.
--
-- The idea is that all decision making logic regarding the physical topology of
-- GPU execution is located here, rather than being scattered across the
-- flattening transformation.
module Futhark.Pass.AddGrid (mkVirtGrid, addGrid) where

import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Futhark.Analysis.PrimExp.Convert
import Futhark.Builder
import Futhark.Construct
import Futhark.IR.GPU
import Futhark.Pass
import Futhark.Transform.ToGPU (getSize)
import Futhark.Util.IntegralExp

-- | Make a non-virtualised grid for a SegThread kernel.
mkFullThreadGrid :: (MonadBuilder m, Rep m ~ GPU) => SegSpace -> m KernelGrid
mkFullThreadGrid space = do
  tblock_size <- getSize "tblock_size" SizeThreadBlock
  num_tblocks <-
    letSubExp "num_tblocks" <=< toExp $
      product (map pe64 (segSpaceDims space)) `ceilDiv` pe64 tblock_size
  pure $ KernelGrid (Count num_tblocks) (Count tblock_size)

-- | Make a non-virtualised grid for a SegBlock kernel.
mkFullBlockGrid ::
  (MonadBuilder m, Rep m ~ GPU) =>
  SegSpace -> [TPrimExp Int64 VName] -> m KernelGrid
mkFullBlockGrid space dims = do
  tblock_size <-
    letSubExp "tblock_size" <=< toExp $ foldl sMax64 1 dims
  num_tblocks <-
    letSubExp "num_tblocks" <=< toExp $ product (map pe64 (segSpaceDims space))
  pure $ KernelGrid (Count num_tblocks) (Count tblock_size)

-- | Make a grid for a fully virtualised kernel.
mkVirtGrid :: (MonadBuilder m, Rep m ~ GPU) => m KernelGrid
mkVirtGrid =
  KernelGrid
    <$> (Count <$> getSize "num_tblocks" SizeGrid)
    <*> (Count <$> getSize "tblock_size" SizeThreadBlock)

-- | Make a grid for a SegThread kernel.
mkThreadGrid :: (MonadBuilder m, Rep m ~ GPU) => SegSpace -> SegVirt -> m KernelGrid
mkThreadGrid space SegNoVirt = mkFullThreadGrid space
mkThreadGrid space (SegNoVirtFull _) = mkFullThreadGrid space
mkThreadGrid space SegVirt = do
  tblock_size <- getSize "tblock_size" SizeThreadBlock
  w <- letSubExp "num_workitems" <=< toExp $ product $ map pe64 $ segSpaceDims space
  name <- nameFromText . prettyText <$> newVName "num_tblocks"
  num_tblocks <- letSubExp "num_tblocks" $ Op $ SizeOp $ CalcNumBlocks w name tblock_size
  pure $ KernelGrid (Count num_tblocks) (Count tblock_size)

-- | Make a grid for a SegBlock kernel.
--
-- If we have purely virtualised nested SegOps, then the thread block size can
-- be set freely, but otherwise it must fit all the non-virtualised SegOps.
mkBlockGrid ::
  (MonadBuilder m, Rep m ~ GPU) =>
  SegSpace ->
  SegVirt ->
  [TPrimExp Int64 VName] ->
  m KernelGrid
mkBlockGrid space SegNoVirt dims = mkFullBlockGrid space dims
mkBlockGrid space (SegNoVirtFull _) dims = mkFullBlockGrid space dims
mkBlockGrid _ SegVirt [] = mkVirtGrid
mkBlockGrid space SegVirt dims = mkFullBlockGrid space dims

onlyNonVirt :: [(a, SegVirt)] -> [a]
onlyNonVirt = mapMaybe f
  where
    f (_, SegVirt) = Nothing
    f (ws, _) = Just ws

addGrid :: Pass GPU GPU
addGrid =
  Pass "add-grid" "add grid info to SegOps" $
    intraproceduralTransformation optimise
  where
    optimise scope stms = runReaderT (runBuilder_ $ mapM onStm stms) scope
    onStm (Let pat aux e) = addStm . Let pat aux =<< onExp e

    onOp (SegOp op)
      | SegThread v Nothing <- segLevel op = do
          lvl <- mkThreadGrid (segSpace op) v
          pure $ SegOp $ setSegLevel op $ SegThread v $ Just lvl
      | SegBlock v Nothing <- segLevel op = do
          -- XXX: we are assuming no irregular parallelism here, as these sizes
          -- may not be in-scope. This is true for how intrablock flattening
          -- currently works, but may not be the case in the future.
          let dims = map (product . map pe64) $ onlyNonVirt $ segOpParallelism $ segBody op
          lvl <- mkBlockGrid (segSpace op) v dims
          pure $ SegOp $ setSegLevel op $ SegBlock v $ Just lvl
    onOp op = pure op

    onBody (Body () stms res) =
      Body () <$> collectStms_ (mapM onStm stms) <*> pure res

    onExp =
      mapExpM
        (identityMapper @GPU)
          { mapOnOp = onOp,
            mapOnBody = \scope -> localScope scope . onBody
          }
