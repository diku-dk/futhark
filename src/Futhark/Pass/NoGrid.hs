-- | Remove grid information from all SegOp operations - the idea is to remove
-- all GPU-specific information from a GPU IR program.
module Futhark.Pass.NoGrid (noGrid) where

import Control.Monad.Identity
import Futhark.IR.GPU
import Futhark.Pass

noGrid :: Pass GPU GPU
noGrid =
  Pass "no-grid" "remove grid info from SegOps" $
    intraproceduralTransformation optimise
  where
    optimise _scope stms = pure $ fmap onStm stms
    onStm (Let pat aux e) =
      Let pat aux $
        mapExp
          (identityMapper @GPU)
            { mapOnOp = pure . onOp,
              mapOnBody = const $ pure . onBody,
              mapOnFParam = pure
            }
          e
    onBody body = body {bodyStms = fmap onStm (bodyStms body)}

    onLevel (SegThread v _) = SegThread v Nothing
    onLevel (SegBlock v _) = SegBlock v Nothing
    onLevel (SegThreadInBlock v) = SegThreadInBlock v

    onOp (SegOp op) =
      SegOp . runIdentity $
        mapSegOpM
          (identitySegOpMapper {mapOnSegOpLevel = pure . onLevel})
          op
    onOp op = op
