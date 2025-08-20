-- | Various definitions used for both forward and reverse mode.
module Futhark.AD.Shared (asVName, mapNest) where

import Control.Monad
import Data.Foldable
import Futhark.Construct
import Futhark.IR.SOACS

asVName :: (MonadBuilder m) => SubExp -> m VName
asVName (Var v) = pure v
asVName (Constant x) = letExp "asv" $ BasicOp $ SubExp $ Constant x

mapNest ::
  (MonadBuilder m, Rep m ~ SOACS, Traversable f) =>
  Shape ->
  f SubExp ->
  (f SubExp -> m (Exp SOACS)) ->
  m (Exp SOACS)
mapNest shape x f = do
  if shape == mempty
    then f x
    else do
      let w = shapeSize 0 shape
      x_v <- traverse asVName x
      x_p <- traverse (newParam "xp" . rowType <=< lookupType) x_v
      lam <- mkLambda (toList x_p) $ do
        fmap (subExpsRes . pure) . letSubExp "tan"
          =<< f (fmap (Var . paramName) x_p)
      pure $ Op $ Screma w (toList x_v) (mapSOAC lam)
