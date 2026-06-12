-- | Various definitions used for both forward and reverse mode.
module Futhark.AD.Shared
  ( auxPerm,
    asVName,
    mapNest,
    mkMap,
  )
where

import Control.Monad
import Data.Foldable
import Futhark.Construct
import Futhark.IR.SOACS

auxPerm :: Shape -> Type -> [Int]
auxPerm aux_shape t =
  [shapeRank aux_shape]
    ++ [0 .. shapeRank aux_shape - 1]
    ++ [shapeRank aux_shape + 1 .. arrayRank t - 1]

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
        fmap (subExpsRes . pure) . letSubExp "mapnest_res"
          =<< f (fmap (Var . paramName) x_p)
      Op . Screma w (toList x_v) <$> mapSOAC lam

-- | Construct a map over the given arrays, which must have the provided outer
-- shape. The purpose of the 'Shape' argument is to handle the case where no
-- arrays are provided.
mkMap ::
  (MonadBuilder m, Rep m ~ SOACS, Traversable f) =>
  Name ->
  Shape ->
  f VName ->
  -- | Action for building the body, passed names
  -- corresponding to elements of the arrays.
  (f VName -> m [VName]) ->
  m [VName]
mkMap desc shape arrs f = do
  let w = shapeSize 0 shape
  x_p <- traverse (newParam "xp" . rowType <=< lookupType) arrs
  lam <- mkLambda (toList x_p) $ varsRes <$> f (fmap paramName x_p)
  letTupExp desc . Op . Screma w (toList arrs) =<< mapSOAC lam
