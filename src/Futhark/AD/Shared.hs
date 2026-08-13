-- | Various definitions used for both forward and reverse mode.
module Futhark.AD.Shared
  ( vecPerm,
    asVName,
    mapNest,
    mkMap,
    accAddLambda,
  )
where

import Control.Monad
import Data.Bifunctor (bimap)
import Data.Foldable
import Futhark.Construct
import Futhark.IR.SOACS
import Futhark.Tools

-- | A permutation for transposing the vector shape past the next dimension.
--
-- That is, converts @[vec...][d][elem...]@ to @[d][vec...][elem...]@.
vecPerm :: (ArrayShape s) => Shape -> TypeBase s u -> [Int]
vecPerm vec_shape t =
  [shapeRank vec_shape]
    ++ [0 .. shapeRank vec_shape - 1]
    ++ [shapeRank vec_shape + 1 .. arrayRank t - 1]

asVName :: (MonadBuilder m) => SubExp -> m VName
asVName (Var v) = pure v
asVName (Constant x) = letExp "asv" $ BasicOp $ SubExp $ Constant x

-- | An addition operator for an accumulator whose index space has the given
-- rank and whose elements have the given types.  The operators for the
-- individual element types are fused horizontally, as an accumulator operator
-- must handle all of them at once.
accAddLambda ::
  (MonadBuilder m, Rep m ~ SOACS) =>
  Int ->
  [Type] ->
  m (Lambda SOACS)
accAddLambda n ts = do
  lams <- mapM addLambda ts
  idx_params <- replicateM n $ newParam "idx" $ Prim int64
  let (xs, ys) = bimap concat concat $ unzip $ map (splitAt 1 . lambdaParams) lams
  mkLambda (idx_params <> xs <> ys) $
    mconcat <$> mapM (bodyBind . lambdaBody) lams

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
