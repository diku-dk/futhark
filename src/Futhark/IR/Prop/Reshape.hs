-- | Facilities for creating, inspecting, and simplifying reshape and
-- coercion operations.
module Futhark.IR.Prop.Reshape
  ( -- * Construction
    shapeCoerce,
    reshapeAll,
    reshapeCoerce,

    -- * Execution
    reshapeOuter,
    reshapeInner,
    newshapeInner,
    applySplice,

    -- * Simplification
    flipReshapeRearrange,
    flipRearrangeReshape,
    simplifyNewShape,

    -- * Shape calculations
    reshapeIndex,
    flattenIndex,
    unflattenIndex,
    sliceSizes,

    -- * Analysis
    ReshapeKind (..),
    reshapeKind,
    newShape,
  )
where

import Control.Monad (guard, mplus)
import Data.Foldable
import Data.List qualified as L
import Data.Maybe
import Data.Ord (comparing)
import Futhark.IR.Prop.Rearrange (isMapTranspose, rearrangeInverse, rearrangeShape)
import Futhark.IR.Syntax
import Futhark.Util (focusNth, mapAccumLM, takeLast)
import Futhark.Util.IntegralExp
import Prelude hiding (product, quot, sum)

-- | Construct a 'NewShape' that completely reshapes the initial shape.
reshapeAll :: (ArrayShape old) => old -> ShapeBase new -> NewShape new
reshapeAll old new = NewShape [DimSplice 0 (shapeRank old) new] new

-- | Construct a 'NewShape' that coerces the shape.
reshapeCoerce :: ShapeBase new -> NewShape new
reshapeCoerce shape = NewShape (zipWith dim (shapeDims shape) [0 ..]) shape
  where
    dim x i = DimSplice i 1 $ Shape [x]

-- | Construct a 'Reshape' that is a 'ReshapeCoerce'.
shapeCoerce :: [SubExp] -> VName -> Exp rep
shapeCoerce newdims arr =
  BasicOp $ Reshape arr (reshapeCoerce (Shape newdims))

-- | @reshapeOuter newshape n oldshape@ returns a 'Reshape' expression
-- that replaces the outer @n@ dimensions of @oldshape@ with @newshape@.
reshapeOuter :: Shape -> Int -> Shape -> Shape
reshapeOuter newshape n oldshape =
  newshape <> Shape (drop n (shapeDims oldshape))

-- | @reshapeInner newshape n oldshape@ produces a shape that replaces the inner
-- @m-n@ dimensions (where @m@ is the rank of @oldshape@) of @src@ with
-- @newshape@.
reshapeInner :: Shape -> Int -> Shape -> Shape
reshapeInner newshape n oldshape =
  Shape (take n (shapeDims oldshape)) <> newshape

-- | @newshapeInner outershape newshape@ bumps all the dimensions in @newshape@
-- by the rank of @outershape@, essentially making them operate on the inner
-- dimensions of a larger array, and also updates the shape of @newshape@ to
-- have @outershape@ outermost.
newshapeInner :: Shape -> NewShape SubExp -> NewShape SubExp
newshapeInner outershape (NewShape ss oldshape) =
  NewShape (map f ss) (outershape <> oldshape)
  where
    r = shapeRank outershape
    f (DimSplice i k shape) = DimSplice (r + i) k shape

-- | @reshapeIndex to_dims from_dims is@ transforms the index list
-- @is@ (which is into an array of shape @from_dims@) into an index
-- list @is'@, which is into an array of shape @to_dims@.  @is@ must
-- have the same length as @from_dims@, and @is'@ will have the same
-- length as @to_dims@.
reshapeIndex ::
  (IntegralExp num) =>
  [num] ->
  [num] ->
  [num] ->
  [num]
reshapeIndex to_dims from_dims is =
  unflattenIndex to_dims $ flattenIndex from_dims is

-- | @unflattenIndex dims i@ computes a list of indices into an array
-- with dimension @dims@ given the flat index @i@.  The resulting list
-- will have the same size as @dims@.
unflattenIndex ::
  (IntegralExp num) =>
  [num] ->
  num ->
  [num]
unflattenIndex = unflattenIndexFromSlices . drop 1 . sliceSizes

unflattenIndexFromSlices ::
  (IntegralExp num) =>
  [num] ->
  num ->
  [num]
unflattenIndexFromSlices [] _ = []
unflattenIndexFromSlices (size : slices) i =
  (i `quot` size) : unflattenIndexFromSlices slices (i - (i `quot` size) * size)

-- | @flattenIndex dims is@ computes the flat index of @is@ into an
-- array with dimensions @dims@.  The length of @dims@ and @is@ must
-- be the same.
flattenIndex ::
  (IntegralExp num) =>
  [num] ->
  [num] ->
  num
flattenIndex dims is
  | length is /= length slicesizes = error "flattenIndex: length mismatch"
  | otherwise = sum $ zipWith (*) is slicesizes
  where
    slicesizes = drop 1 $ sliceSizes dims

-- | Given a length @n@ list of dimensions @dims@, @sizeSizes dims@
-- will compute a length @n+1@ list of the size of each possible array
-- slice.  The first element of this list will be the product of
-- @dims@, and the last element will be 1.
sliceSizes ::
  (IntegralExp num) =>
  [num] ->
  [num]
sliceSizes [] = [1]
sliceSizes (n : ns) =
  product (n : ns) : sliceSizes ns

{- HLINT ignore sliceSizes -}

-- | Interchange a reshape and rearrange. Essentially, rewrite composition
--
-- @
-- let v1 = reshape(v1_shape, v0)
-- let v2 = rearrange(perm, v1)
-- @
--
-- into
--
-- @
-- let v1' = rearrange(perm', v0)
-- let v2' = reshape(v1_shape', v1')
--
-- The function is given the shape of @v0@, @v1@, and the @perm@, and returns
-- @perm'@. This is a meaningful operation when @v2@ is itself reshaped, as the
-- reshape-reshape can be fused. This can significantly simplify long chains of
-- reshapes and rearranges.
flipReshapeRearrange ::
  (Eq d) =>
  [d] ->
  [d] ->
  [Int] ->
  Maybe [Int]
flipReshapeRearrange v0_shape v1_shape perm = do
  (num_map_dims, num_a_dims, num_b_dims) <- isMapTranspose perm
  guard $ num_a_dims == 1
  guard $ num_b_dims == 1
  let map_dims = take num_map_dims v0_shape
      num_b_dims_expanded = length v0_shape - num_map_dims - num_a_dims
      num_a_dims_expanded = length v0_shape - num_map_dims - num_b_dims
      caseA = do
        guard $ take num_a_dims v0_shape == take num_b_dims v1_shape
        let perm' =
              [0 .. num_map_dims - 1]
                ++ map (+ num_map_dims) ([1 .. num_b_dims_expanded] ++ [0])
        Just perm'
      caseB = do
        guard $ takeLast num_b_dims v0_shape == takeLast num_b_dims v1_shape
        let perm' =
              [0 .. num_map_dims - 1]
                ++ map
                  (+ num_map_dims)
                  (num_a_dims_expanded : [0 .. num_a_dims_expanded - 1])
        Just perm'

  guard $ map_dims == take num_map_dims v1_shape

  caseA `mplus` caseB

flipRearrangeReshape :: [Int] -> NewShape d -> Maybe (NewShape d, [Int])
flipRearrangeReshape orig_perm (NewShape ss shape) = do
  (perm', ss') <- mapAccumLM f orig_perm ss
  let shape' = Shape $ rearrangeShape (rearrangeInverse perm') (shapeDims shape)
  Just (NewShape ss' shape', perm')
  where
    f perm (DimSplice i 1 s) = do
      (perm_bef, j, perm_aft) <- focusNth i perm
      let adj l = if l > j then l + length s - 1 else l
      Just
        ( map adj perm_bef ++ [j .. j + length s - 1] ++ map adj perm_aft,
          DimSplice j 1 s
        )
    f _ _ = Nothing

-- | Which kind of reshape is this?
data ReshapeKind
  = -- | New shape is dynamically same as original.
    ReshapeCoerce
  | -- | Any kind of reshaping.
    ReshapeArbitrary
  deriving (Eq, Ord, Show)

reshapeKind :: NewShape SubExp -> ReshapeKind
reshapeKind (NewShape ss _)
  | all unit $ zip (L.sortBy (comparing dim) ss) [0 ..] = ReshapeCoerce
  | otherwise = ReshapeArbitrary
  where
    unit (DimSplice j 1 (Shape [_]), i) = i == j
    unit _ = False
    dim (DimSplice j _ _) = j

-- | Apply the splice to a shape.
applySplice :: ShapeBase d -> DimSplice d -> ShapeBase d
applySplice shape_bef (DimSplice i k shape) =
  takeDims i shape_bef <> shape <> stripDims (i + k) shape_bef

-- | @dimSpan i n s@ gets @n@ dimensions starting from @i@ from @s@.
dimSpan :: Int -> Int -> ShapeBase d -> ShapeBase d
dimSpan i n = takeDims n . dropDims i

next ::
  (Eq d) =>
  ShapeBase d ->
  DimSplice d ->
  DimSplice d ->
  [DimSplice d] ->
  Maybe [DimSplice d]
next shape x y ss =
  (x :) <$> move (applySplice shape x, y) ss

move ::
  (Eq d) =>
  (ShapeBase d, DimSplice d) ->
  [DimSplice d] ->
  Maybe [DimSplice d]
--
-- A coercion that does not do anything.
move (shape_bef, DimSplice i1 n1 shape) ss
  | dimSpan i1 n1 shape_bef == shape =
      Just ss
--
-- See if we can find some redundancy.
move (shape, DimSplice i1 n1 s1) ss
  -- Check for redundant prefix.
  | match <-
      takeWhile (uncurry (==)) $
        zip (shapeDims (dimSpan i1 n1 shape)) (shapeDims s1),
    not $ null match,
    length match /= n1 =
      let k = length match
       in Just $ DimSplice (i1 + k) (n1 - k) (dropDims k s1) : ss
  -- Check for redundant suffix.
  | match <-
      takeWhile (uncurry (==)) $
        zip
          (reverse (shapeDims (dimSpan i1 n1 shape)))
          (reverse (shapeDims s1)),
    not $ null match,
    length match /= n1 =
      let k = length match
       in Just $ DimSplice i1 (n1 - k) (takeDims (length s1 - k) s1) : ss
--
-- Base case.
move _ [] = Nothing
--
-- A coercion can be fused with anything.
move (_, DimSplice i1 1 (Shape [_])) (DimSplice i2 n2 s2 : ss)
  | i1 == i2 =
      Just $ DimSplice i2 n2 s2 : ss
--
-- A flatten with an inverse unflatten turns into nothing.
move (shape_bef, DimSplice i1 n1 _s1) (DimSplice i2 _n2 s2 : ss)
  | i1 == i2,
    dimSpan i1 n1 shape_bef == s2 =
      Just ss
--
-- An unflatten where one of the dimensions is then further unflattened.
move (_, DimSplice i1 n1 s1) (DimSplice i2 n2 s2 : ss)
  | i2 >= i1,
    i2 < i1 + length s1,
    n1 == 1,
    n2 == 1 =
      Just $ DimSplice i1 1 (s1_bef <> s2 <> s1_aft) : ss
  where
    s1_bef = takeDims (i2 - i1) s1
    s1_aft = dropDims (i2 - i1 + 1) s1

--
-- Flatten followed by a flattening of overlapping dimensions.
move (_, DimSplice i1 n1 s1) (DimSplice i2 n2 s2 : ss)
  | length s1 == 1,
    length s2 == 1,
    i1 == i2 + 1,
    n2 > 1 =
      Just $ DimSplice i2 (n1 + n1) s2 : ss
--
-- Flatten into an unflatten.
move (_, DimSplice i1 n1 (Shape [_])) (DimSplice i2 1 s2 : ss)
  | i1 == i2 =
      Just $ DimSplice i1 n1 s2 : ss
--
-- These cases are for updating dimensions as we move across intervening
-- operations.
move (shape, DimSplice i1 n1 s1) (DimSplice i2 n2 s2 : ss)
  | i1 > i2 + n2 =
      next shape (DimSplice i2 n2 s2) (DimSplice (i1 - n2 + length s2) n1 s1) ss
  | i2 > i1 + n1 =
      next shape (DimSplice (i2 - n1 + length s1) n2 s2) (DimSplice i1 n2 s1) ss
  | otherwise = Nothing

-- | This is a quadratic-time function that looks for a DimSplice that can be
-- combined with a move DimSlice (and then does so). Since these lists are
-- usually small, this should not be a problem. It is called to convergence by
-- 'improve'.
improveOne :: (Eq d) => ShapeBase d -> [DimSplice d] -> Maybe [DimSplice d]
improveOne _ [] = Nothing
improveOne shape (s : ss) =
  move (shape, s) ss `mplus` ((s :) <$> improveOne (applySplice shape s) ss)

-- | Try to simplify the given 'NewShape'. Returns 'Nothing' if no improvement
-- is possible.
simplifyNewShape :: (Eq d) => ShapeBase d -> NewShape d -> Maybe (NewShape d)
simplifyNewShape shape_bef (NewShape ss shape) = do
  NewShape <$> (improve <$> improveOne shape_bef ss) <*> pure shape
  where
    improve ss' = maybe ss' improve $ improveOne shape_bef ss'

{-# NOINLINE flipReshapeRearrange #-}

{-# NOINLINE flipRearrangeReshape #-}

{-# NOINLINE reshapeKind #-}

{-# NOINLINE simplifyNewShape #-}

{-# NOINLINE newshapeInner #-}
