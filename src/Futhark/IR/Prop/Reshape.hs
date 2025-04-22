-- | Facilities for creating, inspecting, and simplifying reshape and
-- coercion operations.
module Futhark.IR.Prop.Reshape
  ( -- * Construction
    shapeCoerce,

    -- * Execution
    reshapeOuter,
    reshapeInner,

    -- * Simplification
    flipReshapeRearrange,

    -- * Shape calculations
    reshapeIndex,
    flattenIndex,
    unflattenIndex,
    sliceSizes,
  )
where

import Control.Monad (guard, mplus)
import Data.Foldable
import Futhark.IR.Prop.Rearrange (isMapTranspose)
import Futhark.IR.Syntax
import Futhark.Util (takeLast)
import Futhark.Util.IntegralExp
import Prelude hiding (product, quot, sum)

-- | Construct a 'Reshape' that is a 'ReshapeCoerce'.
shapeCoerce :: [SubExp] -> VName -> Exp rep
shapeCoerce newdims arr =
  BasicOp $ Reshape ReshapeCoerce (Shape newdims) arr

-- | @reshapeOuter newshape n oldshape@ returns a 'Reshape' expression
-- that replaces the outer @n@ dimensions of @oldshape@ with @newshape@.
reshapeOuter :: Shape -> Int -> Shape -> Shape
reshapeOuter newshape n oldshape =
  newshape <> Shape (drop n (shapeDims oldshape))

-- | @reshapeInner newshape n oldshape@ returns a 'Reshape' expression
-- that replaces the inner @m-n@ dimensions (where @m@ is the rank of
-- @oldshape@) of @src@ with @newshape@.
reshapeInner :: Shape -> Int -> Shape -> Shape
reshapeInner newshape n oldshape =
  Shape (take n (shapeDims oldshape)) <> newshape

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
{-# NOINLINE flipReshapeRearrange #-}
