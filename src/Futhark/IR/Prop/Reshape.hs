-- | Facilities for creating, inspecting, and simplifying reshape and
-- coercion operations.
module Futhark.IR.Prop.Reshape
  ( -- * Construction
    shapeCoerce,

    -- * Execution
    reshapeOuter,
    reshapeInner,

    -- * Simplification

    -- * Shape calculations
    reshapeIndex,
    flattenIndex,
    unflattenIndex,
    sliceSizes,
  )
where

import Data.Foldable
import Futhark.IR.Syntax
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
