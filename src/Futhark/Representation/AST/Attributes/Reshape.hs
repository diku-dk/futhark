module Futhark.Representation.AST.Attributes.Reshape
       (
         -- * Basic tools
         newDim
       , newDims
       , newShape

         -- * Construction
       , shapeCoerce
       , repeatShapes

         -- * Execution
       , reshapeOuter
       , reshapeInner
       , repeatDims

         -- * Inspection
       , shapeCoercion

         -- * Simplification
       , fuseReshape
       , fuseReshapes
       , informReshape

         -- * Shape calculations
       , reshapeIndex
       , flattenIndex
       , unflattenIndex
       , sliceSizes
       )
       where

import Data.Foldable

import Prelude hiding (sum, product, quot)

import Futhark.Representation.AST.Attributes.Types
import Futhark.Representation.AST.Syntax
import Futhark.Util.IntegralExp

-- | The new dimension.
newDim :: DimChange d -> d
newDim (DimCoercion se) = se
newDim (DimNew      se) = se

-- | The new dimensions resulting from a reshape operation.
newDims :: ShapeChange d -> [d]
newDims = map newDim

-- | The new shape resulting from a reshape operation.
newShape :: ShapeChange SubExp -> Shape
newShape = Shape . newDims

-- ^ Construct a 'Reshape' where all dimension changes are
-- 'DimCoercion's.
shapeCoerce :: [SubExp] -> VName -> Exp lore
shapeCoerce newdims arr =
  BasicOp $ Reshape (map DimCoercion newdims) arr

-- | Construct a pair suitable for a 'Repeat'.
repeatShapes :: [Shape] -> Type -> ([Shape], Shape)
repeatShapes shapes t =
  case splitAt t_rank shapes of
    (outer_shapes, [inner_shape]) ->
      (outer_shapes, inner_shape)
    _ ->
      (shapes ++ replicate (length shapes - t_rank) (Shape []), Shape [])
  where t_rank = arrayRank t

-- | Modify the shape of an array type as 'Repeat' would do
repeatDims :: [Shape] -> Shape -> Type -> Type
repeatDims shape innershape = modifyArrayShape repeatDims'
  where repeatDims' (Shape ds) =
          Shape $ concat (zipWith (++) (map shapeDims shape) (map pure ds)) ++
          shapeDims innershape

-- | @reshapeOuter newshape n oldshape@ returns a 'Reshape' expression
-- that replaces the outer @n@ dimensions of @oldshape@ with @newshape@.
reshapeOuter :: ShapeChange SubExp -> Int -> Shape -> ShapeChange SubExp
reshapeOuter newshape n oldshape =
  newshape ++ map coercion_or_new (drop n (shapeDims oldshape))
  where coercion_or_new
          | length newshape == n = DimCoercion
          | otherwise            = DimNew

-- | @reshapeInner newshape n oldshape@ returns a 'Reshape' expression
-- that replaces the inner @m-n@ dimensions (where @m@ is the rank of
-- @oldshape@) of @src@ with @newshape@.
reshapeInner :: ShapeChange SubExp -> Int -> Shape -> ShapeChange SubExp
reshapeInner newshape n oldshape =
  map coercion_or_new (take n (shapeDims oldshape)) ++ newshape
  where coercion_or_new
          | length newshape == m-n = DimCoercion
          | otherwise              = DimNew
        m = shapeRank oldshape

-- | If the shape change is nothing but shape coercions, return the new dimensions.  Otherwise, return
-- 'Nothing'.
shapeCoercion :: ShapeChange d -> Maybe [d]
shapeCoercion = mapM dimCoercion
  where dimCoercion (DimCoercion d) = Just d
        dimCoercion (DimNew      _) = Nothing

-- | @fuseReshape s1 s2@ creates a new 'ShapeChange' that is
-- semantically the same as first applying @s1@ and then @s2@.  This
-- may take advantage of properties of 'DimCoercion' versus 'DimNew'
-- to preserve information.
fuseReshape :: Eq d => ShapeChange d -> ShapeChange d -> ShapeChange d
fuseReshape s1 s2
  | length s1 == length s2 =
      zipWith comb s1 s2
  where comb (DimNew _)       (DimCoercion d2) =
          DimNew d2
        comb (DimCoercion d1) (DimNew d2)
          | d1 == d2  = DimCoercion d2
          | otherwise = DimNew d2
        comb _                d2 =
          d2
-- TODO: intelligently handle case where s1 is a prefix of s2.
fuseReshape _ s2 = s2

-- | @fuseReshapes s ss@ creates a fused 'ShapeChange' that is
-- logically the same as first applying @s@ and then the changes in
-- @ss@ from left to right.
fuseReshapes :: (Eq d, Data.Foldable.Foldable t) =>
                ShapeChange d -> t (ShapeChange d) -> ShapeChange d
fuseReshapes = Data.Foldable.foldl fuseReshape

-- | Given concrete information about the shape of the source array,
-- convert some 'DimNew's into 'DimCoercion's.
informReshape :: Eq d => [d] -> ShapeChange d -> ShapeChange d
informReshape shape sc
  | length shape == length sc =
    zipWith inform shape sc
  where inform d1 (DimNew d2)
          | d1 == d2  = DimCoercion d2
        inform _ dc =
          dc
informReshape _ sc = sc

-- | @reshapeIndex to_dims from_dims is@ transforms the index list
-- @is@ (which is into an array of shape @from_dims@) into an index
-- list @is'@, which is into an array of shape @to_dims@.  @is@ must
-- have the same length as @from_dims@, and @is'@ will have the same
-- length as @to_dims@.
reshapeIndex :: IntegralExp num =>
                [num] -> [num] -> [num] -> [num]
reshapeIndex to_dims from_dims is =
  unflattenIndex to_dims $ flattenIndex from_dims is

-- | @unflattenIndex dims i@ computes a list of indices into an array
-- with dimension @dims@ given the flat index @i@.  The resulting list
-- will have the same size as @dims@.
unflattenIndex :: IntegralExp num =>
                  [num] -> num -> [num]
unflattenIndex = unflattenIndexFromSlices . drop 1 . sliceSizes

unflattenIndexFromSlices :: IntegralExp num =>
                            [num] -> num -> [num]
unflattenIndexFromSlices [] _ = []
unflattenIndexFromSlices (size : slices) i =
  (i `quot` size) : unflattenIndexFromSlices slices (i - (i `quot` size) * size)

-- | @flattenIndex dims is@ computes the flat index of @is@ into an
-- array with dimensions @dims@.  The length of @dims@ and @is@ must
-- be the same.
flattenIndex :: IntegralExp num =>
                [num] -> [num] -> num
flattenIndex dims is =
  sum $ zipWith (*) is slicesizes
  where slicesizes = drop 1 $ sliceSizes dims

-- | Given a length @n@ list of dimensions @dims@, @sizeSizes dims@
-- will compute a length @n+1@ list of the size of each possible array
-- slice.  The first element of this list will be the product of
-- @dims@, and the last element will be 1.
sliceSizes :: IntegralExp num =>
              [num] -> [num]
sliceSizes [] = [1]
sliceSizes (n:ns) =
  product (n : ns) : sliceSizes ns

{- HLINT ignore sliceSizes -}
