module Futhark.Representation.AST.Attributes.Reshape
       (
         -- * Basic tools
         newDim
       , newDims
       , newShape

         -- * Construction
       , shapeCoerce

         -- * Execution
       , reshapeOuter
       , reshapeInner

         -- * Inspection
       , shapeCoercion

         -- * Simplification
       , fuseReshape
       , fuseReshapes
       , informReshape
       )
       where

import Data.Foldable

import Prelude

import Futhark.Representation.AST.Syntax

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
shapeCoerce :: Certificates -> [SubExp] -> VName -> Exp lore
shapeCoerce cs newdims arr =
  PrimOp $ Reshape cs (map DimCoercion newdims) arr

-- | @reshapeOuter newshape n oldshape@ returns a 'Reshape' expression
-- that replaces the outer @n@ dimensions of @oldshape@ with @shape@.
reshapeOuter :: ShapeChange SubExp -> Int -> Shape -> ShapeChange SubExp
reshapeOuter newshape n oldshape =
  newshape ++ map DimCoercion (drop n (shapeDims oldshape))

-- | @reshapeInner newshape n oldshape@ returns a 'Reshape' expression
-- that replaces the inner @m-n@ dimensions (where @m@ is the rank of
-- @oldshape@) of @src@ with @newshape@.
reshapeInner :: ShapeChange SubExp -> Int -> Shape -> ShapeChange SubExp
reshapeInner newshape n oldshape =
  map DimCoercion (take n (shapeDims oldshape)) ++ newshape

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
