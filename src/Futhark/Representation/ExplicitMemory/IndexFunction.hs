-- | An index function represents a mapping from an array index space
-- to a flat byte offset.
module Futhark.Representation.ExplicitMemory.IndexFunction
       (
         IxFun
       , index
       , iota
       , offsetIndex
       , strideIndex
       , permute
       , rotate
       , reshape
       , slice
       , base
       , rebase
       , shape
       , rank
       , linearWithOffset
       , rearrangeWithOffset
       , isDirect
       )
       where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.List

import Prelude hiding (div, mod, quot, rem)

import Futhark.Transform.Substitute
import Futhark.Transform.Rename

import Futhark.Representation.AST.Syntax
  (ShapeChange, DimIndex(..), Slice, sliceDims)
import Futhark.Representation.AST.Attributes.Names
import Futhark.Representation.AST.Attributes.Reshape
import Futhark.Representation.AST.Attributes.Rearrange
import Futhark.Representation.AST.Pretty ()
import Futhark.Util.IntegralExp
import Futhark.Util.Pretty

type Shape num = [num]
type Indices num = [num]
type Permutation = [Int]

data IxFun num = Direct (Shape num)
               | Permute (IxFun num) Permutation
               | Rotate (IxFun num) (Indices num)
               | Index (IxFun num) (Slice num)
               | Reshape (IxFun num) (ShapeChange num)
               | Stride (IxFun num) num

--- XXX: this is almost just structural equality, which may be too
--- conservative - unless we normalise first, somehow.
instance (IntegralExp num, Eq num) => Eq (IxFun num) where
  Direct _ == Direct _ =
    True
  Permute ixfun1 perm1 == Permute ixfun2 perm2 =
    ixfun1 == ixfun2 && perm1 == perm2
  Rotate ixfun1 offsets1 == Rotate ixfun2 offsets2 =
    ixfun1 == ixfun2 && offsets1 == offsets2
  Index ixfun1 is1 == Index ixfun2 is2 =
    ixfun1 == ixfun2 && length is1 == length is2 && and (zipWith eqIndex is1 is2)
    -- Two DimSlices are considered equal even if their slice lengths
    -- are not equal, as this allows us to get rid of reshapes.
    where eqIndex (DimFix i) (DimFix j) = i == j
          eqIndex (DimSlice i _) (DimSlice j _) = i == j
          eqIndex _ _ = False
  Reshape ixfun1 shape1 == Reshape ixfun2 shape2 =
    ixfun1 == ixfun2 && length shape1 == length shape2
  _ == _ = False

instance Show num => Show (IxFun num) where
  show (Direct n) = "Direct (" ++ show n ++ ")"
  show (Permute fun perm) = "Permute (" ++ show fun ++ ", " ++ show perm ++ ")"
  show (Rotate fun offsets) = "Rotate (" ++ show fun ++ ", " ++ show offsets ++ ")"
  show (Index fun is) = "Index (" ++ show fun ++ ", " ++ show is ++ ")"
  show (Reshape fun newshape) = "Reshape (" ++ show fun ++ ", " ++ show newshape ++ ")"
  show (Stride fun stride) = "Stride (" ++ show fun ++ ", " ++ show stride ++ ")"

instance Pretty num => Pretty (IxFun num) where
  ppr (Direct dims) =
    text "Direct" <> parens (commasep $ map ppr dims)
  ppr (Permute fun perm) = ppr fun <> ppr perm
  ppr (Rotate fun offsets) = ppr fun <> brackets (commasep $ map ((text "+" <>) . ppr) offsets)
  ppr (Index fun is) = ppr fun <> brackets (commasep $ map ppr is)
  ppr (Reshape fun oldshape) =
    ppr fun <> text "->" <>
    parens (commasep (map ppr oldshape))
  ppr (Stride fun stride) =
    ppr fun <> text "*" <> ppr stride

instance (Eq num, IntegralExp num, Substitute num) => Substitute (IxFun num) where
  substituteNames _ (Direct n) =
    Direct n
  substituteNames subst (Permute fun perm) =
    Permute (substituteNames subst fun) perm
  substituteNames subst (Rotate fun offsets) =
    Rotate (substituteNames subst fun) offsets
  substituteNames subst (Index fun is) =
    slice
    (substituteNames subst fun)
    (map (substituteNames subst) is)
  substituteNames subst (Reshape fun newshape) =
    reshape
    (substituteNames subst fun)
    (map (substituteNames subst) newshape)
  substituteNames subst (Stride fun stride) =
    Stride
    (substituteNames subst fun)
    (substituteNames subst stride)

instance FreeIn num => FreeIn (IxFun num) where
  freeIn (Direct dims) = freeIn dims
  freeIn (Stride ixfun e) = freeIn ixfun <> freeIn e
  freeIn (Permute ixfun _) = freeIn ixfun
  freeIn (Rotate ixfun offsets) = freeIn ixfun <> freeIn offsets
  freeIn (Index ixfun is) =
    freeIn ixfun <> mconcat (map freeIn is)
  freeIn (Reshape ixfun dims) =
    freeIn ixfun <> freeIn dims

instance (Eq num, IntegralExp num, Substitute num, Rename num) => Rename (IxFun num) where
  rename = substituteRename

index :: (Pretty num, IntegralExp num) =>
         IxFun num -> Indices num -> num -> num

index (Direct dims) is element_size =
  sum (zipWith (*) is slicesizes) * element_size
  where slicesizes = drop 1 $ sliceSizes dims

index (Permute fun perm) is_new element_size =
  index fun is_old element_size
  where is_old = rearrangeShape (rearrangeInverse perm) is_new

index (Rotate fun offsets) is element_size =
  index fun (zipWith mod (zipWith (+) is offsets) dims) element_size
  where dims = shape fun

index (Index fun js) is element_size =
  index fun (adjust js is) element_size
  where adjust (DimFix j:js') is' = j : adjust js' is'
        adjust (DimSlice j _:js') (i:is') = j + i : adjust js' is'
        adjust _ _ = []

index (Reshape fun newshape) is element_size =
  let new_indices = reshapeIndex (shape fun) (newDims newshape) is
  in index fun new_indices element_size

index (Stride fun stride) (i:is) element_size =
  index fun (i * stride:is) element_size

index ixfun [] _ =
  error $ "Empty index list provided to " ++ pretty ixfun

iota :: IntegralExp num =>
        Shape num -> IxFun num
iota = Direct

offsetIndex :: (Eq num, IntegralExp num) =>
               IxFun num -> num -> IxFun num
offsetIndex ixfun i | i == 0 = ixfun
offsetIndex ixfun i =
  case dims of
    d:ds -> slice ixfun (DimSlice i (d-i) : map (DimSlice 0) ds)
    []   -> error "offsetIndex: underlying index function has rank zero"
  where dims = shape ixfun

strideIndex :: IntegralExp num =>
               IxFun num -> num -> IxFun num
strideIndex = Stride

permute :: IntegralExp num =>
           IxFun num -> Permutation -> IxFun num
permute (Permute ixfun oldperm) perm
  | rearrangeInverse oldperm == perm = ixfun
  | otherwise = permute ixfun (rearrangeCompose perm oldperm)
permute ixfun perm
  | perm == sort perm = ixfun
  | otherwise = Permute ixfun perm

rotate :: IntegralExp num =>
          IxFun num -> Indices num -> IxFun num
rotate (Rotate ixfun old_offsets) offsets =
  Rotate ixfun $ zipWith (+) old_offsets offsets
rotate ixfun offsets = Rotate ixfun offsets

reshape :: (Eq num, IntegralExp num) =>
           IxFun num -> ShapeChange num -> IxFun num

reshape Direct{} newshape =
  Direct $ map newDim newshape

reshape (Reshape ixfun _) newshape =
  reshape ixfun newshape

reshape ixfun newshape
  | shape ixfun == map newDim newshape =
      ixfun
  | rank ixfun == length newshape,
    Just _ <- shapeCoercion newshape =
      ixfun
  | otherwise =
      Reshape ixfun newshape

slice :: (Eq num, IntegralExp num) =>
         IxFun num -> Slice num -> IxFun num
slice ixfun is
  -- Avoid identity slicing.
  | is == map (DimSlice 0) (shape ixfun) = ixfun
slice (Index ixfun mis) is =
  Index ixfun $ reslice mis is
  where reslice mis' [] = mis'
        reslice (DimFix j:mis') is' =
          DimFix j : reslice mis' is'
        reslice (DimSlice orig_k _:mis') (DimSlice new_k n:is') =
          DimSlice (orig_k + new_k) n : reslice mis' is'
        reslice (DimSlice{}:mis') (DimFix i:is') =
          DimFix i : reslice mis' is'
        reslice _ _ = error "IndexFunction slice: invalid arguments"
slice ixfun [] = ixfun
slice ixfun is = Index ixfun is

rank :: IntegralExp num =>
        IxFun num -> Int
rank = length . shape

shape :: IntegralExp num =>
         IxFun num -> Shape num
shape (Direct dims) =
  dims
shape (Permute ixfun perm) =
  rearrangeShape perm $ shape ixfun
shape (Rotate ixfun _) =
  shape ixfun
shape (Index _ how) =
  sliceDims how
shape (Reshape _ dims) =
  map newDim dims
shape (Stride ixfun _) =
  shape ixfun

base :: IxFun num -> Shape num
base (Direct dims) =
  dims
base (Permute ixfun _) =
  base ixfun
base (Rotate ixfun _) =
  base ixfun
base (Index ixfun _) =
  base ixfun
base (Reshape ixfun _) =
  base ixfun
base (Stride ixfun _) =
  base ixfun

rebase :: (Eq num, IntegralExp num) =>
          IxFun num
       -> IxFun num
       -> IxFun num
rebase new_base (Direct _) =
  new_base
rebase new_base (Stride ixfun stride) =
  strideIndex (rebase new_base ixfun) stride
rebase new_base (Permute ixfun perm) =
  permute (rebase new_base ixfun) perm
rebase new_base (Rotate ixfun offsets) =
  rotate (rebase new_base ixfun) offsets
rebase new_base (Index ixfun is) =
  slice (rebase new_base ixfun) is
rebase new_base (Reshape ixfun new_shape) =
  reshape (rebase new_base ixfun) new_shape

-- This function does not cover all possible cases.  It's a "best
-- effort" kind of thing.
linearWithOffset :: (Eq num, IntegralExp num) =>
                    IxFun num -> num -> Maybe num
linearWithOffset (Direct _) _ =
  Just 0
linearWithOffset (Reshape ixfun _) element_size =
 linearWithOffset ixfun element_size
linearWithOffset (Index ixfun is) element_size = do
  is' <- fixingOuter is inner_shape
  inner_offset <- linearWithOffset ixfun element_size
  let slices = take m $ drop 1 $ sliceSizes $ shape ixfun
  return $ inner_offset + sum (zipWith (*) slices is') * element_size
  where m = length is
        inner_shape = shape ixfun
        fixingOuter (DimFix i:is') (_:ds) = (i:) <$> fixingOuter is' ds
        fixingOuter (DimSlice off _:is') ds
          | is' == map (DimSlice 0) ds = Just [off]
        fixingOuter is' ds
          | is' == map (DimSlice 0) ds = Just []
        fixingOuter _ _ = Nothing
linearWithOffset _ _ = Nothing

rearrangeWithOffset :: (Eq num, IntegralExp num) =>
                       IxFun num -> num -> Maybe (num, [(Int,num)])
rearrangeWithOffset (Reshape ixfun _) element_size =
  rearrangeWithOffset ixfun element_size
rearrangeWithOffset (Permute ixfun perm) element_size = do
  offset <- linearWithOffset ixfun element_size
  return (offset, zip perm $ rearrangeShape perm $ shape ixfun)
rearrangeWithOffset _ _ =
  Nothing

isDirect :: (Eq num, IntegralExp num) => IxFun num -> Bool
isDirect =
  maybe False (==0) . flip linearWithOffset 1
