-- | An index function represents a mapping from an array index space
-- to a flat byte offset.
module Futhark.Representation.ExplicitMemory.IndexFunction
       (
         IxFun(..)
       , index
       , iota
       , offsetIndex
       , strideIndex
       , permute
       , reshape
       , applyInd
       , base
       , rebase
       , shape
       , rank
       , linearWithOffset
       , rearrangeWithOffset
       , isDirect
       )
       where

import Data.Monoid

import Prelude hiding (div, quot)

import Futhark.Transform.Substitute
import Futhark.Transform.Rename

import Futhark.Representation.AST.Syntax (DimChange(..), ShapeChange)
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
               | Offset (IxFun num) num
               | Permute (IxFun num) Permutation
               | Index (IxFun num) (Indices num)
               | Reshape (IxFun num) (ShapeChange num)
               | Stride (IxFun num) num

--- XXX: this is almost just structural equality, which may be too
--- conservative - unless we normalise first, somehow.
instance (IntegralCond num, Eq num) => Eq (IxFun num) where
  Direct _ == Direct _ =
    True
  Offset ixfun1 offset1 == Offset ixfun2 offset2 =
    ixfun1 == ixfun2 && offset1 == offset2
  Permute ixfun1 perm1 == Permute ixfun2 perm2 =
    ixfun1 == ixfun2 && perm1 == perm2
  Index ixfun1 is1 == Index ixfun2 is2 =
    ixfun1 == ixfun2 && is1 == is2
  Reshape ixfun1 shape1 == Reshape ixfun2 shape2 =
    ixfun1 == ixfun2 && length shape1 == length shape2
  _ == _ = False

instance Show num => Show (IxFun num) where
  show (Direct n) = "Direct (" ++ show n ++ ")"
  show (Offset fun k) = "Offset (" ++ show fun ++ ", " ++ show k ++ ")"
  show (Permute fun perm) = "Permute (" ++ show fun ++ ", " ++ show perm ++ ")"
  show (Index fun is) = "Index (" ++ show fun ++ ", " ++ show is ++ ")"
  show (Reshape fun newshape) = "Reshape (" ++ show fun ++ ", " ++ show newshape ++ ")"
  show (Stride fun stride) = "Stride (" ++ show fun ++ ", " ++ show stride ++ ")"

instance Pretty num => Pretty (IxFun num) where
  ppr (Direct dims) =
    text "Direct" <> parens (commasep $ map ppr dims)
  ppr (Offset fun k) = ppr fun <+> text "+" <+> ppr k
  ppr (Permute fun perm) = ppr fun <> ppr perm
  ppr (Index fun is) = ppr fun <> brackets (commasep $ map ppr is)
  ppr (Reshape fun oldshape) =
    ppr fun <> text "->" <>
    parens (commasep (map ppr oldshape))
  ppr (Stride fun stride) =
    ppr fun <> text "*" <> ppr stride

instance (Eq num, IntegralCond num, Substitute num) => Substitute (IxFun num) where
  substituteNames _ (Direct n) =
    Direct n
  substituteNames subst (Offset fun k) =
    Offset (substituteNames subst fun) (substituteNames subst k)
  substituteNames subst (Permute fun perm) =
    Permute (substituteNames subst fun) perm
  substituteNames subst (Index fun is) =
    Index
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
  freeIn (Offset ixfun e) = freeIn ixfun <> freeIn e
  freeIn (Stride ixfun e) = freeIn ixfun <> freeIn e
  freeIn (Permute ixfun _) = freeIn ixfun
  freeIn (Index ixfun is) =
    freeIn ixfun <> mconcat (map freeIn is)
  freeIn (Reshape ixfun dims) =
    freeIn ixfun <> freeIn dims

instance (Eq num, IntegralCond num, Substitute num, Rename num) => Rename (IxFun num) where
  -- Because there is no mapM-like function on sized vectors, we
  -- implement renaming by retrieving the substitution map, then using
  -- 'substituteNames'.  This is safe as index functions do not
  -- contain their own bindings.
  rename ixfun = do
    subst <- renamerSubstitutions
    return $ substituteNames subst ixfun

index :: (Pretty num, IntegralCond num) =>
         IxFun num -> Indices num -> num -> num

index (Direct dims) is element_size =
  sum (zipWith (*) is slicesizes) * element_size
  where slicesizes = drop 1 $ sliceSizes dims

index (Offset fun offset) (i:is) element_size =
  index fun ((i + offset) : is) element_size

index (Permute fun perm) is_new element_size =
  index fun is_old element_size
  where is_old = rearrangeShape (rearrangeInverse perm) is_new

index (Index fun is1) is2 element_size =
  index fun (is1 ++ is2) element_size

index (Reshape fun newshape) is element_size =
  let new_indices = reshapeIndex (shape fun) (newDims newshape) is
  in index fun new_indices element_size

index (Stride fun stride) (i:is) element_size =
  index fun (i * stride:is) element_size

index ixfun [] _ =
  error $ "Empty index list provided to " ++ pretty ixfun

iota :: IntegralCond num =>
        Shape num -> IxFun num
iota = Direct

offsetIndex :: IntegralCond num =>
               IxFun num -> num -> IxFun num
offsetIndex = Offset

strideIndex :: IntegralCond num =>
               IxFun num -> num -> IxFun num
strideIndex = Stride

permute :: IntegralCond num =>
           IxFun num -> Permutation -> IxFun num
permute (Permute ixfun oldperm) perm
  | rearrangeInverse oldperm == perm = ixfun
permute ixfun perm = Permute ixfun perm

reshape :: (Eq num, IntegralCond num) =>
           IxFun num -> ShapeChange num -> IxFun num

reshape (Direct oldshape) newshape
  | length oldshape == length newshape =
      Direct $ map newDim newshape

reshape (Reshape ixfun _) newshape =
  reshape ixfun newshape

reshape ixfun newshape
  | shape ixfun == map newDim newshape =
      ixfun
  | rank ixfun == length newshape,
    Just _ <- shapeCoercion newshape =
      case ixfun of
        Permute ixfun' perm ->
          let ixfun'' = reshape ixfun' $
                rearrangeShape (rearrangeInverse perm) newshape
          in Permute ixfun'' perm

        Offset ixfun' offset ->
          Offset (reshape ixfun' newshape) offset

        Stride{} ->
          Reshape ixfun newshape

        Index ixfun' is ->
          let unchanged_shape =
                map DimCoercion $ take (length is) $ shape ixfun'
              newshape' = unchanged_shape ++ newshape
          in applyInd (reshape ixfun' newshape') is

        Reshape _ _ ->
          Reshape ixfun newshape

        Direct _ ->
          Direct $ map newDim newshape
  | otherwise =
      Reshape ixfun newshape

applyInd :: IntegralCond num =>
            IxFun num -> Indices num -> IxFun num
applyInd (Index ixfun mis) is =
  Index ixfun $ mis ++ is
applyInd ixfun [] = ixfun
applyInd ixfun is = Index ixfun is

rank :: IntegralCond num =>
        IxFun num -> Int
rank (Direct dims) = length dims
rank (Offset ixfun _) = rank ixfun
rank (Permute ixfun _) = rank ixfun
rank (Index ixfun is) = rank ixfun - length is
rank (Reshape _ newshape) = length newshape
rank (Stride ixfun _) = rank ixfun

shape :: IntegralCond num =>
         IxFun num -> Shape num
shape (Direct dims) =
  dims
shape (Permute ixfun perm) =
  rearrangeShape perm $ shape ixfun
shape (Offset ixfun _) =
  shape ixfun
shape (Index ixfun indices) =
  drop (length indices) $ shape ixfun
shape (Reshape _ dims) =
  map newDim dims
shape (Stride ixfun _) =
  shape ixfun

base :: IxFun num -> Shape num
base (Direct dims) =
  dims
base (Offset ixfun _) =
  base ixfun
base (Permute ixfun _) =
  base ixfun
base (Index ixfun _) =
  base ixfun
base (Reshape ixfun _) =
  base ixfun
base (Stride ixfun _) =
  base ixfun

rebase :: (Eq num, IntegralCond num) =>
          IxFun num
       -> IxFun num
       -> IxFun num
rebase new_base (Direct _) =
  new_base
rebase new_base (Offset ixfun o) =
  offsetIndex (rebase new_base ixfun) o
rebase new_base (Stride ixfun stride) =
  strideIndex (rebase new_base ixfun) stride
rebase new_base (Permute ixfun perm) =
  permute (rebase new_base ixfun) perm
rebase new_base (Index ixfun is) =
  applyInd (rebase new_base ixfun) is
rebase new_base (Reshape ixfun new_shape) =
  reshape (rebase new_base ixfun) new_shape

-- This function does not cover all possible cases.  It's a "best
-- effort" kind of thing.
linearWithOffset :: IntegralCond num =>
                    IxFun num -> num -> Maybe num
linearWithOffset (Direct _) _ =
  Just 0
linearWithOffset (Offset ixfun n) element_size = do
  inner_offset <- linearWithOffset ixfun element_size
  case drop 1 $ sliceSizes $ shape ixfun of
    [] -> Nothing
    rowslice : _ ->
      return $ inner_offset + (n * rowslice * element_size)
linearWithOffset (Reshape ixfun _) element_size =
 linearWithOffset ixfun element_size
linearWithOffset (Index ixfun is) element_size = do
  inner_offset <- linearWithOffset ixfun element_size
  let slices = take m $ drop 1 $ sliceSizes $ shape ixfun
  return $ inner_offset + sum (zipWith (*) slices is) * element_size
  where m = length is
linearWithOffset _ _ = Nothing

rearrangeWithOffset :: IntegralCond num =>
                       IxFun num -> num -> Maybe (num, [(Int,num)])
rearrangeWithOffset (Reshape ixfun _) element_size =
  rearrangeWithOffset ixfun element_size
rearrangeWithOffset (Permute ixfun perm) element_size = do
  offset <- linearWithOffset ixfun element_size
  return (offset, zip perm $ rearrangeShape perm $ shape ixfun)
rearrangeWithOffset _ _ =
  Nothing

isDirect :: (Eq num, IntegralCond num) => IxFun num -> Bool
isDirect =
  maybe False (==0) . flip linearWithOffset 1
