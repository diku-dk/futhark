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
       , rotate
       , reshape
       , slice
       , base
       , rebase
       , repeat
       , shape
       , rank
       , linearWithOffset
       , rearrangeWithOffset
       , isDirect
       , substituteInIxFun
       )
       where

import Control.Arrow (first)
import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.List hiding (repeat)
import Control.Monad.Identity
import Control.Monad.Writer

import Prelude hiding (div, mod, quot, rem, repeat)

import qualified Data.Map.Strict as M

import Futhark.Transform.Substitute
import Futhark.Transform.Rename

import Futhark.Representation.AST.Syntax
  (ShapeChange, DimChange(..), DimIndex(..), Slice, sliceDims, unitSlice, VName)
import Futhark.Representation.AST.Attributes.Names
import Futhark.Representation.AST.Attributes.Reshape
import Futhark.Representation.AST.Attributes.Rearrange
import Futhark.Representation.AST.Pretty ()
import Futhark.Util.IntegralExp
import Futhark.Util.Pretty
import Futhark.Util
import Futhark.Analysis.PrimExp.Convert

type Shape num = [num]
type Indices num = [num]
type Permutation = [Int]

data IxFun num = Direct (Shape num)
               | Permute (IxFun num) Permutation
               | Rotate (IxFun num) (Indices num)
               | Index (IxFun num) (Slice num)
               | Reshape (IxFun num) (ShapeChange num)
               | Repeat (IxFun num) [Shape num] (Shape num)
               deriving (Show)

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
          eqIndex (DimSlice i _ s0) (DimSlice j _ s1) = i==j && s0==s1
          eqIndex _ _ = False
  Reshape ixfun1 shape1 == Reshape ixfun2 shape2 =
    ixfun1 == ixfun2 && length shape1 == length shape2
  Repeat ixfun1 outer_shapes1 inner_shape1 == Repeat ixfun2 outer_shapes2 inner_shape2 =
    ixfun1 == ixfun2 && outer_shapes1 == outer_shapes2 && inner_shape1 == inner_shape2
  _ == _ = False

instance Pretty num => Pretty (IxFun num) where
  ppr (Direct dims) =
    text "Direct" <> parens (commasep $ map ppr dims)
  ppr (Permute fun perm) = ppr fun <> ppr perm
  ppr (Rotate fun offsets) = ppr fun <> brackets (commasep $ map ((text "+" <>) . ppr) offsets)
  ppr (Index fun is) = ppr fun <> brackets (commasep $ map ppr is)
  ppr (Reshape fun oldshape) =
    ppr fun <> text "->reshape" <>
    parens (commasep (map ppr oldshape))
  ppr (Repeat fun outer_shapes inner_shape) =
    ppr fun <> text "->repeat" <> parens (commasep (map ppr $ outer_shapes++ [inner_shape]))

instance (Eq num, IntegralExp num, Substitute num) => Substitute (IxFun num) where
  substituteNames substs = fmap $ substituteNames substs

instance FreeIn num => FreeIn (IxFun num) where
  freeIn = foldMap freeIn

instance Functor IxFun where
  fmap f = runIdentity . traverse (return . f)

instance Foldable IxFun where
  foldMap f = execWriter . traverse (tell . f)

instance Traversable IxFun where
  traverse f (Direct dims) =
    Direct <$> traverse f dims
  traverse f (Permute ixfun perm) =
    Permute <$> traverse f ixfun <*> pure perm
  traverse f (Rotate ixfun offsets) =
    Rotate <$> traverse f ixfun <*> traverse f offsets
  traverse f (Index ixfun is) =
    Index <$> traverse f ixfun <*> traverse (traverse f) is
  traverse f (Reshape ixfun dims) =
    Reshape <$> traverse f ixfun <*> traverse (traverse f) dims
  traverse f (Repeat ixfun outer_shapes inner_shape) =
    Repeat <$> traverse f ixfun <*>
    traverse (traverse f) outer_shapes <*>
    traverse f inner_shape

instance (Eq num, IntegralExp num, Substitute num) => Rename (IxFun num) where
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
        adjust (DimSlice j _ s:js') (i:is') = j + i * s : adjust js' is'
        adjust _ _ = []

index (Reshape fun newshape) is element_size =
  let new_indices = reshapeIndex (shape fun) (newDims newshape) is
  in index fun new_indices element_size

index (Repeat fun outer_shapes _) is element_size =
  -- Discard those indices that are just repeats.  It is intentional
  -- that we cut off those indices that correspond to the innermost
  -- repeated dimensions.
  index fun is' element_size
  where flags dims = replicate (length dims) True ++ [False]
        is' = map snd $ filter (not . fst) $ zip (concatMap flags outer_shapes) is

iota :: Shape num -> IxFun num
iota = Direct

offsetIndex :: (Eq num, IntegralExp num) =>
               IxFun num -> num -> IxFun num
offsetIndex ixfun i | i == 0 = ixfun
offsetIndex ixfun i =
  case shape ixfun of
    d:ds -> slice ixfun (DimSlice i (d-i) 1 : map (unitSlice 0) ds)
    []   -> error "offsetIndex: underlying index function has rank zero"

strideIndex :: (Eq num, IntegralExp num) =>
               IxFun num -> num -> IxFun num
strideIndex ixfun s =
  case shape ixfun of
    d:ds -> slice ixfun (DimSlice 0 d s : map (unitSlice 0) ds)
    []   -> error "offsetIndex: underlying index function has rank zero"

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

repeat :: (Eq num, IntegralExp num) =>
          IxFun num -> [Shape num] -> Shape num -> IxFun num
repeat = Repeat

reshape :: (Eq num, IntegralExp num) =>
           IxFun num -> ShapeChange num -> IxFun num

reshape Direct{} newshape =
  Direct $ map newDim newshape

reshape (Reshape ixfun _) newshape =
  reshape ixfun newshape

reshape (Permute ixfun perm) newshape
  | Just (head_coercions, reshapes, tail_coercions) <-
      splitCoercions newshape,
    num_coercions <- length (head_coercions ++ tail_coercions),
    (head_perms, mid_perms, end_perms) <-
      splitAt3 (length head_coercions) (length perm - num_coercions) perm,
    sequential mid_perms,
    first_reshaped <- foldl min (rank ixfun) mid_perms,
    extra_dims <- length newshape - length (shape ixfun),
    perm' <- map (shiftDim first_reshaped extra_dims) head_perms ++
             take (length reshapes) [first_reshaped..] ++
             map (shiftDim first_reshaped extra_dims) end_perms,
    newshape' <- rearrangeShape (rearrangeInverse perm') newshape =
      Permute (reshape ixfun newshape') perm'
  where splitCoercions newshape' = do
          let (head_coercions, newshape'') = span isCoercion newshape'
          let (reshapes, tail_coercions) = break isCoercion newshape''
          guard (all isCoercion tail_coercions)
          return (head_coercions, reshapes, tail_coercions)

        isCoercion DimCoercion{} = True
        isCoercion _ = False

        shiftDim last_reshaped extra_dims x
          | x > last_reshaped = x + extra_dims
          | otherwise = x

        sequential [] = True
        sequential (x:xs) = and $ zipWith (==) xs [x+1, x+2..]

reshape (Index ixfun slicing) newshape
  | (is, rem_slicing) <- splitSlice slicing,
    (fixed_ds, sliced_ds) <- splitAt (length is) $ shape ixfun,
    and $ zipWith isSliceOf rem_slicing sliced_ds =
      -- Move the reshape beneath the slicing.
      let newshape' = map DimCoercion fixed_ds ++ newshape
      in Index (reshape ixfun newshape') $
         map DimFix is ++ map (unitSlice 0) (newDims newshape)
  where isSliceOf (DimSlice _ d1 _) d2 = d1 == d2
        isSliceOf _ _ = False

reshape ixfun newshape
  | shape ixfun == map newDim newshape =
      ixfun
  | rank ixfun == length newshape,
    Just _ <- shapeCoercion newshape =
      ixfun
  | otherwise =
      Reshape ixfun newshape

splitSlice :: Slice num -> ([num], Slice num)
splitSlice [] = ([], [])
splitSlice (DimFix i:is) = first (i:) $ splitSlice is
splitSlice is = ([], is)

slice :: (Eq num, IntegralExp num) =>
         IxFun num -> Slice num -> IxFun num
slice ixfun is
  -- Avoid identity slicing.
  | is == map (unitSlice 0) (shape ixfun) = ixfun
slice (Index ixfun mis) is =
  Index ixfun $ reslice mis is
  where reslice mis' [] = mis'
        reslice (DimFix j:mis') is' =
          DimFix j : reslice mis' is'
        reslice (DimSlice orig_k _ orig_s:mis') (DimSlice new_k n new_s:is') =
          DimSlice (orig_k + new_k * orig_s) n (orig_s*new_s) : reslice mis' is'
        reslice (DimSlice orig_k _ orig_s:mis') (DimFix i:is') =
          DimFix (orig_k+i*orig_s) : reslice mis' is'
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
shape (Repeat ixfun outer_shapes inner_shape) =
  concat (zipWith repeated outer_shapes (shape ixfun)) ++ inner_shape
  where repeated outer_ds d = outer_ds ++ [d]

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
base (Repeat ixfun _ _) =
  base ixfun

rebase :: (Eq num, IntegralExp num) =>
          IxFun num
       -> IxFun num
       -> IxFun num
rebase new_base (Direct old_shape)
  | length old_shape == rank new_base = new_base
  | otherwise = error "IndexFunction.rebase: bad rank for new base."
rebase new_base (Permute ixfun perm) =
  permute (rebase new_base ixfun) perm
rebase new_base (Rotate ixfun offsets) =
  rotate (rebase new_base ixfun) offsets
rebase new_base (Index ixfun is) =
  slice (rebase new_base ixfun) is
rebase new_base (Reshape ixfun new_shape) =
  reshape (rebase new_base ixfun) new_shape
rebase new_base (Repeat ixfun outer_shapes inner_shape) =
  Repeat (rebase new_base ixfun) outer_shapes inner_shape

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
        fixingOuter (DimSlice off _ 1:is') (_:ds)
          | is' == map (unitSlice 0) ds = Just [off]
        fixingOuter is' ds
          | is' == map (unitSlice 0) ds = Just []
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
  (==Just 0) . flip linearWithOffset 1


-- | Substituting a name with a PrimExp in an index function.
substituteInIxFun :: M.Map VName (PrimExp VName) -> IxFun (PrimExp VName)
                  -> IxFun (PrimExp VName)
substituteInIxFun tab (Direct pes) =
  Direct $ map (substituteInPrimExp tab) pes
substituteInIxFun tab (Permute ixfun p) =
  Permute (substituteInIxFun tab ixfun) p
substituteInIxFun tab (Rotate  ixfun pes) =
  Rotate (substituteInIxFun tab ixfun) $ map (substituteInPrimExp tab) pes
substituteInIxFun tab (Index ixfun sl) =
  Index (substituteInIxFun tab ixfun) $ map (fmap $ substituteInPrimExp tab) sl
substituteInIxFun tab (Reshape ixfun newshape) =
  Reshape (substituteInIxFun tab ixfun) $ map (fmap $ substituteInPrimExp tab) newshape
substituteInIxFun tab (Repeat ixfun outer_shapes inner_shape) =
  Repeat (substituteInIxFun tab ixfun) outer_shapes inner_shape
