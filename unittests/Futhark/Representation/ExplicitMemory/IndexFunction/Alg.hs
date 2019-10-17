-- | A simple index operation representation.  Every operation corresponds to a
-- constructor.
module Futhark.Representation.ExplicitMemory.IndexFunction.Alg
  ( IxFun(..)
  , iota
  , offsetIndex
  , strideIndex
  , permute
  , rotate
  , reshape
  , slice
  , rebase
  , repeat
  , shape
  , rank
  , index
  )
where

import Data.List hiding (repeat)

import Prelude hiding (repeat, mod)

import Futhark.Representation.AST.Syntax
  (ShapeChange, DimChange(..), Slice, sliceDims, DimIndex(..), unitSlice)
import Futhark.Representation.AST.Attributes
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
               | Repeat (IxFun num) [Shape num] (Shape num)
               | OffsetIndex (IxFun num) num
               | StrideIndex (IxFun num) num
               | Rebase (IxFun num) (IxFun num)
               deriving (Eq, Show)

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
  ppr (OffsetIndex fun i) =
    ppr fun <> text "->offset_index" <> parens (ppr i)
  ppr (StrideIndex fun s) =
    ppr fun <> text "->stride_index" <> parens (ppr s)
  ppr (Rebase new_base fun) =
    text "rebase(" <> ppr new_base <> text ", " <> ppr fun <> text ")"


iota :: Shape num -> IxFun num
iota = Direct

offsetIndex :: IxFun num -> num -> IxFun num
offsetIndex = OffsetIndex

strideIndex :: IxFun num -> num -> IxFun num
strideIndex = StrideIndex

permute :: IxFun num -> Permutation -> IxFun num
permute = Permute

rotate :: IxFun num -> Indices num -> IxFun num
rotate = Rotate

repeat :: IxFun num -> [Shape num] -> Shape num -> IxFun num
repeat = Repeat

slice :: IxFun num -> Slice num -> IxFun num
slice = Index

rebase :: IxFun num -> IxFun num -> IxFun num
rebase = Rebase

reshape :: IxFun num -> ShapeChange num -> IxFun num
reshape = Reshape

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
shape (OffsetIndex ixfun _) =
  shape ixfun
shape (StrideIndex ixfun _) =
  shape ixfun
shape (Rebase _ ixfun) =
  shape ixfun

rank :: IntegralExp num =>
        IxFun num -> Int
rank = length . shape


index :: (IntegralExp num, Eq num) =>
         IxFun num -> Indices num -> num
index (Direct dims) is =
  sum $ zipWith (*) is slicesizes
  where slicesizes = drop 1 $ sliceSizes dims
index (Permute fun perm) is_new =
  index fun is_old
  where is_old = rearrangeShape (rearrangeInverse perm) is_new
index (Rotate fun offsets) is =
  index fun $ zipWith mod (zipWith (+) is offsets) dims
  where dims = shape fun
index (Index fun js) is =
  index fun (adjust js is)
  where adjust (DimFix j:js') is' = j : adjust js' is'
        adjust (DimSlice j _ s:js') (i:is') = j + i * s : adjust js' is'
        adjust _ _ = []
index (Reshape fun newshape) is =
  let new_indices = reshapeIndex (shape fun) (newDims newshape) is
  in index fun new_indices
index (Repeat fun outer_shapes _) is =
  -- Discard those indices that are just repeats.  It is intentional
  -- that we cut off those indices that correspond to the innermost
  -- repeated dimensions.
  index fun is'
  where flags dims = replicate (length dims) True ++ [False]
        is' = map snd $ filter (not . fst) $ zip (concatMap flags outer_shapes) is
index (OffsetIndex fun i) is =
  case shape fun of
    d : ds ->
      index (Index fun (DimSlice i (d-i) 1 : map (unitSlice 0) ds)) is
    [] -> error "index: OffsetIndex: underlying index function has rank zero"
index (StrideIndex fun s) is =
  case shape fun of
    d : ds ->
      index (Index fun (DimSlice 0 d s : map (unitSlice 0) ds)) is
    [] -> error "index: StrideIndex: underlying index function has rank zero"
index (Rebase new_base fun) is =
  let fun' = case fun of
               Direct old_shape ->
                 if old_shape == shape new_base
                 then new_base
                 else reshape new_base $ map DimCoercion old_shape
               Permute ixfun perm ->
                 permute (rebase new_base ixfun) perm
               Rotate ixfun offsets ->
                 rotate (rebase new_base ixfun) offsets
               Index ixfun iis ->
                 slice (rebase new_base ixfun) iis
               Reshape ixfun new_shape ->
                 reshape (rebase new_base ixfun) new_shape
               Repeat ixfun outer_shapes inner_shape ->
                 repeat (rebase new_base ixfun) outer_shapes inner_shape
               StrideIndex ixfun i ->
                 strideIndex (rebase new_base ixfun) i
               OffsetIndex ixfun s ->
                 offsetIndex (rebase new_base ixfun) s
               r@Rebase{} ->
                 r
  in index fun' is
