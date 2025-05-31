-- | A simple index operation representation.  Every operation corresponds to a
-- constructor.
module Futhark.IR.Mem.IxFun.Alg
  ( IxFun (..),
    iota,
    offsetIndex,
    permute,
    reshape,
    coerce,
    slice,
    flatSlice,
    expand,
    shape,
    index,
    disjoint,
  )
where

import Data.List qualified as L
import Data.Set qualified as S
import Futhark.IR.Pretty ()
import Futhark.IR.Prop
import Futhark.IR.Syntax
  ( DimIndex (..),
    FlatDimIndex (..),
    FlatSlice (..),
    Slice (..),
    flatSliceDims,
    sliceDims,
    unitSlice,
  )
import Futhark.Util.IntegralExp
import Futhark.Util.Pretty
import Prelude hiding (div, mod, span)

type Shape num = [num]

type Indices num = [num]

type Permutation = [Int]

data IxFun num
  = Direct (Shape num)
  | Permute (IxFun num) Permutation
  | Index (IxFun num) (Slice num)
  | FlatIndex (IxFun num) (FlatSlice num)
  | Reshape (IxFun num) (Shape num)
  | Coerce (IxFun num) (Shape num)
  | OffsetIndex (IxFun num) num
  | Expand num num (IxFun num)
  deriving (Eq, Show)

instance (Pretty num) => Pretty (IxFun num) where
  pretty (Direct dims) =
    "Direct" <> parens (commasep $ map pretty dims)
  pretty (Permute fun perm) = pretty fun <> pretty perm
  pretty (Index fun is) = pretty fun <> pretty is
  pretty (FlatIndex fun is) = pretty fun <> pretty is
  pretty (Reshape fun oldshape) =
    pretty fun
      <> "->reshape"
      <> parens (pretty oldshape)
  pretty (Coerce fun oldshape) =
    pretty fun
      <> "->coerce"
      <> parens (pretty oldshape)
  pretty (OffsetIndex fun i) =
    pretty fun <> "->offset_index" <> parens (pretty i)
  pretty (Expand o p fun) =
    "expand(" <> pretty o <> "," <+> pretty p <> "," <+> pretty fun <> ")"

iota :: Shape num -> IxFun num
iota = Direct

offsetIndex :: IxFun num -> num -> IxFun num
offsetIndex = OffsetIndex

permute :: IxFun num -> Permutation -> IxFun num
permute = Permute

slice :: IxFun num -> Slice num -> IxFun num
slice = Index

flatSlice :: IxFun num -> FlatSlice num -> IxFun num
flatSlice = FlatIndex

expand :: num -> num -> IxFun num -> IxFun num
expand = Expand

reshape :: IxFun num -> Shape num -> IxFun num
reshape = Reshape

coerce :: IxFun num -> Shape num -> IxFun num
coerce = Reshape

shape ::
  (IntegralExp num) =>
  IxFun num ->
  Shape num
shape (Direct dims) =
  dims
shape (Permute ixfun perm) =
  rearrangeShape perm $ shape ixfun
shape (Index _ how) =
  sliceDims how
shape (FlatIndex ixfun how) =
  flatSliceDims how <> tail (shape ixfun)
shape (Reshape _ dims) =
  dims
shape (Coerce _ dims) =
  dims
shape (OffsetIndex ixfun _) =
  shape ixfun
shape (Expand _ _ ixfun) =
  shape ixfun

index ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  Indices num ->
  num
index (Direct dims) is =
  sum $ zipWith (*) is slicesizes
  where
    slicesizes = drop 1 $ sliceSizes dims
index (Permute fun perm) is_new =
  index fun is_old
  where
    is_old = rearrangeShape (rearrangeInverse perm) is_new
index (Index fun (Slice js)) is =
  index fun (adjust js is)
  where
    adjust (DimFix j : js') is' = j : adjust js' is'
    adjust (DimSlice j _ s : js') (i : is') = j + i * s : adjust js' is'
    adjust _ _ = []
index (FlatIndex fun (FlatSlice offset js)) is =
  index fun $ sum (offset : zipWith f is js) : drop (length js) is
  where
    f i (FlatDimIndex _ s) = i * s
index (Reshape fun newshape) is =
  let new_indices = reshapeIndex (shape fun) newshape is
   in index fun new_indices
index (Coerce fun _) is =
  index fun is
index (OffsetIndex fun i) is =
  case shape fun of
    d : ds ->
      index (Index fun (Slice (DimSlice i (d - i) 1 : map (unitSlice 0) ds))) is
    [] -> error "index: OffsetIndex: underlying index function has rank zero"
index (Expand o p ixfun) is =
  o + p * index ixfun is

allPoints :: (IntegralExp num, Enum num) => [num] -> [[num]]
allPoints dims =
  let total = product dims
      strides = drop 1 $ L.reverse $ scanl (*) 1 $ L.reverse dims
   in map (unflatInd strides) [0 .. total - 1]
  where
    unflatInd strides x =
      fst $
        foldl
          ( \(res, acc) span ->
              (res ++ [acc `div` span], acc `mod` span)
          )
          ([], x)
          strides

disjoint :: (IntegralExp num, Ord num, Enum num) => IxFun num -> IxFun num -> Bool
disjoint ixf1 ixf2 =
  let shp1 = shape ixf1
      points1 = S.fromList $ allPoints shp1
      allIdxs1 = S.map (index ixf1) points1
      shp2 = shape ixf2
      points2 = S.fromList $ allPoints shp2
      allIdxs2 = S.map (index ixf2) points2
   in S.disjoint allIdxs1 allIdxs2
