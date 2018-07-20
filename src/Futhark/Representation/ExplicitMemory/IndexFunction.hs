-- | An index function represents a mapping from an array index space
-- to a flat byte offset.
module Futhark.Representation.ExplicitMemory.IndexFunction
       (
--         IxFun(..)
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
       , repeat
       , shape
       , rank
       , linearWithOffset
       , rearrangeWithOffset
       , isLinear
       , isDirect
       , substituteInIxFun
       , getInfoMaxUnification
       , subsInIndexIxFun
       , ixFunsCompatibleRaw
       , ixFunHasIndex
       , offsetIndexDWIM
       )
       where

import Control.Arrow (first)
import Data.Maybe
import Data.Monoid ((<>))
import Data.List hiding (repeat)
import Control.Monad.Identity
import Control.Monad.Writer

import Prelude hiding (mod, repeat)

import qualified Data.List as L
import qualified Data.Map.Strict as M

import Futhark.Transform.Substitute
import Futhark.Transform.Rename

import Futhark.Representation.AST.Syntax
  (ShapeChange, DimChange(..), DimIndex(..), Slice, sliceDims, unitSlice, VName(..))
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
               deriving (Eq,Show)

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

instance Substitute num => Substitute (IxFun num) where
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

instance Substitute num => Rename (IxFun num) where
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
    d:ds -> slice ixfun (DimSlice (fromInt32 0) d s : map (unitSlice (fromInt32 0)) ds)
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

repeat :: IxFun num -> [Shape num] -> Shape num -> IxFun num
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
  | [newdim] <- newDims newshape,
    Just slicing' <- findSlice slicing (Just newdim) =
      Index ixfun slicing'
  | (is, rem_slicing) <- splitSlice slicing,
    (fixed_ds, sliced_ds) <- splitAt (length is) $ shape ixfun,
    and $ zipWith isSliceOf rem_slicing sliced_ds =
      -- Move the reshape beneath the slicing.
      let newshape' = map DimCoercion fixed_ds ++ newshape
      in Index (reshape ixfun newshape') $
         map DimFix is ++ map (unitSlice (fromInt32 0)) (newDims newshape)
  where isSliceOf (DimSlice _ d1 1) d2 = d1 == d2
        isSliceOf _ _ = False

        findSlice (DimFix i:is) d = (DimFix i:) <$> findSlice is d
        findSlice (DimSlice j _ stride:is) d = do
          d' <- d
          (DimSlice j d' stride:) <$> findSlice is Nothing
        findSlice [] Just{} = Nothing
        findSlice [] Nothing = Just []

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
  | old_shape == shape new_base = new_base
  | otherwise = reshape new_base $ map DimCoercion old_shape
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

isLinear :: (Eq num, IntegralExp num) => IxFun num -> Bool
isLinear =
  (==Just 0) . flip linearWithOffset 1

isDirect :: IxFun num -> Bool
isDirect Direct{} = True
isDirect _ = False

-- | Substituting a name with a PrimExp in an index function.
substituteInIxFun :: (Ord a) => M.Map a (PrimExp a) -> IxFun (PrimExp a)
                  -> IxFun (PrimExp a)
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

-----------------------------------------------------------
--- Niels' functions for memory management:             ---
--- these are prime candidates to be removed/re-written ---
-----------------------------------------------------------

type IxFn = IxFun (PrimExp VName)

getInfoMaxUnification :: IxFn -> Maybe (IxFn, Slice (PrimExp VName), VName)
getInfoMaxUnification (Index ixfun_start slc) =
  case L.span isDimFix slc of
    (indices_start, [DimSlice _start_offset
                     (LeafExp final_dim@VName{} (IntType Int32))
                     _stride]) ->
        Just (ixfun_start, indices_start, final_dim)
    _ -> Nothing
  where isDimFix DimFix{} = True
        isDimFix _ = False
getInfoMaxUnification _ = Nothing

-- Are two index functions *identical*?  (Silly approach, but the Eq
-- instance is used for something else.)
ixFunsCompatibleRaw :: Eq num => IxFun num -> IxFun num -> Bool
ixFunsCompatibleRaw ixfun0 ixfun1 = ixfun0 `primEq` ixfun1
  where primEq a b = case (a, b) of
          (Direct sa, Direct sb) ->
            sa == sb
          (Permute a1 pa, Permute b1 pb) ->
            a1 `primEq` b1 && pa == pb
          (Rotate a1 ia, Rotate b1 ib) ->
            a1 `primEq` b1 && ia == ib
          (Index a1 sa, Index b1 sb) ->
            a1 `primEq` b1 && sa == sb
          (Reshape a1 sa, Reshape b1 sb) ->
            a1 `primEq` b1 && sa == sb
          (Repeat a1 ssa sa, Repeat b1 ssb sb) ->
            a1 `primEq` b1 && ssa == ssb && sa == sb
          _ -> False

ixFunHasIndex :: IxFun num -> Bool
ixFunHasIndex ixfun = case ixfun of
  Direct _ -> False
  Permute ixfun' _ -> ixFunHasIndex ixfun'
  Rotate ixfun' _ -> ixFunHasIndex ixfun'
  Index{} -> True
  Reshape ixfun' _ -> ixFunHasIndex ixfun'
  Repeat ixfun' _ _ -> ixFunHasIndex ixfun'

subsInIndexIxFun :: IxFn -> VName -> VName -> IxFn
subsInIndexIxFun (Index ixfun_start slc) final_dim final_dim_max_v =
  let tab = M.singleton final_dim (LeafExp final_dim_max_v (IntType Int32))
      ixfun_start' = substituteInIxFun tab ixfun_start
  in  Index ixfun_start' slc
subsInIndexIxFun _ _ _ = error "In IxFun.subsInIndexIxFun: should-not-happen case reached!"

offsetIndexDWIM :: Int -> IxFn -> PrimExp VName -> IxFn
offsetIndexDWIM n_ignore_initial ixfun offset =
  fromMaybe (offsetIndex ixfun offset) $ case ixfun of
  Index ixfun1 dimindices ->
    let (dim_first, dim_rest) = L.splitAt n_ignore_initial dimindices
    in case dim_rest of
      (DimFix i : dim_rest') ->
        Just $ Index ixfun1 (dim_first ++ DimFix (i + offset) : dim_rest')
      _ -> Nothing
  _ -> Nothing
