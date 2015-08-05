{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures, ScopedTypeVariables #-}
module Futhark.Representation.ExplicitMemory.IndexFunction
       (
         IxFun
       , Shape
       , Indices
       , index
       , iota
       , offsetIndex
       , permute
       , reshape
       , applyInd
       , offsetUnderlying
       , underlyingOffset
       , codomain
       , shape
       , linearWithOffset
       )
       where

import Data.Constraint (Dict (..))
import Data.Type.Natural hiding (n1,n2)
import Data.Vector.Sized hiding (index, map)
import qualified Data.Vector.Sized as Vec
import Proof.Equational
import Data.Monoid
import Data.Type.Equality hiding (outer)

import Prelude

import Futhark.Substitute
import Futhark.Renamer

import qualified Futhark.Representation.ExplicitMemory.Permutation as Perm
import Futhark.Representation.ExplicitMemory.SymSet (SymSet)
import Futhark.Representation.AST.Attributes.Names

import Text.PrettyPrint.Mainland

type Shape num = Vector num
type Indices num = Vector num

data IxFun :: * -> Nat -> * where
  Direct :: num -> Shape num n -> IxFun num n
  Offset :: IxFun num n -> num -> IxFun num n
  Permute :: IxFun num n -> Perm.Permutation n -> IxFun num n
  Index :: SNat n -> IxFun num (m:+:n) -> Indices num m -> IxFun num n
  Reshape :: IxFun num ('S m) -> Shape num n -> IxFun num n

--- XXX: this is just structural equality, which may be too
--- conservative - unless we normalise first, somehow.
instance (Fractional num, Eq num) => Eq (IxFun num n) where
  Direct offset1 _ == Direct offset2 _ =
    offset1 == offset2
  Offset ixfun1 offset1 == Offset ixfun2 offset2 =
    ixfun1 == ixfun2 && offset1 == offset2
  Permute ixfun1 perm1 == Permute ixfun2 perm2 =
    ixfun1 == ixfun2 && perm1 == perm2
  Index _ (ixfun1 :: IxFun num (m1 :+: n)) (is1 :: Indices nun m1)
    == Index _ (ixfun2 :: IxFun num (m2 :+: n)) (is2 :: Indices num m2) =
    case testEquality m1' m2' of
      Nothing -> False
      Just Refl ->
        ixfun1 == ixfun2 &&
        Vec.toList is1 == Vec.toList is2
    where m1' :: SNat m1
          m1' = Vec.sLength is1
          m2' :: SNat m2
          m2' = Vec.sLength is2
  Reshape ixfun1 shape1 == Reshape ixfun2 shape2 =
    case testEquality (rank ixfun1) (rank ixfun2) of
      Just Refl ->
        ixfun1 == ixfun2 && shape1 == shape2
      _ -> False
  _ == _ = False

instance Show num => Show (IxFun num n) where
  show (Direct offset n) = "Direct (" ++ show offset ++ "," ++ show n ++ ")"
  show (Offset fun k) = "Offset (" ++ show fun ++ ", " ++ show k ++ ")"
  show (Permute fun perm) = "Permute (" ++ show fun ++ ", " ++ show perm ++ ")"
  show (Index _ fun is) = "Index (" ++ show fun ++ ", " ++ show is ++ ")"
  show (Reshape fun newshape) = "Reshape (" ++ show fun ++ ", " ++ show newshape ++ ")"

instance Pretty num => Pretty (IxFun num n) where
  ppr (Direct offset dims) =
    text "Direct" <> parens (commasep $ ppr offset : map ppr (Vec.toList dims))
  ppr (Offset fun k) = ppr fun <+> text "+" <+> ppr k
  ppr (Permute fun perm) = ppr fun <> ppr perm
  ppr (Index _ fun is) = ppr fun <> brackets (commasep $ map ppr $ Vec.toList is)
  ppr (Reshape fun oldshape) =
    ppr fun <> text "->" <>
    parens (commasep (map ppr $ Vec.toList oldshape))

instance (Eq num, Fractional num, Substitute num) => Substitute (IxFun num n) where
  substituteNames subst (Direct offset n) =
    Direct (substituteNames subst offset) n
  substituteNames subst (Offset fun k) =
    Offset (substituteNames subst fun) (substituteNames subst k)
  substituteNames subst (Permute fun perm) =
    Permute (substituteNames subst fun) perm
  substituteNames subst (Index n fun is) =
    Index n
    (substituteNames subst fun)
    (Vec.map (substituteNames subst) is)
  substituteNames subst (Reshape fun newshape) =
    reshape
    (substituteNames subst fun)
    (Vec.map (substituteNames subst) newshape)

instance (Eq num, Fractional num, Substitute num, Rename num) => Rename (IxFun num n) where
  -- Because there is no mapM-like function on sized vectors, we
  -- implement renaming by retrieving the substitution map, then using
  -- 'substituteNames'.  This is safe as index functions do not
  -- contain their own bindings.
  rename ixfun = do
    subst <- renamerSubstitutions
    return $ substituteNames subst ixfun

index :: forall (n::Nat) num. Fractional num =>
         IxFun num ('S n) -> Indices num ('S n) -> num -> num

index (Direct offset dims) is element_size =
  (Vec.sum (Vec.zipWithSame (*) is slicesizes) * element_size) + offset
  where slicesizes = Vec.tail $ sliceSizes dims

index (Offset fun offset) (i :- is) element_size =
  index fun ((i + offset) :- is) element_size

index (Permute fun perm) is_new element_size =
  index fun is_old element_size
  where is_old = Perm.apply (Perm.invert perm) is_new

index (Index _ fun (is1::Indices num m)) is2 element_size =
  case (singInstance $ sLength is1,
        singInstance $ sLength is2 %:- sOne) of
    (SingInstance,SingInstance) ->
      let is = is1 `Vec.append` is2
          outer = succPlusR (sing :: SNat m) (sing :: SNat n)
          proof :: (m :+ 'S n) :=: 'S (m :+ n)
          proof = succPlusR (sing :: SNat m) (sing :: SNat n)
      in case singInstance $ coerce proof (sLength is) %:- sOne of
        SingInstance ->
          index (coerce outer fun) (coerce outer is) element_size

index (Reshape (fun :: IxFun num ('S m)) newshape) is element_size =
  -- First, compute a flat index based on the new shape.  Then, turn
  -- that into an index set for the inner index function and apply the
  -- inner index function to that set.
  let oldshape = shape fun
      flatidx = computeFlatIndex newshape is
      innerslicesizes = Vec.tail $ sliceSizes oldshape
      new_indices = computeNewIndices innerslicesizes flatidx
  in index fun new_indices element_size

computeNewIndices :: Fractional num =>
                     Vector num k -> num -> Indices num k
computeNewIndices Nil _ =
  Nil
computeNewIndices (size :- slices) i =
  (i / size) :-
  computeNewIndices slices (i - (i / size) * size)

computeFlatIndex :: Fractional num =>
                    Shape num ('S k) -> Indices num ('S k) -> num
computeFlatIndex dims is =
  Vec.sum $ Vec.zipWithSame (*) is slicesizes
  where slicesizes = Vec.tail $ sliceSizes dims

sliceSizes :: Fractional num =>
              Shape num m -> Vector num ('S m)
sliceSizes Nil =
  singleton 1
sliceSizes (n :- ns) =
  Vec.product (n :- ns) :-
  sliceSizes ns

iota :: Fractional num =>
        Shape num n -> IxFun num n
iota = Direct 0

offsetIndex :: Fractional num =>
               IxFun num n -> num -> IxFun num n
offsetIndex = Offset

permute :: Fractional num =>
           IxFun num n -> Perm.Permutation n -> IxFun num n
permute = Permute

reshape :: forall num m n.(Eq num, Fractional num) =>
           IxFun num ('S m) -> Shape num n -> IxFun num n
reshape ixfun newshape =
  case rank ixfun `testEquality` Vec.sLength newshape of
    Just Refl | shape ixfun == newshape ->
      ixfun
    _ ->
      Reshape ixfun newshape

applyInd :: forall num n m.
            Fractional num =>
            SNat n -> IxFun num (m:+:n) -> Indices num m -> IxFun num n
applyInd n (Index m_plus_n (ixfun :: IxFun num (k:+:(m:+:n))) (mis :: Indices num k)) is =
  Index n ixfun' is'
  where k :: SNat k
        k = Vec.sLength mis
        m :: SNat m
        m = case propToBoolLeq $ plusLeqR m n of
              Dict -> coerce (plusMinusEqL m n) $ m_plus_n %- n
        is' :: Indices num (m:+:k)
        is' = coerce (plusCommutative k m) $ Vec.append mis is
        ixfun' :: IxFun num ((m:+:k):+:n)
        ixfun' = coerce (plusCongR n (plusCommutative k m)) $
                 coerce (plusAssociative k m n) ixfun
applyInd n ixfun is = Index n ixfun is

offsetUnderlying :: Fractional num =>
                    IxFun num n -> num -> IxFun num n
offsetUnderlying (Direct offset dims) k =
  Direct (offset + k) dims
offsetUnderlying (Offset ixfun m) k =
  Offset (offsetUnderlying ixfun k) m
offsetUnderlying (Permute ixfun perm) k =
  Permute (offsetUnderlying ixfun k) perm
offsetUnderlying (Index n ixfun is) k =
  Index n (offsetUnderlying ixfun k) is
offsetUnderlying (Reshape ixfun dims) k =
  Reshape (offsetUnderlying ixfun k) dims

underlyingOffset :: Fractional num =>
                    IxFun num n -> num
underlyingOffset (Direct offset _) =
  offset
underlyingOffset (Offset ixfun _) =
  underlyingOffset ixfun
underlyingOffset (Permute ixfun _) =
  underlyingOffset ixfun
underlyingOffset (Index _ ixfun _) =
  underlyingOffset ixfun
underlyingOffset (Reshape ixfun _) =
  underlyingOffset ixfun

codomain :: Fractional num =>
            IxFun num n -> SymSet n
codomain = undefined

rank :: Fractional num =>
        IxFun num n -> SNat n
rank (Direct _ dims) = sLength dims
rank (Offset ixfun _) = rank ixfun
rank (Permute ixfun _) = rank ixfun
rank (Index n _ _) = n
rank (Reshape _ newshape) = sLength newshape

shape :: Fractional num =>
         IxFun num n -> Shape num n
shape (Direct _ dims) =
  dims
shape (Permute ixfun perm) =
  Perm.apply perm $ shape ixfun
shape (Offset ixfun _) =
  shape ixfun
shape (Index n (ixfun::IxFun num (m :+ n)) (indices::Indices num m)) =
  let ixfunshape :: Shape num (m :+ n)
      ixfunshape = shape ixfun
      islen :: SNat m
      islen = Vec.sLength indices
      prop :: Leq m (m :+ n)
      prop = plusLeqL islen n
      prop2 :: ((m :+: n) :-: m) :=: n
      prop2 = plusMinusEqR n islen
  in
   case (propToBoolLeq prop, prop2) of
     (Dict,Refl) ->
       let resshape :: Shape num ((m :+ n) :- m)
           resshape = Vec.drop islen ixfunshape
       in resshape
shape (Reshape _ dims) =
  dims

-- This function does not cover all possible cases.  It's a "best
-- effort" kind of thing.
linearWithOffset :: forall n num. Fractional num =>
                    IxFun num n -> num -> Maybe num
linearWithOffset (Direct offset _) _ =
  Just offset
linearWithOffset (Offset ixfun n) element_size = do
  inner_offset <- linearWithOffset ixfun element_size
  case Vec.tail $ sliceSizes $ shape ixfun of
    Nil -> Nothing
    rowslice :- _ ->
      return $ inner_offset + (n * rowslice * element_size)
linearWithOffset (Reshape ixfun _) element_size =
 linearWithOffset ixfun element_size
linearWithOffset (Index n ixfun (is :: Indices num m)) element_size = do
  inner_offset <- linearWithOffset ixfun element_size
  case propToBoolLeq $ plusLeqL m n of
    Dict ->
      let slices :: Vector num m
          slices = Vec.take m $ Vec.tail $ sliceSizes $ shape ixfun
      in Just $ inner_offset + Vec.sum (Vec.zipWith (*) slices is) * element_size
  where m :: SNat m
        m = Vec.sLength is
linearWithOffset _ _ = Nothing

instance FreeIn num => FreeIn (IxFun num n) where
  freeIn (Direct offset dims) = freeIn offset <> freeIn (Vec.toList dims)
  freeIn (Offset ixfun e) = freeIn ixfun <> freeIn e
  freeIn (Permute ixfun _) = freeIn ixfun
  freeIn (Index _ ixfun is) =
    freeIn ixfun <>
    mconcat (map freeIn $ toList is)
  freeIn (Reshape ixfun dims) =
    freeIn ixfun <> mconcat (map freeIn $ Vec.toList dims)
