{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures, ScopedTypeVariables #-}
module Futhark.Representation.ExplicitMemory.IndexFunction
       (
         IxFun
       , Shape
       , Indices
       , index
       , iota
       , offset
       , permute
       , reshape
       , applyInd
       , codomain
       , shape
       , linearWithOffset
       )
       where

import Data.Constraint (Dict (..))
import Data.Type.Natural hiding (n1,n2)
import Data.Vector.Sized hiding
  (index, map, unsafeFromInt, foldl, drop, zipWith)
import qualified Data.Vector.Sized as Vec
import Proof.Equational
import Data.Monoid
import Data.Type.Equality hiding (outer)

import Futhark.Analysis.ScalExp
import Futhark.Substitute
import Futhark.Renamer

import qualified Futhark.Representation.ExplicitMemory.Permutation as Perm
import Futhark.Representation.ExplicitMemory.SymSet (SymSet)
import Futhark.Representation.AST.Attributes.Names
import Language.Futhark.Core
import Futhark.Representation.AST (SubExp (..), IdentBase (..), TypeBase (..))

import Text.PrettyPrint.Mainland

type Shape = Vector SubExp
type Indices = Vector ScalExp

data IxFun :: Nat -> * where
  Direct :: Shape n -> IxFun n
  Offset :: IxFun n -> ScalExp -> IxFun n
  Permute :: IxFun n -> Perm.Permutation n -> IxFun n
  Index :: SNat n -> IxFun (m:+:n) -> Indices m -> IxFun n
  Reshape :: IxFun (S m) -> Shape n -> IxFun n

--- XXX: this is just structural equality, which may be too
--- conservative - unless we normalise first, somehow.
instance Eq (IxFun n) where
  Direct _ == Direct _ = True
  Offset ixfun1 offset1 == Offset ixfun2 offset2 =
    ixfun1 == ixfun2 && offset1 == offset2
  Permute ixfun1 perm1 == Permute ixfun2 perm2 =
    ixfun1 == ixfun2 && perm1 == perm2
  Index _ (ixfun1 :: IxFun (m1 :+: n)) (is1 :: Indices m1)
    == Index _ (ixfun2 :: IxFun (m2 :+: n)) (is2 :: Indices m2) =
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

instance Show (IxFun n) where
  show (Direct n) = "Direct (" ++ show n ++ ")"
  show (Offset fun k) = "Offset (" ++ show fun ++ ", " ++ show k ++ ")"
  show (Permute fun perm) = "Permute (" ++ show fun ++ ", " ++ show perm ++ ")"
  show (Index _ fun is) = "Index (" ++ show fun ++ ", " ++
                          ", " ++ show is ++ ")"
  show (Reshape fun newshape) = "Reshape (" ++ show fun ++ ", " ++ show newshape ++ ")"

instance Pretty (IxFun n) where
  ppr (Direct dims) =
    text "Direct" <> parens (commasep $ map ppr $ Vec.toList dims)
  ppr (Offset fun k) = ppr fun <+> text "+" <+> ppr k
  ppr (Permute fun perm) = ppr fun <> ppr perm
  ppr (Index _ fun is) = ppr fun <> brackets (commasep $ map ppr $ Vec.toList is)
  ppr (Reshape fun oldshape) =
    ppr fun <> text "->" <>
    parens (commasep (map ppr $ Vec.toList oldshape))

instance Substitute (IxFun n) where
  substituteNames _ (Direct n) =
    Direct n
  substituteNames subst (Offset fun k) =
    Offset (substituteNames subst fun) (substituteNames subst k)
  substituteNames subst (Permute fun perm) =
    Permute (substituteNames subst fun) perm
  substituteNames subst (Index n fun is) =
    Index n
    (substituteNames subst fun)
    (Vec.map (substituteNames subst) is)
  substituteNames subst (Reshape fun newshape) =
    Reshape
    (substituteNames subst fun)
    (Vec.map (substituteNames subst) newshape)

instance Rename (IxFun n) where
  -- Because there is no mapM-like function on sized vectors, we
  -- implement renaming by retrieving the substitution map, then using
  -- 'substituteNames'.  This is safe as index functions do not
  -- contain their own bindings.
  rename ixfun = do
    subst <- renamerSubstitutions
    return $ substituteNames subst ixfun

index :: forall (n::Nat).
         IxFun (S n) -> Indices (S n) -> ScalExp

index (Direct dims) is =
  ssum $ Vec.toList $ Vec.zipWithSame STimes is slicesizes
  where slicesizes :: Vector ScalExp (S n)
        slicesizes = Vec.tail $ sliceSizes dims

index (Offset fun k) vec =
  index fun vec `SPlus` k

index (Permute fun perm) is_new =
  index fun is_old
  where is_old   = Perm.apply (Perm.invert perm) is_new

index (Index _ fun (is1::Indices m)) is2 =
  case (singInstance $ sLength is1,
        singInstance $ sLength is2 %:- sOne) of
    (SingInstance,SingInstance) ->
      let is :: Indices (m :+ S n)
          is = is1 `Vec.append` is2
          outer = succPlusR (sing :: SNat m) (sing :: SNat n)
          proof :: (m :+ S n) :=: S (m :+ n)
          proof = succPlusR (sing :: SNat m) (sing :: SNat n)
      in case singInstance $ coerce proof (sLength is) %:- sOne of
        SingInstance ->
          index (coerce outer fun) (coerce outer is)

index (Reshape (fun :: IxFun (S m)) newshape) is =
  -- First, compute a flat index based on the new shape.  Then, turn
  -- that into an index set for the inner index function and apply the
  -- inner index function to that set.
  let oldshape = shape fun
      flatidx = computeFlatIndex newshape is
      innerslicesizes :: Vector ScalExp (S m)
      innerslicesizes = Vec.tail $ sliceSizes oldshape
      new_indices :: Indices (S m)
      new_indices = computeNewIndices innerslicesizes flatidx
  in index fun new_indices

computeNewIndices :: Vector ScalExp k -> ScalExp -> Indices k
computeNewIndices Nil _ =
  Nil
computeNewIndices (size :- slices) i =
  (i `SDivide` size) :-
  computeNewIndices slices (i `SMinus` ((i `SDivide` size) `STimes` size))

computeFlatIndex :: Shape (S k) -> Indices (S k) -> ScalExp
computeFlatIndex dims is =
  ssum $ Vec.toList $ Vec.zipWithSame STimes is slicesizes
  where slicesizes = Vec.tail $ sliceSizes dims

sliceSizes :: Shape m -> Vector ScalExp (S m)
sliceSizes Nil = singleton $ Val $ IntVal 1
sliceSizes (n :- ns) =
  sproduct (map subExpToScalExp $ n : Vec.toList ns) :-
  sliceSizes ns

iota :: Shape n -> IxFun n
iota = Direct

offset :: IxFun n -> ScalExp -> IxFun n
offset = Offset

permute :: IxFun n -> Perm.Permutation n -> IxFun n
permute = Permute

reshape :: IxFun (S m) -> Shape n -> IxFun n
reshape = Reshape

applyInd :: SNat n -> IxFun (m:+:n) -> Indices m -> IxFun n
applyInd = Index

codomain :: IxFun n -> SymSet n
codomain = undefined

rank :: IxFun n -> SNat n
rank (Direct dims) = sLength dims
rank (Offset ixfun _) = rank ixfun
rank (Permute ixfun _) = rank ixfun
rank (Index n _ _) = n
rank (Reshape _ newshape) = sLength newshape

shape :: IxFun n -> Shape n
shape (Direct dims) =
  dims
shape (Permute ixfun perm) =
  Perm.apply perm $ shape ixfun
shape (Offset ixfun _) =
  shape ixfun
shape (Index n (ixfun::IxFun (m :+ n)) (indices::Indices m)) =
  let ixfunshape :: Shape (m :+ n)
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
       let resshape :: Shape ((m :+ n) :- m)
           resshape = Vec.drop islen ixfunshape
       in resshape
shape (Reshape _ dims) =
  dims

-- FIXME: this function is not yet quite there.
linearWithOffset :: IxFun n -> Maybe ScalExp
linearWithOffset (Direct _) = Just $ Val $ IntVal 0
linearWithOffset (Offset ixfun n) = do
  inner_offset <- linearWithOffset ixfun
  return $ inner_offset `SPlus` n
linearWithOffset _ = Nothing
linearWithOffset (Reshape ixfun _) =
  linearWithOffset ixfun
linearWithOffset (Permute ixfun _) =
  linearWithOffset ixfun
{-
linearWithOffset (Permute {}) _ = Nothing
linearWithOffset (Index _ (Direct _) dims1 is) dims2 =
  Just $ ssum $ Vec.toList $
  Vec.zipWithSame STimes is $
  Vec.map (STimes restsize) $ Vec.tail $ sliceSizes dims1
  where restsize = sproduct $ map subExpToScalExp $ Vec.toList dims2
linearWithOffset (Index {}) _ =
  Nothing
-- linearWithOffset (Reshape ixfun dims) _  =
--   linearWithOffset ixfun dims
-}

instance FreeIn (IxFun n) where
  freeIn (Direct dims) = freeIn $ Vec.toList dims
  freeIn (Offset ixfun e) = freeIn ixfun <> freeIn e
  freeIn (Permute ixfun _) = freeIn ixfun
  freeIn (Index _ ixfun is) =
    freeIn ixfun <>
    mconcat (map freeIn $ toList is)
  freeIn (Reshape ixfun dims) =
    freeIn ixfun <> mconcat (map freeIn $ Vec.toList dims)
{-
test = iota shape
  where shape = Var n :- Var m :- Nil
        n = Ident (ID (nameFromString "n", 0)) $ Basic Int
        m = Ident (ID (nameFromString "m", 0)) $ Basic Int
-}
