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
--       , shape
       , linearWithOffset
       )
       where

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
import Futhark.Representation.AST.Syntax (SubExp (..))

import Text.PrettyPrint.Mainland

type Shape = Vector SubExp
type Indices = Vector ScalExp

data IxFun :: Nat -> * where
  Direct :: SNat n -> IxFun n
  Offset :: IxFun n -> ScalExp -> IxFun n
  Permute :: IxFun n -> Perm.Permutation n -> IxFun n
  Index :: SNat n -> IxFun (m:+:n) -> Shape m -> Indices m -> IxFun n
  Reshape :: SNat n -> IxFun (S m) -> Shape (S m) -> IxFun n

--- XXX: this is just structural equality, which may be too
--- conservative - unless we normalise first, somehow.
instance Eq (IxFun n) where
  Direct _ == Direct _ = True
  Offset ixfun1 offset1 == Offset ixfun2 offset2 =
    ixfun1 == ixfun2 && offset1 == offset2
  Permute ixfun1 perm1 == Permute ixfun2 perm2 =
    ixfun1 == ixfun2 && perm1 == perm2
  Index _ (ixfun1 :: IxFun (m1 :+: n)) dims1 (is1 :: Indices m1)
    == Index _ (ixfun2 :: IxFun (m2 :+: n)) dims2 (is2 :: Indices m2) =
    case testEquality m1' m2' of
      Nothing -> False
      Just Refl ->
        ixfun1 == ixfun2 &&
        Vec.toList is1 == Vec.toList is2 &&
        Vec.toList dims1 == Vec.toList dims2
    where m1' :: SNat m1
          m1' = Vec.sLength is1
          m2' :: SNat m2
          m2' = Vec.sLength is2
  Reshape n1 ixfun1 shape1 == Reshape n2 ixfun2 shape2 =
    case (testEquality (rank ixfun1) (rank ixfun2),
          testEquality n1 n2) of
      (Just Refl, Just Refl) ->
        ixfun1 == ixfun2 && shape1 == shape2
      _ -> False
  _ == _ = False

instance Show (IxFun n) where
  show (Direct n) = "Direct (" ++ show n ++ ")"
  show (Offset fun k) = "Offset (" ++ show fun ++ ", " ++ show k ++ ")"
  show (Permute fun perm) = "Permute (" ++ show fun ++ ", " ++ show perm ++ ")"
  show (Index _ fun dims is) = "Index (" ++ show fun ++ ", " ++
                               show dims ++ ", " ++ show is ++ ")"
  show (Reshape _ fun oldshape) = "Reshape (" ++ show fun ++ ", " ++ show oldshape ++ ")"

instance Pretty (IxFun n) where
  ppr (Direct n) =
    text "Direct" <> parens (text $ show (sNatToInt n::Int))
  ppr (Offset fun k) = ppr fun <+> text "+" <+> ppr k
  ppr (Permute fun perm) = ppr fun <> ppr perm
  ppr (Index _ fun _ is) = ppr fun <> brackets (commasep $ map ppr $ Vec.toList is)
  ppr (Reshape _ fun oldshape) =
    ppr fun <> text "->" <>
    parens (commasep (map ppr $ Vec.toList oldshape))

instance Substitute (IxFun n) where
  substituteNames _ (Direct n) =
    Direct n
  substituteNames subst (Offset fun k) =
    Offset (substituteNames subst fun) (substituteNames subst k)
  substituteNames subst (Permute fun perm) =
    Permute (substituteNames subst fun) perm
  substituteNames subst (Index n fun dims is) =
    Index n
    (substituteNames subst fun)
    (Vec.map (substituteNames subst) dims)
    (Vec.map (substituteNames subst) is)
  substituteNames subst (Reshape n fun oldshape) =
    Reshape n
    (substituteNames subst fun)
    (Vec.map (substituteNames subst) oldshape)

instance Rename (IxFun n) where
  -- Because there is no mapM-like function on sized vectors, we
  -- implement renaming by retrieving the substitution map, then using
  -- 'substituteNames'.  This is safe as index functions do not
  -- contain their own bindings.
  rename ixfun = do
    subst <- renamerSubstitutions
    return $ substituteNames subst ixfun

index :: forall (n::Nat).
         IxFun (S n) -> Shape (S n) -> Indices (S n) -> ScalExp

index (Direct _) dims is =
  ssum $ Vec.toList $ Vec.zipWithSame STimes is slicesizes
  where slicesizes :: Vector ScalExp (S n)
        slicesizes = Vec.tail $ sliceSizes dims

index (Offset fun k) dims vec =
  index fun dims vec `SPlus` k

index (Permute fun perm) dims_new is_new =
  index fun dims_old is_old
  where dims_old = Perm.apply (Perm.invert perm) dims_new
        is_old   = Perm.apply (Perm.invert perm) is_new

index (Index _ fun (dims1::Shape m) (is1::Indices m)) dims2 is2 =
  case (singInstance $ sLength is1,
        singInstance $ sLength is2 %:- sOne) of
    (SingInstance,SingInstance) ->
      let is :: Indices (m :+ S n)
          is = is1 `Vec.append` is2
          dims :: Shape (m :+ S n)
          dims = dims1 `Vec.append` dims2
          outer = succPlusR (sing :: SNat m) (sing :: SNat n)
          proof :: (m :+ S n) :=: S (m :+ n)
          proof = succPlusR (sing :: SNat m) (sing :: SNat n)
      in case singInstance $ coerce proof (sLength is) %:- sOne of
        SingInstance ->
          index (coerce outer fun) (coerce outer dims) (coerce outer is)

index
  (Reshape _ (fun :: IxFun (S m)) oldshape)
  newshape
  is =
  -- First, compute a flat index based on the new shape.  Then, turn
  -- that into an index set for the inner index function and apply the
  -- inner index function to that set.
  let flatidx = computeFlatIndex newshape is
      innerslicesizes :: Vector ScalExp (S m)
      innerslicesizes = Vec.tail $ sliceSizes oldshape
      new_indices :: Indices (S m)
      new_indices = computeNewIndices innerslicesizes flatidx
  in index fun oldshape new_indices

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

iota :: SNat n -> IxFun n
iota = Direct

offset :: IxFun n -> ScalExp -> IxFun n
offset = Offset

permute :: IxFun n -> Perm.Permutation n -> IxFun n
permute = Permute

reshape :: SNat n -> IxFun (S m) -> Shape (S m) -> IxFun n
reshape = Reshape

applyInd :: SNat n -> IxFun (m:+:n) -> Shape m -> Indices m -> IxFun n
applyInd = Index

codomain :: IxFun n -> SymSet n
codomain = undefined

rank :: IxFun n -> SNat n
rank (Direct n) = n
rank (Offset ixfun _) = rank ixfun
rank (Permute ixfun _) = rank ixfun
rank (Index n _ _ _) = n
rank (Reshape n _ _) = n

-- FIXME: this function is not yet quite there.
linearWithOffset :: IxFun n -> Maybe ScalExp
linearWithOffset (Direct _) = Just $ Val $ IntVal 0
linearWithOffset (Offset ixfun n) = do
  inner_offset <- linearWithOffset ixfun
  return $ inner_offset `SPlus` n
linearWithOffset (Permute {}) = Nothing
linearWithOffset (Index _ (Direct _) dims is) =
  Just $ ssum $ Vec.toList $
  Vec.zipWithSame STimes is $
  Vec.tail $ sliceSizes dims
linearWithOffset (Index {}) =
  Nothing
linearWithOffset (Reshape _ ixfun _) =
  linearWithOffset ixfun
instance FreeIn (IxFun n) where
  freeIn (Direct _) = mempty
  freeIn (Offset ixfun e) = freeIn ixfun <> freeIn e
  freeIn (Permute ixfun _) = freeIn ixfun
  freeIn (Index _ ixfun dims is) =
    freeIn ixfun <>
    mconcat (map freeIn $ toList dims) <>
    mconcat (map freeIn $ toList is)
  freeIn (Reshape _ ixfun dims) =
    freeIn ixfun <> mconcat (map freeIn $ Vec.toList dims)
