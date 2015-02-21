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
       , applyInd
       , codomain
       , linearWithOffset
       )
       where

import Data.Type.Natural
import Data.Vector.Sized hiding
  (index, map, unsafeFromInt, foldl, drop, zipWith)
import qualified Data.Vector.Sized as Vec
import Proof.Equational
import Data.Monoid
import Data.List (tails)
import Data.Type.Equality hiding (outer)

import Futhark.Analysis.ScalExp
import Futhark.Substitute
import Futhark.Renamer

import qualified Futhark.Representation.ExplicitMemory.Permutation as Perm
import Futhark.Representation.ExplicitMemory.SymSet (SymSet)
import Futhark.Representation.AST.Attributes.Names
import Language.Futhark.Core

import Text.PrettyPrint.Mainland

type Shape = Vector ScalExp
type Indices = Vector ScalExp

data IxFun :: Nat -> * where
  Direct :: Shape n -> IxFun n
  Offset :: IxFun n -> ScalExp -> IxFun n
  Permute :: IxFun n -> Perm.Permutation n -> IxFun n
  Index :: IxFun (m:+:n) -> Indices m -> IxFun n

--- XXX: this is just structural equality, which may be too
--- conservative - unless we normalise first, somehow.
instance Eq (IxFun n) where
  Direct _ == Direct _ = True
  Offset ixfun1 offset1 == Offset ixfun2 offset2 =
    ixfun1 == ixfun2 && offset1 == offset2
  Permute ixfun1 perm1 == Permute ixfun2 perm2 =
    ixfun1 == ixfun2 && perm1 == perm2
  Index (ixfun1 :: IxFun (m1 :+: n)) (is1 :: Indices m1)
    == Index (ixfun2 :: IxFun (m2 :+: n)) (is2 :: Indices m2) =
    case testEquality m1' m2' of
      Nothing -> False
      Just Refl ->
        ixfun1 == ixfun2 && Vec.toList is1 == Vec.toList is2
    where m1' :: SNat m1
          m1' = Vec.sLength is1
          m2' :: SNat m2
          m2' = Vec.sLength is2
  _ == _ = False

instance Show (IxFun n) where
  show (Direct shape) = "Direct (" ++ show shape ++ ")"
  show (Offset fun k) = "Offset (" ++ show fun ++ ", " ++ show k ++ ")"
  show (Permute fun perm) = "Permute (" ++ show fun ++ ", " ++ show perm ++ ")"
  show (Index fun is) = "Index (" ++ show fun ++ ", " ++ show is ++ ")"

instance Pretty (IxFun n) where
  ppr (Direct shape) =
    text "Direct" <> parens (commasep $ map ppr $ Vec.toList shape)
  ppr (Offset fun k) = ppr fun <+> text "+" <+> ppr k
  ppr (Permute fun perm) = ppr fun <> ppr perm
  ppr (Index fun is) = ppr fun <> brackets (commasep $ map ppr $ Vec.toList is)

instance Substitute (IxFun n) where
  substituteNames subst (Direct shape) =
    Direct $ Vec.map (substituteNames subst) shape
  substituteNames subst (Offset fun k) =
    Offset (substituteNames subst fun) (substituteNames subst k)
  substituteNames subst (Permute fun perm) =
    Permute (substituteNames subst fun) perm
  substituteNames subst (Index fun is) =
    Index (substituteNames subst fun) (Vec.map (substituteNames subst) is)

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

index (Direct shape) vec =
  case vec of
    e :- rest -> descend e shape rest
  where descend :: ScalExp -> Shape (S m) -> Indices m -> ScalExp
        descend e (d :- ds) (i :- is) =
          descend ((e `STimes` d) `SPlus` i) ds is
        descend e _ Nil =
          e

index (Offset fun k) vec =
  index fun vec `SPlus` k

index (Permute fun perm) is =
  index fun $ Perm.apply perm is

index (Index fun (is1::Indices m)) is2 =
  case (singInstance $ sLength is1,
        singInstance $ sLength is2 %:- sOne) of
    (SingInstance,SingInstance) ->
      let is :: Indices (m :+ S n)
          is = is1 `Vec.append` is2
          outer = succPlusR (sing :: SNat m) (sing :: SNat n)
          proof :: (m :+ S n) :=: S (m :+ n)
          proof = succPlusR (sing :: SNat m) (sing :: SNat n)
      in case singInstance $ coerce proof (sLength is) %:- sOne of
        SingInstance -> index (coerce outer fun) (coerce outer is)

iota :: Shape n -> IxFun n
iota = Direct

offset :: IxFun n -> ScalExp -> IxFun n
offset = Offset

permute :: IxFun n -> Perm.Permutation n -> IxFun n
permute = Permute

applyInd :: IxFun (m:+:n) -> Indices m -> IxFun n
applyInd = Index

codomain :: IxFun n -> SymSet n
codomain = undefined

-- FIXME: this function is not yet quite there.
linearWithOffset :: IxFun n -> Maybe ScalExp
linearWithOffset (Direct _) = Just $ Val $ IntVal 0
linearWithOffset (Offset ixfun n) = do
  inner_offset <- linearWithOffset ixfun
  return $ inner_offset `SPlus` n
linearWithOffset (Permute {}) = Nothing
linearWithOffset (Index (Direct shape) is) =
  Just $ ssum $ zipWith STimes (Vec.toList is) sliceSizes
  where sliceSizes =
          map sproduct $ drop 1 $ tails $ Vec.toList shape
linearWithOffset (Index {}) =
  Nothing

instance FreeIn (IxFun n) where
  freeIn (Direct shape) = mconcat $ map freeIn $ toList shape
  freeIn (Offset ixfun e) = freeIn ixfun <> freeIn e
  freeIn (Permute ixfun _) = freeIn ixfun
  freeIn (Index ixfun is) =
    freeIn ixfun <> mconcat (map freeIn $ toList is)
