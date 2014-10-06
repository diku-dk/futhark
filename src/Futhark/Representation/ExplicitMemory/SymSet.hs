{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures, ScopedTypeVariables, AllowAmbiguousTypes #-}
module Futhark.Representation.ExplicitMemory.SymSet
       ( SymSet
       , singleton
       , empty
       , intersection
       , union
       , null
       , freeVars
       , fix
       )
       where

import Prelude hiding (null)

import Data.Constraint hiding (trans)
import Data.Singletons.Prelude
import Data.Monoid
import Data.Type.Natural
import Data.Type.Ordinal
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector.Sized as Vec
import Proof.Equational

import Language.Futhark.Core
import Futhark.Representation.AST.Syntax (identName)
import Futhark.Analysis.ScalExp

type Range = (ScalExp,ScalExp)

data SetOp = Intersect | Union

data SymSet :: Nat -> * where
  SetOp :: SetOp -> SNat n -> SymSet n -> SNat m -> SymSet m -> SymSet (n :+: m)
  Empty :: SymSet Z
  Bound :: Vec.Vector VName n
        -> HM.HashMap VName Range
        -> ScalExp -> SymSet n

sParameters :: SymSet n -> SNat n
sParameters (SetOp _ n _ m _) = n %:+ m
sParameters Empty = SZ
sParameters (Bound params _ _) = Vec.sLength params

singleton :: ScalExp -> SymSet Z
singleton = Bound Vec.Nil HM.empty

union :: SymSet n -> SymSet m -> SymSet (n :+: m)
union s1 s2 = SetOp Union (sParameters s1) s1 (sParameters s2) s2

intersection :: SymSet n -> SymSet m -> SymSet (n :+: m)
intersection s1 s2 = SetOp Intersect (sParameters s1) s1 (sParameters s2) s2

null :: SymSet n -> ScalExp
null Empty = Val $ LogVal True
null (SetOp Intersect _ x _ y) = null x `SLogOr` null y
null (SetOp Union _ _ _ _) = Val $ LogVal False
null (Bound {}) = Val $ LogVal False

empty :: SymSet Z
empty = Empty

freeVars :: SymSet a -> HS.HashSet VName
freeVars (SetOp _ _ s1 _ s2) = freeVars s1 <> freeVars s2
freeVars Empty = mempty
freeVars (Bound vars ranged e) =
  HS.fromList (map identName $ getIds e) `HS.difference`
  HS.fromList (Vec.toList vars ++ HM.keys ranged)

fix :: forall k .
       Ordinal (S k) -> Range -> SymSet (S k) -> SymSet k
fix i replacement (Bound vars ranged sexp) =
  Bound vars' (HM.insert replacee replacement ranged) sexp
  where vars' :: Vec.Vector VName k
        replacee :: VName
        (vars',replacee) = findReplacee 0 vars
        findReplacee :: forall m.
                        Int -> Vec.Vector VName (S m)
                     -> (Vec.Vector VName m, VName)
        findReplacee j (x Vec.:- xs)
          | ordToInt i == j = (xs, x)
          | otherwise       =
            case xs of
              Vec.Nil -> (Vec.Nil, x)
              y Vec.:- ys -> let (xs', k) = findReplacee j (y Vec.:- ys)
                             in (x Vec.:- xs', k)

fix i replacement (SetOp op (n::SNat n) x (m::SNat m) y) =
  case ordToSNat' i of
    CastedOrdinal (inat::SNat i) ->
      case (SS inat %:<<= n,
            SS (inat %:- n) %:<<= m) of
        (STrue, SFalse) ->
          let nLeastOne :: Leq One n
              nLeastOne = SuccLeqSucc (ZeroLeq inat) `leqTrans`
                          boolToPropLeq (SS inat) n
          in case propToBoolLeq nLeastOne of
            Dict ->
              let nWithS :: n :=: S (n :-: One)
                  nWithS = Refl `trans` eqSuccMinus n sOne
                  sizeCorrect :: ((n :-: One) :+: m) :=: k
                  sizeCorrect =
                    sym $ Refl `trans`
                    minusCongEq (Refl :: S k :=: (n :+: m)) sOne `trans`
                    plusMinusCommutesL n m sOne
                  i' :: Ordinal n
                  i' = sNatToOrd' n inat
                  x' :: SymSet (n :-: One)
                  x' = coerce Refl $
                       fix (coerce nWithS i') replacement (coerce nWithS x)
                  u :: SymSet ((n :-: One) :+: m)
                  u = SetOp op (n %:- sOne) x' m y
              in coerce sizeCorrect u
        (SFalse, STrue) ->
          let mLeastOne :: Leq One m
              mLeastOne = boolToPropLeq sOne (SS (inat %:- n)) `leqTrans` mInBounds
              mInBounds :: Leq (S (i :-: n)) m
              mInBounds = boolToPropLeq (SS (inat %:- n)) m
          in case propToBoolLeq mLeastOne of
            Dict ->
              let mWithS :: m :=: S (m :-: One)
                  mWithS = Refl `trans` eqSuccMinus m sOne
                  sizeCorrect :: (n :+: (m :-: One)) :=: k
                  sizeCorrect =
                    sym $ Refl `trans`
                    minusCongEq (Refl :: S k :=: (n :+: m)) sOne `trans`
                    plusMinusCommutesR n m sOne
                  i' :: Ordinal m
                  i' = sNatToOrd' m (inat %:- n)
                  y' :: SymSet (m :-: One)
                  y' = coerce Refl $
                       fix (coerce mWithS i') replacement (coerce mWithS y)
                  u :: SymSet (n :+: (m :-: One))
                  u = SetOp op n x (m %:- sOne) y'
              in coerce sizeCorrect u
        _ ->
          error "Bug in mathematics"

-- Cannot figure out how to prove this one yet.
plusMinusCommutesL :: (k :<<= n) ~ True =>
                     SNat n -> SNat m -> SNat k
                  -> ((n :+: m) :-: k) :=: ((n :-: k) :+: m)
plusMinusCommutesL = undefined

-- Or this one.
plusMinusCommutesR :: (k :<<= m) ~ True =>
                     SNat n -> SNat m -> SNat k
                  -> ((n :+: m) :-: k) :=: (n :+: (m :-: k))
plusMinusCommutesR = undefined
