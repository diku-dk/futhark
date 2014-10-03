{-# LANGUAGE RankNTypes, ScopedTypeVariables, DataKinds, FlexibleContexts, ExistentialQuantification, TypeOperators, AllowAmbiguousTypes, TypeFamilies #-}
module Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe
       (
         IxFun
       , index
       , iota
       , offset
       , permute
       , applyInd
       , codomain
       , SymSet
       )
       where

import Data.Constraint (Dict(..))
import Data.List (sort)
import Data.Singletons.Prelude
import Data.Type.Monomorphic
import Data.Type.Natural
import Data.Type.Ordinal
import Data.Vector.Sized hiding (index, map, unsafeFromInt)
import Proof.Equational

import Futhark.Analysis.ScalExp

import qualified Futhark.Representation.ExplicitMemory.IndexFunction as Safe

data IxFun = forall n . IxFun (SNat (S n)) (Safe.IxFun (S n))

instance Show IxFun where
  show (IxFun _ fun) = show fun

type Indices = [ScalExp]
type Shape   = [ScalExp]

index :: IxFun -> Indices -> ScalExp
index f is = case f of
  IxFun n f' -> Safe.index f' $ unsafeFromList n is

iota :: Shape -> IxFun
iota shape = case toSing (n-1) of
  SomeSing (sb::SNat n) -> IxFun (SS sb) (Safe.iota $ unsafeFromList (SS sb) shape)
  where n = intToNat $ Prelude.length shape

offset :: IxFun -> ScalExp -> IxFun
offset (IxFun n f) se =
  IxFun n $ Safe.offset f se

permute :: IxFun -> [Int] -> IxFun
permute (IxFun (n::SNat (S n)) f) perm
  | sort perm /= [0..sNatToInt n-1] =
    error "IndexFunction.Unsafe.permute: invalid permutation"
  | otherwise =
    IxFun n $ Safe.permute f $
    Prelude.foldr buildPermutation Safe.Identity (Prelude.zip [0..] perm)
  where buildPermutation (to,from) perm' =
          let sw :: Safe.Swap (S n)
              sw = withSingI n $
                   unsafeFromInt from Safe.:-> unsafeFromInt to
          in sw Safe.:%>%: perm'

applyInd :: IxFun -> Indices -> IxFun
applyInd (IxFun (snnat::SNat (S n)) (f::Safe.IxFun (S n))) is =
  case promote (Prelude.length is) :: Monomorphic (Sing :: Nat -> *) of
    Monomorphic (mnat::SNat m) ->
      case mnat %:<<= nnat of
        STrue ->
          let k :: SNat (S (n :- m))
              k = SS $ nnat %- mnat
              nmnat :: SNat (n :- m)
              nmnat = nnat %- mnat
              is' :: Safe.Indices m
              is' = unsafeFromList mnat is
              proof :: S n :=: (m :+: S (n :- m))
              proof = sym $
                      trans (succPlusR mnat nmnat)
                      (succCongEq (minusPlusEqR nnat mnat))
              f' :: Safe.IxFun (m :+: S (n :- m))
              f' = coerce proof f
              ixfun :: Safe.IxFun (S (n :- m))
              ixfun = Safe.applyInd f' is'
          in IxFun k ixfun
        SFalse -> error "IndexFunction.Unsafe.applyInd: Too many indices given."
  where nnat :: SNat n
        nnat = snnat %- sOne

codomain :: IxFun -> SymSet
codomain (IxFun _ f) = Safe.codomain f

type SymSet = Safe.SymSet

-- FIXME: I cannot figure out how to prove this yet.
swapPlusMinus :: forall x y z.(z :<<= (x:+:y)) ~ True =>
                 SNat x -> SNat y -> SNat z
              -> (x :+: (y :-: z)) :=: ((x :+: y) :-: z)
swapPlusMinus _ _ SZ = Refl `trans` Refl
swapPlusMinus SZ _ _ = Refl `trans` Refl
swapPlusMinus x SZ z = case boolToPropLeq z (x %+ SZ) of
  ZeroLeq _ -> Refl `trans` Refl -- if z is zero
  SuccLeqSucc _ -> undefined
swapPlusMinus (SS _) (SS _) (SS _) =
  undefined

minusPlusEqR :: SNat n -> SNat m -> (m :+: (n :-: m)) :=: n
minusPlusEqR n m = case propToBoolLeq $ plusLeqL m n of
  Dict -> swapPlusMinus m n m `trans` plusMinusEqR n m
