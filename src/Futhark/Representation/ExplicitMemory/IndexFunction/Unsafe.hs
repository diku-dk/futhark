{-# LANGUAGE RankNTypes, ScopedTypeVariables, DataKinds, FlexibleContexts, ExistentialQuantification, GADTs, TypeOperators, AllowAmbiguousTypes #-}
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
import Data.Type.Natural
import Data.Vector.Sized hiding (index, map, unsafeFromInt)
import qualified Data.Vector.Sized as Vec
import Data.Singletons.Prelude
import Data.Type.Monomorphic
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
permute (IxFun n f) perm =
  IxFun n $ Safe.permute f $ unsafeFromList n perm

applyInd :: IxFun -> Indices -> IxFun
applyInd (IxFun (snnat::SNat (S n)) (f::Safe.IxFun (S n))) is = case toSing m of
  SomeSing (mnat::SNat m) ->
    case mnat %:<<= nnat of
      STrue  ->
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
  where m = intToNat $ Prelude.length is
        nnat :: SNat n
        nnat = snnat %- sOne

codomain :: IxFun -> SymSet
codomain (IxFun _ f) = Safe.codomain f

type SymSet = Safe.SymSet

swapPlusMinus :: forall x y z.(z :<<= (x:+:y)) ~ True =>
                 SNat x -> SNat y -> SNat z
              -> (x :+: (y :-: z)) :=: ((x :+: y) :-: z)
swapPlusMinus _ _ SZ = Refl `trans` Refl
swapPlusMinus SZ _ _ = Refl `trans` Refl
swapPlusMinus x SZ z = case boolToPropLeq z (x %+ SZ) of
  ZeroLeq _ -> undefined
  SuccLeqSucc _ -> undefined
swapPlusMinus (SS x) (SS y) (SS z) =
  undefined

minusPlusEqR :: SNat n -> SNat m -> (m :+: (n :-: m)) :=: n
minusPlusEqR n m = case propToBoolLeq $ plusLeqL m n of
  Dict -> trans (swapPlusMinus m n m) (plusMinusEqR n m)
