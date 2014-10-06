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
         -- * Utility
       , shapeFromSubExps
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
import Futhark.Representation.AST.Syntax (SubExp(..), Value(..))

import Futhark.Representation.ExplicitMemory.Permutation
  (Swap (..), Permutation (..))
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as Safe
import qualified Futhark.Representation.ExplicitMemory.SymSet as SymSet

data IxFun = forall n . IxFun (SNat (S n)) (Safe.IxFun (S n))

instance Show IxFun where
  show (IxFun _ fun) = show fun

type Indices = [ScalExp]
type Shape   = [ScalExp]

shapeFromSubExps :: [SubExp] -> Shape
shapeFromSubExps = map fromSubExp
  where fromSubExp (Var v)                   = Id v
        fromSubExp (Constant (BasicVal v) _) = Val v
        fromSubExp _                         = error "Not a basic value"

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
    Prelude.foldr buildPermutation Identity (Prelude.zip [0..] perm)
  where buildPermutation (to,from) perm' =
          let sw :: Swap (S n)
              sw = withSingI n $
                   unsafeFromInt from :<->: unsafeFromInt to
          in sw :>>: perm'

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
codomain (IxFun n f) =
  SymSet n $ Safe.codomain f

data SymSet = forall n . SymSet (SNat n) (SymSet.SymSet n)

swapPlusMinus :: forall x y z.(z :<<= y) ~ True =>
                 SNat x -> SNat y -> SNat z
              -> (x :+: (y :-: z)) :=: ((x :+: y) :-: z)
swapPlusMinus _ _ SZ = Refl `trans` Refl
swapPlusMinus SZ _ _ = Refl `trans` Refl
swapPlusMinus _ SZ z = case boolToPropLeq z SZ of
  ZeroLeq _ -> Refl
swapPlusMinus (SS (x' :: SNat x')) y z =
  case propToBoolLeq prop of
    Dict ->
      let p1 :: (S x' :+: (y :-: z)) :=: S (x' :+: (y :-: z))
          p1 = succPlusL x' (y %- z)
          p2 :: S (x' :+: (y :-: z)) :=: S ((x' :+: y) :-: z)
          p2 = eqPreservesS $ swapPlusMinus x' y z
          p3 :: S ((x' :+: y) :-: z) :=: (S (x' :+: y) :-: z)
          p3 = sym $ eqSuccMinus (x' %+ y) z
          p4 :: (S (x' :+: y) :-: z) :=: ((S x' :+: y) :-: z)
          p4 = minusCongEq (sym $ succPlusL x' y) z
      in p1 `trans` p2 `trans` p3 `trans` p4
  where prop :: Leq z (x' :+: y)
        prop = plusMonotone (ZeroLeq x') (boolToPropLeq z y)

minusPlusEqR :: (m :<<= n) ~ True =>
                SNat n -> SNat m -> (m :+: (n :-: m)) :=: n
minusPlusEqR n m =
  swapPlusMinus m n m `trans` plusMinusEqR n m
