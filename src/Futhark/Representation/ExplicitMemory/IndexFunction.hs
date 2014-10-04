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
       )
       where

import Data.Type.Natural
import Data.Vector.Sized hiding (index, map, unsafeFromInt)
import qualified Data.Vector.Sized as Vec
import Proof.Equational

import Futhark.Analysis.ScalExp

import qualified Futhark.Representation.ExplicitMemory.Permutation as Perm
import Futhark.Representation.ExplicitMemory.SymSet (SymSet)

type Shape = Vector ScalExp
type Indices = Vector ScalExp

data IxFun :: Nat -> * where
  Direct :: Shape n -> IxFun n
  Offset :: IxFun n -> ScalExp -> IxFun n
  Permute :: IxFun n -> Perm.Permutation n -> IxFun n
  Index :: IxFun (m:+:n) -> Indices m -> IxFun n

instance Show (IxFun n) where
  show (Direct shape) = "Direct (" ++ show shape ++ ")"
  show (Offset fun k) = "Offset (" ++ show fun ++ ", " ++ show k ++ ")"
  show (Permute fun perm) = "Permute (" ++ show fun ++ ", " ++ show perm ++ ")"
  show (Index fun is) = "Index (" ++ show fun ++ ", " ++ show is ++ ")"

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
