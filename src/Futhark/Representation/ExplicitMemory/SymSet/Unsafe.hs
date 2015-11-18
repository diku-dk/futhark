{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures, ScopedTypeVariables #-}
module Futhark.Representation.ExplicitMemory.SymSet.Unsafe
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

import Data.Singletons.Prelude
import Data.Type.Monomorphic
import Data.Type.Natural hiding (n1, n2)
import Data.Type.Ordinal
import qualified Data.HashSet as HS

import Language.Futhark.Core
import Futhark.Analysis.ScalExp

import qualified Futhark.Representation.ExplicitMemory.SymSet as Safe

type Range = Safe.Range

data SymSet = forall n . SymSet (SNat n) (Safe.SymSet n)

singleton :: ScalExp -> SymSet
singleton e = SymSet sZero (Safe.singleton e)

union :: SymSet -> SymSet -> SymSet
union (SymSet n1 s1) (SymSet n2 s2) =
  SymSet (n1 %+ n2) (Safe.union s1 s2)

intersection :: SymSet -> SymSet -> SymSet
intersection (SymSet n1 s1) (SymSet n2 s2) =
  SymSet (n1 %+ n2) (Safe.intersection s1 s2)

null :: SymSet -> ScalExp
null (SymSet _ s) = Safe.null s

empty :: SymSet
empty = SymSet sZero Safe.empty

freeVars :: SymSet -> HS.HashSet VName
freeVars (SymSet _ s) = Safe.freeVars s

fix :: Int -> Range -> SymSet -> SymSet
fix iint range (SymSet (n::SNat n) s) =
  case n of
    SZ ->
      error "SymSet.Unsafe.fix: no free parameters"
    SS (n'::SNat n') ->
      case promote iint :: Monomorphic (Sing :: Nat -> *) of
        Monomorphic (i::SNat m) ->
          case i %:<<= n' of
            SFalse ->
              error "SymSet.Unsafe.fix: index out of bounds"
            STrue ->
              let i' = sNatToOrd' (SS n') i
                  s' :: Safe.SymSet n'
                  s' = Safe.fix i' range s
              in SymSet n' s'
