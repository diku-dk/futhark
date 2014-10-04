{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures, ScopedTypeVariables #-}
module Futhark.Representation.ExplicitMemory.SymSet
       ( SymSet (..)
       , singleton
       , empty
       , intersection
       , union
       , null
       , shrink
       , freeVars
       )
       where

import Prelude hiding (null)

import Data.Type.Natural
import Data.Type.Ordinal
import qualified Data.HashSet as HS

import Language.Futhark.Core
import Futhark.Analysis.ScalExp

type Range = (ScalExp,ScalExp)

data SymSet :: Nat -> * where
  Singleton :: Int -> SymSet Z

singleton :: Int -> SymSet Z
singleton = Singleton

union :: SymSet n -> SymSet m -> SymSet (n :+: m)
union = undefined

intersection :: SymSet n -> SymSet m -> SymSet (n :+: m)
intersection = undefined

shrink :: Ordinal (S n) -> Range -> SymSet (S n) -> SymSet n
shrink = undefined

null :: SymSet n -> ScalExp
null = undefined

empty :: SymSet a
empty = undefined

freeVars :: SymSet a -> HS.HashSet VName
freeVars = undefined
