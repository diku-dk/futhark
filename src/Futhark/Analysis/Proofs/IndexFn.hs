{-# OPTIONS_GHC -Wno-orphans #-}

module Futhark.Analysis.Proofs.IndexFn where

import Data.List.NonEmpty qualified as NE
import Futhark.Analysis.Proofs.Symbol
import Futhark.SoP.SoP (SoP, int2SoP)
import Language.Futhark (VName)

data IndexFn = IndexFn
  { iterator :: Iterator,
    body :: Cases Symbol (SoP Symbol)
  }
  deriving (Show)

data Domain
  = Iota (SoP Symbol) -- [0, ..., n-1]
  | Cat -- Catenate_{k=0}^{m-1} [b_k, ..., b_{k+1})
      VName -- k
      (SoP Symbol) -- m
      (SoP Symbol) -- b
  deriving (Show)

data Iterator
  = Forall VName Domain
  | Empty
  deriving (Show)

newtype Cases a b = Cases (NE.NonEmpty (a, b))
  deriving (Show, Eq, Ord)

cases :: [(a, b)] -> Cases a b
cases = Cases . NE.fromList

casesToList :: Cases a b -> [(a, b)]
casesToList (Cases cs) = NE.toList cs

getCase :: Int -> Cases a b -> (a, b)
getCase n (Cases cs) = NE.toList cs !! n

getPredicates :: IndexFn -> [Symbol]
getPredicates (IndexFn _ cs) = map fst $ casesToList cs

domainSegStart :: Domain -> SoP Symbol
domainSegStart (Iota _) = int2SoP 0
domainSegStart (Cat _ _ b) = b
