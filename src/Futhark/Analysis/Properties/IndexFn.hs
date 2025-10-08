{-# OPTIONS_GHC -Wno-orphans #-}

module Futhark.Analysis.Properties.IndexFn
  ( IndexFn (..),
    Domain (..),
    Quantified (..),
    Cases (..),
    cases,
    casesToList,
    catVar,
    flattenCases,
    getPredicates,
    getCase,
    guards,
    justSingleCase,
    singleCase,
    fromScalar,
    Iterator,
    cmapValues,
    rank,
  )
where

import Data.List.NonEmpty qualified as NE
import Futhark.Analysis.Properties.Symbol
import Futhark.SoP.SoP (SoP, sym2SoP, (.*.), (.+.))
import Language.Futhark (VName)
import Data.Bifunctor (second)

data IndexFn = IndexFn
  { shape :: [Quantified Domain],     --- [[ Quantified Domain ]]
    body :: Cases Symbol (SoP Symbol)
  }
  deriving (Show, Eq)

data Domain
  = Iota (SoP Symbol) -- [0, ..., n-1]
  | Cat -- Catenate_{k=0}^{m-1} [b_k, ..., b_{k+1})
      VName -- k
      (SoP Symbol) -- m
      (SoP Symbol) -- b
  deriving (Show, Eq)

instance Ord Domain where
  Cat {} <= Iota {} = False
  _ <= _ = True

data Quantified a = Forall { boundVar :: VName, formula :: a }
  deriving (Show)

instance Ord a => Ord (Quantified a) where
  Forall _ x <= Forall _ y = x <= y

instance Eq a => Eq (Quantified a) where
  (Forall _ u) == (Forall _ v) = u == v

type Iterator = Quantified Domain

-- instance Eq (Quantified Domain) where
--   (Forall _ u@(Cat k _ _)) == (Forall _ v@(Cat k' _ _)) = u == v && k == k'
--   (Forall _ u) == (Forall _ v) = u == v

-- instance Ord Iterator where
--   (<=) :: Iterator -> Iterator -> Bool
--   _ <= Empty = False
--   _ <= Forall {} = True

newtype Cases a b = Cases (NE.NonEmpty (a, b))
  deriving (Show, Eq, Ord)

rank :: IndexFn -> Int
rank = length . shape

cases :: [(a, b)] -> Cases a b
cases = Cases . NE.fromList

casesToList :: Cases a b -> [(a, b)]
casesToList (Cases cs) = NE.toList cs

getCase :: Int -> Cases a b -> (a, b)
getCase n (Cases cs) = NE.toList cs !! n

getPredicates :: IndexFn -> [Symbol]
getPredicates (IndexFn _ cs) = map fst $ casesToList cs

guards :: IndexFn -> [(Symbol, SoP Symbol)]
guards = casesToList . body

justSingleCase :: IndexFn -> Maybe (SoP Symbol)
justSingleCase f
  | [(Bool True, f_val)] <- casesToList $ body f =
      Just f_val
  | otherwise =
      Nothing

catVar :: Quantified Domain -> Maybe VName
catVar (Forall _ (Cat k _ _)) = Just k
catVar _ = Nothing

flattenCases :: (Ord u) => Cases u (SoP u) -> SoP u
flattenCases cs = foldl1 (.+.) [sym2SoP p .*. q | (p, q) <- casesToList cs]

singleCase :: a -> Cases Symbol a
singleCase e = cases [(Bool True, e)]

fromScalar :: SoP Symbol -> [IndexFn]
fromScalar e = [IndexFn [] (singleCase e)]

cmap :: ((a, b) -> (c, d)) -> Cases a b -> Cases c d
cmap f (Cases xs) = Cases (fmap f xs)

cmapValues :: (b -> c) -> Cases a b -> Cases a c
cmapValues f = cmap (second f)
