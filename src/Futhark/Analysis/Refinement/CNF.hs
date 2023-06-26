module Futhark.Analysis.Refinement.CNF
  ( CNF (..),
    DNF (..),
    Or (..),
    And (..),
    (&&&),
    (|||),
    cnfTrue,
    cnfFalse,
    cnfNub,
    cnfIsValid,
    cnfIsValidM,
    toDNF,
    listsToCNF,
    cnfToLists,
    dnfToLists,
    negateCNF,
    atomCNF,
    justAnds,
  )
where

import Control.Applicative
import Control.Monad
import Futhark.SoP.Util
import Futhark.Util
import Futhark.Util.Pretty

-- Wrapper around a list (instead of a set) because we want to add
-- 'Monad' instances.
newtype And a = And {ands :: [a]}
  deriving (Show, Eq, Ord)

newtype Or a = Or {ors :: [a]}
  deriving (Show, Eq, Ord)

newtype CNF a = CNF {getCNF :: And (Or a)}
  deriving (Show, Eq, Ord)

newtype DNF a = DNF {getDNF :: Or (And a)}
  deriving (Show, Eq, Ord)

instance (Pretty a) => Pretty (Or a) where
  pretty =
    concatWith (surround " ∨ ") . map pretty . ors

instance (Pretty a) => Pretty (And a) where
  pretty =
    concatWith (surround " ∧ ") . map (parens . pretty) . ands

instance (Pretty a) => Pretty (CNF a) where
  pretty = pretty . getCNF

instance (Pretty a) => Pretty (DNF a) where
  pretty = pretty . getDNF

atomCNF :: a -> CNF a
atomCNF a = listsToCNF [[a]]

cnfFalse :: CNF a
cnfFalse = CNF $ And [Or mempty]

cnfTrue :: CNF a
cnfTrue = CNF $ And mempty

cnfToLists :: CNF a -> [[a]]
cnfToLists = map ors . ands . getCNF

dnfToLists :: DNF a -> [[a]]
dnfToLists = map ands . ors . getDNF

listsToCNF :: [[a]] -> CNF a
listsToCNF = CNF . And . map Or

listsToDNF :: [[a]] -> DNF a
listsToDNF = DNF . Or . map And

cnfNub :: (Ord a) => CNF a -> CNF a
cnfNub = listsToCNF . nubOrd . map nubOrd . cnfToLists

isFalse :: CNF a -> Bool
isFalse (CNF (And [Or as])) = null as
isFalse _ = False

negateCNF :: (a -> a) -> CNF a -> CNF a
negateCNF = fmap

(&&&) :: CNF a -> CNF a -> CNF a
xss &&& yss
  | isFalse xss || isFalse yss = cnfFalse
  | otherwise = listsToCNF $ cnfToLists xss <> cnfToLists yss

infixr 3 &&&

(|||) :: CNF a -> CNF a -> CNF a
xss ||| yss
  | isFalse xss = yss
  | isFalse yss = xss
  | otherwise = listsToCNF $ do
      xs <- cnfToLists xss
      x <- xs
      ys <- cnfToLists yss
      pure $ x : ys

infixr 2 |||

cnfIsValid :: (a -> Bool) -> CNF a -> Bool
cnfIsValid isValid = all (any isValid) . cnfToLists

cnfIsValidM :: (Monad m) => (a -> m Bool) -> CNF a -> m Bool
cnfIsValidM isValid = allM (anyM isValid) . cnfToLists

toDNF :: CNF a -> DNF a
toDNF = listsToDNF . toOrs . cnfToLists
  where
    toOrs [] = [[]]
    toOrs (or' : ors') =
      [a : and' | and' <- toOrs ors', a <- or']

instance Functor CNF where
  fmap f = listsToCNF . (map . map) f . cnfToLists

instance Functor DNF where
  fmap f = listsToDNF . (map . map) f . dnfToLists

instance Applicative CNF where
  pure = listsToCNF . pure . pure
  liftA2 = liftM2

instance Monad CNF where
  xss >>= f =
    foldr (((&&&)) . foldr (|||) cnfFalse) cnfTrue ((map . map) f (cnfToLists xss))

instance Alternative CNF where
  empty = cnfFalse
  (<|>) = (|||)

instance MonadPlus CNF

instance Semigroup (CNF a) where
  (<>) = (&&&)

instance Monoid (CNF a) where
  mempty = cnfFalse

instance Foldable CNF where
  foldMap f = (foldMap . foldMap) f . cnfToLists

instance Foldable DNF where
  foldMap f = (foldMap . foldMap) f . dnfToLists

instance Traversable CNF where
  traverse f = fmap listsToCNF . (traverse . traverse) f . cnfToLists

justAnds :: CNF a -> Bool
justAnds = all (\c -> length c <= 1) . cnfToLists
