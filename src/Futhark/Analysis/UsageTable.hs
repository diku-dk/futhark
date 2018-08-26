{-# LANGUAGE Strict #-}
-- | A usage-table is sort of a bottom-up symbol table, describing how
-- (and if) a variable is used.
module Futhark.Analysis.UsageTable
  ( UsageTable
  , empty
  , contains
  , without
  , lookup
  , keys
  , used
  , expand
  , isConsumed
  , isInResult
  , isUsedDirectly
  , allConsumed
  , usages
  , usage
  , consumedUsage
  , inResultUsage
  , Usages
  , leftScope
  )
  where

import Control.Arrow (first)
import Data.Bits
import qualified Data.Foldable as Foldable
import Data.List (foldl')
import Data.Semigroup ((<>))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Semigroup as Sem

import Prelude hiding (lookup)

import Futhark.Transform.Substitute
import Futhark.Representation.AST

newtype UsageTable = UsageTable (M.Map VName Usages)
                   deriving (Eq, Show)

instance Sem.Semigroup UsageTable where
  UsageTable table1 <> UsageTable table2 =
    UsageTable $ M.unionWith (<>) table1 table2

instance Monoid UsageTable where
  mempty = empty
  mappend = (Sem.<>)

instance Substitute UsageTable where
  substituteNames subst (UsageTable table)
    | not $ M.null $ subst `M.intersection` table =
      UsageTable $ M.fromList $
      map (first $ substituteNames subst) $ M.toList table
    | otherwise = UsageTable table

empty :: UsageTable
empty = UsageTable M.empty

contains :: UsageTable -> [VName] -> Bool
contains (UsageTable table) = Foldable.any (`M.member` table)

without :: UsageTable -> [VName] -> UsageTable
without (UsageTable table) = UsageTable . Foldable.foldl (flip M.delete) table

lookup :: VName -> UsageTable -> Maybe Usages
lookup name (UsageTable table) = M.lookup name table

lookupPred :: (Usages -> Bool) -> VName -> UsageTable -> Bool
lookupPred f name = maybe False f . lookup name

used :: VName -> UsageTable -> Bool
used = lookupPred $ const True

-- | Expand the usage table based on aliasing information.
expand :: (VName -> Names) -> UsageTable -> UsageTable
expand look (UsageTable m) = UsageTable $ foldl' grow m $ M.toList m
  where grow m' (k, v) = foldl' (grow'' $ v `withoutU` presentU) m' $ look k
        grow'' v m'' k = M.insertWith (<>) k v m''

keys :: UsageTable -> [VName]
keys (UsageTable table) = M.keys table

is :: Usages -> VName -> UsageTable -> Bool
is = lookupPred . matches

isConsumed :: VName -> UsageTable -> Bool
isConsumed = is consumedU

isInResult :: VName -> UsageTable -> Bool
isInResult = is inResultU

-- | Has the given name been used directly (i.e. could we rename it or
-- remove it without anyone noticing?)
isUsedDirectly :: VName -> UsageTable -> Bool
isUsedDirectly = is presentU

allConsumed :: UsageTable -> Names
allConsumed (UsageTable m) =
  S.fromList . map fst . filter (matches consumedU . snd) $ M.toList m

usages :: Names -> UsageTable
usages names = UsageTable $ M.fromList [ (name, presentU) | name <- S.toList names ]

usage :: VName -> Usages -> UsageTable
usage name uses = UsageTable $ M.singleton name uses

consumedUsage :: VName -> UsageTable
consumedUsage name = UsageTable $ M.singleton name consumedU

inResultUsage :: VName -> UsageTable
inResultUsage name = UsageTable $ M.singleton name inResultU

newtype Usages = Usages Int
  deriving (Eq, Ord, Show)

instance Sem.Semigroup Usages where
  Usages x <> Usages y = Usages $ x .|. y

instance Monoid Usages where
  mempty = Usages 0
  mappend = (Sem.<>)

consumedU, inResultU, presentU :: Usages
consumedU = Usages 1
inResultU = Usages 2
presentU = Usages 4

-- | Check whether the bits that are set in the first argument are
-- also set in the second.
matches :: Usages -> Usages -> Bool
matches (Usages x) (Usages y) = x == (x .&. y)

-- | x - y, but for Usages.
withoutU :: Usages -> Usages -> Usages
withoutU (Usages x) (Usages y) = Usages $ x .&. complement y

leftScope :: UsageTable -> UsageTable
leftScope (UsageTable table) = UsageTable $ M.map (`withoutU` inResultU) table
