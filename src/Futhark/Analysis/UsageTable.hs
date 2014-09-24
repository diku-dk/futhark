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
  , isPredicate
  , isConsumed
  , allConsumed
  , usages
  , usage
  , predicateUsage
  , consumedUsage
  , Usages
  )
  where

import Prelude hiding (lookup, any, foldl)

import Control.Arrow (first)
import Data.Foldable
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Set as S

import Futhark.Substitute
import Futhark.Representation.AST

newtype UsageTable = UsageTable (HM.HashMap VName Usages)
                   deriving (Eq, Show)

instance Monoid UsageTable where
  mempty = empty
  UsageTable table1 `mappend` UsageTable table2 =
    UsageTable $ HM.unionWith S.union table1 table2

instance Substitute UsageTable where
  substituteNames subst (UsageTable table)
    | not $ HM.null $ subst `HM.intersection` table =
      UsageTable $ HM.fromList $
      map (first $ substituteNames subst) $ HM.toList table
    | otherwise = UsageTable table

empty :: UsageTable
empty = UsageTable HM.empty

contains :: UsageTable -> [VName] -> Bool
contains (UsageTable table) = any (`HM.member` table)

without :: UsageTable -> [VName] -> UsageTable
without (UsageTable table) = UsageTable . foldl (flip HM.delete) table

lookup :: VName -> UsageTable -> Maybe Usages
lookup name (UsageTable table) = HM.lookup name table

lookupPred :: (Usages -> Bool) -> VName -> UsageTable -> Bool
lookupPred f name = maybe False f . lookup name

used :: VName -> UsageTable -> Bool
used = lookupPred $ const True

keys :: UsageTable -> [VName]
keys (UsageTable table) = HM.keys table

isPredicate :: VName -> UsageTable -> Bool
isPredicate = lookupPred $ S.member Predicate

isConsumed :: VName -> UsageTable -> Bool
isConsumed = lookupPred $ S.member Consumed

allConsumed :: UsageTable -> Names
allConsumed (UsageTable m) =
  HS.fromList . map fst . filter (S.member Consumed . snd) $ HM.toList m

usages :: Names -> UsageTable
usages names = UsageTable $ HM.fromList [ (name, S.empty) | name <- HS.toList names ]

usage :: VName -> Usages -> UsageTable
usage name uses = UsageTable $ HM.singleton name uses

predicateUsage :: VName -> UsageTable
predicateUsage name = UsageTable $ HM.singleton name $ S.singleton Predicate

consumedUsage :: VName -> UsageTable
consumedUsage name = UsageTable $ HM.singleton name $ S.singleton Consumed

type Usages = S.Set Usage

data Usage = Predicate
           | Consumed
             deriving (Eq, Ord, Show)
