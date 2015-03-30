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
  , isConsumed
  , isInResult
  , isEqualTo
  , allConsumed
  , usages
  , usage
  , consumedUsage
  , inResultUsage
  , equalToUsage
  , Usages
  , leftScope
  )
  where

import Control.Arrow (first)
import qualified Data.Foldable as Foldable
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Set as S

import Prelude hiding (lookup)

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
contains (UsageTable table) = Foldable.any (`HM.member` table)

without :: UsageTable -> [VName] -> UsageTable
without (UsageTable table) = UsageTable . Foldable.foldl (flip HM.delete) table

lookup :: VName -> UsageTable -> Maybe Usages
lookup name (UsageTable table) = HM.lookup name table

lookupPred :: (Usages -> Bool) -> VName -> UsageTable -> Bool
lookupPred f name = maybe False f . lookup name

used :: VName -> UsageTable -> Bool
used = lookupPred $ const True

keys :: UsageTable -> [VName]
keys (UsageTable table) = HM.keys table

is :: Usage -> VName -> UsageTable -> Bool
is = lookupPred . S.member

isConsumed :: VName -> UsageTable -> Bool
isConsumed = is Consumed

isInResult :: VName -> UsageTable -> Bool
isInResult = is InResult

isEqualTo :: SubExp -> VName -> UsageTable -> Bool
isEqualTo what = is $ EqualTo what

allConsumed :: UsageTable -> Names
allConsumed (UsageTable m) =
  HS.fromList . map fst . filter (S.member Consumed . snd) $ HM.toList m

usages :: Names -> UsageTable
usages names = UsageTable $ HM.fromList [ (name, S.empty) | name <- HS.toList names ]

usage :: VName -> Usages -> UsageTable
usage name uses = UsageTable $ HM.singleton name uses

consumedUsage :: VName -> UsageTable
consumedUsage name = UsageTable $ HM.singleton name $ S.singleton Consumed

inResultUsage :: VName -> UsageTable
inResultUsage name = UsageTable $ HM.singleton name $ S.singleton InResult

equalToUsage :: VName -> SubExp -> UsageTable
equalToUsage name what =
  UsageTable $ HM.singleton name $ S.singleton $ EqualTo what

type Usages = S.Set Usage

data Usage = Consumed
           | InResult
           | EqualTo SubExp
             deriving (Eq, Ord, Show)

leftScope :: UsageTable -> UsageTable
leftScope (UsageTable table) = UsageTable $ HM.map (S.filter $ not . scopeSpecific) table
  where scopeSpecific (EqualTo _) = True
        scopeSpecific InResult    = True
        scopeSpecific _           = False
