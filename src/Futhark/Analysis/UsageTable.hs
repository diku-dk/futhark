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
    UsageTable $ M.unionWith S.union table1 table2

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

keys :: UsageTable -> [VName]
keys (UsageTable table) = M.keys table

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
  S.fromList . map fst . filter (S.member Consumed . snd) $ M.toList m

usages :: Names -> UsageTable
usages names = UsageTable $ M.fromList [ (name, S.empty) | name <- S.toList names ]

usage :: VName -> Usages -> UsageTable
usage name uses = UsageTable $ M.singleton name uses

consumedUsage :: VName -> UsageTable
consumedUsage name = UsageTable $ M.singleton name $ S.singleton Consumed

inResultUsage :: VName -> UsageTable
inResultUsage name = UsageTable $ M.singleton name $ S.singleton InResult

equalToUsage :: VName -> SubExp -> UsageTable
equalToUsage name what =
  UsageTable $ M.singleton name $ S.singleton $ EqualTo what

type Usages = S.Set Usage

data Usage = Consumed
           | InResult
           | EqualTo SubExp
             deriving (Eq, Ord, Show)

leftScope :: UsageTable -> UsageTable
leftScope (UsageTable table) = UsageTable $ M.map (S.filter $ not . scopeSpecific) table
  where scopeSpecific (EqualTo _) = True
        scopeSpecific InResult    = True
        scopeSpecific _           = False
