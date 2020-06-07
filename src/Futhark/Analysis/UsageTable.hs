{-# LANGUAGE Strict #-}
-- | A usage-table is sort of a bottom-up symbol table, describing how
-- (and if) a variable is used.
module Futhark.Analysis.UsageTable
  ( UsageTable
  , without
  , lookup
  , used
  , expand
  , isConsumed
  , isInResult
  , isUsedDirectly
  , usages
  , usage
  , consumedUsage
  , inResultUsage
  , Usages
  , usageInStm
  )
  where

import Data.Bits
import qualified Data.Foldable as Foldable
import Data.List (foldl')
import qualified Data.Map.Strict as M

import Prelude hiding (lookup)

import Futhark.IR
import Futhark.IR.Prop.Aliases

-- | A usage table.
newtype UsageTable = UsageTable (M.Map VName Usages)
                   deriving (Eq, Show)

instance Semigroup UsageTable where
  UsageTable table1 <> UsageTable table2 =
    UsageTable $ M.unionWith (<>) table1 table2

instance Monoid UsageTable where
  mempty = UsageTable mempty

-- | Remove these entries from the usage table.
without :: UsageTable -> [VName] -> UsageTable
without (UsageTable table) = UsageTable . Foldable.foldl (flip M.delete) table

-- | Look up a variable in the usage table.
lookup :: VName -> UsageTable -> Maybe Usages
lookup name (UsageTable table) = M.lookup name table

lookupPred :: (Usages -> Bool) -> VName -> UsageTable -> Bool
lookupPred f name = maybe False f . lookup name

-- | Is the variable present in the usage table?  That is, has it been used?
used :: VName -> UsageTable -> Bool
used = lookupPred $ const True

-- | Expand the usage table based on aliasing information.
expand :: (VName -> Names) -> UsageTable -> UsageTable
expand look (UsageTable m) = UsageTable $ foldl' grow m $ M.toList m
  where grow m' (k, v) = foldl' (grow'' $ v `withoutU` presentU) m' $
                         namesToList $ look k
        grow'' v m'' k = M.insertWith (<>) k v m''

is :: Usages -> VName -> UsageTable -> Bool
is = lookupPred . matches

-- | Has the variable been consumed?
isConsumed :: VName -> UsageTable -> Bool
isConsumed = is consumedU

-- | Has the variable been used in the 'Result' of a body?
isInResult :: VName -> UsageTable -> Bool
isInResult = is inResultU

-- | Has the given name been used directly (i.e. could we rename it or
-- remove it without anyone noticing?)
isUsedDirectly :: VName -> UsageTable -> Bool
isUsedDirectly = is presentU

-- | Construct a usage table reflecting that these variables have been
-- used.
usages :: Names -> UsageTable
usages names = UsageTable $ M.fromList [ (name, presentU) | name <- namesToList names ]

-- | Construct a usage table where the given variable has been used in
-- this specific way.
usage :: VName -> Usages -> UsageTable
usage name uses = UsageTable $ M.singleton name uses

-- | Construct a usage table where the given variable has been consumed.
consumedUsage :: VName -> UsageTable
consumedUsage name = UsageTable $ M.singleton name consumedU

-- | Construct a usage table where the given variable has been used in
-- the 'Result' of a body.
inResultUsage :: VName -> UsageTable
inResultUsage name = UsageTable $ M.singleton name inResultU

-- | A description of how a single variable has been used.
newtype Usages = Usages Int -- Bitmap representation for speed.
  deriving (Eq, Ord, Show)

instance Semigroup Usages where
  Usages x <> Usages y = Usages $ x .|. y

instance Monoid Usages where
  mempty = Usages 0

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

-- | Produce a usage table reflecting the use of the free variables in
-- a single statement.
usageInStm :: (ASTLore lore, Aliased lore) => Stm lore -> UsageTable
usageInStm (Let pat lore e) =
  mconcat [usageInPat,
           usageInExpLore,
           usageInExp e,
           usages (freeIn e)]
  where usageInPat =
          usages (mconcat (map freeIn $ patternElements pat)
                     `namesSubtract`
                     namesFromList (patternNames pat))
        usageInExpLore =
          usages $ freeIn lore

usageInExp :: Aliased lore => Exp lore -> UsageTable
usageInExp (Apply _ args _ _) =
  mconcat [ mconcat $ map consumedUsage $
            namesToList $ subExpAliases arg
          | (arg,d) <- args, d == Consume ]
usageInExp (DoLoop _ merge _ _) =
  mconcat [ mconcat $ map consumedUsage $
            namesToList $ subExpAliases se
          | (v,se) <- merge, unique $ paramDeclType v ]
usageInExp (If _ tbranch fbranch _) =
  foldMap consumedUsage $ namesToList $
  consumedInBody tbranch <> consumedInBody fbranch
usageInExp (BasicOp (Update src _ _)) =
  consumedUsage src
usageInExp (Op op) =
  mconcat $ map consumedUsage (namesToList $ consumedInOp op)
usageInExp _ = mempty
