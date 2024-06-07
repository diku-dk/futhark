{-# LANGUAGE Strict #-}

-- | A usage-table is sort of a bottom-up symbol table, describing how
-- (and if) a variable is used.
module Futhark.Analysis.UsageTable
  ( UsageTable,
    without,
    lookup,
    used,
    expand,
    isConsumed,
    isInResult,
    isUsedDirectly,
    isSize,
    usages,
    usage,
    consumedUsage,
    inResultUsage,
    sizeUsage,
    sizeUsages,
    withoutU,
    Usages,
    consumedU,
    presentU,
    usageInStm,
    usageInPat,
  )
where

import Data.Bits
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IM
import Data.List qualified as L
import Futhark.IR
import Futhark.IR.Prop.Aliases
import Prelude hiding (lookup)

-- | A usage table.
newtype UsageTable = UsageTable (IM.IntMap Usages)
  deriving (Eq, Show)

instance Semigroup UsageTable where
  UsageTable table1 <> UsageTable table2 =
    UsageTable $ IM.unionWith (<>) table1 table2

instance Monoid UsageTable where
  mempty = UsageTable mempty

-- | Remove these entries from the usage table.
without :: UsageTable -> [VName] -> UsageTable
without (UsageTable table) =
  UsageTable . Foldable.foldl (flip IM.delete) table . map baseTag

-- | Look up a variable in the usage table.
lookup :: VName -> UsageTable -> Maybe Usages
lookup name (UsageTable table) = IM.lookup (baseTag name) table

lookupPred :: (Usages -> Bool) -> VName -> UsageTable -> Bool
lookupPred f name = maybe False f . lookup name

-- | Is the variable present in the usage table?  That is, has it been used?
used :: VName -> UsageTable -> Bool
used = lookupPred $ const True

-- | Expand the usage table based on aliasing information.
expand :: (VName -> Names) -> UsageTable -> UsageTable
expand look (UsageTable m) = UsageTable $ L.foldl' grow m $ IM.toList m
  where
    grow m' (k, v) =
      L.foldl'
        (grow'' $ v `withoutU` presentU)
        m'
        (namesIntMap $ look $ VName (nameFromString "") k)
    grow'' v m'' k = IM.insertWith (<>) (baseTag k) v m''

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

-- | Is this name used as the size of something (array or memory block)?
isSize :: VName -> UsageTable -> Bool
isSize = is sizeU

-- | Construct a usage table reflecting that these variables have been
-- used.
usages :: Names -> UsageTable
usages = UsageTable . IM.map (const presentU) . namesIntMap

-- | Construct a usage table where the given variable has been used in
-- this specific way.
usage :: VName -> Usages -> UsageTable
usage name uses = UsageTable $ IM.singleton (baseTag name) uses

-- | Construct a usage table where the given variable has been consumed.
consumedUsage :: VName -> UsageTable
consumedUsage name = UsageTable $ IM.singleton (baseTag name) consumedU

-- | Construct a usage table where the given variable has been used in
-- the 'Result' of a body.
inResultUsage :: VName -> UsageTable
inResultUsage name = UsageTable $ IM.singleton (baseTag name) inResultU

-- | Construct a usage table where the given variable has been used as
-- an array or memory size.
sizeUsage :: VName -> UsageTable
sizeUsage name = UsageTable $ IM.singleton (baseTag name) sizeU

-- | Construct a usage table where the given names have been used as
-- an array or memory size.
sizeUsages :: Names -> UsageTable
sizeUsages = UsageTable . IM.map (const (sizeU <> presentU)) . namesIntMap

-- | A description of how a single variable has been used.
newtype Usages = Usages Int -- Bitmap representation for speed.
  deriving (Eq, Ord, Show)

instance Semigroup Usages where
  Usages x <> Usages y = Usages $ x .|. y

instance Monoid Usages where
  mempty = Usages 0

-- | A kind of usage.
consumedU, inResultU, presentU, sizeU :: Usages
consumedU = Usages 1
inResultU = Usages 2
presentU = Usages 4
sizeU = Usages 8

-- | Check whether the bits that are set in the first argument are
-- also set in the second.
matches :: Usages -> Usages -> Bool
matches (Usages x) (Usages y) = x == (x .&. y)

-- | x - y, but for 'Usages'.
withoutU :: Usages -> Usages -> Usages
withoutU (Usages x) (Usages y) = Usages $ x .&. complement y

usageInBody :: (Aliased rep) => Body rep -> UsageTable
usageInBody = foldMap consumedUsage . namesToList . consumedInBody

-- | Produce a usage table reflecting the use of the free variables in
-- a single statement.
usageInStm :: (Aliased rep) => Stm rep -> UsageTable
usageInStm (Let pat rep e) =
  mconcat
    [ usageInPat pat `without` patNames pat,
      usages $ freeIn rep,
      usageInExp e,
      usages (freeIn e)
    ]

-- | Usage table reflecting use in pattern.  In particular, free
-- variables in the decorations are considered used as sizes, even if
-- they are also bound in this pattern.
usageInPat :: (FreeIn t) => Pat t -> UsageTable
usageInPat = sizeUsages . foldMap freeIn . patElems

usageInExp :: (Aliased rep) => Exp rep -> UsageTable
usageInExp (Apply _ args _ _) =
  mconcat [consumedUsage v | (Var v, Consume) <- args]
usageInExp e@Loop {} =
  foldMap consumedUsage $ namesToList $ consumedInExp e
usageInExp (Match _ cases defbody _) =
  foldMap (usageInBody . caseBody) cases <> usageInBody defbody
usageInExp (WithAcc inputs lam) =
  foldMap inputUsage inputs <> usageInBody (lambdaBody lam)
  where
    inputUsage (_, arrs, _) = foldMap consumedUsage arrs
usageInExp (BasicOp (Update _ src _ _)) =
  consumedUsage src
usageInExp (BasicOp (FlatUpdate src _ _)) =
  consumedUsage src
usageInExp (Op op) =
  mconcat $ map consumedUsage (namesToList $ consumedInOp op)
usageInExp (BasicOp _) = mempty
