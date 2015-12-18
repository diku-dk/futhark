{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Definition of the lore used by the simplification engine.
module Futhark.Optimise.Simplifier.Lore
       (
         Wise (..)
       , removeBindingWisdom
       , removeLambdaWisdom
       , removeExtLambdaWisdom
       , removeProgWisdom
       , removeFunDecWisdom
       , removeExpWisdom
       , removePatternWisdom
       , removeBodyWisdom
       , addWisdomToPattern
       , mkWiseBody
       , mkWiseLetBinding

       , CanBeWise (..)
       )
       where

import Control.Applicative
import Data.Monoid

import Futhark.Representation.AST
import qualified Futhark.Representation.AST.Lore as Lore
import Futhark.Representation.AST.Attributes.Ranges
import Futhark.Representation.Aliases
  (unNames, Names' (..), VarAliases, ConsumedInExp)
import qualified Futhark.Representation.Aliases as Aliases
import qualified Futhark.Representation.Ranges as Ranges
import Futhark.Binder
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Analysis.Rephrase

import Prelude

data Wise lore = Wise lore

-- | The wisdom of the let-bound variable.
data VarWisdom = VarWisdom { varWisdomAliases :: VarAliases
                           , varWisdomRange :: Range
                           }
                  deriving (Eq, Ord, Show)

instance Rename VarWisdom where
  rename (VarWisdom als range) = VarWisdom <$> rename als <*> rename range

instance Substitute VarWisdom where
  substituteNames substs (VarWisdom als range) =
    VarWisdom (substituteNames substs als) (substituteNames substs range)

instance FreeIn VarWisdom where
  freeIn (VarWisdom als range) = freeIn als <> freeIn range

-- | Wisdom about an expression.
type ExpWisdom = ConsumedInExp

-- | Wisdom about a body.
data BodyWisdom = BodyWisdom { bodyWisdomAliases :: [VarAliases]
                             , bodyWisdomConsumed :: ConsumedInExp
                             , bodyWisdomRanges :: [Range]
                             }
                  deriving (Eq, Ord, Show)

instance Rename BodyWisdom where
  rename (BodyWisdom als cons rs) =
    BodyWisdom <$> rename als <*> rename cons <*> rename rs

instance Substitute BodyWisdom where
  substituteNames substs (BodyWisdom als cons rs) =
    BodyWisdom
    (substituteNames substs als)
    (substituteNames substs cons)
    (substituteNames substs rs)

instance FreeIn BodyWisdom where
  freeIn (BodyWisdom als cons rs) =
    freeIn als <> freeIn cons <> freeIn rs

instance (Annotations lore,
          CanBeWise (Op lore)) => Annotations (Wise lore) where
  type LetAttr (Wise lore) = (VarWisdom, LetAttr lore)
  type ExpAttr (Wise lore) = (ExpWisdom, ExpAttr lore)
  type BodyAttr (Wise lore) = (BodyWisdom, BodyAttr lore)
  type FParamAttr (Wise lore) = FParamAttr lore
  type LParamAttr (Wise lore) = LParamAttr lore
  type RetType (Wise lore) = RetType lore
  type Op (Wise lore) = OpWithWisdom (Op lore)

instance (Lore.Lore lore, CanBeWise (Op lore)) => Lore.Lore (Wise lore) where
  representative =
    Wise Lore.representative

  loopResultContext (Wise lore) =
    Lore.loopResultContext lore

instance (Renameable lore, CanBeWise (Op lore)) => Renameable (Wise lore) where
instance (Substitutable lore, CanBeWise (Op lore)) => Substitutable (Wise lore) where
instance (PrettyLore lore, CanBeWise (Op lore)) => PrettyLore (Wise lore) where
instance (Proper lore,
          CanBeWise (Op lore)) => Proper (Wise lore) where

instance AliasesOf (VarWisdom, attr) where
  aliasesOf = unNames . varWisdomAliases . fst

instance RangeOf (VarWisdom, attr) where
  rangeOf = varWisdomRange . fst

instance RangesOf (BodyWisdom, attr) where
  rangesOf = bodyWisdomRanges . fst

instance (Lore.Lore lore, CanBeWise (Op lore)) => Aliased (Wise lore) where
  bodyAliases = map unNames . bodyWisdomAliases . fst . bodyLore
  consumedInBody = unNames . bodyWisdomConsumed . fst . bodyLore

removeWisdom :: CanBeWise (Op lore) => Rephraser (Wise lore) lore
removeWisdom = Rephraser { rephraseExpLore = snd
                         , rephraseLetBoundLore = snd
                         , rephraseBodyLore = snd
                         , rephraseFParamLore = id
                         , rephraseLParamLore = id
                         , rephraseRetType = id
                         , rephraseOp = removeOpWisdom
                         }

removeProgWisdom :: CanBeWise (Op lore) => Prog (Wise lore) -> Prog lore
removeProgWisdom = rephraseProg removeWisdom

removeFunDecWisdom :: CanBeWise (Op lore) => FunDec (Wise lore) -> FunDec lore
removeFunDecWisdom = rephraseFunDec removeWisdom

removeBindingWisdom :: CanBeWise (Op lore) => Binding (Wise lore) -> Binding lore
removeBindingWisdom = rephraseBinding removeWisdom

removeLambdaWisdom :: CanBeWise (Op lore) => Lambda (Wise lore) -> Lambda lore
removeLambdaWisdom = rephraseLambda removeWisdom

removeExtLambdaWisdom :: CanBeWise (Op lore) => ExtLambda (Wise lore) -> ExtLambda lore
removeExtLambdaWisdom = rephraseExtLambda removeWisdom

removeBodyWisdom :: CanBeWise (Op lore) => Body (Wise lore) -> Body lore
removeBodyWisdom = rephraseBody removeWisdom

removeExpWisdom :: CanBeWise (Op lore) => Exp (Wise lore) -> Exp lore
removeExpWisdom = rephraseExp removeWisdom

removePatternWisdom :: PatternT (VarWisdom, a) -> PatternT a
removePatternWisdom = rephrasePattern snd

addWisdomToPattern :: (Proper lore, CanBeWise (Op lore)) =>
                      Pattern lore
                   -> Exp (Wise lore)
                   -> Pattern (Wise lore)
addWisdomToPattern pat e =
  Pattern
  (map (`addRanges` unknownRange) ctxals)
  (zipWith addRanges valals ranges)
  where (ctxals, valals) = Aliases.mkPatternAliases pat e
        addRanges patElem range =
          let (als, innerlore) = patElemAttr patElem
          in patElem `setPatElemLore` (VarWisdom als range, innerlore)
        ranges = expRanges e

mkWiseBody :: (Proper lore, CanBeWise (Op lore)) =>
              BodyAttr lore -> [Binding (Wise lore)] -> Result -> Body (Wise lore)
mkWiseBody innerlore bnds res =
  Body (BodyWisdom aliases consumed ranges, innerlore) bnds res
  where (aliases, consumed) = Aliases.mkBodyAliases bnds res
        ranges = Ranges.mkBodyRanges bnds res

mkWiseLetBinding :: (Proper lore, CanBeWise (Op lore)) =>
                    Pattern lore
                 -> ExpAttr lore -> Exp (Wise lore)
                 -> Binding (Wise lore)
mkWiseLetBinding pat explore e =
  Let (addWisdomToPattern pat e)
  (Names' $ consumedInPattern pat <> consumedInExp e, explore)
  e

instance (Bindable lore,
          CanBeWise (Op lore)) => Bindable (Wise lore) where
  mkLet context values e =
    let Let pat' explore _ = mkLet context values $ removeExpWisdom e
    in mkWiseLetBinding pat' explore e

  mkLetNames names e = do
    Let pat explore _ <- mkLetNames names $ removeExpWisdom e
    return $ mkWiseLetBinding pat explore e

  mkBody bnds res =
    let Body bodylore _ _ = mkBody (map removeBindingWisdom bnds) res
    in mkWiseBody bodylore bnds res

class (AliasedOp (OpWithWisdom op),
       RangedOp (OpWithWisdom op),
       IsOp (OpWithWisdom op)) => CanBeWise op where
  type OpWithWisdom op :: *
  removeOpWisdom :: OpWithWisdom op -> op

instance CanBeWise () where
  type OpWithWisdom () = ()
  removeOpWisdom () = ()
