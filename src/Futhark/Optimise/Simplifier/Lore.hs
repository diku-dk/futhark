{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Definition of the lore used by the simplification engine.
module Futhark.Optimise.Simplifier.Lore
       (
         Wise (..)
       , removeBindingWisdom
       , removeLambdaWisdom
       , removeProgWisdom
       , removeFunDecWisdom
       , removeExpWisdom
       , removePatternWisdom
       , addWisdomToPattern
       , mkWiseBody
       , mkWiseLetBinding

       , CanBeWise (..)
       )
       where

import Data.Monoid

import Futhark.Representation.AST
import qualified Futhark.Representation.AST.Lore as Lore
import qualified Futhark.Representation.AST.Annotations as Annotations
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
type VarWisdom = (VarAliases, Range)

-- | Wisdom about an expression.
type ExpWisdom = ConsumedInExp

-- | Wisdom about a body.
type BodyWisdom = ([VarAliases], ConsumedInExp, [Range])

instance (Annotations.Annotations lore,
          CanBeWise (Op lore)) => Annotations.Annotations (Wise lore) where
  type LetBound (Wise lore) = (VarWisdom, Annotations.LetBound lore)
  type Exp (Wise lore) = (ExpWisdom, Annotations.Exp lore)
  type Body (Wise lore) = (BodyWisdom, Annotations.Body lore)
  type FParam (Wise lore) = Annotations.FParam lore
  type LParam (Wise lore) = Annotations.LParam lore
  type RetType (Wise lore) = Annotations.RetType lore
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

instance (Lore.Lore lore, CanBeWise (Op lore)) => Aliased (Wise lore) where
  bodyAliases body =
    let ((aliases, _, _) ,_) = bodyLore body
    in map unNames aliases
  consumedInBody body =
    let ((_, consumed, _) ,_) = bodyLore body
    in unNames consumed
  patternAliases =
    map (unNames . fst . fst . patElemAttr) . patternElements

instance (Lore.Lore lore, CanBeWise (Op lore)) => Ranged (Wise lore) where
  bodyRanges body =
    let ((_, _, ranges) ,_) = bodyLore body
    in ranges
  patternRanges = map (snd . fst . patElemAttr) . patternElements

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

removeExpWisdom :: CanBeWise (Op lore) => Exp (Wise lore) -> Exp lore
removeExpWisdom = rephraseExp removeWisdom

removePatternWisdom :: CanBeWise (Op lore) => Pattern (Wise lore) -> Pattern lore
removePatternWisdom = rephrasePattern removeWisdom

addWisdomToPattern :: (Proper lore, CanBeWise (Op lore)) =>
                      Pattern lore -> Exp (Wise lore) -> Pattern (Wise lore)
addWisdomToPattern pat e =
  Pattern
  (map (`addRanges` unknownRange) ctxals)
  (zipWith addRanges valals ranges)
  where (ctxals, valals) = Aliases.mkPatternAliases pat e
        addRanges patElem range =
          let (als,innerlore) = patElemAttr patElem
          in patElem `setPatElemLore` ((als, range), innerlore)
        ranges = expRanges e

mkWiseBody :: (Proper lore, CanBeWise (Op lore)) =>
              Annotations.Body lore -> [Binding (Wise lore)] -> Result -> Body (Wise lore)
mkWiseBody innerlore bnds res =
  Body ((aliases, consumed, ranges), innerlore) bnds res
  where (aliases, consumed) = Aliases.mkBodyAliases bnds res
        ranges = Ranges.mkBodyRanges bnds res

mkWiseLetBinding :: (Proper lore, CanBeWise (Op lore)) =>
                    Pattern lore -> Annotations.Exp lore -> Exp (Wise lore)
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
