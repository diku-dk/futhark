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
       )
       where

import Futhark.Representation.AST
import qualified Futhark.Representation.AST.Lore as Lore
import Futhark.Representation.AST.Attributes.Ranges
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.Aliases
  (unNames, Names' (..), VarAliases, ConsumedInExp)
import qualified Futhark.Representation.Aliases as Aliases
import qualified Futhark.Representation.Ranges as Ranges
import Futhark.Optimise.Simplifier.Simplifiable
import Futhark.Binder
import Futhark.Renamer
import Futhark.Substitute
import Futhark.Analysis.Rephrase

data Wise lore = Wise lore

-- | The wisdom of the let-bound variable.
type VarWisdom = (VarAliases, Range)

-- | Wisdom about an expression.
type ExpWisdom = ConsumedInExp

-- | Wisdom about a body.
type BodyWisdom = ([VarAliases], ConsumedInExp, [Range])

instance Lore.Lore lore => Lore.Lore (Wise lore) where
  type LetBound (Wise lore) = (VarWisdom, Lore.LetBound lore)
  type Exp (Wise lore) = (ExpWisdom, Lore.Exp lore)
  type Body (Wise lore) = (BodyWisdom, Lore.Body lore)
  type FParam (Wise lore) = Lore.FParam lore
  type LParam (Wise lore) = Lore.LParam lore
  type RetType (Wise lore) = Lore.RetType lore

  representative =
    Wise Lore.representative

  loopResultContext (Wise lore) =
    Lore.loopResultContext lore

  applyRetType (Wise lore) =
    Lore.applyRetType lore

instance Renameable lore => Renameable (Wise lore) where
instance Substitutable lore => Substitutable (Wise lore) where
instance PrettyLore lore => PrettyLore (Wise lore) where
instance Proper lore => Proper (Wise lore) where
instance Proper lore => Simplifiable (Wise lore) where

instance Lore.Lore lore => Aliased (Wise lore) where
  bodyAliases body =
    let ((aliases, _, _) ,_) = bodyLore body
    in map unNames aliases
  consumedInBody body =
    let ((_, consumed, _) ,_) = bodyLore body
    in unNames consumed
  patternAliases =
    map (unNames . fst . fst . patElemLore) . patternElements

instance Lore.Lore lore => Ranged (Wise lore) where
  bodyRanges body =
    let ((_, _, ranges) ,_) = bodyLore body
    in ranges
  patternRanges = map (snd . fst . patElemLore) . patternElements

removeWisdom :: Rephraser (Wise lore) lore
removeWisdom = Rephraser { rephraseExpLore = snd
                         , rephraseLetBoundLore = snd
                         , rephraseBodyLore = snd
                         , rephraseFParamLore = id
                         , rephraseLParamLore = id
                         , rephraseRetType = id
                         }

removeProgWisdom :: Prog (Wise lore) -> Prog lore
removeProgWisdom = rephraseProg removeWisdom

removeFunDecWisdom :: FunDec (Wise lore) -> FunDec lore
removeFunDecWisdom = rephraseFunDec removeWisdom

removeBindingWisdom :: Binding (Wise lore) -> Binding lore
removeBindingWisdom = rephraseBinding removeWisdom

removeLambdaWisdom :: Lambda (Wise lore) -> Lambda lore
removeLambdaWisdom = rephraseLambda removeWisdom

removeExpWisdom :: Exp (Wise lore) -> Exp lore
removeExpWisdom = rephraseExp removeWisdom

removePatternWisdom :: Pattern (Wise lore) -> Pattern lore
removePatternWisdom = rephrasePattern removeWisdom

addWisdomToPattern :: Lore.Lore lore =>
                      Pattern lore -> Exp (Wise lore) -> Pattern (Wise lore)
addWisdomToPattern pat e =
  Pattern
  (map (`addRanges` unknownRange) ctxals)
  (zipWith addRanges valals ranges)
  where (ctxals, valals) = Aliases.mkPatternAliases pat e
        addRanges patElem range =
          let (als,innerlore) = patElemLore patElem
          in patElem `setPatElemLore` ((als, range), innerlore)
        ranges = expRanges e

mkWiseBody :: Lore.Lore lore =>
              Lore.Body lore -> [Binding (Wise lore)] -> Result -> Body (Wise lore)
mkWiseBody innerlore bnds res =
  Body ((aliases, consumed, ranges), innerlore) bnds res
  where (aliases, consumed) = Aliases.mkBodyAliases bnds res
        ranges = Ranges.mkBodyRanges bnds res

mkWiseLetBinding :: Lore.Lore lore =>
                    Pattern lore -> Lore.Exp lore -> Exp (Wise lore)
                 -> Binding (Wise lore)
mkWiseLetBinding pat explore e =
  Let (addWisdomToPattern pat e)
  (Names' $ consumedInExp pat e, explore)
  e

instance Bindable lore => Bindable (Wise lore) where
  mkLet context values e =
    let Let pat' explore _ = mkLet context values $ removeExpWisdom e
    in mkWiseLetBinding pat' explore e

  mkLetNames names e = do
    Let pat explore _ <- mkLetNames names $ removeExpWisdom e
    return $ mkWiseLetBinding pat explore e

  mkBody bnds res =
    let Body bodylore _ _ = mkBody (map removeBindingWisdom bnds) res
    in mkWiseBody bodylore bnds res
