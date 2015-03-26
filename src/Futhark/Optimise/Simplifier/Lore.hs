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
  (unNames, Names' (..), VarAliases, ConsumedInExp, BodyAliasing)
import qualified Futhark.Representation.Aliases as Aliases
import Futhark.Optimise.Simplifier.Simplifiable
import Futhark.Binder
import Futhark.Renamer
import Futhark.Substitute
import Futhark.Analysis.Rephrase

data Wise lore = Wise lore

instance Lore.Lore lore => Lore.Lore (Wise lore) where
  type LetBound (Wise lore) = (VarAliases, Lore.LetBound lore)
  type Exp (Wise lore) = (ConsumedInExp, Lore.Exp lore)
  type Body (Wise lore) = (BodyAliasing, Lore.Body lore)
  type FParam (Wise lore) = Lore.FParam lore
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
instance Lore.Lore lore => Ranged (Wise lore) where
instance Proper lore => Simplifiable (Wise lore) where

instance Lore.Lore lore => Aliased (Wise lore) where
  bodyAliases = map unNames . fst . fst . bodyLore
  consumedInBody = unNames . snd . fst . bodyLore
  patternAliases = map (unNames . fst . patElemLore) . patternElements

removeWisdom :: Rephraser (Wise lore) lore
removeWisdom = Rephraser { rephraseExpLore = snd
                         , rephraseLetBoundLore = snd
                         , rephraseBodyLore = snd
                         , rephraseFParamLore = id
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
  Pattern $ Aliases.mkPatternAliases pat e

mkWiseBody :: Lore.Lore lore =>
              Lore.Body lore -> [Binding (Wise lore)] -> Result -> Body (Wise lore)
mkWiseBody innerlore bnds res =
    Body (Aliases.mkBodyAliases bnds res, innerlore) bnds res

mkWiseLetBinding :: Lore.Lore lore =>
                    Pattern lore -> Lore.Exp lore -> Exp (Wise lore)
                 -> Binding (Wise lore)
mkWiseLetBinding pat explore e =
  Let (addWisdomToPattern pat e)
  (Names' $ consumedInExp pat e, explore)
  e

instance Bindable lore => Bindable (Wise lore) where
  mkLet pat e =
    let Let pat' explore _ = mkLet pat $ removeExpWisdom e
    in mkWiseLetBinding pat' explore e

  mkLetNames names e = do
    Let pat explore _ <- mkLetNames names $ removeExpWisdom e
    return $ mkWiseLetBinding pat explore e

  mkBody bnds res =
    let Body bodylore _ _ = mkBody (map removeBindingWisdom bnds) res
    in mkWiseBody bodylore bnds res
