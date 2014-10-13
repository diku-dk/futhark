{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
-- | A representation where all bindings are annotated with aliasing
-- information.
module Futhark.Representation.Aliases
       ( -- * The Lore definition
         Aliases
       , Names' (..)
       , module Futhark.Representation.AST.Attributes.Aliases
         -- * Syntax types
       , Prog
       , Body
       , Binding
       , Pattern
       , PrimOp
       , LoopOp
       , Exp
       , Lambda
       , FunDec
         -- * Module re-exports
       , module Futhark.Representation.AST.Attributes
       , module Futhark.Representation.AST.Traversals
       , module Futhark.Representation.AST.Pretty
       , module Futhark.Representation.AST.Syntax
       , AST.LambdaT(Lambda)
       , AST.BodyT(Body)
       , AST.PatternT(Pattern)
       , AST.ProgT(Prog)
       , AST.ExpT(PrimOp)
       , AST.ExpT(LoopOp)
       , AST.FunDecT(FunDec)
         -- * Adding aliases
       , addAliasesToPattern
       , mkAliasedLetBinding
       , mkAliasedBody
         -- * Removing aliases
       , removeProgAliases
       , removeFunDecAliases
       , removeExpAliases
       , removeBodyAliases
       , removeBindingAliases
       , removeLambdaAliases
       )
where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Text.PrettyPrint.Mainland as PP

import qualified Futhark.Representation.AST.Lore as Lore
import qualified Futhark.Representation.AST.Syntax as AST
import Futhark.Representation.AST.Syntax
  hiding (Prog, PrimOp, LoopOp, Exp, Body, Binding, Pattern, Lambda, FunDec)
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Renamer
import Futhark.Binder
import Futhark.Substitute
import Futhark.Analysis.Rephrase

-- | The lore for the basic representation.
data Aliases lore

-- | A wrapper around 'Names' to get around the fact that we need an
-- 'Ord' instance, which 'Names' does not have.
newtype Names' = Names' { unNames :: Names }
               deriving (Show)

instance Monoid Names' where
  mempty = Names' mempty
  x `mappend` y = Names' $ unNames x <> unNames y

instance Eq Names' where
  _ == _ = True

instance Ord Names' where
  _ `compare` _ = EQ

instance Rename Names' where
  rename (Names' names) = Names' <$> rename names

instance Substitute Names' where
  substituteNames substs (Names' names) = Names' $ substituteNames substs names

instance FreeIn Names' where
  freeIn = const mempty

instance Lore.Lore lore => Lore.Lore (Aliases lore) where
  type LetBound (Aliases lore) = (Names', Lore.LetBound lore)
  type Exp (Aliases lore) = (Names', Lore.Exp lore)
  type Body (Aliases lore) = (([Names'], Names'), Lore.Body lore)
  type FParam (Aliases lore) = Lore.FParam lore

type Prog lore = AST.Prog (Aliases lore)
type PrimOp lore = AST.PrimOp (Aliases lore)
type LoopOp lore = AST.LoopOp (Aliases lore)
type Exp lore = AST.Exp (Aliases lore)
type Body lore = AST.Body (Aliases lore)
type Binding lore = AST.Binding (Aliases lore)
type Pattern lore = AST.Pattern (Aliases lore)
type Lambda lore = AST.Lambda (Aliases lore)
type FunDec lore = AST.FunDec (Aliases lore)

instance Renameable lore => Renameable (Aliases lore) where
instance Substitutable lore => Substitutable (Aliases lore) where
instance Proper lore => Proper (Aliases lore) where

instance Aliased (Aliases lore) where
  bodyAliases = map unNames . fst . fst . bodyLore
  consumedInBody = unNames . snd . fst . bodyLore

instance PrettyLore lore => PrettyLore (Aliases lore) where
  ppBindingLore binding =
    case catMaybes [bindeeAnnots,
                    expAnnot,
                    ppBindingLore $ removeBindingAliases binding] of
      [] -> Nothing
      ls -> Just $ PP.folddoc (PP.</>) ls
    where expAnnot = case HS.toList $ consumedInExp $ bindingExp binding of
            []  -> Nothing
            als -> Just $ oneline $
                   PP.text "// Consumes " <> PP.commasep (map PP.ppr als)
          bindeeAnnots =
            case mapMaybe bindeeAnnot $ patternBindees $ bindingPattern binding of
              []     -> Nothing
              annots -> Just $ PP.folddoc (PP.</>) annots
          bindeeAnnot bindee =
            case HS.toList . unNames . fst . bindeeLore $ bindee of
              [] -> Nothing
              als -> Just $ oneline $
                     PP.text "// " <> PP.ppr (bindeeName bindee) <> PP.text " aliases " <>
                     PP.commasep (map PP.ppr als)
          oneline s = PP.text $ PP.displayS (PP.renderCompact s) ""
  ppFunDecLore = ppFunDecLore . removeFunDecAliases

removeAliases :: Rephraser (Aliases lore) lore
removeAliases = Rephraser { rephraseExpLore = snd
                          , rephraseBindeeLore = snd
                          , rephraseBodyLore = snd
                          , rephraseFParamLore = id
                          }

removeProgAliases :: AST.Prog (Aliases lore) -> AST.Prog lore
removeProgAliases = rephraseProg removeAliases

removeFunDecAliases :: AST.FunDec (Aliases lore) -> AST.FunDec lore
removeFunDecAliases = rephraseFunDec removeAliases

removeExpAliases :: AST.Exp (Aliases lore) -> AST.Exp lore
removeExpAliases = rephraseExp removeAliases

removeBodyAliases :: AST.Body (Aliases lore) -> AST.Body lore
removeBodyAliases = rephraseBody removeAliases

removeBindingAliases :: AST.Binding (Aliases lore) -> AST.Binding lore
removeBindingAliases = rephraseBinding removeAliases

removeLambdaAliases :: AST.Lambda (Aliases lore) -> AST.Lambda lore
removeLambdaAliases = rephraseLambda removeAliases

addAliasesToPattern :: AST.Pattern lore -> Exp lore -> Pattern lore
addAliasesToPattern pat e =
  -- Some part of the pattern may be the context, which is not
  -- aliased.
  let patternAliases = replicate (contextSize $ typeOf e) mempty
  in AST.Pattern $ zipWith annotateBindee (patternBindees pat) $
     patternAliases <> aliasesOf e
  where annotateBindee bindee names =
          bindee { bindeeLore =
                      (Names' names', bindeeLore bindee)
                 }
          where names'
                  | basicType $ bindeeType bindee = mempty
                  | otherwise                     = names

mkAliasedBody :: Lore.Body lore -> [Binding lore] -> Result -> Body lore
mkAliasedBody innerlore bnds res =
  -- We need to remove the names that are bound in bnds from the alias
  -- and consumption sets.  We do this by computing the transitive
  -- closure of the alias map (within bnds), then removing anything
  -- bound in bnds.
  let (aliases, consumed) = delve (HM.empty, HS.empty) bnds
      boundNames =
        mconcat $ map (HS.fromList . patternNames . bindingPattern) bnds
      bound = (`HS.member` boundNames)
      aliases' = map (HS.filter (not . bound)) aliases
      consumed' = HS.filter (not . bound) consumed
  in AST.Body ((map Names' aliases', Names' consumed'), innerlore) bnds res
  where delve (aliasmap, consumed) [] =
          (map (aliasClosure aliasmap . subExpAliases) $ resultSubExps res,
           consumed)
        delve (aliasmap, consumed) (bnd:bnds') =
          let e = bindingExp bnd
              aliases =
                replicate (contextSize $ typeOf e) mempty <> aliasesOf e
              names = patternNames $ bindingPattern bnd
              aliasmap' = HM.fromList (zip names aliases) <> aliasmap
              consumed' = consumed <> aliasClosure aliasmap (consumedInExp e)
          in delve (aliasmap', consumed') bnds'
        aliasClosure aliasmap names =
          names `HS.union` mconcat (map look $ HS.toList names)
          where look k = HM.lookupDefault mempty k aliasmap

mkAliasedLetBinding :: AST.Pattern lore -> Lore.Exp lore -> Exp lore -> Binding lore
mkAliasedLetBinding pat explore e =
  Let (addAliasesToPattern pat e) (Names' $ consumedInExp e, explore) e

instance Bindable lore => Bindable (Aliases lore) where
  mkLet vs e =
    let Let pat explore _ = mkLet vs $ removeExpAliases e
    in mkAliasedLetBinding pat explore e

  mkBody bnds res =
    let AST.Body bodylore _ _ = mkBody (map removeBindingAliases bnds) res
    in mkAliasedBody bodylore bnds res
