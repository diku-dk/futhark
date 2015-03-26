{-# LANGUAGE TypeFamilies #-}
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
       , ExtLambda
       , FunDec
       , RetType
         -- * Module re-exports
       , module Futhark.Representation.AST.Attributes
       , module Futhark.Representation.AST.Traversals
       , module Futhark.Representation.AST.Pretty
       , module Futhark.Representation.AST.Syntax
       , AST.LambdaT(Lambda)
       , AST.ExtLambdaT(ExtLambda)
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
       , mkPatternAliases
       , mkBodyAliases
         -- * Removing aliases
       , removeProgAliases
       , removeFunDecAliases
       , removeExpAliases
       , removeBodyAliases
       , removeBindingAliases
       , removeLambdaAliases
       , removePatternAliases
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
  hiding (Prog, PrimOp, LoopOp, Exp, Body, Binding,
          Pattern, Lambda, ExtLambda, FunDec, RetType)
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Renamer
import Futhark.Binder
import Futhark.Substitute
import Futhark.Analysis.Rephrase
import Futhark.Representation.AST.Attributes.Ranges

-- | The lore for the basic representation.
data Aliases lore = Aliases lore

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

instance PP.Pretty Names' where
  ppr = PP.commasep . map PP.ppr . HS.toList . unNames

instance Lore.Lore lore => Lore.Lore (Aliases lore) where
  type LetBound (Aliases lore) = (Names', Lore.LetBound lore)
  type Exp (Aliases lore) = (Names', Lore.Exp lore)
  type Body (Aliases lore) = (([Names'], Names'), Lore.Body lore)
  type FParam (Aliases lore) = Lore.FParam lore
  type RetType (Aliases lore) = Lore.RetType lore

  representative =
    Aliases Lore.representative

  loopResultContext (Aliases lore) =
    Lore.loopResultContext lore

  applyRetType (Aliases lore) =
    Lore.applyRetType lore

instance Ranged lore => Ranged (Aliases lore) where
  bodyRanges = bodyRanges . removeBodyAliases

type Prog lore = AST.Prog (Aliases lore)
type PrimOp lore = AST.PrimOp (Aliases lore)
type LoopOp lore = AST.LoopOp (Aliases lore)
type Exp lore = AST.Exp (Aliases lore)
type Body lore = AST.Body (Aliases lore)
type Binding lore = AST.Binding (Aliases lore)
type Pattern lore = AST.Pattern (Aliases lore)
type Lambda lore = AST.Lambda (Aliases lore)
type ExtLambda lore = AST.ExtLambda (Aliases lore)
type FunDec lore = AST.FunDec (Aliases lore)
type RetType lore = AST.RetType (Aliases lore)

instance Renameable lore => Renameable (Aliases lore) where
instance Substitutable lore => Substitutable (Aliases lore) where
instance Proper lore => Proper (Aliases lore) where

instance Lore.Lore lore => Aliased (Aliases lore) where
  bodyAliases = map unNames . fst . fst . bodyLore
  consumedInBody = unNames . snd . fst . bodyLore
  patternAliases = map (unNames . fst . patElemLore) . patternElements

instance (PrettyLore lore) => PrettyLore (Aliases lore) where
  ppBindingLore binding@(Let pat (consumed,_) _) =
    case catMaybes [patElemAttrs,
                    expAttr,
                    ppBindingLore $ removeBindingAliases binding] of
      [] -> Nothing
      ls -> Just $ PP.folddoc (PP.</>) ls
    where expAttr = case HS.toList $ unNames consumed of
            []  -> Nothing
            als -> Just $ oneline $
                   PP.text "// Consumes " <> PP.commasep (map PP.ppr als)
          patElemAttrs =
            case mapMaybe patElemAttr $ patternElements pat of
              []    -> Nothing
              attrs -> Just $ PP.folddoc (PP.</>) attrs
          patElemAttr patelem =
            case HS.toList . unNames . fst . patElemLore $ patelem of
              [] -> Nothing
              als -> Just $ oneline $
                     PP.text "// " <> PP.ppr (patElemName patelem) <> PP.text " aliases " <>
                     PP.commasep (map PP.ppr als)
          oneline s = PP.text $ PP.displayS (PP.renderCompact s) ""

  ppFunDecLore = ppFunDecLore . removeFunDecAliases
  ppExpLore = ppExpLore . removeExpAliases

removeAliases :: Rephraser (Aliases lore) lore
removeAliases = Rephraser { rephraseExpLore = snd
                          , rephraseLetBoundLore = snd
                          , rephraseBodyLore = snd
                          , rephraseFParamLore = id
                          , rephraseRetType = id
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

removePatternAliases :: AST.Pattern (Aliases lore) -> AST.Pattern lore
removePatternAliases = rephrasePattern removeAliases

addAliasesToPattern :: Lore.Lore lore =>
                       AST.Pattern lore -> Exp lore -> Pattern lore
addAliasesToPattern pat e =
  AST.Pattern $ mkPatternAliases pat e

mkAliasedBody :: Lore.Lore lore =>
                 Lore.Body lore -> [Binding lore] -> Result -> Body lore
mkAliasedBody innerlore bnds res =
  AST.Body (mkBodyAliases bnds res, innerlore) bnds res

mkPatternAliases :: (Lore.Lore anylore, Aliased lore) =>
                    AST.Pattern anylore -> AST.Exp lore
                 -> [PatElemT (Names', Lore.LetBound anylore)]
mkPatternAliases pat e =
  -- Some part of the pattern may be the context, which is not
  -- aliased.
  let als = aliasesOf e
      als' = replicate (patternSize pat - length als) mempty <> als
  in zipWith annotateBindee (patternElements pat) als'
  where annotateBindee bindee names =
            bindee `setPatElemLore` (Names' names', patElemLore bindee)
          where names' =
                  case (patElemBindage bindee, patElemRequires bindee) of
                    (BindInPlace {}, _) -> mempty
                    (_, Array {})       -> names
                    (_, Mem _)          -> names
                    _                   -> mempty


mkBodyAliases :: Aliased lore =>
                 [AST.Binding lore]
              -> Result
              -> ([Names'], Names')
mkBodyAliases bnds res =
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
  in (map Names' aliases', Names' consumed')
  where delve (aliasmap, consumed) [] =
          (map (aliasClosure aliasmap . subExpAliases) $ resultSubExps res,
           consumed)
        delve (aliasmap, consumed) (bnd:bnds') =
          let pat = bindingPattern bnd
              e = bindingExp bnd
              als = HM.fromList $
                    zip (patternNames pat) (patternAliases pat)
              aliasmap' = als <> aliasmap
              consumed' = consumed <> aliasClosure aliasmap (consumedInExp pat e)
          in delve (aliasmap', consumed') bnds'
        aliasClosure aliasmap names =
          names `HS.union` mconcat (map look $ HS.toList names)
          where look k = HM.lookupDefault mempty k aliasmap

mkAliasedLetBinding :: Lore.Lore lore =>
                       AST.Pattern lore -> Lore.Exp lore -> Exp lore
                    -> Binding lore
mkAliasedLetBinding pat explore e =
  Let (addAliasesToPattern pat e)
  (Names' $ consumedInExp pat e, explore)
  e

instance Bindable lore => Bindable (Aliases lore) where
  mkLet pat e =
    let Let pat' explore _ = mkLet pat $ removeExpAliases e
    in mkAliasedLetBinding pat' explore e

  mkLetNames names e = do
    Let pat explore _ <- mkLetNames names $ removeExpAliases e
    return $ mkAliasedLetBinding pat explore e

  mkBody bnds res =
    let AST.Body bodylore _ _ = mkBody (map removeBindingAliases bnds) res
    in mkAliasedBody bodylore bnds res
