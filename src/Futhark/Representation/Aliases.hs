{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
-- | A representation where all bindings are annotated with aliasing
-- information.
module Futhark.Representation.Aliases
       ( -- * The Lore definition
         Aliases
       , Names' (..)
       , VarAliases
       , ConsumedInExp
       , BodyAliasing
       , module Futhark.Representation.AST.Attributes.Aliases
         -- * Syntax types
       , Prog
       , Body
       , Binding
       , Pattern
       , PrimOp
       , LoopOp
       , SegOp
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
       , AST.ExpT(SegOp)
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

import Prelude

import qualified Futhark.Representation.AST.Annotations as Annotations
import qualified Futhark.Representation.AST.Syntax as AST
import Futhark.Representation.AST.Syntax
  hiding (Prog, PrimOp, LoopOp, SegOp, Exp, Body, Binding,
          Pattern, Lambda, ExtLambda, FunDec, RetType)
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import qualified Futhark.Representation.AST.Lore as Lore
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

-- | The aliases of the let-bound variable.
type VarAliases = Names'

-- | Everything consumed in the expression.
type ConsumedInExp = Names'

-- | The aliases of what is returned by the 'Body', and what is
-- consumed inside of it.
type BodyAliasing = ([VarAliases], ConsumedInExp)

instance Annotations.Annotations lore => Annotations.Annotations (Aliases lore) where
  type LetBound (Aliases lore) = (VarAliases, Annotations.LetBound lore)
  type Exp (Aliases lore) = (ConsumedInExp, Annotations.Exp lore)
  type Body (Aliases lore) = (BodyAliasing, Annotations.Body lore)
  type FParam (Aliases lore) = Annotations.FParam lore
  type LParam (Aliases lore) = Annotations.LParam lore
  type RetType (Aliases lore) = Annotations.RetType lore

instance Lore.Lore lore => Lore.Lore (Aliases lore) where
  representative =
    Aliases representative

  loopResultContext (Aliases lore) =
    loopResultContext lore

  applyRetType (Aliases lore) =
    applyRetType lore

  expContext pat e =
    expContext (removePatternAliases pat) (removeExpAliases e)

instance Ranged lore => Ranged (Aliases lore) where
  bodyRanges = bodyRanges . removeBodyAliases
  patternRanges = patternRanges . removePatternAliases

type Prog lore = AST.Prog (Aliases lore)
type PrimOp lore = AST.PrimOp (Aliases lore)
type LoopOp lore = AST.LoopOp (Aliases lore)
type SegOp lore = AST.SegOp (Aliases lore)
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
    maybeComment $ catMaybes [patElemAttrs,
                              expAttr,
                              ppBindingLore $ removeBindingAliases binding]
    where expAttr = case HS.toList $ unNames consumed of
            []  -> Nothing
            als -> Just $ oneline $
                   PP.text "// Consumes " <> PP.commasep (map PP.ppr als)

          patElemAttrs =
            maybeComment $ mapMaybe patElemAttr $ patternElements pat

          patElemAttr patelem =
            oneline <$>
            aliasComment (patElemName patelem)
            (unNames . fst . patElemLore $ patelem)

  ppFunDecLore = ppFunDecLore . removeFunDecAliases

  ppExpLore e@(AST.LoopOp (DoLoop _ merge _ body)) =
    maybeComment $ catMaybes [expAttr, mergeAttr]
    where mergeAttr = let mergeParamAliases fparam als
                            | basicType (paramType fparam) =
                              Nothing
                            | otherwise =
                              resultAliasComment (paramName fparam) als
                      in maybeComment $ catMaybes $
                         zipWith mergeParamAliases (map fst merge) $
                         bodyAliases body
          expAttr = ppExpLore $ removeExpAliases e
  ppExpLore e =
    ppExpLore $ removeExpAliases e

oneline :: PP.Doc -> PP.Doc
oneline s = PP.text $ PP.displayS (PP.renderCompact s) ""

maybeComment :: [PP.Doc] -> Maybe PP.Doc
maybeComment [] = Nothing
maybeComment cs = Just $ PP.folddoc (PP.</>) cs

aliasComment :: (PP.Pretty a, PP.Pretty b) =>
                a -> HS.HashSet b -> Maybe PP.Doc
aliasComment name als =
  case HS.toList als of
    [] -> Nothing
    als' -> Just $ oneline $
            PP.text "// " <> PP.ppr name <> PP.text " aliases " <>
            PP.commasep (map PP.ppr als')

resultAliasComment :: (PP.Pretty a, PP.Pretty b) =>
                a -> HS.HashSet b -> Maybe PP.Doc
resultAliasComment name als =
  case HS.toList als of
    [] -> Nothing
    als' -> Just $ oneline $
            PP.text "// Result of " <> PP.ppr name <> PP.text " aliases " <>
            PP.commasep (map PP.ppr als')

removeAliases :: Rephraser (Aliases lore) lore
removeAliases = Rephraser { rephraseExpLore = snd
                          , rephraseLetBoundLore = snd
                          , rephraseBodyLore = snd
                          , rephraseFParamLore = id
                          , rephraseLParamLore = id
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
  uncurry AST.Pattern $ mkPatternAliases pat e

mkAliasedBody :: Lore.Lore lore =>
                 Annotations.Body lore -> [Binding lore] -> Result -> Body lore
mkAliasedBody innerlore bnds res =
  AST.Body (mkBodyAliases bnds res, innerlore) bnds res

mkPatternAliases :: (Lore.Lore anylore, Aliased lore) =>
                    AST.Pattern anylore -> AST.Exp lore
                 -> ([PatElemT (VarAliases, Annotations.LetBound anylore)],
                     [PatElemT (VarAliases, Annotations.LetBound anylore)])
mkPatternAliases pat e =
  -- Some part of the pattern may  be the context.  This does not have
  -- aliases from aliasesOf, so we  use a hack to compute some aliases
  -- from do-loops.  FIXME.  This should be more general.
  let als = aliasesOf e
      context_als = mkContextAliases pat e
  in (zipWith annotateBindee (patternContextElements pat) context_als,
      zipWith annotateBindee (patternValueElements pat) als)
  where annotateBindee bindee names =
            bindee `setPatElemLore` (Names' names', patElemLore bindee)
          where names' =
                  case (patElemBindage bindee, patElemRequires bindee) of
                    (BindInPlace {}, _) -> mempty
                    (_, Array {})       -> names
                    (_, Mem _)          -> names
                    _                   -> mempty

mkContextAliases :: forall anylore lore.
                    (Lore.Lore anylore, Aliased lore) =>
                    AST.Pattern anylore -> AST.Exp lore
                 -> [Names]
mkContextAliases _ (AST.LoopOp (DoLoop res merge _ body)) =
  let ctx = loopResultContext (representative :: lore) res $ map fst merge
      init_als = zip mergenames $ map (subExpAliases . snd) merge
      expand als = als <> HS.unions (mapMaybe (`lookup` init_als) (HS.toList als))
      merge_als = zip mergenames $
                  map ((`HS.difference` mergenames_set) . expand) $
                  bodyAliases body
  in map (fromMaybe mempty . flip lookup merge_als) ctx
  where mergenames = map (paramName . fst) merge
        mergenames_set = HS.fromList mergenames
mkContextAliases pat _ =
  replicate (length $ patternContextElements pat) mempty

mkBodyAliases :: Aliased lore =>
                 [AST.Binding lore]
              -> Result
              -> BodyAliasing
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
          (map (aliasClosure aliasmap . subExpAliases) res,
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
                       AST.Pattern lore -> Annotations.Exp lore -> Exp lore
                    -> Binding lore
mkAliasedLetBinding pat explore e =
  Let (addAliasesToPattern pat e)
  (Names' $ consumedInExp pat e, explore)
  e

instance Bindable lore => Bindable (Aliases lore) where
  mkLet context values e =
    let Let pat' explore _ = mkLet context values $ removeExpAliases e
    in mkAliasedLetBinding pat' explore e

  mkLetNames names e = do
    Let pat explore _ <- mkLetNames names $ removeExpAliases e
    return $ mkAliasedLetBinding pat explore e

  mkBody bnds res =
    let AST.Body bodylore _ _ = mkBody (map removeBindingAliases bnds) res
    in mkAliasedBody bodylore bnds res
