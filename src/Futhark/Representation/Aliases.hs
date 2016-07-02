{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
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
         -- * Module re-exports
       , module Futhark.Representation.AST.Attributes
       , module Futhark.Representation.AST.Traversals
       , module Futhark.Representation.AST.Pretty
       , module Futhark.Representation.AST.Syntax
         -- * Adding aliases
       , addAliasesToPattern
       , mkAliasedLetBinding
       , mkAliasedBody
       , mkPatternAliases
       , mkBodyAliases
         -- * Removing aliases
       , removeProgAliases
       , removeFunDefAliases
       , removeExpAliases
       , removeBodyAliases
       , removeBindingAliases
       , removeLambdaAliases
       , removeExtLambdaAliases
       , removePatternAliases
       , removeScopeAliases
       )
where

import Control.Applicative
import Control.Monad.Reader
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Prelude

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Transform.Rename
import Futhark.Binder
import Futhark.Transform.Substitute
import Futhark.Analysis.Rephrase
import Futhark.Representation.AST.Attributes.Ranges()
import qualified Futhark.Util.Pretty as PP

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

instance PP.Pretty Names' where
  ppr = PP.commasep . map PP.ppr . HS.toList . unNames

-- | The aliases of the let-bound variable.
type VarAliases = Names'

-- | Everything consumed in the expression.
type ConsumedInExp = Names'

-- | The aliases of what is returned by the 'Body', and what is
-- consumed inside of it.
type BodyAliasing = ([VarAliases], ConsumedInExp)

instance (Annotations lore, CanBeAliased (Op lore)) =>
         Annotations (Aliases lore) where
  type LetAttr (Aliases lore) = (VarAliases, LetAttr lore)
  type ExpAttr (Aliases lore) = (ConsumedInExp, ExpAttr lore)
  type BodyAttr (Aliases lore) = (BodyAliasing, BodyAttr lore)
  type FParamAttr (Aliases lore) = FParamAttr lore
  type LParamAttr (Aliases lore) = LParamAttr lore
  type RetType (Aliases lore) = RetType lore
  type Op (Aliases lore) = OpWithAliases (Op lore)

instance AliasesOf (VarAliases, attr) where
  aliasesOf = unNames . fst

instance (Attributes lore, CanBeAliased (Op lore)) => Attributes (Aliases lore) where
  expContext pat e = do
    env <- asksScope removeScopeAliases
    return $ runReader (expContext (removePatternAliases pat) (removeExpAliases e)) env

instance (Attributes lore, CanBeAliased (Op lore)) => Aliased (Aliases lore) where
  bodyAliases = map unNames . fst . fst . bodyLore
  consumedInBody = unNames . snd . fst . bodyLore

instance PrettyAnnot (PatElem attr) =>
  PrettyAnnot (PatElem (VarAliases, attr)) where

  ppAnnot (PatElem name bindage (Names' als, attr)) =
    (PP.oneLine <$> aliasComment name als) <>
    ppAnnot (PatElem name bindage attr)

instance (Attributes lore, CanBeAliased (Op lore)) => PrettyLore (Aliases lore) where
  ppExpLore (consumed, inner) e =
    maybeComment $ catMaybes [expAttr,
                              mergeAttr,
                              ppExpLore inner $ removeExpAliases e]
    where mergeAttr =
            case e of
              DoLoop _ merge _ body ->
                let mergeParamAliases fparam als
                      | primType (paramType fparam) =
                          Nothing
                      | otherwise =
                          resultAliasComment (paramName fparam) als
                in maybeComment $ catMaybes $
                   zipWith mergeParamAliases (map fst merge) $
                   bodyAliases body
              _ -> Nothing

          expAttr = case HS.toList $ unNames consumed of
            []  -> Nothing
            als -> Just $ PP.oneLine $
                   PP.text "-- Consumes " <> PP.commasep (map PP.ppr als)

maybeComment :: [PP.Doc] -> Maybe PP.Doc
maybeComment [] = Nothing
maybeComment cs = Just $ PP.folddoc (PP.</>) cs

aliasComment :: (PP.Pretty a, PP.Pretty b) =>
                a -> HS.HashSet b -> Maybe PP.Doc
aliasComment name als =
  case HS.toList als of
    [] -> Nothing
    als' -> Just $ PP.oneLine $
            PP.text "-- " <> PP.ppr name <> PP.text " aliases " <>
            PP.commasep (map PP.ppr als')

resultAliasComment :: (PP.Pretty a, PP.Pretty b) =>
                a -> HS.HashSet b -> Maybe PP.Doc
resultAliasComment name als =
  case HS.toList als of
    [] -> Nothing
    als' -> Just $ PP.oneLine $
            PP.text "-- Result of " <> PP.ppr name <> PP.text " aliases " <>
            PP.commasep (map PP.ppr als')

removeAliases :: CanBeAliased (Op lore) => Rephraser (Aliases lore) lore
removeAliases = Rephraser { rephraseExpLore = snd
                          , rephraseLetBoundLore = snd
                          , rephraseBodyLore = snd
                          , rephraseFParamLore = id
                          , rephraseLParamLore = id
                          , rephraseRetType = id
                          , rephraseOp = removeOpAliases
                          }

removeScopeAliases :: Scope (Aliases lore) -> Scope lore
removeScopeAliases = HM.map unAlias
  where unAlias (LetInfo (_, attr)) = LetInfo attr
        unAlias (FParamInfo attr) = FParamInfo attr
        unAlias (LParamInfo attr) = LParamInfo attr
        unAlias IndexInfo = IndexInfo

removeProgAliases :: CanBeAliased (Op lore) =>
                     Prog (Aliases lore) -> Prog lore
removeProgAliases = rephraseProg removeAliases

removeFunDefAliases :: CanBeAliased (Op lore) =>
                       FunDef (Aliases lore) -> FunDef lore
removeFunDefAliases = rephraseFunDef removeAliases

removeExpAliases :: CanBeAliased (Op lore) =>
                    Exp (Aliases lore) -> Exp lore
removeExpAliases = rephraseExp removeAliases

removeBodyAliases :: CanBeAliased (Op lore) =>
                     Body (Aliases lore) -> Body lore
removeBodyAliases = rephraseBody removeAliases

removeBindingAliases :: CanBeAliased (Op lore) =>
                        Binding (Aliases lore) -> Binding lore
removeBindingAliases = rephraseBinding removeAliases

removeLambdaAliases :: CanBeAliased (Op lore) =>
                       Lambda (Aliases lore) -> Lambda lore
removeLambdaAliases = rephraseLambda removeAliases

removeExtLambdaAliases :: CanBeAliased (Op lore) =>
                          ExtLambda (Aliases lore) -> ExtLambda lore
removeExtLambdaAliases = rephraseExtLambda removeAliases

removePatternAliases :: PatternT (Names', a)
                     -> PatternT a
removePatternAliases = rephrasePattern snd

addAliasesToPattern :: (Attributes lore, CanBeAliased (Op lore), Typed attr) =>
                       PatternT attr -> Exp (Aliases lore)
                    -> PatternT (VarAliases, attr)
addAliasesToPattern pat e =
  uncurry Pattern $ mkPatternAliases pat e

mkAliasedBody :: (Attributes lore, CanBeAliased (Op lore)) =>
                 BodyAttr lore -> [Binding (Aliases lore)] -> Result -> Body (Aliases lore)
mkAliasedBody innerlore bnds res =
  Body (mkBodyAliases bnds res, innerlore) bnds res

mkPatternAliases :: (Attributes lore, Aliased lore,
                     AliasedOp (Op lore), Typed attr) =>
                    PatternT attr -> Exp lore
                 -> ([PatElemT (VarAliases, attr)],
                     [PatElemT (VarAliases, attr)])
mkPatternAliases pat e =
  -- Some part of the pattern may be the context.  This does not have
  -- aliases from expAliases, so we use a hack to compute aliases of
  -- the context.
  let als = expAliases e ++ repeat mempty -- In case the pattern has
                                          -- more elements (this
                                          -- implies a type error).
      context_als = mkContextAliases pat e
  in (zipWith annotateBindee (patternContextElements pat) context_als,
      zipWith annotateBindee (patternValueElements pat) als)
  where annotateBindee bindee names =
            bindee `setPatElemLore` (Names' names', patElemAttr bindee)
          where names' =
                  case (patElemBindage bindee, patElemRequires bindee) of
                    (BindInPlace {}, _) -> mempty
                    (_, Array {})       -> names
                    (_, Mem _ _)        -> names
                    _                   -> mempty

mkContextAliases :: (Attributes lore, Aliased lore) =>
                    PatternT attr -> Exp lore
                 -> [Names]
mkContextAliases pat (DoLoop ctxmerge valmerge _ body) =
  let ctx = loopResultContext (map fst ctxmerge) (map fst valmerge)
      init_als = zip mergenames $ map (subExpAliases . snd) $ ctxmerge ++ valmerge
      expand als = als <> HS.unions (mapMaybe (`lookup` init_als) (HS.toList als))
      merge_als = zip mergenames $
                  map ((`HS.difference` mergenames_set) . expand) $
                  bodyAliases body
  in if length ctx == length (patternContextElements pat)
     then map (fromMaybe mempty . flip lookup merge_als . paramName) ctx
     else map (const mempty) $ patternContextElements pat
  where mergenames = map (paramName . fst) $ ctxmerge ++ valmerge
        mergenames_set = HS.fromList mergenames
-- FIXME: handle If here as well.
mkContextAliases pat _ =
  replicate (length $ patternContextElements pat) mempty

mkBodyAliases :: Aliased lore =>
                 [Binding lore]
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
              als = HM.fromList $
                    zip (patternNames pat) (patternAliases pat)
              aliasmap' = als <> aliasmap
              consumed' = consumed <> aliasClosure aliasmap (consumedInBinding bnd)
          in delve (aliasmap', consumed') bnds'
        aliasClosure aliasmap names =
          names `HS.union` mconcat (map look $ HS.toList names)
          where look k = HM.lookupDefault mempty k aliasmap

mkAliasedLetBinding :: (Attributes lore, CanBeAliased (Op lore)) =>
                       Pattern lore
                    -> ExpAttr lore -> Exp (Aliases lore)
                    -> Binding (Aliases lore)
mkAliasedLetBinding pat explore e =
  Let (addAliasesToPattern pat e)
  (Names' $ consumedInPattern pat <> consumedInExp e, explore)
  e

instance (Bindable lore, CanBeAliased (Op lore)) => Bindable (Aliases lore) where
  mkLet context values e =
    let Let pat' explore _ = mkLet context values $ removeExpAliases e
    in mkAliasedLetBinding pat' explore e

  mkLetNames names e = do
    env <- asksScope removeScopeAliases
    flip runReaderT env $ do
      Let pat explore _ <- mkLetNames names $ removeExpAliases e
      return $ mkAliasedLetBinding pat explore e

  mkBody bnds res =
    let Body bodylore _ _ = mkBody (map removeBindingAliases bnds) res
    in mkAliasedBody bodylore bnds res
