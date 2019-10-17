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
       , mkAliasedLetStm
       , mkAliasedBody
       , mkPatternAliases
       , mkBodyAliases
         -- * Removing aliases
       , removeProgAliases
       , removeFunDefAliases
       , removeExpAliases
       , removeBodyAliases
       , removeStmAliases
       , removeLambdaAliases
       , removePatternAliases
       , removeScopeAliases
       -- * Tracking aliases
       , AliasesAndConsumed
       , trackAliases
       , consumedInStms
       )
where

import Control.Monad.Identity
import Control.Monad.Reader
import Data.Foldable
import Data.Maybe
import qualified Data.Map.Strict as M

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Transform.Rename
import Futhark.Binder
import Futhark.Transform.Substitute
import Futhark.Analysis.Rephrase
import qualified Futhark.Util.Pretty as PP

-- | The lore for the basic representation.
data Aliases lore

-- | A wrapper around 'Names' to get around the fact that we need an
-- 'Ord' instance, which 'Names' does not have.
newtype Names' = Names' { unNames :: Names }
               deriving (Show)

instance Semigroup Names' where
  x <> y = Names' $ unNames x <> unNames y

instance Monoid Names' where
  mempty = Names' mempty

instance Eq Names' where
  _ == _ = True

instance Ord Names' where
  _ `compare` _ = EQ

instance Rename Names' where
  rename (Names' names) = Names' <$> rename names

instance Substitute Names' where
  substituteNames substs (Names' names) = Names' $ substituteNames substs names

instance FreeIn Names' where
  freeIn' = const mempty

instance PP.Pretty Names' where
  ppr = PP.commasep . map PP.ppr . namesToList . unNames

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
  type BranchType (Aliases lore) = BranchType lore
  type Op (Aliases lore) = OpWithAliases (Op lore)

instance AliasesOf (VarAliases, attr) where
  aliasesOf = unNames . fst

instance FreeAttr Names' where

withoutAliases :: (HasScope (Aliases lore) m, Monad m) =>
                 ReaderT (Scope lore) m a -> m a
withoutAliases m = do
  scope <- asksScope removeScopeAliases
  runReaderT m scope

instance (Attributes lore, CanBeAliased (Op lore)) => Attributes (Aliases lore) where
  expTypesFromPattern =
    withoutAliases . expTypesFromPattern . removePatternAliases

instance (Attributes lore, CanBeAliased (Op lore)) => Aliased (Aliases lore) where
  bodyAliases = map unNames . fst . fst . bodyAttr
  consumedInBody = unNames . snd . fst . bodyAttr

instance PrettyAnnot (PatElemT attr) =>
  PrettyAnnot (PatElemT (VarAliases, attr)) where

  ppAnnot (PatElem name (Names' als, attr)) =
    let alias_comment = PP.oneLine <$> aliasComment name als
    in case (alias_comment, ppAnnot (PatElem name attr)) of
         (_, Nothing) ->
           alias_comment
         (Just alias_comment', Just inner_comment) ->
           Just $ alias_comment' PP.</> inner_comment
         (Nothing, Just inner_comment) ->
           Just inner_comment


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

          expAttr = case namesToList $ unNames consumed of
            []  -> Nothing
            als -> Just $ PP.oneLine $
                   PP.text "-- Consumes " <> PP.commasep (map PP.ppr als)

maybeComment :: [PP.Doc] -> Maybe PP.Doc
maybeComment [] = Nothing
maybeComment cs = Just $ PP.folddoc (PP.</>) cs

aliasComment :: PP.Pretty a => a -> Names -> Maybe PP.Doc
aliasComment name als =
  case namesToList als of
    [] -> Nothing
    als' -> Just $ PP.oneLine $
            PP.text "-- " <> PP.ppr name <> PP.text " aliases " <>
            PP.commasep (map PP.ppr als')

resultAliasComment :: PP.Pretty a => a -> Names -> Maybe PP.Doc
resultAliasComment name als =
  case namesToList als of
    [] -> Nothing
    als' -> Just $ PP.oneLine $
            PP.text "-- Result of " <> PP.ppr name <> PP.text " aliases " <>
            PP.commasep (map PP.ppr als')

removeAliases :: CanBeAliased (Op lore) => Rephraser Identity (Aliases lore) lore
removeAliases = Rephraser { rephraseExpLore = return . snd
                          , rephraseLetBoundLore = return . snd
                          , rephraseBodyLore = return . snd
                          , rephraseFParamLore = return
                          , rephraseLParamLore = return
                          , rephraseRetType = return
                          , rephraseBranchType = return
                          , rephraseOp = return . removeOpAliases
                          }

removeScopeAliases :: Scope (Aliases lore) -> Scope lore
removeScopeAliases = M.map unAlias
  where unAlias (LetInfo (_, attr)) = LetInfo attr
        unAlias (FParamInfo attr) = FParamInfo attr
        unAlias (LParamInfo attr) = LParamInfo attr
        unAlias (IndexInfo it) = IndexInfo it

removeProgAliases :: CanBeAliased (Op lore) =>
                     Prog (Aliases lore) -> Prog lore
removeProgAliases = runIdentity . rephraseProg removeAliases

removeFunDefAliases :: CanBeAliased (Op lore) =>
                       FunDef (Aliases lore) -> FunDef lore
removeFunDefAliases = runIdentity . rephraseFunDef removeAliases

removeExpAliases :: CanBeAliased (Op lore) =>
                    Exp (Aliases lore) -> Exp lore
removeExpAliases = runIdentity . rephraseExp removeAliases

removeBodyAliases :: CanBeAliased (Op lore) =>
                     Body (Aliases lore) -> Body lore
removeBodyAliases = runIdentity . rephraseBody removeAliases

removeStmAliases :: CanBeAliased (Op lore) =>
                        Stm (Aliases lore) -> Stm lore
removeStmAliases = runIdentity . rephraseStm removeAliases

removeLambdaAliases :: CanBeAliased (Op lore) =>
                       Lambda (Aliases lore) -> Lambda lore
removeLambdaAliases = runIdentity . rephraseLambda removeAliases

removePatternAliases :: PatternT (Names', a)
                     -> PatternT a
removePatternAliases = runIdentity . rephrasePattern (return . snd)

addAliasesToPattern :: (Attributes lore, CanBeAliased (Op lore), Typed attr) =>
                       PatternT attr -> Exp (Aliases lore)
                    -> PatternT (VarAliases, attr)
addAliasesToPattern pat e =
  uncurry Pattern $ mkPatternAliases pat e

mkAliasedBody :: (Attributes lore, CanBeAliased (Op lore)) =>
                 BodyAttr lore -> Stms (Aliases lore) -> Result -> Body (Aliases lore)
mkAliasedBody innerlore bnds res =
  Body (mkBodyAliases bnds res, innerlore) bnds res

mkPatternAliases :: (Aliased lore, Typed attr) =>
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
                  case patElemType bindee of
                    Array {} -> names
                    Mem _    -> names
                    _        -> mempty

mkContextAliases :: Aliased lore =>
                    PatternT attr -> Exp lore -> [Names]
mkContextAliases pat (DoLoop ctxmerge valmerge _ body) =
  let ctx = map fst ctxmerge
      init_als = zip mergenames $ map (subExpAliases . snd) $ ctxmerge ++ valmerge
      expand als = als <> mconcat (mapMaybe (`lookup` init_als) (namesToList als))
      merge_als = zip mergenames $
                  map ((`namesSubtract` mergenames_set) . expand) $
                  bodyAliases body
  in if length ctx == length (patternContextElements pat)
     then map (fromMaybe mempty . flip lookup merge_als . paramName) ctx
     else map (const mempty) $ patternContextElements pat
  where mergenames = map (paramName . fst) $ ctxmerge ++ valmerge
        mergenames_set = namesFromList mergenames
mkContextAliases pat (If _ tbranch fbranch _) =
  take (length $ patternContextNames pat) $
  zipWith (<>) (bodyAliases tbranch) (bodyAliases fbranch)
mkContextAliases pat _ =
  replicate (length $ patternContextElements pat) mempty

mkBodyAliases :: Aliased lore =>
                 Stms lore
              -> Result
              -> BodyAliasing
mkBodyAliases bnds res =
  -- We need to remove the names that are bound in bnds from the alias
  -- and consumption sets.  We do this by computing the transitive
  -- closure of the alias map (within bnds), then removing anything
  -- bound in bnds.
  let (aliases, consumed) = mkStmsAliases bnds res
      boundNames =
        fold $ fmap (namesFromList . patternNames . stmPattern) bnds
      aliases' = map (`namesSubtract` boundNames) aliases
      consumed' = consumed `namesSubtract` boundNames
  in (map Names' aliases', Names' consumed')

mkStmsAliases :: Aliased lore =>
                 Stms lore -> [SubExp]
              -> ([Names], Names)
mkStmsAliases bnds res = delve mempty $ stmsToList bnds
  where delve (aliasmap, consumed) [] =
          (map (aliasClosure aliasmap . subExpAliases) res,
           consumed)
        delve (aliasmap, consumed) (bnd:bnds') =
          delve (trackAliases (aliasmap, consumed) bnd) bnds'
        aliasClosure aliasmap names =
          names <> mconcat (map look $ namesToList names)
          where look k = M.findWithDefault mempty k aliasmap

-- | Everything consumed in the given bindings and result (even transitively).
consumedInStms :: Aliased lore => Stms lore -> [SubExp] -> Names
consumedInStms bnds res = snd $ mkStmsAliases bnds res

type AliasesAndConsumed = (M.Map VName Names,
                           Names)

trackAliases :: Aliased lore =>
                AliasesAndConsumed -> Stm lore
             -> AliasesAndConsumed
trackAliases (aliasmap, consumed) bnd =
  let pat = stmPattern bnd
      als = M.fromList $
            zip (patternNames pat) (map addAliasesOfAliases $ patternAliases pat)
      aliasmap' = als <> aliasmap
      consumed' = consumed <> addAliasesOfAliases (consumedInStm bnd)
  in (aliasmap', consumed')
  where addAliasesOfAliases names = names <> aliasesOfAliases names
        aliasesOfAliases =  mconcat . map look . namesToList
        look k = M.findWithDefault mempty k aliasmap

mkAliasedLetStm :: (Attributes lore, CanBeAliased (Op lore)) =>
                   Pattern lore
                -> StmAux (ExpAttr lore) -> Exp (Aliases lore)
                -> Stm (Aliases lore)
mkAliasedLetStm pat (StmAux cs attr) e =
  Let (addAliasesToPattern pat e)
  (StmAux cs (Names' $ consumedInExp e, attr))
  e

instance (Bindable lore, CanBeAliased (Op lore)) => Bindable (Aliases lore) where
  mkExpAttr pat e =
    let attr = mkExpAttr (removePatternAliases pat) $ removeExpAliases e
    in (Names' $ consumedInExp e, attr)

  mkExpPat ctx val e =
    addAliasesToPattern (mkExpPat ctx val $ removeExpAliases e) e

  mkLetNames names e = do
    env <- asksScope removeScopeAliases
    flip runReaderT env $ do
      Let pat attr _ <- mkLetNames names $ removeExpAliases e
      return $ mkAliasedLetStm pat attr e

  mkBody bnds res =
    let Body bodylore _ _ = mkBody (fmap removeStmAliases bnds) res
    in mkAliasedBody bodylore bnds res

instance (Attributes (Aliases lore), Bindable (Aliases lore)) => BinderOps (Aliases lore) where
  mkBodyB = bindableMkBodyB
  mkExpAttrB = bindableMkExpAttrB
  mkLetNamesB = bindableMkLetNamesB
