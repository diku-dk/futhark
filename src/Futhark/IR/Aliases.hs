{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | A representation where all patterns are annotated with aliasing
-- information.  It also records consumption of variables in bodies.
--
-- Note that this module is mostly not concerned with actually
-- /computing/ the aliasing information; only with shuffling it around
-- and providing some basic building blocks.  See modules such as
-- "Futhark.Analysis.Alias" for computing the aliases in the first
-- place.
module Futhark.IR.Aliases
  ( -- * The representation definition
    Aliases,
    AliasDec (..),
    VarAliases,
    ConsumedInExp,
    BodyAliasing,
    module Futhark.IR.Prop.Aliases,

    -- * Module re-exports
    module Futhark.IR.Prop,
    module Futhark.IR.Traversals,
    module Futhark.IR.Pretty,
    module Futhark.IR.Syntax,

    -- * Adding aliases
    mkAliasedBody,
    mkAliasedPat,
    mkBodyAliasing,
    CanBeAliased (..),
    AliasableRep,

    -- * Removing aliases
    removeProgAliases,
    removeFunDefAliases,
    removeExpAliases,
    removeStmAliases,
    removeBodyAliases,
    removeLambdaAliases,
    removePatAliases,
    removeScopeAliases,

    -- * Tracking aliases
    AliasesAndConsumed,
    trackAliases,
    mkStmsAliases,
    consumedInStms,
  )
where

import Control.Monad.Identity
import Control.Monad.Reader
import Data.Kind qualified
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Builder
import Futhark.IR.Pretty
import Futhark.IR.Prop
import Futhark.IR.Prop.Aliases
import Futhark.IR.Syntax
import Futhark.IR.Traversals
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util.Pretty qualified as PP

-- | The rep for the basic representation.
data Aliases (rep :: Data.Kind.Type)

-- | A wrapper around 'AliasDec' to get around the fact that we need an
-- 'Ord' instance, which 'AliasDec does not have.
newtype AliasDec = AliasDec {unAliases :: Names}
  deriving (Show)

instance Semigroup AliasDec where
  x <> y = AliasDec $ unAliases x <> unAliases y

instance Monoid AliasDec where
  mempty = AliasDec mempty

instance Eq AliasDec where
  _ == _ = True

instance Ord AliasDec where
  _ `compare` _ = EQ

instance Rename AliasDec where
  rename (AliasDec names) = AliasDec <$> rename names

instance Substitute AliasDec where
  substituteNames substs (AliasDec names) = AliasDec $ substituteNames substs names

instance FreeIn AliasDec where
  freeIn' = const mempty

instance PP.Pretty AliasDec where
  pretty = PP.braces . PP.commasep . map PP.pretty . namesToList . unAliases

-- | The aliases of the let-bound variable.
type VarAliases = AliasDec

-- | Everything consumed in the expression.
type ConsumedInExp = AliasDec

-- | The aliases of what is returned by the t'Body', and what is
-- consumed inside of it.
type BodyAliasing = ([VarAliases], ConsumedInExp)

instance (RepTypes rep, ASTConstraints (OpC rep (Aliases rep))) => RepTypes (Aliases rep) where
  type LetDec (Aliases rep) = (VarAliases, LetDec rep)
  type ExpDec (Aliases rep) = (ConsumedInExp, ExpDec rep)
  type BodyDec (Aliases rep) = (BodyAliasing, BodyDec rep)
  type FParamInfo (Aliases rep) = FParamInfo rep
  type LParamInfo (Aliases rep) = LParamInfo rep
  type RetType (Aliases rep) = RetType rep
  type BranchType (Aliases rep) = BranchType rep
  type OpC (Aliases rep) = OpC rep

instance AliasesOf (VarAliases, dec) where
  aliasesOf = unAliases . fst

instance FreeDec AliasDec

withoutAliases ::
  (HasScope (Aliases rep) m, Monad m) =>
  ReaderT (Scope rep) m a ->
  m a
withoutAliases m = do
  scope <- asksScope removeScopeAliases
  runReaderT m scope

instance
  ( ASTRep rep,
    AliasedOp (OpC rep),
    ASTConstraints (OpC rep (Aliases rep))
  ) =>
  ASTRep (Aliases rep)
  where
  expTypesFromPat =
    withoutAliases . expTypesFromPat . removePatAliases

instance
  ( ASTRep rep,
    AliasedOp (OpC rep),
    ASTConstraints (OpC rep (Aliases rep))
  ) =>
  Aliased (Aliases rep)
  where
  bodyAliases = map unAliases . fst . fst . bodyDec
  consumedInBody = unAliases . snd . fst . bodyDec

instance
  ( ASTRep rep,
    AliasedOp (OpC rep),
    ASTConstraints (OpC rep (Aliases rep))
  ) =>
  PrettyRep (Aliases rep)
  where
  ppExpDec (consumed, inner) e =
    maybeComment . catMaybes $
      [exp_dec, merge_dec, ppExpDec inner $ removeExpAliases e]
    where
      merge_dec =
        case e of
          Loop merge _ body ->
            let mergeParamAliases fparam als
                  | primType (paramType fparam) =
                      Nothing
                  | otherwise =
                      resultAliasComment (paramName fparam) als
             in maybeComment . catMaybes $
                  zipWith mergeParamAliases (map fst merge) $
                    bodyAliases body
          _ -> Nothing

      exp_dec = case namesToList $ unAliases consumed of
        [] -> Nothing
        als ->
          Just $
            PP.oneLine $
              "-- Consumes " <> PP.commasep (map PP.pretty als)

maybeComment :: [PP.Doc a] -> Maybe (PP.Doc a)
maybeComment [] = Nothing
maybeComment cs = Just $ PP.stack cs

resultAliasComment :: (PP.Pretty a) => a -> Names -> Maybe (PP.Doc ann)
resultAliasComment name als =
  case namesToList als of
    [] -> Nothing
    als' ->
      Just $
        PP.oneLine $
          "-- Result for "
            <> PP.pretty name
            <> " aliases "
            <> PP.commasep (map PP.pretty als')

removeAliases :: (RephraseOp (OpC rep)) => Rephraser Identity (Aliases rep) rep
removeAliases =
  Rephraser
    { rephraseExpDec = pure . snd,
      rephraseLetBoundDec = pure . snd,
      rephraseBodyDec = pure . snd,
      rephraseFParamDec = pure,
      rephraseLParamDec = pure,
      rephraseRetType = pure,
      rephraseBranchType = pure,
      rephraseOp = rephraseInOp removeAliases
    }

-- | Remove alias information from an aliased scope.
removeScopeAliases :: Scope (Aliases rep) -> Scope rep
removeScopeAliases = M.map unAlias
  where
    unAlias (LetName (_, dec)) = LetName dec
    unAlias (FParamName dec) = FParamName dec
    unAlias (LParamName dec) = LParamName dec
    unAlias (IndexName it) = IndexName it

-- | Remove alias information from a program.
removeProgAliases ::
  (RephraseOp (OpC rep)) =>
  Prog (Aliases rep) ->
  Prog rep
removeProgAliases = runIdentity . rephraseProg removeAliases

-- | Remove alias information from a function.
removeFunDefAliases ::
  (RephraseOp (OpC rep)) =>
  FunDef (Aliases rep) ->
  FunDef rep
removeFunDefAliases = runIdentity . rephraseFunDef removeAliases

-- | Remove alias information from an expression.
removeExpAliases ::
  (RephraseOp (OpC rep)) =>
  Exp (Aliases rep) ->
  Exp rep
removeExpAliases = runIdentity . rephraseExp removeAliases

-- | Remove alias information from statements.
removeStmAliases ::
  (RephraseOp (OpC rep)) =>
  Stm (Aliases rep) ->
  Stm rep
removeStmAliases = runIdentity . rephraseStm removeAliases

-- | Remove alias information from body.
removeBodyAliases ::
  (RephraseOp (OpC rep)) =>
  Body (Aliases rep) ->
  Body rep
removeBodyAliases = runIdentity . rephraseBody removeAliases

-- | Remove alias information from lambda.
removeLambdaAliases ::
  (RephraseOp (OpC rep)) =>
  Lambda (Aliases rep) ->
  Lambda rep
removeLambdaAliases = runIdentity . rephraseLambda removeAliases

-- | Remove alias information from pattern.
removePatAliases ::
  Pat (AliasDec, a) ->
  Pat a
removePatAliases = runIdentity . rephrasePat (pure . snd)

-- | Augment a body decoration with aliasing information provided by
-- the statements and result of that body.
mkAliasedBody ::
  (ASTRep rep, AliasedOp (OpC rep), ASTConstraints (OpC rep (Aliases rep))) =>
  BodyDec rep ->
  Stms (Aliases rep) ->
  Result ->
  Body (Aliases rep)
mkAliasedBody dec stms res =
  Body (mkBodyAliasing stms res, dec) stms res

-- | Augment a pattern with aliasing information provided by the
-- expression the pattern is bound to.
mkAliasedPat ::
  (Aliased rep, Typed dec) =>
  Pat dec ->
  Exp rep ->
  Pat (VarAliases, dec)
mkAliasedPat (Pat pes) e =
  Pat $ zipWith annotate pes $ expAliases pes e
  where
    annotate (PatElem v dec) names = PatElem v (AliasDec names', dec)
      where
        names' =
          case typeOf dec of
            Array {} -> names
            Mem _ -> names
            _ -> mempty

-- | Given statements (with aliasing information) and a body result,
-- produce aliasing information for the corresponding body as a whole.
-- The aliasing includes names bound in the body, i.e. which are not
-- in scope outside of it.  Note that this does *not* include aliases
-- of results that are not bound in the statements!
mkBodyAliasing ::
  (Aliased rep) =>
  Stms rep ->
  Result ->
  BodyAliasing
mkBodyAliasing stms res =
  -- We need to remove the names that are bound in stms from the alias
  -- and consumption sets.  We do this by computing the transitive
  -- closure of the alias map (within stms), then removing anything
  -- bound in stms.
  let (aliases, consumed) = mkStmsAliases stms res
      boundNames = foldMap (namesFromList . patNames . stmPat) stms
      consumed' = consumed `namesSubtract` boundNames
   in (map AliasDec aliases, AliasDec consumed')

-- | The aliases of the result and everything consumed in the given
-- statements.
mkStmsAliases ::
  (Aliased rep) =>
  Stms rep ->
  Result ->
  ([Names], Names)
mkStmsAliases stms res = delve mempty $ stmsToList stms
  where
    delve (aliasmap, consumed) [] =
      ( map (aliasClosure aliasmap . subExpAliases . resSubExp) res,
        consumed
      )
    delve (aliasmap, consumed) (stm : stms') =
      delve (trackAliases (aliasmap, consumed) stm) stms'
    aliasClosure aliasmap names =
      names <> mconcat (map look $ namesToList names)
      where
        look k = M.findWithDefault mempty k aliasmap

-- | A tuple of a mapping from variable names to their aliases, and
-- the names of consumed variables.
type AliasesAndConsumed =
  ( M.Map VName Names,
    Names
  )

-- | The variables consumed in these statements.
consumedInStms :: (Aliased rep) => Stms rep -> Names
consumedInStms = snd . flip mkStmsAliases []

-- | A helper function for computing the aliases of a sequence of
-- statements.  You'd use this while recursing down the statements
-- from first to last.  The 'AliasesAndConsumed' parameter is the
-- current "state" of aliasing, and the function then returns a new
-- state.  The main thing this function provides is proper handling of
-- transitivity and "reverse" aliases.
trackAliases ::
  (Aliased rep) =>
  AliasesAndConsumed ->
  Stm rep ->
  AliasesAndConsumed
trackAliases (aliasmap, consumed) stm =
  let pat = stmPat stm
      pe_als =
        zip (patNames pat) $ map addAliasesOfAliases $ patAliases pat
      als = M.fromList pe_als
      rev_als = foldMap revAls pe_als
      revAls (v, v_als) =
        M.fromList $ map (,oneName v) $ namesToList v_als
      comb = M.unionWith (<>)
      aliasmap' = rev_als `comb` als `comb` aliasmap
      consumed' = consumed <> addAliasesOfAliases (consumedInStm stm)
   in (aliasmap', consumed')
  where
    addAliasesOfAliases names = names <> aliasesOfAliases names
    aliasesOfAliases = mconcat . map look . namesToList
    look k = M.findWithDefault mempty k aliasmap

mkAliasedStm ::
  (ASTRep rep, AliasedOp (OpC rep), ASTConstraints (OpC rep (Aliases rep))) =>
  Pat (LetDec rep) ->
  StmAux (ExpDec rep) ->
  Exp (Aliases rep) ->
  Stm (Aliases rep)
mkAliasedStm pat (StmAux cs attrs dec) e =
  Let
    (mkAliasedPat pat e)
    (StmAux cs attrs (AliasDec $ consumedInExp e, dec))
    e

instance
  ( Buildable rep,
    AliasedOp (OpC rep),
    ASTConstraints (OpC rep (Aliases rep))
  ) =>
  Buildable (Aliases rep)
  where
  mkExpDec pat e =
    let dec = mkExpDec (removePatAliases pat) $ removeExpAliases e
     in (AliasDec $ consumedInExp e, dec)

  mkExpPat ids e =
    mkAliasedPat (mkExpPat ids $ removeExpAliases e) e

  mkLetNames names e = do
    env <- asksScope removeScopeAliases
    flip runReaderT env $ do
      Let pat dec _ <- mkLetNames names $ removeExpAliases e
      pure $ mkAliasedStm pat dec e

  mkBody stms res =
    let Body bodyrep _ _ = mkBody (fmap removeStmAliases stms) res
     in mkAliasedBody bodyrep stms res

instance
  ( ASTRep rep,
    AliasedOp (OpC rep),
    Buildable (Aliases rep)
  ) =>
  BuilderOps (Aliases rep)

-- | What we require of an aliasable representation.
type AliasableRep rep =
  ( ASTRep rep,
    RephraseOp (OpC rep),
    CanBeAliased (OpC rep),
    AliasedOp (OpC rep),
    ASTConstraints (OpC rep (Aliases rep))
  )

-- | The class of operations that can be given aliasing information.
-- This is a somewhat subtle concept that is only used in the
-- simplifier and when using "rep adapters".
class CanBeAliased op where
  -- | Add aliases to this op.
  addOpAliases ::
    (AliasableRep rep) => AliasTable -> op rep -> op (Aliases rep)

instance CanBeAliased NoOp where
  addOpAliases _ NoOp = NoOp
