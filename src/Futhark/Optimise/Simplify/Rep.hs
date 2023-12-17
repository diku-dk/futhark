{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | Representation used by the simplification engine.  It contains
-- aliasing information and a bit of caching for various information
-- that is looked up frequently.  The name is an old relic; feel free
-- to suggest a better one.
module Futhark.Optimise.Simplify.Rep
  ( Wise,
    VarWisdom (..),
    ExpWisdom,
    removeStmWisdom,
    removeLambdaWisdom,
    removeFunDefWisdom,
    removeExpWisdom,
    removePatWisdom,
    removeBodyWisdom,
    removeScopeWisdom,
    addScopeWisdom,
    addWisdomToPat,
    mkWiseBody,
    mkWiseStm,
    mkWiseExpDec,
    CanBeWise (..),

    -- * Constructing representation
    Informing,
    informLambda,
    informFunDef,
    informStms,
    informBody,
  )
where

import Control.Category
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Map.Strict qualified as M
import Futhark.Builder
import Futhark.IR
import Futhark.IR.Aliases
  ( AliasDec (..),
    ConsumedInExp,
    VarAliases,
    unAliases,
  )
import Futhark.IR.Aliases qualified as Aliases
import Futhark.IR.Prop.Aliases
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util.Pretty
import Prelude hiding (id, (.))

-- | Representative phantom type for the simplifier representation.
data Wise rep

-- | The information associated with a let-bound variable.
newtype VarWisdom = VarWisdom {varWisdomAliases :: VarAliases}
  deriving (Eq, Ord, Show)

instance Rename VarWisdom where
  rename = substituteRename

instance Substitute VarWisdom where
  substituteNames substs (VarWisdom als) =
    VarWisdom (substituteNames substs als)

instance FreeIn VarWisdom where
  freeIn' (VarWisdom als) = freeIn' als

-- | Simplifier information about an expression.
data ExpWisdom = ExpWisdom
  { _expWisdomConsumed :: ConsumedInExp,
    -- | The free variables in the expression.
    expWisdomFree :: AliasDec
  }
  deriving (Eq, Ord, Show)

instance FreeIn ExpWisdom where
  freeIn' = mempty

instance FreeDec ExpWisdom where
  precomputed = const . fvNames . unAliases . expWisdomFree

instance Substitute ExpWisdom where
  substituteNames substs (ExpWisdom cons free) =
    ExpWisdom
      (substituteNames substs cons)
      (substituteNames substs free)

instance Rename ExpWisdom where
  rename = substituteRename

-- | Simplifier information about a body.
data BodyWisdom = BodyWisdom
  { bodyWisdomAliases :: [VarAliases],
    bodyWisdomConsumed :: ConsumedInExp,
    bodyWisdomFree :: AliasDec
  }
  deriving (Eq, Ord, Show)

instance Rename BodyWisdom where
  rename = substituteRename

instance Substitute BodyWisdom where
  substituteNames substs (BodyWisdom als cons free) =
    BodyWisdom
      (substituteNames substs als)
      (substituteNames substs cons)
      (substituteNames substs free)

instance FreeIn BodyWisdom where
  freeIn' (BodyWisdom als cons free) =
    freeIn' als <> freeIn' cons <> freeIn' free

instance FreeDec BodyWisdom where
  precomputed = const . fvNames . unAliases . bodyWisdomFree

instance
  ( Informing rep,
    Ord (OpC rep (Wise rep)),
    Eq (OpC rep (Wise rep)),
    Show (OpC rep (Wise rep)),
    IsOp (OpC rep (Wise rep)),
    Pretty (OpC rep (Wise rep))
  ) =>
  RepTypes (Wise rep)
  where
  type LetDec (Wise rep) = (VarWisdom, LetDec rep)
  type ExpDec (Wise rep) = (ExpWisdom, ExpDec rep)
  type BodyDec (Wise rep) = (BodyWisdom, BodyDec rep)
  type FParamInfo (Wise rep) = FParamInfo rep
  type LParamInfo (Wise rep) = LParamInfo rep
  type RetType (Wise rep) = RetType rep
  type BranchType (Wise rep) = BranchType rep
  type OpC (Wise rep) = OpC rep

withoutWisdom ::
  (HasScope (Wise rep) m, Monad m) =>
  ReaderT (Scope rep) m a ->
  m a
withoutWisdom m = do
  scope <- asksScope removeScopeWisdom
  runReaderT m scope

instance (Informing rep, IsOp (OpC rep (Wise rep))) => ASTRep (Wise rep) where
  expTypesFromPat =
    withoutWisdom . expTypesFromPat . removePatWisdom

instance Pretty VarWisdom where
  pretty _ = pretty ()

instance (Informing rep, Pretty (OpC rep (Wise rep))) => PrettyRep (Wise rep) where
  ppExpDec (_, dec) = ppExpDec dec . removeExpWisdom

instance AliasesOf (VarWisdom, dec) where
  aliasesOf = unAliases . varWisdomAliases . fst

instance (Informing rep) => Aliased (Wise rep) where
  bodyAliases = map unAliases . bodyWisdomAliases . fst . bodyDec
  consumedInBody = unAliases . bodyWisdomConsumed . fst . bodyDec

removeWisdom :: (RephraseOp (OpC rep)) => Rephraser Identity (Wise rep) rep
removeWisdom =
  Rephraser
    { rephraseExpDec = pure . snd,
      rephraseLetBoundDec = pure . snd,
      rephraseBodyDec = pure . snd,
      rephraseFParamDec = pure,
      rephraseLParamDec = pure,
      rephraseRetType = pure,
      rephraseBranchType = pure,
      rephraseOp = rephraseInOp removeWisdom
    }

-- | Remove simplifier information from scope.
removeScopeWisdom :: Scope (Wise rep) -> Scope rep
removeScopeWisdom = M.map unAlias
  where
    unAlias (LetName (_, dec)) = LetName dec
    unAlias (FParamName dec) = FParamName dec
    unAlias (LParamName dec) = LParamName dec
    unAlias (IndexName it) = IndexName it

-- | Add simplifier information to scope.  All the aliasing
-- information will be vacuous, however.
addScopeWisdom :: Scope rep -> Scope (Wise rep)
addScopeWisdom = M.map alias
  where
    alias (LetName dec) = LetName (VarWisdom mempty, dec)
    alias (FParamName dec) = FParamName dec
    alias (LParamName dec) = LParamName dec
    alias (IndexName it) = IndexName it

-- | Remove simplifier information from function.
removeFunDefWisdom :: (RephraseOp (OpC rep)) => FunDef (Wise rep) -> FunDef rep
removeFunDefWisdom = runIdentity . rephraseFunDef removeWisdom

-- | Remove simplifier information from statement.
removeStmWisdom :: (RephraseOp (OpC rep)) => Stm (Wise rep) -> Stm rep
removeStmWisdom = runIdentity . rephraseStm removeWisdom

-- | Remove simplifier information from lambda.
removeLambdaWisdom :: (RephraseOp (OpC rep)) => Lambda (Wise rep) -> Lambda rep
removeLambdaWisdom = runIdentity . rephraseLambda removeWisdom

-- | Remove simplifier information from body.
removeBodyWisdom :: (RephraseOp (OpC rep)) => Body (Wise rep) -> Body rep
removeBodyWisdom = runIdentity . rephraseBody removeWisdom

-- | Remove simplifier information from expression.
removeExpWisdom :: (RephraseOp (OpC rep)) => Exp (Wise rep) -> Exp rep
removeExpWisdom = runIdentity . rephraseExp removeWisdom

-- | Remove simplifier information from pattern.
removePatWisdom :: Pat (VarWisdom, a) -> Pat a
removePatWisdom = runIdentity . rephrasePat (pure . snd)

-- | Add simplifier information to pattern.
addWisdomToPat ::
  (Informing rep) =>
  Pat (LetDec rep) ->
  Exp (Wise rep) ->
  Pat (LetDec (Wise rep))
addWisdomToPat pat e =
  f <$> Aliases.mkAliasedPat pat e
  where
    f (als, dec) = (VarWisdom als, dec)

-- | Produce a body with simplifier information.
mkWiseBody ::
  (Informing rep) =>
  BodyDec rep ->
  Stms (Wise rep) ->
  Result ->
  Body (Wise rep)
mkWiseBody dec stms res =
  Body
    ( BodyWisdom aliases consumed (AliasDec $ freeIn $ freeInStmsAndRes stms res),
      dec
    )
    stms
    res
  where
    (aliases, consumed) = Aliases.mkBodyAliasing stms res

-- | Produce a statement with simplifier information.
mkWiseStm ::
  (Informing rep) =>
  Pat (LetDec rep) ->
  StmAux (ExpDec rep) ->
  Exp (Wise rep) ->
  Stm (Wise rep)
mkWiseStm pat (StmAux cs attrs dec) e =
  let pat' = addWisdomToPat pat e
   in Let pat' (StmAux cs attrs $ mkWiseExpDec pat' dec e) e

-- | Produce simplifier information for an expression.
mkWiseExpDec ::
  (Informing rep) =>
  Pat (LetDec (Wise rep)) ->
  ExpDec rep ->
  Exp (Wise rep) ->
  ExpDec (Wise rep)
mkWiseExpDec pat expdec e =
  ( ExpWisdom
      (AliasDec $ consumedInExp e)
      (AliasDec $ freeIn pat <> freeIn expdec <> freeIn e),
    expdec
  )

instance (Buildable rep, Informing rep) => Buildable (Wise rep) where
  mkExpPat ids e =
    addWisdomToPat (mkExpPat ids $ removeExpWisdom e) e

  mkExpDec pat e =
    mkWiseExpDec pat (mkExpDec (removePatWisdom pat) $ removeExpWisdom e) e

  mkLetNames names e = do
    env <- asksScope removeScopeWisdom
    flip runReaderT env $ do
      Let pat dec _ <- mkLetNames names $ removeExpWisdom e
      pure $ mkWiseStm pat dec e

  mkBody stms res =
    let Body bodyrep _ _ = mkBody (fmap removeStmWisdom stms) res
     in mkWiseBody bodyrep stms res

-- | Constraints that let us transform a representation into a 'Wise'
-- representation.
type Informing rep =
  ( ASTRep rep,
    AliasedOp (OpC rep (Wise rep)),
    RephraseOp (OpC rep),
    CanBeWise (OpC rep),
    FreeIn (OpC rep (Wise rep))
  )

-- | A type class for indicating that this operation can be lifted into the simplifier representation.
class CanBeWise op where
  addOpWisdom :: (Informing rep) => op rep -> op (Wise rep)

instance CanBeWise NoOp where
  addOpWisdom NoOp = NoOp

-- | Construct a 'Wise' statement.
informStm :: (Informing rep) => Stm rep -> Stm (Wise rep)
informStm (Let pat aux e) = mkWiseStm pat aux $ informExp e

-- | Construct 'Wise' statements.
informStms :: (Informing rep) => Stms rep -> Stms (Wise rep)
informStms = fmap informStm

-- | Construct a 'Wise' body.
informBody :: (Informing rep) => Body rep -> Body (Wise rep)
informBody (Body dec stms res) = mkWiseBody dec (informStms stms) res

-- | Construct a 'Wise' lambda.
informLambda :: (Informing rep) => Lambda rep -> Lambda (Wise rep)
informLambda (Lambda ps ret body) = Lambda ps ret (informBody body)

-- | Construct a 'Wise' expression.
informExp :: (Informing rep) => Exp rep -> Exp (Wise rep)
informExp (Match cond cases defbody (MatchDec ts ifsort)) =
  Match cond (map (fmap informBody) cases) (informBody defbody) (MatchDec ts ifsort)
informExp (Loop merge form loopbody) =
  Loop merge form $ informBody loopbody
informExp e = runIdentity $ mapExpM mapper e
  where
    mapper =
      Mapper
        { mapOnBody = const $ pure . informBody,
          mapOnSubExp = pure,
          mapOnVName = pure,
          mapOnRetType = pure,
          mapOnBranchType = pure,
          mapOnFParam = pure,
          mapOnLParam = pure,
          mapOnOp = pure . addOpWisdom
        }

-- | Construct a 'Wise' function definition.
informFunDef :: (Informing rep) => FunDef rep -> FunDef (Wise rep)
informFunDef (FunDef entry attrs fname rettype params body) =
  FunDef entry attrs fname rettype params $ informBody body
