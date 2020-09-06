{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definition of the lore used by the simplification engine.
module Futhark.Optimise.Simplify.Lore
  ( Wise,
    VarWisdom (..),
    ExpWisdom,
    removeStmWisdom,
    removeLambdaWisdom,
    removeFunDefWisdom,
    removeExpWisdom,
    removePatternWisdom,
    removeBodyWisdom,
    removeScopeWisdom,
    addScopeWisdom,
    addWisdomToPattern,
    mkWiseBody,
    mkWiseLetStm,
    mkWiseExpDec,
    CanBeWise (..),
  )
where

import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Kind
import qualified Data.Map.Strict as M
import Futhark.Analysis.Rephrase
import Futhark.Binder
import Futhark.IR
import Futhark.IR.Aliases
  ( AliasDec (..),
    ConsumedInExp,
    VarAliases,
    unAliases,
  )
import qualified Futhark.IR.Aliases as Aliases
import Futhark.IR.Prop.Aliases
import Futhark.Transform.Rename
import Futhark.Transform.Substitute

data Wise lore

-- | The wisdom of the let-bound variable.
newtype VarWisdom = VarWisdom {varWisdomAliases :: VarAliases}
  deriving (Eq, Ord, Show)

instance Rename VarWisdom where
  rename = substituteRename

instance Substitute VarWisdom where
  substituteNames substs (VarWisdom als) =
    VarWisdom (substituteNames substs als)

instance FreeIn VarWisdom where
  freeIn' (VarWisdom als) = freeIn' als

-- | Wisdom about an expression.
data ExpWisdom = ExpWisdom
  { _expWisdomConsumed :: ConsumedInExp,
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

-- | Wisdom about a body.
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
  ( Decorations lore,
    CanBeWise (Op lore)
  ) =>
  Decorations (Wise lore)
  where
  type LetDec (Wise lore) = (VarWisdom, LetDec lore)
  type ExpDec (Wise lore) = (ExpWisdom, ExpDec lore)
  type BodyDec (Wise lore) = (BodyWisdom, BodyDec lore)
  type FParamInfo (Wise lore) = FParamInfo lore
  type LParamInfo (Wise lore) = LParamInfo lore
  type RetType (Wise lore) = RetType lore
  type BranchType (Wise lore) = BranchType lore
  type Op (Wise lore) = OpWithWisdom (Op lore)

withoutWisdom ::
  (HasScope (Wise lore) m, Monad m) =>
  ReaderT (Scope lore) m a ->
  m a
withoutWisdom m = do
  scope <- asksScope removeScopeWisdom
  runReaderT m scope

instance (ASTLore lore, CanBeWise (Op lore)) => ASTLore (Wise lore) where
  expTypesFromPattern =
    withoutWisdom . expTypesFromPattern . removePatternWisdom

instance PrettyAnnot (PatElemT dec) => PrettyAnnot (PatElemT (VarWisdom, dec)) where
  ppAnnot = ppAnnot . fmap snd

instance (PrettyLore lore, CanBeWise (Op lore)) => PrettyLore (Wise lore) where
  ppExpLore (_, dec) = ppExpLore dec . removeExpWisdom

instance AliasesOf (VarWisdom, dec) where
  aliasesOf = unAliases . varWisdomAliases . fst

instance (ASTLore lore, CanBeWise (Op lore)) => Aliased (Wise lore) where
  bodyAliases = map unAliases . bodyWisdomAliases . fst . bodyDec
  consumedInBody = unAliases . bodyWisdomConsumed . fst . bodyDec

removeWisdom :: CanBeWise (Op lore) => Rephraser Identity (Wise lore) lore
removeWisdom =
  Rephraser
    { rephraseExpLore = return . snd,
      rephraseLetBoundLore = return . snd,
      rephraseBodyLore = return . snd,
      rephraseFParamLore = return,
      rephraseLParamLore = return,
      rephraseRetType = return,
      rephraseBranchType = return,
      rephraseOp = return . removeOpWisdom
    }

removeScopeWisdom :: Scope (Wise lore) -> Scope lore
removeScopeWisdom = M.map unAlias
  where
    unAlias (LetName (_, dec)) = LetName dec
    unAlias (FParamName dec) = FParamName dec
    unAlias (LParamName dec) = LParamName dec
    unAlias (IndexName it) = IndexName it

addScopeWisdom :: Scope lore -> Scope (Wise lore)
addScopeWisdom = M.map alias
  where
    alias (LetName dec) = LetName (VarWisdom mempty, dec)
    alias (FParamName dec) = FParamName dec
    alias (LParamName dec) = LParamName dec
    alias (IndexName it) = IndexName it

removeFunDefWisdom :: CanBeWise (Op lore) => FunDef (Wise lore) -> FunDef lore
removeFunDefWisdom = runIdentity . rephraseFunDef removeWisdom

removeStmWisdom :: CanBeWise (Op lore) => Stm (Wise lore) -> Stm lore
removeStmWisdom = runIdentity . rephraseStm removeWisdom

removeLambdaWisdom :: CanBeWise (Op lore) => Lambda (Wise lore) -> Lambda lore
removeLambdaWisdom = runIdentity . rephraseLambda removeWisdom

removeBodyWisdom :: CanBeWise (Op lore) => Body (Wise lore) -> Body lore
removeBodyWisdom = runIdentity . rephraseBody removeWisdom

removeExpWisdom :: CanBeWise (Op lore) => Exp (Wise lore) -> Exp lore
removeExpWisdom = runIdentity . rephraseExp removeWisdom

removePatternWisdom :: PatternT (VarWisdom, a) -> PatternT a
removePatternWisdom = runIdentity . rephrasePattern (return . snd)

addWisdomToPattern ::
  (ASTLore lore, CanBeWise (Op lore)) =>
  Pattern lore ->
  Exp (Wise lore) ->
  Pattern (Wise lore)
addWisdomToPattern pat e =
  Pattern (map f ctx) (map f val)
  where
    (ctx, val) = Aliases.mkPatternAliases pat e
    f pe =
      let (als, dec) = patElemDec pe
       in pe `setPatElemLore` (VarWisdom als, dec)

mkWiseBody ::
  (ASTLore lore, CanBeWise (Op lore)) =>
  BodyDec lore ->
  Stms (Wise lore) ->
  Result ->
  Body (Wise lore)
mkWiseBody innerlore bnds res =
  Body
    ( BodyWisdom aliases consumed (AliasDec $ freeIn $ freeInStmsAndRes bnds res),
      innerlore
    )
    bnds
    res
  where
    (aliases, consumed) = Aliases.mkBodyAliases bnds res

mkWiseLetStm ::
  (ASTLore lore, CanBeWise (Op lore)) =>
  Pattern lore ->
  StmAux (ExpDec lore) ->
  Exp (Wise lore) ->
  Stm (Wise lore)
mkWiseLetStm pat (StmAux cs attrs dec) e =
  let pat' = addWisdomToPattern pat e
   in Let pat' (StmAux cs attrs $ mkWiseExpDec pat' dec e) e

mkWiseExpDec ::
  (ASTLore lore, CanBeWise (Op lore)) =>
  Pattern (Wise lore) ->
  ExpDec lore ->
  Exp (Wise lore) ->
  ExpDec (Wise lore)
mkWiseExpDec pat explore e =
  ( ExpWisdom
      (AliasDec $ consumedInExp e)
      (AliasDec $ freeIn pat <> freeIn explore <> freeIn e),
    explore
  )

instance
  ( Bindable lore,
    CanBeWise (Op lore)
  ) =>
  Bindable (Wise lore)
  where
  mkExpPat ctx val e =
    addWisdomToPattern (mkExpPat ctx val $ removeExpWisdom e) e

  mkExpDec pat e =
    mkWiseExpDec pat (mkExpDec (removePatternWisdom pat) $ removeExpWisdom e) e

  mkLetNames names e = do
    env <- asksScope removeScopeWisdom
    flip runReaderT env $ do
      Let pat dec _ <- mkLetNames names $ removeExpWisdom e
      return $ mkWiseLetStm pat dec e

  mkBody bnds res =
    let Body bodylore _ _ = mkBody (fmap removeStmWisdom bnds) res
     in mkWiseBody bodylore bnds res

class
  ( AliasedOp (OpWithWisdom op),
    IsOp (OpWithWisdom op)
  ) =>
  CanBeWise op
  where
  type OpWithWisdom op :: Data.Kind.Type
  removeOpWisdom :: OpWithWisdom op -> op

instance CanBeWise () where
  type OpWithWisdom () = ()
  removeOpWisdom () = ()
