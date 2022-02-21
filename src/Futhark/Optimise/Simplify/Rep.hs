{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Representation used by the simplification engine.
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
    mkWiseLetStm,
    mkWiseExpDec,
    CanBeWise (..),

    -- * Constructing representation
    Informing,
    informLambda,
    informFunDef,
    informStms,
  )
where

import Control.Category
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Kind
import qualified Data.Map.Strict as M
import Futhark.Analysis.Rephrase
import Futhark.Builder
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
import Futhark.Util.Pretty
import Prelude hiding (id, (.))

data Wise rep

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

instance (RepTypes rep, CanBeWise (Op rep)) => RepTypes (Wise rep) where
  type LetDec (Wise rep) = (VarWisdom, LetDec rep)
  type ExpDec (Wise rep) = (ExpWisdom, ExpDec rep)
  type BodyDec (Wise rep) = (BodyWisdom, BodyDec rep)
  type FParamInfo (Wise rep) = FParamInfo rep
  type LParamInfo (Wise rep) = LParamInfo rep
  type RetType (Wise rep) = RetType rep
  type BranchType (Wise rep) = BranchType rep
  type Op (Wise rep) = OpWithWisdom (Op rep)

withoutWisdom ::
  (HasScope (Wise rep) m, Monad m) =>
  ReaderT (Scope rep) m a ->
  m a
withoutWisdom m = do
  scope <- asksScope removeScopeWisdom
  runReaderT m scope

instance (ASTRep rep, CanBeWise (Op rep)) => ASTRep (Wise rep) where
  expTypesFromPat =
    withoutWisdom . expTypesFromPat . removePatWisdom

instance Pretty VarWisdom where
  ppr _ = ppr ()

instance (PrettyRep rep, CanBeWise (Op rep)) => PrettyRep (Wise rep) where
  ppExpDec (_, dec) = ppExpDec dec . removeExpWisdom

instance AliasesOf (VarWisdom, dec) where
  aliasesOf = unAliases . varWisdomAliases . fst

instance (ASTRep rep, CanBeWise (Op rep)) => Aliased (Wise rep) where
  bodyAliases = map unAliases . bodyWisdomAliases . fst . bodyDec
  consumedInBody = unAliases . bodyWisdomConsumed . fst . bodyDec

removeWisdom :: CanBeWise (Op rep) => Rephraser Identity (Wise rep) rep
removeWisdom =
  Rephraser
    { rephraseExpDec = return . snd,
      rephraseLetBoundDec = return . snd,
      rephraseBodyDec = return . snd,
      rephraseFParamDec = return,
      rephraseLParamDec = return,
      rephraseRetType = return,
      rephraseBranchType = return,
      rephraseOp = return . removeOpWisdom
    }

removeScopeWisdom :: Scope (Wise rep) -> Scope rep
removeScopeWisdom = M.map unAlias
  where
    unAlias (LetName (_, dec)) = LetName dec
    unAlias (FParamName dec) = FParamName dec
    unAlias (LParamName dec) = LParamName dec
    unAlias (IndexName it) = IndexName it

addScopeWisdom :: Scope rep -> Scope (Wise rep)
addScopeWisdom = M.map alias
  where
    alias (LetName dec) = LetName (VarWisdom mempty, dec)
    alias (FParamName dec) = FParamName dec
    alias (LParamName dec) = LParamName dec
    alias (IndexName it) = IndexName it

removeFunDefWisdom :: CanBeWise (Op rep) => FunDef (Wise rep) -> FunDef rep
removeFunDefWisdom = runIdentity . rephraseFunDef removeWisdom

removeStmWisdom :: CanBeWise (Op rep) => Stm (Wise rep) -> Stm rep
removeStmWisdom = runIdentity . rephraseStm removeWisdom

removeLambdaWisdom :: CanBeWise (Op rep) => Lambda (Wise rep) -> Lambda rep
removeLambdaWisdom = runIdentity . rephraseLambda removeWisdom

removeBodyWisdom :: CanBeWise (Op rep) => Body (Wise rep) -> Body rep
removeBodyWisdom = runIdentity . rephraseBody removeWisdom

removeExpWisdom :: CanBeWise (Op rep) => Exp (Wise rep) -> Exp rep
removeExpWisdom = runIdentity . rephraseExp removeWisdom

removePatWisdom :: Pat (VarWisdom, a) -> Pat a
removePatWisdom = runIdentity . rephrasePat (return . snd)

addWisdomToPat ::
  (ASTRep rep, CanBeWise (Op rep)) =>
  Pat (LetDec rep) ->
  Exp (Wise rep) ->
  Pat (LetDec (Wise rep))
addWisdomToPat pat e =
  Pat $ map f $ Aliases.mkPatAliases pat e
  where
    f pe =
      let (als, dec) = patElemDec pe
       in pe `setPatElemDec` (VarWisdom als, dec)

mkWiseBody ::
  (ASTRep rep, CanBeWise (Op rep)) =>
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
    (aliases, consumed) = Aliases.mkBodyAliases stms res

mkWiseLetStm ::
  (ASTRep rep, CanBeWise (Op rep)) =>
  Pat (LetDec rep) ->
  StmAux (ExpDec rep) ->
  Exp (Wise rep) ->
  Stm (Wise rep)
mkWiseLetStm pat (StmAux cs attrs dec) e =
  let pat' = addWisdomToPat pat e
   in Let pat' (StmAux cs attrs $ mkWiseExpDec pat' dec e) e

mkWiseExpDec ::
  (ASTRep rep, CanBeWise (Op rep)) =>
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

instance (Buildable rep, CanBeWise (Op rep)) => Buildable (Wise rep) where
  mkExpPat ids e =
    addWisdomToPat (mkExpPat ids $ removeExpWisdom e) e

  mkExpDec pat e =
    mkWiseExpDec pat (mkExpDec (removePatWisdom pat) $ removeExpWisdom e) e

  mkLetNames names e = do
    env <- asksScope removeScopeWisdom
    flip runReaderT env $ do
      Let pat dec _ <- mkLetNames names $ removeExpWisdom e
      return $ mkWiseLetStm pat dec e

  mkBody stms res =
    let Body bodyrep _ _ = mkBody (fmap removeStmWisdom stms) res
     in mkWiseBody bodyrep stms res

-- | Constraints that let us transform a representation into a 'Wise'
-- representation.
type Informing rep = (ASTRep rep, CanBeWise (Op rep))

class
  ( AliasedOp (OpWithWisdom op),
    IsOp (OpWithWisdom op)
  ) =>
  CanBeWise op
  where
  type OpWithWisdom op :: Data.Kind.Type
  removeOpWisdom :: OpWithWisdom op -> op
  addOpWisdom :: op -> OpWithWisdom op

instance CanBeWise () where
  type OpWithWisdom () = ()
  removeOpWisdom () = ()
  addOpWisdom () = ()

informStm :: Informing rep => Stm rep -> Stm (Wise rep)
informStm (Let pat aux e) = mkWiseLetStm pat aux $ informExp e

informStms :: Informing rep => Stms rep -> Stms (Wise rep)
informStms = fmap informStm

informBody :: Informing rep => Body rep -> Body (Wise rep)
informBody (Body dec stms res) = mkWiseBody dec (informStms stms) res

informLambda :: Informing rep => Lambda rep -> Lambda (Wise rep)
informLambda (Lambda ps body ret) = Lambda ps (informBody body) ret

informExp :: Informing rep => Exp rep -> Exp (Wise rep)
informExp (If cond tbranch fbranch (IfDec ts ifsort)) =
  If cond (informBody tbranch) (informBody fbranch) (IfDec ts ifsort)
informExp (DoLoop merge form loopbody) =
  let form' = case form of
        ForLoop i it bound params -> ForLoop i it bound params
        WhileLoop cond -> WhileLoop cond
   in DoLoop merge form' $ informBody loopbody
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

informFunDef :: Informing rep => FunDef rep -> FunDef (Wise rep)
informFunDef (FunDef entry attrs fname rettype params body) =
  FunDef entry attrs fname rettype params $ informBody body
