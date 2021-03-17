{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides facilities for transforming Futhark programs
-- such that names are unique, via the 'renameProg' function.
module Futhark.Transform.Rename
  ( -- * Renaming programs
    renameProg,

    -- * Renaming parts of a program.

    --
    -- These all require execution in a 'MonadFreshNames' environment.
    renameExp,
    renameStm,
    renameBody,
    renameLambda,
    renamePattern,
    renameSomething,

    -- * Renaming annotations
    RenameM,
    substituteRename,
    renamingStms,
    Rename (..),
    Renameable,
  )
where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Maybe
import Futhark.FreshNames hiding (newName)
import Futhark.IR.Prop.Names
import Futhark.IR.Prop.Patterns
import Futhark.IR.Syntax
import Futhark.IR.Traversals
import Futhark.MonadFreshNames (MonadFreshNames (..), modifyNameSource, newName)
import Futhark.Transform.Substitute

runRenamer :: RenameM a -> VNameSource -> (a, VNameSource)
runRenamer (RenameM m) src = runReader (runStateT m src) env
  where
    env = RenameEnv M.empty

-- | Rename variables such that each is unique.  The semantics of the
-- program are unaffected, under the assumption that the program was
-- correct to begin with.  In particular, the renaming may make an
-- invalid program valid.
renameProg ::
  (Renameable lore, MonadFreshNames m) =>
  Prog lore ->
  m (Prog lore)
renameProg prog = modifyNameSource $
  runRenamer $
    renamingStms (progConsts prog) $ \consts -> do
      funs <- mapM rename (progFuns prog)
      return prog {progConsts = consts, progFuns = funs}

-- | Rename bound variables such that each is unique.  The semantics
-- of the expression is unaffected, under the assumption that the
-- expression was correct to begin with.  Any free variables are left
-- untouched.
renameExp ::
  (Renameable lore, MonadFreshNames m) =>
  Exp lore ->
  m (Exp lore)
renameExp = modifyNameSource . runRenamer . rename

-- | Rename bound variables such that each is unique.  The semantics
-- of the binding is unaffected, under the assumption that the
-- binding was correct to begin with.  Any free variables are left
-- untouched, as are the names in the pattern of the binding.
renameStm ::
  (Renameable lore, MonadFreshNames m) =>
  Stm lore ->
  m (Stm lore)
renameStm binding = do
  e <- renameExp $ stmExp binding
  return binding {stmExp = e}

-- | Rename bound variables such that each is unique.  The semantics
-- of the body is unaffected, under the assumption that the body was
-- correct to begin with.  Any free variables are left untouched.
renameBody ::
  (Renameable lore, MonadFreshNames m) =>
  Body lore ->
  m (Body lore)
renameBody = modifyNameSource . runRenamer . rename

-- | Rename bound variables such that each is unique.  The semantics
-- of the lambda is unaffected, under the assumption that the body was
-- correct to begin with.  Any free variables are left untouched.
-- Note in particular that the parameters of the lambda are renamed.
renameLambda ::
  (Renameable lore, MonadFreshNames m) =>
  Lambda lore ->
  m (Lambda lore)
renameLambda = modifyNameSource . runRenamer . rename

-- | Produce an equivalent pattern but with each pattern element given
-- a new name.
renamePattern ::
  (Rename dec, MonadFreshNames m) =>
  PatternT dec ->
  m (PatternT dec)
renamePattern = modifyNameSource . runRenamer . rename'
  where
    rename' pat = bind (patternNames pat) $ rename pat

-- | Rename the bound variables in something (does not affect free variables).
renameSomething ::
  (Rename a, MonadFreshNames m) =>
  a ->
  m a
renameSomething = modifyNameSource . runRenamer . rename

newtype RenameEnv = RenameEnv {envNameMap :: M.Map VName VName}

-- | The monad in which renaming is performed.
newtype RenameM a = RenameM (StateT VNameSource (Reader RenameEnv) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadFreshNames,
      MonadReader RenameEnv
    )

-- | Produce a map of the substitutions that should be performed by
-- the renamer.
renamerSubstitutions :: RenameM Substitutions
renamerSubstitutions = asks envNameMap

-- | Perform a renaming using the 'Substitute' instance.  This only
-- works if the argument does not itself perform any name binding, but
-- it can save on boilerplate for simple types.
substituteRename :: Substitute a => a -> RenameM a
substituteRename x = do
  substs <- renamerSubstitutions
  return $ substituteNames substs x

-- | Members of class 'Rename' can be uniquely renamed.
class Rename a where
  -- | Rename the given value such that it does not contain shadowing,
  -- and has incorporated any substitutions present in the 'RenameM'
  -- environment.
  rename :: a -> RenameM a

instance Rename VName where
  rename name = asks (fromMaybe name . M.lookup name . envNameMap)

instance Rename a => Rename [a] where
  rename = mapM rename

instance (Rename a, Rename b) => Rename (a, b) where
  rename (a, b) = (,) <$> rename a <*> rename b

instance (Rename a, Rename b, Rename c) => Rename (a, b, c) where
  rename (a, b, c) = do
    a' <- rename a
    b' <- rename b
    c' <- rename c
    return (a', b', c')

instance Rename a => Rename (Maybe a) where
  rename = maybe (return Nothing) (fmap Just . rename)

instance Rename Bool where
  rename = return

instance Rename Ident where
  rename (Ident name tp) = do
    name' <- rename name
    tp' <- rename tp
    return $ Ident name' tp'

bind :: [VName] -> RenameM a -> RenameM a
bind vars body = do
  vars' <- mapM newName vars
  -- This works because map union prefers elements from left
  -- operand.
  local (bind' vars') body
  where
    bind' vars' env =
      env
        { envNameMap =
            M.fromList (zip vars vars')
              `M.union` envNameMap env
        }

-- | Rename some statements, then execute an action with the name
-- substitutions induced by the statements active.
renamingStms :: Renameable lore => Stms lore -> (Stms lore -> RenameM a) -> RenameM a
renamingStms stms m = descend mempty stms
  where
    descend stms' rem_stms = case stmsHead rem_stms of
      Nothing -> m stms'
      Just (stm, rem_stms') -> bind (patternNames $ stmPattern stm) $ do
        stm' <- rename stm
        descend (stms' <> oneStm stm') rem_stms'

instance Renameable lore => Rename (FunDef lore) where
  rename (FunDef entry attrs fname ret params body) =
    bind (map paramName params) $ do
      params' <- mapM rename params
      body' <- rename body
      ret' <- rename ret
      return $ FunDef entry attrs fname ret' params' body'

instance Rename SubExp where
  rename (Var v) = Var <$> rename v
  rename (Constant v) = return $ Constant v

instance Rename dec => Rename (Param dec) where
  rename (Param name dec) = Param <$> rename name <*> rename dec

instance Rename dec => Rename (PatternT dec) where
  rename (Pattern context values) = Pattern <$> rename context <*> rename values

instance Rename dec => Rename (PatElemT dec) where
  rename (PatElem ident dec) = PatElem <$> rename ident <*> rename dec

instance Rename Certificates where
  rename (Certificates cs) = Certificates <$> rename cs

instance Rename Attrs where
  rename = pure

instance Rename dec => Rename (StmAux dec) where
  rename (StmAux cs attrs dec) =
    StmAux <$> rename cs <*> rename attrs <*> rename dec

instance Renameable lore => Rename (Body lore) where
  rename (Body dec stms res) = do
    dec' <- rename dec
    renamingStms stms $ \stms' ->
      Body dec' stms' <$> rename res

instance Renameable lore => Rename (Stm lore) where
  rename (Let pat elore e) = Let <$> rename pat <*> rename elore <*> rename e

instance Renameable lore => Rename (Exp lore) where
  rename (WithAcc shape accarrs lam op) =
    WithAcc <$> rename shape <*> rename accarrs <*> rename lam <*> rename op
  rename (DoLoop ctx val form loopbody) = do
    let (ctxparams, ctxinit) = unzip ctx
        (valparams, valinit) = unzip val
    ctxinit' <- mapM rename ctxinit
    valinit' <- mapM rename valinit
    case form of
      -- It is important that 'i' is renamed before the loop_vars, as
      -- 'i' may be used in the annotations for loop_vars (e.g. index
      -- functions).
      ForLoop i it boundexp loop_vars -> bind [i] $ do
        let (loop_params, loop_arrs) = unzip loop_vars
        boundexp' <- rename boundexp
        loop_arrs' <- rename loop_arrs
        bind
          ( map paramName (ctxparams ++ valparams)
              ++ map paramName loop_params
          )
          $ do
            ctxparams' <- mapM rename ctxparams
            valparams' <- mapM rename valparams
            loop_params' <- mapM rename loop_params
            i' <- rename i
            loopbody' <- rename loopbody
            return $
              DoLoop
                (zip ctxparams' ctxinit')
                (zip valparams' valinit')
                ( ForLoop i' it boundexp' $
                    zip loop_params' loop_arrs'
                )
                loopbody'
      WhileLoop cond ->
        bind (map paramName $ ctxparams ++ valparams) $ do
          ctxparams' <- mapM rename ctxparams
          valparams' <- mapM rename valparams
          loopbody' <- rename loopbody
          cond' <- rename cond
          return $
            DoLoop
              (zip ctxparams' ctxinit')
              (zip valparams' valinit')
              (WhileLoop cond')
              loopbody'
  rename e = mapExpM mapper e
    where
      mapper =
        Mapper
          { mapOnBody = const rename,
            mapOnSubExp = rename,
            mapOnVName = rename,
            mapOnRetType = rename,
            mapOnBranchType = rename,
            mapOnFParam = rename,
            mapOnLParam = rename,
            mapOnOp = rename
          }

instance Rename ElemType where
  rename (ElemPrim t) = pure $ ElemPrim t
  rename (ElemAcc acc ispace ts) =
    ElemAcc <$> rename acc <*> rename ispace <*> rename ts

instance Rename shape => Rename (TypeBase shape u) where
  rename (Array et size u) = Array <$> rename et <*> rename size <*> pure u
  rename (Prim t) = return $ Prim t
  rename (Mem space) = pure $ Mem space
  rename (Acc acc ispace ts) =
    Acc <$> rename acc <*> rename ispace <*> rename ts

instance Renameable lore => Rename (Lambda lore) where
  rename (Lambda params body ret) =
    bind (map paramName params) $ do
      params' <- mapM rename params
      body' <- rename body
      ret' <- mapM rename ret
      return $ Lambda params' body' ret'

instance Rename Names where
  rename = fmap namesFromList . mapM rename . namesToList

instance Rename Rank where
  rename = return

instance Rename d => Rename (ShapeBase d) where
  rename (Shape l) = Shape <$> mapM rename l

instance Rename ExtSize where
  rename (Free se) = Free <$> rename se
  rename (Ext x) = return $ Ext x

instance Rename () where
  rename = return

instance Rename d => Rename (DimIndex d) where
  rename (DimFix i) = DimFix <$> rename i
  rename (DimSlice i n s) = DimSlice <$> rename i <*> rename n <*> rename s

-- | Lores in which all annotations are renameable.
type Renameable lore =
  ( Rename (LetDec lore),
    Rename (ExpDec lore),
    Rename (BodyDec lore),
    Rename (FParamInfo lore),
    Rename (LParamInfo lore),
    Rename (RetType lore),
    Rename (BranchType lore),
    Rename (Op lore)
  )
