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
    renamePat,
    renameSomething,
    renameBound,

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
  (Renameable rep, MonadFreshNames m) =>
  Prog rep ->
  m (Prog rep)
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
  (Renameable rep, MonadFreshNames m) =>
  Exp rep ->
  m (Exp rep)
renameExp = modifyNameSource . runRenamer . rename

-- | Rename bound variables such that each is unique.  The semantics
-- of the binding is unaffected, under the assumption that the
-- binding was correct to begin with.  Any free variables are left
-- untouched, as are the names in the pattern of the binding.
renameStm ::
  (Renameable rep, MonadFreshNames m) =>
  Stm rep ->
  m (Stm rep)
renameStm binding = do
  e <- renameExp $ stmExp binding
  return binding {stmExp = e}

-- | Rename bound variables such that each is unique.  The semantics
-- of the body is unaffected, under the assumption that the body was
-- correct to begin with.  Any free variables are left untouched.
renameBody ::
  (Renameable rep, MonadFreshNames m) =>
  Body rep ->
  m (Body rep)
renameBody = modifyNameSource . runRenamer . rename

-- | Rename bound variables such that each is unique.  The semantics
-- of the lambda is unaffected, under the assumption that the body was
-- correct to begin with.  Any free variables are left untouched.
-- Note in particular that the parameters of the lambda are renamed.
renameLambda ::
  (Renameable rep, MonadFreshNames m) =>
  Lambda rep ->
  m (Lambda rep)
renameLambda = modifyNameSource . runRenamer . rename

-- | Produce an equivalent pattern but with each pattern element given
-- a new name.
renamePat ::
  (Rename dec, MonadFreshNames m) =>
  PatT dec ->
  m (PatT dec)
renamePat = modifyNameSource . runRenamer . rename'
  where
    rename' pat = renameBound (patNames pat) $ rename pat

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

-- | Rename variables in binding position.  The provided VNames are
-- associated with new, fresh names in the renaming environment.
renameBound :: [VName] -> RenameM a -> RenameM a
renameBound vars body = do
  vars' <- mapM newName vars
  -- This works because map union prefers elements from left
  -- operand.
  local (renameBound' vars') body
  where
    renameBound' vars' env =
      env
        { envNameMap =
            M.fromList (zip vars vars')
              `M.union` envNameMap env
        }

-- | Rename some statements, then execute an action with the name
-- substitutions induced by the statements active.
renamingStms :: Renameable rep => Stms rep -> (Stms rep -> RenameM a) -> RenameM a
renamingStms stms m = descend mempty stms
  where
    descend stms' rem_stms = case stmsHead rem_stms of
      Nothing -> m stms'
      Just (stm, rem_stms') -> renameBound (patNames $ stmPat stm) $ do
        stm' <- rename stm
        descend (stms' <> oneStm stm') rem_stms'

instance Renameable rep => Rename (FunDef rep) where
  rename (FunDef entry attrs fname ret params body) =
    renameBound (map paramName params) $ do
      params' <- mapM rename params
      body' <- rename body
      ret' <- rename ret
      return $ FunDef entry attrs fname ret' params' body'

instance Rename SubExp where
  rename (Var v) = Var <$> rename v
  rename (Constant v) = return $ Constant v

instance Rename dec => Rename (Param dec) where
  rename (Param attrs name dec) =
    Param <$> rename attrs <*> rename name <*> rename dec

instance Rename dec => Rename (PatT dec) where
  rename (Pat xs) = Pat <$> rename xs

instance Rename dec => Rename (PatElemT dec) where
  rename (PatElem ident dec) = PatElem <$> rename ident <*> rename dec

instance Rename Certs where
  rename (Certs cs) = Certs <$> rename cs

instance Rename Attrs where
  rename = pure

instance Rename dec => Rename (StmAux dec) where
  rename (StmAux cs attrs dec) =
    StmAux <$> rename cs <*> rename attrs <*> rename dec

instance Rename SubExpRes where
  rename (SubExpRes cs se) = SubExpRes <$> rename cs <*> rename se

instance Renameable rep => Rename (Body rep) where
  rename (Body dec stms res) = do
    dec' <- rename dec
    renamingStms stms $ \stms' ->
      Body dec' stms' <$> rename res

instance Renameable rep => Rename (Stm rep) where
  rename (Let pat dec e) = Let <$> rename pat <*> rename dec <*> rename e

instance Renameable rep => Rename (Exp rep) where
  rename (WithAcc inputs lam) =
    WithAcc <$> rename inputs <*> rename lam
  rename (DoLoop merge form loopbody) = do
    let (params, args) = unzip merge
    args' <- mapM rename args
    case form of
      -- It is important that 'i' is renamed before the loop_vars, as
      -- 'i' may be used in the annotations for loop_vars (e.g. index
      -- functions).
      ForLoop i it boundexp loop_vars -> renameBound [i] $ do
        let (arr_params, loop_arrs) = unzip loop_vars
        boundexp' <- rename boundexp
        loop_arrs' <- rename loop_arrs
        renameBound (map paramName params ++ map paramName arr_params) $ do
          params' <- mapM rename params
          arr_params' <- mapM rename arr_params
          i' <- rename i
          loopbody' <- rename loopbody
          return $
            DoLoop
              (zip params' args')
              (ForLoop i' it boundexp' $ zip arr_params' loop_arrs')
              loopbody'
      WhileLoop cond ->
        renameBound (map paramName params) $ do
          params' <- mapM rename params
          loopbody' <- rename loopbody
          cond' <- rename cond
          return $ DoLoop (zip params' args') (WhileLoop cond') loopbody'
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

instance Rename PrimType where
  rename = pure

instance Rename shape => Rename (TypeBase shape u) where
  rename (Array et size u) = Array <$> rename et <*> rename size <*> pure u
  rename (Prim t) = return $ Prim t
  rename (Mem space) = pure $ Mem space
  rename (Acc acc ispace ts u) =
    Acc <$> rename acc <*> rename ispace <*> rename ts <*> pure u

instance Renameable rep => Rename (Lambda rep) where
  rename (Lambda params body ret) =
    renameBound (map paramName params) $ do
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

-- | Representations in which all decorations are renameable.
type Renameable rep =
  ( Rename (LetDec rep),
    Rename (ExpDec rep),
    Rename (BodyDec rep),
    Rename (FParamInfo rep),
    Rename (LParamInfo rep),
    Rename (RetType rep),
    Rename (BranchType rep),
    Rename (Op rep)
  )
