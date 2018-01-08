{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- | This module provides facilities for transforming Futhark programs such
-- that names are unique, via the 'renameProg' function.
-- Additionally, the module also supports adding integral \"tags\" to
-- names (incarnated as the 'ID' type), in order to support more
-- efficient comparisons and renamings.  This is done by 'tagProg'.
-- The intent is that you call 'tagProg' once at some early stage,
-- then use 'renameProg' from then on.  Functions are also provided
-- for removing the tags again from expressions, patterns and typs.
module Futhark.Transform.Rename
  (
  -- * Renaming programs
   renameProg
  -- * Renaming parts of a program.
  --
  -- These all require execution in a 'MonadFreshNames' environment.
  , renameExp
  , renameStm
  , renameBody
  , renameLambda
  , renameFun
  -- * Renaming annotations
  , RenameM
  , substituteRename
  , bindingForRename
  , Rename (..)
  , Renameable
  )
  where

import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.FreshNames
import Futhark.MonadFreshNames (MonadFreshNames(..),
                                modifyNameSource)
import Futhark.Transform.Substitute

runRenamer :: RenameM a -> VNameSource -> (a, VNameSource)
runRenamer m src = runReader (runStateT m src) env
  where env = RenameEnv M.empty newName

-- | Rename variables such that each is unique.  The semantics of the
-- program are unaffected, under the assumption that the program was
-- correct to begin with.  In particular, the renaming may make an
-- invalid program valid.
renameProg :: (Renameable lore, MonadFreshNames m) =>
              Prog lore -> m (Prog lore)
renameProg prog = modifyNameSource $
                  runRenamer $ Prog <$> mapM rename (progFunctions prog)

-- | Rename bound variables such that each is unique.  The semantics
-- of the expression is unaffected, under the assumption that the
-- expression was correct to begin with.  Any free variables are left
-- untouched.
renameExp :: (Renameable lore, MonadFreshNames m) =>
             Exp lore -> m (Exp lore)
renameExp = modifyNameSource . runRenamer . rename

-- | Rename bound variables such that each is unique.  The semantics
-- of the binding is unaffected, under the assumption that the
-- binding was correct to begin with.  Any free variables are left
-- untouched, as are the names in the pattern of the binding.
renameStm :: (Renameable lore, MonadFreshNames m) =>
             Stm lore -> m (Stm lore)
renameStm binding = do
  e <- renameExp $ stmExp binding
  return binding { stmExp = e }

-- | Rename bound variables such that each is unique.  The semantics
-- of the body is unaffected, under the assumption that the body was
-- correct to begin with.  Any free variables are left untouched.
renameBody :: (Renameable lore, MonadFreshNames m) =>
              Body lore -> m (Body lore)
renameBody = modifyNameSource . runRenamer . rename

-- | Rename bound variables such that each is unique.  The semantics
-- of the lambda is unaffected, under the assumption that the body was
-- correct to begin with.  Any free variables are left untouched.
-- Note in particular that the parameters of the lambda are renamed.
renameLambda :: (Renameable lore, MonadFreshNames m) =>
                Lambda lore -> m (Lambda lore)
renameLambda = modifyNameSource . runRenamer . rename

-- | Rename bound variables such that each is unique.  The semantics
-- of the function is unaffected, under the assumption that the body
-- was correct to begin with.  Any free variables are left untouched.
-- Note in particular that the parameters of the lambda are renamed.
renameFun :: (Renameable lore, MonadFreshNames m) =>
             FunDef lore -> m (FunDef lore)
renameFun = modifyNameSource . runRenamer . rename

data RenameEnv = RenameEnv {
    envNameMap :: M.Map VName VName
  , envNameFn  :: VNameSource -> VName -> (VName, VNameSource)
  }

-- | The monad in which renaming is performed.
type RenameM = StateT VNameSource (Reader RenameEnv)

-- | Produce a map of the substitutions that should be performed by
-- the renamer.
renamerSubstitutions :: RenameM Substitutions
renamerSubstitutions = lift $ asks envNameMap

-- | Perform a renaming using the 'Substitute' instance.  This only
-- works if the argument does not itself perform any name binding, but
-- it can save on boilerplate for simple types.
substituteRename :: Substitute a => a -> RenameM a
substituteRename x = do
  substs <- renamerSubstitutions
  return $ substituteNames substs x

-- | Return a fresh, unique name.  The @VName@ is prepended to the
-- name.
new :: VName -> RenameM VName
new k = do (k', src') <- asks envNameFn <*> get <*> pure k
           put src'
           return k'

-- | Members of class 'Rename' can be uniquely renamed.
class Rename a where
  -- | Rename the given value such that it does not contain shadowing,
  -- and has incorporated any substitutions present in the 'RenameM'
  -- environment.
  rename :: a -> RenameM a

instance Rename VName where
  rename name = fromMaybe name <$>
                asks (M.lookup name . envNameMap)

instance Rename a => Rename [a] where
  rename = mapM rename

instance Rename (Stm lore) => Rename (Stms lore) where
  rename = fmap stmsFromList . mapM rename . stmsToList

instance (Rename a, Rename b) => Rename (a,b) where
  rename (a,b) = (,) <$> rename a <*> rename b

instance (Rename a, Rename b, Rename c) => Rename (a,b,c) where
  rename (a,b,c) = do
    a' <- rename a
    b' <- rename b
    c' <- rename c
    return (a',b',c')

instance Rename a => Rename (Maybe a) where
  rename = maybe (return Nothing) (fmap Just . rename)

instance Rename Bool where
  rename = return

instance Rename Ident where
  rename (Ident name tp) = do
    name' <- rename name
    tp' <- rename tp
    return $ Ident name' tp'

-- | Create a bunch of new names and bind them for substitution.
bindingForRename :: [VName] -> RenameM a -> RenameM a
bindingForRename = bind

bind :: [VName] -> RenameM a -> RenameM a
bind vars body = do
  vars' <- mapM new vars
  -- This works because map union prefers elements from left
  -- operand.
  local (bind' vars') body
  where bind' vars' env = env { envNameMap = M.fromList (zip vars vars')
                                             `M.union` envNameMap env }

instance Renameable lore => Rename (FunDef lore) where
  rename (FunDef entry fname ret params body) =
    bind (map paramName params) $ do
      params' <- mapM rename params
      body' <- rename body
      ret' <- rename ret
      return $ FunDef entry fname ret' params' body'

instance Rename SubExp where
  rename (Var v)      = Var <$> rename v
  rename (Constant v) = return $ Constant v

instance Rename attr => Rename (ParamT attr) where
  rename (Param name attr) = Param <$> rename name <*> rename attr

instance Rename attr => Rename (PatternT attr) where
  rename (Pattern context values) = Pattern <$> rename context <*> rename values

instance Rename attr => Rename (PatElemT attr) where
  rename (PatElem ident attr) = PatElem <$> rename ident <*> rename attr

instance Rename Certificates where
  rename (Certificates cs) = Certificates <$> rename cs

instance Rename attr => Rename (StmAux attr) where
  rename (StmAux cs attr) =
    StmAux <$> rename cs <*> rename attr

instance Renameable lore => Rename (Body lore) where
  rename (Body attr stms res) = do
    attr' <- rename attr
    (stms', res') <- descend $ stmsToList stms
    return $ Body attr' (stmsFromList stms') res'
    where descend [] = (,) [] <$> rename res
          descend (stm:stms') =
            bind (patternNames $ stmPattern stm) $ do
              stm' <- rename stm
              (stms'', res') <- descend stms'
              return (stm':stms'', res')

instance Renameable lore => Rename (Stm lore) where
  rename (Let pat elore e) = Let <$> rename pat <*> rename elore <*> rename e

instance Renameable lore => Rename (Exp lore) where
  rename (DoLoop ctx val form loopbody) = do
    let (ctxparams, ctxinit) = unzip ctx
        (valparams, valinit) = unzip val
    ctxinit' <- mapM rename ctxinit
    valinit' <- mapM rename valinit
    case form of
      ForLoop loopvar it boundexp loop_vars -> do
        let (loop_params, loop_arrs) = unzip loop_vars
        boundexp' <- rename boundexp
        loop_arrs' <- rename loop_arrs
        bind (map paramName (ctxparams++valparams) ++
              map paramName loop_params) $ do
          ctxparams' <- mapM rename ctxparams
          valparams' <- mapM rename valparams
          loop_params' <- mapM rename loop_params
          bind [loopvar] $ do
            loopvar'  <- rename loopvar
            loopbody' <- rename loopbody
            return $ DoLoop
              (zip ctxparams' ctxinit') (zip valparams' valinit')
              (ForLoop loopvar' it boundexp' $
               zip loop_params' loop_arrs') loopbody'
      WhileLoop cond ->
        bind (map paramName $ ctxparams++valparams) $ do
          ctxparams' <- mapM rename ctxparams
          valparams' <- mapM rename valparams
          loopbody' <- rename loopbody
          cond'     <- rename cond
          return $ DoLoop
            (zip ctxparams' ctxinit') (zip valparams' valinit')
            (WhileLoop cond') loopbody'
  rename e = mapExpM mapper e
    where mapper = Mapper {
                      mapOnBody = const rename
                    , mapOnSubExp = rename
                    , mapOnVName = rename
                    , mapOnCertificates = rename
                    , mapOnRetType = rename
                    , mapOnBranchType = rename
                    , mapOnFParam = rename
                    , mapOnLParam = rename
                    , mapOnOp = rename
                    }

instance Rename shape =>
         Rename (TypeBase shape u) where
  rename (Array et size u) = do
    size' <- rename size
    return $ Array et size' u
  rename (Prim et) = return $ Prim et
  rename (Mem e space) = Mem <$> rename e <*> pure space

instance Renameable lore => Rename (Lambda lore) where
  rename (Lambda params body ret) =
    bind (map paramName params) $ do
      params' <- mapM rename params
      body' <- rename body
      ret' <- mapM rename ret
      return $ Lambda params' body' ret'

instance Rename Names where
  rename = fmap S.fromList . mapM rename . S.toList

instance Rename Rank where
  rename = return

instance Rename d => Rename (ShapeBase d) where
  rename (Shape l) = Shape <$> mapM rename l

instance Rename ExtSize where
  rename (Free se) = Free <$> rename se
  rename (Ext x)   = return $ Ext x

instance Rename () where
  rename = return

instance Rename d => Rename (DimIndex d) where
  rename (DimFix i)       = DimFix <$> rename i
  rename (DimSlice i n s) = DimSlice <$> rename i <*> rename n <*> rename s

-- | Lores in which all annotations are renameable.
type Renameable lore = (Rename (LetAttr lore),
                        Rename (ExpAttr lore),
                        Rename (BodyAttr lore),
                        Rename (FParamAttr lore),
                        Rename (LParamAttr lore),
                        Rename (RetType lore),
                        Rename (BranchType lore),
                        Rename (Op lore))
