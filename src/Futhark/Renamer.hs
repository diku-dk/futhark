{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
-- | This module provides facilities for transforming Futhark programs such
-- that names are unique, via the 'renameProg' function.
-- Additionally, the module also supports adding integral \"tags\" to
-- names (incarnated as the 'ID' type), in order to support more
-- efficient comparisons and renamings.  This is done by 'tagProg'.
-- The intent is that you call 'tagProg' once at some early stage,
-- then use 'renameProg' from then on.  Functions are also provided
-- for removing the tags again from expressions, patterns and typs.
module Futhark.Renamer
  (
  -- * Renaming programs
   renameProg
  -- * Renaming parts of a program.
  --
  -- These all require execution in a 'MonadFreshNames' environment.
  , renameExp
  , renameBinding
  , renameBody
  , renameLambda
  , renameFun
  -- * Renaming annotations
  , RenameM
  , renamerSubstitutions
  , Rename (..)
  , Renameable
  )
  where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe

import Prelude

import qualified Futhark.Representation.AST.Annotations as Annotations
import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Traversals
import Futhark.FreshNames
import Futhark.MonadFreshNames (MonadFreshNames(..),
                                modifyNameSource,
                                newNameSourceForProg)

runRenamer :: RenameM a -> VNameSource -> (a, VNameSource)
runRenamer m src = runReader (runStateT m src) env
  where env = RenameEnv HM.empty newName

-- | Rename variables such that each is unique.  The semantics of the
-- program are unaffected, under the assumption that the program was
-- correct to begin with.  In particular, the renaming may make an
-- invalid program valid.
renameProg :: Renameable lore => Prog lore -> Prog lore
renameProg prog = Prog $ fst $
                  runRenamer (mapM rename $ progFunctions prog) src
  where src = newNameSourceForProg prog

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
renameBinding :: (Renameable lore, MonadFreshNames m) =>
                 Binding lore -> m (Binding lore)
renameBinding binding = do
  e <- renameExp $ bindingExp binding
  return binding { bindingExp = e }

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
             FunDec lore -> m (FunDec lore)
renameFun = modifyNameSource . runRenamer . rename

data RenameEnv = RenameEnv {
    envNameMap :: HM.HashMap VName VName
  , envNameFn  :: VNameSource -> VName -> (VName, VNameSource)
  }

-- | The monad in which renaming is performed.
type RenameM = StateT VNameSource (Reader RenameEnv)

-- | Produce a map of the substitutions that should be performed by
-- the renamer.
renamerSubstitutions :: RenameM (HM.HashMap VName VName)
renamerSubstitutions = lift $ asks envNameMap

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
                asks (HM.lookup name . envNameMap)

instance Rename a => Rename [a] where
  rename = mapM rename

instance (Rename a, Rename b) => Rename (a,b) where
  rename (a,b) = (,) <$> rename a <*> rename b

instance (Rename a, Rename b, Rename c) => Rename (a,b,c) where
  rename (a,b,c) = do
    a' <- rename a
    b' <- rename b
    c' <- rename c
    return (a',b',c')

instance Rename a => Rename (Maybe a) where
  rename = maybe (return Nothing) (liftM Just . rename)

instance Rename Bool where
  rename = return

instance Rename Ident where
  rename (Ident name tp) = do
    name' <- rename name
    tp' <- rename tp
    return $ Ident name' tp'

bind :: [VName] -> RenameM a -> RenameM a
bind vars body = do
  vars' <- mapM new vars
  -- This works because map union prefers elements from left
  -- operand.
  local (bind' vars') body
  where bind' vars' env = env { envNameMap = HM.fromList (zip vars vars')
                                             `HM.union` envNameMap env }

instance Renameable lore => Rename (FunDec lore) where
  rename (FunDec fname ret params body) =
    bind (map paramName params) $ do
      params' <- mapM rename params
      body' <- rename body
      ret' <- rename ret
      return $ FunDec fname ret' params' body'

instance Rename SubExp where
  rename (Var v)      = Var <$> rename v
  rename (Constant v) = return $ Constant v

instance Rename attr => Rename (ParamT attr) where
  rename (Param ident attr) = Param <$> rename ident <*> rename attr

instance Renameable lore => Rename (Pattern lore) where
  rename (Pattern context values) = Pattern <$> rename context <*> rename values

instance Rename attr => Rename (PatElemT attr) where
  rename (PatElem ident bindage attr) =
    PatElem <$> rename ident <*> rename bindage <*> rename attr

instance Rename Bindage where
  rename BindVar =
    return BindVar
  rename (BindInPlace cs src is) =
    BindInPlace <$>
    mapM rename cs <*>
    rename src <*>
    mapM rename is

instance Renameable lore => Rename (Body lore) where
  rename (Body lore [] res) =
    Body <$> rename lore <*> pure [] <*> rename res
  rename (Body blore (Let pat elore e:bnds) res) = do
    e1' <- rename e
    elore' <- rename elore
    bind (patternNames pat) $ do
      pat' <- rename pat
      Body blore' bnds' res' <- rename $ Body blore bnds res
      return $ Body blore' (Let pat' elore' e1':bnds') res'

instance Renameable lore => Rename (Exp lore) where
  rename (LoopOp (DoLoop respat merge form loopbody)) = do
    let (mergepat, mergeexp) = unzip merge
    mergeexp' <- mapM rename mergeexp
    bind (map paramName mergepat) $ do
      mergepat' <- mapM rename mergepat
      respat' <- mapM rename respat
      case form of
        ForLoop loopvar boundexp -> do
          boundexp' <- rename boundexp
          bind [loopvar] $ do
            loopvar'  <- rename loopvar
            loopbody' <- rename loopbody
            return $ LoopOp $ DoLoop respat' (zip mergepat' mergeexp')
              (ForLoop loopvar' boundexp') loopbody'
        WhileLoop cond -> do
          loopbody' <- rename loopbody
          cond'     <- rename cond
          return $ LoopOp $ DoLoop respat' (zip mergepat' mergeexp')
            (WhileLoop cond') loopbody'
  rename e = mapExpM mapper e
    where mapper = Mapper {
                      mapOnBody = rename
                    , mapOnSubExp = rename
                    , mapOnVName = rename
                    , mapOnLambda = rename
                    , mapOnExtLambda = rename
                    , mapOnCertificates = mapM rename
                    , mapOnRetType = rename
                    , mapOnFParam = rename
                    }

instance (Rename shape) =>
         Rename (TypeBase shape) where
  rename (Array et size u) = do
    size' <- rename size
    return $ Array et size' u
  rename (Basic et) = return $ Basic et
  rename (Mem e) = Mem <$> rename e

instance Renameable lore => Rename (Lambda lore) where
  rename (Lambda index params body ret) =
    bind (index : map paramName params) $ do
      index' <- rename index
      params' <- mapM rename params
      body' <- rename body
      ret' <- mapM rename ret
      return $ Lambda index' params' body' ret'

instance Renameable lore => Rename (ExtLambda lore) where
  rename (ExtLambda index params body rettype) =
    bind (index : map paramName params) $ do
      index' <- rename index
      params' <- mapM rename params
      body' <- rename body
      rettype' <- rename rettype
      return $ ExtLambda index' params' body' rettype'

instance Rename Names where
  rename = liftM HS.fromList . mapM rename . HS.toList

instance Rename Rank where
  rename = return

instance Rename Shape where
  rename (Shape l) = Shape <$> mapM rename l

instance Rename ExtShape where
  rename (ExtShape l) = ExtShape <$> mapM rename l

instance Rename ExtDimSize where
  rename (Free se) = Free <$> rename se
  rename (Ext x)   = return $ Ext x

instance Rename () where
  rename = return

instance Rename ExtRetType where
  rename = liftM ExtRetType . mapM rename . retTypeValues

-- | A class for lores in which all annotations are renameable.
class (Rename (Annotations.LetBound lore),
       Rename (Annotations.Exp lore),
       Rename (Annotations.Body lore),
       Rename (Annotations.FParam lore),
       Rename (Annotations.LParam lore),
       Rename (Annotations.RetType lore)) =>
      Renameable lore where
