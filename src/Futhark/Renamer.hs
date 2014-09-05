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
  , renameBody
  , renameLambda
  , renameFun
  -- * Renaming annotations
  , RenameM
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

import qualified Futhark.Representation.AST.Lore as Lore
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

type RenameM = StateT VNameSource (Reader RenameEnv)

-- | Return a fresh, unique name.  The @VName@ is prepended to the
-- name.
new :: VName -> RenameM VName
new k = do (k', src') <- asks envNameFn <*> get <*> pure k
           put src'
           return k'

class Rename a where
  rename :: a -> RenameM a

instance Rename VName where
  rename name = fromMaybe name <$>
                asks (HM.lookup name . envNameMap)

instance Rename a => Rename [a] where
  rename = mapM rename

instance (Rename a, Rename b) => Rename (a,b) where
  rename (a,b) = (,) <$> rename a <*> rename b

instance Rename a => Rename (Maybe a) where
  rename = maybe (return Nothing) (liftM Just . rename)

instance Rename Bool where
  rename = return

instance (Rename als, Rename shape) =>
         Rename (IdentBase als shape) where
  rename (Ident name tp loc) = do
    name' <- rename name
    tp' <- rename tp
    return $ Ident name' tp' loc

bind :: [IdentBase als shape] -> RenameM a -> RenameM a
bind vars body = do
  vars' <- mapM new varnames
  -- This works because map union prefers elements from left
  -- operand.
  local (bind' vars') body
  where varnames = map identName vars
        bind' vars' env = env { envNameMap = HM.fromList (zip varnames vars')
                                             `HM.union` envNameMap env }

instance Renameable lore => Rename (FunDec lore) where
  rename (fname, ret, params, body, loc) =
    bind params $ do
      params' <- mapM rename params
      body' <- rename body
      ret' <- mapM rename ret
      return (fname, ret', params', body', loc)

instance Rename SubExp where
  rename (Var v)          = Var <$> rename v
  rename (Constant v loc) = return $ Constant v loc

instance Rename Result where
  rename (Result cs ses loc) =
    Result <$> mapM rename cs <*> mapM rename ses <*> pure loc

instance Renameable lore => Rename (Bindee lore) where
  rename (Bindee ident lore) = Bindee <$> rename ident <*> rename lore

instance Renameable lore => Rename (Pattern lore) where
  rename (Pattern l) = Pattern <$> rename l

instance Renameable lore => Rename (Body lore) where
  rename (Body [] res) = Body [] <$> rename res
  rename (Body (Let pat lore e:bnds) res) = do
    e1' <- rename e
    lore' <- rename lore
    bind (patternIdents pat) $ do
      pat' <- rename pat
      Body bnds' res' <- rename $ Body bnds res
      return $ Body (Let pat' lore' e1':bnds') res'

instance Renameable lore => Rename (Exp lore) where
  rename (DoLoop respat merge loopvar boundexp loopbody loc) = do
    let (mergepat, mergeexp) = unzip merge
    boundexp' <- rename boundexp
    mergeexp' <- mapM rename mergeexp
    bind mergepat $ do
      mergepat' <- mapM rename mergepat
      respat' <- mapM rename respat
      bind [loopvar] $ do
        loopvar'  <- rename loopvar
        loopbody' <- rename loopbody
        return $ DoLoop respat' (zip mergepat' mergeexp')
                        loopvar' boundexp' loopbody' loc
  rename e = mapExpM mapper e
    where mapper = Mapper {
                      mapOnBinding = fail "Unhandled binding in Renamer"
                    , mapOnBody = rename
                    , mapOnSubExp = rename
                    , mapOnIdent = rename
                    , mapOnLambda = rename
                    , mapOnType = rename
                    , mapOnValue = return
                    , mapOnCertificates = mapM rename
                    }

instance (Rename als, Rename shape) =>
         Rename (TypeBase als shape) where
  rename (Array et size u als) = do
    als' <- rename als
    size' <- rename size
    return $ Array et size' u als'
  rename (Basic et) = return $ Basic et

instance Renameable lore => Rename (Lambda lore) where
  rename (Lambda params body ret loc) =
    bind params $ do
      params' <- mapM rename params
      body' <- rename body
      ret' <- mapM rename ret
      return $ Lambda params' body' ret' loc

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

-- | A class for lores in which all annotations are renameable.
class (Rename (Lore.Binding lore),
       Rename (Lore.Exp lore)) =>
      Renameable lore where
