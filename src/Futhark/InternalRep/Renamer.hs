{-# LANGUAGE FlexibleInstances #-}
-- | This module provides facilities for transforming Futhark programs such
-- that names are unique, via the 'renameProg' function.
-- Additionally, the module also supports adding integral \"tags\" to
-- names (incarnated as the 'ID' type), in order to support more
-- efficient comparisons and renamings.  This is done by 'tagProg'.
-- The intent is that you call 'tagProg' once at some early stage,
-- then use 'renameProg' from then on.  Functions are also provided
-- for removing the tags again from expressions, patterns and typs.
module Futhark.InternalRep.Renamer
  (
  -- * Renaming programs
   renameProg
  -- * Renaming parts of a program.
  --
  -- These all require execution in a 'MonadFreshNames' environment.
  , renameBody
  , renameLambda
  )
  where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe

import Futhark.InternalRep
import Futhark.FreshNames
import Futhark.MonadFreshNames (MonadFreshNames(..), modifyNameSource)

runRenamer :: RenameM a -> VNameSource -> (a, VNameSource)
runRenamer m src = runReader (runStateT m src) env
  where env = RenameEnv HM.empty newName

-- | Rename variables such that each is unique.  The semantics of the
-- program are unaffected, under the assumption that the program was
-- correct to begin with.  In particular, the renaming may make an
-- invalid program valid.
renameProg :: Prog -> Prog
renameProg prog = Prog $ fst $
                  runRenamer (mapM renameFun $ progFunctions prog) src
  where src = newNameSourceForProg prog

-- | Rename bound variables such that each is unique.  The semantics
-- of the body is unaffected, under the assumption that the body was
-- correct to begin with.  Any free variables are left untouched.
renameBody :: MonadFreshNames m => Body -> m Body
renameBody = modifyNameSource . runRenamer . rename

-- | Rename bound variables such that each is unique.  The semantics
-- of the lambda is unaffected, under the assumption that the body was
-- correct to begin with.  Any free variables are left untouched.
-- Note in particular that the parameters of the lambda are renamed.
renameLambda :: MonadFreshNames m => Lambda -> m Lambda
renameLambda = modifyNameSource . runRenamer . rename

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

instance (Rename als, Rename shape) => Rename (IdentBase als shape) where
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

renameFun :: FunDec -> RenameM FunDec
renameFun (fname, ret, params, body, loc) =
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

instance Rename Body where
  rename (Body [] res) = Body [] <$> rename res
  rename (Body (LetWith cs dest src idxs ve:bnds) res) = do
    cs' <- mapM rename cs
    src' <- rename src
    idxs' <- mapM rename idxs
    ve' <- rename ve
    bind [dest] $ do
      dest' <- rename dest
      Body bnds' res' <- rename $ Body bnds res
      return $ Body (LetWith cs' dest' src' idxs' ve':bnds') res'
  rename (Body (Let pat e:bnds) res) = do
    e1' <- rename e
    bind pat $ do
      pat' <- mapM rename pat
      Body bnds' res' <- rename $ Body bnds res
      return $ Body (Let pat' e1':bnds') res'
  rename (Body (DoLoop merge loopvar boundexp loopbody:bnds) res) = do
    let (mergepat, mergeexp) = unzip merge
    boundexp' <- rename boundexp
    mergeexp' <- mapM rename mergeexp
    bind mergepat $ do
      mergepat' <- mapM rename mergepat
      Body bnds' res' <- rename $ Body bnds res
      bind [loopvar] $ do
        loopvar'  <- rename loopvar
        loopbody' <- rename loopbody
        return $ Body (DoLoop (zip mergepat' mergeexp')
                                loopvar' boundexp' loopbody':bnds')
                      res'

instance Rename Exp where
  rename = mapExpM mapper
    where mapper = Mapper {
                      mapOnExp = rename
                    , mapOnBody = rename
                    , mapOnSubExp = rename
                    , mapOnIdent = rename
                    , mapOnLambda = rename
                    , mapOnType = rename
                    , mapOnValue = return
                    , mapOnCertificates = mapM rename
                    }

instance (Rename als, Rename shape) => Rename (TypeBase als shape) where
  rename (Array et size u als) = do
    als' <- rename als
    size' <- rename size
    return $ Array et size' u als'
  rename (Basic et) = return $ Basic et

instance Rename Lambda where
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

instance Rename () where
  rename = return
