{-# LANGUAGE FlexibleInstances #-}
-- | This module provides facilities for transforming L0 programs such
-- that names are unique, via the 'renameProg' function.
-- Additionally, the module also supports adding integral \"tags\" to
-- names (incarnated as the 'ID' type), in order to support more
-- efficient comparisons and renamings.  This is done by 'tagProg'.
-- The intent is that you call 'tagProg' once at some early stage,
-- then use 'renameProg' from then on.  Functions are also provided
-- for removing the tags again from expressions, patterns and typs.
module L0C.InternalRep.Renamer
  (
  -- * Renaming programs
   renameProg
  )
  where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import L0C.InternalRep
import L0C.FreshNames

-- | Rename variables such that each is unique.  The semantics of the
-- program are unaffected, under the assumption that the program was
-- correct to begin with.  In particular, the renaming may make an
-- invalid program valid.
renameProg :: Prog -> Prog
renameProg prog = Prog $ runReader (evalStateT f src) env
  where env = RenameEnv HM.empty newName
        src = newNameSourceForProg prog
        f = mapM renameFun $ progFunctions prog

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
  rename name = maybe (new name) return =<<
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

instance Rename Body where
  rename (LetWith cs dest src idxs ve body loc) = do
    cs' <- mapM rename cs
    src' <- rename src
    idxs' <- mapM rename idxs
    ve' <- rename ve
    bind [dest] $ do
      dest' <- rename dest
      body' <- rename body
      return $ LetWith cs' dest' src' idxs' ve' body' loc
  rename (LetPat pat e body loc) = do
    e1' <- rename e
    bind pat $ do
      pat' <- mapM rename pat
      body' <- rename body
      return $ LetPat pat' e1' body' loc
  rename (DoLoop merge loopvar boundexp loopbody letbody loc) = do
    let (mergepat, mergeexp) = unzip merge
    boundexp' <- rename boundexp
    mergeexp' <- mapM rename mergeexp
    bind mergepat $ do
      mergepat' <- mapM rename mergepat
      letbody' <- rename letbody
      bind [loopvar] $ do
        loopvar'  <- rename loopvar
        loopbody' <- rename loopbody
        return $ DoLoop (zip mergepat' mergeexp')
                        loopvar' boundexp' loopbody' letbody' loc
  rename (Result cs ses loc) =
    Result <$> mapM rename cs <*> mapM rename ses <*> pure loc

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
