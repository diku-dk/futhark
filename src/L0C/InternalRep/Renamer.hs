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

-- | 'repl s' returns the new name of the variable 's'.
repl :: Ident -> RenameM Ident
repl (Ident name tp loc) = do
  name' <- replName name
  tp' <- renameType tp
  return $ Ident name' tp' loc

replName :: VName -> RenameM VName
replName name = maybe (new name) return =<<
                asks (HM.lookup name . envNameMap)

bind :: [IdentBase als] -> RenameM a -> RenameM a
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
    params' <- mapM (liftM toParam . repl . fromParam) params
    body' <- renameBody body
    ret' <- mapM (liftM toDecl . renameType . fromDecl) ret
    return (fname, ret', params', body', loc)

renameSubExp :: SubExp -> RenameM SubExp
renameSubExp (Var v)          = Var <$> repl v
renameSubExp (Constant v loc) = return $ Constant v loc

renameBody :: Body -> RenameM Body
renameBody (LetWith cs dest src idxs ve body loc) = do
  cs' <- mapM repl cs
  src' <- repl src
  idxs' <- mapM renameSubExp idxs
  ve' <- renameSubExp ve
  bind [dest] $ do
    dest' <- repl dest
    body' <- renameBody body
    return $ LetWith cs' dest' src' idxs' ve' body' loc
renameBody (LetPat pat e body loc) = do
  e1' <- renameExp e
  bind pat $ do
    pat' <- mapM repl pat
    body' <- renameBody body
    return $ LetPat pat' e1' body' loc
renameBody (DoLoop merge loopvar boundexp loopbody letbody loc) = do
  let (mergepat, mergeexp) = unzip merge
  boundexp' <- renameSubExp boundexp
  mergeexp' <- mapM renameSubExp mergeexp
  bind mergepat $ do
    mergepat' <- mapM repl mergepat
    letbody' <- renameBody letbody
    bind [loopvar] $ do
      loopvar'  <- repl loopvar
      loopbody' <- renameBody loopbody
      return $ DoLoop (zip mergepat' mergeexp')
                      loopvar' boundexp' loopbody' letbody' loc
renameBody (Result cs ses loc) =
  Result <$> mapM repl cs <*> mapM renameSubExp ses <*> pure loc

renameExp :: Exp -> RenameM Exp
renameExp = mapExpM rename

renameType :: Type -> RenameM Type
renameType (Array et dims u als) = do
  als' <- HS.fromList <$> mapM replName (HS.toList als)
  return $ Array et (replicate (length dims) Nothing) u als'
renameType (Basic et) = return $ Basic et

rename :: Mapper RenameM
rename = Mapper {
           mapOnExp = renameExp
         , mapOnBody = renameBody
         , mapOnSubExp = renameSubExp
         , mapOnIdent = repl
         , mapOnLambda = renameLambda
         , mapOnType = renameType
         , mapOnValue = return
         , mapOnCertificates = mapM repl
         }

renameLambda :: Lambda -> RenameM Lambda
renameLambda (Lambda params body ret loc) =
  bind params $ do
    params' <- mapM (liftM toParam . repl . fromParam) params
    body' <- renameBody body
    ret' <- mapM (liftM toDecl . renameType . fromDecl) ret
    return $ Lambda params' body' ret' loc
