module L0.Renamer
  ( renameProg )
  where

import Control.Monad.State
import Control.Monad.Reader

import Data.Data
import Data.Generics
import qualified Data.Map as M

import L0.AbSyn

-- | Rename variables such that each is unique.  The semantics of the
-- program are unaffected, under the assumption that the program was
-- correct to begin with.  In particular, the renaming may make an
-- invalid program valid.  To help enforce that this does not happen,
-- only type-checked programs can be renamed.
renameProg :: Prog Type -> Prog Type
renameProg prog = runReader (evalStateT (mapM renameFun prog) 0) M.empty

type RenameM = StateT Int (Reader (M.Map String String))

-- | Return a fresh, unique name.  The @String@ is prepended to the
-- name.
new :: String -> RenameM String
new s = do i <- get
           modify (+1)
           return $ s ++ "_" ++ show i

-- | 'repl s' returns the new name of the variable 's'.
repl :: String -> RenameM String
repl s = do sub <- asks $ M.lookup s
            case sub of Just s' -> return s'
                        Nothing -> new s

bind :: [String] -> RenameM a -> RenameM a
bind vars body = do
  vars' <- mapM new vars
  -- This works because Data.Map.union prefers elements from left
  -- operand.
  local (M.fromList (zip vars vars') `M.union`) body

renameFun :: FunDec Type -> RenameM (FunDec Type)
renameFun (fname, ret, params, body, pos) =
  bind (map fst params) $ do
    params' <- mapM renameBinding params
    body' <- renameExp body
    return (fname, ret, params', body', pos)

renameBinding :: Binding -> RenameM Binding
renameBinding (s, t) = do
  s' <- repl s
  return (s', t)

renameExp :: Exp Type -> RenameM (Exp Type)
renameExp (Var s t pos) = do
  s' <- repl s
  return $ Var s' t pos
renameExp (LetWith name e1 idxs ve body pos) = do
  e1' <- renameExp e1
  idxs' <- mapM renameExp idxs
  ve' <- renameExp ve
  bind [name] $ do
    name' <- repl name
    body' <- renameExp body
    return (LetWith name' e1' idxs' ve' body' pos)
renameExp (LetPat pat e body pos) = do
  e1' <- renameExp e
  bind (names pat) $ do
    pat' <- renamePattern pat
    body' <- renameExp body
    return $ LetPat pat' e1' body' pos
  where names (Id s _ _) = [s]
        names (TupId pats _) = concatMap names pats
renameExp (Index s idxs t1 t2 pos) = do
  s' <- repl s
  idxs' <- mapM renameExp idxs
  return $ Index s' idxs' t1 t2 pos
renameExp (DoLoop loopvar e body mergevars pos) = do
  e' <- renameExp e
  bind [loopvar] $ do
    loopvar' <- repl loopvar
    body' <- renameExp body
    mergevars' <- mapM repl mergevars
    return $ DoLoop loopvar' e' body' mergevars' pos
-- The above case may have to be extended if syntax nodes ever contain
-- anything but lambdas, expressions or lists of expressions.  Pay
-- particular attention to the fact that the latter has to be
-- specially handled.
renameExp e = gmapM (mkM renameExp `extM` renameLambda `extM` mapM renameExp) e

renameLambda :: Lambda Type -> RenameM (Lambda Type)
renameLambda (AnonymFun params body ret pos) =
  bind (map fst params) $ do
    params' <- mapM renameBinding params
    body' <- renameExp body
    return (AnonymFun params' body' ret pos)
renameLambda (CurryFun fname curryargexps rettype pos) = do
  curryargexps' <- mapM renameExp curryargexps
  return (CurryFun fname curryargexps' rettype pos)

renamePattern :: TupIdent Type -> RenameM (TupIdent Type)
renamePattern (Id s t pos) = do
  s' <- repl s
  return $ Id s' t pos
renamePattern (TupId pats pos) = do
  pats' <- mapM renamePattern pats
  return $ TupId pats' pos
