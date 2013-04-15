module L0.Renamer
  ( renameProg )
  where

import Control.Monad.State
import Control.Monad.Reader

import Data.Data
import Data.Generics
import qualified Data.Map as M

import L0.AbSyn
import L0.FreshNames

-- | Rename variables such that each is unique.  The semantics of the
-- program are unaffected, under the assumption that the program was
-- correct to begin with.  In particular, the renaming may make an
-- invalid program valid.  To help enforce that this does not happen,
-- only type-checked programs can be renamed.
renameProg :: Prog Type -> Prog Type
renameProg prog = runReader (evalStateT (mapM renameFun prog) (newNameSourceForProg prog)) M.empty

type RenameM = StateT NameSource (Reader (M.Map String String))

-- | Return a fresh, unique name.  The @String@ is prepended to the
-- name.
new :: String -> RenameM String
new = state . newName

-- | 'repl s' returns the new name of the variable 's'.
repl :: Ident ty -> RenameM (Ident ty)
repl (Ident name tp loc) = do
  name' <- maybe (new name) return =<< (asks $ M.lookup name)
  return $ Ident name' tp loc

bind :: [Ident ty] -> RenameM a -> RenameM a
bind vars body = do
  vars' <- mapM new varnames
  -- This works because Data.Map.union prefers elements from left
  -- operand.
  local (M.fromList (zip varnames vars') `M.union`) body
  where varnames = map identName vars

renameFun :: FunDec Type -> RenameM (FunDec Type)
renameFun (fname, ret, params, body, pos) =
  bind params $ do
    params' <- mapM repl params
    body' <- renameExp body
    return (fname, ret, params', body', pos)

renameExp :: Exp Type -> RenameM (Exp Type)
renameExp (Var ident) = liftM Var $ repl ident
renameExp (LetWith dest src idxs ve body pos) = do
  src' <- repl src
  idxs' <- mapM renameExp idxs
  ve' <- renameExp ve
  bind [dest] $ do
    dest' <- repl dest
    body' <- renameExp body
    return (LetWith dest' src' idxs' ve' body' pos)
renameExp (LetPat pat e body pos) = do
  e1' <- renameExp e
  bind (names pat) $ do
    pat' <- renamePattern pat
    body' <- renameExp body
    return $ LetPat pat' e1' body' pos
  where names (Id ident) = [ident]
        names (TupId pats _) = concatMap names pats
renameExp (Index s idxs t1 t2 pos) = do
  s' <- repl s
  idxs' <- mapM renameExp idxs
  return $ Index s' idxs' t1 t2 pos
renameExp (DoLoop merges loopvar e loopbody letbody pos) = do
  e' <- renameExp e
  let (mergevars, mergeexps) = unzip merges
  mergeexps' <- mapM renameExp mergeexps
  bind mergevars $ do
    mergevars' <- mapM repl mergevars
    letbody' <- renameExp letbody
    bind [loopvar] $ do
      loopvar'  <- repl loopvar
      loopbody' <- renameExp loopbody
      return $ DoLoop (zip mergevars' mergeexps') loopvar' e' loopbody' letbody' pos
-- The above case may have to be extended if syntax nodes ever contain
-- anything but lambdas, expressions, lists of expressions or lists of
-- pairs of expression-types.  Pay particular attention to the fact
-- that the latter has to be specially handled.
renameExp e = gmapM (mkM renameExp
                     `extM` renameLambda
                     `extM` mapM renameExp
                     `extM` mapM renameExpPair) e

renameExpPair :: (Exp Type, Type) -> RenameM (Exp Type, Type)
renameExpPair (e,t) = do e' <- renameExp e
                         return (e',t)

renameLambda :: Lambda Type -> RenameM (Lambda Type)
renameLambda (AnonymFun params body ret pos) =
  bind params $ do
    params' <- mapM repl params
    body' <- renameExp body
    return (AnonymFun params' body' ret pos)
renameLambda (CurryFun fname curryargexps rettype pos) = do
  curryargexps' <- mapM renameExp curryargexps
  return (CurryFun fname curryargexps' rettype pos)

renamePattern :: TupIdent Type -> RenameM (TupIdent Type)
renamePattern (Id ident) = do
  ident' <- repl ident
  return $ Id ident'
renamePattern (TupId pats pos) = do
  pats' <- mapM renamePattern pats
  return $ TupId pats' pos
