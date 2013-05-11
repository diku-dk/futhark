module L0.Renamer
  ( renameProg )
  where

import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as M

import L0.AbSyn
import L0.FreshNames
import L0.Traversals

-- | Rename variables such that each is unique.  The semantics of the
-- program are unaffected, under the assumption that the program was
-- correct to begin with.  In particular, the renaming may make an
-- invalid program valid.  To help enforce that this does not happen,
-- only type-checked programs can be renamed.
renameProg :: TypeBox ty => Prog ty -> Prog ty
renameProg prog = runReader (evalStateT (mapM renameFun prog) (newNameSourceForProg prog)) M.empty

type RenameM = StateT NameSource (Reader (M.Map Name Name))

-- | Return a fresh, unique name.  The @Name@ is prepended to the
-- name.
new :: Name -> RenameM Name
new = state . newName

-- | 'repl s' returns the new name of the variable 's'.
repl :: Ident ty -> RenameM (Ident ty)
repl (Ident name tp loc) = do
  name' <- maybe (new name) return =<< asks (M.lookup name)
  return $ Ident name' tp loc

bind :: [Ident ty] -> RenameM a -> RenameM a
bind vars body = do
  vars' <- mapM new varnames
  -- This works because Data.Map.union prefers elements from left
  -- operand.
  local (M.fromList (zip varnames vars') `M.union`) body
  where varnames = map identName vars

renameFun :: FunDec ty -> RenameM (FunDec ty)
renameFun (fname, ret, params, body, pos) =
  bind params $ do
    params' <- mapM repl params
    body' <- renameExp body
    return (fname, ret, params', body', pos)

renameExp :: Exp ty -> RenameM (Exp ty)
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
  bind (patternNames pat) $ do
    pat' <- renamePattern pat
    body' <- renameExp body
    return $ LetPat pat' e1' body' pos
renameExp (Index s idxs t1 t2 pos) = do
  s' <- repl s
  idxs' <- mapM renameExp idxs
  return $ Index s' idxs' t1 t2 pos
renameExp (DoLoop mergepat mergeexp loopvar e loopbody letbody pos) = do
  e' <- renameExp e
  mergeexp' <- renameExp mergeexp
  bind (patternNames mergepat) $ do
    mergepat' <- renamePattern mergepat
    letbody' <- renameExp letbody
    bind [loopvar] $ do
      loopvar'  <- repl loopvar
      loopbody' <- renameExp loopbody
      return $ DoLoop mergepat' mergeexp' loopvar' e' loopbody' letbody' pos
renameExp e = mapExpM rename e

rename :: Mapper ty RenameM
rename = identityMapper {
           mapOnExp = renameExp
         , mapOnPattern = renamePattern
         , mapOnIdent = repl
         , mapOnLambda = renameLambda
         }

renameLambda :: Lambda ty -> RenameM (Lambda ty)
renameLambda (AnonymFun params body ret pos) =
  bind params $ do
    params' <- mapM repl params
    body' <- renameExp body
    return (AnonymFun params' body' ret pos)
renameLambda (CurryFun fname curryargexps rettype pos) = do
  curryargexps' <- mapM renameExp curryargexps
  return (CurryFun fname curryargexps' rettype pos)

renamePattern :: TupIdent ty -> RenameM (TupIdent ty)
renamePattern (Id ident) = do
  ident' <- repl ident
  return $ Id ident'
renamePattern (TupId pats pos) = do
  pats' <- mapM renamePattern pats
  return $ TupId pats' pos

patternNames :: TupIdent ty -> [Ident ty]
patternNames (Id ident) = [ident]
patternNames (TupId pats _) = concatMap patternNames pats
