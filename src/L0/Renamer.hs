module L0.Renamer
  ( renameProg )
  where

import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as M
import Data.Loc

import L0.AbSyn

-- | Rename variables such that each is unique.  The semantics of the
-- program are unaffected, under the assumption that the program was
-- correct to begin with.  In particular, the renaming may make an
-- invalid program valid.
renameProg :: Prog tf -> Prog tf
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

renameFun :: FunDec tf -> RenameM (FunDec tf)
renameFun (fname, ret, params, body, pos) =
  bind (map fst params) $ do
    params' <- mapM renameBinding params
    body' <- renameExp body
    return (fname, ret, params', body', pos)

renameBinding :: Binding -> RenameM Binding
renameBinding (s, t) = do
  s' <- repl s
  return (s', t)

renameExp :: Exp tf -> RenameM (Exp tf)
renameExp (Literal val) = return $ Literal val
renameExp (TupLit es ts pos) = do
  es' <- mapM renameExp es
  return $ TupLit es' ts pos
renameExp (ArrayLit es ts pos) = do
  es' <- mapM renameExp es
  return $ ArrayLit es' ts pos
renameExp (BinOp op e1 e2 t pos) = do
  e1' <- renameExp e1
  e2' <- renameExp e2
  return $ BinOp op e1' e2' t pos
renameExp (And e1 e2 pos) = renameBinOpNoType And e1 e2 pos
renameExp (Or e1 e2 pos) = renameBinOpNoType Or e1 e2 pos
renameExp (Not e pos) = do
  e' <- renameExp e
  return $ Not e' pos
renameExp (Negate e t pos) = do
  e' <- renameExp e
  return $ Negate e' t pos
renameExp (If e1 e2 e3 t pos) = do
  e1' <- renameExp e1
  e2' <- renameExp e2
  e3' <- renameExp e3
  return $ If e1' e2' e3' t pos
renameExp (Var s t pos) = do
  s' <- repl s
  return $ Var s' t pos
renameExp (Apply fname args t pos) = do
  args' <- mapM renameExp args
  return $ Apply fname args' t pos
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
renameExp (Iota e pos) = do
  e' <- renameExp e
  return $ Iota e' pos
renameExp (Size e pos) = do
  e' <- renameExp e
  return $ Size e' pos
renameExp (Replicate e1 e2 t pos) = do
  e1' <- renameExp e1
  e2' <- renameExp e2
  return $ Replicate e1' e2' t pos
renameExp (Reshape es e t1 t2 pos) = do
  es' <- mapM renameExp es
  e' <- renameExp e
  return $ Reshape es' e' t1 t2 pos
renameExp (Transpose e t1 t2 pos) = do
  e' <- renameExp e
  return $ Transpose e' t1 t2 pos
renameExp (Map fun e t1 t2 pos) = do
  fun' <- renameLambda fun
  e' <- renameExp e
  return $ Map fun' e' t1 t2 pos
renameExp (Reduce fun e1 e2 t pos) = do
  fun' <- renameLambda fun
  e1' <- renameExp e1
  e2' <- renameExp e2
  return $ Reduce fun' e1' e2' t pos
renameExp (ZipWith fun es t1 t2 pos) = do
  fun' <- renameLambda fun
  es' <- mapM renameExp es
  return $ ZipWith fun' es' t1 t2 pos
renameExp (Zip es t pos) = do
  es' <- mapM renameExp es
  return $ Zip es' t pos
renameExp (Unzip e ts pos) = do
  e' <- renameExp e
  return $ Unzip e' ts pos
renameExp (Scan fun e1 e2 t pos) = do
  fun' <- renameLambda fun
  e1' <- renameExp e1
  e2' <- renameExp e2
  return $ Scan fun' e1' e2' t pos
renameExp (Filter fun e t pos) = do
  fun' <- renameLambda fun
  e' <- renameExp e
  return $ Filter fun' e' t pos
renameExp (Mapall fun e t1 t2 pos) = do
  fun' <- renameLambda fun
  e' <- renameExp e
  return $ Mapall fun' e' t1 t2 pos
renameExp (Redomap fun1 fun2 e1 e2 t1 t2 pos) = do
  fun1' <- renameLambda fun1
  fun2' <- renameLambda fun2
  e1' <- renameExp e1
  e2' <- renameExp e2
  return $ Redomap fun1' fun2' e1' e2' t1 t2 pos
renameExp (Split e1 e2 t pos) = do
  e1' <- renameExp e1
  e2' <- renameExp e2
  return $ Split e1' e2' t pos
renameExp (Concat e1 e2 t pos) = do
  e1' <- renameExp e1
  e2' <- renameExp e2
  return $ Concat e1' e2' t pos
renameExp (Read t pos) =
  return $ Read t pos
renameExp (Write e t pos) = do
  e' <- renameExp e
  return $ Write e' t pos
renameExp (DoLoop loopvar e body mergevars pos) = do
  e' <- renameExp e
  bind [loopvar] $ do
    loopvar' <- repl loopvar
    body' <- renameExp body
    mergevars' <- mapM repl mergevars
    return $ DoLoop loopvar' e' body' mergevars' pos

renameBinOpNoType :: (Exp tf -> Exp tf -> Loc -> Exp tf)
                  -> Exp tf -> Exp tf -> Loc
                  -> RenameM (Exp tf)
renameBinOpNoType op e1 e2 pos = do
  e1' <- renameExp e1
  e2' <- renameExp e2
  return (op e1' e2' pos)

renameLambda :: Lambda tf -> RenameM (Lambda tf)
renameLambda (AnonymFun params body ret pos) =
  bind (map fst params) $ do
    params' <- mapM renameBinding params
    body' <- renameExp body
    return (AnonymFun params' body' ret pos)
renameLambda (CurryFun fname curryargexps curryargts rettype pos) = do
  curryargexps' <- mapM renameExp curryargexps
  return (CurryFun fname curryargexps' curryargts rettype pos)

renamePattern :: TupIdent tf -> RenameM (TupIdent tf)
renamePattern (Id s t pos) = do
  s' <- repl s
  return $ Id s' t pos
renamePattern (TupId pats pos) = do
  pats' <- mapM renamePattern pats
  return $ TupId pats' pos
