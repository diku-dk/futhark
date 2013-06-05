module L0C.Renamer
  ( renameProg
  , tagProg
  , tagProg'
  , untagProg
  , untagExp
  , untagPattern
  )
  where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as M

import L0C.L0
import L0C.FreshNames

-- | Rename variables such that each is unique.  The semantics of the
-- program are unaffected, under the assumption that the program was
-- correct to begin with.  In particular, the renaming may make an
-- invalid program valid.
renameProg :: VarName vn => ProgBase ty vn -> ProgBase ty vn
renameProg prog = Prog $ runReader (evalStateT f src) env
  where env = RenameEnv M.empty newName
        src = newNameSourceForProg prog
        f = mapM renameFun $ progFunctions prog

-- | Associate a unique integer with each name in the program, taking
-- binding into account, such that the resulting 'VName's are unique.
-- The semantics of the program are unaffected, under the assumption
-- that the program was correct to begin with.
tagProg :: VarName vn => ProgBase ty vn -> ProgBase ty (ID vn)
tagProg prog = Prog $ runReader (evalStateT f blankNameSource) env
  where env = RenameEnv M.empty newID
        f = mapM renameFun $ progFunctions prog

-- | As 'tagProg', but also return the final state of the name
-- generator.
tagProg' :: VarName vn => ProgBase ty vn -> (ProgBase ty (ID vn), NameSource (ID vn))
tagProg' prog = let (funs, src) = runReader (runStateT f blankNameSource) env
                in (Prog funs, src)
  where env = RenameEnv M.empty newID
        f = mapM renameFun $ progFunctions prog

-- | Remove tags from a program.  Note that this is potentially
-- semantics-changing if the underlying names are not each unique.
untagProg :: VarName vn => ProgBase ty (ID vn) -> ProgBase ty vn
untagProg prog = Prog $ runReader (evalStateT f blankNameSource) env
  where env = RenameEnv M.empty rmTag
        rmTag (ID (s, _)) src = (s, src)
        f = mapM renameFun $ progFunctions prog

-- | Remove tags from an expression.  The same caveats as with
-- 'untagProg' apply.
untagExp :: VarName vn => ExpBase ty (ID vn) -> ExpBase ty vn
untagExp e = runReader (evalStateT f blankNameSource) env
  where env = RenameEnv M.empty rmTag
        rmTag (ID (s, _)) src = (s, src)
        f = renameExp e

-- | Remove tags from a patter.  The same caveats as with
-- 'untagProg' apply.
untagPattern :: VarName vn => TupIdentBase ty (ID vn) -> TupIdentBase ty vn
untagPattern pat = runReader (evalStateT f blankNameSource) env
  where env = RenameEnv M.empty rmTag
        rmTag (ID (s, _)) src = (s, src)
        f = renamePattern pat

data RenameEnv f t = RenameEnv {
    envNameMap :: M.Map f t
  , envNameFn  :: f -> NameSource t -> (t, NameSource t)
  }

type RenameM f t = StateT (NameSource t) (Reader (RenameEnv f t))

-- | Return a fresh, unique name.  The @Name@ is prepended to the
-- name.
new :: f -> RenameM f t t
new k = do (k', src') <- asks envNameFn <*> pure k <*> get
           put src'
           return k'

-- | 'repl s' returns the new name of the variable 's'.
repl :: VarName f => IdentBase ty f -> RenameM f t (IdentBase ty t)
repl (Ident name tp loc) = do
  name' <- maybe (new name) return =<< lookupName name
  return $ Ident name' tp loc

lookupName :: VarName f => f -> RenameM f t (Maybe t)
lookupName name = asks $ M.lookup name . envNameMap

bind :: VarName f => [IdentBase ty f] -> RenameM f t a -> RenameM f t a
bind vars body = do
  vars' <- mapM new varnames
  -- This works because Data.Map.union prefers elements from left
  -- operand.
  local (bind' vars') body
  where varnames = map identName vars
        bind' vars' env = env { envNameMap = M.fromList (zip varnames vars')
                                             `M.union` envNameMap env }

renameFun :: VarName f => FunDecBase ty f -> RenameM f t (FunDecBase ty t)
renameFun (fname, ret, params, body, pos) =
  bind params $ do
    params' <- mapM repl params
    body' <- renameExp body
    return (fname, ret, params', body', pos)

renameExp :: VarName f => ExpBase ty f -> RenameM f t (ExpBase ty t)
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
renameExp (Index s idxs t2 pos) = do
  s' <- repl s
  idxs' <- mapM renameExp idxs
  return $ Index s' idxs' t2 pos
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

rename :: VarName f => MapperBase ty ty f t (RenameM f t)
rename = Mapper {
           mapOnExp = renameExp
         , mapOnPattern = renamePattern
         , mapOnIdent = repl
         , mapOnLambda = renameLambda
         , mapOnType = return
         , mapOnValue = return
         }

renameLambda :: VarName f => LambdaBase ty f -> RenameM f t (LambdaBase ty t)
renameLambda (AnonymFun params body ret pos) =
  bind params $ do
    params' <- mapM repl params
    body' <- renameExp body
    return (AnonymFun params' body' ret pos)
renameLambda (CurryFun fname curryargexps rettype pos) = do
  curryargexps' <- mapM renameExp curryargexps
  return (CurryFun fname curryargexps' rettype pos)

renamePattern :: VarName f => TupIdentBase ty f -> RenameM f t (TupIdentBase ty t)
renamePattern (Id ident) = do
  ident' <- repl ident
  return $ Id ident'
renamePattern (TupId pats pos) = do
  pats' <- mapM renamePattern pats
  return $ TupId pats' pos

patternNames :: TupIdentBase ty f -> [IdentBase ty f]
patternNames (Id ident) = [ident]
patternNames (TupId pats _) = concatMap patternNames pats
