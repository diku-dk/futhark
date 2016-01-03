{-# LANGUAGE FlexibleContexts #-}
-- | This module provides facilities for transforming Futhark programs such
-- that names are unique, via the 'renameProg' function.
-- Additionally, the module also supports adding integral \"tags\" to
-- names (incarnated as the 'ID' type), in order to support more
-- efficient comparisons and renamings.  This is done by 'tagProg'.
-- The intent is that you call 'tagProg' once at some early stage,
-- then use 'renameProg' from then on.  Functions are also provided
-- for removing the tags again from expressions, patterns and typs.
module Futhark.Representation.External.Renamer
  (
  -- * Renaming programs
   renameProg

  -- * Tagging
  , tagProg
  , tagProg'
  , tagExp
  , tagExp'
  , tagType
  , tagType'
  , tagLambda
  , tagLambda'

  -- * Untagging
  , untagProg
  , untagExp
  , untagLambda
  , untagPattern
  , untagType
  )
  where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Prelude

import Futhark.Representation.External
import Futhark.FreshNames

-- | Rename variables such that each is unique.  The semantics of the
-- program are unaffected, under the assumption that the program was
-- correct to begin with.  In particular, the renaming may make an
-- invalid program valid.
renameProg :: (TypeBox ty, VarName vn) =>
              ProgBase ty vn -> ProgBase ty vn
renameProg prog = Prog $ runReader (evalStateT f src) env
  where env = RenameEnv HM.empty newName
        src = newNameSourceForProg prog
        f = mapM renameFun $ progFunctions prog

-- | Associate a unique integer with each name in the program, taking
-- binding into account, such that the resulting 'VName's are unique.
-- The semantics of the program are unaffected, under the assumption
-- that the program was correct to begin with.
tagProg :: (TypeBox ty, VarName vn) =>
           ProgBase ty vn -> ProgBase ty (ID vn)
tagProg prog = Prog $ runReader (evalStateT f blankNameSource) env
  where env = RenameEnv HM.empty newID
        f = mapM renameFun $ progFunctions prog

-- | As 'tagProg', but accepts an initial name source and returns the
-- resulting one.
tagProg' :: (TypeBox ty, VarName vn) =>
            NameSource (ID vn) -> ProgBase ty vn -> (ProgBase ty (ID vn), NameSource (ID vn))
tagProg' src prog = let (funs, src') = runReader (runStateT f src) env
                    in (Prog funs, src')
  where env = RenameEnv HM.empty newID
        f = mapM renameFun $ progFunctions prog

-- | As 'tagExp', but accepts an initial name source and returns the
-- new one.
tagExp' :: (TypeBox ty, VarName vn) =>
           NameSource (ID vn) -> ExpBase ty vn -> (ExpBase ty (ID vn), NameSource (ID vn))
tagExp' src e = runReader (runStateT (renameExp e) src) env
  where env = RenameEnv HM.empty newID

-- | As 'tagProg', but for expressions.
tagExp :: (TypeBox ty, VarName vn) =>
           ExpBase ty vn -> ExpBase ty (ID vn)
tagExp = fst . tagExp' blankNameSource

-- | As 'tagType', but accepts an initial name source and returns the
-- new one.
tagType' :: (TypeBox ty, VarName vn) =>
            NameSource (ID vn) -> ty vn -> (ty (ID vn), NameSource (ID vn))
tagType' src t = runReader (runStateT (renameType t) src) env
  where env = RenameEnv HM.empty newID

-- | As 'tagProg', but for types.
tagType :: (TypeBox ty, VarName vn) =>
           ty vn -> ty (ID vn)
tagType = fst . tagType' blankNameSource

-- | As 'tagLambda', but accepts an initial name source and returns
-- the new one.
tagLambda' :: (TypeBox ty, VarName vn) =>
            NameSource (ID vn) -> LambdaBase ty vn
         -> (LambdaBase ty (ID vn), NameSource (ID vn))
tagLambda' src t = runReader (runStateT (renameLambda t) src) env
  where env = RenameEnv HM.empty newID

-- | As 'tagProg', but for anonymous functions.
tagLambda :: (TypeBox ty, VarName vn) =>
             LambdaBase ty vn -> LambdaBase ty (ID vn)
tagLambda = fst . tagLambda' blankNameSource

-- | Remove tags from a program.  Note that this is potentially
-- semantics-changing if the underlying names are not each unique.
untagProg :: (TypeBox ty, VarName vn) =>
             ProgBase ty (ID vn) -> ProgBase ty vn
untagProg = untagger $ liftM Prog . mapM renameFun . progFunctions

-- | Remove tags from an expression.  The same caveats as with
-- 'untagProg' apply.
untagExp :: (TypeBox ty, VarName vn) =>
            ExpBase ty (ID vn) -> ExpBase ty vn
untagExp = untagger renameExp

-- | Remove tags from an anonymous function.  The same caveats as with
-- 'untagProg' apply.
untagLambda :: (TypeBox ty, VarName vn) =>
               LambdaBase ty (ID vn) -> LambdaBase ty vn
untagLambda = untagger renameLambda

-- | Remove tags from a pattern.  The same caveats as with 'untagProg'
-- apply.
untagPattern :: (TypeBox ty, VarName vn) =>
                TupIdentBase ty (ID vn) -> TupIdentBase ty vn
untagPattern = untagger renamePattern

-- | Remove tags from a type.  The same caveats as with 'untagProg'
-- apply.
untagType :: (TypeBox (TypeBase shape als), VarName vn) =>
             TypeBase shape als (ID vn) -> TypeBase shape als vn
untagType = untagger renameType

untagger :: VarName vn =>
            (t -> RenameM (ID vn) vn a) -> t -> a
untagger f x = runReader (evalStateT (f x) blankNameSource) env
  where env = RenameEnv HM.empty rmTag
        rmTag src (ID (s, _)) = (s, src)

data RenameEnv f t = RenameEnv {
    envNameMap :: HM.HashMap f t
  , envNameFn  :: NameSource t -> f -> (t, NameSource t)
  }

type RenameM f t = StateT (NameSource t) (Reader (RenameEnv f t))

-- | Return a fresh, unique name.  The @Name@ is prepended to the
-- name.
new :: f -> RenameM f t t
new k = do (k', src') <- asks envNameFn <*> get <*> pure k
           put src'
           return k'

-- | 'repl s' returns the new name of the variable 's'.
repl :: (TypeBox ty, VarName f, VarName t) =>
        IdentBase ty f -> RenameM f t (IdentBase ty t)
repl (Ident name tp loc) = do
  name' <- replName name
  tp' <- renameType tp
  return $ Ident name' tp' loc

declRepl :: (VarName f, VarName t) =>
            IdentBase (TypeBase ShapeDecl NoInfo) f
         -> RenameM f t (IdentBase (TypeBase ShapeDecl NoInfo) t)
declRepl (Ident name tp loc) = do
  name' <- replName name
  tp' <- renameDeclType tp
  return $ Ident name' tp' loc

replName :: (VarName f, VarName t) => f -> RenameM f t t
replName name = maybe (new name) return =<<
                asks (HM.lookup name . envNameMap)

bindNames :: VarName f => [f] -> RenameM f t a -> RenameM f t a
bindNames varnames body = do
  vars' <- mapM new varnames
  -- This works because map union prefers elements from left
  -- operand.
  local (bind' vars') body
  where bind' vars' env = env { envNameMap = HM.fromList (zip varnames vars')
                                             `HM.union` envNameMap env }

bind :: (VarName f) => [IdentBase ty f] -> RenameM f t a -> RenameM f t a
bind = bindNames . map identName

bindParams :: VarName f =>
              [ParamBase f]
           -> RenameM f t a
           -> RenameM f t a
bindParams params =
  bind params .
  bindNames (concatMap (mapMaybe inspectDim . arrayDims . identType) params)
  where inspectDim AnyDim =
          Nothing
        inspectDim (ConstDim _) =
          Nothing
        inspectDim (NamedDim name) =
          Just name

renameFun :: (TypeBox ty, VarName f, VarName t) =>
             FunDecBase ty f -> RenameM f t (FunDecBase ty t)
renameFun (fname, ret, params, body, pos) =
  bindParams params $ do
    params' <- mapM declRepl params
    body' <- renameExp body
    ret' <- renameDeclType ret
    return (fname, ret', params', body', pos)

renameExp :: (TypeBox ty, VarName f, VarName t) =>
             ExpBase ty f -> RenameM f t (ExpBase ty t)
renameExp (LetWith dest src idxs ve body loc) = do
  src' <- repl src
  idxs' <- mapM renameExp idxs
  ve' <- renameExp ve
  bind [dest] $ do
    dest' <- repl dest
    body' <- renameExp body
    return $ LetWith dest' src' idxs' ve' body' loc
renameExp (LetPat pat e body pos) = do
  e1' <- renameExp e
  bind (patternNames pat) $ do
    pat' <- renamePattern pat
    body' <- renameExp body
    return $ LetPat pat' e1' body' pos
renameExp (DoLoop mergepat mergeexp form loopbody letbody pos) = do
  mergeexp' <- renameExp mergeexp
  case form of
    For dir lbound loopvar ubound -> do
      lbound' <- renameExp lbound
      ubound' <- renameExp ubound
      bind (patternNames mergepat) $ do
        mergepat' <- renamePattern mergepat
        letbody' <- renameExp letbody
        bind [loopvar] $ do
          loopvar'  <- repl loopvar
          loopbody' <- renameExp loopbody
          return $ DoLoop mergepat' mergeexp'
            (For dir lbound' loopvar' ubound') loopbody' letbody' pos
    While cond ->
      bind (patternNames mergepat) $ do
        mergepat' <- renamePattern mergepat
        letbody' <- renameExp letbody
        cond' <- renameExp cond
        loopbody' <- renameExp loopbody
        return $ DoLoop mergepat' mergeexp'
          (While cond') loopbody' letbody' pos
renameExp (Stream form lam arr ii pos) = do
  form' <- case form of
               MapLike o -> return $ MapLike o
               RedLike o comm lam0 acc -> do
                 lam0' <- renameLambda lam0
                 acc'  <- renameExp    acc
                 return $ RedLike o comm lam0' acc'
               Sequential acc ->
                 return Sequential <*> renameExp acc
  lam' <- renameLambda lam
  arr' <- renameExp    arr
  return $ Stream form' lam' arr' ii pos
renameExp e = mapExpM rename e

renameType :: (TypeBox ty, VarName f, VarName t) => ty f -> RenameM f t (ty t)
renameType = mapType $ renameTypeGeneric
             (\(Rank n) -> return $ Rank n)
             (liftM HS.fromList . mapM replName . HS.toList)

renameDeclType :: (VarName f, VarName t) =>
                  TypeBase ShapeDecl NoInfo f
               -> RenameM f t (TypeBase ShapeDecl NoInfo t)
renameDeclType = renameTypeGeneric
                 (liftM ShapeDecl . mapM renameDim . shapeDims)
                 (const $ return NoInfo)
  where renameDim AnyDim       = return AnyDim
        renameDim (NamedDim v) = NamedDim <$> replName v
        renameDim (ConstDim n) = return $ ConstDim n

renameTypeGeneric :: (VarName f, VarName t) =>
                     (shape f -> RenameM f t (shape t))
                  -> (als f -> RenameM f t (als t))
                  -> TypeBase shape als f
                  -> RenameM f t (TypeBase shape als t)
renameTypeGeneric renameShape renameAliases = renameType'
  where renameType' (Array at) = Array <$> renameArrayType at
        renameType' (Basic bt) = return $ Basic bt
        renameType' (Tuple ts) = Tuple <$> mapM renameType' ts
        renameArrayType (BasicArray bt shape u als) = do
          shape' <- renameShape shape
          als' <- renameAliases als
          return $ BasicArray bt shape' u als'
        renameArrayType (TupleArray et shape u) = do
          et' <- mapM renameTupleArrayElem et
          shape' <- renameShape shape
          return $ TupleArray et' shape' u
        renameTupleArrayElem (BasicArrayElem bt als) =
          BasicArrayElem bt <$> renameAliases als
        renameTupleArrayElem (ArrayArrayElem at) =
          ArrayArrayElem <$> renameArrayType at
        renameTupleArrayElem (TupleArrayElem ts) =
          TupleArrayElem <$> mapM renameTupleArrayElem ts

rename :: (TypeBox ty, VarName f, VarName t) => MapperBase ty ty f t (RenameM f t)
rename = Mapper {
           mapOnExp = renameExp
         , mapOnPattern = renamePattern
         , mapOnIdent = repl
         , mapOnLambda = renameLambda
         , mapOnType = renameType
         , mapOnValue = return
         }

renameLambda :: (TypeBox ty, VarName f, VarName t) =>
                LambdaBase ty f -> RenameM f t (LambdaBase ty t)
renameLambda (AnonymFun params body ret pos) =
  bind params $ do
    params' <- mapM declRepl params
    body' <- renameExp body
    ret' <- renameDeclType ret
    return (AnonymFun params' body' ret' pos)
renameLambda (CurryFun fname curryargexps rettype pos) = do
  curryargexps' <- mapM renameExp curryargexps
  rettype' <- renameType rettype
  return (CurryFun fname curryargexps' rettype' pos)
renameLambda (UnOpFun bop t loc) =
  UnOpFun bop <$> renameType t <*> pure loc
renameLambda (BinOpFun bop t loc) =
  BinOpFun bop <$> renameType t <*> pure loc
renameLambda (CurryBinOpLeft bop x t loc) =
  CurryBinOpLeft bop <$> renameExp x <*> renameType t <*> pure loc
renameLambda (CurryBinOpRight bop x t loc) =
  CurryBinOpRight bop <$> renameExp x <*> renameType t <*> pure loc

renamePattern :: (TypeBox ty, VarName f, VarName t) =>
                 TupIdentBase ty f -> RenameM f t (TupIdentBase ty t)
renamePattern (Id ident) = do
  ident' <- repl ident
  return $ Id ident'
renamePattern (TupId pats pos) = do
  pats' <- mapM renamePattern pats
  return $ TupId pats' pos
renamePattern (Wildcard t loc) = do
  t' <- renameType t
  return $ Wildcard t' loc

patternNames :: TupIdentBase ty f -> [IdentBase ty f]
patternNames (Id ident)     = [ident]
patternNames (TupId pats _) = concatMap patternNames pats
patternNames (Wildcard _ _)   = []
