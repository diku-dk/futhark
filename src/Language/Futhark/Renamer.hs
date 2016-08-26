{-# LANGUAGE FlexibleContexts #-}
-- | This module exports a facility for assigning every name in a
-- Futhark program a unique integer, thus getting rid of name
-- shadowing.
module Language.Futhark.Renamer
  (
  -- * Tagging
    tagProg

  -- * Untagging
  , untagProg
  , untagExp
  , untagPattern
  )
  where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Data.Hashable
import Data.Maybe

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Prelude

import Language.Futhark
import Futhark.FreshNames


-- | Associate a unique integer with each name in the program, taking
-- binding into account, such that the resulting 'VName's are unique.
-- The semantics of the program are unaffected, under the assumption
-- that the program was correct to begin with.
tagProg :: VNameSource -> ProgBase NoInfo Name  -> (ProgBase NoInfo VName, VNameSource)
tagProg src prog = let (decs , src') = runReader (runStateT f src) env
                    in (Prog decs, src')

  where env = RenameEnv HM.empty newVNameFromName
        f = mapM renameDec $ progDecs prog

untagProg :: ProgBase NoInfo VName -> ProgBase NoInfo Name
untagProg prog =
  let decs = untagger (mapM renameDec . progDecs) prog
  in Prog decs

-- | Remove tags from an expression.  The same caveats as with
-- 'untagProg' apply.
untagExp :: ExpBase NoInfo VName -> ExpBase NoInfo Name
untagExp = untagger renameExp

-- | Remove tags from a pattern.  The same caveats as with 'untagProg'
-- apply.
untagPattern :: PatternBase NoInfo VName -> PatternBase NoInfo Name
untagPattern = untagger renamePattern

untagger :: (t -> RenameM VName Name a) -> t -> a
untagger f x = runReader (evalStateT (f x) blankNameSource) env
  where env = RenameEnv HM.empty rmTag
        rmTag src (ID (s, _)) = (s, src)

data RenameEnv f t = RenameEnv {
    envNameMap :: HM.HashMap f t
  , envNameFn  :: VNameSource -> f -> (t, VNameSource)
  }

type RenameM f t = StateT VNameSource (Reader (RenameEnv f t))

-- | Return a fresh, unique name.  The @Name@ is prepended to the
-- name.
new :: f -> RenameM f t t
new k = do (k', src') <- asks envNameFn <*> get <*> pure k
           put src'
           return k'

-- | 'repl s' returns the new name of the variable 's'.
repl :: (Eq f, Hashable f) =>
        IdentBase NoInfo f -> RenameM f t (IdentBase NoInfo t)
repl (Ident name NoInfo loc) = do
  name' <- replName name
  return $ Ident name' NoInfo loc

declRepl :: (Eq f, Hashable f) =>
            ParamBase NoInfo f
         -> RenameM f t (ParamBase NoInfo t)
declRepl (Param name (Just (TypeDecl tp NoInfo)) NoInfo loc) = do
  name' <- replName name
  tp' <- renameUserType tp
  return $ Param name' (Just (TypeDecl tp' NoInfo)) NoInfo loc
declRepl (Param name Nothing NoInfo loc) = do
  name' <- replName name
  return $ Param name' Nothing NoInfo loc

replName :: (Eq f, Hashable f) => f -> RenameM f t t
replName name = maybe (new name) return =<<
                asks (HM.lookup name . envNameMap)

bindNames :: (Eq f, Hashable f) => [f] -> RenameM f t a -> RenameM f t a
bindNames varnames body = do
  vars' <- mapM new varnames
  -- This works because map union prefers elements from left
  -- operand.
  local (bind' vars') body
  where bind' vars' env = env { envNameMap = HM.fromList (zip varnames vars')
                                             `HM.union` envNameMap env }

bind :: (Eq f, Hashable f) => [IdentBase x f] -> RenameM f t a -> RenameM f t a
bind = bindNames . map identName

bindParams :: (Ord f, Hashable f) =>
              [ParamBase NoInfo f]
           -> RenameM f t a
           -> RenameM f t a
bindParams params =
  bindNames (map paramName params) .
  bindNames (concatMap (mapMaybe inspectDim . maybe [] nestedDims' . paramDeclaredType) params)
  where inspectDim AnyDim =
          Nothing
        inspectDim (ConstDim _) =
          Nothing
        inspectDim (NamedDim name) =
          Just name

renameDec :: (Eq f, Ord f, Hashable f, Eq t, Hashable t) =>
             DecBase NoInfo f -> RenameM f t (DecBase NoInfo t)
renameDec (SigDec sig) = do
  sig' <- renameSignature sig
  return $ SigDec sig'


renameDec (ModDec modd) = do
  modd' <- renameModule modd
  return $ ModDec modd'

renameDec (FunOrTypeDec funortype) =
  case funortype of
    FunDec fun -> do
      fun' <- renameFun fun
      return $ FunOrTypeDec $ FunDec fun'
    TypeDec tp -> do
      tp' <- renameTypeAlias tp
      return $ FunOrTypeDec $ TypeDec tp'

renameFun :: (Ord f, Hashable f, Eq t, Hashable t) =>
             FunDefBase NoInfo f -> RenameM f t (FunDefBase NoInfo t)
renameFun (FunDef entry fname (TypeDecl ret NoInfo) params body pos) =
  bindParams params $
    FunDef entry fname <$>
    (TypeDecl <$> renameUserType ret <*> pure NoInfo) <*>
    mapM declRepl params <*>
    renameExp body <*>
    pure pos

renameTypeAlias :: (Eq f, Hashable f, Eq t, Hashable t) =>
                   TypeDefBase NoInfo f -> RenameM f t (TypeDefBase NoInfo t)
renameTypeAlias (TypeDef name typedecl loc) = do
  typedecl' <- renameUserTypeDecl typedecl
  return $ TypeDef name typedecl' loc

renameUserTypeDecl :: (Eq f, Hashable f, Eq t, Hashable t) =>
                      TypeDeclBase NoInfo f
                   -> RenameM f t (TypeDeclBase NoInfo t)
renameUserTypeDecl (TypeDecl usertype NoInfo) = do
  usertype' <- renameUserType usertype
  return $ TypeDecl usertype' NoInfo

renameSignature :: (Eq f, Hashable f, Eq t, Hashable t) =>
                   SigDefBase NoInfo f -> RenameM f t (SigDefBase NoInfo t)
renameSignature (SigDef name sigdecs loc) = do
  sigdecs' <- mapM renameSigDec sigdecs
  return $ SigDef name sigdecs' loc

renameModule :: (Eq f, Hashable f, Ord f, Eq t, Hashable t) =>
                ModDefBase NoInfo f -> RenameM f t (ModDefBase NoInfo t)
renameModule (ModDef name moddecs loc) = do
  moddecs' <- mapM renameDec moddecs
  return $ ModDef name moddecs' loc

renameExp :: (Eq f, Hashable f, Eq t, Hashable t) =>
             ExpBase NoInfo f -> RenameM f t (ExpBase NoInfo t)
renameExp (LetWith dest src idxs ve body loc) = do
  src' <- repl src
  idxs' <- mapM renameDimIndex idxs
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
renameExp (Stream form lam arr pos) = do
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
  return $ Stream form' lam' arr' pos
renameExp e = mapExpM rename e

renameSigDec :: (Eq f, Hashable f, Eq t, Hashable t) =>
                 SigDeclBase NoInfo f
              -> RenameM f t (SigDeclBase NoInfo t)
renameSigDec (FunSig name params rettype) = do
  params' <- mapM renameUserTypeDecl params
  rettype' <- renameUserTypeDecl rettype
  return $ FunSig name params' rettype'

renameSigDec (TypeSig usertype) = do
  usertype' <- renameTypeAlias usertype
  return $ TypeSig usertype'

renameUserType :: (Eq f, Hashable f) =>
                   UserType f
                -> RenameM f t (UserType t)
renameUserType (UserPrim bt loc) = return $ UserPrim bt loc
renameUserType (UserUnique bt loc) = UserUnique <$> renameUserType bt <*> pure loc
renameUserType (UserTuple ts loc) = UserTuple <$> mapM renameUserType ts <*> pure loc
renameUserType (UserTypeAlias name loc) = return $ UserTypeAlias name loc
renameUserType (UserArray at d loc) =
  UserArray <$> renameUserType at <*> renameDim d <*> pure loc
  where renameDim AnyDim       = return AnyDim
        renameDim (NamedDim v) = NamedDim <$> replName v
        renameDim (ConstDim n) = return $ ConstDim n

renameCompType :: (Eq f, Hashable f, Eq t, Hashable t) =>
                  CompTypeBase f
               -> RenameM f t (CompTypeBase t)
renameCompType = renameTypeGeneric
                 (pure . Rank . shapeRank)
                 (fmap HS.fromList . mapM replName . HS.toList)

renameTypeGeneric :: (shape f -> RenameM f t (shape t))
                  -> (als f -> RenameM f t (als t))
                  -> TypeBase shape als f
                  -> RenameM f t (TypeBase shape als t)
renameTypeGeneric renameShape renameAliases = renameType'
  where renameType' (Array at) = Array <$> renameArrayType at
        renameType' (Prim bt) = return $ Prim bt
        renameType' (Tuple ts) = Tuple <$> mapM renameType' ts
        renameArrayType (PrimArray bt shape u als) = do
          shape' <- renameShape shape
          als' <- renameAliases als
          return $ PrimArray bt shape' u als'
        renameArrayType (TupleArray et shape u) = do
          et' <- mapM renameTupleArrayElem et
          shape' <- renameShape shape
          return $ TupleArray et' shape' u
        renameTupleArrayElem (PrimArrayElem bt als u) =
          PrimArrayElem bt <$> renameAliases als <*> pure u
        renameTupleArrayElem (ArrayArrayElem at) =
          ArrayArrayElem <$> renameArrayType at
        renameTupleArrayElem (TupleArrayElem ts) =
          TupleArrayElem <$> mapM renameTupleArrayElem ts



rename :: (Eq f, Hashable f, Eq t, Hashable t) =>
          MapperBase f t (RenameM f t)
rename = Mapper {
           mapOnExp = renameExp
         , mapOnPattern = renamePattern
         , mapOnIdent = repl
         , mapOnName = replName
         , mapOnLambda = renameLambda
         , mapOnType = renameCompType
         , mapOnValue = return
         }

renameLambda :: (Eq f, Hashable f, Eq t, Hashable t) =>
                LambdaBase NoInfo f -> RenameM f t (LambdaBase NoInfo t)
renameLambda (AnonymFun params body (TypeDecl ret NoInfo) pos) =
  bindNames (map paramName params) $ do
    params' <- mapM declRepl params
    body' <- renameExp body
    ret' <- renameUserType ret
    return (AnonymFun params' body' (TypeDecl ret' NoInfo) pos)
renameLambda (CurryFun fname curryargexps NoInfo pos) = do
  curryargexps' <- mapM renameExp curryargexps
  return (CurryFun fname curryargexps' NoInfo pos)
renameLambda (UnOpFun bop NoInfo NoInfo loc) =
  pure $ UnOpFun bop NoInfo NoInfo loc
renameLambda (BinOpFun bop NoInfo NoInfo NoInfo loc) =
  pure $ BinOpFun bop NoInfo NoInfo NoInfo loc
renameLambda (CurryBinOpLeft bop x NoInfo NoInfo loc) =
  CurryBinOpLeft bop <$> renameExp x <*>
  pure NoInfo <*> pure NoInfo <*> pure loc
renameLambda (CurryBinOpRight bop x NoInfo NoInfo loc) =
  CurryBinOpRight bop <$> renameExp x <*>
  pure NoInfo <*> pure NoInfo <*> pure loc

renamePattern :: (Eq f, Hashable f) =>
                 PatternBase NoInfo f -> RenameM f t (PatternBase NoInfo t)
renamePattern (Id ident) = do
  ident' <- repl ident
  return $ Id ident'
renamePattern (TuplePattern pats pos) = do
  pats' <- mapM renamePattern pats
  return $ TuplePattern pats' pos
renamePattern (Wildcard NoInfo loc) =
  pure $ Wildcard NoInfo loc

renameDimIndex :: (Eq f, Hashable f, Eq t, Hashable t) =>
                  DimIndexBase NoInfo f
               -> RenameM f t (DimIndexBase NoInfo t)
renameDimIndex (DimFix i) = DimFix <$> renameExp i
renameDimIndex (DimSlice i j) = DimSlice <$> renameExp i <*> renameExp j

patternNames :: PatternBase ty f -> [IdentBase ty f]
patternNames (Id ident) = [ident]
patternNames (TuplePattern pats _) = concatMap patternNames pats
patternNames (Wildcard _ _) = []
