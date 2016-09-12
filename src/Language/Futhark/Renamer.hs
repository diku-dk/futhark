{-# LANGUAGE FlexibleContexts #-}
-- | This module exports a facility for assigning every name in a
-- Futhark program a unique integer, thus getting rid of name
-- shadowing.
module Language.Futhark.Renamer
  (
  -- * Tagging
    tagProg

  -- * Untagging
  , untagPattern
  , untagQualName
  )
  where
import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Hashable
import           Data.Maybe
import           Data.Monoid

import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import           Prelude

import           Futhark.FreshNames
import           Language.Futhark


-- | Associate a unique integer with each name in the program, taking
-- binding into account, such that the resulting 'VName's are unique.
-- The semantics of the program are unaffected, under the assumption
-- that the program was correct to begin with.
tagProg :: ProgBase NoInfo Name  -> (ProgBase NoInfo VName, VNameSource)
tagProg prog = let (decs', src') = runReader (runStateT f src) env
               in (Prog decs', src')

  where env = RenameEnv builtInMap newVNameFromName
        src = newNameSource $ succ $ maximum $ map baseTag $ HM.elems builtInMap
        decs = progDecs prog
        f = fst <$> renameDecs mempty decs

-- | Remove tags from a pattern.
untagPattern :: PatternBase NoInfo VName -> PatternBase NoInfo Name
untagPattern = untagger renamePattern

-- | Remove tags from a qualified name.
untagQualName :: QualName VName -> QualName Name
untagQualName (QualName (quals,v)) = QualName (quals, baseName v)

untagger :: (t -> RenameM VName Name a) -> t -> a
untagger f x = runReader (evalStateT (f x) blankNameSource) env
  where env = RenameEnv HM.empty rmTag
        rmTag src (ID (s, _)) = (s, src)

data Namespace = Term -- ^ Functions and values.
               | Type
               | Module
               deriving (Eq, Ord, Show, Enum)

instance Hashable Namespace where
  hashWithSalt salt = hashWithSalt salt . fromEnum

type NameMap f t = HM.HashMap (Namespace, QualName f) t

data RenameEnv f t = RenameEnv {
    envNameMap :: NameMap f t
  , envNameFn  :: VNameSource -> f -> (t, VNameSource)
  }

type RenameM f t = StateT VNameSource (Reader (RenameEnv f t))

builtInMap :: NameMap Name VName
builtInMap = HM.fromList $ map mapping $ HM.keys builtInFunctions
  where mapping v = ((Term, QualName ([], baseName v)), v)

-- | Return a fresh, unique name based on the given name.
new :: f -> RenameM f t t
new k = do (k', src') <- asks envNameFn <*> get <*> pure k
           put src'
           return k'

-- | 'repl s' returns the new name of the variable 's'.
repl :: (Eq f, Hashable f) =>
        IdentBase NoInfo f -> RenameM f t (IdentBase NoInfo t)
repl (Ident name NoInfo loc) = do
  name' <- replName Term name
  return $ Ident name' NoInfo loc

replName :: (Eq f, Hashable f) => Namespace -> f -> RenameM f t t
replName space name =
  maybe (new name) return =<<
  asks (HM.lookup (space, QualName ([], name)) . envNameMap)

replQual :: (Eq f, Hashable f, Show t, Show f) =>
            Namespace -> QualName f -> RenameM f t (QualName t)
replQual space qn@(QualName (quals, v)) = do
  v' <- maybe (new v) return =<< asks (HM.lookup (space, qn) . envNameMap)
  return $ QualName (quals, v')

bindNames :: (Eq f, Hashable f) => [f] -> RenameM f t a -> RenameM f t a
bindNames varnames = bindSpaced varnames'
  where varnames' = [ (Term, QualName ([], v)) | v <- varnames ]

bindSpaced :: (Eq f, Hashable f) =>
             [(Namespace, QualName f)] -> RenameM f t a -> RenameM f t a
bindSpaced varnames body = do
  vars' <- mapM newQual varnames
  -- This works because map union prefers elements from left
  -- operand.
  bindNameMap (HM.fromList (zip varnames vars')) body
  where newQual (_, QualName (_, v)) = new v

bindNameMap :: (Eq f, Hashable f) => NameMap f t -> RenameM f t a -> RenameM f t a
bindNameMap m = local $ \env -> env { envNameMap = m <> envNameMap env }

bind :: (Eq f, Hashable f) => [IdentBase x f] -> RenameM f t a -> RenameM f t a
bind = bindNames . map identName

renameFunOrTypeDec :: (Eq t, Ord f, Show t, Show f, Hashable t, Hashable f) =>
                      FunOrTypeDecBase NoInfo f
                   -> RenameM f t (FunOrTypeDecBase NoInfo t)
renameFunOrTypeDec (FunDec fun) = FunDec <$> renameFun fun
renameFunOrTypeDec (TypeDec td) = TypeDec <$> renameTypeAlias td

renameFun :: (Ord f, Hashable f, Eq t, Hashable t, Show t, Show f) =>
             FunDefBase NoInfo f -> RenameM f t (FunDefBase NoInfo t)
renameFun (FunDef entry fname (TypeDecl ret NoInfo) params body pos) =
 bindNames (concatMap (HS.toList . patNameSet) params) $
    FunDef entry <$> replName Term fname <*>
    (TypeDecl <$> renameUserType ret <*> pure NoInfo) <*>
    mapM renamePattern params <*>
    renameExp body <*>
    pure pos

renameTypeAlias :: (Eq f, Hashable f, Eq t, Hashable t, Show t, Show f) =>
                   TypeDefBase NoInfo f -> RenameM f t (TypeDefBase NoInfo t)
renameTypeAlias (TypeDef name typedecl loc) =
  TypeDef <$> replName Type name <*> renameUserTypeDecl typedecl <*> pure loc

renameUserTypeDecl :: (Eq f, Hashable f, Eq t, Hashable t, Show t, Show f) =>
                      TypeDeclBase NoInfo f
                   -> RenameM f t (TypeDeclBase NoInfo t)
renameUserTypeDecl (TypeDecl usertype NoInfo) =
  TypeDecl <$> renameUserType usertype <*> pure NoInfo

renameSignature :: (Eq f, Hashable f, Eq t, Hashable t, Show t, Show f) =>
                   SigDefBase NoInfo f -> RenameM f t (SigDefBase NoInfo t)
renameSignature (SigDef name sigdecs loc) =
  SigDef <$> replName Module name <*> mapM renameSigDec sigdecs <*> pure loc

renameModule :: ModDefBase NoInfo Name
             -> RenameM Name VName (ModDefBase NoInfo VName, NameMap Name VName)
renameModule (ModDef name moddecs loc) = do
  name' <- replName Module name
  (moddecs', m) <- renameDecs mempty moddecs
  return (ModDef name' moddecs' loc,
          HM.fromList $ map prepend $ HM.toList m)
  where prepend ((space, QualName (quals, v)), r) =
          ((space, QualName (name : quals, v)), r)

renameDecs :: NameMap Name VName -> [DecBase NoInfo Name]
           -> RenameM Name VName ([DecBase NoInfo VName], NameMap Name VName)
renameDecs m [] = return ([], m)
renameDecs m (SigDec sig:ds) = do
  sig' <- renameSignature sig
  (ds', m') <- renameDecs m ds
  return (SigDec sig':ds', m')
renameDecs m (ModDec modd:ds) = do
  (modd', bound) <- renameModule modd
  (ds', m') <- bindNameMap bound $ renameDecs (m<>bound) ds
  return (ModDec modd' : ds', m')
renameDecs m ds = do
  let (t_and_f_decs, ds') = chompDecs ds
      bound = concatMap lhs t_and_f_decs
  bindSpaced bound $ do
    t_and_f_decs' <- mapM renameFunOrTypeDec t_and_f_decs
    let m' = HM.fromList (zip bound $
                          concatMap (map unQual . lhs) t_and_f_decs') <>
             m
    (ds'', m'') <- renameDecs m' ds'
    return (map FunOrTypeDec t_and_f_decs' ++ ds'', m'')
  where lhs (FunDec dec)  = [(Term, QualName ([], funDefName dec))]
        lhs (TypeDec dec) = [(Type, QualName ([], typeAlias dec))]
        unQual (_, QualName (_, v)) = v

renameExp :: (Ord f, Eq f, Hashable f, Eq t, Hashable t, Show t, Show f) =>
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
  bindNames (HS.toList $ patNameSet pat) $ do
    pat' <- renamePattern pat
    body' <- renameExp body
    return $ LetPat pat' e1' body' pos
renameExp (DoLoop mergepat mergeexp form loopbody letbody pos) = do
  mergeexp' <- renameExp mergeexp
  case form of
    For dir lbound loopvar ubound -> do
      lbound' <- renameExp lbound
      ubound' <- renameExp ubound
      bindNames (HS.toList $ patNameSet mergepat) $ do
        mergepat' <- renamePattern mergepat
        letbody' <- renameExp letbody
        bind [loopvar] $ do
          loopvar'  <- repl loopvar
          loopbody' <- renameExp loopbody
          return $ DoLoop mergepat' mergeexp'
            (For dir lbound' loopvar' ubound') loopbody' letbody' pos
    While cond ->
      bindNames (HS.toList $ patNameSet mergepat) $ do
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

renameSigDec :: (Eq f, Hashable f, Eq t, Hashable t, Show t, Show f) =>
                SigDeclBase NoInfo f
             -> RenameM f t (SigDeclBase NoInfo t)
renameSigDec (FunSig name params rettype) =
  FunSig
  <$> replName Term name
  <*> mapM renameUserTypeDecl params
  <*> renameUserTypeDecl rettype

renameSigDec (TypeSig usertype) =
  TypeSig <$> renameTypeAlias usertype

renameUserType :: (Eq f, Hashable f, Eq t, Hashable t, Show t, Show f) =>
                   UserType f
                -> RenameM f t (UserType t)
renameUserType (UserPrim bt loc) = return $ UserPrim bt loc
renameUserType (UserUnique bt loc) = UserUnique <$> renameUserType bt <*> pure loc
renameUserType (UserTuple ts loc) = UserTuple <$> mapM renameUserType ts <*> pure loc
renameUserType (UserTypeAlias name loc) = UserTypeAlias <$> replQual Type name <*> pure loc
renameUserType (UserArray at d loc) =
  UserArray <$> renameUserType at <*> renameDim d <*> pure loc
  where renameDim AnyDim       = return AnyDim
        renameDim (NamedDim v) = NamedDim <$> replName Term v
        renameDim (ConstDim n) = return $ ConstDim n

renameCompType :: (Eq f, Hashable f, Eq t, Hashable t) =>
                  CompTypeBase f
               -> RenameM f t (CompTypeBase t)
renameCompType = renameTypeGeneric
                 (pure . Rank . shapeRank)
                 (fmap HS.fromList . mapM (replName Term) . HS.toList)

renameTypeGeneric :: (shape f -> RenameM f t (shape t))
                  -> (als f -> RenameM f t (als t))
                  -> TypeBase shape als f
                  -> RenameM f t (TypeBase shape als t)
renameTypeGeneric renameShape renameAliases = renameType'
  where renameType' (Array at) = Array <$> renameArrayType at
        renameType' (Prim bt)  = return $ Prim bt
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

rename :: (Ord f, Eq f, Hashable f, Eq t, Hashable t, Show t, Show f) =>
          MapperBase f t (RenameM f t)
rename = Mapper {
           mapOnExp = renameExp
         , mapOnPattern = renamePattern
         , mapOnIdent = repl
         , mapOnName = replName Term
         , mapOnQualName = replQual Term
         , mapOnLambda = renameLambda
         , mapOnType = renameCompType
         , mapOnValue = return
         }

renameLambda :: (Ord f, Eq f, Hashable f, Eq t, Hashable t, Show t, Show f) =>
                LambdaBase NoInfo f -> RenameM f t (LambdaBase NoInfo t)
renameLambda (AnonymFun params body maybe_ret NoInfo loc) =
  bindNames (concatMap (HS.toList . patNameSet) params) $ do
    params' <- mapM renamePattern params
    body' <- renameExp body
    maybe_ret' <- case maybe_ret of
                    Just (TypeDecl ret NoInfo) ->
                      Just <$> (TypeDecl <$> renameUserType ret <*> pure NoInfo)
                    Nothing ->
                      return Nothing
    return $ AnonymFun params' body' maybe_ret' NoInfo loc
renameLambda (CurryFun fname curryargexps NoInfo pos) =
  CurryFun <$> replQual Term fname <*> mapM renameExp curryargexps <*> pure NoInfo <*> pure pos
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

renamePattern :: (Eq f, Hashable f, Eq t, Hashable t, Show t, Show f) =>
                 PatternBase NoInfo f -> RenameM f t (PatternBase NoInfo t)
renamePattern (Id ident) =
  Id <$> repl ident
renamePattern (TuplePattern pats loc) =
  TuplePattern <$> mapM renamePattern pats <*> pure loc
renamePattern (Wildcard NoInfo loc) =
  return $ Wildcard NoInfo loc
renamePattern (PatternAscription p t) =
  PatternAscription <$> renamePattern p <*> renameUserTypeDecl t

renameDimIndex :: (Ord f, Eq f, Hashable f, Eq t, Hashable t, Show t, Show f) =>
                  DimIndexBase NoInfo f
               -> RenameM f t (DimIndexBase NoInfo t)
renameDimIndex (DimFix i)     = DimFix <$> renameExp i
renameDimIndex (DimSlice i j) = DimSlice <$> renameExp i <*> renameExp j

-- Take leading function and type declarations.
chompDecs :: [DecBase NoInfo Name]
          -> ([FunOrTypeDecBase NoInfo Name], [DecBase NoInfo Name])
chompDecs decs = f ([], decs)
  where f (foo , FunOrTypeDec dec : xs) = f (dec:foo , xs)
        f (foo , bar)                   = (foo, bar)
