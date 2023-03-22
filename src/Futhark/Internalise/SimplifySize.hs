-- | This Simplification module converts a well-typed, polymorphic,
-- module-free Futhark program into an equivalent program without
-- arbitrary size expression.
module Futhark.Internalise.SimplifySize (transformProg) where

import Control.Monad.RWS hiding (Sum)
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.MonadFreshNames
import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Types

data Env = Env
  { envNameSrc :: VNameSource,
    envPreviousDecs :: S.Set VName
  }

newtype SimplifyM a
  = SimplifyM (State Env a)
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadState Env
    )

runSimplifier :: VNameSource -> SimplifyM a -> (a, VNameSource)
runSimplifier src (SimplifyM m) = (r, envNameSrc env)
  where
    (r, env) = runState m (Env src mempty)

addBind :: VName -> SimplifyM ()
addBind vn = do
  Env src prev <- get
  put $ Env src $ S.insert vn prev

instance MonadFreshNames SimplifyM where
  getNameSource = gets envNameSrc
  putNameSource src =
    modify $ \env -> env {envNameSrc = src}

newtype InnerSimplifyM a
  = InnerSimplifyM
      ( ReaderT
          (S.Set VName)
          (StateT (M.Map Size VName) SimplifyM)
          a
      )
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader (S.Set VName),
      MonadState (M.Map Size VName)
    )

runInnerSimplifier ::
  InnerSimplifyM a ->
  S.Set VName ->
  M.Map Size VName ->
  SimplifyM (a, M.Map Size VName)
runInnerSimplifier (InnerSimplifyM m) =
  runStateT . runReaderT m

withArgs :: S.Set VName -> InnerSimplifyM a -> InnerSimplifyM a
withArgs = local . S.union

withArg :: VName -> InnerSimplifyM a -> InnerSimplifyM a
withArg = local . S.insert

instance MonadFreshNames InnerSimplifyM where
  getNameSource =
    InnerSimplifyM $
      ReaderT $
        const $
          StateT (((,) <$> getNameSource <*>) . pure)
  putNameSource src =
    InnerSimplifyM $
      ReaderT $
        const $
          StateT (((,) <$> putNameSource src <*>) . pure)

unParamDim :: TypeParam -> Maybe VName
unParamDim (TypeParamDim vn _) = Just vn
unParamDim _ = Nothing

patArg :: Pat -> S.Set VName
patArg (TuplePat ps _) = foldMap patArg ps
patArg (RecordPat fs _) = foldMap (patArg . snd) fs
patArg (PatParens p _) = patArg p
patArg (Id vn _ _) = S.singleton vn
patArg (Wildcard _ _) = mempty
patArg (PatAscription p _ _) = patArg p
patArg (PatLit {}) = mempty
patArg (PatConstr _ _ ps _) = foldMap patArg ps
patArg (PatAttr _ p _) = patArg p

unSizeExpr :: Size -> Exp
unSizeExpr (SizeExpr e) = e
unSizeExpr s = error $ "unSizeExpr " ++ prettyString s

maybeOldSize :: Exp -> Maybe Size
maybeOldSize e =
  case bareCleanExp e of
    Var qn _ loc -> Just $ sizeFromName qn loc
    IntLit v _ loc -> Just $ sizeFromInteger v loc
    _ -> Nothing

insertDimCalculus :: MonadFreshNames m => (Size, VName) -> Exp -> m Exp
insertDimCalculus (dim, name) body = do
  reName <- newNameFromString $ baseString name
  let expr = unSizeExpr dim
  pure $
    AppExp
      ( LetPat
          []
          (Id name (Info $ Scalar $ Prim $ Signed Int64) (srclocOf expr))
          expr
          body
          mempty
      )
      (appRes reName body)
  where
    appRes reName (AppExp _ (Info (AppRes ty ext))) =
      Info $ AppRes (applySubst (subst reName) ty) (ext <> [reName])
    appRes reName e =
      Info $ AppRes (applySubst (subst reName) $ typeOf e) [reName]

    subst reName vn
      | True <- vn == name = Just $ SizeSubst $ sizeFromName (qualName reName) mempty
      | otherwise = Nothing

hardOnRetType :: RetTypeBase Size as -> InnerSimplifyM (RetTypeBase Size as)
hardOnRetType (RetType dims ty) = do
  predBind <- get
  ty' <- onType ty
  newBind <- get
  let rl = newBind `M.difference` predBind
  let dims' = dims <> M.elems rl
  pure $ RetType dims' ty'

unscoping :: S.Set VName -> Exp -> InnerSimplifyM Exp
unscoping argset body = do
  (localDims, nxtBind) <-
    gets $
      M.partitionWithKey
        (const . not . S.disjoint argset . M.keysSet . unFV . freeInExp . unSizeExpr)
  put nxtBind
  foldrM insertDimCalculus body $ M.toList localDims

expFree :: Exp -> InnerSimplifyM Exp
expFree (AppExp (DoLoop dims pat ei form body loc) (Info resT)) = do
  let dimArgs = S.fromList dims
  (form', formArgs) <- onForm form
  let argset = dimArgs `S.union` formArgs `S.union` patArg pat
  pat' <- withArgs dimArgs (onPat pat) -------
  resT' <- Info <$> onResType resT
  AppExp
    <$> ( DoLoop dims {-TODO: add dims from pat'-} pat'
            <$> expFree ei
            <*> pure form'
            <*> withArgs argset (expFree body >>= unscoping argset)
            <*> pure loc
        )
    <*> pure resT'
  where
    onForm (For ident e) =
      (,S.singleton $ identName ident) <$> (For ident <$> expFree e)
    onForm (ForIn fpat e) =
      (,patArg fpat) <$> (ForIn fpat <$> expFree e)
    onForm (While e) =
      (,S.empty) <$> (While <$> expFree e)
expFree (AppExp (LetFun name (typeParams, args, rettype_te, Info retT, body) body_nxt loc) (Info resT)) = do
  let argset =
        S.fromList (mapMaybe unParamDim typeParams)
          `S.union` foldMap patArg args
          `S.union` foldMap (M.keysSet . unFV . freeInPat) args
  args' <- mapM onPat args
  newBind <- get
  let (localBind, nxtBind) =
        M.partitionWithKey
          (const . not . S.disjoint argset . M.keysSet . unFV . freeInExp . unSizeExpr)
          newBind
  retT' <- onRetType argset retT
  body' <- withArgs argset (expFree body >>= unscoping argset)
  resT' <- Info <$> onResType resT
  bodyNxt' <- withArg name (expFree body_nxt >>= unscoping (S.singleton name))
  put nxtBind
  pure $
    AppExp
      ( LetFun
          name
          ( typeParams <> map (\(e, vn) -> TypeParamDim vn (srclocOf $ unSizeExpr e)) (M.toList localBind),
            args',
            rettype_te, -- ?
            Info retT',
            body'
          )
          bodyNxt'
          loc
      )
      resT'
expFree (AppExp (LetPat dims pat e1 body loc) (Info resT)) = do
  let dimArgs = S.fromList (map sizeName dims)
  let letArgs = patArg pat
  let argset = dimArgs `S.union` letArgs
  resT' <- Info <$> onResType resT
  pat' <- withArgs dimArgs (onPat pat)
  AppExp
    <$> ( LetPat dims {-TODO: add dims from pat'-} pat'
            <$> expFree e1
            <*> withArgs argset (expFree body >>= unscoping argset)
            <*> pure loc
        )
    <*> pure resT'
expFree (AppExp (LetWith dest src slice e body loc) (Info resT)) = do
  resT' <- Info <$> onResType resT
  AppExp
    <$> ( LetWith
            <$> onIdent dest
            <*> onIdent src
            <*> mapM onSlice slice
            <*> expFree e
            <*> withArg (identName dest) (expFree body >>= unscoping (S.singleton $ identName dest))
            <*> pure loc
        )
    <*> pure resT'
  where
    onSlice (DimFix de) =
      DimFix <$> expFree de
    onSlice (DimSlice e1 e2 e3) =
      DimSlice
        <$> mapM expFree e1
        <*> mapM expFree e2
        <*> mapM expFree e3
    onIdent (Ident vn (Info ty) iloc) =
      Ident vn <$> (Info <$> onType ty) <*> pure iloc
expFree (AppExp (Match e cs loc) (Info resT)) = do
  resT' <- Info <$> onResType resT
  AppExp
    <$> ( Match
            <$> expFree e
            <*> mapM onCase cs
            <*> pure loc
        )
    <*> pure resT'
  where
    onCase (CasePat pat body cloc) = do
      let args = patArg pat
      CasePat
        <$> onPat pat
        <*> withArgs args (expFree body >>= unscoping args)
        <*> pure cloc
expFree (AppExp app (Info resT)) = do
  resT' <- Info <$> onResType resT
  AppExp <$> astMap mapper app <*> pure resT'
  where
    mapper =
      ASTMapper
        { mapOnExp = expFree,
          mapOnName = pure,
          mapOnStructType = onType,
          mapOnPatType = onType,
          mapOnStructRetType = error "mapOnRetType called in expFree: should not happen",
          mapOnPatRetType = error "mapOnRetType called in expFree: should not happen"
        }
expFree (Lambda args body rettype_te (Info (as, retT)) loc) =
  let argset = foldMap patArg args `S.union` foldMap (M.keysSet . unFV . freeInPat) args
   in Lambda
        <$> mapM onPat args
        <*> withArgs argset (expFree body >>= unscoping argset)
        <*> pure rettype_te -- ?
        <*> (Info . (as,) <$> onRetType argset retT)
        <*> pure loc
expFree (OpSectionLeft op (Info ty) e (Info (n1, ty1, m1), Info (n2, ty2)) (Info retT, Info ext) loc) = do
  let args =
        S.fromList ext
          `S.union` case n2 of
            Named vn -> S.singleton vn
            Unnamed -> mempty
  ty1' <- onType ty1
  ty2' <- onType ty2
  OpSectionLeft op
    <$> (Info <$> onType ty)
    <*> expFree e
    <*> pure (Info (n1, ty1', m1), Info (n2, ty2'))
    <*> ((,Info ext) . Info <$> onRetType args retT)
    <*> pure loc
expFree (OpSectionRight op (Info ty) e (Info (n1, ty1), Info (n2, ty2, m2)) (Info retT) loc) = do
  let args = case n1 of
        Named vn -> S.singleton vn
        Unnamed -> mempty
  ty1' <- onType ty1
  ty2' <- onType ty2
  OpSectionRight op
    <$> (Info <$> onType ty)
    <*> expFree e
    <*> pure (Info (n1, ty1'), Info (n2, ty2', m2))
    <*> (Info <$> onRetType args retT)
    <*> pure loc
expFree e = astMap mapper e
  where
    mapper =
      ASTMapper
        { mapOnExp = expFree,
          mapOnName = pure,
          mapOnStructType = onType,
          mapOnPatType = onType,
          mapOnStructRetType = error "mapOnRetType called in expFree: should not happen",
          mapOnPatRetType = error "mapOnRetType called in expFree: should not happen"
        }

onResType :: AppRes -> InnerSimplifyM AppRes
onResType (AppRes ty ext) =
  AppRes <$> withArgs (S.fromList ext) (onType ty) <*> pure ext

onPat :: Pat -> InnerSimplifyM Pat
onPat =
  astMap mapper
  where
    mapper =
      identityMapper
        { mapOnName = pure,
          mapOnStructType = onType,
          mapOnPatType = onType
        }

onRetType :: S.Set VName -> RetTypeBase Size as -> InnerSimplifyM (RetTypeBase Size as)
onRetType argset (RetType dims ty) = do
  intros <- asks (argset `S.difference`)
  ty' <- withArgs argset $ onType ty
  newBind <- get
  let (rl, nxtBind) = M.partitionWithKey (const . not . S.disjoint intros . M.keysSet . unFV . freeInExp . unSizeExpr) newBind
  put nxtBind
  let dims' = dims <> M.elems rl
  pure $ RetType dims' ty'

onScalar :: ScalarTypeBase Size as -> InnerSimplifyM (ScalarTypeBase Size as)
onScalar (Record fs) =
  Record <$> traverse onType fs
onScalar (Sum cs) =
  Sum <$> (traverse . traverse) onType cs
onScalar (Arrow as argName d argT retT) =
  Arrow as argName d <$> onType argT <*> onRetType argset retT
  where
    argset =
      M.keysSet (unFV $ freeInType argT)
        <> case argName of
          Unnamed -> mempty
          Named vn -> S.singleton vn
onScalar ty = pure ty

onType ::
  TypeBase Size as ->
  InnerSimplifyM (TypeBase Size as)
onType (Array as u shape scalar) =
  Array as u <$> mapM onSize shape <*> onScalar scalar
onType (Scalar ty) =
  Scalar <$> onScalar ty

onSize :: Size -> InnerSimplifyM Size
onSize (SizeExpr e) =
  onExp e
onSize s = pure s

onExp :: Exp -> InnerSimplifyM Size
onExp e = do
  let e' = SizeExpr e
  case maybeOldSize e of
    Just s -> pure s
    Nothing -> do
      prev <- gets $ M.lookup e'
      case prev of
        Just vn -> pure $ sizeFromName (qualName vn) (srclocOf e)
        Nothing -> do
          vn <- newNameFromString $ "d{" ++ prettyString e ++ "}"
          modify $ M.insert e' vn
          pure $ sizeFromName (qualName vn) (srclocOf e)

removeExpFromValBind ::
  ValBindBase Info VName -> SimplifyM (ValBindBase Info VName)
removeExpFromValBind valbind = do
  scope <-
    gets
      ( S.union (S.fromList $ mapMaybe unParamDim $ valBindTypeParams valbind)
          . envPreviousDecs
      )
  (params', expNaming) <- runInnerSimplifier (mapM onPat $ valBindParams valbind) scope mempty
  let typeParams' =
        valBindTypeParams valbind
          <> map (\(e, vn) -> TypeParamDim vn (srclocOf $ unSizeExpr e)) (M.toList expNaming)
  let args = foldMap patArg params'
  (rety', _) <- runInnerSimplifier (hardOnRetType $ unInfo $ valBindRetType valbind) (scope `S.union` args) expNaming
  let scope' = scope `S.union` args
  (body', expNaming') <- runInnerSimplifier (expFree $ valBindBody valbind) scope' expNaming
  let newNames = expNaming' `M.difference` expNaming
  body'' <- foldrM insertDimCalculus body' $ M.toList newNames
  addBind $ valBindName valbind
  pure $
    valbind
      { valBindRetType = Info rety',
        valBindTypeParams = typeParams',
        valBindParams = params',
        valBindBody = body''
      }

simplifyDecs :: [Dec] -> SimplifyM [Dec]
simplifyDecs [] = pure []
simplifyDecs (ValDec valbind : ds) = do
  valbind' <- removeExpFromValBind valbind
  (ValDec valbind' :) <$> simplifyDecs ds
simplifyDecs (TypeDec td : ds) =
  (TypeDec td :) <$> simplifyDecs ds
simplifyDecs (dec : _) =
  error $
    "The simplification module expects a module-free "
      ++ "input program, but received: "
      ++ prettyString dec

transformProg :: MonadFreshNames m => [Dec] -> m [Dec]
transformProg decs = do
  src <- getNameSource
  let (ret, src') = runSimplifier src (simplifyDecs decs)
  putNameSource src'
  pure ret
