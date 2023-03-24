-- | This Normalisation module converts a well-typed, polymorphic,
-- module-free Futhark program into an equivalent program without
-- arbitrary size expression.
module Futhark.Internalise.NormaliseSize (transformProg) where

import Control.Monad.RWS hiding (Sum)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer hiding (Sum)
import Data.Bifunctor
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.MonadFreshNames
import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Types

-- import Debug.Trace

newtype SimplifyM a
  = SimplifyM
      ( ReaderT
          (S.Set VName)
          (State VNameSource)
          a
      )
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader (S.Set VName),
      MonadState VNameSource,
      MonadFreshNames
    )

runSimplifier :: SimplifyM a -> VNameSource -> (a, VNameSource)
runSimplifier (SimplifyM m) =
  runState (runReaderT m mempty)

addBind :: VName -> SimplifyM a -> SimplifyM a
addBind = local . S.insert

newtype InnerSimplifyM a
  = InnerSimplifyM
      ( ReaderT
          (S.Set VName, M.Map Size VName)
          (StateT (M.Map Size VName) SimplifyM)
          a
      )
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader (S.Set VName, M.Map Size VName),
      MonadState (M.Map Size VName)
    )

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

runInnerSimplifier ::
  InnerSimplifyM a ->
  M.Map Size VName ->
  S.Set VName ->
  M.Map Size VName ->
  SimplifyM (a, M.Map Size VName)
runInnerSimplifier (InnerSimplifyM m) params =
  runStateT . runReaderT m . (,params)

withArgs :: S.Set VName -> InnerSimplifyM a -> InnerSimplifyM a
withArgs = local . first . S.union

withParams :: M.Map Size VName -> InnerSimplifyM a -> InnerSimplifyM a
withParams = local . second . M.union

askIntros :: S.Set VName -> InnerSimplifyM (S.Set VName)
askIntros argset =
  asks $ (S.filter notIntrisic argset `S.difference`) . fst
  where
    notIntrisic vn = baseTag vn > maxIntrinsicTag

parametrizing :: S.Set VName -> InnerSimplifyM (M.Map Size VName)
parametrizing argset = do
  intros <- askIntros argset
  (params, nxtBind) <- gets $ M.partitionWithKey (const . not . S.disjoint intros . M.keysSet . unFV . freeInExp . unSizeExpr)
  put nxtBind
  pure params

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

unscoping :: S.Set VName -> Exp -> InnerSimplifyM Exp
unscoping argset body = do
  intros <- askIntros argset
  (localDims, nxtBind) <-
    gets $
      M.partitionWithKey
        (const . not . S.disjoint intros . M.keysSet . unFV . freeInExp . unSizeExpr)
  put nxtBind
  foldrM insertDimCalculus body $ M.toList localDims

scoping :: S.Set VName -> InnerSimplifyM Exp -> InnerSimplifyM Exp
scoping argset m =
  withArgs argset m >>= unscoping argset

expFree :: Exp -> InnerSimplifyM Exp
expFree (AppExp (DoLoop dims pat ei form body loc) (Info resT)) = do
  let dimArgs =
        S.fromList dims
          `S.union` M.keysSet (unFV $ freeInPat pat)
  (form', formArgs) <- onForm form
  let argset = dimArgs `S.union` formArgs `S.union` patArg pat
  pat' <- withArgs dimArgs (onPat pat)
  params <- parametrizing argset
  let dims' = dims <> M.elems params
  resT' <- Info <$> onResType resT
  AppExp
    <$> ( DoLoop dims' pat'
            <$> expFree ei
            <*> pure form'
            <*> withParams params (scoping argset (expFree body))
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
  params <- parametrizing argset
  retT' <- withParams params $ onRetType argset retT
  body' <- withParams params $ scoping argset (expFree body)
  bodyNxt' <- scoping (S.singleton name) (expFree body_nxt)
  resT' <- Info <$> onResType resT
  pure $
    AppExp
      ( LetFun
          name
          ( typeParams <> map (\(e, vn) -> TypeParamDim vn (srclocOf $ unSizeExpr e)) (M.toList params),
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
  let dimArgs =
        S.fromList (map sizeName dims)
          `S.union` M.keysSet (unFV $ freeInPat pat)
  let letArgs = patArg pat
  let argset =
        dimArgs
          `S.union` letArgs
  pat' <- withArgs dimArgs (onPat pat)
  params <- parametrizing argset
  let dims' = dims <> map (\(e, vn) -> SizeBinder vn (srclocOf $ unSizeExpr e)) (M.toList params)
  resT' <- Info <$> withParams params (onResType resT)
  body' <- withParams params $ scoping argset (expFree body)
  e1' <- withParams params $ scoping dimArgs (expFree e1)
  pure $
    AppExp
      ( LetPat dims' pat' e1' body' loc
      )
      resT'
expFree (AppExp (LetWith dest src slice e body loc) (Info resT)) = do
  resT' <- Info <$> onResType resT
  AppExp
    <$> ( LetWith
            <$> onIdent dest
            <*> onIdent src
            <*> mapM onSlice slice
            <*> expFree e
            <*> scoping (S.singleton $ identName dest) (expFree body)
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
        <*> scoping args (expFree body)
        <*> pure cloc
expFree (AppExp (Coerce e expType eloc) (Info resT)) = do
  resT' <- Info <$> onResType resT
  AppExp
    <$> ( Coerce
            <$> expFree e
            <*> f expType
            <*> pure eloc
        )
    <*> pure resT'
  where
    f te@TEVar {} = pure te
    f (TETuple es loc) =
      TETuple <$> traverse f es <*> pure loc
    f (TERecord fs loc) =
      TERecord <$> traverse (mapM f) fs <*> pure loc
    f (TEArray s te loc) =
      TEArray <$> onSizeExp s <*> f te <*> pure loc
    f (TEUnique te loc) =
      TEUnique <$> f te <*> pure loc
    f (TEApply te tae loc) =
      TEApply <$> f te <*> tae' <*> pure loc
      where
        tae' = case tae of
          TypeArgExpSize s -> TypeArgExpSize <$> onSizeExp s
          TypeArgExpType t -> TypeArgExpType <$> f t
    f (TEArrow (Just vn) tea ter loc) =
      TEArrow (Just vn) <$> f tea <*> {-todo : add ext-} withArgs (S.singleton vn) (f ter) <*> pure loc
    f (TEArrow Nothing tea ter loc) =
      TEArrow Nothing <$> f tea <*> f ter <*> pure loc
    f (TESum constrs loc) =
      TESum <$> traverse (mapM (traverse f)) constrs <*> pure loc
    f (TEDim vns te loc) =
      TEDim vns <$> withArgs (S.fromList vns) (f te) <*> pure loc

    onSizeExp (SizeExp se loc) =
      SizeExp <$> (unSizeExpr <$> onExp se) <*> pure loc
    onSizeExp (SizeExpAny loc) =
      pure $ SizeExpAny loc
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
expFree (Lambda args body rettype_te (Info (as, retT)) loc) = do
  let argset = foldMap patArg args `S.union` foldMap (M.keysSet . unFV . freeInPat) args
  Lambda
    <$> mapM onPat args
    <*> scoping argset (expFree body)
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
  ty' <- withArgs argset $ onType ty
  rl <- parametrizing argset
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
      prev_param <- asks $ M.lookup e' . snd
      case (prev_param, prev) of
        (Just vn, _) -> pure $ sizeFromName (qualName vn) (srclocOf e)
        (Nothing, Just vn) -> pure $ sizeFromName (qualName vn) (srclocOf e)
        (Nothing, Nothing) -> do
          vn <- newNameFromString $ "d{" ++ prettyString e ++ "}"
          modify $ M.insert e' vn
          pure $ sizeFromName (qualName vn) (srclocOf e)

type ArrowArgM a = ReaderT (S.Set VName, [VName]) (WriterT (Maybe (S.Set VName)) SimplifyM) a

-- Reader (scope, dimtoPush) Writer(arrow arguments if an there's an arrow)

removeExpFromValBind ::
  ValBindBase Info VName -> SimplifyM (ValBindBase Info VName)
removeExpFromValBind valbind = do
  scope <-
    asks $
      S.union (S.fromList $ mapMaybe unParamDim $ valBindTypeParams valbind)
  (params', expNaming) <- runInnerSimplifier (mapM onPat $ valBindParams valbind) mempty scope mempty

  let args = foldMap patArg params'
  let argsParams = M.elems expNaming
  (rety', _) <- runInnerSimplifier (hardOnRetType $ unInfo $ valBindRetType valbind) expNaming (scope <> args) expNaming
  (rety'', funArg) <-
    second (fromMaybe mempty)
      <$> runWriterT (runReaderT (arrowArgRetType args rety') (scope, mempty))
  let newParams = funArg `S.union` S.fromList argsParams
      rety''' = arrowCleanRetType newParams rety''
      typeParams' =
        valBindTypeParams valbind
          <> map (`TypeParamDim` mempty) (S.toList newParams)

  let scope' = scope `S.union` args
  (body', expNaming'') <- runInnerSimplifier (expFree $ valBindBody valbind) expNaming scope' expNaming
  let newNames = expNaming'' `M.difference` expNaming
  body'' <- foldrM insertDimCalculus body' $ M.toList newNames
  pure $
    valbind
      { valBindRetType = Info rety''',
        valBindTypeParams = typeParams',
        valBindParams = params',
        valBindBody = body''
      }
  where
    hardOnRetType (RetType dims ty) = do
      predBind <- get
      ty' <- withArgs (S.fromList dims) (hardOnType ty)
      rl <- gets (`M.difference` predBind)
      unbounded <- askIntros $ M.keysSet (unFV $ freeInType ty')
      put predBind
      let dims' = M.elems rl <> S.toList unbounded
      pure $ RetType dims' ty'
    hardOnScalar (Record fs) =
      Record <$> traverse hardOnType fs
    hardOnScalar (Sum cs) =
      Sum <$> (traverse . traverse) hardOnType cs
    hardOnScalar (Arrow as argName d argT retT) =
      Arrow as argName d <$> hardOnType argT <*> withArgs argset (hardOnRetType retT)
      where
        argset =
          M.keysSet (unFV $ freeInType argT)
            <> case argName of
              Unnamed -> mempty
              Named vn -> S.singleton vn
    hardOnScalar ty = pure ty

    hardOnType (Array as u shape scalar) =
      Array as u <$> mapM onSize shape <*> hardOnScalar scalar
    hardOnType (Scalar ty) =
      Scalar <$> hardOnScalar ty

    arrowArgRetType :: S.Set VName -> RetTypeBase Size as -> ArrowArgM (RetTypeBase Size as)
    arrowArgRetType argset (RetType dims ty) = do
      scope <- asks $ (argset `S.union` S.fromList dims `S.union`) . fst
      (ty', arrArg) <- listen $ local (bimap (const scope) (<> dims)) $ arrowArgType ty
      case arrArg of
        Nothing {-rightmost of arrows-} -> do
          dims' <- asks $ (dims <>) . snd
          pure $ RetType dims' ty'
        Just args -> do
          canExt <- lift $ lift $ runReaderT (extBindType ty) scope
          writer (RetType (filter (`S.member` canExt) dims) ty', Just $ args `S.difference` canExt)

    arrowArgScalar :: ScalarTypeBase Size as -> ArrowArgM (ScalarTypeBase Size as)
    arrowArgScalar (Record fs) =
      Record <$> traverse arrowArgType fs
    arrowArgScalar (Sum cs) =
      Sum <$> (traverse . traverse) arrowArgType cs
    arrowArgScalar (Arrow as argName d argT retT) = do
      intros <- asks $ (S.filter notIntrisic argset `S.difference`) . fst
      retT' <- local (second $ filter (`S.notMember` intros)) $ arrowArgRetType fullArgset retT
      tell $ Just intros
      pure $ Arrow as argName d argT retT'
      where
        notIntrisic vn = baseTag vn > maxIntrinsicTag
        argset =
          M.keysSet (unFV $ freeInType argT)
        fullArgset =
          argset
            <> case argName of
              Unnamed -> mempty
              Named vn -> S.singleton vn
    arrowArgScalar ty = pure ty

    arrowArgType :: TypeBase Size as -> ArrowArgM (TypeBase Size as)
    arrowArgType (Array as u shape scalar) =
      Array as u shape <$> arrowArgScalar scalar
    arrowArgType (Scalar ty) =
      Scalar <$> arrowArgScalar ty
    --
    extBindRetType :: S.Set VName -> RetTypeBase Size as -> ReaderT (S.Set VName) SimplifyM (S.Set VName)
    extBindRetType argset (RetType dims ty) =
      local (argset `S.union` S.fromList dims `S.union`) $ extBindType ty

    extBindScalar :: ScalarTypeBase Size as -> ReaderT (S.Set VName) SimplifyM (S.Set VName)
    extBindScalar (Record fs) =
      foldM (\acc -> fmap (S.union acc) . extBindType) mempty fs
    extBindScalar (Sum cs) =
      foldM (\acc -> fmap (S.union acc) . foldM (\inacc -> fmap (S.union inacc) . extBindType) mempty) mempty cs
    extBindScalar (Arrow _ argName _ argT retT) = do
      intros <- asks (S.filter notIntrisic argset `S.difference`)
      (`S.difference` intros) <$> extBindRetType argset retT
      where
        notIntrisic vn = baseTag vn > maxIntrinsicTag
        argset =
          M.keysSet (unFV $ freeInType argT)
            <> case argName of
              Unnamed -> mempty
              Named vn -> S.singleton vn
    extBindScalar _ = pure mempty

    extBindType :: TypeBase Size as -> ReaderT (S.Set VName) SimplifyM (S.Set VName)
    extBindType (Array _ _ shape scalar) =
      (foldMap extBindSize shape `S.union`) <$> extBindScalar scalar
      where
        extBindSize (SizeExpr (Var qn _ _)) = S.singleton $ qualLeaf qn
        extBindSize _ = mempty
    extBindType (Scalar ty) =
      extBindScalar ty
    --
    arrowCleanRetType :: S.Set VName -> RetTypeBase Size as -> RetTypeBase Size as
    arrowCleanRetType paramed (RetType dims ty) =
      RetType (filter (`S.notMember` paramed) dims) (arrowCleanType (paramed `S.union` S.fromList dims) ty)

    arrowCleanScalar :: S.Set VName -> ScalarTypeBase Size as -> ScalarTypeBase Size as
    arrowCleanScalar paramed (Record fs) =
      Record $ M.map (arrowCleanType paramed) fs
    arrowCleanScalar paramed (Sum cs) =
      Sum $ (M.map . map) (arrowCleanType paramed) cs
    arrowCleanScalar paramed (Arrow as argName d argT retT) =
      Arrow as argName d argT (arrowCleanRetType paramed retT)
    arrowCleanScalar _ ty = ty

    arrowCleanType :: S.Set VName -> TypeBase Size as -> TypeBase Size as
    arrowCleanType paramed (Array as u shape scalar) =
      Array as u shape $ arrowCleanScalar paramed scalar
    arrowCleanType paramed (Scalar ty) =
      Scalar $ arrowCleanScalar paramed ty

simplifyDecs :: [Dec] -> SimplifyM [Dec]
simplifyDecs [] = pure []
simplifyDecs (ValDec valbind : ds) = do
  valbind' <- removeExpFromValBind valbind
  (ValDec valbind' :) <$> addBind (valBindName valbind) (simplifyDecs ds)
simplifyDecs (TypeDec td : ds) =
  (TypeDec td :) <$> simplifyDecs ds
simplifyDecs (dec : _) =
  error $
    "The normalisation module expects a module-free "
      ++ "input program, but received: "
      ++ prettyString dec

transformProg :: MonadFreshNames m => [Dec] -> m [Dec]
transformProg decs = do
  src <- getNameSource
  let (ret, src') = runSimplifier (simplifyDecs decs) src
  putNameSource src'
  pure ret
