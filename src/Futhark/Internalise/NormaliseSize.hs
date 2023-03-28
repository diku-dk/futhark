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
import Data.List (nub)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.MonadFreshNames
import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Types

-- import Debug.Trace

newtype NormaliseM a
  = NormaliseM
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

runNormaliser :: NormaliseM a -> VNameSource -> (a, VNameSource)
runNormaliser (NormaliseM m) =
  runState (runReaderT m mempty)

addBind :: VName -> NormaliseM a -> NormaliseM a
addBind = local . S.insert

newtype InnerNormaliseM a
  = InnerNormaliseM
      ( ReaderT
          (S.Set VName, M.Map Size VName)
          (StateT (M.Map Size VName) NormaliseM)
          a
      )
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader (S.Set VName, M.Map Size VName),
      MonadState (M.Map Size VName)
    )

instance MonadFreshNames InnerNormaliseM where
  getNameSource =
    InnerNormaliseM $
      ReaderT $
        const $
          StateT (((,) <$> getNameSource <*>) . pure)
  putNameSource src =
    InnerNormaliseM $
      ReaderT $
        const $
          StateT (((,) <$> putNameSource src <*>) . pure)

runInnerNormaliser ::
  InnerNormaliseM a ->
  M.Map Size VName ->
  S.Set VName ->
  M.Map Size VName ->
  NormaliseM (a, M.Map Size VName)
runInnerNormaliser (InnerNormaliseM m) params =
  runStateT . runReaderT m . (,params)

withArgs :: S.Set VName -> InnerNormaliseM a -> InnerNormaliseM a
withArgs = local . first . S.union

withParams :: M.Map Size VName -> InnerNormaliseM a -> InnerNormaliseM a
withParams = local . second . M.union

askIntros :: S.Set VName -> InnerNormaliseM (S.Set VName)
askIntros argset =
  asks $ (S.filter notIntrisic argset `S.difference`) . fst
  where
    notIntrisic vn = baseTag vn > maxIntrinsicTag

askScope :: InnerNormaliseM (S.Set VName)
askScope =
  asks fst

parametrizing :: S.Set VName -> InnerNormaliseM (M.Map Size VName)
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

canCalculate :: S.Set VName -> M.Map Size VName -> M.Map Size VName
canCalculate scope =
  M.filterWithKey (const . (`S.isSubsetOf` scope) . S.filter notIntrisic . M.keysSet . unFV . freeInExp . unSizeExpr)
  where
    notIntrisic vn = baseTag vn > maxIntrinsicTag

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

unscoping :: S.Set VName -> Exp -> InnerNormaliseM Exp
unscoping argset body = do
  intros <- askIntros argset
  (localDims, nxtBind) <-
    gets $
      M.partitionWithKey
        (const . not . S.disjoint intros . M.keysSet . unFV . freeInExp . unSizeExpr)
  put nxtBind
  scope <- S.union argset <$> askScope
  foldrM insertDimCalculus body $ M.toList $ canCalculate scope localDims

scoping :: S.Set VName -> InnerNormaliseM Exp -> InnerNormaliseM Exp
scoping argset m =
  withArgs argset m >>= unscoping argset

expFree :: Exp -> InnerNormaliseM Exp
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

onResType :: AppRes -> InnerNormaliseM AppRes
onResType (AppRes ty ext) =
  AppRes <$> withArgs (S.fromList ext) (onType ty) <*> pure ext

onPat :: Pat -> InnerNormaliseM Pat
onPat =
  astMap mapper
  where
    mapper =
      identityMapper
        { mapOnName = pure,
          mapOnStructType = onType,
          mapOnPatType = onType
        }

onRetType :: S.Set VName -> RetTypeBase Size as -> InnerNormaliseM (RetTypeBase Size as)
onRetType argset (RetType dims ty) = do
  ty' <- withArgs argset $ onType ty
  rl <- parametrizing argset
  let dims' = dims <> M.elems rl
  pure $ RetType dims' ty'

onScalar :: ScalarTypeBase Size as -> InnerNormaliseM (ScalarTypeBase Size as)
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
  InnerNormaliseM (TypeBase Size as)
onType (Array as u shape scalar) =
  Array as u <$> mapM onSize shape <*> onScalar scalar
onType (Scalar ty) =
  Scalar <$> onScalar ty

onSize :: Size -> InnerNormaliseM Size
onSize (SizeExpr e) =
  onExp e
onSize s = pure s

onExp :: Exp -> InnerNormaliseM Size
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

-- | arrowArg takes a type (or return type) and returns it
-- with the existentials bound moved at the right of arrows.
-- It also gives (through writer monad) size variables used in arrow arguments
-- and variables that are constructively used.
-- The returned type should be cleanned, as too many existentials are introduced.
type ArrowArgM a = ReaderT (S.Set VName, [VName]) (WriterT (S.Set VName, S.Set VName) NormaliseM) a

-- Reader (scope, dimtoPush)
-- Writer(arrow arguments, names that can be existentialy bound)

arrowArgRetType :: S.Set VName -> RetTypeBase Size as -> ArrowArgM (RetTypeBase Size as)
arrowArgRetType argset (RetType dims ty) =
  pass $ do
    (ty', (_, canExt)) <- listen $ local (bimap (argset `S.union`) (<> dims)) $ arrowArgType ty
    dims' <- asks $ (dims <>) . snd
    pure (RetType (filter (`S.member` canExt) dims') ty', first (`S.difference` canExt))

arrowArgScalar :: ScalarTypeBase Size as -> ArrowArgM (ScalarTypeBase Size as)
arrowArgScalar (Record fs) =
  Record <$> traverse arrowArgType fs
arrowArgScalar (Sum cs) =
  Sum <$> (traverse . traverse) arrowArgType cs
arrowArgScalar (Arrow as argName d argT retT) =
  pass $ do
    intros <- asks $ (S.filter notIntrisic argset `S.difference`) . fst
    retT' <- local (second $ filter (`S.notMember` intros)) $ arrowArgRetType fullArgset retT
    pure (Arrow as argName d argT retT', bimap (intros `S.union`) (const mempty))
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
  Array as u <$> traverse arrowArgSize shape <*> arrowArgScalar scalar
  where
    arrowArgSize s@(SizeExpr (Var qn _ _)) = writer (s, (mempty, S.singleton $ qualLeaf qn))
    arrowArgSize s = pure s
arrowArgType (Scalar ty) =
  Scalar <$> arrowArgScalar ty

-- | arrowClean cleans the mess of arrowArg
arrowCleanRetType :: S.Set VName -> RetTypeBase Size as -> RetTypeBase Size as
arrowCleanRetType paramed (RetType dims ty) =
  RetType (nub $ filter (`S.notMember` paramed) dims) (arrowCleanType (paramed `S.union` S.fromList dims) ty)

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

expReplace :: M.Map Size VName -> Exp -> NormaliseM Exp
expReplace mapping e
  | Just vn <- M.lookup (SizeExpr e) mapping = pure $ Var (qualName vn) (Info $ typeOf e) (srclocOf e)
expReplace mapping e = astMap mapper e
  where
    mapper = identityMapper {mapOnExp = expReplace mapping}

removeExpFromValBind ::
  ValBindBase Info VName -> NormaliseM (ValBindBase Info VName)
removeExpFromValBind valbind = do
  scope <-
    asks $
      S.union (S.fromList $ mapMaybe unParamDim $ valBindTypeParams valbind)
  (params', expNaming) <- runInnerNormaliser (mapM onPat $ valBindParams valbind) mempty scope mempty

  let args = foldMap patArg params'
  let argsParams = M.elems expNaming
  (rety', extNaming) <- runInnerNormaliser (hardOnRetType $ unInfo $ valBindRetType valbind) expNaming (scope <> args) expNaming
  (rety'', (funArg, _)) <-
    runWriterT (runReaderT (arrowArgRetType args rety') (scope, mempty))
  let newParams =
        funArg
          `S.union` S.fromList
            ( if null params' && isNothing (valBindEntryPoint valbind)
                then filter (`elem` M.elems extNaming) $ retDims rety''
                else argsParams
            )
      rety''' = arrowCleanRetType newParams rety''
      typeParams' =
        valBindTypeParams valbind
          <> map (`TypeParamDim` mempty) (S.toList newParams)
      expNaming' = M.filter (`S.member` newParams) extNaming

  let scope' = scope `S.union` args `S.union` newParams
  (body', expNaming'') <- runInnerNormaliser (expFree $ valBindBody valbind) expNaming' scope' expNaming'
  let newNames = expNaming'' `M.difference` expNaming'
  body'' <- foldrM insertDimCalculus body' $ M.toList $ canCalculate scope' newNames
  body''' <- expReplace expNaming' body''

  pure $
    valbind
      { valBindRetType = Info rety''',
        valBindTypeParams = typeParams',
        valBindParams = params',
        valBindBody = body'''
      }
  where
    hardOnRetType (RetType _ ty) = do
      ty' <- onType ty
      unbounded <- askIntros $ M.keysSet (unFV $ freeInType ty')
      let dims' = S.toList unbounded
      pure $ RetType dims' ty'

normaliseDecs :: [Dec] -> NormaliseM [Dec]
normaliseDecs [] = pure []
normaliseDecs (ValDec valbind : ds) = do
  valbind' <- removeExpFromValBind valbind
  (ValDec valbind' :) <$> addBind (valBindName valbind) (normaliseDecs ds)
normaliseDecs (TypeDec td : ds) =
  (TypeDec td :) <$> normaliseDecs ds
normaliseDecs (dec : _) =
  error $
    "The normalisation module expects a module-free "
      ++ "input program, but received: "
      ++ prettyString dec

transformProg :: MonadFreshNames m => [Dec] -> m [Dec]
transformProg decs = do
  src <- getNameSource
  let (ret, src') = runNormaliser (normaliseDecs decs) src
  putNameSource src'
  pure ret
