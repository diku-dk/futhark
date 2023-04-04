-- | This Normalisation module converts a well-typed, monomorphised,
-- module-free Futhark program into an equivalent program without
-- arbitrary size expression.
-- This is done by replacing all complex size expresssions by an equivalent
-- of the old style NamedSize e.g. size just described as a variable.
-- This variables are then calculated in let-bindings or as a size parameter,
-- depending on the context.
module Futhark.Internalise.NormaliseSize (transformProg) where

import Control.Monad.Identity
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
import Futhark.Util (mapAccumLM)
import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Types

-- | Main Normalisation monad
-- Used all the long of a unique ValBind,
-- it keeps track of expressions that are being parametrized and variables in the scope
-- and remembers expressions being replaced to replace identical expressions with the same variable
newtype NormaliseM a
  = NormaliseM
      ( ReaderT
          (S.Set VName, M.Map Size VName)
          (StateT (M.Map Size VName) (State VNameSource))
          a
      )
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader (S.Set VName, M.Map Size VName),
      MonadState (M.Map Size VName)
    )

instance MonadFreshNames NormaliseM where
  getNameSource = NormaliseM $ lift $ lift getNameSource
  putNameSource = NormaliseM . lift . lift . putNameSource

runNormaliseM ::
  MonadFreshNames m =>
  NormaliseM a ->
  S.Set VName ->
  M.Map Size VName ->
  m (a, M.Map Size VName)
runNormaliseM (NormaliseM m) initialScope params =
  modifyNameSource $ \src ->
    runState (runReaderT m (initialScope, params) `runStateT` mempty) src

withArgs :: S.Set VName -> NormaliseM a -> NormaliseM a
withArgs = local . first . S.union

withParams :: M.Map Size VName -> NormaliseM a -> NormaliseM a
withParams = local . second . M.union

-- | Asks the introduced variables in a set of argument,
-- that is arguments not currently in scope.
askIntros :: S.Set VName -> NormaliseM (S.Set VName)
askIntros argset =
  asks $ (S.filter notIntrisic argset `S.difference`) . fst
  where
    notIntrisic vn = baseTag vn > maxIntrinsicTag

askScope :: NormaliseM (S.Set VName)
askScope =
  asks fst

-- | Gets and removes expressions that could not be calculated when
-- the arguments set will be unscoped.
-- This should be called without argset in scope, for good detection of intros.
parametrizing :: S.Set VName -> NormaliseM (M.Map Size VName)
parametrizing argset = do
  intros <- askIntros argset
  (params, nxtBind) <- gets $ M.partitionWithKey (const . not . S.disjoint intros . M.keysSet . unFV . freeInExp . unSizeExpr)
  put nxtBind
  pure params

unParamDim :: TypeParam -> Maybe VName
unParamDim (TypeParamDim vn _) = Just vn
unParamDim _ = Nothing

unSizeExpr :: Size -> Exp
unSizeExpr (SizeExpr e) = e
unSizeExpr s = error $ "unSizeExpr " ++ prettyString s

-- Avoid replacing of some 'already normalised' sizes that are just surounded by some parentheses.
maybeOldSize :: Exp -> Maybe Size
maybeOldSize (Parens e _) = maybeOldSize e
maybeOldSize (Attr _ e _) = maybeOldSize e
maybeOldSize (Assert _ e _ _) = maybeOldSize e
maybeOldSize (Var qn _ loc) = Just $ sizeFromName qn loc
maybeOldSize (IntLit v _ loc) = Just $ sizeFromInteger v loc
maybeOldSize _ = Nothing

canCalculate :: S.Set VName -> M.Map Size VName -> M.Map Size VName
canCalculate scope =
  M.filterWithKey (const . (`S.isSubsetOf` scope) . S.filter notIntrisic . M.keysSet . unFV . freeInExp . unSizeExpr)
  where
    notIntrisic vn = baseTag vn > maxIntrinsicTag

insertDimCalculus :: MonadFreshNames m => (Size, VName) -> Exp -> m Exp
insertDimCalculus (dim, name) body = do
  reName <- newName name
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
      | True <- vn == name = Just $ ExpSubst $ sizeVar (qualName reName) mempty
      | otherwise = Nothing

unscoping :: S.Set VName -> Exp -> NormaliseM Exp
unscoping argset body = do
  localDims <- parametrizing argset
  scope <- S.union argset <$> askScope
  foldrM insertDimCalculus body $ M.toList $ canCalculate scope localDims

scoping :: S.Set VName -> NormaliseM Exp -> NormaliseM Exp
scoping argset m =
  withArgs argset m >>= unscoping argset

-- Normalise types in an expression
expFree :: Exp -> NormaliseM Exp
expFree (AppExp (DoLoop dims pat ei form body loc) (Info resT)) = do
  let dimArgs = S.fromList dims
  (form', formArgs) <- onForm form
  let argset = dimArgs `S.union` formArgs `S.union` patNames pat
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
      (,patNames fpat) <$> (ForIn <$> onPat fpat <*> expFree e)
    onForm (While e) =
      (,S.empty) <$> (While <$> expFree e)
expFree (AppExp (LetFun name (typeParams, args, rettype_te, retT, body) body_nxt loc) resT) = do
  let argset =
        S.fromList (mapMaybe unParamDim typeParams)
          `S.union` foldMap patNames args
  args' <- mapM onPat args
  params <- parametrizing argset
  retT' <- withParams params $ onRetType argset $ unInfo retT
  body' <- withParams params $ scoping argset (expFree body)
  bodyNxt' <- scoping (S.singleton name) (expFree body_nxt)
  resT' <- Info <$> onResType (unInfo resT)
  pure $
    AppExp
      ( LetFun
          name
          ( typeParams <> map (`TypeParamDim` mempty) (M.elems params),
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
  implicitDims <- withArgs dimArgs $ askIntros (M.keysSet $ unFV $ freeInPat pat)
  let dimArgs' = dimArgs <> implicitDims
      letArgs = patNames pat
      argset = dimArgs' `S.union` letArgs
  pat' <- withArgs dimArgs' (onPat pat)
  params <- parametrizing dimArgs'
  let dims' = dims <> map (`SizeBinder` mempty) (M.elems params <> S.toList implicitDims)
  resT' <- Info <$> withParams params (onResType resT)
  body' <- withParams params $ scoping argset (expFree body)
  e1' <- expFree e1
  pure $ AppExp (LetPat dims' pat' e1' body' loc) resT'
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
  implicitDims <- askIntros (M.keysSet $ unFV $ freeInType $ typeOf e)
  e' <- expFree e
  cs' <- mapM (onCase implicitDims) cs
  if S.null implicitDims
    then pure $ AppExp (Match e' cs' loc) resT'
    else do
      tmpVar <- newNameFromString "matched_variable"
      pure $
        AppExp
          ( LetPat
              (map (`SizeBinder` mempty) $ S.toList implicitDims)
              (Id tmpVar (Info $ typeOf e') mempty)
              e'
              ( AppExp
                  (Match (Var (qualName tmpVar) (Info $ typeOf e') mempty) cs' loc)
                  resT'
              )
              mempty
          )
          resT'
  where
    onCase implicitDims (CasePat pat body cloc) = do
      let args = patNames pat
      CasePat
        <$> onPat pat
        <*> scoping (args <> implicitDims) (expFree body)
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
  let argset = foldMap patNames args `S.union` foldMap (M.keysSet . unFV . freeInPat) args
  Lambda
    <$> mapM onPat args
    <*> scoping argset (expFree body)
    <*> pure rettype_te -- ?
    <*> (Info . (as,) <$> onRetType argset retT)
    <*> pure loc
expFree e@(OpSectionLeft {}) = error $ "Unexpected section: " ++ prettyString e
expFree e@(OpSectionRight {}) = error $ "Unexpected section: " ++ prettyString e
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

-- Normalise a Type, ResType, RetType or types in a pattern
onResType :: AppRes -> NormaliseM AppRes
onResType (AppRes ty ext) =
  AppRes <$> withArgs (S.fromList ext) (onType ty) <*> pure ext

onPat :: Pat -> NormaliseM Pat
onPat =
  astMap mapper
  where
    mapper =
      identityMapper
        { mapOnName = pure,
          mapOnStructType = onType,
          mapOnPatType = onType
        }

onRetType :: S.Set VName -> RetTypeBase Size as -> NormaliseM (RetTypeBase Size as)
onRetType argset (RetType dims ty) = do
  ty' <- withArgs argset $ onType ty
  rl <- parametrizing argset
  let dims' = dims <> M.elems rl
  pure $ RetType dims' ty'

onScalar :: ScalarTypeBase Size as -> NormaliseM (ScalarTypeBase Size as)
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
onScalar (TypeVar as uniq qn args) =
  TypeVar as uniq qn <$> mapM onArg args
  where
    onArg (TypeArgDim dim loc) = TypeArgDim <$> onSize dim <*> pure loc
    onArg (TypeArgType ty loc) = TypeArgType <$> onType ty <*> pure loc
onScalar ty = pure ty

onType ::
  TypeBase Size as ->
  NormaliseM (TypeBase Size as)
onType (Array as u shape scalar) =
  Array as u <$> mapM onSize shape <*> onScalar scalar
onType (Scalar ty) =
  Scalar <$> onScalar ty

onSize :: Size -> NormaliseM Size
onSize (SizeExpr e) =
  onExp e
onSize s = pure s

-- | Creates a new expression replacement if needed, this always return old style sizes.
-- (e.g. single variable or constant)
onExp :: Exp -> NormaliseM Size
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
          vn <- newNameFromString $ "d<{" ++ prettyString e ++ "}>"
          modify $ M.insert e' vn
          pure $ sizeFromName (qualName vn) (srclocOf e)

-- | Monad for arrowArg
-- Reader (scope, dimtoPush)
-- Writer (arrow arguments, names that can be existentialy bound)
type ArrowArgM a = ReaderT (S.Set VName, [VName]) (Writer (S.Set VName, S.Set VName)) a

-- | arrowArg takes a type (or return type) and returns it
-- with the existentials bound moved at the right of arrows.
-- It also gives (through writer monad) size variables used in arrow arguments
-- and variables that are constructively used.
-- The returned type should be cleanned, as too many existentials are introduced.
arrowArgRetType ::
  S.Set VName ->
  RetTypeBase Size as ->
  ArrowArgM (RetTypeBase Size as)
arrowArgRetType argset (RetType dims ty) = pass $ do
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
    argset = M.keysSet (unFV $ freeInType argT)
    fullArgset =
      argset
        <> case argName of
          Unnamed -> mempty
          Named vn -> S.singleton vn
arrowArgScalar (TypeVar as uniq qn args) =
  TypeVar as uniq qn <$> mapM arrowArgArg args
  where
    arrowArgArg (TypeArgDim dim loc) = TypeArgDim <$> arrowArgSize dim <*> pure loc
    arrowArgArg (TypeArgType ty loc) = TypeArgType <$> arrowArgType ty <*> pure loc
arrowArgScalar ty = pure ty

arrowArgType :: TypeBase Size as -> ArrowArgM (TypeBase Size as)
arrowArgType (Array as u shape scalar) =
  Array as u <$> traverse arrowArgSize shape <*> arrowArgScalar scalar
arrowArgType (Scalar ty) =
  Scalar <$> arrowArgScalar ty

arrowArgSize :: Size -> ArrowArgM Size
arrowArgSize s@(SizeExpr (Var qn _ _)) = writer (s, (mempty, S.singleton $ qualLeaf qn))
arrowArgSize s = pure s

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
arrowCleanScalar paramed (TypeVar as uniq qn args) =
  TypeVar as uniq qn $ map arrowCleanArg args
  where
    arrowCleanArg (TypeArgDim dim loc) = TypeArgDim dim loc
    arrowCleanArg (TypeArgType ty loc) = TypeArgType (arrowCleanType paramed ty) loc
arrowCleanScalar _ ty = ty

arrowCleanType :: S.Set VName -> TypeBase Size as -> TypeBase Size as
arrowCleanType paramed (Array as u shape scalar) =
  Array as u shape $ arrowCleanScalar paramed scalar
arrowCleanType paramed (Scalar ty) =
  Scalar $ arrowCleanScalar paramed ty

-- Replace some expressions by a parameter.
-- (probably not needed anymore, but may be much more used to reduced number of calculation of sizes)
expReplace :: M.Map Size VName -> Exp -> Exp
expReplace mapping e
  | Just vn <- M.lookup (SizeExpr e) mapping =
      Var (qualName vn) (Info $ typeOf e) (srclocOf e)
expReplace mapping e = runIdentity $ astMap mapper e
  where
    mapper = identityMapper {mapOnExp = pure . expReplace mapping}

-- Normalise a ValBind
normaliseValBind :: MonadFreshNames m => S.Set VName -> ValBind -> m (S.Set VName, ValBind)
normaliseValBind prev_scope valbind = do
  let scope =
        prev_scope
          <> S.fromList (mapMaybe unParamDim $ valBindTypeParams valbind)
  (params', exp_naming) <-
    runNormaliseM (mapM onPat $ valBindParams valbind) scope mempty

  let args = foldMap patNames params'
      args_params = M.elems exp_naming
  (rety', extNaming) <-
    runNormaliseM (hardOnRetType $ unInfo $ valBindRetType valbind) (scope <> args) exp_naming
  let (rety'', (funArg, _)) =
        runWriter (runReaderT (arrowArgRetType args rety') (scope, mempty))
      new_params = funArg `S.union` S.fromList args_params
      rety''' = arrowCleanRetType new_params rety''
      typeParams' =
        valBindTypeParams valbind
          <> map (`TypeParamDim` mempty) (S.toList new_params)
      exp_naming' = M.filter (`S.member` new_params) extNaming

  let scope' = scope `S.union` args `S.union` new_params
  (body', exp_naming'') <-
    runNormaliseM (expFree $ valBindBody valbind) scope' exp_naming'
  let new_names = exp_naming'' `M.difference` exp_naming'
  body'' <-
    expReplace exp_naming'
      <$> foldrM insertDimCalculus body' (M.toList $ canCalculate scope' new_names)

  pure
    ( S.insert (valBindName valbind) prev_scope,
      valbind
        { valBindRetType = Info rety''',
          valBindTypeParams = typeParams',
          valBindParams = params',
          valBindBody = body''
        }
    )
  where
    hardOnRetType (RetType _ ty) = do
      ty' <- onType ty
      unbounded <- askIntros $ M.keysSet (unFV $ freeInType ty')
      let dims' = S.toList unbounded
      pure $ RetType dims' ty'

transformProg :: MonadFreshNames m => [ValBind] -> m [ValBind]
transformProg = fmap snd . mapAccumLM normaliseValBind mempty
