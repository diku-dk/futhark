-- | This Simplification module converts a well-typed, polymorphic,
-- module-free Futhark program into an equivalent program without
-- arbitrary size expression.
module Futhark.Internalise.SimplifySize (transformProg) where

import Control.Monad.RWS hiding (Sum)
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

removeExpFromValBind ::
  ValBindBase Info VName -> SimplifyM (ValBindBase Info VName)
removeExpFromValBind valbind = do
  scope <-
    gets
      ( S.union (S.fromList $ mapMaybe unParamDim $ valBindTypeParams valbind)
          . envPreviousDecs
      )
  (params', expNaming) <- runStateT (mapM (onPat scope) $ valBindParams valbind) mempty
  let typeParams' =
        valBindTypeParams valbind
          <> map (\(e, vn) -> TypeParamDim vn (srclocOf $ unSizeExpr e)) (M.toList expNaming)
  let args = foldMap patArg params'
  (rety', _) <- runStateT (hardOnRetType (scope `S.union` args) $ unInfo $ valBindRetType valbind) expNaming
  let scope' = scope `S.union` args
  (body', expNaming') <- runStateT (expFree scope' $ valBindBody valbind) expNaming
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
  where
    unParamDim (TypeParamDim vn _) = Just vn
    unParamDim _ = Nothing

    patArg (TuplePat ps _) = foldMap patArg ps
    patArg (RecordPat fs _) = foldMap (patArg . snd) fs
    patArg (PatParens p _) = patArg p
    patArg (Id vn _ _) = S.singleton vn
    patArg (Wildcard _ _) = mempty
    patArg (PatAscription p _ _) = patArg p
    patArg (PatLit {}) = mempty
    patArg (PatConstr _ _ ps _) = foldMap patArg ps
    patArg (PatAttr _ p _) = patArg p

    unSizeExpr (SizeExpr e) = e
    unSizeExpr _ = error "internal error in removeExpFromValBind"

    insertDimCalculus :: MonadFreshNames m' => (Size, VName) -> Exp -> m' Exp
    insertDimCalculus (dim, name) body = do
      reName <- newNameFromString $ baseString name
      let expr = unSizeExpr dim
      pure $ AppExp (LetPat [] (Id name (Info $ Scalar $ Prim $ Signed Int64) (srclocOf expr)) expr body mempty) (appRes reName body)
      where
        appRes reName (AppExp _ (Info (AppRes ty ext))) =
          Info $ AppRes (applySubst (subst reName) ty) (ext <> [reName])
        appRes reName e =
          Info $ AppRes (applySubst (subst reName) $ typeOf e) [reName]

        subst reName vn
          | True <- vn == name = Just $ SizeSubst $ sizeFromName (qualName reName) mempty
          | otherwise = Nothing

    -- using StateT (M.Map Exp VName) (SimplifyM m) a
    hardOnRetType argset (RetType dims ty) = do
      predBind <- get
      ty' <- onType argset ty
      newBind <- get
      let rl = newBind `M.difference` predBind
      let dims' = dims <> M.elems rl
      pure $ RetType dims' ty'

    unscoping argset body = do
      (localDims, nxtBind) <-
        gets $
          M.partitionWithKey
            (const . not . S.disjoint argset . M.keysSet . unFV . freeInExp . unSizeExpr)
      put nxtBind
      lift $ foldrM insertDimCalculus body $ M.toList localDims

    expFree scope (AppExp (DoLoop dims pat ei form body loc) (Info resT)) = do
      let dimArgs = S.fromList dims
      (form', formArgs) <- onForm form
      let argset = dimArgs `S.union` formArgs `S.union` patArg pat
      pat' <- onPat (scope `S.union` dimArgs) pat -------
      resT' <- Info <$> onResType scope resT
      AppExp
        <$> ( DoLoop dims {-TODO: add dims from pat'-} pat'
                <$> expFree scope ei
                <*> pure form'
                <*> (expFree (scope `S.union` argset) body >>= unscoping argset)
                <*> pure loc
            )
        <*> pure resT'
      where
        onForm (For ident e) =
          (,S.singleton $ identName ident) <$> (For ident <$> expFree scope e)
        onForm (ForIn fpat e) =
          (,patArg fpat) <$> (ForIn fpat <$> expFree scope e)
        onForm (While e) =
          (,S.empty) <$> (While <$> expFree scope e)
    expFree scope (AppExp (LetFun name (typeParams, args, rettype_te, Info retT, body) body_nxt loc) (Info resT)) = do
      let argset =
            S.fromList (mapMaybe unParamDim typeParams)
              `S.union` foldMap patArg args
              `S.union` foldMap (M.keysSet . unFV . freeInPat) args
      args' <- mapM (onPat scope) args
      newBind <- get
      let (localBind, nxtBind) =
            M.partitionWithKey
              (const . not . S.disjoint argset . M.keysSet . unFV . freeInExp . unSizeExpr)
              newBind
      retT' <- onRetType scope argset retT
      body' <- unscoping argset =<< expFree (scope `S.union` argset) body
      resT' <- Info <$> onResType scope resT
      bodyNxt' <- expFree (S.insert name scope) body_nxt >>= unscoping (S.singleton name)
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
    expFree scope (AppExp (LetPat dims pat e1 body loc) (Info resT)) = do
      let dimArgs = S.fromList (map sizeName dims)
      let letArgs = patArg pat
      let scope' = scope `S.union` dimArgs
      let scope'' = scope' `S.union` letArgs
      let argset = dimArgs `S.union` letArgs
      resT' <- Info <$> onResType scope resT
      pat' <- onPat scope' pat
      AppExp
        <$> ( LetPat dims {-TODO: add dims from pat'-} pat'
                <$> expFree scope e1
                <*> (expFree scope'' body >>= unscoping argset)
                <*> pure loc
            )
        <*> pure resT'
    expFree scope (AppExp (LetWith dest src slice e body loc) (Info resT)) = do
      resT' <- Info <$> onResType scope resT
      AppExp
        <$> ( LetWith
                <$> onIdent dest
                <*> onIdent src
                <*> mapM onSlice slice
                <*> expFree scope e
                <*> (expFree (S.insert (identName dest) scope) body >>= unscoping (S.singleton $ identName dest))
                <*> pure loc
            )
        <*> pure resT'
      where
        onSlice (DimFix de) =
          DimFix <$> expFree scope de
        onSlice (DimSlice e1 e2 e3) =
          DimSlice
            <$> mapM (expFree scope) e1
            <*> mapM (expFree scope) e2
            <*> mapM (expFree scope) e3
        onIdent (Ident vn (Info ty) iloc) =
          Ident vn <$> (Info <$> onType scope ty) <*> pure iloc
    expFree scope (AppExp (Match e cs loc) (Info resT)) = do
      resT' <- Info <$> onResType scope resT
      AppExp
        <$> ( Match
                <$> expFree scope e
                <*> mapM onCase cs
                <*> pure loc
            )
        <*> pure resT'
      where
        onCase (CasePat pat body cloc) = do
          let args = patArg pat
          CasePat
            <$> onPat scope pat
            <*> (expFree (scope `S.union` args) body >>= unscoping args)
            <*> pure cloc
    expFree scope (AppExp app (Info resT)) = do
      resT' <- Info <$> onResType scope resT
      AppExp <$> astMap mapper app <*> pure resT'
      where
        mapper =
          ASTMapper
            { mapOnExp = expFree scope,
              mapOnName = pure,
              mapOnStructType = onType scope,
              mapOnPatType = onType scope,
              mapOnStructRetType = mapOnRetType,
              mapOnPatRetType = mapOnRetType
            }
    expFree scope (Lambda args body rettype_te (Info (as, retT)) loc) =
      let argset = foldMap patArg args `S.union` foldMap (M.keysSet . unFV . freeInPat) args
       in Lambda
            <$> mapM (onPat scope) args
            <*> (expFree (scope `S.union` argset) body >>= unscoping argset)
            <*> pure rettype_te -- ?
            <*> (Info . (as,) <$> onRetType scope argset retT)
            <*> pure loc
    expFree scope (OpSectionLeft op (Info ty) e (Info (n1, ty1, m1), Info (n2, ty2)) (Info retT, Info ext) loc) = do
      let args =
            S.fromList ext
              `S.union` case n2 of
                Named vn -> S.singleton vn
                Unnamed -> mempty
      ty1' <- onType scope ty1
      ty2' <- onType scope ty2
      OpSectionLeft op
        <$> (Info <$> onType scope ty)
        <*> expFree scope e
        <*> pure (Info (n1, ty1', m1), Info (n2, ty2'))
        <*> ((,Info ext) . Info <$> onRetType scope args retT)
        <*> pure loc
    expFree scope (OpSectionRight op (Info ty) e (Info (n1, ty1), Info (n2, ty2, m2)) (Info retT) loc) = do
      let args = case n1 of
            Named vn -> S.singleton vn
            Unnamed -> mempty
      ty1' <- onType scope ty1
      ty2' <- onType scope ty2
      OpSectionRight op
        <$> (Info <$> onType scope ty)
        <*> expFree scope e
        <*> pure (Info (n1, ty1'), Info (n2, ty2', m2))
        <*> (Info <$> onRetType scope args retT)
        <*> pure loc
    expFree scope e = astMap mapper e
      where
        mapper =
          ASTMapper
            { mapOnExp = expFree scope,
              mapOnName = pure,
              mapOnStructType = onType scope,
              mapOnPatType = onType scope,
              mapOnStructRetType = mapOnRetType,
              mapOnPatRetType = mapOnRetType
            }

    mapOnRetType _ =
      error "mapOnRetType called in expFree: should not happen"

    onResType scope (AppRes ty ext) =
      AppRes <$> onType (scope `S.union` S.fromList ext) ty <*> pure ext

    onPat scope =
      astMap mapper
      where
        mapper =
          identityMapper
            { mapOnName = pure,
              mapOnStructType = onType scope,
              mapOnPatType = onType scope
            }

    onRetType scope argset (RetType dims ty) = do
      let intros = argset `S.difference` scope
      ty' <- onType (scope `S.union` argset) ty
      newBind <- get
      let (rl, nxtBind) = M.partitionWithKey (const . not . S.disjoint intros . M.keysSet . unFV . freeInExp . unSizeExpr) newBind
      put nxtBind
      let dims' = dims <> M.elems rl
      pure $ RetType dims' ty'

    onScalar scope (Record fs) =
      Record <$> traverse (onType scope) fs
    onScalar scope (Sum cs) =
      Sum <$> (traverse . traverse) (onType scope) cs
    onScalar scope (Arrow as argName d argT retT) =
      Arrow as argName d <$> onType scope argT <*> onRetType scope argset retT
      where
        argset =
          M.keysSet (unFV $ freeInType argT)
            <> case argName of
              Unnamed -> mempty
              Named vn -> S.singleton vn
    onScalar _ ty = pure ty

    onType ::
      S.Set VName ->
      TypeBase Size as ->
      StateT (M.Map Size VName) SimplifyM (TypeBase Size as) -- Precise the typing, else haskell refuse it
    onType scope (Array as u shape scalar) =
      Array as u <$> traverse onSize shape <*> onScalar scope scalar
    onType scope (Scalar ty) =
      Scalar <$> onScalar scope ty

    onSize (SizeExpr e) =
      onExp e
    onSize s = pure s

    onExp e@(Var {}) = pure $ SizeExpr e
    onExp e@(IntLit {}) = pure $ SizeExpr e
    onExp e = do
      let e' = SizeExpr e
      case maybeOldSize e' of
        Just s -> pure s
        Nothing -> do
          prev <- gets $ M.lookup e'
          case prev of
            Just vn -> pure $ sizeFromName (qualName vn) (srclocOf e)
            Nothing -> do
              vn <- lift $ newNameFromString $ "d{" ++ prettyString e ++ "}"
              modify $ M.insert e' vn
              pure $ sizeFromName (qualName vn) (srclocOf e)

    maybeOldSize (SizeExpr e) =
      case bareCleanExp e of
        Var qn _ loc -> Just $ sizeFromName qn loc
        IntLit v _ loc -> Just $ sizeFromInteger v loc
        _ -> Nothing
    maybeOldSize _ = error "internal error : maybeOldSize"

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
