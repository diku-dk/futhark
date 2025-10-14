-- | Type checking of patterns.
module Language.Futhark.TypeChecker.Terms.Pat
  ( binding,
    bindingParams,
    bindingParam,
    bindingPat,
    bindingIdent,
    bindingSizes,
  )
where

import Control.Monad
import Data.Bifunctor
import Data.Either
import Data.List (find, isPrefixOf, sortBy)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Ord (comparing)
import Data.Set qualified as S
import Futhark.Util.Pretty hiding (group, space)
import Language.Futhark
import Language.Futhark.TypeChecker.Monad hiding (BoundV)
import Language.Futhark.TypeChecker.Terms.Monad
import Language.Futhark.TypeChecker.Types
import Language.Futhark.TypeChecker.Unify hiding (Usage)
import Prelude hiding (mod)

nonrigidFor :: [(SizeBinder VName, QualName VName)] -> StructType -> StructType
nonrigidFor [] = id -- Minor optimisation.
nonrigidFor sizes = applySubst onDim
  where
    onDim v
      | Just (s, v') <- find ((== v) . sizeName . fst) sizes =
          Just $ ExpSubst $ sizeFromName v' (srclocOf s)
    onDim _ = Nothing

-- | Bind these identifiers locally while running the provided action.
binding ::
  [Ident StructType] ->
  TermTypeM a ->
  TermTypeM a
binding idents m =
  localScope (`bindVars` idents) $ do
    -- Those identifiers that can potentially also be sizes are
    -- added as type constraints.  This is necessary so that we
    -- can properly detect scope violations during unification.
    -- We do this for *all* identifiers, not just those that are
    -- integers, because they may become integers later due to
    -- inference...
    forM_ idents $ \ident ->
      constrain (identName ident) $ ParamSize $ locOf ident
    m
  where
    bindVars = foldl bindVar

    bindVar scope (Ident name (Info tp) _) =
      scope
        { scopeVtable =
            M.insert name (BoundV [] tp) $ scopeVtable scope
        }

bindingTypes ::
  [Either (VName, TypeBinding) (VName, Constraint)] ->
  TermTypeM a ->
  TermTypeM a
bindingTypes types m = do
  lvl <- curLevel
  modifyConstraints (<> M.map (lvl,) (M.fromList constraints))
  localScope extend m
  where
    (tbinds, constraints) = partitionEithers types
    extend scope =
      scope
        { scopeTypeTable = M.fromList tbinds <> scopeTypeTable scope
        }

bindingTypeParams :: [TypeParam] -> TermTypeM a -> TermTypeM a
bindingTypeParams tparams =
  binding (mapMaybe typeParamIdent tparams)
    . bindingTypes (concatMap typeParamType tparams)
  where
    typeParamType (TypeParamType l v loc) =
      [ Left (v, TypeAbbr l [] $ RetType [] $ Scalar (TypeVar mempty (qualName v) [])),
        Right (v, ParamType l $ locOf loc)
      ]
    typeParamType (TypeParamDim v loc) =
      [Right (v, ParamSize $ locOf loc)]

typeParamIdent :: TypeParam -> Maybe (Ident StructType)
typeParamIdent (TypeParamDim v loc) =
  Just $ Ident v (Info $ Scalar $ Prim $ Signed Int64) loc
typeParamIdent _ = Nothing

-- | Bind @let@-bound sizes.  This is usually followed by 'bindingPat'
-- immediately afterwards.
bindingSizes :: [SizeBinder VName] -> TermTypeM a -> TermTypeM a
bindingSizes [] m = m -- Minor optimisation.
bindingSizes sizes m = binding (map sizeWithType sizes) m
  where
    sizeWithType size =
      Ident (sizeName size) (Info (Scalar (Prim (Signed Int64)))) (srclocOf size)

-- | Bind a single term-level identifier.
bindingIdent :: Ident StructType -> TermTypeM a -> TermTypeM a
bindingIdent ident = binding [ident]

checkPat' ::
  [(SizeBinder VName, QualName VName)] ->
  Pat ParamType ->
  Inferred ParamType ->
  TermTypeM (Pat ParamType)
checkPat' sizes (PatParens p loc) t =
  PatParens <$> checkPat' sizes p t <*> pure loc
checkPat' sizes (PatAttr attr p loc) t =
  PatAttr <$> checkAttr attr <*> checkPat' sizes p t <*> pure loc
checkPat' _ (Id name (Info t) loc) NoneInferred = do
  t' <- replaceTyVars loc t
  pure $ Id name (Info t') loc
checkPat' _ (Id name (Info t1) loc) (Ascribed t2) = do
  t' <- instTyVars loc [] (first (const ()) t1) t2
  pure $ Id name (Info t') loc
checkPat' _ (Wildcard (Info t) loc) NoneInferred = do
  t' <- replaceTyVars loc t
  pure $ Wildcard (Info t') loc
checkPat' _ (Wildcard (Info t1) loc) (Ascribed t2) = do
  t' <- instTyVars loc [] (first (const ()) t1) t2
  pure $ Wildcard (Info t') loc
checkPat' sizes p@(TuplePat ps loc) (Ascribed t)
  | Just ts <- isTupleRecord t,
    length ts == length ps =
      TuplePat
        <$> zipWithM (checkPat' sizes) ps (map Ascribed ts)
        <*> pure loc
  | otherwise = do
      ps_t <- replicateM (length ps) (newTypeVar loc "t")
      unify (mkUsage loc "matching a tuple pattern") (Scalar (tupleRecord ps_t)) (toStruct t)
      checkPat' sizes p $ Ascribed $ toParam Observe $ Scalar $ tupleRecord ps_t
checkPat' sizes (TuplePat ps loc) NoneInferred =
  TuplePat <$> mapM (\p -> checkPat' sizes p NoneInferred) ps <*> pure loc
checkPat' _ (RecordPat p_fs _) _
  | Just (L loc f, _) <- find (("_" `isPrefixOf`) . nameToString . unLoc . fst) p_fs =
      typeError loc mempty $
        "Underscore-prefixed fields are not allowed."
          </> "Did you mean"
          <> dquotes (pretty (drop 1 (nameToString f)) <> "=_")
          <> "?"
checkPat' sizes p@(RecordPat p_fs loc) (Ascribed t)
  | Scalar (Record t_fs) <- t,
    p_fs' <- sortBy (comparing fst) p_fs,
    t_fs' <- sortBy (comparing fst) (M.toList t_fs),
    map fst t_fs' == map (unLoc . fst) p_fs' =
      RecordPat <$> zipWithM check p_fs' t_fs' <*> pure loc
  | otherwise = do
      p_fs' <- traverse (const $ newTypeVar loc "t") $ M.fromList $ map (first unLoc) p_fs
      unify (mkUsage loc "matching a record pattern") (Scalar (Record p_fs')) (toStruct t)
      checkPat' sizes p $ Ascribed $ toParam Observe $ Scalar (Record p_fs')
  where
    check (L f_loc f, p_f) (_, t_f) = (L f_loc f,) <$> checkPat' sizes p_f (Ascribed t_f)
checkPat' sizes (RecordPat fs loc) NoneInferred =
  RecordPat . M.toList
    <$> traverse (\p -> checkPat' sizes p NoneInferred) (M.fromList fs)
    <*> pure loc
checkPat' sizes (PatAscription p t loc) maybe_outer_t = do
  (t', st, _) <- checkTypeExpNonrigid t

  case maybe_outer_t of
    Ascribed outer_t -> do
      let st_forunify = nonrigidFor sizes $ toStruct st
      unify (mkUsage loc "explicit type ascription") st_forunify (toStruct outer_t)

      PatAscription
        <$> checkPat' sizes p (Ascribed (resToParam st))
        <*> pure t'
        <*> pure loc
    NoneInferred ->
      PatAscription
        <$> checkPat' sizes p (Ascribed (resToParam st))
        <*> pure t'
        <*> pure loc
checkPat' _ (PatLit l (Info t) loc) _ = do
  t' <- replaceTyVars loc t
  pure $ PatLit l (Info t') loc
checkPat' sizes (PatConstr n info ps loc) NoneInferred = do
  ps' <- mapM (\p -> checkPat' sizes p NoneInferred) ps
  pure $ PatConstr n info ps' loc
checkPat' sizes (PatConstr n _ ps loc) (Ascribed (Scalar (Sum cs)))
  | Just ts <- M.lookup n cs = do
      ps' <- zipWithM (\p t -> checkPat' sizes p (Ascribed t)) ps ts
      pure $ PatConstr n (Info (Scalar (Sum cs))) ps' loc
checkPat' _ p t =
  error . unlines $
    [ "checkPat': bad case",
      prettyString p,
      show t
    ]

checkPat ::
  [(SizeBinder VName, QualName VName)] ->
  Pat ParamType ->
  Inferred StructType ->
  (Pat ParamType -> TermTypeM a) ->
  TermTypeM a
checkPat sizes p t m = do
  p' <-
    onFailure (CheckingPat (fmap toStruct p) t) $
      checkPat' sizes p (fmap (toParam Observe) t)

  let explicit = mustBeExplicitInType $ patternStructType p'

  case filter ((`S.member` explicit) . sizeName . fst) sizes of
    (size, _) : _ ->
      typeError size mempty $
        "Cannot bind"
          <+> pretty size
          <+> "as it is never used as the size of a concrete (non-function) value."
    [] ->
      m p'

-- | Check and bind a single parameter.
bindingParam ::
  Pat ParamType ->
  StructType ->
  (Pat ParamType -> TermTypeM a) ->
  TermTypeM a
bindingParam p t m = do
  checkPat mempty p (Ascribed t) $ \p' ->
    binding (patIdents (fmap toStruct p')) $ m p'

-- | Check and bind a @let@-pattern.
bindingPat ::
  [SizeBinder VName] ->
  Pat (TypeBase Size u) ->
  StructType ->
  (Pat ParamType -> TermTypeM a) ->
  TermTypeM a
bindingPat sizes p t m = do
  substs <- mapM mkSizeSubst sizes
  checkPat substs (fmap (toParam Observe) p) (Ascribed t) $ \p' ->
    binding (patIdents (fmap toStruct p')) $
      case filter ((`S.notMember` fvVars (freeInPat p')) . sizeName) sizes of
        [] -> m p'
        size : _ -> unusedSize size
  where
    mkSizeSubst v = do
      v' <- newID $ baseName $ sizeName v
      constrain v' . Size Nothing $
        mkUsage v "ambiguous size of bound expression"
      pure (v, qualName v')

-- | Check and bind type and value parameters.
bindingParams ::
  [TypeParam] ->
  [Pat ParamType] ->
  ([Pat ParamType] -> TermTypeM a) ->
  TermTypeM a
bindingParams tps orig_ps m = bindingTypeParams tps $ do
  let descend ps' (p : ps) =
        checkPat [] p NoneInferred $ \p' ->
          binding (patIdents $ fmap toStruct p') $
            incLevel $
              descend (p' : ps') ps
      descend ps' [] = m $ reverse ps'

  incLevel $ descend [] orig_ps
