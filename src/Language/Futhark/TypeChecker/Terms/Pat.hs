-- | Type checking of patterns.
module Language.Futhark.TypeChecker.Terms.Pat
  ( binding,
    bindingParams,
    bindingPat,
    bindingIdent,
    bindingSizes,
  )
where

import Control.Monad
import Data.Bifunctor
import Data.Either
import Data.List (find, isPrefixOf, sort)
import Data.Map.Strict qualified as M
import Data.Maybe
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
nonrigidFor sizes = first onDim
  where
    onDim (Var (QualName _ v) info loc)
      | Just (_, v') <- find ((== v) . sizeName . fst) sizes =
          Var v' info loc
    onDim d = d

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
bindingIdent ::
  IdentBase NoInfo VName StructType ->
  StructType ->
  (Ident StructType -> TermTypeM a) ->
  TermTypeM a
bindingIdent (Ident v NoInfo vloc) t m = do
  let ident = Ident v (Info t) vloc
  binding [ident] $ m ident

-- All this complexity is just so we can handle un-suffixed numeric
-- literals in patterns.
patLitMkType :: PatLit -> SrcLoc -> TermTypeM ParamType
patLitMkType (PatLitInt _) loc = do
  t <- newTypeVar loc "t"
  mustBeOneOf anyNumberType (mkUsage loc "integer literal") (toStruct t)
  pure t
patLitMkType (PatLitFloat _) loc = do
  t <- newTypeVar loc "t"
  mustBeOneOf anyFloatType (mkUsage loc "float literal") (toStruct t)
  pure t
patLitMkType (PatLitPrim v) _ =
  pure $ Scalar $ Prim $ primValueType v

checkPat' ::
  [(SizeBinder VName, QualName VName)] ->
  PatBase NoInfo VName ParamType ->
  Inferred ParamType ->
  TermTypeM (Pat ParamType)
checkPat' sizes (PatParens p loc) t =
  PatParens <$> checkPat' sizes p t <*> pure loc
checkPat' sizes (PatAttr attr p loc) t =
  PatAttr <$> checkAttr attr <*> checkPat' sizes p t <*> pure loc
checkPat' _ (Id name NoInfo loc) (Ascribed t) =
  pure $ Id name (Info t) loc
checkPat' _ (Id name NoInfo loc) NoneInferred = do
  t <- newTypeVar loc "t"
  pure $ Id name (Info t) loc
checkPat' _ (Wildcard _ loc) (Ascribed t) =
  pure $ Wildcard (Info t) loc
checkPat' _ (Wildcard NoInfo loc) NoneInferred = do
  t <- newTypeVar loc "t"
  pure $ Wildcard (Info t) loc
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
  | Just (f, fp) <- find (("_" `isPrefixOf`) . nameToString . fst) p_fs =
      typeError fp mempty $
        "Underscore-prefixed fields are not allowed."
          </> "Did you mean"
          <> dquotes (pretty (drop 1 (nameToString f)) <> "=_")
          <> "?"
checkPat' sizes p@(RecordPat p_fs loc) (Ascribed t)
  | Scalar (Record t_fs) <- t,
    sort (map fst p_fs) == sort (M.keys t_fs) =
      RecordPat . M.toList <$> check t_fs <*> pure loc
  | otherwise = do
      p_fs' <- traverse (const $ newTypeVar loc "t") $ M.fromList p_fs

      when (sort (M.keys p_fs') /= sort (map fst p_fs)) $
        typeError loc mempty $
          "Duplicate fields in record pattern" <+> pretty p <> "."

      unify (mkUsage loc "matching a record pattern") (Scalar (Record p_fs')) (toStruct t)
      checkPat' sizes p $ Ascribed $ toParam Observe $ Scalar (Record p_fs')
  where
    check t_fs =
      traverse (uncurry (checkPat' sizes)) $
        M.intersectionWith (,) (M.fromList p_fs) (fmap Ascribed t_fs)
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
checkPat' _ (PatLit l NoInfo loc) (Ascribed t) = do
  t' <- patLitMkType l loc
  unify (mkUsage loc "matching against literal") (toStruct t') (toStruct t)
  pure $ PatLit l (Info t') loc
checkPat' _ (PatLit l NoInfo loc) NoneInferred = do
  t' <- patLitMkType l loc
  pure $ PatLit l (Info t') loc
checkPat' sizes (PatConstr n NoInfo ps loc) (Ascribed (Scalar (Sum cs)))
  | Just ts <- M.lookup n cs = do
      when (length ps /= length ts) $
        typeError loc mempty $
          "Pattern #"
            <> pretty n
            <> " expects"
              <+> pretty (length ps)
              <+> "constructor arguments, but type provides"
              <+> pretty (length ts)
              <+> "arguments."
      ps' <- zipWithM (checkPat' sizes) ps $ map Ascribed ts
      pure $ PatConstr n (Info (Scalar (Sum cs))) ps' loc
checkPat' sizes (PatConstr n NoInfo ps loc) (Ascribed t) = do
  t' <- newTypeVar loc "t"
  ps' <- forM ps $ \p -> do
    p_t <- newTypeVar (srclocOf p) "t"
    checkPat' sizes p $ Ascribed p_t
  mustHaveConstr usage n (toStruct t') (patternStructType <$> ps')
  unify usage t' (toStruct t)
  pure $ PatConstr n (Info t) ps' loc
  where
    usage = mkUsage loc "matching against constructor"
checkPat' sizes (PatConstr n NoInfo ps loc) NoneInferred = do
  ps' <- mapM (\p -> checkPat' sizes p NoneInferred) ps
  t <- newTypeVar loc "t"
  mustHaveConstr usage n (toStruct t) (patternStructType <$> ps')
  pure $ PatConstr n (Info t) ps' loc
  where
    usage = mkUsage loc "matching against constructor"

checkPat ::
  [(SizeBinder VName, QualName VName)] ->
  PatBase NoInfo VName (TypeBase Size u) ->
  Inferred StructType ->
  (Pat ParamType -> TermTypeM a) ->
  TermTypeM a
checkPat sizes p t m = do
  p' <-
    onFailure (CheckingPat (fmap toStruct p) t) $
      checkPat' sizes (fmap (toParam Observe) p) (fmap (toParam Observe) t)

  let explicit = mustBeExplicitInType $ patternStructType p'

  case filter ((`S.member` explicit) . sizeName . fst) sizes of
    (size, _) : _ ->
      typeError size mempty $
        "Cannot bind"
          <+> pretty size
          <+> "as it is never used as the size of a concrete (non-function) value."
    [] ->
      m p'

-- | Check and bind a @let@-pattern.
bindingPat ::
  [SizeBinder VName] ->
  PatBase NoInfo VName (TypeBase Size u) ->
  StructType ->
  (Pat ParamType -> TermTypeM a) ->
  TermTypeM a
bindingPat sizes p t m = do
  substs <- mapM mkSizeSubst sizes
  checkPat substs p (Ascribed t) $ \p' -> binding (patIdents (fmap toStruct p')) $
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
  [PatBase NoInfo VName ParamType] ->
  ([Pat ParamType] -> TermTypeM a) ->
  TermTypeM a
bindingParams tps orig_ps m = bindingTypeParams tps $ do
  let descend ps' (p : ps) =
        checkPat [] p NoneInferred $ \p' ->
          binding (patIdents $ fmap toStruct p') $ incLevel $ descend (p' : ps') ps
      descend ps' [] = m $ reverse ps'

  incLevel $ descend [] orig_ps
