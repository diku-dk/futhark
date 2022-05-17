{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Type checking of patterns.
module Language.Futhark.TypeChecker.Terms.Pat
  ( binding,
    bindingParams,
    bindingPat,
    bindingIdent,
    bindingSizes,
    doNotShadow,
    boundAliases,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Bitraversable
import Data.Either
import Data.List (find, isPrefixOf, sort)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Futhark.Util.Pretty hiding (bool, group, space)
import Language.Futhark
import Language.Futhark.TypeChecker.Monad hiding (BoundV)
import Language.Futhark.TypeChecker.Terms.Monad
import Language.Futhark.TypeChecker.Types
import Language.Futhark.TypeChecker.Unify hiding (Usage)
import Prelude hiding (mod)

-- | Names that may not be shadowed.
doNotShadow :: [String]
doNotShadow = ["&&", "||"]

nonrigidFor :: [SizeBinder VName] -> StructType -> TermTypeM StructType
nonrigidFor [] t = pure t -- Minor optimisation.
nonrigidFor sizes t = evalStateT (bitraverse onDim pure t) mempty
  where
    onDim (NamedDim (QualName _ v))
      | Just size <- find ((== v) . sizeName) sizes = do
          prev <- gets $ lookup v
          case prev of
            Nothing -> do
              v' <- lift $ newID $ baseName v
              lift $ constrain v' $ Size Nothing $ mkUsage' $ srclocOf size
              modify ((v, v') :)
              pure $ NamedDim $ qualName v'
            Just v' ->
              pure $ NamedDim $ qualName v'
    onDim d = pure d

-- | The set of in-scope variables that are being aliased.
boundAliases :: Aliasing -> S.Set VName
boundAliases = S.map aliasVar . S.filter bound
  where
    bound AliasBound {} = True
    bound AliasFree {} = False

checkIfUsed :: Bool -> Occurrences -> Ident -> TermTypeM ()
checkIfUsed allow_consume occs v
  | not allow_consume,
    not $ unique $ unInfo $ identType v,
    Just occ <- find consumes occs =
      typeError (srclocOf occ) mempty $
        "Consuming"
          <+> pquote (pprName $ identName v)
          <+> textwrap ("which is a non-consumable parameter bound at " <> locStr (locOf v) <> ".")
  | not $ identName v `S.member` allOccurring occs,
    not $ "_" `isPrefixOf` prettyName (identName v) =
      warn (srclocOf v) $
        "Unused variable" <+> pquote (pprName $ identName v) <+> "."
  | otherwise =
      pure ()
  where
    consumes = maybe False (identName v `S.member`) . consumed

-- | Bind these identifiers locally while running the provided action.
-- Checks that the identifiers are used properly within the scope
-- (e.g. consumption).
binding ::
  -- | Allow consumption, even if the type is not unique.
  Bool ->
  [Ident] ->
  TermTypeM a ->
  TermTypeM a
binding allow_consume idents = check . handleVars
  where
    handleVars m =
      localScope (`bindVars` idents) $ do
        -- Those identifiers that can potentially also be sizes are
        -- added as type constraints.  This is necessary so that we
        -- can properly detect scope violations during unification.
        -- We do this for *all* identifiers, not just those that are
        -- integers, because they may become integers later due to
        -- inference...
        forM_ idents $ \ident ->
          constrain (identName ident) $ ParamSize $ srclocOf ident
        m

    bindVars :: TermScope -> [Ident] -> TermScope
    bindVars = foldl bindVar

    bindVar :: TermScope -> Ident -> TermScope
    bindVar scope (Ident name (Info tp) _) =
      let inedges = boundAliases $ aliases tp
          update (BoundV l tparams in_t)
            | Array {} <- tp = BoundV l tparams (in_t `addAliases` S.insert (AliasBound name))
            | otherwise = BoundV l tparams in_t
          update b = b

          tp' = tp `addAliases` S.insert (AliasBound name)
       in scope
            { scopeVtable =
                M.insert name (BoundV Local [] tp') $
                  adjustSeveral update inedges $
                    scopeVtable scope
            }

    adjustSeveral f = flip $ foldl $ flip $ M.adjust f

    -- Check whether the bound variables have been used correctly
    -- within their scope.
    check m = do
      (a, usages) <- collectBindingsOccurrences m
      checkOccurrences usages

      mapM_ (checkIfUsed allow_consume usages) idents

      pure a

    -- Collect and remove all occurences of @idents@.  This relies
    -- on the fact that no variables shadow any other.
    collectBindingsOccurrences m = do
      (x, usage) <- collectOccurrences m
      let (relevant, rest) = split usage
      occur rest
      pure (x, relevant)
      where
        onOcc occ =
          let (obs1, obs2) = divide $ observed occ
              occ_cons = divide <$> consumed occ
              con1 = fst <$> occ_cons
              con2 = snd <$> occ_cons
           in ( occ {observed = obs1, consumed = con1},
                occ {observed = obs2, consumed = con2}
              )
        split = unzip . map onOcc
        names = S.fromList $ map identName idents
        divide s = (s `S.intersection` names, s `S.difference` names)

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
  binding False (mapMaybe typeParamIdent tparams)
    . bindingTypes (concatMap typeParamType tparams)
  where
    typeParamType (TypeParamType l v loc) =
      [ Left (v, TypeAbbr l [] $ RetType [] $ Scalar (TypeVar () Nonunique (typeName v) [])),
        Right (v, ParamType l loc)
      ]
    typeParamType (TypeParamDim v loc) =
      [Right (v, ParamSize loc)]

typeParamIdent :: TypeParam -> Maybe Ident
typeParamIdent (TypeParamDim v loc) =
  Just $ Ident v (Info $ Scalar $ Prim $ Signed Int64) loc
typeParamIdent _ = Nothing

-- | Bind a single term-level identifier.
bindingIdent ::
  IdentBase NoInfo Name ->
  PatType ->
  (Ident -> TermTypeM a) ->
  TermTypeM a
bindingIdent (Ident v NoInfo vloc) t m =
  bindSpaced [(Term, v)] $ do
    v' <- checkName Term v vloc
    let ident = Ident v' (Info t) vloc
    binding True [ident] $ m ident

-- | Bind @let@-bound sizes.  This is usually followed by 'bindingPat'
-- immediately afterwards.
bindingSizes :: [SizeBinder Name] -> ([SizeBinder VName] -> TermTypeM a) -> TermTypeM a
bindingSizes [] m = m [] -- Minor optimisation.
bindingSizes sizes m = do
  foldM_ lookForDuplicates mempty sizes
  bindSpaced (map sizeWithSpace sizes) $ do
    sizes' <- mapM check sizes
    binding False (map sizeWithType sizes') $ m sizes'
  where
    lookForDuplicates prev size
      | Just prevloc <- M.lookup (sizeName size) prev =
          typeError size mempty $
            "Size name also bound at "
              <> text (locStrRel (srclocOf size) prevloc)
              <> "."
      | otherwise =
          pure $ M.insert (sizeName size) (srclocOf size) prev

    sizeWithSpace size =
      (Term, sizeName size)
    sizeWithType size =
      Ident (sizeName size) (Info (Scalar (Prim (Signed Int64)))) (srclocOf size)

    check (SizeBinder v loc) =
      SizeBinder <$> checkName Term v loc <*> pure loc

sizeBinderToParam :: SizeBinder VName -> UncheckedTypeParam
sizeBinderToParam (SizeBinder v loc) = TypeParamDim (baseName v) loc

-- | Check and bind a @let@-pattern.
bindingPat ::
  [SizeBinder VName] ->
  PatBase NoInfo Name ->
  InferredType ->
  (Pat -> TermTypeM a) ->
  TermTypeM a
bindingPat sizes p t m = do
  checkForDuplicateNames (map sizeBinderToParam sizes) [p]
  checkPat sizes p t $ \p' -> binding True (S.toList $ patIdents p') $ do
    -- Perform an observation of every declared dimension.  This
    -- prevents unused-name warnings for otherwise unused dimensions.
    let ident (SizeBinder v loc) =
          Ident v (Info (Scalar $ Prim $ Signed Int64)) loc
    mapM_ (observe . ident) sizes

    let used_sizes = typeDimNames $ patternStructType p'
    case filter ((`S.notMember` used_sizes) . sizeName) sizes of
      [] -> m p'
      size : _ -> unusedSize size

-- All this complexity is just so we can handle un-suffixed numeric
-- literals in patterns.
patLitMkType :: PatLit -> SrcLoc -> TermTypeM StructType
patLitMkType (PatLitInt _) loc = do
  t <- newTypeVar loc "t"
  mustBeOneOf anyNumberType (mkUsage loc "integer literal") t
  pure t
patLitMkType (PatLitFloat _) loc = do
  t <- newTypeVar loc "t"
  mustBeOneOf anyFloatType (mkUsage loc "float literal") t
  pure t
patLitMkType (PatLitPrim v) _ =
  pure $ Scalar $ Prim $ primValueType v

checkPat' ::
  [SizeBinder VName] ->
  UncheckedPat ->
  InferredType ->
  TermTypeM Pat
checkPat' sizes (PatParens p loc) t =
  PatParens <$> checkPat' sizes p t <*> pure loc
checkPat' sizes (PatAttr attr p loc) t =
  PatAttr <$> checkAttr attr <*> checkPat' sizes p t <*> pure loc
checkPat' _ (Id name _ loc) _
  | name' `elem` doNotShadow =
      typeError loc mempty $ "The" <+> text name' <+> "operator may not be redefined."
  where
    name' = nameToString name
checkPat' _ (Id name NoInfo loc) (Ascribed t) = do
  name' <- newID name
  pure $ Id name' (Info t) loc
checkPat' _ (Id name NoInfo loc) NoneInferred = do
  name' <- newID name
  t <- newTypeVar loc "t"
  pure $ Id name' (Info t) loc
checkPat' _ (Wildcard _ loc) (Ascribed t) =
  pure $ Wildcard (Info $ t `setUniqueness` Nonunique) loc
checkPat' _ (Wildcard NoInfo loc) NoneInferred = do
  t <- newTypeVar loc "t"
  pure $ Wildcard (Info t) loc
checkPat' sizes (TuplePat ps loc) (Ascribed t)
  | Just ts <- isTupleRecord t,
    length ts == length ps =
      TuplePat
        <$> zipWithM (checkPat' sizes) ps (map Ascribed ts)
        <*> pure loc
checkPat' sizes p@(TuplePat ps loc) (Ascribed t) = do
  ps_t <- replicateM (length ps) (newTypeVar loc "t")
  unify (mkUsage loc "matching a tuple pattern") (Scalar (tupleRecord ps_t)) $ toStruct t
  t' <- normTypeFully t
  checkPat' sizes p $ Ascribed t'
checkPat' sizes (TuplePat ps loc) NoneInferred =
  TuplePat <$> mapM (\p -> checkPat' sizes p NoneInferred) ps <*> pure loc
checkPat' _ (RecordPat p_fs _) _
  | Just (f, fp) <- find (("_" `isPrefixOf`) . nameToString . fst) p_fs =
      typeError fp mempty $
        "Underscore-prefixed fields are not allowed."
          </> "Did you mean" <> dquotes (text (drop 1 (nameToString f)) <> "=_") <> "?"
checkPat' sizes (RecordPat p_fs loc) (Ascribed (Scalar (Record t_fs)))
  | sort (map fst p_fs) == sort (M.keys t_fs) =
      RecordPat . M.toList <$> check <*> pure loc
  where
    check =
      traverse (uncurry (checkPat' sizes)) $
        M.intersectionWith (,) (M.fromList p_fs) (fmap Ascribed t_fs)
checkPat' sizes p@(RecordPat fields loc) (Ascribed t) = do
  fields' <- traverse (const $ newTypeVar loc "t") $ M.fromList fields

  when (sort (M.keys fields') /= sort (map fst fields)) $
    typeError loc mempty $ "Duplicate fields in record pattern" <+> ppr p <> "."

  unify (mkUsage loc "matching a record pattern") (Scalar (Record fields')) $ toStruct t
  t' <- normTypeFully t
  checkPat' sizes p $ Ascribed t'
checkPat' sizes (RecordPat fs loc) NoneInferred =
  RecordPat . M.toList
    <$> traverse (\p -> checkPat' sizes p NoneInferred) (M.fromList fs)
    <*> pure loc
checkPat' sizes (PatAscription p t loc) maybe_outer_t = do
  (t', st, _) <- checkTypeExpNonrigid t

  case maybe_outer_t of
    Ascribed outer_t -> do
      st_forunify <- nonrigidFor sizes st
      unify (mkUsage loc "explicit type ascription") st_forunify (toStruct outer_t)

      outer_t' <- normTypeFully outer_t
      PatAscription
        <$> checkPat' sizes p (Ascribed (addAliasesFromType st outer_t'))
        <*> pure t'
        <*> pure loc
    NoneInferred ->
      PatAscription <$> checkPat' sizes p (Ascribed (fromStruct st))
        <*> pure t'
        <*> pure loc
checkPat' _ (PatLit l NoInfo loc) (Ascribed t) = do
  t' <- patLitMkType l loc
  unify (mkUsage loc "matching against literal") t' (toStruct t)
  pure $ PatLit l (Info (fromStruct t')) loc
checkPat' _ (PatLit l NoInfo loc) NoneInferred = do
  t' <- patLitMkType l loc
  pure $ PatLit l (Info (fromStruct t')) loc
checkPat' sizes (PatConstr n NoInfo ps loc) (Ascribed (Scalar (Sum cs)))
  | Just ts <- M.lookup n cs = do
      ps' <- zipWithM (checkPat' sizes) ps $ map Ascribed ts
      pure $ PatConstr n (Info (Scalar (Sum cs))) ps' loc
checkPat' sizes (PatConstr n NoInfo ps loc) (Ascribed t) = do
  t' <- newTypeVar loc "t"
  ps' <- mapM (\p -> checkPat' sizes p NoneInferred) ps
  mustHaveConstr usage n t' (patternStructType <$> ps')
  unify usage t' (toStruct t)
  t'' <- normTypeFully t
  pure $ PatConstr n (Info t'') ps' loc
  where
    usage = mkUsage loc "matching against constructor"
checkPat' sizes (PatConstr n NoInfo ps loc) NoneInferred = do
  ps' <- mapM (\p -> checkPat' sizes p NoneInferred) ps
  t <- newTypeVar loc "t"
  mustHaveConstr usage n t (patternStructType <$> ps')
  pure $ PatConstr n (Info $ fromStruct t) ps' loc
  where
    usage = mkUsage loc "matching against constructor"

patNameMap :: Pat -> NameMap
patNameMap = M.fromList . map asTerm . S.toList . patNames
  where
    asTerm v = ((Term, baseName v), qualName v)

checkPat ::
  [SizeBinder VName] ->
  UncheckedPat ->
  InferredType ->
  (Pat -> TermTypeM a) ->
  TermTypeM a
checkPat sizes p t m = do
  checkForDuplicateNames (map sizeBinderToParam sizes) [p]
  p' <- onFailure (CheckingPat p t) $ checkPat' sizes p t

  let explicit = mustBeExplicitInType $ patternStructType p'

  case filter ((`S.member` explicit) . sizeName) sizes of
    size : _ ->
      typeError size mempty $
        "Cannot bind" <+> ppr size
          <+> "as it is never used as the size of a concrete (non-function) value."
    [] ->
      bindNameMap (patNameMap p') $ m p'

-- | Check and bind type and value parameters.
bindingParams ::
  [UncheckedTypeParam] ->
  [UncheckedPat] ->
  ([TypeParam] -> [Pat] -> TermTypeM a) ->
  TermTypeM a
bindingParams tps orig_ps m = do
  checkForDuplicateNames tps orig_ps
  checkTypeParams tps $ \tps' -> bindingTypeParams tps' $ do
    let descend ps' (p : ps) =
          checkPat [] p NoneInferred $ \p' ->
            binding False (S.toList $ patIdents p') $ descend (p' : ps') ps
        descend ps' [] = do
          -- Perform an observation of every type parameter.  This
          -- prevents unused-name warnings for otherwise unused
          -- dimensions.
          mapM_ observe $ mapMaybe typeParamIdent tps'
          m tps' $ reverse ps'

    descend [] orig_ps
