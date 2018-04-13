{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Language.Futhark.TypeChecker.Types
  ( checkTypeExp
  , checkTypeDecl

  , unifyTypes
  , unifyTypesU
  , subtypeOf
  , subuniqueOf
  , similarTo

  , checkForDuplicateNames
  , checkTypeParams

  , TypeSub(..)
  , TypeSubs
  , substituteTypes
  , substituteTypesInBoundV

  , instantiatePolymorphic

  , Substitutable(..)
  , substTypesAny
  )
where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Data.List
import Data.Loc
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Map.Strict as M

import Language.Futhark
import Language.Futhark.TypeChecker.Monad

-- | @t1 `unifyTypes` t2@ attempts to unify @t1@ and @t2@.  If
-- unification cannot happen, 'Nothing' is returned, otherwise a type
-- that combines the aliasing of @t1@ and @t2@ is returned.  The
-- uniqueness of the resulting type will be the least of the
-- uniqueness of @t1@ and @t2@.
unifyTypes :: (Monoid als, Eq als, ArrayDim dim) =>
              TypeBase dim als -> TypeBase dim als -> Maybe (TypeBase dim als)
unifyTypes = unifyTypesU $ \x y -> Just $ x <> y

unifyTypesU :: (Monoid als, Eq als, ArrayDim dim) =>
              (Uniqueness -> Uniqueness -> Maybe Uniqueness)
           -> TypeBase dim als -> TypeBase dim als -> Maybe (TypeBase dim als)
unifyTypesU _ (Prim t1) (Prim t2)
  | t1 == t2  = Just $ Prim t1
  | otherwise = Nothing
unifyTypesU uf (TypeVar t1 targs1) (TypeVar t2 targs2)
  | t1 == t2 = do
      targs3 <- zipWithM (unifyTypeArgs uf) targs1 targs2
      Just $ TypeVar t1 targs3
  | otherwise = Nothing
unifyTypesU uf (Array et1 shape1 u1) (Array et2 shape2 u2) =
  Array <$> unifyArrayElemTypes uf et1 et2 <*>
  unifyShapes shape1 shape2 <*> uf u1 u2
unifyTypesU uf (Record ts1) (Record ts2)
  | length ts1 == length ts2,
    sort (M.keys ts1) == sort (M.keys ts2) =
      Record <$> traverse (uncurry (unifyTypesU uf))
      (M.intersectionWith (,) ts1 ts2)
unifyTypesU uf (Arrow as1 mn1 t1 t1') (Arrow as2 _ t2 t2') =
  Arrow (as1 <> as2) mn1 <$> unifyTypesU uf t1 t2 <*> unifyTypesU uf t1' t2'
unifyTypesU _ _ _ = Nothing

unifyTypeArgs :: (Monoid als, Eq als, ArrayDim dim) =>
                 (Uniqueness -> Uniqueness -> Maybe Uniqueness)
              -> TypeArg dim als -> TypeArg dim als -> Maybe (TypeArg dim als)
unifyTypeArgs _ (TypeArgDim d1 loc) (TypeArgDim d2 _) =
  TypeArgDim <$> unifyDims d1 d2 <*> pure loc
unifyTypeArgs uf (TypeArgType t1 loc) (TypeArgType t2 _) =
  TypeArgType <$> unifyTypesU uf t1 t2 <*> pure loc
unifyTypeArgs _ _ _ =
  Nothing

unifyArrayElemTypes :: (Monoid als, Eq als, ArrayDim dim) =>
                       (Uniqueness -> Uniqueness -> Maybe Uniqueness)
                    -> ArrayElemTypeBase dim als
                    -> ArrayElemTypeBase dim als
                    -> Maybe (ArrayElemTypeBase dim als)
unifyArrayElemTypes _ (ArrayPrimElem bt1 als1) (ArrayPrimElem bt2 als2)
  | bt1 == bt2 =
      Just $ ArrayPrimElem bt1 (als1 <> als2)
unifyArrayElemTypes _ (ArrayPolyElem bt1 targs1 als1) (ArrayPolyElem bt2 targs2 als2)
  | bt1 == bt2, targs1 == targs2 =
      Just $ ArrayPolyElem bt1 targs1 (als1 <> als2)
unifyArrayElemTypes uf (ArrayRecordElem et1) (ArrayRecordElem et2)
  | sort (M.keys et1) == sort (M.keys et2) =
    ArrayRecordElem <$>
    traverse (uncurry $ unifyRecordArrayElemTypes uf) (M.intersectionWith (,) et1 et2)
unifyArrayElemTypes _ _ _ =
  Nothing

unifyRecordArrayElemTypes :: (Monoid als, Eq als, ArrayDim dim) =>
                             (Uniqueness -> Uniqueness -> Maybe Uniqueness)
                          -> RecordArrayElemTypeBase dim als
                          -> RecordArrayElemTypeBase dim als
                          -> Maybe (RecordArrayElemTypeBase dim als)
unifyRecordArrayElemTypes uf (RecordArrayElem et1) (RecordArrayElem et2) =
  RecordArrayElem <$> unifyArrayElemTypes uf et1 et2
unifyRecordArrayElemTypes uf (RecordArrayArrayElem et1 shape1 u1) (RecordArrayArrayElem et2 shape2 u2) =
  RecordArrayArrayElem <$> unifyArrayElemTypes uf et1 et2 <*>
  unifyShapes shape1 shape2 <*> uf u1 u2
unifyRecordArrayElemTypes _ _ _ =
  Nothing

-- | @x \`subtypeOf\` y@ is true if @x@ is a subtype of @y@ (or equal to
-- @y@), meaning @x@ is valid whenever @y@ is.
subtypeOf :: ArrayDim dim =>
             TypeBase dim as1 -> TypeBase dim as2 -> Bool
subtypeOf t1 t2 = isJust $ unifyTypesU unifyUniqueness (toStruct t1) (toStruct t2)
  where unifyUniqueness u2 u1 = if u2 `subuniqueOf` u1 then Just u1 else Nothing

-- | @x \`similarTo\` y@ is true if @x@ and @y@ are the same type,
-- ignoring uniqueness and aliasing.
similarTo :: ArrayDim dim =>
             TypeBase dim as1
          -> TypeBase dim as2
          -> Bool
similarTo t1 t2 = t1' `subtypeOf` t2' || t2' `subtypeOf` t1'
  where t1' = toStruct t1
        t2' = toStruct t2

-- | @x `subuniqueOf` y@ is true if @x@ is not less unique than @y@.
subuniqueOf :: Uniqueness -> Uniqueness -> Bool
subuniqueOf Nonunique Unique = False
subuniqueOf _ _              = True

data Bindage = BoundAsVar | UsedFree
             deriving (Show, Eq)

checkTypeDecl :: MonadTypeChecker m =>
                 TypeDeclBase NoInfo Name -> m (TypeDeclBase Info VName)
checkTypeDecl (TypeDecl t NoInfo) = do
  checkForDuplicateNamesInType t
  (t', st) <- checkTypeExp t
  return $ TypeDecl t' $ Info st

checkTypeExp :: MonadTypeChecker m =>
                TypeExp Name
             -> m (TypeExp VName, StructType)
checkTypeExp (TEVar name loc) = do
  (name', ps, t) <- lookupType loc name
  case ps of
    [] -> return (TEVar name' loc, t)
    _  -> throwError $ TypeError loc $
          "Type constructor " ++ pretty name ++ " used without any arguments."
checkTypeExp (TETuple ts loc) = do
  (ts', ts_s) <- unzip <$> mapM checkTypeExp ts
  return (TETuple ts' loc, tupleRecord ts_s)
checkTypeExp t@(TERecord fs loc) = do
  -- Check for duplicate field names.
  let field_names = map fst fs
  unless (sort field_names == sort (nub field_names)) $
    throwError $ TypeError loc $ "Duplicate record fields in " ++ pretty t

  fs_and_ts <- traverse checkTypeExp $ M.fromList fs
  let fs' = fmap fst fs_and_ts
      ts_s = fmap snd fs_and_ts
  return (TERecord (M.toList fs') loc, Record ts_s)
checkTypeExp (TEArray t d loc) = do
  (t', st) <- checkTypeExp t
  d' <- checkDimDecl d
  case arrayOf st (ShapeDecl [d']) Nonunique of
    Just st' -> return (TEArray t' d' loc, st')
    Nothing -> throwError $ TypeError loc $
               "Cannot create array with elements of type " ++ pretty st
  where checkDimDecl AnyDim =
          return AnyDim
        checkDimDecl (ConstDim k) =
          return $ ConstDim k
        checkDimDecl (NamedDim v) =
          NamedDim <$> checkNamedDim loc v
checkTypeExp (TEUnique t loc) = do
  (t', st) <- checkTypeExp t
  case st of
    Array{} -> return (t', st `setUniqueness` Unique)
    _       -> throwError $ InvalidUniqueness loc $ toStructural st
checkTypeExp (TEArrow (Just v) t1 t2 loc) = do
  (t1', st1) <- checkTypeExp t1
  bindSpaced [(Term, v)] $ do
    v' <- checkName Term v loc
    let env = mempty { envVtable = M.singleton v' $ BoundV [] st1 }
    localEnv env $ do
      (t2', st2) <- checkTypeExp t2
      return (TEArrow (Just v') t1' t2' loc,
              Arrow mempty (Just v') st1 st2)
checkTypeExp (TEArrow Nothing t1 t2 loc) = do
  (t1', st1) <- checkTypeExp t1
  (t2', st2) <- checkTypeExp t2
  return (TEArrow Nothing t1' t2' loc,
          Arrow mempty Nothing st1 st2)
checkTypeExp ote@TEApply{} = do
  (tname, tname_loc, targs) <- rootAndArgs ote
  (tname', ps, t) <- lookupType tloc tname
  if length ps /= length targs
  then throwError $ TypeError tloc $
       "Type constructor " ++ pretty tname ++ " requires " ++ show (length ps) ++
       " arguments, but application at " ++ locStr tloc ++ " provides " ++ show (length targs)
  else do
    (targs', substs) <- unzip <$> zipWithM checkArgApply ps targs
    return (foldl (\x y -> TEApply x y tloc)
            (TEVar tname' tname_loc) targs',
            substituteTypes (mconcat substs) t)
  where tloc = srclocOf ote

        rootAndArgs :: MonadTypeChecker m => TypeExp Name -> m (QualName Name, SrcLoc, [TypeArgExp Name])
        rootAndArgs (TEVar qn loc) = return (qn, loc, [])
        rootAndArgs (TEApply op arg _) = do (op', loc, args) <- rootAndArgs op
                                            return (op', loc, args++[arg])
        rootAndArgs te' = throwError $ TypeError (srclocOf te') $
                          "Type '" ++ pretty te' ++ "' is not a type constructor."

        checkArgApply (TypeParamDim pv _) (TypeArgExpDim (NamedDim v) loc) = do
          v' <- checkNamedDim loc v
          return (TypeArgExpDim (NamedDim v') loc,
                  M.singleton pv $ DimSub $ NamedDim v')
        checkArgApply (TypeParamDim pv _) (TypeArgExpDim (ConstDim x) loc) =
          return (TypeArgExpDim (ConstDim x) loc,
                  M.singleton pv $ DimSub $ ConstDim x)
        checkArgApply (TypeParamDim pv _) (TypeArgExpDim AnyDim loc) =
          return (TypeArgExpDim AnyDim loc,
                  M.singleton pv $ DimSub AnyDim)

        checkArgApply (TypeParamType pv _) (TypeArgExpType te) = do
          (te', st) <- checkTypeExp te
          return (TypeArgExpType te',
                  M.singleton pv $ TypeSub $ TypeAbbr [] st)

        checkArgApply (TypeParamLiftedType pv _) (TypeArgExpType te) = do
          (te', st) <- checkTypeExp te
          return (TypeArgExpType te',
                  M.singleton pv $ TypeSub $ TypeAbbr [] st)

        checkArgApply p a =
          throwError $ TypeError tloc $ "Type argument " ++ pretty a ++
          " not valid for a type parameter " ++ pretty p


checkNamedDim :: MonadTypeChecker m =>
                 SrcLoc -> QualName Name -> m (QualName VName)
checkNamedDim loc v = do
  (v', t) <- lookupVar loc v
  case t of
    Prim (Signed Int32) -> return v'
    _                   -> throwError $ DimensionNotInteger loc v

-- | Check for duplication of names inside a pattern group.  Produces
-- a description of all names used in the pattern group.
checkForDuplicateNames :: MonadTypeChecker m =>
                          [UncheckedPattern] -> m ()
checkForDuplicateNames = (`evalStateT` mempty) . mapM_ check
  where check (Id v _ loc) = seen v loc
        check (PatternParens p _) = check p
        check Wildcard{} = return ()
        check (TuplePattern ps _) = mapM_ check ps
        check (RecordPattern fs _) = mapM_ (check . snd) fs
        check (PatternAscription p _ _) = check p

        seen v loc = do
          already <- gets $ M.lookup v
          case already of
            Just prev_loc ->
              lift $ throwError $ TypeError loc $
              "Name " ++ pretty v ++ " also bound at " ++ locStr prev_loc
            Nothing ->
              modify $ M.insert v loc

-- | Check whether the type contains arrow types that define the same
-- parameter.  These might also exist further down, but that's not
-- really a problem - we mostly do this checking to help the user,
-- since it is likely an error, but it's easy to assign a semantics to
-- it (normal name shadowing).
checkForDuplicateNamesInType :: MonadTypeChecker m =>
                                TypeExp Name -> m ()
checkForDuplicateNamesInType = checkForDuplicateNames . pats
  where pats (TEArrow (Just v) t1 t2 loc) = Id v NoInfo loc : pats t1 ++ pats t2
        pats (TEArrow Nothing t1 t2 _) = pats t1 ++ pats t2
        pats (TETuple ts _) = concatMap pats ts
        pats (TERecord fs _) = concatMap (pats . snd) fs
        pats (TEArray t _ _) = pats t
        pats (TEUnique t _) = pats t
        pats (TEApply t1 (TypeArgExpType t2) _) = pats t1 ++ pats t2
        pats (TEApply t1 TypeArgExpDim{} _) = pats t1
        pats TEVar{} = []

checkTypeParams :: MonadTypeChecker m =>
                   [TypeParamBase Name]
                -> ([TypeParamBase VName] -> m a)
                -> m a
checkTypeParams ps m =
  bindSpaced (map typeParamSpace ps) $
  m =<< evalStateT (mapM checkTypeParam ps) mempty
  where typeParamSpace (TypeParamDim pv _) = (Term, pv)
        typeParamSpace (TypeParamType pv _) = (Type, pv)
        typeParamSpace (TypeParamLiftedType pv _) = (Type, pv)

        checkParamName ns v loc = do
          seen <- M.lookup (ns,v) <$> get
          case seen of
            Just prev ->
              throwError $ TypeError loc $
              "Type parameter " ++ pretty v ++ " previously defined at " ++ locStr prev
            Nothing -> do
              modify $ M.insert (ns,v) loc
              lift $ checkName ns v loc

        checkTypeParam (TypeParamDim pv loc) =
          TypeParamDim <$> checkParamName Term pv loc <*> pure loc
        checkTypeParam (TypeParamType pv loc) =
          TypeParamType <$> checkParamName Type pv loc <*> pure loc
        checkTypeParam (TypeParamLiftedType pv loc) =
          TypeParamLiftedType <$> checkParamName Type pv loc <*> pure loc

data TypeSub = TypeSub TypeBinding
             | DimSub (DimDecl VName)
             deriving (Show)

type TypeSubs = M.Map VName TypeSub

substituteTypes :: TypeSubs -> StructType -> StructType
substituteTypes substs ot = case ot of
  Array at shape u ->
    fromMaybe nope $ arrayOf (substituteTypesInArrayElem at) (substituteInShape shape) u
  Prim t -> Prim t
  TypeVar v targs
    | Just (TypeSub (TypeAbbr ps t)) <-
        M.lookup (qualLeaf (qualNameFromTypeName v)) substs ->
        applyType ps t $ map substituteInTypeArg targs
    | otherwise -> TypeVar v $ map substituteInTypeArg targs
  Record ts ->
    Record $ fmap (substituteTypes substs) ts
  Arrow als v t1 t2 ->
    Arrow als v (substituteTypes substs t1) (substituteTypes substs t2)
  where nope = error "substituteTypes: Cannot create array after substitution."

        substituteTypesInArrayElem (ArrayPrimElem t ()) =
          Prim t
        substituteTypesInArrayElem (ArrayPolyElem v targs ())
          | Just (TypeSub (TypeAbbr ps t)) <-
              M.lookup (qualLeaf (qualNameFromTypeName v)) substs =
              applyType ps t (map substituteInTypeArg targs)
          | otherwise =
              TypeVar v (map substituteInTypeArg targs)
        substituteTypesInArrayElem (ArrayRecordElem ts) =
          Record ts'
          where ts' = fmap (substituteTypes substs .
                            fst . recordArrayElemToType) ts

        substituteInTypeArg (TypeArgDim d loc) =
          TypeArgDim (substituteInDim d) loc
        substituteInTypeArg (TypeArgType t loc) =
          TypeArgType (substituteTypes substs t) loc

        substituteInShape (ShapeDecl ds) =
          ShapeDecl $ map substituteInDim ds

        substituteInDim (NamedDim v)
          | Just (DimSub d) <- M.lookup (qualLeaf v) substs = d
        substituteInDim d = d

substituteTypesInBoundV :: TypeSubs -> BoundV -> BoundV
substituteTypesInBoundV substs (BoundV tps t) =
  BoundV tps (substituteTypes substs t)

applyType :: [TypeParam] -> StructType -> [StructTypeArg] -> StructType
applyType ps t args =
  substituteTypes substs t
  where substs = M.fromList $ zipWith mkSubst ps args
        -- We are assuming everything has already been type-checked for correctness.
        mkSubst (TypeParamDim pv _) (TypeArgDim (NamedDim v) _) =
          (pv, DimSub $ NamedDim v)
        mkSubst (TypeParamDim pv _) (TypeArgDim (ConstDim x) _) =
          (pv, DimSub $ ConstDim x)
        mkSubst (TypeParamDim pv _) (TypeArgDim AnyDim  _) =
          (pv, DimSub AnyDim)
        mkSubst (TypeParamType pv _) (TypeArgType at _) =
          (pv, TypeSub $ TypeAbbr [] at)
        mkSubst p a =
          error $ "applyType mkSubst: cannot substitute " ++ pretty a ++ " for " ++ pretty p

type InstantiateM = StateT
                    (M.Map VName (TypeBase () (),SrcLoc))
                    (Either (Maybe String))

instantiatePolymorphic :: [VName] -> SrcLoc -> M.Map VName (TypeBase () (),SrcLoc)
                       -> TypeBase () () -> TypeBase () ()
                       -> Either (Maybe String) (M.Map VName (TypeBase () (),SrcLoc))
instantiatePolymorphic tnames loc orig_substs x y =
  execStateT (instantiate x y) orig_substs
  where

    instantiate :: TypeBase () () -> TypeBase () ()
                -> InstantiateM ()
    instantiate (TypeVar (TypeName [] tn) []) orig_arg_t
      | tn `elem` tnames = do
          substs <- get
          case M.lookup tn substs of
            Just (old_arg_t, old_arg_loc) | old_arg_t /= orig_arg_t ->
              lift $ Left $ Just $ "Argument determines type parameter '" ++
              pretty (baseName tn) ++ "' as " ++ pretty arg_t ++
              ", but previously determined as " ++ pretty old_arg_t ++
              " at " ++ locStr old_arg_loc
            _ -> modify $ M.insert tn (arg_t, loc)
            -- Ignore uniqueness when dealing with type variables.
            where arg_t = orig_arg_t `setUniqueness` Nonunique
    instantiate (TypeVar (TypeName _ tn) targs)
                (TypeVar (TypeName _ arg_tn) arg_targs)
      | tn == arg_tn, length targs == length arg_targs =
          zipWithM_ instantiateTypeArg targs arg_targs
    instantiate (Record fs) (Record arg_fs)
      | M.keys fs == M.keys arg_fs =
        mapM_ (uncurry instantiate) $ M.intersectionWith (,) fs arg_fs
    instantiate (Array et shape u) arg_t@(Array _ _ p_u)
      | Just arg_t' <- peelArray (shapeRank shape) arg_t,
        p_u `subuniqueOf` u =
          instantiateArrayElemType et arg_t'
    instantiate (Prim pt) (Prim p_pt)
      | pt == p_pt = return ()
    instantiate (Arrow () _ t1 t2) (Arrow () _ t1' t2') =
      instantiate t1 t1' >> instantiate t2 t2'
    instantiate _ _ =
      lift $ Left Nothing

    instantiateArrayElemType (ArrayPrimElem pt ()) (Prim arg_pt)
      | pt == arg_pt =
          return ()
    instantiateArrayElemType (ArrayRecordElem fs) (Record arg_fs)
      | M.keys fs == M.keys arg_fs =
          mapM_ (uncurry instantiateRecordArrayElemType) $
          M.intersectionWith (,) fs arg_fs
    instantiateArrayElemType (ArrayPolyElem tn [] ()) arg_t =
      instantiate (TypeVar tn []) arg_t
    instantiateArrayElemType _ _ =
      lift $ Left Nothing

    instantiateRecordArrayElemType (RecordArrayElem et) arg_t =
      instantiateArrayElemType et arg_t
    instantiateRecordArrayElemType (RecordArrayArrayElem et shape u) arg_t =
      instantiate (Array et shape u) arg_t

    instantiateTypeArg TypeArgDim{} TypeArgDim{} =
      return ()
    instantiateTypeArg (TypeArgType t _) (TypeArgType arg_t _) =
      instantiate t arg_t
    instantiateTypeArg _ _ =
      lift $ Left Nothing

-- | Class of types which allow for substitution of types with no
-- annotations for type variable names.
class Substitutable a where
  applySubst :: M.Map VName (TypeBase () ()) -> a -> a

instance Substitutable (TypeBase () ()) where
  applySubst = substTypesAny

instance Substitutable (TypeBase () Names) where
  applySubst = substTypesAny . M.map fromStruct

instance Substitutable (TypeBase (DimDecl VName) ()) where
  applySubst = substTypesAny . M.map vacuousShapeAnnotations

instance Substitutable (TypeBase (DimDecl VName) Names) where
  applySubst = substTypesAny . M.map (vacuousShapeAnnotations . fromStruct)

-- | Perform substitutions, from type names to types, on a type. Works
-- regardless of what shape and uniqueness information is attached to the type.
substTypesAny :: (ArrayDim dim, Monoid as) =>
                 M.Map VName (TypeBase dim as)
              -> TypeBase dim as -> TypeBase dim as
substTypesAny substs ot = case ot of
  Prim t -> Prim t
  Array et shape u -> fromMaybe nope $
                      uncurry arrayOfWithAliases (subsArrayElem et) shape u
  -- We only substitute for a type variable with no arguments, since
  -- type parameters cannot have higher kind.
  TypeVar v []
    | Just t <- M.lookup (qualLeaf (qualNameFromTypeName v)) substs -> t
  TypeVar v targs -> TypeVar v $ map subsTypeArg targs
  Record ts ->  Record $ fmap (substTypesAny substs) ts
  Arrow als v t1 t2 ->
    Arrow als v (substTypesAny substs t1) (substTypesAny substs t2)

  where nope = error "substTypesAny: Cannot create array after substitution."

        subsArrayElem (ArrayPrimElem t as) = (Prim t, as)
        subsArrayElem (ArrayPolyElem v [] as)
          | Just t <- M.lookup (qualLeaf (qualNameFromTypeName v)) substs = (t, as)
        subsArrayElem (ArrayPolyElem v targs as) =
          (TypeVar v (map subsTypeArg targs), as)
        subsArrayElem (ArrayRecordElem ts) =
          let ts' = fmap recordArrayElemToType ts
          in (Record $ fmap (substTypesAny substs . fst) ts', foldMap snd ts')

        subsTypeArg (TypeArgType t loc) =
          TypeArgType (substTypesAny substs t) loc
        subsTypeArg t = t
