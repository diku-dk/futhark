{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Language.Futhark.TypeChecker.Types
  ( checkTypeExp
  , checkTypeDecl

  , unifyTypesU
  , subtypeOf
  , subuniqueOf

  , checkForDuplicateNames
  , checkTypeParams

  , TypeSub(..)
  , TypeSubs
  , substituteTypes
  , substituteTypesInBoundV

  , Subst(..)
  , Substitutable(..)
  , substTypesAny
  )
where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.List
import Data.Loc
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Map.Strict as M

import Language.Futhark
import Language.Futhark.TypeChecker.Monad

-- | @unifyTypes uf t2 t2@ attempts to unify @t1@ and @t2@.  If
-- unification cannot happen, 'Nothing' is returned, otherwise a type
-- that combines the aliasing of @t1@ and @t2@ is returned.
-- Uniqueness is unified with @uf@.
unifyTypesU :: (Monoid als, Eq als, ArrayDim dim) =>
              (Uniqueness -> Uniqueness -> Maybe Uniqueness)
           -> TypeBase dim als -> TypeBase dim als -> Maybe (TypeBase dim als)
unifyTypesU _ (Prim t1) (Prim t2)
  | t1 == t2  = Just $ Prim t1
  | otherwise = Nothing
unifyTypesU uf (TypeVar als1 u1 t1 targs1) (TypeVar als2 u2 t2 targs2)
  | t1 == t2 = do
      u3 <- uf u1 u2
      targs3 <- zipWithM (unifyTypeArgs uf) targs1 targs2
      Just $ TypeVar (als1 <> als2) u3 t1 targs3
  | otherwise = Nothing
unifyTypesU uf (Array als1 u1 et1 shape1) (Array als2 u2 et2 shape2) =
  Array (als1 <> als2) <$> uf u1 u2
  <*> unifyArrayElemTypes uf et1 et2 <*> unifyShapes shape1 shape2
unifyTypesU uf (Record ts1) (Record ts2)
  | length ts1 == length ts2,
    sort (M.keys ts1) == sort (M.keys ts2) =
      Record <$> traverse (uncurry (unifyTypesU uf))
      (M.intersectionWith (,) ts1 ts2)
unifyTypesU uf (Arrow as1 mn1 t1 t1') (Arrow as2 _ t2 t2') =
  Arrow (as1 <> as2) mn1 <$> unifyTypesU (flip uf) t1 t2 <*> unifyTypesU uf t1' t2'
unifyTypesU _ e1@Enum{} e2@Enum{}
  | e1 == e2 = Just e1
unifyTypesU _ _ _ = Nothing

unifyTypeArgs :: (ArrayDim dim) =>
                 (Uniqueness -> Uniqueness -> Maybe Uniqueness)
              -> TypeArg dim -> TypeArg dim -> Maybe (TypeArg dim)
unifyTypeArgs _ (TypeArgDim d1 loc) (TypeArgDim d2 _) =
  TypeArgDim <$> unifyDims d1 d2 <*> pure loc
unifyTypeArgs uf (TypeArgType t1 loc) (TypeArgType t2 _) =
  TypeArgType <$> unifyTypesU uf t1 t2 <*> pure loc
unifyTypeArgs _ _ _ =
  Nothing

unifyArrayElemTypes :: (ArrayDim dim) =>
                       (Uniqueness -> Uniqueness -> Maybe Uniqueness)
                    -> ArrayElemTypeBase dim
                    -> ArrayElemTypeBase dim
                    -> Maybe (ArrayElemTypeBase dim)
unifyArrayElemTypes _ (ArrayPrimElem bt1) (ArrayPrimElem bt2)
  | bt1 == bt2 =
      Just $ ArrayPrimElem bt1
unifyArrayElemTypes _ (ArrayPolyElem bt1 targs1) (ArrayPolyElem bt2 targs2)
  | bt1 == bt2, targs1 == targs2 =
      Just $ ArrayPolyElem bt1 targs1
unifyArrayElemTypes uf (ArrayRecordElem et1) (ArrayRecordElem et2)
  | sort (M.keys et1) == sort (M.keys et2) =
    ArrayRecordElem <$>
    traverse (uncurry $ unifyRecordArrayElemTypes uf) (M.intersectionWith (,) et1 et2)
unifyArrayElemTypes _ (ArrayEnumElem cs1) (ArrayEnumElem cs2)
  | cs1 == cs2 =
     Just $ ArrayEnumElem cs1
unifyArrayElemTypes _ _ _ =
  Nothing

unifyRecordArrayElemTypes :: (ArrayDim dim) =>
                             (Uniqueness -> Uniqueness -> Maybe Uniqueness)
                          -> RecordArrayElemTypeBase dim
                          -> RecordArrayElemTypeBase dim
                          -> Maybe (RecordArrayElemTypeBase dim)
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

-- | @x `subuniqueOf` y@ is true if @x@ is not less unique than @y@.
subuniqueOf :: Uniqueness -> Uniqueness -> Bool
subuniqueOf Nonunique Unique = False
subuniqueOf _ _              = True

data Bindage = BoundAsVar | UsedFree
             deriving (Show, Eq)

checkTypeDecl :: MonadTypeChecker m =>
                 TypeDeclBase NoInfo Name
              -> m (TypeDeclBase Info VName, Liftedness)
checkTypeDecl (TypeDecl t NoInfo) = do
  checkForDuplicateNamesInType t
  (t', st, l) <- checkTypeExp t
  return (TypeDecl t' $ Info st, l)

checkTypeExp :: MonadTypeChecker m =>
                TypeExp Name
             -> m (TypeExp VName, StructType, Liftedness)
checkTypeExp (TEVar name loc) = do
  (name', ps, t, l) <- lookupType loc name
  case ps of
    [] -> return (TEVar name' loc, t, l)
    _  -> throwError $ TypeError loc $
          "Type constructor " ++ pretty name ++ " used without any arguments."
checkTypeExp (TETuple ts loc) = do
  (ts', ts_s, ls) <- unzip3 <$> mapM checkTypeExp ts
  return (TETuple ts' loc, tupleRecord ts_s, foldl' max Unlifted ls)
checkTypeExp t@(TERecord fs loc) = do
  -- Check for duplicate field names.
  let field_names = map fst fs
  unless (sort field_names == sort (nub field_names)) $
    throwError $ TypeError loc $ "Duplicate record fields in " ++ pretty t

  fs_ts_ls <- traverse checkTypeExp $ M.fromList fs
  let fs' = fmap (\(x,_,_) -> x) fs_ts_ls
      ts_s = fmap (\(_,y,_) -> y) fs_ts_ls
      ls = fmap (\(_,_,z) -> z) fs_ts_ls
  return (TERecord (M.toList fs') loc, Record ts_s, foldl' max Unlifted ls)
checkTypeExp (TEArray t d loc) = do
  (t', st, l) <- checkTypeExp t
  d' <- checkDimDecl d
  case (l, arrayOf st (ShapeDecl [d']) Nonunique) of
    (Unlifted, Just st') -> return (TEArray t' d' loc, st', Unlifted)
    _ -> throwError $ TypeError loc $
         "Cannot create array with elements of type `" ++ pretty st ++ "` (might be functional)."
  where checkDimDecl AnyDim =
          return AnyDim
        checkDimDecl (ConstDim k) =
          return $ ConstDim k
        checkDimDecl (NamedDim v) =
          NamedDim <$> checkNamedDim loc v
checkTypeExp (TEUnique t loc) = do
  (t', st, l) <- checkTypeExp t
  unless (mayContainArray st) $
    warn loc $ "Declaring `" <> pretty st <> "` as unique has no effect."
  return (TEUnique t' loc, st `setUniqueness` Unique, l)
  where mayContainArray Prim{} = False
        mayContainArray Array{} = True
        mayContainArray (Record fs) = any mayContainArray fs
        mayContainArray TypeVar{} = True
        mayContainArray Arrow{} = False
        mayContainArray Enum{} = False
checkTypeExp (TEArrow (Just v) t1 t2 loc) = do
  (t1', st1, _) <- checkTypeExp t1
  bindSpaced [(Term, v)] $ do
    v' <- checkName Term v loc
    let env = mempty { envVtable = M.singleton v' $ BoundV [] st1 }
    localEnv env $ do
      (t2', st2, _) <- checkTypeExp t2
      return (TEArrow (Just v') t1' t2' loc,
              Arrow mempty (Just v') st1 st2,
              Lifted)
checkTypeExp (TEArrow Nothing t1 t2 loc) = do
  (t1', st1, _) <- checkTypeExp t1
  (t2', st2, _) <- checkTypeExp t2
  return (TEArrow Nothing t1' t2' loc,
          Arrow mempty Nothing st1 st2,
          Lifted)
checkTypeExp ote@TEApply{} = do
  (tname, tname_loc, targs) <- rootAndArgs ote
  (tname', ps, t, l) <- lookupType tloc tname
  if length ps /= length targs
  then throwError $ TypeError tloc $
       "Type constructor " ++ pretty tname ++ " requires " ++ show (length ps) ++
       " arguments, but application at " ++ locStr tloc ++ " provides " ++ show (length targs)
  else do
    (targs', substs) <- unzip <$> zipWithM checkArgApply ps targs
    return (foldl (\x y -> TEApply x y tloc) (TEVar tname' tname_loc) targs',
            substituteTypes (mconcat substs) t,
            l)
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

        checkArgApply (TypeParamType l pv _) (TypeArgExpType te) = do
          (te', st, _) <- checkTypeExp te
          return (TypeArgExpType te',
                  M.singleton pv $ TypeSub $ TypeAbbr l [] st)

        checkArgApply p a =
          throwError $ TypeError tloc $ "Type argument " ++ pretty a ++
          " not valid for a type parameter " ++ pretty p

checkTypeExp t@(TEEnum names loc) = do
  unless (sort names == sort (nub names)) $
    throwError $ TypeError loc $ "Duplicate constructors in " ++ pretty t
  unless (length names <= 256) $
    throwError $ TypeError loc "Enums must have 256 or fewer constructors."
  return (TEEnum names loc, Enum names,  Unlifted)

checkNamedDim :: MonadTypeChecker m =>
                 SrcLoc -> QualName Name -> m (QualName VName)
checkNamedDim loc v = do
  (v', t) <- lookupVar loc v
  case t of
    Prim (Signed Int32) -> return v'
    _                   -> throwError $ TypeError loc $
                           "Dimension declaration " ++ pretty v ++
                           " should be of type `i32`."

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
        check PatternLit{} = return ()

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
        pats TEEnum{} = []

checkTypeParams :: MonadTypeChecker m =>
                   [TypeParamBase Name]
                -> ([TypeParamBase VName] -> m a)
                -> m a
checkTypeParams ps m =
  bindSpaced (map typeParamSpace ps) $
  m =<< evalStateT (mapM checkTypeParam ps) mempty
  where typeParamSpace (TypeParamDim pv _) = (Term, pv)
        typeParamSpace (TypeParamType _ pv _) = (Type, pv)

        checkParamName ns v loc = do
          seen <- gets $ M.lookup (ns,v)
          case seen of
            Just prev ->
              throwError $ TypeError loc $
              "Type parameter " ++ pretty v ++ " previously defined at " ++ locStr prev
            Nothing -> do
              modify $ M.insert (ns,v) loc
              lift $ checkName ns v loc

        checkTypeParam (TypeParamDim pv loc) =
          TypeParamDim <$> checkParamName Term pv loc <*> pure loc
        checkTypeParam (TypeParamType l pv loc) =
          TypeParamType l <$> checkParamName Type pv loc <*> pure loc

data TypeSub = TypeSub TypeBinding
             | DimSub (DimDecl VName)
             deriving (Show)

type TypeSubs = M.Map VName TypeSub

substituteTypes :: TypeSubs -> StructType -> StructType
substituteTypes substs ot = case ot of
  Array als u at shape ->
    fromMaybe nope $ arrayOfWithAliases (substituteTypesInArrayElem at) als (substituteInShape shape) u
  Prim t -> Prim t
  TypeVar () u v targs
    | Just (TypeSub (TypeAbbr _ ps t)) <-
        M.lookup (qualLeaf (qualNameFromTypeName v)) substs ->
        applyType ps t (map substituteInTypeArg targs)
        `setUniqueness` u
    | otherwise -> TypeVar () u v $ map substituteInTypeArg targs
  Record ts ->
    Record $ fmap (substituteTypes substs) ts
  Arrow als v t1 t2 ->
    Arrow als v (substituteTypes substs t1) (substituteTypes substs t2)
  Enum cs -> Enum cs
  where nope = error "substituteTypes: Cannot create array after substitution."

        substituteTypesInArrayElem (ArrayPrimElem t) =
          Prim t
        substituteTypesInArrayElem (ArrayPolyElem v targs)
          | Just (TypeSub (TypeAbbr _ ps t)) <-
              M.lookup (qualLeaf (qualNameFromTypeName v)) substs =
              applyType ps t (map substituteInTypeArg targs)
          | otherwise =
              TypeVar () Nonunique v (map substituteInTypeArg targs)
        substituteTypesInArrayElem (ArrayRecordElem ts) =
          Record ts'
          where ts' = fmap (substituteTypes substs . recordArrayElemToType) ts
        substituteTypesInArrayElem (ArrayEnumElem cs) =
          Enum cs

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
        mkSubst (TypeParamType l pv _) (TypeArgType at _) =
          (pv, TypeSub $ TypeAbbr l [] at)
        mkSubst p a =
          error $ "applyType mkSubst: cannot substitute " ++ pretty a ++ " for " ++ pretty p

-- | A type substituion may be a substitution or a yet-unknown
-- substitution (but which is certainly an overloaded primitive
-- type!).  The latter is used to remove aliases from types that are
-- yet-unknown but that we know cannot carry aliases (see issue #682).
data Subst t = Subst t | PrimSubst

instance Functor Subst where
  fmap f (Subst t) = Subst $ f t
  fmap _ PrimSubst = PrimSubst

-- | Class of types which allow for substitution of types with no
-- annotations for type variable names.
class Substitutable a where
  applySubst :: (VName -> Maybe (Subst (TypeBase () ()))) -> a -> a

instance Substitutable (TypeBase () ()) where
  applySubst = substTypesAny

instance Substitutable (TypeBase () Names) where
  applySubst = substTypesAny . (fmap (fmap fromStruct).)

instance Substitutable (TypeBase (DimDecl VName) ()) where
  applySubst = substTypesAny . (fmap (fmap vacuousShapeAnnotations).)

instance Substitutable (TypeBase (DimDecl VName) Names) where
  applySubst = substTypesAny . (fmap (fmap (vacuousShapeAnnotations . fromStruct)).)

-- | Perform substitutions, from type names to types, on a type. Works
-- regardless of what shape and uniqueness information is attached to the type.
substTypesAny :: (ArrayDim dim, Monoid as) =>
                 (VName -> Maybe (Subst (TypeBase dim as)))
              -> TypeBase dim as -> TypeBase dim as
substTypesAny lookupSubst ot = case ot of
  Prim t -> Prim t
  Array als u et shape ->
    fromMaybe nope $
    arrayOfWithAliases (subsArrayElem et) als shape u
  -- We only substitute for a type variable with no arguments, since
  -- type parameters cannot have higher kind.
  TypeVar als u v targs ->
    case lookupSubst $ qualLeaf (qualNameFromTypeName v) of
      Just (Subst t) -> t `setUniqueness` u
      Just PrimSubst -> TypeVar mempty u v $ map subsTypeArg targs
      Nothing -> TypeVar als u v $ map subsTypeArg targs
  Record ts ->  Record $ fmap (substTypesAny lookupSubst) ts
  Arrow als v t1 t2 ->
    Arrow als v (substTypesAny lookupSubst t1) (substTypesAny lookupSubst t2)
  Enum names -> Enum names

  where nope = error "substTypesAny: Cannot create array after substitution."

        subsArrayElem (ArrayPrimElem t) = Prim t
        subsArrayElem (ArrayPolyElem v targs) =
          case lookupSubst $ qualLeaf $ qualNameFromTypeName v of
            Just (Subst t) -> t
            -- It is intentional that we do not handle PrimSubst
            -- specially here, as we are inside an array, and that
            -- gives the aliasing.
            _ -> TypeVar mempty Nonunique v $ map subsTypeArg targs
        subsArrayElem (ArrayRecordElem ts) =
          Record $ substTypesAny lookupSubst . recordArrayElemToType <$> ts
        subsArrayElem (ArrayEnumElem cs) = Enum cs

        subsTypeArg (TypeArgType t loc) =
          TypeArgType (substTypesAny lookupSubst' t) loc
          where lookupSubst' = fmap (fmap $ bimap id (const ())) . lookupSubst
        subsTypeArg t = t
