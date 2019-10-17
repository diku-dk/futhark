{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Language.Futhark.TypeChecker.Types
  ( checkTypeExp
  , checkTypeDecl

  , unifyTypesU
  , subtypeOf
  , subuniqueOf

  , checkForDuplicateNames
  , checkForDuplicateNamesInType
  , checkTypeParams
  , typeParamToArg

  , typeExpUses
  , checkShapeParamUses

  , TypeSub(..)
  , TypeSubs
  , substituteTypes

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
import qualified Data.Map.Strict as M

import Language.Futhark
import Language.Futhark.TypeChecker.Monad

-- | @unifyTypes uf t1 t2@ attempts to unify @t1@ and @t2@.  If
-- unification cannot happen, 'Nothing' is returned, otherwise a type
-- that combines the aliasing of @t1@ and @t2@ is returned.
-- Uniqueness is unified with @uf@.
unifyTypesU :: (Monoid als, ArrayDim dim) =>
               (Uniqueness -> Uniqueness -> Maybe Uniqueness)
            -> TypeBase dim als -> TypeBase dim als -> Maybe (TypeBase dim als)
unifyTypesU uf (Array als1 u1 et1 shape1) (Array als2 u2 et2 shape2) =
  Array (als1 <> als2) <$> uf u1 u2
  <*> unifyScalarTypes uf et1 et2 <*> unifyShapes shape1 shape2
unifyTypesU uf (Scalar t1) (Scalar t2) = Scalar <$> unifyScalarTypes uf t1 t2
unifyTypesU _ _ _ = Nothing

unifyScalarTypes :: (Monoid als, ArrayDim dim) =>
                    (Uniqueness -> Uniqueness -> Maybe Uniqueness)
                 -> ScalarTypeBase dim als -> ScalarTypeBase dim als -> Maybe (ScalarTypeBase dim als)
unifyScalarTypes _ (Prim t1) (Prim t2)
  | t1 == t2  = Just $ Prim t1
  | otherwise = Nothing
unifyScalarTypes uf (TypeVar als1 u1 t1 targs1) (TypeVar als2 u2 t2 targs2)
  | t1 == t2 = do
      u3 <- uf u1 u2
      targs3 <- zipWithM (unifyTypeArgs uf) targs1 targs2
      Just $ TypeVar (als1 <> als2) u3 t1 targs3
  | otherwise = Nothing
unifyScalarTypes uf (Record ts1) (Record ts2)
  | length ts1 == length ts2,
    sort (M.keys ts1) == sort (M.keys ts2) =
      Record <$> traverse (uncurry (unifyTypesU uf))
      (M.intersectionWith (,) ts1 ts2)
unifyScalarTypes uf (Arrow as1 mn1 t1 t1') (Arrow as2 _ t2 t2') =
  Arrow (as1 <> as2) mn1 <$> unifyTypesU (flip uf) t1 t2 <*> unifyTypesU uf t1' t2'
unifyScalarTypes uf (Sum cs1) (Sum cs2)
  | length cs1 == length cs2,
    sort (M.keys cs1) == sort (M.keys cs2) =
      Sum <$> traverse (uncurry (zipWithM (unifyTypesU uf)))
      (M.intersectionWith (,) cs1 cs2)
unifyScalarTypes _ _ _ = Nothing

unifyTypeArgs :: (ArrayDim dim) =>
                 (Uniqueness -> Uniqueness -> Maybe Uniqueness)
              -> TypeArg dim -> TypeArg dim -> Maybe (TypeArg dim)
unifyTypeArgs _ (TypeArgDim d1 loc) (TypeArgDim d2 _) =
  TypeArgDim <$> unifyDims d1 d2 <*> pure loc
unifyTypeArgs uf (TypeArgType t1 loc) (TypeArgType t2 _) =
  TypeArgType <$> unifyTypesU uf t1 t2 <*> pure loc
unifyTypeArgs _ _ _ =
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

checkTypeDecl :: MonadTypeChecker m =>
                 [TypeParam]
              -> TypeDeclBase NoInfo Name
              -> m (TypeDeclBase Info VName, Liftedness)
checkTypeDecl tps (TypeDecl t NoInfo) = do
  checkForDuplicateNamesInType t
  (t', st, l) <- checkTypeExp t
  checkShapeParamUses typeExpUses tps $ unfoldTypeExp t'
  return (TypeDecl t' $ Info st, l)

unfoldTypeExp :: TypeExp VName -> [TypeExp VName]
unfoldTypeExp (TEArrow _ t1 t2 _) = t1 : unfoldTypeExp t2
unfoldTypeExp t = [t]

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
  return (TERecord (M.toList fs') loc,
          Scalar $ Record ts_s,
          foldl' max Unlifted ls)
checkTypeExp (TEArray t d loc) = do
  (t', st, l) <- checkTypeExp t
  d' <- checkDimDecl d
  case (l, arrayOf st (ShapeDecl [d']) Nonunique) of
    (Unlifted, st') -> return (TEArray t' d' loc, st', Unlifted)
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
  where mayContainArray (Scalar Prim{}) = False
        mayContainArray Array{} = True
        mayContainArray (Scalar (Record fs)) = any mayContainArray fs
        mayContainArray (Scalar TypeVar{}) = True
        mayContainArray (Scalar Arrow{}) = False
        mayContainArray (Scalar (Sum cs)) = (any . any) mayContainArray cs
checkTypeExp (TEArrow (Just v) t1 t2 loc) = do
  (t1', st1, _) <- checkTypeExp t1
  bindSpaced [(Term, v)] $ do
    v' <- checkName Term v loc
    bindVal v' (BoundV [] st1) $ do
      (t2', st2, _) <- checkTypeExp t2
      return (TEArrow (Just v') t1' t2' loc,
              Scalar $ Arrow mempty (Named v') st1 st2,
              Lifted)
checkTypeExp (TEArrow Nothing t1 t2 loc) = do
  (t1', st1, _) <- checkTypeExp t1
  (t2', st2, _) <- checkTypeExp t2
  return (TEArrow Nothing t1' t2' loc,
          Scalar $ Arrow mempty Unnamed st1 st2,
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

checkTypeExp t@(TESum cs loc) = do
  let constructors = map fst cs
  unless (sort constructors == sort (nub constructors)) $
    throwError $ TypeError loc $ "Duplicate constructors in " ++ pretty t

  unless (length constructors <= 256) $
    throwError $ TypeError loc "Sum types must have 256 or fewer constructors."

  cs_ts_ls <- (traverse . traverse) checkTypeExp $ M.fromList cs
  let cs'  = (fmap . fmap) (\(x,_,_) -> x) cs_ts_ls
      ts_s = (fmap . fmap) (\(_, y, _) -> y) cs_ts_ls
      ls   = (concatMap . fmap) (\(_, _, z) -> z) cs_ts_ls
  return (TESum (M.toList cs') loc,
          Scalar $ Sum ts_s,
          foldl' max Unlifted ls)

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
        check (PatternConstr _ _ ps _) = mapM_ check ps

        seen v loc = do
          already <- gets $ M.lookup v
          case already of
            Just prev_loc ->
              lift $ throwError $ TypeError loc $
              "Name " ++ quote (pretty v) ++ " also bound at " ++ locStr prev_loc
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
        pats (TESum cs _) = concatMap (concatMap pats . snd) cs

-- | Ensure that every shape parameter is used in positive position at
-- least once before being used in negative position.
checkShapeParamUses :: (MonadTypeChecker m, Located a) =>
                       (a -> ([VName], [VName])) -> [TypeParam] -> [a]
                    -> m ()
checkShapeParamUses getUses tps ps = do
  pos_uses <- foldM checkShapePositions [] ps
  mapM_ (checkUsed pos_uses) tps
  where tp_names = map typeParamName tps

        checkShapePositions pos_uses p = do
          let (pos, neg) = getUses p
              pos_uses' = pos <> pos_uses
          forM_ neg $ \pv ->
            unless ((pv `notElem` tp_names) || (pv `elem` pos_uses')) $
            throwError $ TypeError (srclocOf p) $ "Shape parameter `" ++
            pretty (baseName pv) ++ "` must first be given in " ++
            "a positive position (non-functional parameter)."
          return pos_uses'
        checkUsed uses (TypeParamDim pv loc)
          | pv `elem` uses = return ()
          | otherwise =
              throwError $ TypeError loc $ "Size parameter `" ++
              pretty (baseName pv) ++ "` unused."
        checkUsed _ _ = return ()

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
              "Type parameter " ++ quote (pretty v) ++
              " previously defined at " ++ locStr prev ++ "."
            Nothing -> do
              modify $ M.insert (ns,v) loc
              lift $ checkName ns v loc

        checkTypeParam (TypeParamDim pv loc) =
          TypeParamDim <$> checkParamName Term pv loc <*> pure loc
        checkTypeParam (TypeParamType l pv loc) =
          TypeParamType l <$> checkParamName Type pv loc <*> pure loc

-- | Construct a type argument corresponding to a type parameter.
typeParamToArg :: TypeParam -> StructTypeArg
typeParamToArg (TypeParamDim v ploc) =
  TypeArgDim (NamedDim $ qualName v) ploc
typeParamToArg (TypeParamType _ v ploc) =
  TypeArgType (Scalar (TypeVar () Nonunique (typeName v) [])) ploc

-- | Return the shapes used in a given type expression in positive and negative
-- position, respectively.
typeExpUses :: TypeExp VName -> ([VName], [VName])
typeExpUses (TEVar _ _) = mempty
typeExpUses (TETuple tes _) = foldMap typeExpUses tes
typeExpUses (TERecord fs _) = foldMap (typeExpUses . snd) fs
typeExpUses (TEArray te d _) = typeExpUses te <> dimDeclUses d
typeExpUses (TEUnique te _) = typeExpUses te
typeExpUses (TEApply te targ _) = typeExpUses te <> typeArgUses targ
  where typeArgUses (TypeArgExpDim d _) = dimDeclUses d
        typeArgUses (TypeArgExpType tae) = typeExpUses tae
typeExpUses (TEArrow _ t1 t2 _) =
  let (pos, neg) = typeExpUses t1 <> typeExpUses t2
  in (mempty, pos <> neg)
typeExpUses (TESum cs _) = foldMap (mconcat . fmap typeExpUses . snd) cs

dimDeclUses :: DimDecl VName -> ([VName], [VName])
dimDeclUses (NamedDim v) = ([qualLeaf v], [])
dimDeclUses _ = mempty

data TypeSub = TypeSub TypeBinding
             | DimSub (DimDecl VName)
             deriving (Show)

type TypeSubs = M.Map VName TypeSub

substituteTypes :: Monoid als => TypeSubs -> TypeBase (DimDecl VName) als -> TypeBase (DimDecl VName) als
substituteTypes substs ot = case ot of
  Array als u at shape ->
    arrayOf (substituteTypes substs (Scalar at) `setAliases` mempty)
    (substituteInShape shape) u `addAliases` (<>als)
  Scalar (Prim t) -> Scalar $ Prim t
  Scalar (TypeVar als u v targs)
    | Just (TypeSub (TypeAbbr _ ps t)) <-
        M.lookup (qualLeaf (qualNameFromTypeName v)) substs ->
        applyType ps (t `setAliases` mempty) (map substituteInTypeArg targs)
        `setUniqueness` u `addAliases` (<>als)
    | otherwise -> Scalar $ TypeVar als u v $ map substituteInTypeArg targs
  Scalar (Record ts) ->
    Scalar $ Record $ fmap (substituteTypes substs) ts
  Scalar (Arrow als v t1 t2) ->
    Scalar $ Arrow als v (substituteTypes substs t1) (substituteTypes substs t2)
  Scalar (Sum cs) ->
    Scalar $ Sum $ (fmap . fmap) (substituteTypes substs) cs
  where substituteInTypeArg (TypeArgDim d loc) =
          TypeArgDim (substituteInDim d) loc
        substituteInTypeArg (TypeArgType t loc) =
          TypeArgType (substituteTypes substs t) loc

        substituteInShape (ShapeDecl ds) =
          ShapeDecl $ map substituteInDim ds

        substituteInDim (NamedDim v)
          | Just (DimSub d) <- M.lookup (qualLeaf v) substs = d
        substituteInDim d = d

applyType :: Monoid als =>
             [TypeParam] -> TypeBase (DimDecl VName) als -> [StructTypeArg] -> TypeBase (DimDecl VName) als
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

instance Substitutable (TypeBase () Aliasing) where
  applySubst = substTypesAny . (fmap (fmap fromStruct).)

instance Substitutable (TypeBase (DimDecl VName) ()) where
  applySubst = substTypesAny . (fmap (fmap vacuousShapeAnnotations).)

instance Substitutable (TypeBase (DimDecl VName) Aliasing) where
  applySubst = substTypesAny . (fmap (fmap (vacuousShapeAnnotations . fromStruct)).)

-- | Perform substitutions, from type names to types, on a type. Works
-- regardless of what shape and uniqueness information is attached to the type.
substTypesAny :: (ArrayDim dim, Monoid as) =>
                 (VName -> Maybe (Subst (TypeBase dim as)))
              -> TypeBase dim as -> TypeBase dim as
substTypesAny lookupSubst ot = case ot of
  Array als u et shape ->
    arrayOf (substTypesAny lookupSubst' (Scalar et)) shape u `setAliases` als
  Scalar (Prim t) -> Scalar $ Prim t
  -- We only substitute for a type variable with no arguments, since
  -- type parameters cannot have higher kind.
  Scalar (TypeVar als u v targs) ->
    case lookupSubst $ qualLeaf (qualNameFromTypeName v) of
      Just (Subst t) -> t `setUniqueness` u `addAliases` (<>als)
      Just PrimSubst -> Scalar $ TypeVar mempty u v $ map subsTypeArg targs
      Nothing -> Scalar $ TypeVar als u v $ map subsTypeArg targs
  Scalar (Record ts) -> Scalar $ Record $ fmap (substTypesAny lookupSubst) ts
  Scalar (Arrow als v t1 t2) ->
    Scalar $ Arrow als v (substTypesAny lookupSubst t1) (substTypesAny lookupSubst t2)
  Scalar (Sum ts) ->
    Scalar $ Sum $ (fmap . fmap) (substTypesAny lookupSubst) ts

  where subsTypeArg (TypeArgType t loc) =
          TypeArgType (substTypesAny lookupSubst' t) loc
        subsTypeArg t = t

        lookupSubst' = fmap (fmap $ second (const ())) . lookupSubst
