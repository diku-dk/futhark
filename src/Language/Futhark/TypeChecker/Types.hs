{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Type checker building blocks that do not involve unification.
module Language.Futhark.TypeChecker.Types
  ( checkTypeExp,
    checkTypeDecl,
    unifyTypesU,
    subtypeOf,
    subuniqueOf,
    checkForDuplicateNames,
    checkTypeParams,
    typeParamToArg,
    Subst (..),
    substFromAbbr,
    TypeSubs,
    Substitutable (..),
    substTypesAny,
  )
where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.List (foldl', sort)
import qualified Data.Map.Strict as M
import Data.Maybe
import Futhark.Util (nubOrd)
import Futhark.Util.Pretty hiding ((<|>))
import Language.Futhark
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Monad

-- | @unifyTypes uf t1 t2@ attempts to unify @t1@ and @t2@.  If
-- unification cannot happen, 'Nothing' is returned, otherwise a type
-- that combines the aliasing of @t1@ and @t2@ is returned.
-- Uniqueness is unified with @uf@.  Assumes sizes already match, and
-- always picks the size of the leftmost type.
unifyTypesU ::
  (Monoid als, ArrayDim dim) =>
  (Uniqueness -> Uniqueness -> Maybe Uniqueness) ->
  TypeBase dim als ->
  TypeBase dim als ->
  Maybe (TypeBase dim als)
unifyTypesU uf (Array als1 u1 et1 shape1) (Array als2 u2 et2 _shape2) =
  Array (als1 <> als2) <$> uf u1 u2
    <*> unifyScalarTypes uf et1 et2
    <*> pure shape1
unifyTypesU uf (Scalar t1) (Scalar t2) = Scalar <$> unifyScalarTypes uf t1 t2
unifyTypesU _ _ _ = Nothing

unifyScalarTypes ::
  (Monoid als, ArrayDim dim) =>
  (Uniqueness -> Uniqueness -> Maybe Uniqueness) ->
  ScalarTypeBase dim als ->
  ScalarTypeBase dim als ->
  Maybe (ScalarTypeBase dim als)
unifyScalarTypes _ (Prim t1) (Prim t2)
  | t1 == t2 = Just $ Prim t1
  | otherwise = Nothing
unifyScalarTypes uf (TypeVar als1 u1 tv1 targs1) (TypeVar als2 u2 tv2 targs2)
  | tv1 == tv2 = do
    u3 <- uf u1 u2
    targs3 <- zipWithM unifyTypeArgs targs1 targs2
    Just $ TypeVar (als1 <> als2) u3 tv1 targs3
  | otherwise = Nothing
  where
    unifyTypeArgs (TypeArgDim d1 loc) (TypeArgDim _d2 _) =
      pure $ TypeArgDim d1 loc
    unifyTypeArgs (TypeArgType t1 loc) (TypeArgType t2 _) =
      TypeArgType <$> unifyTypesU uf t1 t2 <*> pure loc
    unifyTypeArgs _ _ =
      Nothing
unifyScalarTypes uf (Record ts1) (Record ts2)
  | length ts1 == length ts2,
    sort (M.keys ts1) == sort (M.keys ts2) =
    Record
      <$> traverse
        (uncurry (unifyTypesU uf))
        (M.intersectionWith (,) ts1 ts2)
unifyScalarTypes uf (Arrow as1 mn1 t1 t1') (Arrow as2 _ t2 t2') =
  Arrow (as1 <> as2) mn1 <$> unifyTypesU (flip uf) t1 t2 <*> unifyTypesU uf t1' t2'
unifyScalarTypes uf (Sum cs1) (Sum cs2)
  | length cs1 == length cs2,
    sort (M.keys cs1) == sort (M.keys cs2) =
    Sum
      <$> traverse
        (uncurry (zipWithM (unifyTypesU uf)))
        (M.intersectionWith (,) cs1 cs2)
unifyScalarTypes _ _ _ = Nothing

-- | @x \`subtypeOf\` y@ is true if @x@ is a subtype of @y@ (or equal
-- to @y@), meaning @x@ is valid whenever @y@ is.  Ignores sizes.
-- Mostly used for checking uniqueness.
subtypeOf :: TypeBase () () -> TypeBase () () -> Bool
subtypeOf t1 t2 = isJust $ unifyTypesU unifyUniqueness (toStruct t1) (toStruct t2)
  where
    unifyUniqueness u2 u1 = if u2 `subuniqueOf` u1 then Just u1 else Nothing

-- | @x `subuniqueOf` y@ is true if @x@ is not less unique than @y@.
subuniqueOf :: Uniqueness -> Uniqueness -> Bool
subuniqueOf Nonunique Unique = False
subuniqueOf _ _ = True

-- | Use 'checkTypeExp' to check a type declaration.
checkTypeDecl ::
  MonadTypeChecker m =>
  TypeDeclBase NoInfo Name ->
  m (TypeDeclBase Info VName, Liftedness)
checkTypeDecl (TypeDecl t NoInfo) = do
  checkForDuplicateNamesInType t
  (t', st, l) <- checkTypeExp t
  return (TypeDecl t' $ Info st, l)

-- | Type-check a single 'TypeExp', returning the checked 'TypeExp',
-- its fully expanded type (modulo yet-unelaborated type variables),
-- and whether it is potentially higher-order.
checkTypeExp ::
  MonadTypeChecker m =>
  TypeExp Name ->
  m (TypeExp VName, StructType, Liftedness)
checkTypeExp (TEVar name loc) = do
  (name', ps, t, l) <- lookupType loc name
  case ps of
    [] -> return (TEVar name' loc, t, l)
    _ ->
      typeError loc mempty $
        "Type constructor" <+> pquote (spread (ppr name : map ppr ps))
          <+> "used without any arguments."
checkTypeExp (TETuple ts loc) = do
  (ts', ts_s, ls) <- unzip3 <$> mapM checkTypeExp ts
  return (TETuple ts' loc, tupleRecord ts_s, foldl' max Unlifted ls)
checkTypeExp t@(TERecord fs loc) = do
  -- Check for duplicate field names.
  let field_names = map fst fs
  unless (sort field_names == sort (nubOrd field_names)) $
    typeError loc mempty $ "Duplicate record fields in" <+> ppr t <> "."

  fs_ts_ls <- traverse checkTypeExp $ M.fromList fs
  let fs' = fmap (\(x, _, _) -> x) fs_ts_ls
      ts_s = fmap (\(_, y, _) -> y) fs_ts_ls
      ls = fmap (\(_, _, z) -> z) fs_ts_ls
  return
    ( TERecord (M.toList fs') loc,
      Scalar $ Record ts_s,
      foldl' max Unlifted ls
    )
checkTypeExp (TEArray t d loc) = do
  (t', st, l) <- checkTypeExp t
  (d', d'') <- checkDimExp d
  case (l, arrayOf st (ShapeDecl [d'']) Nonunique) of
    (Unlifted, st') -> return (TEArray t' d' loc, st', Unlifted)
    (SizeLifted, _) ->
      typeError loc mempty $
        "Cannot create array with elements of size-lifted type" <+> pquote (ppr t)
          <+/> "(might cause irregular array)."
    (Lifted, _) ->
      typeError loc mempty $
        "Cannot create array with elements of lifted type" <+> pquote (ppr t)
          <+/> "(might contain function)."
  where
    checkDimExp DimExpAny =
      return (DimExpAny, AnyDim Nothing)
    checkDimExp (DimExpConst k dloc) =
      return (DimExpConst k dloc, ConstDim k)
    checkDimExp (DimExpNamed v dloc) = do
      v' <- checkNamedDim loc v
      return (DimExpNamed v' dloc, NamedDim v')
checkTypeExp (TEUnique t loc) = do
  (t', st, l) <- checkTypeExp t
  unless (mayContainArray st) $
    warn loc $ "Declaring" <+> pquote (ppr st) <+> "as unique has no effect."
  return (TEUnique t' loc, st `setUniqueness` Unique, l)
  where
    mayContainArray (Scalar Prim {}) = False
    mayContainArray Array {} = True
    mayContainArray (Scalar (Record fs)) = any mayContainArray fs
    mayContainArray (Scalar TypeVar {}) = True
    mayContainArray (Scalar Arrow {}) = False
    mayContainArray (Scalar (Sum cs)) = (any . any) mayContainArray cs
checkTypeExp (TEArrow (Just v) t1 t2 loc) = do
  (t1', st1, _) <- checkTypeExp t1
  bindSpaced [(Term, v)] $ do
    v' <- checkName Term v loc
    bindVal v' (BoundV [] st1) $ do
      (t2', st2, _) <- checkTypeExp t2
      return
        ( TEArrow (Just v') t1' t2' loc,
          Scalar $ Arrow mempty (Named v') st1 st2,
          Lifted
        )
checkTypeExp (TEArrow Nothing t1 t2 loc) = do
  (t1', st1, _) <- checkTypeExp t1
  (t2', st2, _) <- checkTypeExp t2
  return
    ( TEArrow Nothing t1' t2' loc,
      Scalar $ Arrow mempty Unnamed st1 st2,
      Lifted
    )
checkTypeExp ote@TEApply {} = do
  (tname, tname_loc, targs) <- rootAndArgs ote
  (tname', ps, t, l) <- lookupType tloc tname
  if length ps /= length targs
    then
      typeError tloc mempty $
        "Type constructor" <+> pquote (ppr tname) <+> "requires" <+> ppr (length ps)
          <+> "arguments, but provided"
          <+> ppr (length targs) <> "."
    else do
      (targs', substs) <- unzip <$> zipWithM checkArgApply ps targs
      return
        ( foldl (\x y -> TEApply x y tloc) (TEVar tname' tname_loc) targs',
          applySubst (`M.lookup` mconcat substs) t,
          l
        )
  where
    tloc = srclocOf ote

    rootAndArgs :: MonadTypeChecker m => TypeExp Name -> m (QualName Name, SrcLoc, [TypeArgExp Name])
    rootAndArgs (TEVar qn loc) = return (qn, loc, [])
    rootAndArgs (TEApply op arg _) = do
      (op', loc, args) <- rootAndArgs op
      return (op', loc, args ++ [arg])
    rootAndArgs te' =
      typeError (srclocOf te') mempty $
        "Type" <+> pquote (ppr te') <+> "is not a type constructor."

    checkArgApply (TypeParamDim pv _) (TypeArgExpDim (DimExpNamed v dloc) loc) = do
      v' <- checkNamedDim loc v
      return
        ( TypeArgExpDim (DimExpNamed v' dloc) loc,
          M.singleton pv $ SizeSubst $ NamedDim v'
        )
    checkArgApply (TypeParamDim pv _) (TypeArgExpDim (DimExpConst x dloc) loc) =
      return
        ( TypeArgExpDim (DimExpConst x dloc) loc,
          M.singleton pv $ SizeSubst $ ConstDim x
        )
    checkArgApply (TypeParamDim pv _) (TypeArgExpDim DimExpAny loc) = do
      d <- newID "d"
      return
        ( TypeArgExpDim DimExpAny loc,
          M.singleton pv $ SizeSubst $ AnyDim $ Just d
        )
    checkArgApply (TypeParamType _ pv _) (TypeArgExpType te) = do
      (te', st, _) <- checkTypeExp te
      return
        ( TypeArgExpType te',
          M.singleton pv $ Subst [] st
        )
    checkArgApply p a =
      typeError tloc mempty $
        "Type argument" <+> ppr a
          <+> "not valid for a type parameter"
          <+> ppr p <> "."
checkTypeExp t@(TESum cs loc) = do
  let constructors = map fst cs
  unless (sort constructors == sort (nubOrd constructors)) $
    typeError loc mempty $ "Duplicate constructors in" <+> ppr t

  unless (length constructors < 256) $
    typeError loc mempty "Sum types must have less than 256 constructors."

  cs_ts_ls <- (traverse . traverse) checkTypeExp $ M.fromList cs
  let cs' = (fmap . fmap) (\(x, _, _) -> x) cs_ts_ls
      ts_s = (fmap . fmap) (\(_, y, _) -> y) cs_ts_ls
      ls = (concatMap . fmap) (\(_, _, z) -> z) cs_ts_ls
  return
    ( TESum (M.toList cs') loc,
      Scalar $ Sum ts_s,
      foldl' max Unlifted ls
    )

-- | Check for duplication of names inside a pattern group.  Produces
-- a description of all names used in the pattern group.
checkForDuplicateNames ::
  MonadTypeChecker m =>
  [UncheckedPat] ->
  m ()
checkForDuplicateNames = (`evalStateT` mempty) . mapM_ check
  where
    check (Id v _ loc) = seen v loc
    check (PatParens p _) = check p
    check Wildcard {} = return ()
    check (TuplePat ps _) = mapM_ check ps
    check (RecordPat fs _) = mapM_ (check . snd) fs
    check (PatAscription p _ _) = check p
    check PatLit {} = return ()
    check (PatConstr _ _ ps _) = mapM_ check ps

    seen v loc = do
      already <- gets $ M.lookup v
      case already of
        Just prev_loc ->
          lift $
            typeError loc mempty $
              "Name" <+> pquote (ppr v) <+> "also bound at"
                <+> text (locStr prev_loc) <> "."
        Nothing ->
          modify $ M.insert v loc

-- | Check whether the type contains arrow types that define the same
-- parameter.  These might also exist further down, but that's not
-- really a problem - we mostly do this checking to help the user,
-- since it is likely an error, but it's easy to assign a semantics to
-- it (normal name shadowing).
checkForDuplicateNamesInType ::
  MonadTypeChecker m =>
  TypeExp Name ->
  m ()
checkForDuplicateNamesInType = check mempty
  where
    check seen (TEArrow (Just v) t1 t2 loc)
      | Just prev_loc <- M.lookup v seen =
        typeError loc mempty $
          text "Name" <+> pquote (ppr v)
            <+> "also bound at"
            <+> text (locStr prev_loc) <> "."
      | otherwise =
        check seen' t1 >> check seen' t2
      where
        seen' = M.insert v loc seen
    check seen (TEArrow Nothing t1 t2 _) =
      check seen t1 >> check seen t2
    check seen (TETuple ts _) = mapM_ (check seen) ts
    check seen (TERecord fs _) = mapM_ (check seen . snd) fs
    check seen (TEUnique t _) = check seen t
    check seen (TESum cs _) = mapM_ (mapM (check seen) . snd) cs
    check seen (TEApply t1 (TypeArgExpType t2) _) =
      check seen t1 >> check seen t2
    check seen (TEApply t1 TypeArgExpDim {} _) =
      check seen t1
    check _ TEArray {} = return ()
    check _ TEVar {} = return ()

-- | @checkTypeParams ps m@ checks the type parameters @ps@, then
-- invokes the continuation @m@ with the checked parameters, while
-- extending the monadic name map with @ps@.
checkTypeParams ::
  MonadTypeChecker m =>
  [TypeParamBase Name] ->
  ([TypeParamBase VName] -> m a) ->
  m a
checkTypeParams ps m =
  bindSpaced (map typeParamSpace ps) $
    m =<< evalStateT (mapM checkTypeParam ps) mempty
  where
    typeParamSpace (TypeParamDim pv _) = (Term, pv)
    typeParamSpace (TypeParamType _ pv _) = (Type, pv)

    checkParamName ns v loc = do
      seen <- gets $ M.lookup (ns, v)
      case seen of
        Just prev ->
          lift $
            typeError loc mempty $
              text "Type parameter" <+> pquote (ppr v)
                <+> "previously defined at"
                <+> text (locStr prev) <> "."
        Nothing -> do
          modify $ M.insert (ns, v) loc
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
  TypeArgType (Scalar $ TypeVar () Nonunique (typeName v) []) ploc

-- | A type substituion may be a substitution or a yet-unknown
-- substitution (but which is certainly an overloaded primitive
-- type!).  The latter is used to remove aliases from types that are
-- yet-unknown but that we know cannot carry aliases (see issue #682).
data Subst t = Subst [TypeParam] t | PrimSubst | SizeSubst (DimDecl VName)
  deriving (Show)

substFromAbbr :: TypeBinding -> Subst StructType
substFromAbbr (TypeAbbr _ ps t) = Subst ps t

-- | Substitutions to apply in a type.
type TypeSubs = VName -> Maybe (Subst StructType)

instance Functor Subst where
  fmap f (Subst ps t) = Subst ps $ f t
  fmap _ PrimSubst = PrimSubst
  fmap _ (SizeSubst v) = SizeSubst v

-- | Class of types which allow for substitution of types with no
-- annotations for type variable names.
class Substitutable a where
  applySubst :: TypeSubs -> a -> a

instance Substitutable (TypeBase (DimDecl VName) ()) where
  applySubst = substTypesAny

instance Substitutable (TypeBase (DimDecl VName) Aliasing) where
  applySubst = substTypesAny . (fmap (fmap fromStruct) .)

instance Substitutable (DimDecl VName) where
  applySubst f (NamedDim (QualName _ v))
    | Just (SizeSubst d) <- f v = d
  applySubst _ d = d

instance Substitutable d => Substitutable (ShapeDecl d) where
  applySubst f = fmap $ applySubst f

instance Substitutable Pat where
  applySubst f = runIdentity . astMap mapper
    where
      mapper =
        ASTMapper
          { mapOnExp = return,
            mapOnName = return,
            mapOnQualName = return,
            mapOnStructType = return . applySubst f,
            mapOnPatType = return . applySubst f
          }

applyType ::
  Monoid als =>
  [TypeParam] ->
  TypeBase (DimDecl VName) als ->
  [StructTypeArg] ->
  TypeBase (DimDecl VName) als
applyType ps t args = substTypesAny (`M.lookup` substs) t
  where
    substs = M.fromList $ zipWith mkSubst ps args
    -- We are assuming everything has already been type-checked for correctness.
    mkSubst (TypeParamDim pv _) (TypeArgDim d _) =
      (pv, SizeSubst d)
    mkSubst (TypeParamType _ pv _) (TypeArgType at _) =
      (pv, Subst [] $ second mempty at)
    mkSubst p a =
      error $ "applyType mkSubst: cannot substitute " ++ pretty a ++ " for " ++ pretty p

-- | Perform substitutions, from type names to types, on a type. Works
-- regardless of what shape and uniqueness information is attached to the type.
substTypesAny ::
  Monoid as =>
  (VName -> Maybe (Subst (TypeBase (DimDecl VName) as))) ->
  TypeBase (DimDecl VName) as ->
  TypeBase (DimDecl VName) as
substTypesAny lookupSubst ot = case ot of
  Array als u et shape ->
    arrayOf
      (substTypesAny lookupSubst' (Scalar et))
      (applySubst lookupSubst' shape)
      u
      `setAliases` als
  Scalar (Prim t) -> Scalar $ Prim t
  Scalar (TypeVar als u v targs) ->
    let targs' = map subsTypeArg targs
     in case lookupSubst $ qualLeaf (qualNameFromTypeName v) of
          Just (Subst ps t) ->
            applyType ps (t `setAliases` mempty) targs'
              `setUniqueness` u `addAliases` (<> als)
          Just PrimSubst -> Scalar $ TypeVar mempty u v targs'
          _ -> Scalar $ TypeVar als u v targs'
  Scalar (Record ts) -> Scalar $ Record $ fmap (substTypesAny lookupSubst) ts
  Scalar (Arrow als v t1 t2) ->
    Scalar $ Arrow als v (substTypesAny lookupSubst t1) (substTypesAny lookupSubst t2)
  Scalar (Sum ts) ->
    Scalar $ Sum $ fmap (fmap $ substTypesAny lookupSubst) ts
  where
    subsTypeArg (TypeArgType t loc) =
      TypeArgType (substTypesAny lookupSubst' t) loc
    subsTypeArg (TypeArgDim v loc) =
      TypeArgDim (applySubst lookupSubst' v) loc

    lookupSubst' = fmap (fmap $ second (const ())) . lookupSubst
