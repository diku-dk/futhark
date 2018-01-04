{-# LANGUAGE FlexibleContexts #-}
module Language.Futhark.TypeChecker.Types
  ( checkTypeExp
  , checkTypeDecl

  , unifyTypes

  , checkPattern
  , InferredType(..)

  , checkForDuplicateNames
  , checkTypeParams

  , TypeSub(..)
  , TypeSubs
  , substituteTypes
  , substituteTypesInBoundV

  , instantiatePolymorphic

  , arrayOfM
  )
where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Data.List
import Data.Loc
import Data.Maybe
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Language.Futhark
import Language.Futhark.TypeChecker.Monad
import Futhark.Util.Pretty (Pretty)

-- | @t1 `unifyTypes` t2@ attempts to unify @t1@ and @t2@.  If
-- unification cannot happen, 'Nothing' is returned, otherwise a type
-- that combines the aliasing of @t1@ and @t2@ is returned.  The
-- uniqueness of the resulting type will be the least of the
-- uniqueness of @t1@ and @t2@.
unifyTypes :: (Monoid als, Eq als, ArrayDim dim) =>
              TypeBase dim als
           -> TypeBase dim als
           -> Maybe (TypeBase dim als)
unifyTypes (Prim t1) (Prim t2)
  | t1 == t2  = Just $ Prim t1
  | otherwise = Nothing
unifyTypes (TypeVar t1 targs1) (TypeVar t2 targs2)
  | t1 == t2 = do
      targs3 <- zipWithM unifyTypeArgs targs1 targs2
      Just $ TypeVar t1 targs3
  | otherwise = Nothing
unifyTypes (Array et1 shape1 u1) (Array et2 shape2 u2) =
  Array <$> unifyArrayElemTypes et1 et2 <*>
  unifyShapes shape1 shape2 <*> pure (u1 <> u2)
unifyTypes (Record ts1) (Record ts2)
  | length ts1 == length ts2,
    sort (M.keys ts1) == sort (M.keys ts2) =
      Record <$> traverse (uncurry unifyTypes)
      (M.intersectionWith (,) ts1 ts2)
unifyTypes _ _ = Nothing

unifyTypeArgs :: (Monoid als, Eq als, ArrayDim dim) =>
                 TypeArg dim als -> TypeArg dim als -> Maybe (TypeArg dim als)
unifyTypeArgs (TypeArgDim d1 loc) (TypeArgDim d2 _) =
  TypeArgDim <$> unifyDims d1 d2 <*> pure loc
unifyTypeArgs (TypeArgType t1 loc) (TypeArgType t2 _) =
  TypeArgType <$> unifyTypes t1 t2 <*> pure loc
unifyTypeArgs _ _ =
  Nothing

unifyArrayElemTypes :: (Monoid als, Eq als, ArrayDim dim) =>
                       ArrayElemTypeBase dim als
                    -> ArrayElemTypeBase dim als
                    -> Maybe (ArrayElemTypeBase dim als)
unifyArrayElemTypes (ArrayPrimElem bt1 als1) (ArrayPrimElem bt2 als2)
  | bt1 == bt2 =
      Just $ ArrayPrimElem bt1 (als1 <> als2)
unifyArrayElemTypes (ArrayPolyElem bt1 targs1 als1) (ArrayPolyElem bt2 targs2 als2)
  | bt1 == bt2, targs1 == targs2 =
      Just $ ArrayPolyElem bt1 targs1 (als1 <> als2)
unifyArrayElemTypes (ArrayRecordElem et1) (ArrayRecordElem et2)
  | sort (M.keys et1) == sort (M.keys et2) =
    ArrayRecordElem <$>
    traverse (uncurry unifyRecordArrayElemTypes) (M.intersectionWith (,) et1 et2)
unifyArrayElemTypes _ _ =
  Nothing

unifyRecordArrayElemTypes :: (Monoid als, Eq als, ArrayDim dim) =>
                             RecordArrayElemTypeBase dim als
                          -> RecordArrayElemTypeBase dim als
                          -> Maybe (RecordArrayElemTypeBase dim als)
unifyRecordArrayElemTypes (RecordArrayElem et1) (RecordArrayElem et2) =
  RecordArrayElem <$> unifyArrayElemTypes et1 et2
unifyRecordArrayElemTypes (RecordArrayArrayElem et1 shape1 u1) (RecordArrayArrayElem et2 shape2 u2) =
  RecordArrayArrayElem <$> unifyArrayElemTypes et1 et2 <*>
  unifyShapes shape1 shape2 <*> pure (u1<>u2)
unifyRecordArrayElemTypes _ _ =
  Nothing

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

data InferredType = NoneInferred
                  | Inferred CompType
                  | Ascribed PatternType

bindPatternNames :: MonadTypeChecker m =>
                    PatternBase NoInfo Name -> m a -> m a
bindPatternNames = bindSpaced . map asTerm . S.toList . patIdentSet
  where asTerm v = (Term, identName v)

checkPattern :: MonadTypeChecker m =>
                UncheckedPattern -> InferredType -> (Pattern -> m a)
             -> m a
checkPattern p t m = do
  checkForDuplicateNames [p]
  bindPatternNames p $
    m =<< checkPattern' p t

checkPattern' :: MonadTypeChecker m =>
                 UncheckedPattern -> InferredType
              -> m Pattern

checkPattern' (PatternParens p loc) t =
  PatternParens <$> checkPattern' p t <*> pure loc

checkPattern' (Id name NoInfo loc) (Inferred t) = do
  name' <- checkName Term name loc
  let t' = vacuousShapeAnnotations $
           case t of Record{} -> t
                     _        -> t `addAliases` S.insert name'
  return $ Id name' (Info $ t' `setUniqueness` Nonunique) loc
checkPattern' (Id name NoInfo loc) (Ascribed t) = do
  name' <- checkName Term name loc
  let t' = case t of Record{} -> t
                     _        -> t `addAliases` S.insert name'
  return $ Id name' (Info t') loc

checkPattern' (Wildcard _ loc) (Inferred t) =
  return $ Wildcard (Info $ vacuousShapeAnnotations $ t `setUniqueness` Nonunique) loc
checkPattern' (Wildcard _ loc) (Ascribed t) =
  return $ Wildcard (Info $ t `setUniqueness` Nonunique) loc

checkPattern' (TuplePattern ps loc) (Inferred t)
  | Just ts <- isTupleRecord t, length ts == length ps =
  TuplePattern <$> zipWithM checkPattern' ps (map Inferred ts) <*> pure loc
checkPattern' (TuplePattern ps loc) (Ascribed t)
  | Just ts <- isTupleRecord t, length ts == length ps =
      TuplePattern <$> zipWithM checkPattern' ps (map Ascribed ts) <*> pure loc
checkPattern' p@TuplePattern{} (Inferred t) =
  throwError $ TypeError (srclocOf p) $ "Pattern " ++ pretty p ++ " cannot match " ++ pretty t
checkPattern' p@TuplePattern{} (Ascribed t) =
  throwError $ TypeError (srclocOf p) $ "Pattern " ++ pretty p ++ " cannot match " ++ pretty t
checkPattern' (TuplePattern ps loc) NoneInferred =
  TuplePattern <$> mapM (`checkPattern'` NoneInferred) ps <*> pure loc

checkPattern' (RecordPattern p_fs loc) (Inferred (Record t_fs))
  | sort (map fst p_fs) == sort (M.keys t_fs) =
    RecordPattern . M.toList <$> check <*> pure loc
    where check = traverse (uncurry checkPattern') $ M.intersectionWith (,)
                  (M.fromList p_fs) (fmap Inferred t_fs)
checkPattern' (RecordPattern p_fs loc) (Ascribed (Record t_fs))
  | sort (map fst p_fs) == sort (M.keys t_fs) =
    RecordPattern . M.toList <$> check <*> pure loc
    where check = traverse (uncurry checkPattern') $ M.intersectionWith (,)
                  (M.fromList p_fs) (fmap Ascribed t_fs)
checkPattern' p@RecordPattern{} (Inferred t) =
  throwError $ TypeError (srclocOf p) $ "Pattern " ++ pretty p ++ " cannot match " ++ pretty t
checkPattern' p@RecordPattern{} (Ascribed t) =
  throwError $ TypeError (srclocOf p) $ "Pattern " ++ pretty p ++ " cannot match " ++ pretty t
checkPattern' (RecordPattern fs loc) NoneInferred =
  RecordPattern . M.toList <$> traverse (`checkPattern'` NoneInferred) (M.fromList fs) <*> pure loc

checkPattern' fullp@(PatternAscription p (TypeDecl t NoInfo)) maybe_outer_t = do
  (t', st) <- checkTypeExp t

  r <- getType (srclocOf fullp) st
  case r of
    Left _ -> throwError $ TypeError (srclocOf fullp) "Cannot bind a function in a pattern."
    Right _ -> return ()

  let maybe_outer_t' = case maybe_outer_t of
                         Inferred outer_t -> Just $ vacuousShapeAnnotations outer_t
                         Ascribed outer_t -> Just outer_t
                         NoneInferred -> Nothing
      st' = fromStruct st
  case maybe_outer_t' of
    Just outer_t
      | Just t'' <- unifyTypes outer_t st' ->
          PatternAscription <$> checkPattern' p (Ascribed t'') <*>
          pure (TypeDecl t' (Info st))
      | otherwise ->
          let outer_t_for_error =
                modifyShapeAnnotations (fmap baseName) $ outer_t `setAliases` ()
          in throwError $ InvalidPatternError fullp outer_t_for_error Nothing $ srclocOf p
    _ -> PatternAscription <$> checkPattern' p (Ascribed st') <*>
         pure (TypeDecl t' (Info st))

checkPattern' p NoneInferred =
  throwError $ TypeError (srclocOf p) $ "Cannot determine type of " ++ pretty p

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
        check (PatternAscription p _) = check p

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
                            recordArrayElemToType) ts

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

arrayOfM :: (MonadTypeChecker m, Pretty (ShapeDecl dim), ArrayDim dim, Monoid as) =>
            SrcLoc
         -> TypeBase dim as
         -> ShapeDecl dim
         -> Uniqueness
         -> m (TypeBase dim as)
arrayOfM loc t shape u = maybe nope return $ arrayOf t shape u
  where nope = throwError $ TypeError loc $
               "Cannot form an array with elements of type " ++ pretty t
