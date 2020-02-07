{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Futhark.TypeChecker.Unify
  ( Constraint(..)
  , Usage
  , mkUsage
  , mkUsage'
  , Level
  , Constraints
  , MonadUnify(..)
  , Rigidity(..)
  , RigidSource(..)
  , BreadCrumb(..)
  , typeError
  , dimNotes
  , mkTypeVarName

  , zeroOrderType
  , mustHaveConstr
  , mustHaveField
  , mustBeOneOf
  , equalityType
  , normType
  , normPatternType
  , normTypeFully
  , instantiateEmptyArrayDims

  , unify
  , expect
  , unifyMostCommon
  , anyDimOnMismatch
  , doUnification
  )
where

import Control.Monad.Except
import Control.Monad.Writer hiding (Sum)
import Control.Monad.RWS.Strict hiding (Sum)
import Data.Bifoldable (biany)
import Data.List
import Data.Loc
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Language.Futhark
import Language.Futhark.TypeChecker.Monad hiding (BoundV)
import Language.Futhark.TypeChecker.Types
import Futhark.Util.Pretty (Pretty, prettyOneLine)

-- | A usage that caused a type constraint.
data Usage = Usage (Maybe String) SrcLoc

mkUsage :: SrcLoc -> String -> Usage
mkUsage = flip (Usage . Just)

mkUsage' :: SrcLoc -> Usage
mkUsage' = Usage Nothing

instance Show Usage where
  show (Usage Nothing loc) = "use at " ++ locStr loc
  show (Usage (Just s) loc) = s ++ " at " ++ locStr loc

instance Located Usage where
  locOf (Usage _ loc) = locOf loc

-- | The level at which a type variable is bound.  Higher means
-- deeper.  We can only unify a type variable at level 'i' with a type
-- 't' if all type names that occur in 't' are at most at level 'i'.
type Level = Int

data Constraint = NoConstraint Liftedness Usage
                | ParamType Liftedness SrcLoc
                | Constraint StructType Usage
                | Overloaded [PrimType] Usage
                | HasFields (M.Map Name StructType) Usage
                | Equality Usage
                | HasConstrs (M.Map Name [StructType]) Usage
                | ParamSize SrcLoc
                | Size (Maybe (DimDecl VName)) Usage
                  -- ^ Is not actually a type, but a term-level size,
                  -- possibly already set to something specific.
                | UnknowableSize SrcLoc RigidSource
                  -- ^ A size that does not unify with anything -
                  -- created from the result of applying a function
                  -- whose return size is existential, or otherwise
                  -- hiding a size.
                deriving Show

instance Located Constraint where
  locOf (NoConstraint _ usage) = locOf usage
  locOf (ParamType _ usage) = locOf usage
  locOf (Constraint _ usage) = locOf usage
  locOf (Overloaded _ usage) = locOf usage
  locOf (HasFields _ usage) = locOf usage
  locOf (Equality usage) = locOf usage
  locOf (HasConstrs _ usage) = locOf usage
  locOf (ParamSize loc) = locOf loc
  locOf (Size _ usage) = locOf usage
  locOf (UnknowableSize loc _) = locOf loc

-- | Mapping from fresh type variables, instantiated from the type
-- schemes of polymorphic functions, to (possibly) specific types as
-- determined on application and the location of that application, or
-- a partial constraint on their type.
type Constraints = M.Map VName (Level, Constraint)

lookupSubst :: VName -> Constraints -> Maybe (Subst StructType)
lookupSubst v constraints = case snd <$> M.lookup v constraints of
                              Just (Constraint t _) -> Just $ Subst t
                              Just Overloaded{} -> Just PrimSubst
                              Just (Size (Just d) _) ->
                                Just $ SizeSubst $ applySubst (`lookupSubst` constraints) d
                              _ -> Nothing

-- | The source of a rigid size.
data RigidSource
  = RigidArg (Maybe (QualName VName)) String
    -- ^ A function argument that is not a constant or variable name.
  | RigidRet (Maybe (QualName VName))
    -- ^ An existential return size.
  | RigidLoop
  | RigidSlice (Maybe (DimDecl VName)) String
    -- ^ Produced by a complicated slice expression.
  | RigidRange
    -- ^ Produced by a complicated range expression.
  | RigidBound String
    -- ^ Produced by a range expression with this bound.
  | RigidCond StructType StructType
    -- ^ Mismatch in branches.
  | RigidUnify
    -- ^ Invented during unification.
  | RigidOutOfScope SrcLoc VName
  deriving (Eq, Ord, Show)

-- | The ridigity of a size variable.  All rigid sizes are tagged with
-- information about how they were generated.
data Rigidity = Rigid RigidSource | Nonrigid
              deriving (Eq, Ord, Show)

prettySource :: SrcLoc -> SrcLoc -> RigidSource -> String

prettySource ctx loc (RigidRet Nothing) =
  "is unknown size returned by function at " <> locStrRel ctx loc <> "."

prettySource ctx loc (RigidRet (Just fname)) =
  "is unknown size returned by " <> quote (pretty fname) <>
  " at " <> locStrRel ctx loc <> "."

prettySource ctx loc (RigidArg fname arg) =
  "is value of argument\n" <>
  indent (shorten arg) <>
  "\npassed to " <> fname' <> " at " <> locStrRel ctx loc <> "."
  where fname' = maybe "function" (quote . pretty) fname

prettySource ctx loc (RigidSlice d slice) =
  "is size produced by slice\n" <>
  indent (shorten slice) <>
  "\n" <> d_desc <> "at " <> locStrRel ctx loc <> "."
  where d_desc = case d of
                   Just d' -> "of dimension of size " <> quote (pretty d') <> " "
                   Nothing -> ""

prettySource ctx loc RigidLoop =
  "is unknown size of value returned at " <> locStrRel ctx loc <> "."

prettySource ctx loc RigidRange =
  "is unknown length of range at " <> locStrRel ctx loc <> "."

prettySource ctx loc (RigidBound bound) =
  "generated from expression\n" <>
  indent (shorten bound)
  <> "\nused in range at " <> locStrRel ctx loc <> "."

prettySource ctx loc (RigidOutOfScope boundloc v) =
  "is an unknown size arising from " <> quote (prettyName v) <>
  " going out of scope at " <> locStrRel ctx loc <> ".\n" <>
  "Originally bound at " <> locStrRel ctx boundloc <> "."

prettySource _ _ RigidUnify =
  "is an artificial size invented during unification of functions with anonymous sizes"

prettySource ctx loc (RigidCond t1 t2) =
  "is unknown due to conditional expression at " <> locStrRel ctx loc <> ".\n" <>
  "One branch returns array of type: " <> prettyOneLine t1 <> "\n" <>
  "The other an array of type:       " <> prettyOneLine t2

dimNotes :: (Located a, MonadUnify m) => a -> DimDecl VName -> m Notes
dimNotes ctx (NamedDim d) = do
  c <- M.lookup (qualLeaf d) <$> getConstraints
  case c of
    Just (_, UnknowableSize loc rsrc) ->
      return $ aNote $ quote (pretty d) <> " " <>
      prettySource (srclocOf ctx) loc rsrc
    _ -> return mempty
dimNotes _ _ = return mempty

typeNotes :: (Located a, MonadUnify m) => a -> StructType -> m Notes
typeNotes ctx =
  fmap mconcat . mapM (dimNotes ctx . NamedDim . qualName) .
  S.toList . typeDimNames

class (MonadBreadCrumbs m, MonadError TypeError m) => MonadUnify m where
  getConstraints :: m Constraints
  putConstraints :: Constraints -> m ()
  modifyConstraints :: (Constraints -> Constraints) -> m ()
  modifyConstraints f = do
    x <- getConstraints
    putConstraints $ f x

  newTypeVar :: Monoid als => SrcLoc -> String -> m (TypeBase dim als)
  newDimVar :: SrcLoc -> Rigidity -> String -> m VName

  curLevel :: m Level

normTypeFully :: (Substitutable a, MonadUnify m) => a -> m a
normTypeFully t = do constraints <- getConstraints
                     return $ applySubst (`lookupSubst` constraints) t

normType :: MonadUnify m => StructType -> m StructType
normType t@(Scalar (TypeVar _ _ (TypeName [] v) [])) = do
  constraints <- getConstraints
  case snd <$> M.lookup v constraints of
    Just (Constraint t' _) -> normType t'
    _ -> return t
normType t = return t

normPatternType :: MonadUnify m => PatternType -> m PatternType
normPatternType t@(Scalar (TypeVar als u (TypeName [] v) [])) = do
  constraints <- getConstraints
  case snd <$> M.lookup v constraints of
    Just (Constraint t' _) ->
      normPatternType $ t' `setUniqueness` u `setAliases` als
    _ -> return t
normPatternType t = return t

rigidConstraint :: Constraint -> Bool
rigidConstraint ParamType{} = True
rigidConstraint ParamSize{} = True
rigidConstraint UnknowableSize{} = True
rigidConstraint _ = False

instantiateEmptyArrayDims :: MonadUnify m =>
                             SrcLoc -> String -> Rigidity
                          -> TypeBase (DimDecl VName) als
                          -> m (TypeBase (DimDecl VName) als, [VName])
instantiateEmptyArrayDims tloc desc r = runWriterT . traverseDims onDim
  where onDim PosImmediate AnyDim = inst
        onDim PosParam AnyDim = inst
        onDim _ d = return d
        inst = do
          dim <- lift $ newDimVar tloc r desc
          tell [dim]
          return $ NamedDim $ qualName dim

-- | Is the given type variable the name of an abstract type or type
-- parameter, which we cannot substitute?
isRigid :: VName -> Constraints -> Bool
isRigid v constraints =
  maybe True (rigidConstraint . snd) $ M.lookup v constraints

-- | If the given type variable is nonrigid, what is its level?
isNonRigid :: VName -> Constraints -> Maybe Level
isNonRigid v constraints = do
  (lvl, c) <- M.lookup v constraints
  guard $ not $ rigidConstraint c
  return lvl

unifySharedConstructors :: MonadUnify m =>
                           Usage
                        -> M.Map Name [StructType]
                        -> M.Map Name [StructType]
                        -> m ()
unifySharedConstructors usage cs1 cs2 =
  forM_ (M.toList $ M.intersectionWith (,) cs1 cs2) $ \(c, (f1, f2)) ->
  unifyConstructor c f1 f2
  where unifyConstructor c f1 f2
          | length f1 == length f2 =
              zipWithM_ (unify usage) f1 f2
          | otherwise =
              typeError usage mempty $
              "Cannot unify constructor " ++ quote (prettyName c) ++ "."

indent :: String -> String
indent = intercalate "\n" . map ("  "++) . lines

unifyWith :: MonadUnify m =>
             ([VName] ->
              (VName -> Maybe Int) ->
              DimDecl VName -> DimDecl VName -> m ())
          -> Usage -> StructType -> StructType -> m ()
unifyWith onDims usage orig_t1 orig_t2 =
  breadCrumb (MatchingTypes orig_t1 orig_t2) $
  subunify False mempty orig_t1 orig_t2
  where
    swap True x y = (y, x)
    swap False x y = (x, y)

    subunify ord bound t1 t2 = do
      constraints <- getConstraints

      t1' <- normType t1
      t2' <- normType t2

      let nonrigid v = isNonRigid v constraints

          failure
            -- This case is to avoid repeating the types that are also
            -- shown in the breadcrumb.
            | t1 == orig_t1, t2 == orig_t2 =
                typeError (srclocOf usage) mempty "Types do not match."
            | otherwise =
                typeError (srclocOf usage) mempty $ "Couldn't match expected type\n" ++
                indent (pretty t1') ++ "\nwith actual type\n" ++ indent (pretty t2')

          -- Remove any of the intermediate dimensions we added just
          -- for unification purposes.
          link v lvl = linkVarToType usage v lvl . applySubst unbind
            where unbind d | d `elem` bound = Just $ SizeSubst AnyDim
                           | otherwise      = Nothing

          unifyTypeArg (TypeArgDim d1 _) (TypeArgDim d2 _) =
            onDims' (swap ord d1 d2)
          unifyTypeArg (TypeArgType t _) (TypeArgType arg_t _) =
            subunify ord bound t arg_t
          unifyTypeArg _ _ = typeError usage mempty
            "Cannot unify a type argument with a dimension argument (or vice versa)."

          onDims' (d1, d2) =
            onDims bound nonrigid
            (applySubst (`lookupSubst` constraints) d1)
            (applySubst (`lookupSubst` constraints) d2)

      case (t1', t2') of
        (Scalar (Record fs),
         Scalar (Record arg_fs))
          | M.keys fs == M.keys arg_fs ->
              forM_ (M.toList $ M.intersectionWith (,) fs arg_fs) $ \(k, (k_t1, k_t2)) ->
              breadCrumb (MatchingFields k) $ subunify ord bound k_t1 k_t2

        (Scalar (TypeVar _ _ (TypeName _ tn) targs),
         Scalar (TypeVar _ _ (TypeName _ arg_tn) arg_targs))
          | tn == arg_tn, length targs == length arg_targs ->
              zipWithM_ unifyTypeArg targs arg_targs

        (Scalar (TypeVar _ _ (TypeName [] v1) []),
         Scalar (TypeVar _ _ (TypeName [] v2) [])) ->
          case (nonrigid v1, nonrigid v2) of
            (Nothing, Nothing) -> failure
            (Just lvl1, Nothing) -> link v1 lvl1 t2'
            (Nothing, Just lvl2) -> link v2 lvl2 t1'
            (Just lvl1, Just lvl2)
              | lvl1 <= lvl2 -> link v1 lvl1 t2'
              | otherwise    -> link v2 lvl2 t1'

        (Scalar (TypeVar _ _ (TypeName [] v1) []), _)
          | Just lvl <- nonrigid v1 ->
              link v1 lvl t2'
        (_, Scalar (TypeVar _ _ (TypeName [] v2) []))
          | Just lvl <- nonrigid v2 ->
              link v2 lvl t1'

        (Scalar (Arrow _ p1 a1 b1),
         Scalar (Arrow _ p2 a2 b2)) -> do
          let (r1, r2) = swap ord (Rigid RigidUnify) Nonrigid
          (a1', a1_dims) <- instantiateEmptyArrayDims (srclocOf usage) "anonymous" r1 a1
          (a2', a2_dims) <- instantiateEmptyArrayDims (srclocOf usage) "anonymous" r2 a2
          let bound' = bound <> mapMaybe pname [p1, p2] <> a1_dims <> a2_dims
          subunify (not ord) bound a1' a2'
          subunify ord bound' b1' b2'
          where (b1', b2') =
                  -- Replace one parameter name with the other in the
                  -- return type, in case of dependent types.  I.e.,
                  -- we want type '(n: i32) -> [n]i32' to unify with
                  -- type '(x: i32) -> [x]i32'.
                  case (p1, p2) of
                    (Named p1', Named p2') ->
                      let f v | v == p2' = Just $ SizeSubst $ NamedDim $ qualName p1'
                              | otherwise = Nothing
                      in (b1, applySubst f b2)

                    (_, _) ->
                      (b1, b2)

                pname (Named x) = Just x
                pname Unnamed = Nothing

        (Array{}, Array{})
          | ShapeDecl (t1_d : _) <- arrayShape t1',
            ShapeDecl (t2_d : _) <- arrayShape t2',
            Just t1'' <- peelArray 1 t1',
            Just t2'' <- peelArray 1 t2' -> do
              onDims' (swap ord t1_d t2_d)
              subunify ord bound t1'' t2''

        (Scalar (Sum cs),
         Scalar (Sum arg_cs))
          | M.keys cs == M.keys arg_cs ->
              unifySharedConstructors usage cs arg_cs

        _ | t1' == t2' -> return ()
          | otherwise -> failure

-- | Unifies two types.
unify :: MonadUnify m => Usage -> StructType -> StructType -> m ()
unify usage = unifyWith onDims usage
  where onDims _ _ d1 d2
          | isJust $ unifyDims d1 d2 = return ()
        onDims _ nonrigid (NamedDim (QualName _ d1)) d2
          | Just lvl1 <- nonrigid d1 =
              linkVarToDim usage d1 lvl1 d2
        onDims _ nonrigid d1 (NamedDim (QualName _ d2))
          | Just lvl2 <- nonrigid d2 =
              linkVarToDim usage d2 lvl2 d1
        onDims _ _ d1 d2 = do
          notes <- (<>) <$> dimNotes usage d1 <*> dimNotes usage d2
          typeError usage notes $
            "Dimensions " ++ quote (pretty d1) ++
            " and " ++ quote (pretty d2) ++ " do not match."

-- | @expect super sub@ checks that @sub@ is a subtype of @super@.
expect :: MonadUnify m => Usage -> StructType -> StructType -> m ()
expect usage = unifyWith onDims usage
  where onDims _ _ AnyDim _ = return ()
        onDims _ _ d1 d2
          | d1 == d2 = return ()
        onDims bound nonrigid (NamedDim (QualName _ d1)) d2
          | Just lvl1 <- nonrigid d1, d2 /= AnyDim, not $ boundParam bound d2 =
              linkVarToDim usage d1 lvl1 d2
        onDims bound nonrigid d1 (NamedDim (QualName _ d2))
          | Just lvl2 <- nonrigid d2, not $ boundParam bound d1 =
              linkVarToDim usage d2 lvl2 d1
        onDims _ _ d1 d2 = do
          notes <- (<>) <$> dimNotes usage d1 <*> dimNotes usage d2
          typeError usage notes $ "Dimensions " ++ quote (pretty d1) ++
            " and " ++ quote (pretty d2) ++ " do not match."

        boundParam bound (NamedDim (QualName _ d)) = d `elem` bound
        boundParam _ _ = False

hasEmptyDims :: StructType -> Bool
hasEmptyDims = biany empty (const False)
  where empty AnyDim = True
        empty _ = False

occursCheck :: MonadUnify m => Usage -> VName -> StructType -> m ()
occursCheck usage vn tp =
  when (vn `S.member` typeVars tp) $
  typeError usage mempty $ "Occurs check: cannot instantiate " ++
  prettyName vn ++ " with " ++ pretty tp

scopeCheck :: MonadUnify m => Usage -> VName -> Level -> StructType -> m ()
scopeCheck usage vn max_lvl tp = do
  constraints <- getConstraints
  checkType constraints tp
  where checkType constraints t =
          mapM_ (check constraints) $ typeVars t <> typeDimNames t

        check constraints v
          | Just (lvl, c) <- M.lookup v constraints,
            lvl > max_lvl =
              if rigidConstraint c
              then scopeViolation v
              else modifyConstraints $ M.insert v (max_lvl, c)

          | otherwise =
              return ()

        scopeViolation v = do
          notes <- typeNotes usage tp
          typeError usage notes $ "Cannot unify type\n" ++
            indent (prettyOneLine tp) ++
            "\nwith " ++ quote (prettyName vn) ++ " (scope violation).\n" ++
            "This is because " ++ quote (prettyName v) ++ " is rigidly bound in a deeper scope."

linkVarToType :: MonadUnify m => Usage -> VName -> Level -> StructType -> m ()
linkVarToType usage vn lvl tp = do
  occursCheck usage vn tp
  scopeCheck usage vn lvl tp

  constraints <- getConstraints
  let tp' = removeUniqueness tp
  modifyConstraints $ M.insert vn (lvl, Constraint tp' usage)
  case snd <$> M.lookup vn constraints of

    Just (NoConstraint Unlifted unlift_usage) -> do
      zeroOrderType usage (show unlift_usage) tp'

      when (hasEmptyDims tp') $
        typeError usage mempty $ "Type variable " ++ prettyName vn ++
        " cannot be instantiated with type containing anonymous sizes:\n" ++
        indent (pretty tp) ++
        "\nThis is usually because the size of arrays returned a higher-order function argument\n" ++
        "cannot be determined statically.  Add type annotation to clarify."

    Just (Equality _) ->
      equalityType usage tp'

    Just (Overloaded ts old_usage)
      | tp `notElem` map (Scalar . Prim) ts ->
          case tp' of
            Scalar (TypeVar _ _ (TypeName [] v) [])
              | not $ isRigid v constraints ->
                  linkVarToTypes usage v ts
            _ ->
              typeError usage mempty $ "Cannot unify " ++ quote (prettyName vn) ++
              "' with type\n" ++ indent (pretty tp) ++ "\nas " ++
              quote (prettyName vn) ++ " must be one of " ++
              intercalate ", " (map pretty ts) ++
              " due to " ++ show old_usage ++ ")."

    Just (HasFields required_fields old_usage) ->
      case tp of
        Scalar (Record tp_fields)
          | all (`M.member` tp_fields) $ M.keys required_fields ->
              mapM_ (uncurry $ unify usage) $ M.elems $
              M.intersectionWith (,) required_fields tp_fields
        Scalar (TypeVar _ _ (TypeName [] v) [])
          | not $ isRigid v constraints ->
              modifyConstraints $ M.insert v
              (lvl, HasFields required_fields old_usage)
        _ ->
          typeError usage mempty $
          "Cannot unify " ++ quote (prettyName vn) ++ " with type\n" ++
          indent (pretty tp) ++ "\nas " ++ quote (prettyName vn) ++
          " must be a record with fields\n" ++
          pretty (Record required_fields) ++
          "\ndue to " ++ show old_usage ++ "."

    Just (HasConstrs required_cs old_usage) ->
      case tp of
        Scalar (Sum ts)
          | all (`M.member` ts) $ M.keys required_cs ->
              unifySharedConstructors usage required_cs ts
        Scalar (TypeVar _ _ (TypeName [] v) [])
          | not $ isRigid v constraints -> do
              case M.lookup v constraints of
                Just (_, HasConstrs v_cs _) ->
                  unifySharedConstructors usage required_cs v_cs
                _ -> return ()
              modifyConstraints $ M.insertWith combineConstrs v
                (lvl, HasConstrs required_cs old_usage)
              where combineConstrs (_, HasConstrs cs1 usage1) (_, HasConstrs cs2 _) =
                      (lvl, HasConstrs (M.union cs1 cs2) usage1)
                    combineConstrs hasCs _ = hasCs
        _ -> noSumType

    _ -> return ()

  where noSumType = typeError usage mempty "Cannot unify a sum type with a non-sum type"

linkVarToDim :: MonadUnify m => Usage -> VName -> Level -> DimDecl VName -> m ()
linkVarToDim usage vn lvl dim = do
  constraints <- getConstraints

  case dim of
    NamedDim dim'
      | Just (dim_lvl, c) <- qualLeaf dim' `M.lookup` constraints,
        dim_lvl > lvl ->
          case c of
            ParamSize{} -> do
              notes <- dimNotes usage dim
              typeError usage notes $
                "Cannot unify size variable " ++ quote (pretty dim') ++
                " with " ++ quote (prettyName vn) ++ " (scope violation).\n" ++
                "This is because " ++ quote (pretty dim') ++
                " is rigidly bound in a deeper scope."
            _ -> modifyConstraints $ M.insert (qualLeaf dim') (lvl, c)
    _ -> return ()

  modifyConstraints $ M.insert vn (lvl, Size (Just dim) usage)

removeUniqueness :: TypeBase dim as -> TypeBase dim as
removeUniqueness (Scalar (Record ets)) =
  Scalar $ Record $ fmap removeUniqueness ets
removeUniqueness (Scalar (Arrow als p t1 t2)) =
  Scalar $ Arrow als p (removeUniqueness t1) (removeUniqueness t2)
removeUniqueness (Scalar (Sum cs)) =
  Scalar $ Sum $ (fmap . fmap) removeUniqueness cs
removeUniqueness t = t `setUniqueness` Nonunique

mustBeOneOf :: MonadUnify m => [PrimType] -> Usage -> StructType -> m ()
mustBeOneOf [req_t] usage t = unify usage (Scalar (Prim req_t)) t
mustBeOneOf ts usage t = do
  t' <- normType t
  constraints <- getConstraints
  let isRigid' v = isRigid v constraints

  case t' of
    Scalar (TypeVar _ _ (TypeName [] v) [])
      | not $ isRigid' v -> linkVarToTypes usage v ts

    Scalar (Prim pt) | pt `elem` ts -> return ()

    _ -> failure

  where failure = typeError usage mempty $ "Cannot unify type \"" ++ pretty t ++
                  "\" with any of " ++ intercalate "," (map pretty ts) ++ "."

linkVarToTypes :: MonadUnify m => Usage -> VName -> [PrimType] -> m ()
linkVarToTypes usage vn ts = do
  vn_constraint <- M.lookup vn <$> getConstraints
  case vn_constraint of
    Just (lvl, Overloaded vn_ts vn_usage) ->
      case ts `intersect` vn_ts of
        [] -> typeError usage mempty $ "Type constrained to one of " ++
              intercalate "," (map pretty ts) ++ " but also one of " ++
              intercalate "," (map pretty vn_ts) ++ " due to " ++ show vn_usage ++ "."
        ts' -> modifyConstraints $ M.insert vn (lvl, Overloaded ts' usage)

    Just (_, HasConstrs _ vn_usage) ->
      typeError usage mempty $ "Type constrained to one of " ++
      intercalate "," (map pretty ts) ++ ", but also inferred to be sum type due to " ++
      show vn_usage ++ "."

    Just (_, HasFields _ vn_usage) ->
      typeError usage mempty $ "Type constrained to one of " ++
      intercalate "," (map pretty ts) ++ ", but also inferred to be record due to " ++
      show vn_usage ++ "."

    Just (lvl, _) -> modifyConstraints $ M.insert vn (lvl, Overloaded ts usage)

    Nothing ->
      typeError usage mempty $
      "Cannot constrain type to one of " ++ intercalate "," (map pretty ts)

equalityType :: (MonadUnify m, Pretty (ShapeDecl dim), Monoid as) =>
                Usage -> TypeBase dim as -> m ()
equalityType usage t = do
  unless (orderZero t) $
    typeError usage mempty $
    "Type \"" ++ pretty t ++ "\" does not support equality (is higher-order)."
  mapM_ mustBeEquality $ typeVars t
  where mustBeEquality vn = do
          constraints <- getConstraints
          case M.lookup vn constraints of
            Just (_, Constraint (Scalar (TypeVar _ _ (TypeName [] vn') [])) _) ->
              mustBeEquality vn'
            Just (_, Constraint vn_t cusage)
              | not $ orderZero vn_t ->
                  typeError usage mempty $
                  unlines ["Type \"" ++ pretty t ++ "\" does not support equality.",
                           "Constrained to be higher-order due to " ++ show cusage ++ "."]
              | otherwise -> return ()
            Just (lvl, NoConstraint _ _) ->
              modifyConstraints $ M.insert vn (lvl, Equality usage)
            Just (_, Overloaded _ _) ->
              return () -- All primtypes support equality.
            Just (_, Equality{}) ->
              return ()
            Just (_, HasConstrs cs _) ->
              mapM_ (equalityType usage) $ concat $ M.elems cs
            _ ->
              typeError usage mempty $ "Type " ++ pretty (prettyName vn) ++
              " does not support equality."

zeroOrderType :: (MonadUnify m, Pretty (ShapeDecl dim), Monoid as) =>
                 Usage -> String -> TypeBase dim as -> m ()
zeroOrderType usage desc t = do
  unless (orderZero t) $
    typeError usage mempty $ "Type " ++ desc ++
    " must not be functional, but is " ++ quote (pretty t) ++ "."
  mapM_ mustBeZeroOrder . S.toList . typeVars $ t
  where mustBeZeroOrder vn = do
          constraints <- getConstraints
          case M.lookup vn constraints of
            Just (_, Constraint vn_t old_usage)
              | not $ orderZero t ->
                typeError usage mempty $ "Type " ++ desc ++
                " must be non-function, but inferred to be " ++
                quote (pretty vn_t) ++ " due to " ++ show old_usage ++ "."
            Just (lvl, NoConstraint _ _) ->
              modifyConstraints $ M.insert vn (lvl, NoConstraint Unlifted usage)
            Just (_, ParamType Lifted ploc) ->
              typeError usage mempty $ "Type " ++ desc ++
              " must be non-function, but type parameter " ++ quote (prettyName vn) ++ " at " ++
              locStr ploc ++ " may be a function."
            _ -> return ()

-- | In @mustHaveConstr usage c t fs@, the type @t@ must have a
-- constructor named @c@ that takes arguments of types @ts@.
mustHaveConstr :: MonadUnify m =>
                  Usage -> Name -> StructType -> [StructType] -> m ()
mustHaveConstr usage c t fs = do
  constraints <- getConstraints
  case t of
    Scalar (TypeVar _ _ (TypeName _ tn) [])
      | Just (lvl, NoConstraint{}) <- M.lookup tn constraints -> do
          mapM_ (scopeCheck usage tn lvl) fs
          modifyConstraints $ M.insert tn (lvl, HasConstrs (M.singleton c fs) usage)
      | Just (lvl, HasConstrs cs _) <- M.lookup tn constraints ->
        case M.lookup c cs of
          Nothing  -> modifyConstraints $ M.insert tn (lvl, HasConstrs (M.insert c fs cs) usage)
          Just fs'
            | length fs == length fs' -> zipWithM_ (unify usage) fs fs'
            | otherwise ->
                typeError usage mempty $ "Different arity for constructor "
                ++ quote (pretty c) ++ "."

    Scalar (Sum cs) ->
      case M.lookup c cs of
        Nothing ->
          typeError usage mempty $
          "Constuctor " ++ quote (pretty c) ++ " not present in type."
        Just fs'
            | length fs == length fs' -> zipWithM_ (unify usage) fs fs'
            | otherwise ->
                typeError usage mempty $
                "Different arity for constructor " ++ quote (pretty c) ++ "."

    _ -> do unify usage t $ Scalar $ Sum $ M.singleton c fs
            return ()

mustHaveField :: MonadUnify m =>
                 Usage -> Name -> PatternType -> m PatternType
mustHaveField usage l t = do
  constraints <- getConstraints
  l_type <- newTypeVar (srclocOf usage) "t"
  let l_type' = toStruct l_type
  case t of
    Scalar (TypeVar _ _ (TypeName _ tn) [])
      | Just (lvl, NoConstraint{}) <- M.lookup tn constraints -> do
          scopeCheck usage tn lvl l_type'
          modifyConstraints $ M.insert tn (lvl, HasFields (M.singleton l l_type') usage)
          return l_type
      | Just (lvl, HasFields fields _) <- M.lookup tn constraints -> do
          case M.lookup l fields of
            Just t' -> unify usage l_type' t'
            Nothing -> modifyConstraints $ M.insert tn
                       (lvl, HasFields (M.insert l l_type' fields) usage)
          return l_type
    Scalar (Record fields)
      | Just t' <- M.lookup l fields -> do
          unify usage l_type' $ toStruct t'
          return t'
      | otherwise ->
          typeError usage mempty $
            "Attempt to access field " ++ quote (pretty l) ++ " of value of type " ++
            pretty (toStructural t) ++ "."
    _ -> do unify usage (toStruct t) $ Scalar $ Record $ M.singleton l l_type'
            return l_type

-- | Replace dimension mismatches with AnyDim.  Where one of the types
-- contains an AnyDim dimension, the corresponding dimension in the
-- other type is used.
anyDimOnMismatch :: Monoid as =>
                    TypeBase (DimDecl VName) as -> TypeBase (DimDecl VName) as
                 -> (TypeBase (DimDecl VName) as, [(DimDecl VName, DimDecl VName)])
anyDimOnMismatch t1 t2 = runWriter $ matchDims onDims t1 t2
  where onDims AnyDim d2 = return d2
        onDims d1 AnyDim = return d1
        onDims d1 d2
          | d1 == d2 = return d1
          | otherwise = do tell [(d1, d2)]
                           return AnyDim

-- | Like unification, but creates new size variables where mismatches
-- occur.  Returns the new dimensions thus created.
unifyMostCommon :: MonadUnify m =>
                   Usage -> PatternType -> PatternType -> m (PatternType, [VName])
unifyMostCommon usage t1 t2 = do
  -- We are ignoring the dimensions here, because any mismatches
  -- should be turned into fresh size variables.
  unify usage (toStruct (anyDimShapeAnnotations t1))
              (toStruct (anyDimShapeAnnotations t2))
  t1' <- normTypeFully t1
  t2' <- normTypeFully t2
  let rsrc = RigidCond (toStruct t1') (toStruct t2')
  instantiateEmptyArrayDims (srclocOf usage) "differ" (Rigid rsrc) $
    fst $ anyDimOnMismatch t1' t2'

-- Simple MonadUnify implementation.

type UnifyMState = (Constraints, Int)

newtype UnifyM a = UnifyM (RWST [BreadCrumb] () UnifyMState (Except TypeError) a)
  deriving (Monad, Functor, Applicative,
            MonadReader [BreadCrumb],
            MonadState UnifyMState,
            MonadError TypeError)

newVar :: String -> UnifyM VName
newVar name = do
  (x, i) <- get
  put (x, i+1)
  return $ VName (mkTypeVarName name i) i

instance MonadUnify UnifyM where
  getConstraints = gets fst
  putConstraints x = modify $ \(_, i) -> (x, i)

  newTypeVar loc name = do
    v <- newVar name
    modifyConstraints $ M.insert v (0, NoConstraint Lifted $ Usage Nothing loc)
    return $ Scalar $ TypeVar mempty Nonunique (typeName v) []

  newDimVar loc rigidity name = do
    dim <- newVar name
    case rigidity of
      Rigid src -> modifyConstraints $ M.insert dim (0, UnknowableSize loc src)
      Nonrigid -> modifyConstraints $ M.insert dim (0, Size Nothing $ Usage Nothing loc)
    return dim

  curLevel = pure 0

-- | Construct a the name of a new type variable given a base
-- description and a tag number (note that this is distinct from
-- actually constructing a VName; the tag here is intended for human
-- consumption but the machine does not care).
mkTypeVarName :: String -> Int -> Name
mkTypeVarName desc i =
  nameFromString $ desc ++ mapMaybe subscript (show i)
  where subscript = flip lookup $ zip "0123456789" "₀₁₂₃₄₅₆₇₈₉"

instance MonadBreadCrumbs UnifyM where
  breadCrumb bc = local (bc:)
  getBreadCrumbs = ask

runUnifyM :: [TypeParam] -> UnifyM a -> Either TypeError a
runUnifyM tparams (UnifyM m) = runExcept $ fst <$> evalRWST m [] (constraints, 0)
  where constraints = M.fromList $ map f tparams
        f (TypeParamDim p loc) = (p, (0, Size Nothing $ Usage Nothing loc))
        f (TypeParamType l p loc) = (p, (0, NoConstraint l $ Usage Nothing loc))

-- | Perform a unification of two types outside a monadic context.
-- The type parameters are allowed to be instantiated; all other types
-- are considered rigid.
doUnification :: SrcLoc -> [TypeParam]
              -> StructType -> StructType
              -> Either TypeError StructType
doUnification loc tparams t1 t2 = runUnifyM tparams $ do
  let rsrc = RigidUnify
  (t1', _) <- instantiateEmptyArrayDims loc "n" (Rigid rsrc) t1
  (t2', _) <- instantiateEmptyArrayDims loc "m" (Rigid rsrc) t2
  expect (Usage Nothing loc) t1' t2'
  normTypeFully t2
