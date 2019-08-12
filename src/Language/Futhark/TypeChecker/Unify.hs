{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Futhark.TypeChecker.Unify
  ( Constraint(..)
  , Usage
  , mkUsage
  , mkUsage'
  , Constraints
  , lookupSubst
  , MonadUnify(..)
  , BreadCrumb(..)
  , typeError
  , mkTypeVarName

  , zeroOrderType
  , mustHaveConstr
  , mustHaveField
  , mustBeOneOf
  , equalityType
  , normaliseType

  , unify
  , doUnification
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.List
import Data.Loc
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Language.Futhark
import Language.Futhark.TypeChecker.Monad hiding (BoundV)
import Language.Futhark.TypeChecker.Types
import Futhark.Util.Pretty (Pretty)

-- | Mapping from fresh type variables, instantiated from the type
-- schemes of polymorphic functions, to (possibly) specific types as
-- determined on application and the location of that application, or
-- a partial constraint on their type.
type Constraints = M.Map VName Constraint

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

data Constraint = NoConstraint (Maybe Liftedness) Usage
                | ParamType Liftedness SrcLoc
                | Constraint (TypeBase () ()) Usage
                | Overloaded [PrimType] Usage
                | HasFields (M.Map Name (TypeBase () ())) Usage
                | Equality Usage
                | HasConstrs (M.Map Name [TypeBase () ()]) Usage
                deriving Show

instance Located Constraint where
  locOf (NoConstraint _ usage) = locOf usage
  locOf (ParamType _ loc) = locOf loc
  locOf (Constraint _ usage) = locOf usage
  locOf (Overloaded _ usage) = locOf usage
  locOf (HasFields _ usage) = locOf usage
  locOf (Equality usage) = locOf usage
  locOf (HasConstrs _ usage) = locOf usage

lookupSubst :: VName -> Constraints -> Maybe (Subst (TypeBase () ()))
lookupSubst v constraints = case M.lookup v constraints of
                              Just (Constraint t _) -> Just $ Subst t
                              Just Overloaded{} -> Just PrimSubst
                              _ -> Nothing

class (MonadBreadCrumbs m, MonadError TypeError m) => MonadUnify m where
  getConstraints :: m Constraints
  putConstraints :: Constraints -> m ()
  modifyConstraints :: (Constraints -> Constraints) -> m ()
  modifyConstraints f = do
    x <- getConstraints
    putConstraints $ f x

  newTypeVar :: Monoid als => SrcLoc -> String -> m (TypeBase dim als)

normaliseType :: (Substitutable a, MonadUnify m) => a -> m a
normaliseType t = do constraints <- getConstraints
                     return $ applySubst (`lookupSubst` constraints) t

-- | Is the given type variable actually the name of an abstract type
-- or type parameter, which we cannot substitute?
isRigid :: VName -> Constraints -> Bool
isRigid v constraints = case M.lookup v constraints of
                             Nothing -> True
                             Just ParamType{} -> True
                             _ -> False

unifySharedConstructors :: MonadUnify m =>
                           Usage
                        -> M.Map Name [TypeBase () ()]
                        -> M.Map Name [TypeBase () ()]
                        -> m ()
unifySharedConstructors usage cs1 cs2 =
  forM_ (M.toList $ M.intersectionWith (,) cs1 cs2) $ \(c, (f1, f2)) ->
  unifyConstructor c f1 f2
  where unifyConstructor c f1 f2
          | length f1 == length f2 =
              zipWithM_ (unify usage) f1 f2
          | otherwise = typeError usage $ "Cannot unify constructor " ++
                        quote (prettyName c) ++ "."

indent :: String -> String
indent = intercalate "\n" . map ("  "++) . lines

-- | Unifies two types.
unify :: MonadUnify m => Usage -> TypeBase () () -> TypeBase () () -> m ()
unify usage orig_t1 orig_t2 = do
  orig_t1' <- normaliseType orig_t1
  orig_t2' <- normaliseType orig_t2
  breadCrumb (MatchingTypes orig_t1' orig_t2') $ subunify orig_t1 orig_t2
  where
    subunify t1 t2 = do
      constraints <- getConstraints

      let isRigid' v = isRigid v constraints
          t1' = applySubst (`lookupSubst` constraints) t1
          t2' = applySubst (`lookupSubst` constraints) t2

          failure
            -- This case is to avoid repeating the types that are also
            -- shown in the breadcrumb.
            | t1 == orig_t1, t2 == orig_t2 =
                typeError (srclocOf usage) "Types do not match."
            | otherwise =
                typeError (srclocOf usage) $ "Couldn't match expected type\n" ++
                indent (pretty t1') ++ "\nwith actual type\n" ++ indent (pretty t2')

      case (t1', t2') of
        _ | t1' == t2' -> return ()

        (Scalar (Record fs),
         Scalar (Record arg_fs))
          | M.keys fs == M.keys arg_fs ->
              forM_ (M.toList $ M.intersectionWith (,) fs arg_fs) $ \(k, (k_t1, k_t2)) ->
              breadCrumb (MatchingFields k) $ subunify k_t1 k_t2

        (Scalar (TypeVar _ _ (TypeName _ tn) targs),
         Scalar (TypeVar _ _ (TypeName _ arg_tn) arg_targs))
          | tn == arg_tn, length targs == length arg_targs ->
              zipWithM_ unifyTypeArg targs arg_targs

        (Scalar (TypeVar _ _ (TypeName [] v1) []),
         Scalar (TypeVar _ _ (TypeName [] v2) [])) ->
          case (isRigid' v1, isRigid' v2) of
            (True, True) -> failure
            (True, False) -> linkVarToType usage v2 t1'
            (False, True) -> linkVarToType usage v1 t2'
            (False, False) -> linkVarToType usage v1 t2'

        (Scalar (TypeVar _ _ (TypeName [] v1) []), _)
          | not $ isRigid' v1 ->
              linkVarToType usage v1 t2'
        (_, Scalar (TypeVar _ _ (TypeName [] v2) []))
          | not $ isRigid' v2 ->
              linkVarToType usage v2 t1'

        (Scalar (Arrow _ _ a1 b1),
         Scalar (Arrow _ _ a2 b2)) -> do
          subunify a1 a2
          subunify b1 b2

        (Array{}, Array{})
          | Just t1'' <- peelArray 1 t1',
            Just t2'' <- peelArray 1 t2' ->
              subunify t1'' t2''

        (Scalar (Sum cs),
         Scalar (Sum arg_cs))
          | M.keys cs == M.keys arg_cs ->
              unifySharedConstructors usage cs arg_cs
        (_, _) -> failure

      where unifyTypeArg TypeArgDim{} TypeArgDim{} = return ()
            unifyTypeArg (TypeArgType t _) (TypeArgType arg_t _) =
              subunify t arg_t
            unifyTypeArg _ _ = typeError usage
              "Cannot unify a type argument with a dimension argument (or vice versa)."

applySubstInConstraint :: VName -> Subst (TypeBase () ()) -> Constraint -> Constraint
applySubstInConstraint vn subst (Constraint t loc) =
  Constraint (applySubst (flip M.lookup $ M.singleton vn subst) t) loc
applySubstInConstraint vn subst (HasFields fs loc) =
  HasFields (M.map (applySubst (flip M.lookup $ M.singleton vn subst)) fs) loc
applySubstInConstraint _ _ (NoConstraint l loc) = NoConstraint l loc
applySubstInConstraint _ _ (Overloaded ts usage) = Overloaded ts usage
applySubstInConstraint _ _ (Equality loc) = Equality loc
applySubstInConstraint _ _ (ParamType l loc) = ParamType l loc
applySubstInConstraint vn subst (HasConstrs cs loc) =
  HasConstrs (M.map (map (applySubst (flip M.lookup $ M.singleton vn subst))) cs) loc

linkVarToType :: MonadUnify m => Usage -> VName -> TypeBase () () -> m ()
linkVarToType usage vn tp = do
  constraints <- getConstraints
  if vn `S.member` typeVars tp
    then typeError usage $ "Occurs check: cannot instantiate " ++
         prettyName vn ++ " with " ++ pretty tp'
    else do modifyConstraints $ M.insert vn $ Constraint tp' usage
            modifyConstraints $ M.map $ applySubstInConstraint vn $ Subst tp'
            case M.lookup vn constraints of

              Just (NoConstraint (Just Unlifted) unlift_usage) ->
                zeroOrderType usage (show unlift_usage) tp'

              Just (Equality _) ->
                equalityType usage tp'

              Just (Overloaded ts old_usage)
                | tp `notElem` map (Scalar . Prim) ts ->
                    case tp' of
                      Scalar (TypeVar _ _ (TypeName [] v) [])
                        | not $ isRigid v constraints -> linkVarToTypes usage v ts
                      _ ->
                        typeError usage $ "Cannot unify " ++ quote (prettyName vn) ++
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
                        modifyConstraints $ M.insert v $
                        HasFields required_fields old_usage
                  _ ->
                    typeError usage $
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
                          Just (HasConstrs v_cs _) ->
                            unifySharedConstructors usage required_cs v_cs
                          _ -> return ()
                        modifyConstraints $ M.insertWith combineConstrs v $
                          HasConstrs required_cs old_usage
                        where combineConstrs (HasConstrs cs1 usage1) (HasConstrs cs2 _) =
                                HasConstrs (M.union cs1 cs2) usage1
                              combineConstrs hasCs _ = hasCs
                  _ -> noSumType

              _ -> return ()

  where tp' = removeUniqueness tp
        noSumType = typeError usage "Cannot unify a sum type with a non-sum type"

removeUniqueness :: TypeBase dim as -> TypeBase dim as
removeUniqueness (Scalar (Record ets)) =
  Scalar $ Record $ fmap removeUniqueness ets
removeUniqueness (Scalar (Arrow als p t1 t2)) =
  Scalar $ Arrow als p (removeUniqueness t1) (removeUniqueness t2)
removeUniqueness (Scalar (Sum cs)) =
  Scalar $ Sum $ (fmap . fmap) removeUniqueness cs
removeUniqueness t = t `setUniqueness` Nonunique

mustBeOneOf :: MonadUnify m => [PrimType] -> Usage -> TypeBase () () -> m ()
mustBeOneOf [req_t] loc t = unify loc (Scalar (Prim req_t)) t
mustBeOneOf ts loc t = do
  constraints <- getConstraints
  let t' = applySubst (`lookupSubst` constraints) t
      isRigid' v = isRigid v constraints

  case t' of
    Scalar (TypeVar _ _ (TypeName [] v) [])
      | not $ isRigid' v -> linkVarToTypes loc v ts

    Scalar (Prim pt) | pt `elem` ts -> return ()

    _ -> failure

  where failure = typeError loc $ "Cannot unify type \"" ++ pretty t ++
                  "\" with any of " ++ intercalate "," (map pretty ts) ++ "."

linkVarToTypes :: MonadUnify m => Usage -> VName -> [PrimType] -> m ()
linkVarToTypes usage vn ts = do
  vn_constraint <- M.lookup vn <$> getConstraints
  case vn_constraint of
    Just (Overloaded vn_ts vn_usage) ->
      case ts `intersect` vn_ts of
        [] -> typeError usage $ "Type constrained to one of " ++
              intercalate "," (map pretty ts) ++ " but also one of " ++
              intercalate "," (map pretty vn_ts) ++ " due to " ++ show vn_usage ++ "."
        ts' -> modifyConstraints $ M.insert vn $ Overloaded ts' usage

    _ -> modifyConstraints $ M.insert vn $ Overloaded ts usage

equalityType :: (MonadUnify m, Pretty (ShapeDecl dim), Monoid as) =>
                Usage -> TypeBase dim as -> m ()
equalityType usage t = do
  unless (orderZero t) $
    typeError usage $
    "Type \"" ++ pretty t ++ "\" does not support equality (is higher-order)."
  mapM_ mustBeEquality $ typeVars t
  where mustBeEquality vn = do
          constraints <- getConstraints
          case M.lookup vn constraints of
            Just (Constraint (Scalar (TypeVar _ _ (TypeName [] vn') [])) _) ->
              mustBeEquality vn'
            Just (Constraint vn_t cusage)
              | not $ orderZero vn_t ->
                  typeError usage $
                  unlines ["Type \"" ++ pretty t ++ "\" does not support equality.",
                           "Constrained to be higher-order due to " ++ show cusage ++ "."]
              | otherwise -> return ()
            Just (NoConstraint _ _) ->
              modifyConstraints $ M.insert vn (Equality usage)
            Just (Overloaded _ _) ->
              return () -- All primtypes support equality.
            Just (HasConstrs cs _) ->
              mapM_ (equalityType usage) $ concat $ M.elems cs
            _ ->
              typeError usage $ "Type " ++ pretty (prettyName vn) ++
              " does not support equality."

zeroOrderType :: (MonadUnify m, Pretty (ShapeDecl dim), Monoid as) =>
                 Usage -> String -> TypeBase dim as -> m ()
zeroOrderType usage desc t = do
  unless (orderZero t) $
    typeError usage $ "Type " ++ desc ++
    " must not be functional, but is " ++ quote (pretty t) ++ "."
  mapM_ mustBeZeroOrder . S.toList . typeVars $ t
  where mustBeZeroOrder vn = do
          constraints <- getConstraints
          case M.lookup vn constraints of
            Just (Constraint vn_t old_usage)
              | not $ orderZero t ->
                typeError usage $ "Type " ++ desc ++
                " must be non-function, but inferred to be " ++
                quote (pretty vn_t) ++ " due to " ++ show old_usage ++ "."
            Just (NoConstraint _ _) ->
              modifyConstraints $ M.insert vn (NoConstraint (Just Unlifted) usage)
            Just (ParamType Lifted ploc) ->
              typeError usage $ "Type " ++ desc ++
              " must be non-function, but type parameter " ++ quote (prettyName vn) ++ " at " ++
              locStr ploc ++ " may be a function."
            _ -> return ()

-- | In @mustHaveConstr usage c t fs@, the type @t@ must have a
-- constructor named @c@ that takes arguments of types @ts@.
mustHaveConstr :: MonadUnify m =>
                  Usage -> Name -> TypeBase dim as -> [TypeBase () ()] -> m ()
mustHaveConstr usage c t fs = do
  let struct_f = toStructural <$> fs
  constraints <- getConstraints
  case t of
    Scalar (TypeVar _ _ (TypeName _ tn) [])
      | Just NoConstraint{} <- M.lookup tn constraints ->
          modifyConstraints $ M.insert tn $ HasConstrs (M.singleton c struct_f) usage
      | Just (HasConstrs cs _) <- M.lookup tn constraints ->
        case M.lookup c cs of
          Nothing  -> modifyConstraints $ M.insert tn $ HasConstrs (M.insert c fs cs) usage
          Just fs'
            | length fs == length fs' -> zipWithM_ (unify usage) fs fs'
            | otherwise -> typeError usage $ "Different arity for constructor "
                           ++ quote (pretty c) ++ "."

    Scalar (Sum cs) ->
      case M.lookup c cs of
        Nothing -> typeError usage $ "Constuctor " ++ quote (pretty c) ++ " not present in type."
        Just fs'
            | length fs == length fs' -> zipWithM_ (unify usage) fs (toStructural <$> fs')
            | otherwise -> typeError usage $ "Different arity for constructor " ++
                           quote (pretty c) ++ "."

    _ -> do unify usage (toStructural t) $ Scalar $ Sum $ M.singleton c fs
            return ()

mustHaveField :: (MonadUnify m, Monoid as) =>
                 Usage -> Name -> TypeBase dim as -> m (TypeBase dim as)
mustHaveField usage l t = do
  constraints <- getConstraints
  l_type <- newTypeVar (srclocOf usage) "t"
  let l_type' = toStructural l_type
  case t of
    Scalar (TypeVar _ _ (TypeName _ tn) [])
      | Just NoConstraint{} <- M.lookup tn constraints -> do
          modifyConstraints $ M.insert tn $ HasFields (M.singleton l l_type') usage
          return l_type
      | Just (HasFields fields _) <- M.lookup tn constraints -> do
          case M.lookup l fields of
            Just t' -> unify usage l_type' t'
            Nothing -> modifyConstraints $ M.insert tn $
                       HasFields (M.insert l l_type' fields) usage
          return l_type
    Scalar (Record fields)
      | Just t' <- M.lookup l fields -> do
          unify usage l_type' (toStructural t')
          return t'
      | otherwise ->
          typeError usage $
          "Attempt to access field " ++ quote (pretty l) ++ "` of value of type " ++
          quote (pretty (toStructural t)) ++ "."
    _ -> do unify usage (toStructural t) $ Scalar $ Record $ M.singleton l l_type'
            return l_type

-- Simple MonadUnify implementation.

type UnifyMState = (Constraints, Int)

newtype UnifyM a = UnifyM (StateT UnifyMState (Except TypeError) a)
  deriving (Monad, Functor, Applicative,
            MonadState UnifyMState,
            MonadError TypeError)

instance MonadUnify UnifyM where
  getConstraints = gets fst
  putConstraints x = modify $ \s -> (x, snd s)

  newTypeVar loc desc = do
    i <- do (x, i) <- get
            put (x, i+1)
            return i
    let v = VName (mkTypeVarName desc i) 0
    modifyConstraints $ M.insert v $ NoConstraint Nothing $ Usage Nothing loc
    return $ Scalar $ TypeVar mempty Nonunique (typeName v) []

-- | Construct a the name of a new type variable given a base
-- description and a tag number (note that this is distinct from
-- actually constructing a VName; the tag here is intended for human
-- consumption but the machine does not care).
mkTypeVarName :: String -> Int -> Name
mkTypeVarName desc i =
  nameFromString $ desc ++ mapMaybe subscript (show i)
  where subscript = flip lookup $ zip "0123456789" "₀₁₂₃₄₅₆₇₈₉"

instance MonadBreadCrumbs UnifyM where

-- | Perform a unification of two types outside a monadic context.
-- The type parameters are allowed to be instantiated (with
-- 'TypeParamDim ignored); all other types are considered rigid.
doUnification :: SrcLoc -> [TypeParam]
              -> TypeBase () () -> TypeBase () ()
              -> Either TypeError (TypeBase () ())
doUnification loc tparams t1 t2 = runUnifyM tparams $ do
  unify (Usage Nothing loc) t1 t2
  normaliseType t2

runUnifyM :: [TypeParam] -> UnifyM a -> Either TypeError a
runUnifyM tparams (UnifyM m) = runExcept $ evalStateT m (constraints, 0)
  where constraints = M.fromList $ mapMaybe f tparams
        f TypeParamDim{} = Nothing
        f (TypeParamType l p loc) = Just (p, NoConstraint (Just l) $ Usage Nothing loc)
