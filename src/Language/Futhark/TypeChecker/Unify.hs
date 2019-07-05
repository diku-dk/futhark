{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Futhark.TypeChecker.Unify
  ( Constraint(..)
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

data Constraint = NoConstraint (Maybe Liftedness) SrcLoc
                | ParamType Liftedness SrcLoc
                | Constraint (TypeBase () ()) SrcLoc
                | Overloaded [PrimType] SrcLoc
                | HasFields (M.Map Name (TypeBase () ())) SrcLoc
                | Equality SrcLoc
                | HasConstrs (M.Map Name [TypeBase () ()]) SrcLoc
                deriving Show

instance Located Constraint where
  locOf (NoConstraint _ loc) = locOf loc
  locOf (ParamType _ loc) = locOf loc
  locOf (Constraint _ loc) = locOf loc
  locOf (Overloaded _ loc) = locOf loc
  locOf (HasFields _ loc) = locOf loc
  locOf (Equality loc) = locOf loc
  locOf (HasConstrs _ loc) = locOf loc

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

-- | Unifies two types.
unify :: MonadUnify m => SrcLoc -> TypeBase () () -> TypeBase () () -> m ()
unify loc orig_t1 orig_t2 = do
  orig_t1' <- normaliseType orig_t1
  orig_t2' <- normaliseType orig_t2
  breadCrumb (MatchingTypes orig_t1' orig_t2') $ subunify orig_t1 orig_t2
  where
    subunify t1 t2 = do
      constraints <- getConstraints

      let isRigid' v = isRigid v constraints
          t1' = applySubst (`lookupSubst` constraints) t1
          t2' = applySubst (`lookupSubst` constraints) t2

          failure =
            typeError loc $ "Couldn't match expected type `" ++
            pretty t1' ++ "' with actual type `" ++ pretty t2' ++ "'."

      traceM' $ unlines ["t1':" ++ show t1', "t2':" ++ show t2', "constraints:" ++ show constraints]
      case (t1', t2') of
        _ | t1' == t2' -> return ()

        (Record fs,
         Record arg_fs)
          | M.keys fs == M.keys arg_fs ->
              forM_ (M.toList $ M.intersectionWith (,) fs arg_fs) $ \(k, (k_t1, k_t2)) ->
              breadCrumb (MatchingFields k) $ subunify k_t1 k_t2

        (TypeVar _ _ (TypeName _ tn) targs,
         TypeVar _ _ (TypeName _ arg_tn) arg_targs)
          | tn == arg_tn, length targs == length arg_targs ->
              zipWithM_ unifyTypeArg targs arg_targs

        (TypeVar _ _ (TypeName [] v1) [],
         TypeVar _ _ (TypeName [] v2) []) ->
          case (isRigid' v1, isRigid' v2) of
            (True, True) -> failure
            (True, False) -> linkVarToType loc v2 t1'
            (False, True) -> linkVarToType loc v1 t2'
            (False, False) -> linkVarToType loc v1 t2'

        (TypeVar _ _ (TypeName [] v1) [], _)
          | not $ isRigid' v1 ->
              linkVarToType loc v1 t2'
        (_, TypeVar _ _ (TypeName [] v2) [])
          | not $ isRigid' v2 ->
              linkVarToType loc v2 t1'

        (Arrow _ _ a1 b1,
         Arrow _ _ a2 b2) -> do
          subunify a1 a2
          subunify b1 b2

        (Array{}, Array{})
          | Just t1'' <- peelArray 1 t1',
            Just t2'' <- peelArray 1 t2' -> do
              subunify t1'' t2''

        (SumT cs,
         SumT arg_cs)
          | M.keys cs == M.keys arg_cs ->
              forM_ (M.toList $ M.intersectionWith (,) cs arg_cs) $ \(_, (f1, f2)) ->
              if (length f1 == length f2)
              then
                zipWithM_ subunify f1 f2 -- TODO: improve
              else
                failure
        (_, _) -> failure

      where unifyTypeArg TypeArgDim{} TypeArgDim{} = return ()
            unifyTypeArg (TypeArgType t _) (TypeArgType arg_t _) =
              subunify t arg_t
            unifyTypeArg _ _ = typeError loc
              "Cannot unify a type argument with a dimension argument (or vice versa)."

applySubstInConstraint :: VName -> Subst (TypeBase () ()) -> Constraint -> Constraint
applySubstInConstraint vn subst (Constraint t loc) =
  Constraint (applySubst (flip M.lookup $ M.singleton vn subst) t) loc
applySubstInConstraint vn subst (HasFields fs loc) =
  HasFields (M.map (applySubst (flip M.lookup $ M.singleton vn subst)) fs) loc
applySubstInConstraint _ _ (NoConstraint l loc) = NoConstraint l loc
applySubstInConstraint _ _ (Overloaded ts loc) = Overloaded ts loc
applySubstInConstraint _ _ (Equality loc) = Equality loc
applySubstInConstraint _ _ (ParamType l loc) = ParamType l loc
applySubstInConstraint vn subst (HasConstrs cs loc) =
  HasConstrs (M.map (map (applySubst (flip M.lookup $ M.singleton vn subst))) cs) loc

linkVarToType :: MonadUnify m => SrcLoc -> VName -> TypeBase () () -> m ()
linkVarToType loc vn tp = do
  constraints <- getConstraints
  if vn `S.member` typeVars tp
    then typeError loc $ "Occurs check: cannot instantiate " ++
         prettyName vn ++ " with " ++ pretty tp'
    else do modifyConstraints $ M.insert vn $ Constraint tp' loc
            modifyConstraints $ M.map $ applySubstInConstraint vn $ Subst tp'
            case M.lookup vn constraints of
              Just (NoConstraint (Just Unlifted) unlift_loc) ->
                zeroOrderType loc ("used at " ++ locStr unlift_loc) tp'
              Just (Equality _) ->
                equalityType loc tp'
              Just (Overloaded ts old_loc)
                | tp `notElem` map Prim ts ->
                    case tp' of
                      TypeVar _ _ (TypeName [] v) []
                        | not $ isRigid v constraints -> linkVarToTypes loc v ts
                      _ -> do
                        typeError loc $ "Cannot unify `" ++ prettyName vn ++ "' with type `" ++
                          pretty tp ++ "' (`" ++ prettyName vn ++
                          "` must be one of " ++ intercalate ", " (map pretty ts) ++
                          " due to use at " ++ locStr old_loc ++ ")."
              Just (HasFields required_fields old_loc) ->
                case tp of
                  Record tp_fields
                    | all (`M.member` tp_fields) $ M.keys required_fields ->
                        mapM_ (uncurry $ unify loc) $ M.elems $
                        M.intersectionWith (,) required_fields tp_fields
                  TypeVar _ _ (TypeName [] v) []
                    | not $ isRigid v constraints ->
                        modifyConstraints $ M.insert v $
                        HasFields required_fields old_loc
                  _ ->
                    let required_fields' =
                          intercalate ", " $ map field $ M.toList required_fields
                        field (l, t) = pretty l ++ ": " ++ pretty t
                    in typeError loc $
                       "Cannot unify `" ++ prettyName vn ++ "' with type `" ++
                       pretty tp ++ "' (must be a record with fields {" ++
                       required_fields' ++
                       "} due to use at " ++ locStr old_loc ++ ")."
              Just (HasConstrs required_cs old_loc) ->
                case tp of
                  SumT ts
                    | all (`M.member` ts) $ M.keys required_cs ->
                        mapM_ (uncurry (zipWithM_ (unify loc))) $ M.elems $
                          M.intersectionWith (,) required_cs ts
                  TypeVar _ _ (TypeName [] v) []
                    | not $ isRigid v constraints -> do
                        modifyConstraints $ M.insertWith combineConstrs v $
                                   HasConstrs required_cs old_loc
                        where combineConstrs (HasConstrs cs1 loc1) (HasConstrs cs2 _) =
                                HasConstrs (M.union cs1 cs2) loc1
                              combineConstrs hasCs _ = hasCs
                  _ -> typeError loc "Can't unify." -- TODO : Improve
              _ -> return ()
  where tp' = removeUniqueness tp

removeUniqueness :: TypeBase dim as -> TypeBase dim as
removeUniqueness (Record ets) =
  Record $ fmap removeUniqueness ets
removeUniqueness (Arrow als p t1 t2) =
  Arrow als p (removeUniqueness t1) (removeUniqueness t2)
removeUniqueness (SumT cs) =
  SumT $ (fmap . fmap) removeUniqueness cs
removeUniqueness t = t `setUniqueness` Nonunique

mustBeOneOf :: MonadUnify m => [PrimType] -> SrcLoc -> TypeBase () () -> m ()
mustBeOneOf [req_t] loc t = unify loc (Prim req_t) t
mustBeOneOf ts loc t = do
  constraints <- getConstraints
  let t' = applySubst (`lookupSubst` constraints) t
      isRigid' v = isRigid v constraints

  case t' of
    TypeVar _ _ (TypeName [] v) []
      | not $ isRigid' v -> linkVarToTypes loc v ts

    Prim pt | pt `elem` ts -> return ()

    _ -> failure

  where failure = typeError loc $ "Cannot unify type \"" ++ pretty t ++
                  "\" with any of " ++ intercalate "," (map pretty ts) ++ "."

linkVarToTypes :: MonadUnify m => SrcLoc -> VName -> [PrimType] -> m ()
linkVarToTypes loc vn ts = do
  vn_constraint <- M.lookup vn <$> getConstraints
  case vn_constraint of
    Just (Overloaded vn_ts vn_loc) ->
      case ts `intersect` vn_ts of
        [] -> typeError loc $ "Type constrained to one of " ++
              intercalate "," (map pretty ts) ++ " but also one of " ++
              intercalate "," (map pretty vn_ts) ++ " at " ++ locStr vn_loc ++ "."
        ts' -> modifyConstraints $ M.insert vn $ Overloaded ts' loc

    _ -> modifyConstraints $ M.insert vn $ Overloaded ts loc

equalityType :: (MonadUnify m, Pretty (ShapeDecl dim), Monoid as) =>
                SrcLoc -> TypeBase dim as -> m ()
equalityType loc t = do
  unless (orderZero t) $
    typeError loc $
    "Type \"" ++ pretty t ++ "\" does not support equality."
  mapM_ mustBeEquality $ typeVars t
  where mustBeEquality vn = do
          constraints <- getConstraints
          case M.lookup vn constraints of
            Just (Constraint (TypeVar _ _ (TypeName [] vn') []) _) ->
              mustBeEquality vn'
            Just (Constraint vn_t _)
              | not $ orderZero vn_t ->
                  typeError loc $ "Type \"" ++ pretty t ++
                  "\" does not support equality."
              | otherwise -> return ()
            Just (NoConstraint _ _) ->
              modifyConstraints $ M.insert vn (Equality loc)
            Just (Overloaded _ _) ->
              return () -- All primtypes support equality.
            _ ->
              typeError loc $ "Type " ++ pretty (prettyName vn) ++
              " does not support equality."

zeroOrderType :: (MonadUnify m, Pretty (ShapeDecl dim), Monoid as) =>
                 SrcLoc -> String -> TypeBase dim as -> m ()
zeroOrderType loc desc t = do
  unless (orderZero t) $
    typeError loc $ "Type " ++ desc ++
    " must not be functional, but is " ++ pretty t ++ "."
  mapM_ mustBeZeroOrder . S.toList . typeVars $ t
  where mustBeZeroOrder vn = do
          constraints <- getConstraints
          case M.lookup vn constraints of
            Just (Constraint vn_t old_loc)
              | not $ orderZero t ->
                typeError loc $ "Type " ++ desc ++
                " must be non-function, but inferred to be " ++
                pretty vn_t ++ " at " ++ locStr old_loc ++ "."
            Just (NoConstraint _ _) ->
              modifyConstraints $ M.insert vn (NoConstraint (Just Unlifted) loc)
            Just (ParamType Lifted ploc) ->
              typeError loc $ "Type " ++ desc ++
              " must be non-function, but type parameter " ++ prettyName vn ++ " at " ++
              locStr ploc ++ " may be a function."
            _ -> return ()

-- In @mustHaveConstr loc c t fs@, the type @t@ must have a
-- constructor named @c@ that takes arguments of types @ts@.
mustHaveConstr :: MonadUnify m =>
                  SrcLoc -> Name -> TypeBase dim as -> [TypeBase () ()] -> m ()
mustHaveConstr loc c t fs = do
  let struct_f = toStructural <$> fs
  constraints <- getConstraints
  case t of
    TypeVar _ _ (TypeName _ tn) []
      | Just NoConstraint{} <- M.lookup tn constraints ->
          modifyConstraints $ M.insert tn $ HasConstrs (M.singleton c struct_f) loc
      | Just (HasConstrs cs _) <- M.lookup tn constraints ->
        case M.lookup c cs of
          Nothing  -> modifyConstraints $ M.insert tn $ HasConstrs (M.insert c fs cs) loc
          Just fs'
            | length fs == length fs' -> zipWithM_ (unify loc) fs fs'
            | otherwise -> throwError $ TypeError loc "Differing constructor arity" -- TODO: Improve

    SumT cs ->
      case M.lookup c cs of
        Nothing -> throwError $ TypeError loc "Constuctor not present in type." -- TODO: Improve
        Just fs'
            | length fs == length fs' -> zipWithM_ (unify loc) fs (toStructural <$> fs')
            | otherwise -> throwError $ TypeError loc "Differing constructor arity" -- TODO: Improve

    _ -> do unify loc (toStructural t) $ SumT $ M.singleton c fs
            return ()

mustHaveField :: (MonadUnify m, Monoid as) =>
                 SrcLoc -> Name -> TypeBase dim as -> m (TypeBase dim as)
mustHaveField loc l t = do
  constraints <- getConstraints
  l_type <- newTypeVar loc "t"
  let l_type' = toStructural l_type
  case t of
    TypeVar _ _ (TypeName _ tn) []
      | Just NoConstraint{} <- M.lookup tn constraints -> do
          modifyConstraints $ M.insert tn $ HasFields (M.singleton l l_type') loc
          return l_type
      | Just (HasFields fields _) <- M.lookup tn constraints -> do
          case M.lookup l fields of
            Just t' -> unify loc l_type' t'
            Nothing -> modifyConstraints $ M.insert tn $
                       HasFields (M.insert l l_type' fields) loc
          return l_type
    Record fields
      | Just t' <- M.lookup l fields -> do
          unify loc l_type' (toStructural t')
          return t'
      | otherwise ->
          throwError $ TypeError loc $
          "Attempt to access field '" ++ pretty l ++ "' of value of type " ++
          pretty (toStructural t) ++ "."
    _ -> do unify loc (toStructural t) $ Record $ M.singleton l l_type'
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
    modifyConstraints $ M.insert v $ NoConstraint Nothing loc
    return $ TypeVar mempty Nonunique (typeName v) []

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
  unify loc t1 t2
  normaliseType t2

runUnifyM :: [TypeParam] -> UnifyM a -> Either TypeError a
runUnifyM tparams (UnifyM m) = runExcept $ evalStateT m (constraints, 0)
  where constraints = M.fromList $ mapMaybe f tparams
        f TypeParamDim{} = Nothing
        f (TypeParamType l p loc) = Just (p, NoConstraint (Just l) loc)
