{-# LANGUAGE FlexibleContexts #-}
module Futhark.Internalise.TypesValues
  (
   -- * Internalising types
    BoundInTypes
  , boundInTypes
  , internaliseReturnType
  , internaliseEntryReturnType
  , internaliseParamTypes
  , internaliseType
  , internaliseTypes
  , internaliseUniqueness
  , internalisePrimType
  , internalisedTypeSize
  , internaliseTypeM
  , internaliseTypeAbbr

  , mapTypeVariables
  , fullyApplyType

  -- * Internalising values
  , internalisePrimValue
  , internaliseValue
  )
  where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Array as A
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Data.Monoid

import Prelude hiding (mapM)

import qualified Language.Futhark as E
import qualified Language.Futhark.TypeChecker.Types as E
import qualified Language.Futhark.TypeChecker.Monad as E
import Futhark.Representation.SOACS as I
import Futhark.Internalise.Monad
import Futhark.MonadFreshNames
import Futhark.Util

internaliseUniqueness :: E.Uniqueness -> I.Uniqueness
internaliseUniqueness E.Nonunique = I.Nonunique
internaliseUniqueness E.Unique = I.Unique

-- | The names that are bound for some types, either implicitly or
-- explicitly.
newtype BoundInTypes = BoundInTypes (S.Set VName)

-- | Determine the names bound for some types.
boundInTypes :: [E.TypeParam] -> BoundInTypes
boundInTypes = BoundInTypes . S.fromList . mapMaybe isTypeParam
  where isTypeParam (E.TypeParamDim v _) = Just v
        isTypeParam E.TypeParamType{} = Nothing

internaliseParamTypes :: BoundInTypes
                      -> M.Map VName VName
                      -> [E.TypeBase (E.DimDecl VName) ()]
                      -> InternaliseM ([[I.TypeBase ExtShape Uniqueness]],
                                       M.Map VName Int,
                                       ConstParams)
internaliseParamTypes (BoundInTypes bound) pnames ts = do
  (ts', subst, cm) <- runInternaliseTypeM bound $
                      withDims (M.map (Free . Var) pnames) $
                      mapM internaliseTypeM ts
  return (ts', subst, cm)

internaliseReturnType :: E.TypeBase (E.DimDecl VName) ()
                      -> InternaliseM ([I.TypeBase ExtShape Uniqueness],
                                       M.Map VName Int,
                                       ConstParams)
internaliseReturnType t = do
  (ts', subst', cm') <- internaliseEntryReturnType t
  return (concat ts', subst', cm')

-- | As 'internaliseReturnType', but returns components of a top-level
-- tuple type piecemeal.
internaliseEntryReturnType :: E.TypeBase (E.DimDecl VName) ()
                           -> InternaliseM ([[I.TypeBase ExtShape Uniqueness]],
                                            M.Map VName Int,
                                            ConstParams)
internaliseEntryReturnType t = do
  let ts = case E.isTupleRecord t of Just tts -> tts
                                     _        -> [t]
  (ts', subst', cm') <-
    runInternaliseTypeM mempty $ mapM internaliseTypeM ts
  return (ts', subst', cm')

internaliseTypes :: E.ArrayDim dim =>
                    [E.TypeBase dim ()]
                 -> InternaliseM [[I.TypeBase I.ExtShape Uniqueness]]
internaliseTypes ts = do
  (st', _, _) <-
    runInternaliseTypeM mempty $
    mapM (internaliseTypeM . E.vacuousShapeAnnotations) ts
  return st'

internaliseType :: E.TypeBase () ()
                -> InternaliseM [I.TypeBase I.ExtShape Uniqueness]
internaliseType t = do
  (t', _, _) <-
    runInternaliseTypeM mempty $
    internaliseTypeM $ E.vacuousShapeAnnotations t
  return t'

reExt :: ExtType -> InternaliseTypeM ExtType
reExt (Array t (Shape ds) u) = do
  (_, ds') <- mapAccumLM update mempty ds
  return $ Array t (Shape ds') u
  where update seen (Ext x)
          | Just x' <- M.lookup x seen =
              return (seen, Ext x')
          | otherwise = do
              x' <- newId
              return (M.insert x x' seen, Ext x')
        update seen d =
          return (seen, d)
reExt t = return t

newId :: InternaliseTypeM Int
newId = do (i,m,cm) <- get
           put (i + 1, m, cm)
           return i

internaliseDim :: E.DimDecl VName
               -> InternaliseTypeM ExtSize
internaliseDim d =
  case d of
    E.AnyDim -> Ext <$> newId
    E.ConstDim n -> return $ Free $ intConst I.Int32 $ toInteger n
    E.NamedDim name -> namedDim name
  where namedDim name = do
          name' <- liftInternaliseM $ lookupSubst name
          subst <- liftInternaliseM $ asks $ M.lookup name' . envSubsts
          is_dim <- lookupDim name'
          case (is_dim, subst) of
            (Just dim, _) -> return dim
            (Nothing, Just [v]) -> return $ I.Free v
            _ -> do -- Then it must be a constant.
              let fname = nameFromString $ pretty name' ++ "f"
              (i,m,cm) <- get
              case find ((==fname) . fst) cm of
                Just (_, known) -> return $ I.Free $ I.Var known
                Nothing -> do new <- liftInternaliseM $ newVName $ baseString name'
                              put (i, m, (fname,new):cm)
                              return $ I.Free $ I.Var new

internaliseTypeM :: E.StructType
                 -> InternaliseTypeM [I.TypeBase ExtShape Uniqueness]
internaliseTypeM orig_t =
  case orig_t of
    E.Prim bt -> return [I.Prim $ internalisePrimType bt]
    E.TypeVar v targs ->
      map (`I.toDecl` Nonunique) <$> applyType v targs
    E.Record ets ->
      concat <$> mapM (internaliseTypeM . snd) (E.sortFields ets)
    E.Array at ->
      internaliseArrayType at
  where internaliseArrayType (E.PrimArray bt shape u _) = do
          dims <- internaliseShape shape
          return [I.arrayOf (I.Prim $ internalisePrimType bt) (Shape dims) $
                  internaliseUniqueness u]

        internaliseArrayType (E.PolyArray v targs shape u _) = do
          ts <- applyType v targs
          dims <- internaliseShape shape
          forM ts $ \t ->
            return $ I.arrayOf t (Shape dims) $ internaliseUniqueness u

        internaliseArrayType (E.RecordArray elemts shape u) = do
          innerdims <- Shape <$> internaliseShape shape
          ts <- concat <$> mapM (internaliseRecordArrayElem . snd) (E.sortFields elemts)
          return [ I.arrayOf ct innerdims $
                   if I.unique ct then Unique
                   else if I.primType ct then u
                        else I.uniqueness ct
                 | ct <- ts ]

        internaliseRecordArrayElem (E.PrimArrayElem bt _) =
          return [I.Prim $ internalisePrimType bt]
        internaliseRecordArrayElem (E.PolyArrayElem v targs _ _) =
          map (`I.toDecl` Nonunique) <$> applyType v targs
        internaliseRecordArrayElem (E.ArrayArrayElem aet) =
          internaliseArrayType aet
        internaliseRecordArrayElem (E.RecordArrayElem ts) =
          concat <$> mapM (internaliseRecordArrayElem . snd) (E.sortFields ts)

        internaliseShape = mapM internaliseDim . E.shapeDims

internaliseSimpleType :: E.TypeBase () ()
                      -> Maybe [I.TypeBase ExtShape NoUniqueness]
internaliseSimpleType = fmap (map I.fromDecl) . internaliseTypeWithUniqueness

internaliseTypeWithUniqueness :: E.TypeBase () ()
                              -> Maybe [I.TypeBase ExtShape Uniqueness]
internaliseTypeWithUniqueness = flip evalStateT 0 . internaliseType'
  where internaliseType' E.TypeVar{} =
          lift Nothing
        internaliseType' (E.Prim bt) =
          return [I.Prim $ internalisePrimType bt]
        internaliseType' (E.Record ets) =
          concat <$> mapM (internaliseType' . snd) (E.sortFields ets)
        internaliseType' (E.Array at) =
          internaliseArrayType at

        internaliseArrayType E.PolyArray{} =
          lift Nothing
        internaliseArrayType (E.PrimArray bt shape u _) = do
          dims <- map Ext <$> replicateM (E.shapeRank shape) newId'
          return [I.arrayOf (I.Prim $ internalisePrimType bt) (Shape dims) $
                  internaliseUniqueness u]
        internaliseArrayType (E.RecordArray elemts shape u) = do
          dims <- map Ext <$> replicateM (E.shapeRank shape) newId'
          ts <- concat <$> mapM (internaliseRecordArrayElem . snd) (E.sortFields elemts)
          return [ I.arrayOf t (Shape dims) $
                    if I.unique t then Unique
                    else if I.primType t then u
                         else I.uniqueness t
                 | t <- ts ]

        internaliseRecordArrayElem E.PolyArrayElem{} =
          lift Nothing
        internaliseRecordArrayElem (E.PrimArrayElem bt _) =
          return [I.Prim $ internalisePrimType bt]
        internaliseRecordArrayElem (E.ArrayArrayElem at) =
          internaliseArrayType at
        internaliseRecordArrayElem (E.RecordArrayElem ts) =
          concat <$> mapM (internaliseRecordArrayElem . snd) (E.sortFields ts)

        newId' = do i <- get
                    put $ i + 1
                    return i

data TypeArg = TypeArgDim ExtSize | TypeArgType [I.ExtType]

internaliseTypeArg :: E.StructTypeArg -> InternaliseTypeM TypeArg
internaliseTypeArg (E.TypeArgDim d _) =
  TypeArgDim <$> internaliseDim d
internaliseTypeArg (E.TypeArgType t _) =
  TypeArgType . map I.fromDecl <$> internaliseTypeM t

internaliseTypeAbbr :: TypeEntry
                    -> [E.StructTypeArg]
                    -> InternaliseTypeM [I.ExtType]
internaliseTypeAbbr (substs, ps, tb_t) targs = do
  targs' <- mapM internaliseTypeArg targs
  cur_substs <- liftInternaliseM allSubsts
  mapM (reExt . I.fromDecl) <=<
    withTypeDecSubstitutions substs $ do
    let dims = M.fromList $ mapMaybe dimSubst $ zip ps targs'
        types = M.fromList $ mapMaybe (typeSubst cur_substs) $ zip ps targs
    withDims dims $ withTypes types $ internaliseTypeM tb_t
  where dimSubst (E.TypeParamDim p _, TypeArgDim d) = Just (p, d)
        dimSubst _ = Nothing
        typeSubst cur_substs (E.TypeParamType p _, E.TypeArgType t _) =
          Just (p, (cur_substs, [], t))
        typeSubst _ _ = Nothing

applyType :: E.TypeName -> [E.StructTypeArg]
          -> InternaliseTypeM [I.ExtType]
applyType tname targs = do
  entry <-
    lookupTypeVar =<< liftInternaliseM (lookupSubst (E.qualNameFromTypeName tname))
  internaliseTypeAbbr entry targs

-- | Map type variables from the first argument to corresponding types
-- in the second argument.
mapTypeVariables :: E.TypeBase () () -> E.TypeBase () ()
                 -> InternaliseM (M.Map VName (E.TypeBase () ()))
mapTypeVariables _ y_t@E.TypeVar{} =
  fail $ "mapTypeVariables: Type variable \"" ++ pretty y_t ++ "\" in second argument."
mapTypeVariables _ y_t@(E.Array E.PolyArray{}) =
  fail $ "mapTypeVariables: Polymorphic array " ++ pretty y_t ++ " in second argument."
mapTypeVariables (E.TypeVar (E.TypeName _ tn) []) t =
  return $ M.singleton tn t
mapTypeVariables (E.TypeVar tn targs) y_t = do
  tn' <- lookupSubst $ E.qualNameFromTypeName tn
  ((_, tn_ps, tn_t), _, _) <- runInternaliseTypeM mempty $ lookupTypeVar tn'
  substs <- mapTypeVariables (E.removeShapeAnnotations tn_t) y_t
  mconcat <$> zipWithM (unify substs) tn_ps targs
  where unify substs (E.TypeParamType pv _) (E.TypeArgType at _)
          | Just t <- M.lookup pv substs =
              mapTypeVariables at t
        unify _ _ _ = return mempty
mapTypeVariables x@E.Array{} y@E.Array{}
  | Just x' <- E.peelArray (E.arrayRank x) x,
    Just y' <- E.peelArray (E.arrayRank x) y =
      mapTypeVariables x' y'
mapTypeVariables (E.Record fs_x) (E.Record fs_y) =
  mconcat <$> zipWithM mapTypeVariables
    (map snd $ M.toList fs_x) (map snd $ M.toList fs_y)
mapTypeVariables _ _ =
  return mempty

fullyApplyType :: E.TypeBase () () -> InternaliseM (E.TypeBase () ())
fullyApplyType t = do
  (t', _, _) <- runInternaliseTypeM mempty $ fullyApplyTypeM $ E.vacuousShapeAnnotations t
  return $ E.removeShapeAnnotations t'

fullyApplyTypeM :: E.StructType -> InternaliseTypeM E.StructType
fullyApplyTypeM (E.TypeVar tn targs) = do
  (tn_substs, tn_ps, tn_t) <-
    lookupTypeVar =<< liftInternaliseM (lookupSubst $ E.qualNameFromTypeName tn)
  let tsubsts = mconcat $ zipWith typeSubst tn_ps targs
  withTypeDecSubstitutions tn_substs $
    fullyApplyTypeM $ E.substituteTypes tsubsts tn_t
  where typeSubst (E.TypeParamType p _) (E.TypeArgType t _) =
          M.singleton p $ E.TypeSub $ E.TypeAbbr [] $ E.vacuousShapeAnnotations t
        typeSubst _ _ = mempty
fullyApplyTypeM (E.Prim t) = return $ E.Prim t
fullyApplyTypeM (E.Record fs) = E.Record <$> traverse fullyApplyTypeM fs
fullyApplyTypeM (E.Array at) = inArray at
  where inArray (E.PrimArray t shape u ()) =
          return $ E.Array $ E.PrimArray t shape u ()
        inArray (E.PolyArray tn targs shape u ()) = do
          tn' <- fullyApplyTypeM $ E.TypeVar tn targs
          return $ E.arrayOf tn' shape u
        inArray (E.RecordArray fs shape u) =
          E.Array <$> (E.RecordArray <$> traverse (inRecord u) fs <*>
                       pure shape <*> pure u)

        inRecord u =
          fmap (`E.typeToRecordArrayElem` u) .
          fullyApplyTypeM . E.recordArrayElemToType

-- | How many core language values are needed to represent one source
-- language value of the given type?
internalisedTypeSize :: E.ArrayDim dim =>
                        E.TypeBase dim () -> InternaliseM Int
internalisedTypeSize = fmap length . internaliseType . E.removeShapeAnnotations

-- | Transform an external value to a number of internal values.
-- Roughly:
--
-- * The resulting list is empty if the original value is an empty
--   tuple.
--
-- * It contains a single element if the original value was a
-- singleton tuple or non-tuple.
--
-- * The list contains more than one element if the original value was
-- a non-empty non-singleton tuple.
--
-- Although note that the transformation from arrays-of-tuples to
-- tuples-of-arrays may also contribute to several discrete arrays
-- being returned for a single input array.
--
-- If the input value is or contains a non-regular array, 'Nothing'
-- will be returned.
internaliseValue :: E.Value -> Maybe [I.Value]
internaliseValue (E.ArrayValue arr rt) = do
  arrayvalues <- mapM internaliseValue $ A.elems arr
  ts <- internaliseSimpleType rt
  let arrayvalues' =
        case arrayvalues of
          [] -> replicate (length ts) []
          _  -> transpose arrayvalues
  zipWithM asarray ts arrayvalues'
  where asarray rt' values =
          let shape = determineShape (I.arrayRank rt') values
              values' = concatMap flat values
              size = product shape
          in if size == length values' then
               Just $ I.ArrayVal (A.listArray (0,size - 1) values')
               (I.elemType rt') shape
             else Nothing
        flat (I.PrimVal bv)      = [bv]
        flat (I.ArrayVal bvs _ _) = A.elems bvs
internaliseValue (E.PrimValue bv) =
  return [I.PrimVal $ internalisePrimValue bv]

determineShape :: Int -> [I.Value] -> [Int]
determineShape _ vs@(I.ArrayVal _ _ shape : _) =
  length vs : shape
determineShape r vs =
  length vs : replicate r 0

-- | Convert an external primitive to an internal primitive.
internalisePrimType :: E.PrimType -> I.PrimType
internalisePrimType (E.Signed t) = I.IntType t
internalisePrimType (E.Unsigned t) = I.IntType t
internalisePrimType (E.FloatType t) = I.FloatType t
internalisePrimType E.Bool = I.Bool

-- | Convert an external primitive value to an internal primitive value.
internalisePrimValue :: E.PrimValue -> I.PrimValue
internalisePrimValue (E.SignedValue v) = I.IntValue v
internalisePrimValue (E.UnsignedValue v) = I.IntValue v
internalisePrimValue (E.FloatValue v) = I.FloatValue v
internalisePrimValue (E.BoolValue b) = I.BoolValue b
