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

  , mapTypeVariables
  , fullyApplyType

  -- * Internalising values
  , internalisePrimValue
  , internaliseValue
  )
  where

import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Array as A
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe

import qualified Language.Futhark as E
import Futhark.Representation.SOACS as I
import Futhark.Internalise.Monad
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
        isTypeParam _ = Nothing

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
  where namedDim (E.QualName _ name) = do
          subst <- liftInternaliseM $ asks $ M.lookup name . envSubsts
          is_dim <- lookupDim name
          case (is_dim, subst) of
            (Just dim, _) -> return dim
            (Nothing, Just [v]) -> return $ I.Free v
            _ -> do -- Then it must be a constant.
              let fname = nameFromString $ pretty name ++ "f"
              (i,m,cm) <- get
              case find ((==fname) . fst) cm of
                Just (_, known) -> return $ I.Free $ I.Var known
                Nothing -> do new <- liftInternaliseM $ newVName $ baseString name
                              put (i, m, (fname,new):cm)
                              return $ I.Free $ I.Var new

internaliseTypeM :: E.StructType
                 -> InternaliseTypeM [I.TypeBase ExtShape Uniqueness]
internaliseTypeM orig_t =
  case orig_t of
    E.Prim bt -> return [I.Prim $ internalisePrimType bt]
    E.TypeVar v _ ->
      map (`I.toDecl` Nonunique) <$> applyType v
    E.Record ets ->
      concat <$> mapM (internaliseTypeM . snd) (E.sortFields ets)
    E.Array et shape u -> do
      dims <- internaliseShape shape
      ets <- internaliseElemType et
      return [I.arrayOf et' (Shape dims) $ internaliseUniqueness u | et' <- ets ]
    E.Arrow{} -> fail "internaliseTypeM: cannot handle function type."

  where internaliseElemType (E.ArrayPolyElem v _ _) =
          map (`toDecl` Nonunique) <$> applyType v
        internaliseElemType (E.ArrayPrimElem bt _) =
          return [I.Prim $ internalisePrimType bt]
        internaliseElemType (E.ArrayRecordElem elemts) =
          concat <$> mapM (internaliseRecordElem . snd) (E.sortFields elemts)

        internaliseRecordElem (E.RecordArrayElem et) =
          internaliseElemType et
        internaliseRecordElem (E.RecordArrayArrayElem et shape u) =
          internaliseTypeM $ E.Array et shape u

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
        internaliseType' (E.Array et shape u) = do
          dims <- map Ext <$> replicateM (E.shapeRank shape) newId'
          ets <- internaliseElemType et
          return [I.arrayOf et' (Shape dims) $ internaliseUniqueness u | et' <- ets ]
        internaliseType' E.Arrow{} =
          fail "internaliseTypeWithUniqueness: cannot handle function type."

        internaliseElemType E.ArrayPolyElem{} =
          lift Nothing
        internaliseElemType (E.ArrayPrimElem bt _) =
          return [I.Prim $ internalisePrimType bt]
        internaliseElemType (E.ArrayRecordElem elemts) =
          concat <$> mapM (internaliseRecordElem . snd) (E.sortFields elemts)

        internaliseRecordElem (E.RecordArrayElem et) =
          internaliseElemType et
        internaliseRecordElem (E.RecordArrayArrayElem et shape u) =
          internaliseType' $ E.Array et shape u

        newId' = do i <- get
                    put $ i + 1
                    return i

applyType :: E.TypeName -> InternaliseTypeM [I.ExtType]
applyType (E.TypeName _ tname) = do
  entry <- lookupTypeVar tname
  mapM (reExt . I.fromDecl) =<< internaliseTypeM entry

-- | Map type variables from the first argument to corresponding types
-- in the second argument.
mapTypeVariables :: E.TypeBase () () -> E.TypeBase () ()
                 -> InternaliseM (M.Map VName (E.TypeBase () ()))
mapTypeVariables _ y_t@E.TypeVar{} =
  fail $ "mapTypeVariables: Type variable \"" ++ pretty y_t ++ "\" in second argument."
mapTypeVariables (E.TypeVar (E.TypeName _ tn) _) t =
  return $ M.singleton tn t
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
fullyApplyTypeM (E.TypeVar tn _) = lookupTypeVar $ E.typeLeaf tn
fullyApplyTypeM (E.Prim t) = return $ E.Prim t
fullyApplyTypeM (E.Record fs) = E.Record <$> traverse fullyApplyTypeM fs
fullyApplyTypeM (E.Array at shape u) = inArray at
  where inArray (E.ArrayPrimElem t ()) =
          return $ E.Array (E.ArrayPrimElem t ()) shape u
        inArray (E.ArrayPolyElem tn targs ()) = do
          t <- fullyApplyTypeM (E.TypeVar tn targs)
          maybe nope return $ E.arrayOf t shape u
        inArray (E.ArrayRecordElem fs) = do
          fs' <- traverse (fullyApplyTypeM . fst . E.recordArrayElemToType) fs
          maybe nope return $ E.arrayOf (E.Record fs') shape u
        nope = fail "fullyApplyTypeM: cannot construct array."
fullyApplyTypeM E.Arrow{} =
  fail "fullyApplyTypeM: cannot handle function type."

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
