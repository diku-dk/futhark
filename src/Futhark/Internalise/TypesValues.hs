{-# LANGUAGE FlexibleContexts #-}
module Futhark.Internalise.TypesValues
  (
   -- * Internalising types
    internaliseReturnType
  , internaliseEntryReturnType
  , internaliseParamTypes
  , internaliseType
  , internaliseUniqueness
  , internalisePrimType
  , internalisedTypeSize
  , internaliseTypeM

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
import Data.Maybe
import Data.Monoid

import Prelude hiding (mapM)

import qualified Language.Futhark as E
import Futhark.Representation.SOACS as I
import Futhark.Internalise.Monad
import Futhark.MonadFreshNames
import Futhark.Util

internaliseUniqueness :: E.Uniqueness -> I.Uniqueness
internaliseUniqueness E.Nonunique = I.Nonunique
internaliseUniqueness E.Unique = I.Unique

internaliseParamTypes :: [E.TypeBase (E.ShapeDecl VName) als]
                      -> InternaliseM ([[I.TypeBase ExtShape Uniqueness]],
                                       M.Map VName Int,
                                       ConstParams)
internaliseParamTypes ts = do
  (ts', subst, cm) <- runInternaliseTypeM $ mapM internaliseTypeM ts
  return (ts', subst, cm)

internaliseReturnType :: E.TypeBase (E.ShapeDecl VName) als
                      -> InternaliseM ([I.TypeBase ExtShape Uniqueness],
                                       M.Map VName Int,
                                       ConstParams)
internaliseReturnType t = do
  (ts', subst', cm') <- internaliseEntryReturnType t
  return (concat ts', subst', cm')

-- | As 'internaliseReturnType', but returns components of a top-level
-- tuple type piecemeal.
internaliseEntryReturnType :: E.TypeBase (E.ShapeDecl VName) als
                           -> InternaliseM ([[I.TypeBase ExtShape Uniqueness]],
                                            M.Map VName Int,
                                            ConstParams)
internaliseEntryReturnType t = do
  let ts = case E.isTupleRecord t of Just tts -> tts
                                     _        -> [t]
  (ts', subst', cm') <-
    runInternaliseTypeM $ mapM internaliseTypeM ts
  return (ts', subst', cm')

internaliseType :: E.ArrayShape shape =>
                   E.TypeBase shape als
                -> InternaliseM [I.TypeBase I.Rank Uniqueness]
internaliseType t = do
  (t', _, _) <-
    runInternaliseTypeM $ internaliseTypeM $ E.vacuousShapeAnnotations t
  return $ map I.rankShaped t'

reExt :: TypeBase ExtShape u -> InternaliseTypeM (TypeBase ExtShape u)
reExt (Array t (ExtShape ds) u) = do
  (_, ds') <- mapAccumLM update mempty ds
  return $ Array t (ExtShape ds') u
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
               -> InternaliseTypeM ExtDimSize
internaliseDim d =
  case d of
    E.AnyDim -> Ext <$> newId
    E.ConstDim n -> return $ Free $ intConst I.Int32 $ toInteger n
    E.BoundDim name -> Ext <$> knownOrNewId name
    E.NamedDim name -> namedDim name
  where knownOrNewId name = do
          (i,m,cm) <- get
          case M.lookup name m of
            Nothing -> do put (i + 1, M.insert name i m, cm)
                          return i
            Just j  -> return j

        namedDim name = do
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

internaliseTypeM :: E.TypeBase (E.ShapeDecl VName) als
                 -> InternaliseTypeM [I.TypeBase ExtShape Uniqueness]
internaliseTypeM orig_t =
  case orig_t of
    E.Prim bt -> return [I.Prim $ internalisePrimType bt]
    E.TypeVar v targs ->
      map (`toDecl` Nonunique) <$> applyType v targs
    E.Record ets ->
      concat <$> mapM (internaliseTypeM . snd) (E.sortFields ets)
    E.Array at ->
      internaliseArrayType at
  where internaliseArrayType (E.PrimArray bt shape u _) = do
          dims <- internaliseShape shape
          return [I.arrayOf (I.Prim $ internalisePrimType bt) (ExtShape dims) $
                  internaliseUniqueness u]

        internaliseArrayType (E.PolyArray v targs shape u _) = do
          ts <- applyType v targs
          dims <- internaliseShape shape
          forM ts $ \t ->
            return $ I.arrayOf t (ExtShape dims) $ internaliseUniqueness u

        internaliseArrayType (E.RecordArray elemts shape u) = do
          innerdims <- ExtShape <$> internaliseShape shape
          ts <- concat <$> mapM (internaliseRecordArrayElem . snd) (E.sortFields elemts)
          return [ I.arrayOf ct innerdims $
                   if I.unique ct then Unique
                   else if I.primType ct then u
                        else I.uniqueness ct
                 | ct <- ts ]

        internaliseRecordArrayElem (E.PrimArrayElem bt _ _) =
          return [I.Prim $ internalisePrimType bt]
        internaliseRecordArrayElem (E.PolyArrayElem v targs _ _) =
          map (`toDecl` Nonunique) <$> applyType v targs
        internaliseRecordArrayElem (E.ArrayArrayElem aet) =
          internaliseArrayType aet
        internaliseRecordArrayElem (E.RecordArrayElem ts) =
          concat <$> mapM (internaliseRecordArrayElem . snd) (E.sortFields ts)

        internaliseShape = mapM internaliseDim . E.shapeDims

internaliseSimpleType :: E.TypeBase E.Rank als
                      -> Maybe [I.TypeBase ExtShape NoUniqueness]
internaliseSimpleType = fmap (map I.fromDecl) . internaliseTypeWithUniqueness

internaliseTypeWithUniqueness :: E.TypeBase E.Rank als
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
          return [I.arrayOf (I.Prim $ internalisePrimType bt) (ExtShape dims) $
                  internaliseUniqueness u]
        internaliseArrayType (E.RecordArray elemts shape u) = do
          dims <- map Ext <$> replicateM (E.shapeRank shape) newId'
          ts <- concat <$> mapM (internaliseRecordArrayElem . snd) (E.sortFields elemts)
          return [ I.arrayOf t (ExtShape dims) $
                    if I.unique t then Unique
                    else if I.primType t then u
                         else I.uniqueness t
                 | t <- ts ]

        internaliseRecordArrayElem E.PolyArrayElem{} =
          lift Nothing
        internaliseRecordArrayElem (E.PrimArrayElem bt _ _) =
          return [I.Prim $ internalisePrimType bt]
        internaliseRecordArrayElem (E.ArrayArrayElem at) =
          internaliseArrayType at
        internaliseRecordArrayElem (E.RecordArrayElem ts) =
          concat <$> mapM (internaliseRecordArrayElem . snd) (E.sortFields ts)

        newId' = do i <- get
                    put $ i + 1
                    return i

internaliseTypeArg :: E.TypeArg (E.ShapeDecl VName) als -> InternaliseTypeM TypeArg
internaliseTypeArg (E.TypeArgDim d _) =
  TypeArgDim <$> internaliseDim d
internaliseTypeArg (E.TypeArgType t _) =
  TypeArgType . map fromDecl <$> internaliseTypeM t

applyType :: E.TypeName -> [E.TypeArg (E.ShapeDecl VName) als]
          -> InternaliseTypeM [I.TypeBase ExtShape NoUniqueness]
applyType tname targs = do
  m <- lookupTypeVar =<< liftInternaliseM (lookupSubst (E.qualNameFromTypeName tname))
  targs' <- mapM internaliseTypeArg targs
  mapM reExt =<< m targs'

-- | How many core language values are needed to represent one source
-- language value of the given type?
internalisedTypeSize :: E.ArrayShape shape =>
                        E.TypeBase shape als -> InternaliseM Int
internalisedTypeSize = fmap length . internaliseType

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
