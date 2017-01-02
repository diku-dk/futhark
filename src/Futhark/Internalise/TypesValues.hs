{-# LANGUAGE FlexibleContexts #-}
module Futhark.Internalise.TypesValues
  (
   -- * Internalising types
    internaliseReturnType
  , internaliseParamTypes
  , internaliseType
  , internaliseUniqueness
  , internalisePrimType

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
import qualified Data.HashMap.Lazy as HM
import Data.Monoid

import Prelude hiding (mapM)

import Language.Futhark as E
import Futhark.Representation.SOACS as I
import Futhark.Internalise.Monad
import Futhark.MonadFreshNames

internaliseUniqueness :: E.Uniqueness -> I.Uniqueness
internaliseUniqueness E.Nonunique = I.Nonunique
internaliseUniqueness E.Unique = I.Unique

internaliseParamTypes :: [E.TypeBase (E.ShapeDecl VName) als]
                      -> InternaliseM ([[I.TypeBase ExtShape Uniqueness]],
                                       HM.HashMap VName Int)
internaliseParamTypes ts = do
  (ts', (_, subst, _)) <- runStateT (mapM (internaliseDeclType' BindDims) ts) (0, HM.empty, mempty)
  return (ts', subst)

internaliseReturnType :: E.TypeBase (E.ShapeDecl VName) als
                      -> InternaliseM ([I.TypeBase ExtShape Uniqueness],
                                       HM.HashMap VName Int,
                                       ConstParams)
internaliseReturnType t = do
  (t', (_, subst, cm)) <- runStateT (internaliseDeclType' AssertDims t) (0, HM.empty, mempty)
  return (t', subst, cm)

internaliseType :: E.ArrayShape shape =>
                   E.TypeBase shape als
                -> InternaliseM [I.TypeBase I.Rank Uniqueness]
internaliseType t = do
  (t', _) <- runStateT
             (internaliseDeclType' BindDims $ vacuousShapeAnnotations t)
             (0, HM.empty, mempty)
  return $ map I.rankShaped t'

data DimDeclInterpretation = AssertDims
                           | BindDims

internaliseDeclType' :: DimDeclInterpretation
                     -> E.TypeBase (E.ShapeDecl VName) als
                     -> StateT (Int, HM.HashMap VName Int, ConstParams)
                        InternaliseM [I.TypeBase ExtShape Uniqueness]
internaliseDeclType' _ (E.Prim bt) =
  return [I.Prim $ internalisePrimType bt]
internaliseDeclType' _ (E.TypeVar v) =
  lift $ staticShapes . pure . (`toDecl` Nonunique) <$> lookupTypeVar v
internaliseDeclType' ddi (E.Tuple ets) =
  concat <$> mapM (internaliseDeclType' ddi) ets
internaliseDeclType' ddi (E.Array at) =
  internaliseArrayType at
  where internaliseArrayType (E.PrimArray bt shape u _) = do
          dims <- internaliseShape shape
          return [I.arrayOf (I.Prim $ internalisePrimType bt) (ExtShape dims) $
                  internaliseUniqueness u]

        internaliseArrayType (E.PolyArray v shape u _) = do
          dims <- internaliseShape shape
          t <- lift $ staticShapes1 <$> lookupTypeVar v
          return [I.arrayOf t (ExtShape dims) $ internaliseUniqueness u]

        internaliseArrayType (E.TupleArray elemts shape u) = do
          innerdims <- ExtShape <$> internaliseShape shape
          ts <- concat <$> mapM internaliseTupleArrayElem elemts
          return [ I.arrayOf ct innerdims $
                   if I.unique ct then Unique
                   else if I.primType ct then u
                        else I.uniqueness ct
                 | ct <- ts ]

        internaliseTupleArrayElem (PrimArrayElem bt _ _) =
          return [I.Prim $ internalisePrimType bt]
        internaliseTupleArrayElem (PolyArrayElem v _ _) =
          lift $ staticShapes . pure . (`toDecl` Nonunique) <$> lookupTypeVar v
        internaliseTupleArrayElem (ArrayArrayElem aet) =
          internaliseArrayType aet
        internaliseTupleArrayElem (TupleArrayElem ts) =
          concat <$> mapM internaliseTupleArrayElem ts

        newId = do (i,m,cm) <- get
                   put (i + 1, m, cm)
                   return i

        knownOrNewId name = do
          (i,m,cm) <- get
          case HM.lookup name m of
            Nothing -> do put (i + 1, HM.insert name i m, cm)
                          return i
            Just j  -> return j

        internaliseShape = mapM (internaliseDim ddi) . E.shapeDims

        internaliseDim _ AnyDim =
          Ext <$> newId
        internaliseDim _ (ConstDim n) =
          return $ Free $ intConst I.Int32 $ toInteger n
        internaliseDim BindDims (NamedDim name) =
          Ext <$> knownOrNewId name
        internaliseDim AssertDims (NamedDim name) = do
          subst <- asks $ HM.lookup name . envSubsts
          I.Free <$> case subst of
            Just [v] -> return v
            _ -> do -- Then it must be a constant.
              let fname = nameFromString $ pretty name ++ "f"
              (i,m,cm) <- get
              case find ((==fname) . fst) cm of
                Just (_, known) -> return $ I.Var known
                Nothing -> do new <- lift $ newVName $ baseString name
                              put (i, m, (fname,new):cm)
                              return $ I.Var new

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
        internaliseType' (E.Tuple ets) =
          concat <$> mapM internaliseType' ets
        internaliseType' (E.Array at) =
          internaliseArrayType at

        internaliseArrayType E.PolyArray{} =
          lift Nothing
        internaliseArrayType (E.PrimArray bt shape u _) = do
          dims <- map Ext <$> replicateM (E.shapeRank shape) newId
          return [I.arrayOf (I.Prim $ internalisePrimType bt) (ExtShape dims) $
                  internaliseUniqueness u]
        internaliseArrayType (E.TupleArray elemts shape u) = do
          dims <- map Ext <$> replicateM (E.shapeRank shape) newId
          ts <- concat <$> mapM internaliseTupleArrayElem elemts
          return [ I.arrayOf t (ExtShape dims) $
                    if I.unique t then Unique
                    else if I.primType t then u
                         else I.uniqueness t
                 | t <- ts ]

        internaliseTupleArrayElem PolyArrayElem{} =
          lift Nothing
        internaliseTupleArrayElem (PrimArrayElem bt _ _) =
          return [I.Prim $ internalisePrimType bt]
        internaliseTupleArrayElem (ArrayArrayElem at) =
          internaliseArrayType at
        internaliseTupleArrayElem (TupleArrayElem ts) =
          concat <$> mapM internaliseTupleArrayElem ts

        newId = do i <- get
                   put $ i + 1
                   return i

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

internaliseValue (E.TupValue vs) =
  concat <$> mapM internaliseValue vs
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
