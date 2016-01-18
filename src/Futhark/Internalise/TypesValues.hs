{-# LANGUAGE FlexibleContexts #-}
module Futhark.Internalise.TypesValues
  (
   -- * Internalising types
    internaliseReturnType
  , internaliseParamTypes
  , internaliseType
  , internaliseTypeWithUniqueness
  , internaliseUniqueness

  -- * Internalising values
  , internaliseValue
  )
  where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Array as A
import Data.List
import qualified Data.HashMap.Lazy as HM

import Futhark.Representation.External as E
import Futhark.Representation.SOACS as I
import Futhark.Internalise.Monad

import Prelude hiding (mapM)

internaliseUniqueness :: E.Uniqueness -> I.Uniqueness
internaliseUniqueness E.Nonunique = I.Nonunique
internaliseUniqueness E.Unique = I.Unique

internaliseParamTypes :: [E.TypeBase E.ShapeDecl als VName]
                      -> InternaliseM ([[I.TypeBase ExtShape Uniqueness]],
                                       HM.HashMap VName Int)
internaliseParamTypes ts = do
  (ts', (_, subst)) <- runStateT (mapM (internaliseDeclType' BindDims) ts) (0, HM.empty)
  return (ts', subst)

internaliseReturnType :: E.TypeBase E.ShapeDecl als VName
                      -> InternaliseM ([I.TypeBase ExtShape Uniqueness],
                                       HM.HashMap VName Int)
internaliseReturnType t = do
  (t', (_, subst)) <- runStateT (internaliseDeclType' AssertDims t) (0, HM.empty)
  return (t', subst)

data DimDeclInterpretation = AssertDims
                           | BindDims

internaliseDeclType' :: DimDeclInterpretation
                     -> E.TypeBase E.ShapeDecl als VName
                     -> StateT (Int, HM.HashMap VName Int)
                        InternaliseM [I.TypeBase ExtShape Uniqueness]
internaliseDeclType' _ (E.Prim bt) =
  return [I.Prim bt]
internaliseDeclType' ddi (E.Tuple ets) =
  concat <$> mapM (internaliseDeclType' ddi) ets
internaliseDeclType' ddi (E.Array at) =
  internaliseArrayType at
  where internaliseArrayType (E.PrimArray bt shape u _) = do
          dims <- internaliseShape shape
          return [I.arrayOf (I.Prim bt) (ExtShape dims) $
                  internaliseUniqueness u]

        internaliseArrayType (E.TupleArray elemts shape u) = do
          innerdims <- ExtShape <$> internaliseShape shape
          ts <- concat <$> mapM internaliseTupleArrayElem elemts
          return [ I.arrayOf ct innerdims $
                   if I.unique ct then Unique
                   else if I.primType ct then u
                        else I.uniqueness ct
                 | ct <- ts ]

        internaliseTupleArrayElem (PrimArrayElem bt _) =
          return [I.Prim bt]
        internaliseTupleArrayElem (ArrayArrayElem aet) =
          internaliseArrayType aet
        internaliseTupleArrayElem (TupleArrayElem ts) =
          concat <$> mapM internaliseTupleArrayElem ts

        newId = do (i,m) <- get
                   put (i + 1, m)
                   return i

        knownOrNewId name = do
          (i,m) <- get
          case HM.lookup name m of
            Nothing -> do put (i + 1, HM.insert name i m)
                          return i
            Just j  -> return j

        internaliseShape = mapM (internaliseDim ddi) . E.shapeDims

        internaliseDim _ AnyDim =
          Ext <$> newId
        internaliseDim _ (ConstDim n) =
          return $ Free $ Constant $ intvalue Int32 $ toInteger n
        internaliseDim BindDims (NamedDim name) =
          Ext <$> knownOrNewId name
        internaliseDim AssertDims (NamedDim name) = do
          subst <- asks $ HM.lookup name . envSubsts
          I.Free <$> I.Var <$> case subst of
            Just [v] -> return v
            _        -> fail $ "Shape declaration " ++ pretty name ++ " not found"

internaliseType :: Ord vn =>
                   E.TypeBase E.Rank als vn
                -> [I.TypeBase ExtShape NoUniqueness]
internaliseType = map I.fromDecl . internaliseTypeWithUniqueness

internaliseTypeWithUniqueness :: Ord vn =>
                                 E.TypeBase E.Rank als vn
                              -> [I.TypeBase ExtShape Uniqueness]
internaliseTypeWithUniqueness = flip evalState 0 . internaliseType'
  where internaliseType' (E.Prim bt) =
          return [I.Prim bt]
        internaliseType' (E.Tuple ets) =
          concat <$> mapM internaliseType' ets
        internaliseType' (E.Array at) =
          internaliseArrayType at

        internaliseArrayType (E.PrimArray bt shape u _) = do
          dims <- map Ext <$> replicateM (E.shapeRank shape) newId
          return [I.arrayOf (I.Prim bt) (ExtShape dims) $
                  internaliseUniqueness u]

        internaliseArrayType (E.TupleArray elemts shape u) = do
          outerdim <- Ext <$> newId
          innerdims <- map Ext <$> replicateM (E.shapeRank shape - 1) newId
          ts <- concat <$> mapM internaliseTupleArrayElem elemts
          return [ I.arrayOf t (ExtShape $ outerdim : innerdims) $
                    if I.unique t then Unique
                    else if I.primType t then u
                         else I.uniqueness t
                 | t <- ts ]

        internaliseTupleArrayElem (PrimArrayElem bt _) =
          return [I.Prim bt]
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
  let ts          = internaliseType rt
      arrayvalues' =
        case arrayvalues of
          [] -> replicate (length ts) []
          _  -> transpose arrayvalues
  zipWithM asarray ts arrayvalues'
  where asarray rt' values =
          let shape = determineShape (I.arrayRank rt') values
              values' = concatMap flatten values
              size = product shape
          in if size == length values' then
               Just $ I.ArrayVal (A.listArray (0,size - 1) values')
               (I.elemType rt') shape
             else Nothing
        flatten (I.PrimVal bv)      = [bv]
        flatten (I.ArrayVal bvs _ _) = A.elems bvs

internaliseValue (E.TupValue vs) =
  concat <$> mapM internaliseValue vs
internaliseValue (E.PrimValue bv) =
  return [I.PrimVal bv]

determineShape :: Int -> [I.Value] -> [Int]
determineShape _ vs@(I.ArrayVal _ _ shape : _) =
  length vs : shape
determineShape r vs =
  length vs : replicate r 0
