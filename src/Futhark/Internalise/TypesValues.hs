module Futhark.Internalise.TypesValues
  (
   -- * Internalising types
    internaliseDeclType
  , internaliseDeclTypes
  , internaliseType
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
import Futhark.Representation.Basic as I
import Futhark.Internalise.Monad

import Prelude hiding (mapM)

internaliseUniqueness :: E.Uniqueness -> I.Uniqueness
internaliseUniqueness E.Nonunique = I.Nonunique
internaliseUniqueness E.Unique = I.Unique

internaliseDeclTypes :: [E.TypeBase E.ShapeDecl als VName]
                     -> InternaliseM ([[I.TypeBase ExtShape]],
                                      HM.HashMap VName Int)
internaliseDeclTypes ts = do
  (ts', (_, subst)) <- runStateT (mapM internaliseDeclType' ts) (0, HM.empty)
  return (ts', subst)

internaliseDeclType :: E.TypeBase E.ShapeDecl als VName
                    -> InternaliseM ([I.TypeBase ExtShape],
                                     HM.HashMap VName Int)
internaliseDeclType t = do
  (t', (_, subst)) <- runStateT (internaliseDeclType' t) (0, HM.empty)
  return (t', subst)

internaliseDeclType' :: E.TypeBase E.ShapeDecl als VName
                     -> StateT (Int, HM.HashMap VName Int)
                        InternaliseM [I.TypeBase ExtShape]
internaliseDeclType' (E.Basic bt) =
  return [I.Basic bt]
internaliseDeclType' (E.Tuple ets) =
  concat <$> mapM internaliseDeclType' ets
internaliseDeclType' (E.Array at) =
  internaliseArrayType at
  where internaliseArrayType (E.BasicArray bt shape u _) = do
          dims <- internaliseShape shape
          return [I.arrayOf (I.Basic bt) (ExtShape dims) $
                  internaliseUniqueness u]

        internaliseArrayType (E.TupleArray elemts shape u) = do
          outerdim <- Ext <$> newId
          innerdims <- map Ext <$> replicateM (E.shapeRank shape - 1) newId
          ts <- concat <$> mapM internaliseTupleArrayElem elemts
          return [ I.arrayOf ct (ExtShape $ outerdim : innerdims) $
                   if I.unique ct then Unique
                   else if I.basicType ct then u
                        else I.uniqueness ct
                 | ct <- ts ]

        internaliseTupleArrayElem (BasicArrayElem bt _) =
          return [I.Basic bt]
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

        internaliseShape = mapM internaliseDim . E.shapeDims

        internaliseDim AnyDim =
          Ext <$> newId
        internaliseDim (ConstDim n) =
          return $ Free $ Constant $ IntVal n
        internaliseDim (VarDim name) =
          Ext <$> knownOrNewId name
        internaliseDim (KnownDim name) = do
          subst <- asks $ HM.lookup name . envSubsts
          return $ I.Free $ I.Var $ case subst of
            Just [v] -> v
            _        -> I.Ident name $ I.Basic Int

internaliseType :: Ord vn =>
                   E.TypeBase E.Rank als vn -> [I.TypeBase ExtShape]
internaliseType = flip evalState 0 . internaliseType'
  where internaliseType' (E.Basic bt) =
          return [I.Basic bt]
        internaliseType' (E.Tuple ets) =
          concat <$> mapM internaliseType' ets
        internaliseType' (E.Array at) =
          internaliseArrayType at

        internaliseArrayType (E.BasicArray bt shape u _) = do
          dims <- map Ext <$> replicateM (E.shapeRank shape) newId
          return [I.arrayOf (I.Basic bt) (ExtShape dims) $
                  internaliseUniqueness u]

        internaliseArrayType (E.TupleArray elemts shape u) = do
          outerdim <- Ext <$> newId
          innerdims <- map Ext <$> replicateM (E.shapeRank shape - 1) newId
          ts <- concat <$> mapM internaliseTupleArrayElem elemts
          return [ I.arrayOf t (ExtShape $ outerdim : innerdims) $
                   if I.unique t then Unique
                   else if I.basicType t then u
                        else I.uniqueness t
                 | t <- ts ]

        internaliseTupleArrayElem (BasicArrayElem bt _) =
          return [I.Basic bt]
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
internaliseValue :: E.Value -> [I.Value]
internaliseValue (E.ArrayVal arr rt) =
  let ts          = internaliseType rt
      arrayvalues = map internaliseValue $ A.elems arr
      arrayvalues' =
        case arrayvalues of
          [] -> replicate (length ts) []
          _  -> transpose arrayvalues
  in zipWith asarray ts arrayvalues'
  where asarray rt' values =
          let shape = determineShape (I.arrayRank rt') values
          in I.ArrayVal (A.listArray (0,product shape-1) $
                         concatMap flatten values)
             (I.elemType rt') shape
        flatten (I.BasicVal bv)      = [bv]
        flatten (I.ArrayVal bvs _ _) = A.elems bvs

internaliseValue (E.TupVal vs) = concatMap internaliseValue vs
internaliseValue (E.BasicVal bv) = [I.BasicVal bv]

determineShape :: Int -> [I.Value] -> [Int]
determineShape _ vs@(I.ArrayVal _ _ shape : _) =
  length vs : shape
determineShape r vs =
  length vs : replicate r 0
