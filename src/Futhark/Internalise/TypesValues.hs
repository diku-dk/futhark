module Futhark.Internalise.TypesValues
  (
   -- * Internalising types
    internaliseType
  , internaliseUniqueness

  -- * Internalising values
  , internaliseValue
  )
  where

import Control.Applicative
import Control.Monad.State
import qualified Data.Array as A
import Data.List

import Futhark.Representation.External as E
import Futhark.Representation.Basic as I

import Prelude hiding (mapM)

internaliseUniqueness :: E.Uniqueness -> I.Uniqueness
internaliseUniqueness E.Nonunique = I.Nonunique
internaliseUniqueness E.Unique = I.Unique

internaliseType :: E.GenType als -> [I.TypeBase ExtShape]
internaliseType = flip evalState 0 . internaliseType'
  where internaliseType' :: E.GenType als -> State Int [I.TypeBase ExtShape]
        internaliseType' (E.Basic bt) =
          return [I.Basic bt]
        internaliseType' (E.Tuple ets) =
          concat <$> mapM internaliseType' ets
        internaliseType' (E.Array at) =
          internaliseArrayType at

        internaliseArrayType (E.BasicArray bt size u _) = do
          dims <- map Ext <$> replicateM (length size) newId
          return [I.arrayOf (I.Basic bt) (ExtShape dims) $
                  internaliseUniqueness u]

        internaliseArrayType (E.TupleArray elemts size u) = do
          outerdim <- Ext <$> newId
          innerdims <- map Ext <$> replicateM (length size - 1) newId
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
  let ts          = internaliseType $ E.addNames rt
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
