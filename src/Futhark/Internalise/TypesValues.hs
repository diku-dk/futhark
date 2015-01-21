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
        internaliseType' (E.Elem (E.Basic bt)) =
          return [I.Basic bt]
        internaliseType' (E.Elem (E.Tuple ets)) =
          concat <$> mapM internaliseType' ets
        internaliseType' (E.Array (E.Basic bt) size u _) = do
          dims <- map Ext <$> replicateM (length size) newId
          return [I.arrayOf (I.Basic bt) (ExtShape dims) $
                  internaliseUniqueness u]
        internaliseType' (E.Array (E.Tuple elemts) size u _) = do
          outerdim <- Ext <$> newId
          innerdims <- map Ext <$> replicateM (length size - 1) newId
          ts <- concat <$>
                mapM internaliseType' elemts
          return [ I.arrayOf t (ExtShape $ outerdim : innerdims) $
                   internaliseUniqueness u
                 | t <- ts ]

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
  case internaliseType $ E.addNames rt of
    [rt'] -> [I.arrayVal (concatMap internaliseValue $ A.elems arr) rt']
    ts
      | [] <- A.elems arr -> map emptyOf ts
      | otherwise         -> zipWith asarray ts (transpose arrayvalues)
  where emptyOf = I.arrayVal []
        asarray t vs = I.arrayVal vs t
        arrayvalues = map internaliseValue $ A.elems arr
        -- Above should never happen in well-typed program.
internaliseValue (E.TupVal vs) = concatMap internaliseValue vs
internaliseValue (E.BasicVal bv) = [I.BasicVal bv]
