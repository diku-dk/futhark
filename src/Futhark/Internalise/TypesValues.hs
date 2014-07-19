module Futhark.Internalise.TypesValues
  (
   -- * Internalising types
    internaliseType
  , internaliseType'
  , internaliseElemType
  , internaliseElemType'
  , internaliseUniqueness

  -- * Internalising values
  , internaliseValue
  , internaliseParamValues

  , noInfoToUnit
  )
  where

import qualified Data.Array as A
import Data.Maybe
import Data.Monoid
import Data.List

import Futhark.Representation.External as E
import Futhark.Representation.Basic as I

import Prelude hiding (mapM)

internaliseUniqueness :: E.Uniqueness -> I.Uniqueness
internaliseUniqueness E.Nonunique = I.Nonunique
internaliseUniqueness E.Unique = I.Unique

internaliseElemType :: Monoid (als VName) =>
                       E.GenElemType als -> [I.TypeBase (als VName) Rank]
internaliseElemType (Tuple elemts) =
  concatMap internaliseType elemts
internaliseElemType (E.Basic bt)  = [I.Basic bt]

internaliseElemType' :: Monoid (als VName) =>
                     E.GenElemType als -> [I.TypeBase (als VName) Rank]
internaliseElemType' (Tuple elemts) =
  concatMap internaliseType' elemts
internaliseElemType' t = internaliseElemType t

-- | Perform the tuple internaliseation on a single type.
--
-- Example (the 'setAliases' part is to disambiguate the type of the
-- aliasing information):
--
-- >>> internaliseType $ (Elem $ Tuple [Elem $ Tuple [Elem Int, Elem Real], Elem Char]) `setAliases` NoInfo
-- Elem (Tuple [Elem Int,Elem Int,Elem Real,Elem Real,Elem Char])
internaliseType :: Monoid (als VName) =>
                   E.GenType als -> [I.TypeBase (als VName) Rank]

internaliseType t@(E.Array {}) =
  case internaliseType' t of et1:et2:ets -> I.Basic I.Cert : et1 : et2 : ets
                             t'          -> t'
internaliseType (E.Elem et) = internaliseElemType et

internaliseType' :: Monoid (als VName) =>
                    E.GenType als -> [I.TypeBase (als VName) Rank]
internaliseType' (E.Array (E.Tuple elemts) size u als) =
  concatMap (internaliseType' . arr) elemts
  where arr t = E.arrayOf t (replicate (length size) Nothing) u `E.setAliases` als
internaliseType' (E.Array elemt size u als) =
  map (`I.setAliases` als) ets
  where ets = case internaliseElemType' $ elemt `E.setElemAliases` als of
                elemts -> map arr elemts
        arr t = I.arrayOf t (Rank $ length size) $ internaliseUniqueness u
internaliseType' (E.Elem et) = internaliseElemType' et

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
      | [] <- A.elems arr ->
        I.BasicVal I.Checked : map (emptyOf . noInfoToUnit) ts
      | otherwise         ->
        I.BasicVal I.Checked : zipWith asarray ts (transpose arrayvalues)
  where emptyOf t = I.blankValue $ I.arrayOf t (Rank 1) I.Nonunique
        asarray t vs = I.arrayVal vs t
        arrayvalues = map internaliseValue $ A.elems arr
        -- Above should never happen in well-typed program.
internaliseValue (E.TupVal vs) = concatMap internaliseValue vs
internaliseValue (E.BasicVal bv) = [I.BasicVal bv]

-- | Internalise the values given as per 'internaliseValue', but also
-- add their sizes.
internaliseParamValues :: [E.Value] -> [I.Value]
internaliseParamValues vs = concatMap valueShapes vs' ++ vs'
  where vs' = concatMap internaliseValue vs
        valueShapes = map (I.BasicVal . I.IntVal) . I.valueShape

noInfoToUnit :: I.TypeBase (NoInfo VName) shape -> I.TypeBase () shape
noInfoToUnit = (`I.setAliases` ())
