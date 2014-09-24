module Futhark.Internalise.TypesValues
  (
   -- * Internalising types
    internaliseType
  , internaliseType'
  , internaliseElemType
  , internaliseElemType'
  , internaliseUniqueness
  , internaliseParamTypes

  -- * Internalising values
  , internaliseValue
  , internaliseParamValues
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
                       E.GenElemType als -> [I.TypeBase Rank]
internaliseElemType (Tuple elemts) =
  concatMap internaliseType elemts
internaliseElemType (E.Basic bt)  = [I.Basic bt]

internaliseElemType' :: Monoid (als VName) =>
                     E.GenElemType als -> [I.TypeBase Rank]
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
                   E.GenType als -> [I.TypeBase Rank]

internaliseType t@(E.Array {}) =
  case internaliseType' t of et1:et2:ets -> I.Basic I.Cert : et1 : et2 : ets
                             t'          -> t'
internaliseType (E.Elem et) = internaliseElemType et

internaliseType' :: Monoid (als VName) =>
                    E.GenType als -> [I.TypeBase Rank]
internaliseType' (E.Array (E.Tuple elemts) size u als) =
  concatMap (internaliseType' . arr) elemts
  where arr t = E.arrayOf t (replicate (length size) Nothing) u `E.setAliases` als
internaliseType' (E.Array elemt size u als) =
  map arr $ internaliseElemType' $ elemt `E.setElemAliases` als
  where arr t = I.arrayOf t (Rank $ length size) $ internaliseUniqueness u
internaliseType' (E.Elem et) = internaliseElemType' et

-- Does not return the shape context.
internaliseParamTypes :: Monoid (als VName) =>
                         [E.GenType als] -> [I.DeclType]
internaliseParamTypes = concatMap internaliseType

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
        I.BasicVal I.Checked : map emptyOf ts
      | otherwise         ->
        I.BasicVal I.Checked : zipWith asarray ts (transpose arrayvalues)
  where emptyOf = I.arrayVal []
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
