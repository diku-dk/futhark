-- | Utility declarations for performing range analysis.
module Futhark.Representation.AST.Attributes.Ranges
       ( Bound
       , Range
       , unknownRange
       , Ranged (..)
       , subExpRange
       , expRanges
       )
       where

import Futhark.Representation.AST.Attributes.TypeOf
import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Lore (Lore)
import Futhark.Analysis.ScalExp

-- | A (possibly undefined) scalar bound on a value.
type Bound = Maybe ScalExp

-- | Upper and lower bound, both inclusive.
type Range = (Bound, Bound)

-- | A range in which both upper and lower bounds are 'Nothing.
unknownRange :: Range
unknownRange = (Nothing, Nothing)

minimumBound :: Bound -> Bound -> Bound
minimumBound Nothing   Nothing   = Nothing
minimumBound (Just se) (Nothing) = Just se
minimumBound Nothing   (Just se) = Just se
minimumBound (Just x)  (Just y)  = Just $ MaxMin True [x, y]

maximumBound :: Bound -> Bound -> Bound
maximumBound Nothing   Nothing   = Nothing
maximumBound (Just se) Nothing   = Just se
maximumBound Nothing   (Just se) = Just se
maximumBound (Just x)  (Just y)  = Just $ MaxMin False [x, y]

-- | The lore has embedded range information.  Note that it may not be
-- up to date, unless whatever maintains the syntax tree is careful.
class Lore lore => Ranged lore where
  -- | The range of the value parts of the 'Body'.  The default method
  -- returns empty/unknown ranges.
  bodyRanges :: Body lore -> [Range]
  bodyRanges body =
    replicate (length $ resultSubExps $ bodyResult body)
    (Nothing, Nothing)

-- | The range of a subexpression.
subExpRange :: SubExp -> Range
subExpRange se = (Just $ subExpToScalExp se,
                  Just $ subExpToScalExp se)

primOpRanges :: PrimOp lore -> Range
primOpRanges (SubExp se) =
  subExpRange se
primOpRanges (Iota n) =
  (Just zero, Just $ subExpToScalExp n `SMinus` one)
  where zero = Val $ IntVal 0
        one = Val $ IntVal 1
primOpRanges (Replicate _ v) =
  subExpRange v
primOpRanges (Rearrange _ _ v) =
  (Just $ Id v, Just $ Id v)
primOpRanges (Split _ _ v) =
  (Just $ Id v, Just $ Id v)
primOpRanges (Copy se) =
  subExpRange se
primOpRanges (Index _ v _) =
  (Just (Id v), Just (Id v))
primOpRanges _ =
  (Nothing, Nothing)

expRanges :: Ranged lore => Exp lore -> [Range]
expRanges (PrimOp op) =
  [primOpRanges op]
expRanges (If _ tbranch fbranch _) =
  zip
  (zipWith minimumBound t_lower f_lower)
  (zipWith maximumBound t_upper f_upper)
  where (t_lower, t_upper) = unzip $ bodyRanges tbranch
        (f_lower, f_upper) = unzip $ bodyRanges fbranch
expRanges e =
  replicate (length $ expExtType e) unknownRange
