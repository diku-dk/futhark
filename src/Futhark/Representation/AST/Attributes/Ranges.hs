-- | Utility declarations for performing range analysis.
module Futhark.Representation.AST.Attributes.Ranges
       ( Bound
       , KnownBound (..)
       , boundToScalExp
       , minimumBound
       , maximumBound
       , Range
       , unknownRange
       , ScalExpRange
       , Ranged (..)
       , subExpRange
       , expRanges
       )
       where

import Control.Applicative
import Data.Monoid

import Prelude

import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Lore (Lore)
import Futhark.Analysis.ScalExp
import Futhark.Substitute
import Futhark.Renamer
import qualified Text.PrettyPrint.Mainland as PP

-- | A known bound on a value.
data KnownBound = VarBound VName
                  -- ^ Has the same bounds as this variable.  VERY
                  -- IMPORTANT: this variable may be an array, so it
                  -- cannot be immediately translated to a 'ScalExp'.
                | MinimumBound KnownBound KnownBound
                  -- ^ Bounded by the minimum of these two bounds.
                | MaximumBound KnownBound KnownBound
                  -- ^ Bounded by the maximum of these two bounds.
                | ScalarBound ScalExp
                  -- ^ Bounded by this scalar expression.
                deriving (Eq, Ord, Show)

instance Substitute KnownBound where
  substituteNames substs (VarBound name) =
    VarBound $ substituteNames substs name
  substituteNames substs (MinimumBound b1 b2) =
    MinimumBound (substituteNames substs b1) (substituteNames substs b2)
  substituteNames substs (MaximumBound b1 b2) =
    MaximumBound (substituteNames substs b1) (substituteNames substs b2)
  substituteNames substs (ScalarBound se) =
    ScalarBound $ substituteNames substs se

instance Rename KnownBound where
  rename (VarBound v)         = VarBound <$> rename v
  rename (MinimumBound b1 b2) = MinimumBound <$> rename b1 <*> rename b2
  rename (MaximumBound b1 b2) = MaximumBound <$> rename b1 <*> rename b2
  rename (ScalarBound e)      = ScalarBound <$> rename e

instance FreeIn KnownBound where
  freeIn (VarBound v)         = freeIn v
  freeIn (MinimumBound b1 b2) = freeIn b1 <> freeIn b2
  freeIn (MaximumBound b1 b2) = freeIn b1 <> freeIn b2
  freeIn (ScalarBound e)      = freeIn e

instance PP.Pretty KnownBound where
  ppr (VarBound v) =
    PP.text "variable " <> PP.ppr v
  ppr (MinimumBound b1 b2) =
    PP.text "min" <> PP.parens (PP.ppr b1 <> PP.comma PP.<+> PP.ppr b2)
  ppr (MaximumBound b1 b2) =
    PP.text "min" <> PP.parens (PP.ppr b1 <> PP.comma PP.<+> PP.ppr b2)
  ppr (ScalarBound e) =
    PP.ppr e

-- | Convert the bound to a scalar expression if possible.  This is
-- possible for all bounds that do not contain 'VarBound's.
boundToScalExp :: KnownBound -> Maybe ScalExp
boundToScalExp (VarBound _) = Nothing
boundToScalExp (ScalarBound se) = Just se
boundToScalExp (MinimumBound b1 b2) = do
  b1' <- boundToScalExp b1
  b2' <- boundToScalExp b2
  return $ MaxMin True [b1', b2']
boundToScalExp (MaximumBound b1 b2) = do
  b1' <- boundToScalExp b1
  b2' <- boundToScalExp b2
  return $ MaxMin False [b1', b2']

-- | A possibly undefined bound on a value.
type Bound = Maybe KnownBound

-- | Construct a 'MinimumBound' from two possibly known bounds.  The
-- resulting bound will be unknown unless both of the given 'Bound's
-- are known.  This may seem counterintuitive, but it actually makes
-- sense when you consider the task of combining the lower bounds for
-- two different flows of execution (like an @if@ expression).  If we
-- only have knowledge about one of the branches, this means that we
-- have no useful information about the combined lower bound, as the
-- other branch may take any value.
minimumBound :: Bound -> Bound -> Bound
minimumBound (Just x)  (Just y) = Just $ MinimumBound x y
minimumBound _         _        = Nothing

-- | Like 'minimumBound', but constructs a 'MaximumBound'.
maximumBound :: Bound -> Bound -> Bound
maximumBound (Just x)  (Just y) = Just $ MaximumBound x y
maximumBound _         _        = Nothing

-- | Upper and lower bound, both inclusive.
type Range = (Bound, Bound)

-- | A range in which both upper and lower bounds are 'Nothing.
unknownRange :: Range
unknownRange = (Nothing, Nothing)

-- | The range as a pair of scalar expressions.
type ScalExpRange = (Maybe ScalExp, Maybe ScalExp)

-- | The lore has embedded range information.  Note that it may not be
-- up to date, unless whatever maintains the syntax tree is careful.
class Lore lore => Ranged lore where
  -- | The range of the value parts of the 'Body'.
  bodyRanges :: Body lore -> [Range]

  -- | The range of the pattern elements.
  patternRanges :: Pattern lore -> [Range]

-- | The range of a subexpression.
subExpRange :: SubExp -> Range
subExpRange se = (Just lower, Just upper)
  where (lower, upper) = subExpKnownRange se

subExpKnownRange :: SubExp -> (KnownBound, KnownBound)
subExpKnownRange (Var v) =
  (VarBound v,
   VarBound v)
subExpKnownRange (Constant val) =
  (ScalarBound $ Val val,
   ScalarBound $ Val val)

primOpRanges :: PrimOp lore -> [Range]
primOpRanges (SubExp se) =
  [subExpRange se]
primOpRanges (Iota n) =
  [(Just $ ScalarBound zero,
    Just $ ScalarBound $ n' `SMinus` one)]
  where zero = Val $ IntVal 0
        one = Val $ IntVal 1
        n' = case n of
          Var v        -> Id v Int
          Constant val -> Val val
primOpRanges (Replicate _ v) =
  [subExpRange v]
primOpRanges (Rearrange _ _ v) =
  [subExpRange $ Var v]
primOpRanges (Split _ sizeexps v) =
  replicate (length sizeexps) $ subExpRange $ Var v
primOpRanges (Copy se) =
  [subExpRange se]
primOpRanges (Index _ v _) =
  [subExpRange $ Var v]
primOpRanges (Partition _ n _ arr) =
  replicate n unknownRange ++ [subExpRange $ Var arr]
primOpRanges (ArrayLit (e:es) _) =
  [(Just lower, Just upper)]
  where (e_lower, e_upper) = subExpKnownRange e
        (es_lower, es_upper) = unzip $ map subExpKnownRange es
        lower = foldl MinimumBound e_lower es_lower
        upper = foldl MaximumBound e_upper es_upper
primOpRanges _ =
  [unknownRange]

-- | Ranges of the value parts of the expression.
expRanges :: Ranged lore =>
             Exp lore -> [Range]
expRanges (PrimOp op) =
  primOpRanges op
expRanges (If _ tbranch fbranch _) =
  zip
  (zipWith minimumBound t_lower f_lower)
  (zipWith maximumBound t_upper f_upper)
  where (t_lower, t_upper) = unzip $ bodyRanges tbranch
        (f_lower, f_upper) = unzip $ bodyRanges fbranch
expRanges e =
  replicate (expExtTypeSize e) unknownRange
