{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
       , Ranged
       , RangeOf (..)
       , RangesOf (..)
       , expRanges
       , RangedOp (..)
       , CanBeRanged (..)
       )
       where

import qualified Data.Kind
import qualified Data.Map.Strict as M

import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Syntax
import qualified Futhark.Analysis.ScalExp as SE
import qualified Futhark.Analysis.AlgSimplify as AS
import Futhark.Transform.Substitute
import Futhark.Transform.Rename
import qualified Futhark.Util.Pretty as PP

-- | A known bound on a value.
data KnownBound = VarBound VName
                  -- ^ Has the same bounds as this variable.  VERY
                  -- IMPORTANT: this variable may be an array, so it
                  -- cannot be immediately translated to a 'ScalExp'.
                | MinimumBound KnownBound KnownBound
                  -- ^ Bounded by the minimum of these two bounds.
                | MaximumBound KnownBound KnownBound
                  -- ^ Bounded by the maximum of these two bounds.
                | ScalarBound SE.ScalExp
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
  rename = substituteRename

instance FreeIn KnownBound where
  freeIn' (VarBound v)         = freeIn' v
  freeIn' (MinimumBound b1 b2) = freeIn' b1 <> freeIn' b2
  freeIn' (MaximumBound b1 b2) = freeIn' b1 <> freeIn' b2
  freeIn' (ScalarBound e)      = freeIn' e

instance FreeAttr KnownBound where
  precomputed _ = id

instance PP.Pretty KnownBound where
  ppr (VarBound v) =
    PP.text "variable " <> PP.ppr v
  ppr (MinimumBound b1 b2) =
    PP.text "min" <> PP.parens (PP.ppr b1 <> PP.comma PP.<+> PP.ppr b2)
  ppr (MaximumBound b1 b2) =
    PP.text "max" <> PP.parens (PP.ppr b1 <> PP.comma PP.<+> PP.ppr b2)
  ppr (ScalarBound e) =
    PP.ppr e

-- | Convert the bound to a scalar expression if possible.  This is
-- possible for all bounds that do not contain 'VarBound's.
boundToScalExp :: KnownBound -> Maybe SE.ScalExp
boundToScalExp (VarBound _) = Nothing
boundToScalExp (ScalarBound se) = Just se
boundToScalExp (MinimumBound b1 b2) = do
  b1' <- boundToScalExp b1
  b2' <- boundToScalExp b2
  return $ SE.MaxMin True [b1', b2']
boundToScalExp (MaximumBound b1 b2) = do
  b1' <- boundToScalExp b1
  b2' <- boundToScalExp b2
  return $ SE.MaxMin False [b1', b2']

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
type ScalExpRange = (Maybe SE.ScalExp, Maybe SE.ScalExp)

-- | The lore has embedded range information.  Note that it may not be
-- up to date, unless whatever maintains the syntax tree is careful.
type Ranged lore = (Attributes lore,
                    RangedOp (Op lore),
                    RangeOf (LetAttr lore),
                    RangesOf (BodyAttr lore))

-- | Something that contains range information.
class RangeOf a where
  -- | The range of the argument element.
  rangeOf :: a -> Range

instance RangeOf Range where
  rangeOf = id

instance RangeOf attr => RangeOf (PatElemT attr) where
  rangeOf = rangeOf . patElemAttr

instance RangeOf SubExp where
  rangeOf se = (Just lower, Just upper)
    where (lower, upper) = subExpKnownRange se

-- | Something that contains range information for several things,
-- most notably 'Body' or 'Pattern'.
class RangesOf a where
  -- | The ranges of the argument.
  rangesOf :: a -> [Range]

instance RangeOf a => RangesOf [a] where
  rangesOf = map rangeOf

instance RangeOf attr => RangesOf (PatternT attr) where
  rangesOf = map rangeOf . patternElements

instance Ranged lore => RangesOf (Body lore) where
  rangesOf = rangesOf . bodyAttr

subExpKnownRange :: SubExp -> (KnownBound, KnownBound)
subExpKnownRange (Var v) =
  (VarBound v,
   VarBound v)
subExpKnownRange (Constant val) =
  (ScalarBound $ SE.Val val,
   ScalarBound $ SE.Val val)

-- | The range of a scalar expression.
scalExpRange :: SE.ScalExp -> Range
scalExpRange se =
  (Just $ ScalarBound se, Just $ ScalarBound se)

primOpRanges :: BasicOp lore -> [Range]
primOpRanges (SubExp se) =
  [rangeOf se]

primOpRanges (BinOp (Add t) x y) =
  [scalExpRange $ SE.SPlus (SE.subExpToScalExp x $ IntType t) (SE.subExpToScalExp y $ IntType t)]
primOpRanges (BinOp (Sub t) x y) =
  [scalExpRange $ SE.SMinus (SE.subExpToScalExp x $ IntType t) (SE.subExpToScalExp y $ IntType t)]
primOpRanges (BinOp (Mul t) x y) =
  [scalExpRange $ SE.STimes (SE.subExpToScalExp x $ IntType t) (SE.subExpToScalExp y $ IntType t)]
primOpRanges (BinOp (SDiv t) x y) =
  [scalExpRange $ SE.SDiv (SE.subExpToScalExp x $ IntType t) (SE.subExpToScalExp y $ IntType t)]

primOpRanges (ConvOp (SExt from to) x)
  | from < to = [rangeOf x]

primOpRanges (Iota n x s Int32) =
  [(Just $ ScalarBound x',
    Just $ ScalarBound $ x' + (n' - 1) * s')]
  where n' = case n of
          Var v        -> SE.Id v $ IntType Int32
          Constant val -> SE.Val val
        x' = case x of
          Var v        -> SE.Id v $ IntType Int32
          Constant val -> SE.Val val
        s' = case s of
          Var v        -> SE.Id v $ IntType Int32
          Constant val -> SE.Val val
primOpRanges (Replicate _ v) =
  [rangeOf v]
primOpRanges (Rearrange _ v) =
  [rangeOf $ Var v]
primOpRanges (Copy se) =
  [rangeOf $ Var se]
primOpRanges (Index v _) =
  [rangeOf $ Var v]
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
expRanges (BasicOp op) =
  primOpRanges op
expRanges (If _ tbranch fbranch _) =
  zip
  (zipWith minimumBound t_lower f_lower)
  (zipWith maximumBound t_upper f_upper)
  where (t_lower, t_upper) = unzip $ rangesOf tbranch
        (f_lower, f_upper) = unzip $ rangesOf fbranch
expRanges (DoLoop ctxmerge valmerge (ForLoop i Int32 iterations _) body) =
  zipWith returnedRange valmerge $ rangesOf body
  where bound_in_loop =
          namesFromList $ i : map (paramName . fst) (ctxmerge++valmerge) ++
          concatMap (patternNames . stmPattern) (bodyStms body)

        returnedRange mergeparam (lower, upper) =
          (returnedBound mergeparam lower,
           returnedBound mergeparam upper)

        returnedBound (param, mergeinit) (Just bound)
          | paramType param == Prim (IntType Int32),
            Just bound' <- boundToScalExp bound,
            let se_diff =
                  AS.simplify (SE.SMinus (SE.Id (paramName param) $ IntType Int32) bound') M.empty,
            namesIntersect bound_in_loop $ freeIn se_diff =
              Just $ ScalarBound $ SE.SPlus (SE.subExpToScalExp mergeinit $ IntType Int32) $
              SE.STimes se_diff $ SE.MaxMin False
              [SE.subExpToScalExp iterations $ IntType Int32, 0]
        returnedBound _ _ = Nothing
expRanges (Op ranges) = opRanges ranges
expRanges e =
  replicate (expExtTypeSize e) unknownRange

class IsOp op => RangedOp op where
  opRanges :: op -> [Range]

instance RangedOp () where
  opRanges () = []

class RangedOp (OpWithRanges op) =>
      CanBeRanged op where
  type OpWithRanges op :: Data.Kind.Type
  removeOpRanges :: OpWithRanges op -> op
  addOpRanges :: op -> OpWithRanges op

instance CanBeRanged () where
  type OpWithRanges () = ()
  removeOpRanges = id
  addOpRanges = id
