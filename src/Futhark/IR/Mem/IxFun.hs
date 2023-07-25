{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | This module contains a representation for the index function based on
-- linear-memory accessor descriptors; see Zhu, Hoeflinger and David work.
module Futhark.IR.Mem.IxFun
  ( IxFun (..),
    Shape,
    LMAD (..),
    LMADDim (..),
    index,
    mkExistential,
    iota,
    iotaOffset,
    permute,
    reshape,
    coerce,
    slice,
    flatSlice,
    rebase,
    embed,
    shape,
    rank,
    isDirect,
    substituteInIxFun,
    substituteInLMAD,
    existentialize,
    closeEnough,
    equivalent,
    permuteInv,
    disjoint,
    disjoint2,
    disjoint3,
    dynamicEqualsLMAD,
  )
where

import Control.Category
import Control.Monad
import Control.Monad.State
import Data.Map.Strict qualified as M
import Data.Traversable
import Futhark.Analysis.PrimExp
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Mem.LMAD hiding
  ( flatSlice,
    index,
    iota,
    mkExistential,
    permute,
    rank,
    reshape,
    shape,
    slice,
  )
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.IR.Prop
import Futhark.IR.Syntax
  ( FlatSlice (..),
    Slice (..),
    unitSlice,
  )
import Futhark.IR.Syntax.Core (Ext (..))
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util.IntegralExp
import Futhark.Util.Pretty
import Prelude hiding (gcd, id, mod, (.))

-- | An index function is a mapping from a multidimensional array
-- index space (the domain) to a one-dimensional memory index space.
-- Essentially, it explains where the element at position @[i,j,p]@ of
-- some array is stored inside the flat one-dimensional array that
-- constitutes its memory.  For example, we can use this to
-- distinguish row-major and column-major representations.
--
-- An index function is represented as an LMAD.
data IxFun num = IxFun
  { ixfunLMAD :: LMAD num,
    -- | the shape of the support array, i.e., the original array
    --   that birthed (is the start point) of this index function.
    base :: Shape num
  }
  deriving (Show, Eq)

instance Pretty num => Pretty (IxFun num) where
  pretty (IxFun lmad oshp) =
    braces . semistack $
      [ "base:" <+> brackets (commasep $ map pretty oshp),
        "LMAD:" <+> pretty lmad
      ]

instance Substitute num => Substitute (IxFun num) where
  substituteNames substs = fmap $ substituteNames substs

instance Substitute num => Rename (IxFun num) where
  rename = substituteRename

instance FreeIn num => FreeIn (IxFun num) where
  freeIn' = foldMap freeIn'

instance Functor IxFun where
  fmap = fmapDefault

instance Foldable IxFun where
  foldMap = foldMapDefault

-- It is important that the traversal order here is the same as in
-- mkExistential.
instance Traversable IxFun where
  traverse f (IxFun lmad oshp) =
    IxFun <$> traverse f lmad <*> traverse f oshp

-- | Substitute a name with a PrimExp in an index function.
substituteInIxFun ::
  Ord a =>
  M.Map a (TPrimExp t a) ->
  IxFun (TPrimExp t a) ->
  IxFun (TPrimExp t a)
substituteInIxFun tab (IxFun lmad oshp) =
  IxFun
    (substituteInLMAD tab lmad)
    (map (TPrimExp . substituteInPrimExp tab' . untyped) oshp)
  where
    tab' = fmap untyped tab

-- | Is this is a row-major array?
isDirect :: (Eq num, IntegralExp num) => IxFun num -> Bool
isDirect (IxFun (LMAD offset dims) oshp) =
  let strides_expected = reverse $ scanl (*) 1 (reverse (tail oshp))
   in length oshp == length dims
        && offset == 0
        && all
          (\(LMADDim s n, d, se) -> s == se && n == d)
          (zip3 dims oshp strides_expected)

-- | The index space of the index function.  This is the same as the
-- shape of arrays that the index function supports.
shape :: (Eq num, IntegralExp num) => IxFun num -> Shape num
shape = LMAD.shape . ixfunLMAD

-- | Compute the flat memory index for a complete set @inds@ of array indices
-- and a certain element size @elem_size@.
index ::
  (IntegralExp num, Eq num) =>
  IxFun num ->
  Indices num ->
  num
index = LMAD.index . ixfunLMAD

-- | iota with offset.
iotaOffset :: IntegralExp num => num -> Shape num -> IxFun num
iotaOffset o ns = IxFun (LMAD.iota o ns) ns

-- | iota.
iota :: IntegralExp num => Shape num -> IxFun num
iota = iotaOffset 0

-- | Create a single-LMAD index function that is
-- existential in everything, with the provided permutation.
mkExistential :: Int -> Int -> Int -> IxFun (Ext a)
mkExistential basis_rank lmad_rank start =
  IxFun (LMAD.mkExistential lmad_rank start) basis
  where
    basis = take basis_rank $ map Ext [start + 1 + lmad_rank * 2 ..]

-- | Permute dimensions.
permute ::
  IntegralExp num =>
  IxFun num ->
  Permutation ->
  IxFun num
permute (IxFun lmad oshp) perm_new =
  IxFun (LMAD.permute lmad perm_new) oshp

-- | Slice an index function.
slice ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  Slice num ->
  IxFun num
slice ixfun@(IxFun lmad@(LMAD _ _) oshp) (Slice is)
  -- Avoid identity slicing.
  | is == map (unitSlice 0) (shape ixfun) = ixfun
  | otherwise =
      IxFun (LMAD.slice lmad (Slice is)) oshp

-- | Flat-slice an index function.
flatSlice ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  FlatSlice num ->
  IxFun num
flatSlice (IxFun lmad oshp) s = IxFun (LMAD.flatSlice lmad s) oshp

-- | Reshape an index function.
--
-- There are four conditions that all must hold for the result of a reshape
-- operation to remain in the one-LMAD domain:
--
--   (1) the permutation of the underlying LMAD must leave unchanged
--       the LMAD dimensions that were *not* reshape coercions.
--   (2) the repetition of dimensions of the underlying LMAD must
--       refer only to the coerced-dimensions of the reshape operation.
--
-- If any of these conditions do not hold, then the reshape operation
-- will conservatively add a new LMAD to the list, leading to a
-- representation that provides less opportunities for further
-- analysis
reshape ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  Shape num ->
  Maybe (IxFun num)
reshape (IxFun lmad _) new_shape =
  IxFun <$> LMAD.reshape lmad new_shape <*> pure new_shape

-- | Coerce an index function to look like it has a new shape.
-- Dynamically the shape must be the same.
coerce ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  Shape num ->
  IxFun num
coerce (IxFun lmad _) new_shape =
  IxFun (onLMAD lmad) new_shape
  where
    onLMAD (LMAD offset dims) = LMAD offset $ zipWith onDim dims new_shape
    onDim ld d = ld {ldShape = d}

-- | The number of dimensions in the domain of the input function.
rank :: IntegralExp num => IxFun num -> Int
rank (IxFun (LMAD _ sss) _) = length sss

-- | Essentially @rebase new_base ixfun = ixfun o new_base@
-- Core soundness condition: @base ixfun == shape new_base@
-- Handles the case where a rebase operation can stay within m + n - 1 LMADs,
-- where m is the number of LMADs in the index function, and n is the number of
-- LMADs in the new base.  If both index function have only on LMAD, this means
-- that we stay within the single-LMAD domain.
--
-- We can often stay in that domain if the original ixfun is essentially a
-- slice, e.g. `x[i, (k1,m,s1), (k2,n,s2)] = orig`.
--
-- However, I strongly suspect that for in-place update what we need is actually
-- the INVERSE of the rebase function, i.e., given an index function new-base
-- and another one orig, compute the index function ixfun0 such that:
--
--   new-base == rebase ixfun0 ixfun, or equivalently:
--   new-base == ixfun o ixfun0
--
-- because then I can go bottom up and compose with ixfun0 all the index
-- functions corresponding to the memory block associated with ixfun.
rebase ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  IxFun num ->
  Maybe (IxFun num)
rebase new_base@(IxFun lmad_base _) ixfun@(IxFun lmad shp) = do
  let dims = LMAD.dims lmad

  guard $
    -- Core rebase condition.
    base ixfun == shape new_base
      -- To not have to worry about ixfun having non-1 strides, we also check that
      -- it is a row-major array (modulo permutation, which is handled
      -- separately).  Accept a non-full outermost dimension.  XXX: Maybe this can
      -- be less conservative?
      && and
        ( zipWith3
            (\sn ld inner -> inner || sn == ldShape ld)
            shp
            dims
            (True : replicate (length dims - 1) False)
        )

  -- Reverse strides and adjust offset if necessary.
  let lmad_base' = lmad_base
      dims_base = LMAD.dims lmad_base'
      n_fewer_dims = length dims_base - length dims
      (dims_base', offs_contrib) =
        unzip $
          zipWith
            ( \(LMADDim s1 n1) (LMADDim {}) ->
                let (s', off') = (s1, 0)
                 in (LMADDim s' n1, off')
            )
            -- If @dims@ is morally a slice, it might have fewer dimensions than
            -- @dims_base@.  Drop extraneous outer dimensions.
            (drop n_fewer_dims dims_base)
            dims
      off_base = LMAD.offset lmad_base' + sum offs_contrib
      lmad_base'' =
        LMAD.setShape
          (LMAD.shape lmad)
          ( LMAD
              (off_base + ldStride (last dims_base) * LMAD.offset lmad)
              dims_base'
          )
  pure $ IxFun lmad_base'' shp

embed ::
  (Eq num, IntegralExp num) => num -> num -> [num] -> IxFun num -> Maybe (IxFun num)
embed o op ps (IxFun lmad base) = do
  guard $ length ps == LMAD.rank lmad
  let onDim p ld = ld {LMAD.ldStride = LMAD.ldStride ld * p}
      lmad' =
        LMAD
          (o + op * LMAD.offset lmad)
          (zipWith onDim ps (LMAD.dims lmad))
  pure $ IxFun lmad' base

-- | Turn all the leaves of the index function into 'Ext's.  We
--  require that there's only one LMAD, that the index function is
--  contiguous, and the base shape has only one dimension.
existentialize ::
  IxFun (TPrimExp Int64 a) ->
  IxFun (TPrimExp Int64 (Ext b))
existentialize ixfun = evalState (traverse (const mkExt) ixfun) 0
  where
    mkExt = do
      i <- get
      put $ i + 1
      pure $ TPrimExp $ LeafExp (Ext i) int64

-- | When comparing index functions as part of the type check in KernelsMem,
-- we may run into problems caused by the simplifier. As index functions can be
-- generalized over if-then-else expressions, the simplifier might hoist some of
-- the code from inside the if-then-else (computing the offset of an array, for
-- instance), but now the type checker cannot verify that the generalized index
-- function is valid, because some of the existentials are computed somewhere
-- else. To Work around this, we've had to relax the KernelsMem type-checker
-- a bit, specifically, we've introduced this function to verify whether two
-- index functions are "close enough" that we can assume that they match. We use
-- this instead of `ixfun1 == ixfun2` and hope that it's good enough.
closeEnough :: IxFun num -> IxFun num -> Bool
closeEnough ixf1 ixf2 =
  (length (base ixf1) == length (base ixf2))
    && closeEnoughLMADs (ixfunLMAD ixf1) (ixfunLMAD ixf2)
  where
    closeEnoughLMADs lmad1 lmad2 =
      length (LMAD.dims lmad1) == length (LMAD.dims lmad2)

-- | Returns true if two 'IxFun's are equivalent.
--
-- Equivalence in this case is defined as having the same number of LMADs, with
-- each pair of LMADs matching in permutation, offsets, and strides.
equivalent :: Eq num => IxFun num -> IxFun num -> Bool
equivalent ixf1 ixf2 =
  equivalentLMADs (ixfunLMAD ixf1) (ixfunLMAD ixf2)
  where
    equivalentLMADs lmad1 lmad2 =
      length (LMAD.dims lmad1) == length (LMAD.dims lmad2)
        && LMAD.offset lmad1 == LMAD.offset lmad2
        && map ldStride (LMAD.dims lmad1) == map ldStride (LMAD.dims lmad2)
