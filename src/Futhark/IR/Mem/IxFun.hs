{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | This module contains a representation for the index function based on
-- linear-memory accessor descriptors; see Zhu, Hoeflinger and David work.
module Futhark.IR.Mem.IxFun
  ( IxFun (..),
    Shape,
    LMAD (..),
    LMADDim (..),
    Monotonicity (..),
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
    shape,
    permutation,
    rank,
    linearWithOffset,
    rearrangeWithOffset,
    isDirect,
    isLinear,
    substituteInIxFun,
    substituteInLMAD,
    existentialize,
    closeEnough,
    equivalent,
    permuteInv,
    conservativeFlatten,
    disjoint,
    disjoint2,
    disjoint3,
    dynamicEqualsLMAD,
  )
where

import Control.Category
import Control.Monad
import Control.Monad.State
import Data.List (sort, zip4)
import Data.Map.Strict qualified as M
import Data.Traversable
import Futhark.Analysis.PrimExp
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Mem.LMAD hiding
  ( flatSlice,
    index,
    iota,
    mkExistential,
    permutation,
    permute,
    reshape,
    shape,
    slice,
  )
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.IR.Prop
import Futhark.IR.Syntax
  ( DimIndex (..),
    FlatSlice (..),
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
    base :: Shape num,
    -- | ignoring permutations, is the index function contiguous?
    contiguous :: Bool
  }
  deriving (Show, Eq)

instance Pretty num => Pretty (IxFun num) where
  pretty (IxFun lmad oshp cg) =
    braces . semistack $
      [ "base:" <+> brackets (commasep $ map pretty oshp),
        "contiguous:" <+> if cg then "true" else "false",
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
  traverse f (IxFun lmad oshp cg) =
    IxFun <$> traverse f lmad <*> traverse f oshp <*> pure cg

-- | Substitute a name with a PrimExp in an index function.
substituteInIxFun ::
  Ord a =>
  M.Map a (TPrimExp t a) ->
  IxFun (TPrimExp t a) ->
  IxFun (TPrimExp t a)
substituteInIxFun tab (IxFun lmad oshp cg) =
  IxFun
    (substituteInLMAD tab lmad)
    (map (TPrimExp . substituteInPrimExp tab' . untyped) oshp)
    cg
  where
    tab' = fmap untyped tab

-- | Is this is a row-major array?
isDirect :: (Eq num, IntegralExp num) => IxFun num -> Bool
isDirect ixfun@(IxFun (LMAD offset dims) oshp True) =
  let strides_expected = reverse $ scanl (*) 1 (reverse (tail oshp))
   in hasContiguousPerm ixfun
        && length oshp == length dims
        && offset == 0
        && all
          (\(LMADDim s n p _, m, d, se) -> s == se && n == d && p == m)
          (zip4 dims [0 .. length dims - 1] oshp strides_expected)
isDirect _ = False

-- | Does the index function have an ascending permutation?
hasContiguousPerm :: IxFun num -> Bool
hasContiguousPerm (IxFun lmad _ _) =
  let perm = LMAD.permutation lmad
   in perm == sort perm

-- | The index space of the index function.  This is the same as the
-- shape of arrays that the index function supports.
shape :: (Eq num, IntegralExp num) => IxFun num -> Shape num
shape (IxFun lmad _ _) =
  permuteFwd (LMAD.permutation lmad) $ LMAD.shapeBase lmad

-- | The permutation of the first LMAD of the index function.
permutation :: IxFun num -> Permutation
permutation = map LMAD.ldPerm . LMAD.dims . ixfunLMAD

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
iotaOffset o ns = IxFun (LMAD.iota Inc o ns) ns True

-- | iota.
iota :: IntegralExp num => Shape num -> IxFun num
iota = iotaOffset 0

-- | Create a contiguous single-LMAD index function that is
-- existential in everything, with the provided permutation,
-- monotonicity, and contiguousness.
mkExistential :: Int -> [(Int, Monotonicity)] -> Bool -> Int -> IxFun (Ext a)
mkExistential basis_rank perm contig start =
  IxFun (LMAD.mkExistential perm start) basis contig
  where
    basis = take basis_rank $ map Ext [start + 1 + dims_rank * 2 ..]
    dims_rank = length perm

-- | Permute dimensions.
permute ::
  IntegralExp num =>
  IxFun num ->
  Permutation ->
  IxFun num
permute (IxFun lmad oshp cg) perm_new =
  IxFun (LMAD.permute lmad perm_new) oshp cg

slicePreservesContiguous ::
  (Eq num, IntegralExp num) =>
  LMAD num ->
  Slice num ->
  Bool
slicePreservesContiguous (LMAD _ dims) (Slice slc) =
  -- remove from the slice the LMAD dimensions that have stride 0.
  -- If the LMAD was contiguous in mem, then these dims will not
  -- influence the contiguousness of the result.
  -- Also normalize the input slice, i.e., 0-stride and size-1
  -- slices are rewritten as DimFixed.
  let (dims', slc') =
        unzip $
          filter ((/= 0) . ldStride . fst) $
            zip dims $
              map normIndex slc
      -- Check that:
      -- 1. a clean split point exists between Fixed and Sliced dims
      -- 2. the outermost sliced dim has +/- 1 stride.
      -- 3. the rest of inner sliced dims are full.
      (_, success) =
        foldl
          ( \(found, res) (slcdim, LMADDim _ n _ _) ->
              case (slcdim, found) of
                (DimFix {}, True) -> (found, False)
                (DimFix {}, False) -> (found, res)
                (DimSlice _ _ ds, False) ->
                  -- outermost sliced dim: +/-1 stride
                  let res' = (ds == 1 || ds == -1)
                   in (True, res && res')
                (DimSlice _ ne ds, True) ->
                  -- inner sliced dim: needs to be full
                  let res' = (n == ne) && (ds == 1 || ds == -1)
                   in (found, res && res')
          )
          (False, True)
          $ zip slc' dims'
   in success
  where
    normIndex ::
      (Eq num, IntegralExp num) =>
      DimIndex num ->
      DimIndex num
    normIndex (DimSlice b 1 _) = DimFix b
    normIndex (DimSlice b _ 0) = DimFix b
    normIndex d = d

-- | Slice an index function.
slice ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  Slice num ->
  IxFun num
slice ixfun@(IxFun lmad@(LMAD _ _) oshp cg) (Slice is)
  -- Avoid identity slicing.
  | is == map (unitSlice 0) (shape ixfun) = ixfun
  | otherwise =
      IxFun (LMAD.slice lmad (Slice is)) oshp cg'
  where
    cg' = cg && slicePreservesContiguous lmad (Slice (permuteInv (LMAD.permutation lmad) is))

-- | Flat-slice an index function.
flatSlice ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  FlatSlice num ->
  Maybe (IxFun num)
flatSlice (IxFun lmad oshp cg) s = do
  lmad' <- LMAD.flatSlice lmad s
  Just $ IxFun lmad' oshp cg

-- | Reshape an index function.
--
-- There are four conditions that all must hold for the result of a reshape
-- operation to remain in the one-LMAD domain:
--
--   (1) the permutation of the underlying LMAD must leave unchanged
--       the LMAD dimensions that were *not* reshape coercions.
--   (2) the repetition of dimensions of the underlying LMAD must
--       refer only to the coerced-dimensions of the reshape operation.
--   (3) finally, the underlying memory is contiguous (and monotonous).
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
reshape (IxFun lmad _ cg) new_shape = do
  guard cg
  lmad' <- LMAD.reshape lmad new_shape
  Just $ IxFun lmad' new_shape cg

-- | Coerce an index function to look like it has a new shape.
-- Dynamically the shape must be the same.
coerce ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  Shape num ->
  IxFun num
coerce (IxFun lmad _ cg) new_shape =
  IxFun (onLMAD lmad) new_shape cg
  where
    onLMAD (LMAD offset dims) = LMAD offset $ zipWith onDim dims new_shape
    onDim ld d = ld {ldShape = d}

-- | The number of dimensions in the domain of the input function.
rank ::
  IntegralExp num =>
  IxFun num ->
  Int
rank (IxFun (LMAD _ sss) _ _) = length sss

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
rebase
  new_base@(IxFun lmad_base _ cg_base)
  ixfun@(IxFun lmad shp cg) = do
    let dims = LMAD.dims lmad
        perm = LMAD.permutation lmad
        perm_base = LMAD.permutation lmad_base

    guard $
      -- Core rebase condition.
      base ixfun == shape new_base
        -- Conservative safety conditions: ixfun is contiguous and has known
        -- monotonicity for all dimensions.
        && cg
        && all ((/= Unknown) . ldMon) dims
        -- XXX: We should be able to handle some basic cases where both index
        -- functions have non-trivial permutations.
        && (hasContiguousPerm ixfun || hasContiguousPerm new_base)
        -- We need the permutations to be of the same size if we want to compose
        -- them.  They don't have to be of the same size if the ixfun has a trivial
        -- permutation.  Supporting this latter case allows us to rebase when ixfun
        -- has been created by slicing with fixed dimensions.
        && (length perm == length perm_base || hasContiguousPerm ixfun)
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

    -- Compose permutations, reverse strides and adjust offset if necessary.
    let perm_base' =
          if hasContiguousPerm ixfun
            then perm_base
            else map (perm !!) perm_base
        lmad_base' = LMAD.setPermutation perm_base' lmad_base
        dims_base = LMAD.dims lmad_base'
        n_fewer_dims = length dims_base - length dims
        (dims_base', offs_contrib) =
          unzip $
            zipWith
              ( \(LMADDim s1 n1 p1 _) (LMADDim _ _ _ m2) ->
                  let (s', off')
                        | m2 == Inc = (s1, 0)
                        | otherwise = (s1 * (-1), s1 * (n1 - 1))
                   in (LMADDim s' n1 (p1 - n_fewer_dims) Inc, off')
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
    pure $ IxFun lmad_base'' shp (cg && cg_base)

-- | If the memory support of the index function is contiguous and row-major
-- (i.e., no transpositions etc.), then this should
-- return the offset from which the memory-support of this index function
-- starts.
linearWithOffset ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  num ->
  Maybe num
linearWithOffset ixfun@(IxFun lmad _ cg) elem_size
  | hasContiguousPerm ixfun && cg && ixfunMonotonicity ixfun == Inc =
      Just $ LMAD.offset lmad * elem_size
linearWithOffset _ _ = Nothing

-- | Similar restrictions to @linearWithOffset@ except for transpositions, which
-- are returned together with the offset.
rearrangeWithOffset ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  num ->
  Maybe (num, [(Int, num)])
rearrangeWithOffset (IxFun lmad oshp cg) elem_size = do
  -- Note that @cg@ describes whether the index function is
  -- contiguous, *ignoring permutations*.  This function requires that
  -- functionality.
  let perm = LMAD.permutation lmad
      perm_contig = [0 .. length perm - 1]
  offset <-
    linearWithOffset
      (IxFun (LMAD.setPermutation perm_contig lmad) oshp cg)
      elem_size
  pure (offset, zip perm (permuteFwd perm (LMAD.shapeBase lmad)))

-- | Is this a row-major array starting at offset zero?
isLinear :: (Eq num, IntegralExp num) => IxFun num -> Bool
isLinear = (== Just 0) . flip linearWithOffset 1

-- | Check monotonicity of an index function.
ixfunMonotonicity ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  Monotonicity
ixfunMonotonicity = LMAD.monotonicity . ixfunLMAD

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
    -- This treats ixf1 as the "declared type" that we are matching against.
    && (contiguous ixf1 <= contiguous ixf2)
  where
    closeEnoughLMADs lmad1 lmad2 =
      length (LMAD.dims lmad1) == length (LMAD.dims lmad2)
        && map ldPerm (LMAD.dims lmad1) == map ldPerm (LMAD.dims lmad2)

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
        && map ldPerm (LMAD.dims lmad1) == map ldPerm (LMAD.dims lmad2)
        && LMAD.offset lmad1 == LMAD.offset lmad2
        && map ldStride (LMAD.dims lmad1) == map ldStride (LMAD.dims lmad2)
