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
    permute,
    reshape,
    coerce,
    slice,
    flatSlice,
    expand,
    shape,
    rank,
    isDirect,
    substituteInIxFun,
    substituteInLMAD,
    existentialize,
    existentialized,
    closeEnough,
    disjoint,
    disjoint2,
    disjoint3,
    range,
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
  ( equivalent,
    flatSlice,
    index,
    iota,
    isDirect,
    mkExistential,
    permute,
    range,
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

instance (Pretty num) => Pretty (IxFun num) where
  pretty (IxFun lmad oshp) =
    braces . semistack $
      [ "base:" <+> brackets (commasep $ map pretty oshp),
        "LMAD:" <+> pretty lmad
      ]

instance (Substitute num) => Substitute (IxFun num) where
  substituteNames substs = fmap $ substituteNames substs

instance (Substitute num) => Rename (IxFun num) where
  rename = substituteRename

instance (FreeIn num) => FreeIn (IxFun num) where
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
  (Ord a) =>
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
iotaOffset :: (IntegralExp num) => num -> Shape num -> IxFun num
iotaOffset o ns = IxFun (LMAD.iota o ns) ns

-- | iota.
iota :: (IntegralExp num) => Shape num -> IxFun num
iota = iotaOffset 0

-- | Create a single-LMAD index function that is existential in
-- everything except shape, with the provided shape.
mkExistential :: Int -> Shape (Ext a) -> Int -> IxFun (Ext a)
mkExistential basis_rank lmad_shape start =
  IxFun (LMAD.mkExistential lmad_shape start) basis
  where
    basis = take basis_rank $ map Ext [start + 1 + length lmad_shape ..]

-- | Permute dimensions.
permute ::
  (IntegralExp num) =>
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
rank :: (IntegralExp num) => IxFun num -> Int
rank (IxFun (LMAD _ sss) _) = length sss

-- | Conceptually expand index function to be a particular slice of
-- another by adjusting the offset and strides.  Used for memory
-- expansion.
expand ::
  (Eq num, IntegralExp num) => num -> num -> IxFun num -> Maybe (IxFun num)
expand o p (IxFun lmad base) =
  let onDim ld = ld {LMAD.ldStride = p * LMAD.ldStride ld}
      lmad' =
        LMAD
          (o + p * LMAD.offset lmad)
          (map onDim (LMAD.dims lmad))
   in Just $ IxFun lmad' base

-- | Turn all the leaves of the index function into 'Ext's, except for
--  the shape, which where the leaves are simply made 'Free'.
existentialize ::
  Int ->
  IxFun (TPrimExp Int64 a) ->
  IxFun (TPrimExp Int64 (Ext a))
existentialize start (IxFun lmad base) = evalState (IxFun <$> lmad' <*> base') start
  where
    mkExt = do
      i <- get
      put $ i + 1
      pure $ TPrimExp $ LeafExp (Ext i) int64
    lmad' = LMAD <$> mkExt <*> mapM onDim (dims lmad)
    base' = traverse (const mkExt) base
    onDim ld = LMADDim <$> mkExt <*> pure (fmap Free (ldShape ld))

-- | Retrieve those elements that 'existentialize' changes. That is,
-- everything except the shape (and in the same order as
-- 'existentialise' existentialises them).
existentialized :: IxFun a -> [a]
existentialized (IxFun (LMAD offset dims) base) =
  offset : concatMap onDim dims <> base
  where
    onDim (LMADDim ldstride _) = [ldstride]

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

-- | The largest possible linear address reachable by this index
-- function.
range :: (Pretty num) => IxFun (TPrimExp Int64 num) -> TPrimExp Int64 num
range = LMAD.range . ixfunLMAD
