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
    lmadShape,
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
    hasOneLmad,
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
import Data.Function (on, (&))
import Data.List (elemIndex, partition, sort, sortBy, zip4, zipWith4)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Traversable
import Futhark.Analysis.AlgSimplify qualified as AlgSimplify
import Futhark.Analysis.PrimExp
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Mem.Interval
import Futhark.IR.Prop
import Futhark.IR.Syntax
  ( DimIndex (..),
    FlatDimIndex (..),
    FlatSlice (..),
    Slice (..),
    Type,
    dimFix,
    flatSliceDims,
    flatSliceStrides,
    unitSlice,
  )
import Futhark.IR.Syntax.Core (Ext (..), VName (..))
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util
import Futhark.Util.IntegralExp
import Futhark.Util.Pretty
import Prelude hiding (gcd, id, mod, (.))

-- | The shape of an index function.
type Shape num = [num]

type Indices num = [num]

type Permutation = [Int]

-- | The physical element ordering alongside a dimension, i.e. the
-- sign of the stride.
data Monotonicity
  = -- | Increasing.
    Inc
  | -- | Decreasing.
    Dec
  | -- | Unknown.
    Unknown
  deriving (Show, Eq)

-- | A single dimension in an 'LMAD'.
data LMADDim num = LMADDim
  { ldStride :: num,
    ldShape :: num,
    ldPerm :: Int,
    ldMon :: Monotonicity
  }
  deriving (Show, Eq)

instance Ord Monotonicity where
  (<=) _ Inc = True
  (<=) Unknown _ = True
  (<=) _ Unknown = False
  (<=) Inc Dec = False
  (<=) _ Dec = True

instance Ord num => Ord (LMADDim num) where
  (LMADDim s1 q1 p1 m1) <= (LMADDim s2 q2 p2 m2) =
    ([q1, s1] < [q2, s2])
      || ( ([q1, s1] == [q2, s2])
             && ( (p1 < p2)
                    || ( (p1 == p2)
                           && (m1 <= m2)
                       )
                )
         )

-- | LMAD's representation consists of a general offset and for each dimension a
-- stride, number of elements (or shape), permutation, and
-- monotonicity. Note that the permutation is not strictly necessary in that the
-- permutation can be performed directly on LMAD dimensions, but then it is
-- difficult to extract the permutation back from an LMAD.
--
-- LMAD algebra is closed under composition w.r.t. operators such as
-- permute, index and slice.  However, other operations, such as
-- reshape, cannot always be represented inside the LMAD algebra.
--
-- It follows that the general representation of an index function is a list of
-- LMADS, in which each following LMAD in the list implicitly corresponds to an
-- irregular reshaping operation.
--
-- However, we expect that the common case is when the index function is one
-- LMAD -- we call this the "nice" representation.
--
-- Finally, the list of LMADs is kept in an @IxFun@ together with the shape of
-- the original array, and a bit to indicate whether the index function is
-- contiguous, i.e., if we instantiate all the points of the current index
-- function, do we get a contiguous memory interval?
--
-- By definition, the LMAD \( \sigma + \{ (n_1, s_1), \ldots, (n_k, s_k) \} \),
-- where \(n\) and \(s\) denote the shape and stride of each dimension, denotes
-- the set of points:
--
-- \[
--    \{ ~ \sigma + i_1 * s_1 + \ldots + i_m * s_m ~ | ~ 0 \leq i_1 < n_1, \ldots, 0 \leq i_m < n_m ~ \}
-- \]
data LMAD num = LMAD
  { lmadOffset :: num,
    lmadDims :: [LMADDim num]
  }
  deriving (Show, Eq, Ord)

-- | An index function is a mapping from a multidimensional array
-- index space (the domain) to a one-dimensional memory index space.
-- Essentially, it explains where the element at position @[i,j,p]@ of
-- some array is stored inside the flat one-dimensional array that
-- constitutes its memory.  For example, we can use this to
-- distinguish row-major and column-major representations.
--
-- An index function is represented as a sequence of 'LMAD's.
data IxFun num = IxFun
  { ixfunLMADs :: NonEmpty (LMAD num),
    -- | the shape of the support array, i.e., the original array
    --   that birthed (is the start point) of this index function.
    base :: Shape num,
    -- | ignoring permutations, is the index function contiguous?
    contiguous :: Bool
  }
  deriving (Show, Eq)

instance Pretty Monotonicity where
  pretty = pretty . show

instance Pretty num => Pretty (LMAD num) where
  pretty (LMAD offset dims) =
    braces . semistack $
      [ "offset:" <+> group (pretty offset),
        "strides:" <+> p ldStride,
        "shape:" <+> p ldShape,
        "permutation:" <+> p ldPerm,
        "monotonicity:" <+> p ldMon
      ]
    where
      p f = group $ brackets $ align $ commasep $ map (pretty . f) dims

instance Pretty num => Pretty (IxFun num) where
  pretty (IxFun lmads oshp cg) =
    braces . semistack $
      [ "base:" <+> brackets (commasep $ map pretty oshp),
        "contiguous:" <+> if cg then "true" else "false",
        "LMADs:" <+> brackets (commastack $ NE.toList $ NE.map pretty lmads)
      ]

instance Substitute num => Substitute (LMAD num) where
  substituteNames substs = fmap $ substituteNames substs

instance Substitute num => Substitute (IxFun num) where
  substituteNames substs = fmap $ substituteNames substs

instance Substitute num => Rename (LMAD num) where
  rename = substituteRename

instance Substitute num => Rename (IxFun num) where
  rename = substituteRename

instance FreeIn num => FreeIn (LMAD num) where
  freeIn' = foldMap freeIn'

instance FreeIn num => FreeIn (IxFun num) where
  freeIn' = foldMap freeIn'

instance FreeIn num => FreeIn (LMADDim num) where
  freeIn' (LMADDim s n _ _) = freeIn' s <> freeIn' n

instance Functor LMAD where
  fmap = fmapDefault

instance Functor IxFun where
  fmap = fmapDefault

instance Foldable LMAD where
  foldMap = foldMapDefault

instance Foldable IxFun where
  foldMap = foldMapDefault

instance Traversable LMAD where
  traverse f (LMAD offset dims) =
    LMAD <$> f offset <*> traverse f' dims
    where
      f' (LMADDim s n p m) = LMADDim <$> f s <*> f n <*> pure p <*> pure m

-- It is important that the traversal order here is the same as in
-- mkExistential.
instance Traversable IxFun where
  traverse f (IxFun lmads oshp cg) =
    IxFun <$> traverse (traverse f) lmads <*> traverse f oshp <*> pure cg

(++@) :: [a] -> NonEmpty a -> NonEmpty a
es ++@ (ne :| nes) = case es of
  e : es' -> e :| es' ++ [ne] ++ nes
  [] -> ne :| nes

(@++@) :: NonEmpty a -> NonEmpty a -> NonEmpty a
(x :| xs) @++@ (y :| ys) = x :| xs ++ [y] ++ ys

invertMonotonicity :: Monotonicity -> Monotonicity
invertMonotonicity Inc = Dec
invertMonotonicity Dec = Inc
invertMonotonicity Unknown = Unknown

lmadPermutation :: LMAD num -> Permutation
lmadPermutation = map ldPerm . lmadDims

setLMADPermutation :: Permutation -> LMAD num -> LMAD num
setLMADPermutation perm lmad =
  lmad {lmadDims = zipWith (\dim p -> dim {ldPerm = p}) (lmadDims lmad) perm}

setLMADShape :: Shape num -> LMAD num -> LMAD num
setLMADShape shp lmad = lmad {lmadDims = zipWith (\dim s -> dim {ldShape = s}) (lmadDims lmad) shp}

-- | Substitute a name with a PrimExp in an LMAD.
substituteInLMAD ::
  Ord a =>
  M.Map a (TPrimExp t a) ->
  LMAD (TPrimExp t a) ->
  LMAD (TPrimExp t a)
substituteInLMAD tab (LMAD offset dims) =
  let offset' = sub offset
      dims' =
        map
          ( \(LMADDim s n p m) ->
              LMADDim
                (sub s)
                (sub n)
                p
                m
          )
          dims
   in LMAD offset' dims'
  where
    tab' = fmap untyped tab
    sub = TPrimExp . substituteInPrimExp tab' . untyped

-- | Substitute a name with a PrimExp in an index function.
substituteInIxFun ::
  Ord a =>
  M.Map a (TPrimExp t a) ->
  IxFun (TPrimExp t a) ->
  IxFun (TPrimExp t a)
substituteInIxFun tab (IxFun lmads oshp cg) =
  IxFun
    (NE.map (substituteInLMAD tab) lmads)
    (map (TPrimExp . substituteInPrimExp tab' . untyped) oshp)
    cg
  where
    tab' = fmap untyped tab

-- | Is this is a row-major array?
isDirect :: (Eq num, IntegralExp num) => IxFun num -> Bool
isDirect ixfun@(IxFun (LMAD offset dims :| []) oshp True) =
  let strides_expected = reverse $ scanl (*) 1 (reverse (tail oshp))
   in hasContiguousPerm ixfun
        && length oshp == length dims
        && offset == 0
        && all
          (\(LMADDim s n p _, m, d, se) -> s == se && n == d && p == m)
          (zip4 dims [0 .. length dims - 1] oshp strides_expected)
isDirect _ = False

-- | Is index function "analyzable", i.e., consists of one LMAD
hasOneLmad :: IxFun num -> Bool
hasOneLmad (IxFun (_ :| []) _ _) = True
hasOneLmad _ = False

-- | Does the index function have an ascending permutation?
hasContiguousPerm :: IxFun num -> Bool
hasContiguousPerm (IxFun (lmad :| []) _ _) =
  let perm = lmadPermutation lmad
   in perm == sort perm
hasContiguousPerm _ = False

-- | The index space of the index function.  This is the same as the
-- shape of arrays that the index function supports.
shape :: (Eq num, IntegralExp num) => IxFun num -> Shape num
shape (IxFun (lmad :| _) _ _) =
  permuteFwd (lmadPermutation lmad) $ lmadShapeBase lmad

-- | Shape of an LMAD.
lmadShape :: (Eq num, IntegralExp num) => LMAD num -> Shape num
lmadShape lmad = permuteInv (lmadPermutation lmad) $ lmadShapeBase lmad

-- | Shape of an LMAD, ignoring permutations.
lmadShapeBase :: (Eq num, IntegralExp num) => LMAD num -> Shape num
lmadShapeBase = map ldShape . lmadDims

-- | Compute the flat memory index for a complete set @inds@ of array indices
-- and a certain element size @elem_size@.
index ::
  (IntegralExp num, Eq num) =>
  IxFun num ->
  Indices num ->
  num
index = indexFromLMADs . ixfunLMADs
  where
    indexFromLMADs ::
      (IntegralExp num, Eq num) =>
      NonEmpty (LMAD num) ->
      Indices num ->
      num
    indexFromLMADs (lmad :| []) inds = indexLMAD lmad inds
    indexFromLMADs (lmad1 :| lmad2 : lmads) inds =
      let i_flat = indexLMAD lmad1 inds
          new_inds = unflattenIndex (permuteFwd (lmadPermutation lmad2) $ lmadShapeBase lmad2) i_flat
       in indexFromLMADs (lmad2 :| lmads) new_inds
    indexLMAD ::
      (IntegralExp num, Eq num) =>
      LMAD num ->
      Indices num ->
      num
    indexLMAD lmad@(LMAD off dims) inds =
      let prod =
            sum $
              zipWith
                flatOneDim
                (map ldStride dims)
                (permuteInv (lmadPermutation lmad) inds)
       in off + prod

-- | iota with offset.
iotaOffset :: IntegralExp num => num -> Shape num -> IxFun num
iotaOffset o ns = IxFun (makeRotIota Inc o ns :| []) ns True

-- | iota.
iota :: IntegralExp num => Shape num -> IxFun num
iota = iotaOffset 0

-- | Create a contiguous single-LMAD index function that is
-- existential in everything, with the provided permutation,
-- monotonicity, and contiguousness.
mkExistential :: Int -> [(Int, Monotonicity)] -> Bool -> Int -> IxFun (Ext a)
mkExistential basis_rank perm contig start =
  IxFun (NE.singleton lmad) basis contig
  where
    basis = take basis_rank $ map Ext [start + 1 + dims_rank * 2 ..]
    dims_rank = length perm
    lmad = LMAD (Ext start) $ zipWith onDim perm [0 ..]
    onDim (p, mon) i =
      LMADDim (Ext (start + 1 + i * 2)) (Ext (start + 2 + i * 2)) p mon

-- | Permute dimensions.
permute ::
  IntegralExp num =>
  IxFun num ->
  Permutation ->
  IxFun num
permute (IxFun (lmad :| lmads) oshp cg) perm_new =
  let perm_cur = lmadPermutation lmad
      perm = map (perm_cur !!) perm_new
   in IxFun (setLMADPermutation perm lmad :| lmads) oshp cg

-- | Handle the case where a slice can stay within a single LMAD.
sliceOneLMAD ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  Slice num ->
  Maybe (IxFun num)
sliceOneLMAD (IxFun (lmad@(LMAD _ ldims) :| lmads) oshp cg) (Slice is) = do
  let perm = lmadPermutation lmad
      is' = permuteInv perm is
      cg' = cg && slicePreservesContiguous lmad (Slice is')
  let lmad' = foldl sliceOne (LMAD (lmadOffset lmad) []) $ zip is' ldims
      -- need to remove the fixed dims from the permutation
      perm' =
        updatePerm perm $
          map fst $
            filter (isJust . dimFix . snd) $
              zip [0 .. length is' - 1] is'

  pure $ IxFun (setLMADPermutation perm' lmad' :| lmads) oshp cg'
  where
    updatePerm ps inds = concatMap decrease ps
      where
        decrease p =
          let f n i
                | i == p = -1
                | i > p = n
                | n /= -1 = n + 1
                | otherwise = n
              d = foldl f 0 inds
           in [p - d | d /= -1]

    -- XXX: TODO: what happens to r on a negative-stride slice; is there
    -- such a case?
    sliceOne ::
      (Eq num, IntegralExp num) =>
      LMAD num ->
      (DimIndex num, LMADDim num) ->
      LMAD num
    sliceOne (LMAD off dims) (DimFix i, LMADDim s _x _ _) =
      LMAD (off + flatOneDim s i) dims
    sliceOne (LMAD off dims) (DimSlice _ ne _, LMADDim 0 _ p _) =
      LMAD off (dims ++ [LMADDim 0 ne p Unknown])
    sliceOne (LMAD off dims) (dmind, dim@(LMADDim _ n _ _))
      | dmind == unitSlice 0 n = LMAD off (dims ++ [dim])
    sliceOne (LMAD off dims) (dmind, LMADDim s n p m)
      | dmind == DimSlice (n - 1) n (-1) =
          let off' = off + flatOneDim s (n - 1)
           in LMAD off' (dims ++ [LMADDim (s * (-1)) n p (invertMonotonicity m)])
    sliceOne (LMAD off dims) (DimSlice b ne 0, LMADDim s _ p _) =
      LMAD (off + flatOneDim s b) (dims ++ [LMADDim 0 ne p Unknown])
    sliceOne (LMAD off dims) (DimSlice bs ns ss, LMADDim s _ p m) =
      let m' = case sgn ss of
            Just 1 -> m
            Just (-1) -> invertMonotonicity m
            _ -> Unknown
       in LMAD (off + s * bs) (dims ++ [LMADDim (ss * s) ns p m'])

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
slice ixfun@(IxFun (lmad@(LMAD _ _) :| lmads) oshp cg) dim_slices
  -- Avoid identity slicing.
  | unSlice dim_slices == map (unitSlice 0) (shape ixfun) = ixfun
  | Just ixfun' <- sliceOneLMAD ixfun dim_slices = ixfun'
  | otherwise =
      case sliceOneLMAD (iota (lmadShape lmad)) dim_slices of
        Just (IxFun (lmad' :| []) _ cg') ->
          IxFun (lmad' :| lmad : lmads) oshp (cg && cg')
        _ -> error "slice: reached impossible case"

-- | Flat-slice an index function.
flatSlice ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  FlatSlice num ->
  IxFun num
flatSlice ixfun@(IxFun (LMAD offset (dim : dims) :| lmads) oshp cg) (FlatSlice new_offset is)
  | hasContiguousPerm ixfun =
      let lmad =
            LMAD
              (offset + new_offset * ldStride dim)
              (map (helper $ ldStride dim) is <> dims)
              & setLMADPermutation [0 ..]
       in IxFun (lmad :| lmads) oshp cg
  where
    helper s0 (FlatDimIndex n s) =
      let new_mon = if s0 * s == 1 then Inc else Unknown
       in LMADDim (s0 * s) n 0 new_mon
flatSlice (IxFun (lmad :| lmads) oshp cg) s@(FlatSlice new_offset _) =
  IxFun (LMAD (new_offset * base_stride) (new_dims <> tail_dims) :| lmad : lmads) oshp cg
  where
    tail_shapes = tail $ lmadShape lmad
    base_stride = product tail_shapes
    tail_strides = tail $ scanr (*) 1 tail_shapes
    tail_dims = zipWith4 LMADDim tail_strides tail_shapes [length new_shapes ..] (repeat Inc)
    new_shapes = flatSliceDims s
    new_strides = map (* base_stride) $ flatSliceStrides s
    new_dims = zipWith4 LMADDim new_strides new_shapes [0 ..] (repeat Inc)

-- | Handle the case where a reshape operation can stay inside a single LMAD.
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
-- If any of these conditions do not hold, then the reshape operation will
-- conservatively add a new LMAD to the list, leading to a representation that
-- provides less opportunities for further analysis.
reshapeOneLMAD ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  Shape num ->
  Maybe (IxFun num)
reshapeOneLMAD ixfun@(IxFun (lmad@(LMAD off dims) :| lmads) oldbase cg) newshape = do
  let perm = lmadPermutation lmad
      dims_perm = permuteFwd perm dims
      mid_dims = take (length dims) dims_perm
      mon = ixfunMonotonicity ixfun

  guard $
    -- checking conditions (2)
    all (\(LMADDim s _ _ _) -> s /= 0) mid_dims
      &&
      -- checking condition (1)
      consecutive 0 (map ldPerm mid_dims)
      &&
      -- checking condition (3)
      hasContiguousPerm ixfun
      && cg
      && (mon == Inc || mon == Dec)

  -- make new permutation
  let rsh_len = length newshape
      diff = length newshape - length dims
      iota_shape = [0 .. length newshape - 1]
      perm' =
        map
          ( \i ->
              let ind = i - diff
               in if (i >= 0) && (i < rsh_len)
                    then i -- already checked mid_dims not affected
                    else ldPerm (dims !! ind) + diff
          )
          iota_shape
      -- split the dimensions
      (support_inds, repeat_inds) =
        foldl
          (\(sup, rpt) (shpdim, ip) -> ((ip, shpdim) : sup, rpt))
          ([], [])
          $ reverse
          $ zip newshape perm'

      (sup_inds, support) = unzip $ sortBy (compare `on` fst) support_inds
      (rpt_inds, repeats) = unzip repeat_inds
      LMAD off' dims_sup = makeRotIota mon off support
      repeats' = map (\n -> LMADDim 0 n 0 Unknown) repeats
      dims' =
        map snd $
          sortBy (compare `on` fst) $
            zip sup_inds dims_sup ++ zip rpt_inds repeats'
      lmad' = LMAD off' dims'
  pure $ IxFun (setLMADPermutation perm' lmad' :| lmads) oldbase cg
  where
    consecutive _ [] = True
    consecutive i [p] = i == p
    consecutive i ps = and $ zipWith (==) ps [i, i + 1 ..]

-- | Reshape an index function.
reshape ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  Shape num ->
  IxFun num
reshape ixfun new_shape
  | Just ixfun' <- reshapeOneLMAD ixfun new_shape = ixfun'
reshape (IxFun (lmad0 :| lmad0s) oshp cg) new_shape =
  case iota new_shape of
    IxFun (lmad :| []) _ _ -> IxFun (lmad :| lmad0 : lmad0s) oshp cg
    _ -> error "reshape: reached impossible case"

-- | Coerce an index function to look like it has a new shape.
-- Dynamically the shape must be the same.
coerce ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  Shape num ->
  IxFun num
coerce (IxFun (lmad :| lmads) oshp cg) new_shape =
  IxFun (onLMAD lmad :| lmads) oshp cg
  where
    onLMAD (LMAD offset dims) = LMAD offset $ zipWith onDim dims new_shape
    onDim ld d = ld {ldShape = d}

-- | The number of dimensions in the domain of the input function.
rank ::
  IntegralExp num =>
  IxFun num ->
  Int
rank (IxFun (LMAD _ sss :| _) _ _) = length sss

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
-- XXX: TODO: handle repetitions in both lmads.
--
-- How to handle repeated dimensions in the original?
--
--   (a) Shave them off of the last lmad of original
--   (b) Compose the result from (a) with the first
--       lmad of the new base
--   (c) apply a repeat operation on the result of (b).
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
rebaseNice ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  IxFun num ->
  Maybe (IxFun num)
rebaseNice
  new_base@(IxFun (lmad_base :| lmads_base) _ cg_base)
  ixfun@(IxFun lmads shp cg) = do
    let (lmad :| lmads') = NE.reverse lmads
        dims = lmadDims lmad
        perm = lmadPermutation lmad
        perm_base = lmadPermutation lmad_base

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
        -- separately).  Accept a non-full innermost dimension.  XXX: Maybe this can
        -- be less conservative?
        && and
          ( zipWith3
              (\sn ld inner -> sn == ldShape ld || (inner && ldStride ld == 1))
              shp
              dims
              (replicate (length dims - 1) False ++ [True])
          )

    -- Compose permutations, reverse strides and adjust offset if necessary.
    let perm_base' =
          if hasContiguousPerm ixfun
            then perm_base
            else map (perm !!) perm_base
        lmad_base' = setLMADPermutation perm_base' lmad_base
        dims_base = lmadDims lmad_base'
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
        off_base = lmadOffset lmad_base' + sum offs_contrib
        lmad_base''
          | lmadOffset lmad == 0 = LMAD off_base dims_base'
          | otherwise =
              -- If the innermost dimension of the ixfun was not full (but still
              -- had a stride of 1), add its offset relative to the new base.
              setLMADShape
                (lmadShape lmad)
                ( LMAD
                    (off_base + ldStride (last dims_base) * lmadOffset lmad)
                    dims_base'
                )
        new_base' = IxFun (lmad_base'' :| lmads_base) shp cg_base
        IxFun lmads_base' _ _ = new_base'
        lmads'' = lmads' ++@ lmads_base'
    pure $ IxFun lmads'' shp (cg && cg_base)

-- | Rebase an index function on top of a new base.
rebase ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  IxFun num ->
  IxFun num
rebase new_base@(IxFun lmads_base shp_base cg_base) ixfun@(IxFun lmads shp cg)
  | Just ixfun' <- rebaseNice new_base ixfun = ixfun'
  -- In the general case just concatenate LMADs since this refers to index
  -- function composition, which is always safe.
  | otherwise =
      let (lmads_base', shp_base') =
            if base ixfun == shape new_base
              then (lmads_base, shp_base)
              else
                let IxFun lmads' shp_base'' _ = reshape new_base shp
                 in (lmads', shp_base'')
       in IxFun (lmads @++@ lmads_base') shp_base' (cg && cg_base)

-- | If the memory support of the index function is contiguous and row-major
-- (i.e., no transpositions, repetitions, rotates, etc.), then this should
-- return the offset from which the memory-support of this index function
-- starts.
linearWithOffset ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  num ->
  Maybe num
linearWithOffset ixfun@(IxFun (lmad :| []) _ cg) elem_size
  | hasContiguousPerm ixfun && cg && ixfunMonotonicity ixfun == Inc =
      Just $ lmadOffset lmad * elem_size
linearWithOffset _ _ = Nothing

-- | Similar restrictions to @linearWithOffset@ except for transpositions, which
-- are returned together with the offset.
rearrangeWithOffset ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  num ->
  Maybe (num, [(Int, num)])
rearrangeWithOffset (IxFun (lmad :| []) oshp cg) elem_size = do
  -- Note that @cg@ describes whether the index function is
  -- contiguous, *ignoring permutations*.  This function requires that
  -- functionality.
  let perm = lmadPermutation lmad
      perm_contig = [0 .. length perm - 1]
  offset <-
    linearWithOffset
      (IxFun (setLMADPermutation perm_contig lmad :| []) oshp cg)
      elem_size
  pure (offset, zip perm (permuteFwd perm (lmadShapeBase lmad)))
rearrangeWithOffset _ _ = Nothing

-- | Is this a row-major array starting at offset zero?
isLinear :: (Eq num, IntegralExp num) => IxFun num -> Bool
isLinear = (== Just 0) . flip linearWithOffset 1

permuteFwd :: Permutation -> [a] -> [a]
permuteFwd ps elems = map (elems !!) ps

permuteInv :: Permutation -> [a] -> [a]
permuteInv ps elems = map snd $ sortBy (compare `on` fst) $ zip ps elems

flatOneDim ::
  (Eq num, IntegralExp num) =>
  num ->
  num ->
  num
flatOneDim s i
  | s == 0 = 0
  | otherwise = i * s

-- | Generalised iota with user-specified offset and rotates.
makeRotIota ::
  IntegralExp num =>
  Monotonicity ->
  -- | Offset
  num ->
  -- | Shape
  [num] ->
  LMAD num
makeRotIota mon off ns
  | mon == Inc || mon == Dec =
      let rk = length ns
          ss0 = reverse $ take rk $ scanl (*) 1 $ reverse ns
          ss =
            if mon == Inc
              then ss0
              else map (* (-1)) ss0
          ps = map fromIntegral [0 .. rk - 1]
          fi = replicate rk mon
       in LMAD off $ zipWith4 LMADDim ss ns ps fi
  | otherwise = error "makeRotIota: requires Inc or Dec"

-- | Check monotonicity of an index function.
ixfunMonotonicity ::
  (Eq num, IntegralExp num) =>
  IxFun num ->
  Monotonicity
ixfunMonotonicity (IxFun (lmad :| lmads) _ _) =
  let mon0 = lmadMonotonicityRots lmad
   in if all ((== mon0) . lmadMonotonicityRots) lmads
        then mon0
        else Unknown
  where
    lmadMonotonicityRots ::
      (Eq num, IntegralExp num) =>
      LMAD num ->
      Monotonicity
    lmadMonotonicityRots (LMAD _ dims)
      | all (isMonDim Inc) dims = Inc
      | all (isMonDim Dec) dims = Dec
      | otherwise = Unknown

    isMonDim ::
      (Eq num, IntegralExp num) =>
      Monotonicity ->
      LMADDim num ->
      Bool
    isMonDim mon (LMADDim s _ _ ldmon) =
      s == 0 || mon == ldmon

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
    && (NE.length (ixfunLMADs ixf1) == NE.length (ixfunLMADs ixf2))
    && all closeEnoughLMADs (NE.zip (ixfunLMADs ixf1) (ixfunLMADs ixf2))
    -- This treats ixf1 as the "declared type" that we are matching against.
    && (contiguous ixf1 <= contiguous ixf2)
  where
    closeEnoughLMADs :: (LMAD num, LMAD num) -> Bool
    closeEnoughLMADs (lmad1, lmad2) =
      length (lmadDims lmad1) == length (lmadDims lmad2)
        && map ldPerm (lmadDims lmad1)
          == map ldPerm (lmadDims lmad2)

-- | Returns true if two 'IxFun's are equivalent.
--
-- Equivalence in this case is defined as having the same number of LMADs, with
-- each pair of LMADs matching in permutation, offsets, strides and rotations.
equivalent :: Eq num => IxFun num -> IxFun num -> Bool
equivalent ixf1 ixf2 =
  NE.length (ixfunLMADs ixf1) == NE.length (ixfunLMADs ixf2)
    && all equivalentLMADs (NE.zip (ixfunLMADs ixf1) (ixfunLMADs ixf2))
  where
    equivalentLMADs (lmad1, lmad2) =
      length (lmadDims lmad1) == length (lmadDims lmad2)
        && map ldPerm (lmadDims lmad1)
          == map ldPerm (lmadDims lmad2)
        && lmadOffset lmad1
          == lmadOffset lmad2
        && map ldStride (lmadDims lmad1)
          == map ldStride (lmadDims lmad2)

-- | Computes the maximum span of an 'LMAD'. The result is the lowest and
-- highest flat values representable by that 'LMAD'.
flatSpan :: LMAD (TPrimExp Int64 VName) -> TPrimExp Int64 VName
flatSpan (LMAD _ dims) =
  foldr
    ( \dim upper ->
        let spn = ldStride dim * (ldShape dim - 1)
         in -- If you've gotten this far, you've already lost
            spn + upper
    )
    0
    dims

-- | Conservatively flatten a list of LMAD dimensions
--
-- Since not all LMADs can actually be flattened, we try to overestimate the
-- flattened array instead. This means that any "holes" in betwen dimensions
-- will get filled out.
-- conservativeFlatten :: (IntegralExp e, Ord e, Pretty e) => LMAD e -> LMAD e
conservativeFlatten :: LMAD (TPrimExp Int64 VName) -> Maybe (LMAD (TPrimExp Int64 VName))
conservativeFlatten (LMAD offset []) =
  pure $ LMAD offset [LMADDim 1 1 0 Inc]
conservativeFlatten l@(LMAD _ [_]) =
  pure l
conservativeFlatten l@(LMAD offset dims) = do
  strd <-
    foldM
      gcd
      (ldStride $ head dims)
      $ map ldStride dims
  pure $ LMAD offset [LMADDim strd (shp + 1) 0 Unknown]
  where
    shp = flatSpan l

-- | Very conservative GCD calculation. Returns 'Nothing' if the result cannot
-- be immediately determined. Does not recurse at all.
gcd :: TPrimExp Int64 VName -> TPrimExp Int64 VName -> Maybe (TPrimExp Int64 VName)
gcd x y = gcd' (abs x) (abs y)
  where
    gcd' a b | a == b = Just a
    gcd' 1 _ = Just 1
    gcd' _ 1 = Just 1
    gcd' a 0 = Just a
    gcd' _ _ = Nothing -- gcd' b (a `Futhark.Util.IntegralExp.rem` b)

-- | Returns @True@ if the two 'LMAD's could be proven disjoint.
--
-- Uses some best-approximation heuristics to determine disjointness. For two
-- 1-dimensional arrays, we can guarantee whether or not they are disjoint, but
-- as soon as more than one dimension is involved, things get more
-- tricky. Currently, we try to 'conservativelyFlatten' any LMAD with more than
-- one dimension.
disjoint :: [(VName, PrimExp VName)] -> Names -> LMAD (TPrimExp Int64 VName) -> LMAD (TPrimExp Int64 VName) -> Bool
disjoint less_thans non_negatives (LMAD offset1 [dim1]) (LMAD offset2 [dim2]) =
  doesNotDivide (gcd (ldStride dim1) (ldStride dim2)) (offset1 - offset2)
    || AlgSimplify.lessThanish
      less_thans
      non_negatives
      (offset2 + (ldShape dim2 - 1) * ldStride dim2)
      offset1
    || AlgSimplify.lessThanish
      less_thans
      non_negatives
      (offset1 + (ldShape dim1 - 1) * ldStride dim1)
      offset2
  where
    doesNotDivide :: Maybe (TPrimExp Int64 VName) -> TPrimExp Int64 VName -> Bool
    doesNotDivide (Just x) y =
      Futhark.Util.IntegralExp.mod y x
        & untyped
        & constFoldPrimExp
        & TPrimExp
        & (.==.) (0 :: TPrimExp Int64 VName)
        & primBool
        & maybe False not
    doesNotDivide _ _ = False
disjoint less_thans non_negatives lmad1 lmad2 =
  case (conservativeFlatten lmad1, conservativeFlatten lmad2) of
    (Just lmad1', Just lmad2') -> disjoint less_thans non_negatives lmad1' lmad2'
    _ -> False

disjoint2 :: scope -> asserts -> [(VName, PrimExp VName)] -> Names -> LMAD (TPrimExp Int64 VName) -> LMAD (TPrimExp Int64 VName) -> Bool
disjoint2 _ _ less_thans non_negatives lmad1 lmad2 =
  let (offset1, interval1) = lmadToIntervals lmad1
      (offset2, interval2) = lmadToIntervals lmad2
      (neg_offset, pos_offset) =
        partition AlgSimplify.negated $
          offset1 `AlgSimplify.sub` offset2
      (interval1', interval2') =
        unzip $
          sortBy (flip AlgSimplify.compareComplexity `on` (AlgSimplify.simplify0 . untyped . stride . fst)) $
            intervalPairs interval1 interval2
   in case ( distributeOffset pos_offset interval1',
             distributeOffset (map AlgSimplify.negate neg_offset) interval2'
           ) of
        (Just interval1'', Just interval2'') ->
          isNothing
            ( selfOverlap () () less_thans (map (flip LeafExp $ IntType Int64) $ namesToList non_negatives) interval1''
            )
            && isNothing
              ( selfOverlap () () less_thans (map (flip LeafExp $ IntType Int64) $ namesToList non_negatives) interval2''
              )
            && not
              ( all
                  (uncurry (intervalOverlap less_thans non_negatives))
                  (zip interval1'' interval2'')
              )
        _ ->
          False

disjoint3 :: M.Map VName Type -> [PrimExp VName] -> [(VName, PrimExp VName)] -> [PrimExp VName] -> LMAD (TPrimExp Int64 VName) -> LMAD (TPrimExp Int64 VName) -> Bool
disjoint3 scope asserts less_thans non_negatives lmad1 lmad2 =
  let (offset1, interval1) = lmadToIntervals lmad1
      (offset2, interval2) = lmadToIntervals lmad2
      interval1' = fixPoint (mergeDims . joinDims) $ sortBy (flip AlgSimplify.compareComplexity `on` (AlgSimplify.simplify0 . untyped . stride)) interval1
      interval2' = fixPoint (mergeDims . joinDims) $ sortBy (flip AlgSimplify.compareComplexity `on` (AlgSimplify.simplify0 . untyped . stride)) interval2
      (interval1'', interval2'') =
        unzip $
          sortBy (flip AlgSimplify.compareComplexity `on` (AlgSimplify.simplify0 . untyped . stride . fst)) $
            intervalPairs interval1' interval2'
   in disjointHelper 4 interval1'' interval2'' $ offset1 `AlgSimplify.sub` offset2
  where
    disjointHelper :: Int -> [Interval] -> [Interval] -> AlgSimplify.SofP -> Bool
    disjointHelper 0 _ _ _ = False
    disjointHelper i is10 is20 offset =
      let (is1, is2) =
            unzip $
              sortBy (flip AlgSimplify.compareComplexity `on` (AlgSimplify.simplify0 . untyped . stride . fst)) $
                intervalPairs is10 is20
          (neg_offset, pos_offset) = partition AlgSimplify.negated offset
       in case ( distributeOffset pos_offset is1,
                 distributeOffset (map AlgSimplify.negate neg_offset) is2
               ) of
            (Just is1', Just is2') -> do
              let overlap1 = selfOverlap scope asserts less_thans non_negatives is1'
              let overlap2 = selfOverlap scope asserts less_thans non_negatives is2'
              case (overlap1, overlap2) of
                (Nothing, Nothing) ->
                  case namesFromList <$> mapM justLeafExp non_negatives of
                    Just non_negatives' ->
                      not $
                        all
                          (uncurry (intervalOverlap less_thans non_negatives'))
                          (zip is1 is2)
                    _ -> False
                (Just overlapping_dim, _) ->
                  let expanded_offset = AlgSimplify.simplifySofP' <$> expandOffset offset is1
                      splits = splitDim overlapping_dim is1'
                   in all (\(new_offset, new_is1) -> disjointHelper (i - 1) (joinDims new_is1) (joinDims is2') new_offset) splits
                        || maybe False (disjointHelper (i - 1) is1 is2) expanded_offset
                (_, Just overlapping_dim) ->
                  let expanded_offset = AlgSimplify.simplifySofP' <$> expandOffset offset is2
                      splits = splitDim overlapping_dim is2'
                   in all
                        ( \(new_offset, new_is2) ->
                            disjointHelper (i - 1) (joinDims is1') (joinDims new_is2) $
                              map AlgSimplify.negate new_offset
                        )
                        splits
                        || maybe False (disjointHelper (i - 1) is1 is2) expanded_offset
            _ -> False

joinDims :: [Interval] -> [Interval]
joinDims = helper []
  where
    helper acc [] = reverse acc
    helper acc [x] = reverse $ x : acc
    helper acc (x : y : rest) =
      if stride x == stride y && lowerBound x == 0 && lowerBound y == 0
        then helper acc $ x {numElements = numElements x * numElements y} : rest
        else helper (x : acc) (y : rest)

mergeDims :: [Interval] -> [Interval]
mergeDims = helper [] . reverse
  where
    helper acc [] = acc
    helper acc [x] = x : acc
    helper acc (x : y : rest) =
      if stride x * numElements x == stride y && lowerBound x == 0 && lowerBound y == 0
        then helper acc $ x {numElements = numElements x * numElements y} : rest
        else helper (x : acc) (y : rest)

splitDim :: Interval -> [Interval] -> [(AlgSimplify.SofP, [Interval])]
splitDim overlapping_dim0 is
  | [st] <- AlgSimplify.simplify0 $ untyped $ stride overlapping_dim0,
    [st1] <- AlgSimplify.simplify0 $ untyped $ stride overlapping_dim,
    [spn] <- AlgSimplify.simplify0 $ untyped $ stride overlapping_dim * numElements overlapping_dim,
    lowerBound overlapping_dim == 0,
    Just big_dim_elems <- AlgSimplify.maybeDivide spn st,
    Just small_dim_elems <- AlgSimplify.maybeDivide st st1 =
      [ ( [],
          init before
            <> [ Interval 0 (isInt64 $ AlgSimplify.prodToExp big_dim_elems) (stride overlapping_dim0),
                 Interval 0 (isInt64 $ AlgSimplify.prodToExp small_dim_elems) (stride overlapping_dim)
               ]
            <> after
        )
      ]
  | otherwise =
      let shrunk_dim = overlapping_dim {numElements = numElements overlapping_dim - 1}
          point_offset = AlgSimplify.simplify0 $ untyped $ (numElements overlapping_dim - 1 + lowerBound overlapping_dim) * stride overlapping_dim
       in [ (point_offset, before <> after),
            ([], before <> [shrunk_dim] <> after)
          ]
  where
    (before, overlapping_dim, after) =
      fromJust $
        elemIndex overlapping_dim0 is
          >>= (flip focusNth is . (+ 1))

lmadToIntervals :: LMAD (TPrimExp Int64 VName) -> (AlgSimplify.SofP, [Interval])
lmadToIntervals (LMAD offset []) = (AlgSimplify.simplify0 $ untyped offset, [Interval 0 1 1])
lmadToIntervals lmad@(LMAD offset dims0) =
  (offset', map helper $ permuteInv (lmadPermutation lmad) dims0)
  where
    offset' = AlgSimplify.simplify0 $ untyped offset

    helper :: LMADDim (TPrimExp Int64 VName) -> Interval
    helper (LMADDim strd shp _ _) = do
      Interval 0 (AlgSimplify.simplify' shp) (AlgSimplify.simplify' strd)

-- | Dynamically determine if two 'LMADDim' are equal.
--
-- True if the dynamic values of their constituents are equal.
dynamicEqualsLMADDim :: Eq num => LMADDim (TPrimExp t num) -> LMADDim (TPrimExp t num) -> TPrimExp Bool num
dynamicEqualsLMADDim dim1 dim2 =
  ldStride dim1 .==. ldStride dim2
    .&&. ldShape dim1 .==. ldShape dim2
    .&&. fromBool (ldPerm dim1 == ldPerm dim2)
    .&&. fromBool (ldMon dim1 == ldMon dim2)

-- | Dynamically determine if two 'LMAD' are equal.
--
-- True if offset and constituent 'LMADDim' are equal.
dynamicEqualsLMAD :: Eq num => LMAD (TPrimExp t num) -> LMAD (TPrimExp t num) -> TPrimExp Bool num
dynamicEqualsLMAD lmad1 lmad2 =
  lmadOffset lmad1 .==. lmadOffset lmad2
    .&&. foldr
      ((.&&.) . uncurry dynamicEqualsLMADDim)
      true
      (zip (lmadDims lmad1) (lmadDims lmad2))
