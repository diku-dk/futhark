-- | This module contains a representation of linear-memory accessor
-- descriptors (LMAD); see work by Zhu, Hoeflinger and David.
--
-- This module is designed to be used as a qualified import, as the
-- exported names are quite generic.
module Futhark.IR.Mem.LMAD
  ( -- * Core
    Shape,
    Indices,
    LMAD (..),
    LMADDim (..),
    Permutation,
    index,
    slice,
    flatSlice,
    reshape,
    coerce,
    permute,
    shape,
    substitute,
    iota,
    equivalent,
    range,

    -- * Exotic
    expand,
    isDirect,
    disjoint,
    disjoint2,
    disjoint3,
    dynamicEqualsLMAD,
    mkExistential,
    closeEnough,
    existentialize,
    existentialized,
  )
where

import Control.Category
import Control.Monad
import Control.Monad.State
import Data.Function (on, (&))
import Data.List (elemIndex, partition, sortBy)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, isNothing)
import Data.Traversable
import Futhark.Analysis.AlgSimplify qualified as AlgSimplify
import Futhark.Analysis.PrimExp
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Mem.Interval
import Futhark.IR.Prop
import Futhark.IR.Syntax
  ( DimIndex (..),
    Ext (..),
    FlatDimIndex (..),
    FlatSlice (..),
    Slice (..),
    Type,
    unitSlice,
  )
import Futhark.IR.Syntax.Core (VName (..))
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util
import Futhark.Util.IntegralExp
import Futhark.Util.Pretty
import Prelude hiding (gcd, id, mod, (.))

-- | The shape of an index function.
type Shape num = [num]

-- | Indices passed to an LMAD.  Must always match the rank of the LMAD.
type Indices num = [num]

-- | A complete permutation.
type Permutation = [Int]

-- | A single dimension in an 'LMAD'.
data LMADDim num = LMADDim
  { ldStride :: num,
    ldShape :: num
  }
  deriving (Show, Eq, Ord)

-- | LMAD's representation consists of a general offset and for each
-- dimension a stride, number of elements (or shape), and
-- permutation. Note that the permutation is not strictly necessary in
-- that the permutation can be performed directly on LMAD dimensions,
-- but then it is difficult to extract the permutation back from an
-- LMAD.
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
-- Finally, the list of LMADs is kept in an @LMAD@ together with the shape of
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
  { offset :: num,
    dims :: [LMADDim num]
  }
  deriving (Show, Eq, Ord)

instance (Pretty num) => Pretty (LMAD num) where
  pretty (LMAD offset dims) =
    braces . semistack $
      [ "offset:" <+> group (pretty offset),
        "strides:" <+> p ldStride,
        "shape:" <+> p ldShape
      ]
    where
      p f = group $ brackets $ align $ commasep $ map (pretty . f) dims

instance (Substitute num) => Substitute (LMAD num) where
  substituteNames substs = fmap $ substituteNames substs

instance (Substitute num) => Rename (LMAD num) where
  rename = substituteRename

instance (FreeIn num) => FreeIn (LMAD num) where
  freeIn' = foldMap freeIn'

instance (FreeIn num) => FreeIn (LMADDim num) where
  freeIn' (LMADDim s n) = freeIn' s <> freeIn' n

instance Functor LMAD where
  fmap = fmapDefault

instance Foldable LMAD where
  foldMap = foldMapDefault

instance Traversable LMAD where
  traverse f (LMAD offset dims) =
    LMAD <$> f offset <*> traverse f' dims
    where
      f' (LMADDim s n) = LMADDim <$> f s <*> f n

flatOneDim ::
  (Eq num, IntegralExp num) =>
  num ->
  num ->
  num
flatOneDim s i
  | s == 0 = 0
  | otherwise = i * s

index :: (IntegralExp num, Eq num) => LMAD num -> Indices num -> num
index (LMAD off dims) inds =
  off + sum prods
  where
    prods = zipWith flatOneDim (map ldStride dims) inds

-- | Handle the case where a slice can stay within a single LMAD.
slice ::
  (Eq num, IntegralExp num) =>
  LMAD num ->
  Slice num ->
  LMAD num
slice lmad@(LMAD _ ldims) (Slice is) =
  foldl sliceOne (LMAD (offset lmad) []) $ zip is ldims
  where
    sliceOne ::
      (Eq num, IntegralExp num) =>
      LMAD num ->
      (DimIndex num, LMADDim num) ->
      LMAD num
    sliceOne (LMAD off dims) (DimFix i, LMADDim s _x) =
      LMAD (off + flatOneDim s i) dims
    sliceOne (LMAD off dims) (DimSlice _ ne _, LMADDim 0 _) =
      LMAD off (dims ++ [LMADDim 0 ne])
    sliceOne (LMAD off dims) (dmind, dim@(LMADDim _ n))
      | dmind == unitSlice 0 n = LMAD off (dims ++ [dim])
    sliceOne (LMAD off dims) (dmind, LMADDim s n)
      | dmind == DimSlice (n - 1) n (-1) =
          let off' = off + flatOneDim s (n - 1)
           in LMAD off' (dims ++ [LMADDim (s * (-1)) n])
    sliceOne (LMAD off dims) (DimSlice b ne 0, LMADDim s _) =
      LMAD (off + flatOneDim s b) (dims ++ [LMADDim 0 ne])
    sliceOne (LMAD off dims) (DimSlice bs ns ss, LMADDim s _) =
      LMAD (off + s * bs) (dims ++ [LMADDim (ss * s) ns])

-- | Flat-slice an LMAD.
flatSlice ::
  (IntegralExp num) =>
  LMAD num ->
  FlatSlice num ->
  LMAD num
flatSlice (LMAD offset (dim : dims)) (FlatSlice new_offset is) =
  LMAD
    (offset + new_offset * ldStride dim)
    (map (helper $ ldStride dim) is <> dims)
  where
    helper s0 (FlatDimIndex n s) = LMADDim (s0 * s) n
flatSlice (LMAD offset []) _ = LMAD offset []

-- | Reshape an LMAD.
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
  (Eq num, IntegralExp num) => LMAD num -> Shape num -> Maybe (LMAD num)
--
-- First a special case for when we are merely injecting unit
-- dimensions into an LMAD.
reshape (LMAD off dims) newshape
  | Just dims' <- addingVacuous newshape dims =
      Just $ LMAD off dims'
  where
    addingVacuous (dnew : dnews) (dold : dolds)
      | dnew == ldShape dold =
          (dold :) <$> addingVacuous dnews dolds
    addingVacuous (1 : dnews) dolds =
      (LMADDim 0 1 :) <$> addingVacuous dnews dolds
    addingVacuous [] [] = Just []
    addingVacuous _ _ = Nothing

-- Then the general case.
reshape lmad@(LMAD off dims) newshape = do
  let base_stride = ldStride (last dims)
      no_zero_stride = all (\ld -> ldStride ld /= 0) dims
      strides_as_expected = lmad == iotaStrided off base_stride (shape lmad)

  guard $ no_zero_stride && strides_as_expected

  Just $ iotaStrided off base_stride newshape
{-# NOINLINE reshape #-}

-- | Coerce an index function to look like it has a new shape.
-- Dynamically the shape must be the same.
coerce :: LMAD num -> Shape num -> LMAD num
coerce (LMAD offset dims) new_shape =
  LMAD offset $ zipWith onDim dims new_shape
  where
    onDim ld d = ld {ldShape = d}
{-# NOINLINE coerce #-}

-- | Substitute a name with a PrimExp in an LMAD.
substitute ::
  (Ord a) =>
  M.Map a (TPrimExp t a) ->
  LMAD (TPrimExp t a) ->
  LMAD (TPrimExp t a)
substitute tab (LMAD offset dims) =
  LMAD (sub offset) $ map (\(LMADDim s n) -> LMADDim (sub s) (sub n)) dims
  where
    tab' = fmap untyped tab
    sub = TPrimExp . substituteInPrimExp tab' . untyped

-- | Shape of an LMAD.
shape :: LMAD num -> Shape num
shape = map ldShape . dims

iotaStrided ::
  (IntegralExp num) =>
  -- | Offset
  num ->
  -- | Base Stride
  num ->
  -- | Shape
  [num] ->
  LMAD num
iotaStrided off s ns =
  let ss = tail $ reverse $ scanl (*) s $ reverse ns
   in LMAD off $ zipWith LMADDim ss ns

-- | Generalised iota with user-specified offset.
iota ::
  (IntegralExp num) =>
  -- | Offset
  num ->
  -- | Shape
  [num] ->
  LMAD num
iota off = iotaStrided off 1
{-# NOINLINE iota #-}

-- | Create an LMAD that is existential in everything except shape.
mkExistential :: Shape (Ext a) -> Int -> LMAD (Ext a)
mkExistential shp start = LMAD (Ext start) $ zipWith onDim shp [0 .. r - 1]
  where
    r = length shp
    onDim d i = LMADDim {ldStride = Ext (start + 1 + i), ldShape = d}

-- | Permute dimensions.
permute :: LMAD num -> Permutation -> LMAD num
permute lmad perm =
  lmad {dims = rearrangeShape perm $ dims lmad}

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
  pure $ LMAD offset [LMADDim 1 1]
conservativeFlatten l@(LMAD _ [_]) =
  pure l
conservativeFlatten l@(LMAD offset dims) = do
  strd <-
    foldM
      gcd
      (ldStride $ head dims)
      $ map ldStride dims
  pure $ LMAD offset [LMADDim strd (shp + 1)]
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
lmadToIntervals (LMAD offset dims0) =
  (offset', map helper dims0)
  where
    offset' = AlgSimplify.simplify0 $ untyped offset

    helper :: LMADDim (TPrimExp Int64 VName) -> Interval
    helper (LMADDim strd shp) = do
      Interval 0 (AlgSimplify.simplify' shp) (AlgSimplify.simplify' strd)

-- | Dynamically determine if two 'LMADDim' are equal.
--
-- True if the dynamic values of their constituents are equal.
dynamicEqualsLMADDim :: (Eq num) => LMADDim (TPrimExp t num) -> LMADDim (TPrimExp t num) -> TPrimExp Bool num
dynamicEqualsLMADDim dim1 dim2 =
  ldStride dim1 .==. ldStride dim2 .&&. ldShape dim1 .==. ldShape dim2

-- | Dynamically determine if two 'LMAD' are equal.
--
-- True if offset and constituent 'LMADDim' are equal.
dynamicEqualsLMAD :: (Eq num) => LMAD (TPrimExp t num) -> LMAD (TPrimExp t num) -> TPrimExp Bool num
dynamicEqualsLMAD lmad1 lmad2 =
  offset lmad1 .==. offset lmad2
    .&&. foldr
      ((.&&.) . uncurry dynamicEqualsLMADDim)
      true
      (zip (dims lmad1) (dims lmad2))
{-# NOINLINE dynamicEqualsLMAD #-}

-- | Returns true if two 'LMAD's are equivalent.
--
-- Equivalence in this case is matching in offsets and strides.
equivalent :: (Eq num) => LMAD num -> LMAD num -> Bool
equivalent lmad1 lmad2 =
  offset lmad1 == offset lmad2
    && map ldStride (dims lmad1) == map ldStride (dims lmad2)
{-# NOINLINE equivalent #-}

-- | Is this is a row-major array with zero offset?
isDirect :: (Eq num, IntegralExp num) => LMAD num -> Bool
isDirect lmad = lmad == iota 0 (map ldShape $ dims lmad)
{-# NOINLINE isDirect #-}

-- | The largest possible linear address reachable by this LMAD, not
-- counting the offset. If you add one to this number (and multiply it
-- with the element size), you get the amount of bytes you need to
-- allocate for an array with this LMAD (assuming zero offset).
range :: (Pretty num) => LMAD (TPrimExp Int64 num) -> TPrimExp Int64 num
range lmad =
  -- The idea is that the largest possible offset must be the sum of
  -- the maximum offsets reachable in each dimension, which must be at
  -- either the minimum or maximum index.
  sum (map dimRange $ dims lmad)
  where
    dimRange LMADDim {ldStride, ldShape} =
      0 `sMax64` ((0 `sMax64` (ldShape - 1)) * ldStride)
{-# NOINLINE range #-}

-- | When comparing LMADs as part of the type check in GPUMem, we
-- may run into problems caused by the simplifier. As index functions
-- can be generalized over if-then-else expressions, the simplifier
-- might hoist some of the code from inside the if-then-else
-- (computing the offset of an array, for instance), but now the type
-- checker cannot verify that the generalized index function is valid,
-- because some of the existentials are computed somewhere else. To
-- Work around this, we've had to relax the KernelsMem type-checker a
-- bit, specifically, we've introduced this function to verify whether
-- two index functions are "close enough" that we can assume that they
-- match. We use this instead of `lmad1 == lmad2` and hope that it's
-- good enough.
closeEnough :: LMAD num -> LMAD num -> Bool
closeEnough lmad1 lmad2 =
  length (dims lmad1) == length (dims lmad2)
{-# NOINLINE closeEnough #-}

-- | Turn all the leaves of the LMAD into 'Ext's, except for
--  the shape, which where the leaves are simply made 'Free'.
existentialize ::
  Int ->
  LMAD (TPrimExp Int64 a) ->
  LMAD (TPrimExp Int64 (Ext a))
existentialize start lmad = evalState lmad' start
  where
    mkExt = do
      i <- get
      put $ i + 1
      pure $ TPrimExp $ LeafExp (Ext i) int64
    lmad' = LMAD <$> mkExt <*> mapM onDim (dims lmad)
    onDim ld = LMADDim <$> mkExt <*> pure (fmap Free (ldShape ld))

-- | Retrieve those elements that 'existentialize' changes. That is,
-- everything except the shape (and in the same order as
-- 'existentialise' existentialises them).
existentialized :: LMAD a -> [a]
existentialized (LMAD offset dims) =
  offset : concatMap onDim dims
  where
    onDim (LMADDim ldstride _) = [ldstride]

-- | Conceptually expand LMAD to be a particular slice of
-- another by adjusting the offset and strides.  Used for memory
-- expansion.
expand ::
  (IntegralExp num) => num -> num -> LMAD num -> LMAD num
expand o p lmad =
  LMAD (o + p * offset lmad) (map onDim (dims lmad))
  where
    onDim ld = ld {ldStride = p * ldStride ld}
