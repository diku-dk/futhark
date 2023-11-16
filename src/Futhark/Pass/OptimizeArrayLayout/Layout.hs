module Futhark.Pass.OptimizeArrayLayout.Layout (permutationTableFromIndexTable, Layout, Permutation, commonPermutationEliminators, PermutationTable) where

import Data.IntMap.Strict qualified as S
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Debug.Pretty.Simple
import Futhark.Analysis.AccessPattern
import Futhark.Analysis.AnalyzePrimExp
import Futhark.IR.Aliases
import Futhark.IR.GPU
import Futhark.IR.GPUMem
import Futhark.IR.MC
import Futhark.IR.MCMem
import Futhark.IR.SOACS
import Futhark.IR.Seq
import Futhark.IR.SeqMem

type Permutation = [Int]

type PermutationTable = M.Map SegOpName (M.Map ArrayName (M.Map IndexExprName Permutation))

class (PrimExpAnalysis rep) => Layout rep where
  -- | Return a coalescing permutation that will be used to create a manifest of the array.
  -- Returns Nothing if the array is already in the optimal layout or if the array access
  -- is too complex to confidently determine the optimal layout.
  -- Map each list of `DimAccess` in the IndexTable to a permutation in a generic way
  -- that can be handled uniquely by each backend.
  permutationFromDimAccess :: SegOpName -> ArrayName -> IndexExprName -> [DimAccess rep] -> Maybe Permutation

instance Layout GPU where
  permutationFromDimAccess _segOpName (_arrayName, nest) _idxName dimAccesses =
    -- Dont accept indices where the last index is constant
    if null . dependencies $ last dimAccesses
      then Nothing
      else -- Create a candidate permutation

        let perm = (map originalDimension . sortGPU) dimAccesses
         in -- Check if we want to manifest this array with the permutation
            if commonPermutationEliminators perm nest dimAccesses
              then Nothing
              else Just perm

multicorePermutation :: SegOpName -> ArrayName -> IndexExprName -> [DimAccess rep] -> Maybe Permutation
multicorePermutation _segOpName (_arrayName, nest) _idxName dimAccesses = do
  -- Create a candidate permutation
  let perm = (map originalDimension . sortMC) dimAccesses

  -- Check if we want to manifest this array with the permutation
  if commonPermutationEliminators perm nest dimAccesses
    then Nothing
    else Just perm

instance Layout MC where
  permutationFromDimAccess = multicorePermutation

instance Layout MCMem where
  permutationFromDimAccess = multicorePermutation

instance Layout GPUMem where
  permutationFromDimAccess = error $ notImplementedYet "GPUMem"

instance Layout Seq where
  permutationFromDimAccess = error $ notImplementedYet "Seq"

instance Layout SeqMem where
  permutationFromDimAccess = error $ notImplementedYet "SeqMem"

instance Layout SOACS where
  permutationFromDimAccess = error $ notImplementedYet "SOACS"

-- | Reasons common to all backends to not manifest an array.
commonPermutationEliminators :: [Int] -> [BodyType] -> [DimAccess rep] -> Bool
commonPermutationEliminators perm nest dimAccesses = do
  -- Don't manifest if the permutation is the permutation is invalid
  let isInvalidPerm =
        -- Don't manifest if the permutation is the identity permutation
        perm `L.isPrefixOf` [0 ..]
          -- or is not a transpose.
          || (isNothing . isMapTranspose) perm
          -- or is not a permutation.
          || not (L.sort perm `L.isPrefixOf` [0 ..])
          -- or if the last idx remains last
          || (last perm == originalDimension (last dimAccesses))

  -- Don't manifest if the array is defined inside a segOp or loop body
  let isInsideUndesired = any isUndesired nest
  -- Don't manifest if the array is indexed by something weird

  isInvalidPerm || isInsideUndesired
  where
    -- Why is this not in the prelude, there's probably a monad for this.
    anyOf :: forall a t. (Foldable t) => t (a -> Bool) -> a -> Bool
    anyOf preds input = any (\f -> f input) preds

    isUndesired :: BodyType -> Bool
    isUndesired bodyType = case bodyType of
      SegOpName _ -> True
      LoopBodyName _ -> True
      _ -> False

-- | Given an ordering function for `DimAccess`, and an IndexTable, return
-- a PermutationTable.
-- We remove entries with no results after `permutationFromDimAccess`
permutationTableFromIndexTable :: (Layout rep) => IndexTable rep -> PermutationTable
permutationTableFromIndexTable = tableMapMaybe permutationFromDimAccess

sortGPU :: [DimAccess rep] -> [DimAccess rep]
sortGPU =
  L.sortBy dimdexGPUcmp
  where
    dimdexGPUcmp a b = do
      let depsA = dependencies a
      let depsB = dependencies b
      let deps1' = map (f (originalDimension a) . snd) $ S.toList depsA
      let deps2' = map (f (originalDimension b) . snd) $ S.toList depsB
      let aggr1 = foldl maxIdxPat Nothing deps1'
      let aggr2 = foldl maxIdxPat Nothing deps2'
      cmpIdxPat aggr1 aggr2
      where
        cmpIdxPat ::
          Maybe (IterationType rep, Int, Int) ->
          Maybe (IterationType rep, Int, Int) ->
          Ordering
        cmpIdxPat Nothing Nothing = EQ
        cmpIdxPat (Just _) Nothing = GT
        cmpIdxPat Nothing (Just _) = LT
        cmpIdxPat
          (Just (iterL, lvlL, originalLevelL))
          (Just (iterR, lvlR, originalLevelR)) =
            case (iterL, iterR) of
              (Parallel, Sequential) -> GT
              (Sequential, Parallel) -> LT
              _ ->
                (lvlL, originalLevelL) `compare` (lvlR, originalLevelR)

        maxIdxPat ::
          Maybe (IterationType rep, Int, Int) ->
          Maybe (IterationType rep, Int, Int) ->
          Maybe (IterationType rep, Int, Int)

        maxIdxPat lhs rhs =
          case cmpIdxPat lhs rhs of
            LT -> rhs
            _ -> lhs

        f og (_, lvl, itertype, _) = Just (itertype, lvl, og)

sortMC :: [DimAccess rep] -> [DimAccess rep]
sortMC =
  L.sortBy dimdexGPUcmp
  where
    dimdexGPUcmp a b = do
      let depsA = dependencies a
      let depsB = dependencies b
      let deps1' = map (f (originalDimension a) . snd) $ S.toList depsA
      let deps2' = map (f (originalDimension b) . snd) $ S.toList depsB
      let aggr1 = foldl maxIdxPat Nothing deps1'
      let aggr2 = foldl maxIdxPat Nothing deps2'
      cmpIdxPat aggr1 aggr2
      where
        cmpIdxPat ::
          Maybe (IterationType rep, Int, Int) ->
          Maybe (IterationType rep, Int, Int) ->
          Ordering
        cmpIdxPat Nothing Nothing = EQ
        cmpIdxPat (Just _) Nothing = GT
        cmpIdxPat Nothing (Just _) = LT
        cmpIdxPat
          (Just (iterL, lvlL, originalLevelL))
          (Just (iterR, lvlR, originalLevelR)) =
            case (iterL, iterR) of
              (Parallel, Sequential) -> LT
              (Sequential, Parallel) -> GT
              _ ->
                (lvlL, originalLevelL) `compare` (lvlR, originalLevelR)

        maxIdxPat ::
          Maybe (IterationType rep, Int, Int) ->
          Maybe (IterationType rep, Int, Int) ->
          Maybe (IterationType rep, Int, Int)

        maxIdxPat lhs rhs =
          case cmpIdxPat lhs rhs of
            LT -> rhs
            _ -> lhs

        f og (_, lvl, itertype, _) = Just (itertype, lvl, og)

-- | like mapMaybe, but works on nested maps. Eliminates "dangling" maps / rows
-- with missing (Nothing) values.
tableMapMaybe :: (k0 -> k1 -> k2 -> a -> Maybe b) -> M.Map k0 (M.Map k1 (M.Map k2 a)) -> M.Map k0 (M.Map k1 (M.Map k2 b))
tableMapMaybe f =
  M.mapMaybeWithKey $
    \key0 -> mapToMaybe $ mapToMaybe . f key0
  where
    maybeMap :: M.Map k a -> Maybe (M.Map k a)
    maybeMap val = if null val then Nothing else Just val

    mapToMaybe g = maybeMap . M.mapMaybeWithKey g
