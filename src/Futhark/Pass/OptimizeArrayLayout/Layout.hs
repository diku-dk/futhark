module Futhark.Pass.OptimizeArrayLayout.Layout (permutationTableFromIndexTable, Layout, Permutation, PermutationTable) where

import Data.IntMap.Strict qualified as S
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.AccessPattern
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

class (Analyze rep) => Layout rep where
  -- | Return a coalescing permutation that will be used to create a manifest of the array.
  -- Returns Nothing if the array is already in the optimal layout
  permutationFromMemoryEntry :: SegOpName -> ArrayName -> IndexExprName -> MemoryEntry rep -> Maybe Permutation

commonPermutationEliminators :: [Int] -> Bool
commonPermutationEliminators =
  anyOf
    [ -- Don't manifest if the permutation is the identity permutation
      (`L.isPrefixOf` [0 ..]),
      -- or is not a transpose.
      isNothing . isMapTranspose
    ]
  where
    -- Why is this not in the prelude, there's probably a monad for this.
    anyOf :: forall a t. Foldable t => t (a -> Bool) -> a -> Bool
    anyOf preds input =
      any (\f -> f input) preds

-- | Given an ordering function for `DimIdxPat`, and an IndexTable, return
-- a PermutationTable.
permutationTableFromIndexTable :: (Layout rep) => IndexTable rep -> PermutationTable
permutationTableFromIndexTable =
  -- Map each MemoryEntry in the IndexTable to a permutation in a generic way
  -- that can be handled uniquely by each backend.
  -- We remove entries with no results after `permutationFromMemoryEntry`
  M.mapMaybeWithKey $
    \segOpName -> mapToMaybe $ mapToMaybe . permutationFromMemoryEntry segOpName
  where
    maybeMap :: M.Map k a -> Maybe (M.Map k a)
    maybeMap val = if null val then Nothing else Just val

    mapToMaybe f = maybeMap . M.mapMaybeWithKey f

instance Layout GPU where
  permutationFromMemoryEntry _segOpName (_arrayName, nest) _idxName memEntry = do
    let perm = (map originalDimension . sortGPU) memEntry

    -- Don't manifest if the array is defined inside a segOp or loop body
    let nestSegOps = filter isUndesired nest
    let isInsideUndesired = not (null nestSegOps)

    if isInsideUndesired || commonPermutationEliminators perm
      then Nothing
      else Just perm
    where
      isUndesired bodyType = case bodyType of
        SegOpName _ -> True
        LoopBodyName _ -> True
        _ -> False

multikernePermutering :: SegOpName -> ArrayName -> IndexExprName -> MemoryEntry rep -> Maybe Permutation
multikernePermutering _segOpName (_arrayName, nest) _idxName memEntry = do
  let perm = (map originalDimension . sortMC) memEntry

  -- Don't manifest if the array is defined inside a segOp or loop body
  let nestSegOps = filter isUndesired nest
  let isInsideUndesired = not (null nestSegOps)

  if isInsideUndesired || commonPermutationEliminators perm
    then Nothing
    else Just perm
  where
    isUndesired bodyType = case bodyType of
      SegOpName _ -> True
      LoopBodyName _ -> True
      _ -> False

instance Layout MC where
  permutationFromMemoryEntry = multikernePermutering

sortGPU :: [DimIdxPat rep] -> [DimIdxPat rep]
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

        f og (_, lvl, itertype) = Just (itertype, lvl, og)

sortMC :: [DimIdxPat rep] -> [DimIdxPat rep]
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

        f og (_, lvl, itertype) = Just (itertype, lvl, og)

instance Layout MCMem where
  permutationFromMemoryEntry = multikernePermutering

instance Layout GPUMem where
  permutationFromMemoryEntry = error $ notImplementedYet "GPUMem"

instance Layout Seq where
  permutationFromMemoryEntry = error $ notImplementedYet "Seq"

instance Layout SeqMem where
  permutationFromMemoryEntry = error $ notImplementedYet "SeqMem"

instance Layout SOACS where
  permutationFromMemoryEntry = error $ notImplementedYet "SOACS"
