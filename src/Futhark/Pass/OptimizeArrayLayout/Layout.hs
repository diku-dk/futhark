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
  permutationFromMemoryEntry :: SegOpName -> IndexExprName -> ArrayName -> MemoryEntry rep -> Maybe Permutation

-- | Given an ordering function for `DimIdxPat`, and an IndexTable, return
-- a PermutationTable.
permutationTableFromIndexTable :: (Layout rep) => IndexTable rep -> PermutationTable
permutationTableFromIndexTable indexTable =
  -- Map each MemoryEntry in the IndexTable to a permutation in a generic way
  -- that can be handled uniquely by each backend.
  M.fromList
    $ map
      ( \(segOpName, arrayMap) ->
          ( segOpName,
            M.fromList
              $ map
                ( \(arrayName, idxMap) ->
                    ( arrayName,
                      M.fromList
                        $ mapMaybe
                          ( \(idxName, memEntry) -> do
                              case permutationFromMemoryEntry segOpName idxName arrayName memEntry of
                                Nothing -> Nothing
                                Just perm -> Just (idxName, perm)
                          )
                        $ M.toList idxMap
                    )
                )
              $ M.toList arrayMap
          )
      )
    $ M.toList indexTable

instance Layout GPU where
  permutationFromMemoryEntry _segOpName _idxName (_arrayName, nest) memEntry = do
    let perm = (map originalDimension . (sortGPU . dimensions)) memEntry

    -- Don't manifest if the permutation is the identity permutation or is not
    -- a transpose.
    let isIdentity = perm `L.isPrefixOf` [0 ..]
    let isNotTranspose = isNothing (isMapTranspose perm)

    -- Don't manifest if the array is defined inside a segOp or loop body
    let nestSegOps = filter isUndesired nest
    let isInsideUndesired = not (null nestSegOps)

    if isInsideUndesired || isIdentity || isNotTranspose
      then Nothing
      else Just perm
    where
      isUndesired bodyType = case bodyType of
        SegOpName _ -> True
        LoopBodyName _ -> True
        _ -> False

multikernePermutering :: SegOpName -> IndexExprName -> ArrayName -> MemoryEntry rep -> Maybe Permutation
multikernePermutering _segOpName _idxName (_arrayName, nest) memEntry = do
  let perm = (map originalDimension . (sortMC . dimensions)) memEntry

  -- Don't manifest if the permutation is the identity permutation or is not
  -- a transpose.
  let isIdentity = perm `L.isPrefixOf` [0 ..]
  let isNotTranspose = isNothing (isMapTranspose perm)

  -- Don't manifest if the array is defined inside a segOp or loop body
  let nestSegOps = filter isUndesired nest
  let isInsideUndesired = not (null nestSegOps)

  if isInsideUndesired || isIdentity || isNotTranspose
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
