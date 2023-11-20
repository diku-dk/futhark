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
  permutationFromDimAccess :: PrimExpTable -> SegOpName -> ArrayName -> IndexExprName -> [DimAccess rep] -> Maybe Permutation

instance Layout GPU where
  permutationFromDimAccess primExpTable _segOpName (_arrayName, nest) _idxName dimAccesses =
    do
      -- Create a candidate permutation
      let perm = (map originalDimension . sortGPU) dimAccesses

      -- Dont accept indices where the last index is invariant
      let lastIdxIsInvariant = isInvariant $ last dimAccesses

      -- Check if any of the dependencies are too complex to reason about
      let deps = concatMap (map ((\(_, n, _, _, _) -> n) . snd) . S.toList . dependencies) dimAccesses
      let counters = concatMap (map ((\(_, _, _, _, t) -> isCounter t) . snd) . S.toList . dependencies) dimAccesses
      let primExps = map (`M.lookup` primExpTable) deps
      let inscrutable = any (uncurry isInscrutable) (zip primExps counters)

      -- Check if we want to manifest this array with the permutation
      if lastIdxIsInvariant || inscrutable || commonPermutationEliminators perm nest dimAccesses
        then Nothing
        else Just perm

isInscrutable :: Maybe (Maybe (PrimExp VName)) -> Bool -> Bool
isInscrutable maybeOp@(Just (Just op@(BinOpExp _ a b))) counter =
  if counter
    then -- Calculate stride and offset for loop-counters and thread-IDs
    case reduceStrideAndOffset op of
      -- Maximum allowable stride and offset
      Just (s, o) -> s > 8 || o > 128 -- TODO: Tune these values
      Nothing -> True
    else isInscrutableRec maybeOp
isInscrutable maybeOp _ = isInscrutableRec maybeOp

isInscrutableRec :: Maybe (Maybe (PrimExp VName)) -> Bool
isInscrutableRec Nothing = True
isInscrutableRec (Just Nothing) = True
isInscrutableRec (Just (Just (LeafExp _ _))) = False
isInscrutableRec (Just (Just (ValueExp _))) = False
isInscrutableRec (Just (Just op@(BinOpExp _ a b))) =
  isInscrutableRec (Just $ Just a) || isInscrutableRec (Just $ Just b)
-- TODO: Handle UnOpExp
isInscrutableRec _ = True

reduceStrideAndOffset :: PrimExp VName -> Maybe (Int, Int)
reduceStrideAndOffset (BinOpExp oper (ValueExp (IntValue v)) op)
  | LeafExp _ _ <- op = case oper of
      Add _ _ -> Just (1, valueIntegral v)
      Sub _ _ -> Just (1, -valueIntegral v)
      Mul _ _ -> Just (valueIntegral v, 0)
      _ -> Nothing
  | BinOpExp {} <- op = case reduceStrideAndOffset op of
      Nothing -> Nothing
      Just (s, o) -> case oper of
        Add _ _ -> Just (s, o + valueIntegral v)
        Sub _ _ -> Just (s, o - valueIntegral v)
        Mul _ _ -> Just (s * valueIntegral v, o * valueIntegral v)
        _ -> Nothing
reduceStrideAndOffset _ = Nothing

multicorePermutation :: PrimExpTable -> SegOpName -> ArrayName -> IndexExprName -> [DimAccess rep] -> Maybe Permutation
multicorePermutation primExpTable _segOpName (_arrayName, nest) _idxName dimAccesses = do
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

  isInvalidPerm || isInsideUndesired
  where
    isUndesired :: BodyType -> Bool
    isUndesired bodyType = case bodyType of
      SegOpName _ -> True
      LoopBodyName _ -> True
      _ -> False

-- | Given an ordering function for `DimAccess`, and an IndexTable, return
-- a PermutationTable.
-- We remove entries with no results after `permutationFromDimAccess`
permutationTableFromIndexTable :: (Layout rep) => PrimExpTable -> IndexTable rep -> PermutationTable
permutationTableFromIndexTable primExpTable = tableMapMaybe (permutationFromDimAccess primExpTable)

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

        f og (_, _, lvl, itertype, _) = Just (itertype, lvl, og)

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

        f og (_, _, lvl, itertype, _) = Just (itertype, lvl, og)

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
