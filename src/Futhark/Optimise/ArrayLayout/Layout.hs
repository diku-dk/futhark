module Futhark.Optimise.ArrayLayout.Layout
  ( layoutTableFromIndexTable,
    Layout,
    Permutation,
    LayoutTable,

    -- * Exposed for testing
    commonPermutationEliminators,
  )
where

import Control.Monad (join)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.AccessPattern
import Futhark.Analysis.PrimExp.Table (PrimExpTable)
import Futhark.IR.Aliases
import Futhark.IR.GPU
import Futhark.IR.MC
import Futhark.IR.MCMem
import Futhark.Util (mininum)

type Permutation = [Int]

type LayoutTable =
  M.Map
    SegOpName
    ( M.Map
        ArrayName
        (M.Map IndexExprName Permutation)
    )

class Layout rep where
  -- | Produce a coalescing permutation that will be used to create a
  -- manifest of the array. Returns Nothing if the array is already in
  -- the optimal layout or if the array access is too complex to
  -- confidently determine the optimal layout. Map each list of
  -- 'DimAccess' in the IndexTable to a permutation in a generic way
  -- that can be handled uniquely by each backend.
  permutationFromDimAccess ::
    PrimExpTable ->
    SegOpName ->
    ArrayName ->
    IndexExprName ->
    [DimAccess rep] ->
    Maybe Permutation

isInscrutableExp :: PrimExp VName -> Bool
isInscrutableExp (LeafExp _ _) = False
isInscrutableExp (ValueExp _) = False
isInscrutableExp (BinOpExp _ a b) =
  isInscrutableExp a || isInscrutableExp b
isInscrutableExp (UnOpExp _ a) =
  isInscrutableExp a
isInscrutableExp _ = True

isInscrutable :: PrimExp VName -> Bool -> Bool
isInscrutable op@(BinOpExp {}) counter =
  if counter
    then -- Calculate stride and offset for loop-counters and thread-IDs
      case reduceStrideAndOffset op of
        -- Maximum allowable stride, might need tuning.
        Just (s, _) -> s > 8
        Nothing -> isInscrutableExp op
    else isInscrutableExp op
isInscrutable op _ = isInscrutableExp op

reduceStrideAndOffset :: PrimExp l -> Maybe (Int, Int)
reduceStrideAndOffset (LeafExp _ _) = Just (1, 0)
reduceStrideAndOffset (BinOpExp oper a b) = case (a, b) of
  (ValueExp (IntValue v), _) -> reduce v b
  (_, ValueExp (IntValue v)) -> reduce v a
  _ -> Nothing
  where
    reduce v (LeafExp _ _) =
      case oper of
        Add _ _ -> Just (1, valueIntegral v)
        Sub _ _ -> Just (1, -valueIntegral v)
        Mul _ _ -> Just (valueIntegral v, 0)
        _ -> Nothing
    reduce v op@(BinOpExp {}) =
      case reduceStrideAndOffset op of
        Nothing -> Nothing
        Just (s, o) -> case oper of
          Add _ _ -> Just (s, o + valueIntegral v)
          Sub _ _ -> Just (s, o - valueIntegral v)
          Mul _ _ -> Just (s * valueIntegral v, o * valueIntegral v)
          _ -> Nothing
    reduce _ (UnOpExp (Neg Bool) _) = Nothing
    reduce _ (UnOpExp (Complement _) _) = Nothing
    reduce _ (UnOpExp (Abs _) _) = Nothing
    reduce _ (UnOpExp _ sub_op) = reduceStrideAndOffset sub_op
    reduce _ (ConvOpExp _ sub_op) = reduceStrideAndOffset sub_op
    reduce _ _ = Nothing
reduceStrideAndOffset _ = Nothing

-- | Reasons common to all backends to not manifest an array.
commonPermutationEliminators :: [Int] -> [BodyType] -> Bool
commonPermutationEliminators perm nest = do
  -- Don't manifest if the permutation is the permutation is invalid
  let is_invalid_perm = not (L.sort perm `L.isPrefixOf` [0 ..])
      -- Don't manifest if the permutation is the identity permutation
      is_identity = perm `L.isPrefixOf` [0 ..]
      -- or is not a transpose.
      inefficient_transpose = isNothing $ isMapTranspose perm
      -- or if the last idx remains last
      static_last_idx = last perm == length perm - 1
      -- Don't manifest if the array is defined inside a segOp
      inside_undesired = any undesired nest

  is_invalid_perm
    || is_identity
    || inefficient_transpose
    || static_last_idx
    || inside_undesired
  where
    undesired :: BodyType -> Bool
    undesired bodyType = case bodyType of
      SegOpName _ -> True
      _ -> False

sortMC :: [(Int, DimAccess rep)] -> [(Int, DimAccess rep)]
sortMC =
  L.sortBy dimdexMCcmp
  where
    dimdexMCcmp (ia, a) (ib, b) = do
      let aggr1 =
            foldl max' Nothing $ map (f ia . snd) $ M.toList $ dependencies a
          aggr2 =
            foldl max' Nothing $ map (f ib . snd) $ M.toList $ dependencies b
      cmpIdxPat aggr1 aggr2
      where
        cmpIdxPat Nothing Nothing = EQ
        cmpIdxPat (Just _) Nothing = GT
        cmpIdxPat Nothing (Just _) = LT
        cmpIdxPat
          (Just (iterL, lvlL, original_lvl_L))
          (Just (iterR, lvlR, original_lvl_R)) =
            case (iterL, iterR) of
              (ThreadID, ThreadID) -> (lvlL, original_lvl_L) `compare` (lvlR, original_lvl_R)
              (ThreadID, _) -> LT
              (_, ThreadID) -> GT
              _ -> (lvlL, original_lvl_L) `compare` (lvlR, original_lvl_R)

        max' lhs rhs =
          case cmpIdxPat lhs rhs of
            LT -> rhs
            _ -> lhs

        f og (Dependency lvl varType) = Just (varType, lvl, og)

multicorePermutation :: PrimExpTable -> SegOpName -> ArrayName -> IndexExprName -> [DimAccess rep] -> Maybe Permutation
multicorePermutation primExpTable _segOpName (_arr_name, nest, arr_layout) _idx_name dimAccesses = do
  -- Dont accept indices where the last index is invariant
  let lastIdxIsInvariant = isInvariant $ last dimAccesses

  -- Check if any of the dependencies are too complex to reason about
  let dimAccesses' = filter (isJust . originalVar) dimAccesses
      deps = mapMaybe originalVar dimAccesses'
      counters = concatMap (map (isCounter . varType . snd) . M.toList . dependencies) dimAccesses'
      primExps = mapM (join . (`M.lookup` primExpTable)) deps
      inscrutable = maybe True (any (uncurry isInscrutable) . flip zip counters) primExps

  -- Create a candidate permutation
  let perm = map fst $ sortMC (zip arr_layout dimAccesses)

  -- Check if we want to manifest this array with the permutation
  if lastIdxIsInvariant || inscrutable || commonPermutationEliminators perm nest
    then Nothing
    else Just perm

instance Layout MC where
  permutationFromDimAccess = multicorePermutation

sortGPU :: [(Int, DimAccess rep)] -> [(Int, DimAccess rep)]
sortGPU =
  L.sortBy dimdexGPUcmp
  where
    dimdexGPUcmp (ia, a) (ib, b) = do
      let aggr1 =
            foldl max' Nothing $ map (f ia . snd) $ M.toList $ dependencies a
          aggr2 =
            foldl max' Nothing $ map (f ib . snd) $ M.toList $ dependencies b
      cmpIdxPat aggr1 aggr2
      where
        cmpIdxPat Nothing Nothing = EQ
        cmpIdxPat (Just _) Nothing = GT
        cmpIdxPat Nothing (Just _) = LT
        cmpIdxPat
          (Just (iterL, lvlL, original_lvl_L))
          (Just (iterR, lvlR, original_lvl_R)) = case (iterL, iterR) of
            (ThreadID, ThreadID) -> (lvlL, original_lvl_L) `compare` (lvlR, original_lvl_R)
            (ThreadID, _) -> GT
            (_, ThreadID) -> LT
            _ -> (lvlL, original_lvl_L) `compare` (lvlR, original_lvl_R)

        max' lhs rhs =
          case cmpIdxPat lhs rhs of
            LT -> rhs
            _ -> lhs

        f og (Dependency lvl varType) = Just (varType, lvl, og)

gpuPermutation :: PrimExpTable -> SegOpName -> ArrayName -> IndexExprName -> [DimAccess rep] -> Maybe Permutation
gpuPermutation primExpTable _segOpName (_arr_name, nest, arr_layout) _idx_name dimAccesses = do
  -- Find the outermost parallel level. XXX: this is a bit hacky. Why
  -- don't we simply know at this point the nest in which this index
  -- occurs?
  let outermost_par = mininum $ foldMap (map lvl . parDeps) dimAccesses
      invariantToPar = (< outermost_par) . lvl

  -- Do nothing if last index is invariant to segop.
  let lastIdxIsInvariant = all invariantToPar $ dependencies $ last dimAccesses

  -- Do nothing if any index is constant, because otherwise we can end
  -- up transposing a too-large array.
  let anyIsConstant = any (null . dependencies) dimAccesses

  -- Check if any of the dependencies are too complex to reason about
  let dimAccesses' = filter (isJust . originalVar) dimAccesses
      deps = mapMaybe originalVar dimAccesses'
      counters = concatMap (map (isCounter . varType . snd) . M.toList . dependencies) dimAccesses'
      primExps = mapM (join . (`M.lookup` primExpTable)) deps
      inscrutable = maybe True (any (uncurry isInscrutable) . flip zip counters) primExps

  -- Create a candidate permutation
  let perm = map fst $ sortGPU (zip arr_layout dimAccesses)

  -- Check if we want to manifest this array with the permutation
  if lastIdxIsInvariant
    || anyIsConstant
    || inscrutable
    || commonPermutationEliminators perm nest
    then Nothing
    else Just perm
  where
    parDeps = filter ((== ThreadID) . varType) . M.elems . dependencies

instance Layout GPU where
  permutationFromDimAccess = gpuPermutation

-- | like mapMaybe, but works on nested maps. Eliminates "dangling"
-- maps / rows with missing (Nothing) values.
tableMapMaybe ::
  (k0 -> k1 -> k2 -> a -> Maybe b) ->
  M.Map k0 (M.Map k1 (M.Map k2 a)) ->
  M.Map k0 (M.Map k1 (M.Map k2 b))
tableMapMaybe f =
  M.mapMaybeWithKey $ \key0 -> mapToMaybe $ mapToMaybe . f key0
  where
    maybeMap :: M.Map k a -> Maybe (M.Map k a)
    maybeMap val = if null val then Nothing else Just val

    mapToMaybe g = maybeMap . M.mapMaybeWithKey g

-- | Given an ordering function for `DimAccess`, and an IndexTable,
-- return a LayoutTable. We remove entries with no results after
-- `permutationFromDimAccess`
layoutTableFromIndexTable ::
  (Layout rep) =>
  PrimExpTable ->
  IndexTable rep ->
  LayoutTable
layoutTableFromIndexTable = tableMapMaybe . permutationFromDimAccess
