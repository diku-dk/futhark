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
import Futhark.Analysis.AnalysePrimExp
import Futhark.IR.Aliases
import Futhark.IR.GPU
import Futhark.IR.MC
import Futhark.IR.MCMem

type Permutation = [Int]

type LayoutTable =
  M.Map
    SegOpName
    ( M.Map
        ArrayName
        (M.Map IndexExprName Permutation)
    )

class (PrimExpAnalysis rep) => Layout rep where
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

isInscrutable :: PrimExp VName -> Bool -> Bool
isInscrutable op@(BinOpExp {}) counter =
  if counter
    then -- Calculate stride and offset for loop-counters and thread-IDs
    case reduceStrideAndOffset op of
      -- Maximum allowable stride, might need tuning.
      Just (s, _) -> s > 8
      Nothing -> True
    else isInscrutableRec op
isInscrutable op _ = isInscrutableRec op

isInscrutableRec :: PrimExp VName -> Bool
isInscrutableRec (LeafExp _ _) = False
isInscrutableRec (ValueExp _) = False
isInscrutableRec (BinOpExp _ a b) =
  isInscrutableRec a || isInscrutableRec b
-- TODO: Handle UnOpExp
isInscrutableRec _ = True

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
    reduce _ (UnOpExp Not _) = Nothing
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
      inefficient_transpose = (isNothing . isMapTranspose) perm
      -- or if the last idx remains last
      static_last_idx = last perm == length perm - 1
      -- Don't manifest if the array is defined inside a segOp or loop body
      inside_undesired = any undesired nest

  is_invalid_perm || is_identity || inefficient_transpose || static_last_idx || inside_undesired
  where
    undesired :: BodyType -> Bool
    undesired bodyType = case bodyType of
      SegOpName _ -> True
      LoopBodyName _ -> True
      _ -> False

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

-- | like mapMaybe, but works on nested maps. Eliminates "dangling"
-- maps / rows with missing (Nothing) values.
tableMapMaybe :: (k0 -> k1 -> k2 -> a -> Maybe b) -> M.Map k0 (M.Map k1 (M.Map k2 a)) -> M.Map k0 (M.Map k1 (M.Map k2 b))
tableMapMaybe f =
  M.mapMaybeWithKey $
    \key0 -> mapToMaybe $ mapToMaybe . f key0
  where
    maybeMap :: M.Map k a -> Maybe (M.Map k a)
    maybeMap val = if null val then Nothing else Just val

    mapToMaybe g = maybeMap . M.mapMaybeWithKey g

sortGPU :: [(Int, DimAccess rep)] -> [(Int, DimAccess rep)]
sortGPU =
  L.sortBy dimdexGPUcmp
  where
    dimdexGPUcmp (ia, a) (ib, b) = do
      let depsA = dependencies a
      let depsB = dependencies b
      let deps1' = map (f ia . snd) $ M.toList depsA
      let deps2' = map (f ib . snd) $ M.toList depsB
      let aggr1 = foldl maxIdxPat Nothing deps1'
      let aggr2 = foldl maxIdxPat Nothing deps2'
      cmpIdxPat aggr1 aggr2
      where
        cmpIdxPat :: Maybe (VarType, Int, Int) -> Maybe (VarType, Int, Int) -> Ordering
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

        maxIdxPat :: Maybe (VarType, Int, Int) -> Maybe (VarType, Int, Int) -> Maybe (VarType, Int, Int)
        maxIdxPat lhs rhs =
          case cmpIdxPat lhs rhs of
            LT -> rhs
            _ -> lhs

        f og (Dependency lvl varType) = Just (varType, lvl, og)

sortMC :: [(Int, DimAccess rep)] -> [(Int, DimAccess rep)]
sortMC =
  L.sortBy dimdexMCcmp
  where
    dimdexMCcmp (ia, a) (ib, b) = do
      let depsA = dependencies a
          depsB = dependencies b
          deps1' = map (f ia . snd) $ M.toList depsA
          deps2' = map (f ib . snd) $ M.toList depsB
          aggr1 = foldl maxIdxPat Nothing deps1'
          aggr2 = foldl maxIdxPat Nothing deps2'
      cmpIdxPat aggr1 aggr2
      where
        cmpIdxPat :: Maybe (VarType, Int, Int) -> Maybe (VarType, Int, Int) -> Ordering
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

        maxIdxPat :: Maybe (VarType, Int, Int) -> Maybe (VarType, Int, Int) -> Maybe (VarType, Int, Int)
        maxIdxPat lhs rhs =
          case cmpIdxPat lhs rhs of
            LT -> rhs
            _ -> lhs

        f og (Dependency lvl varType) = Just (varType, lvl, og)

instance Layout MC where
  permutationFromDimAccess = multicorePermutation

instance Layout GPU where
  permutationFromDimAccess primExpTable _segOpName (_arr_name, nest, arr_layout) _idx_name dimAccesses = do
    -- Dont accept indices where the last index is invariant
    let lastIdxIsInvariant = isInvariant $ last dimAccesses

    -- Check if any of the dependencies are too complex to reason about
    let dimAccesses' = filter (isJust . originalVar) dimAccesses
        deps = mapMaybe originalVar dimAccesses'
        counters = concatMap (map (isCounter . varType . snd) . M.toList . dependencies) dimAccesses'
        primExps = mapM (join . (`M.lookup` primExpTable)) deps
        inscrutable = maybe True (any (uncurry isInscrutable) . flip zip counters) primExps

    -- Create a candidate permutation
    let perm = map fst $ sortGPU (zip arr_layout dimAccesses)

    -- Check if we want to manifest this array with the permutation
    if lastIdxIsInvariant || inscrutable || commonPermutationEliminators perm nest
      then Nothing
      else Just perm

-- | Given an ordering function for `DimAccess`, and an IndexTable,
-- return a LayoutTable. We remove entries with no results after
-- `permutationFromDimAccess`
layoutTableFromIndexTable :: (Layout rep) => PrimExpTable -> IndexTable rep -> LayoutTable
layoutTableFromIndexTable primExpTable =
  tableMapMaybe (permutationFromDimAccess primExpTable)
