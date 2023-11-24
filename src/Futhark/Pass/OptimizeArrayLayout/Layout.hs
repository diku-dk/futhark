module Futhark.Pass.OptimizeArrayLayout.Layout (permutationTableFromIndexTable, Layout, Permutation, commonPermutationEliminators, PermutationTable) where

import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
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
  permutationFromDimAccess primExpTable _segOpName (_arr_name, nest, arr_layout) _idxName dimAccesses =
    do
      -- Create a candidate permutation
      let perm = map fst $ sortGPU (zip arr_layout dimAccesses)

      -- Dont accept indices where the last index is invariant
      let lastIdxIsInvariant = isInvariant $ last dimAccesses

      -- Check if any of the dependencies are too complex to reason about
      let dimAccesses' = filter (isJust . originalVar) dimAccesses
      let deps = mapMaybe originalVar dimAccesses'
      let counters = concatMap (map (isCounter . varType . snd) . M.toList . dependencies) dimAccesses'
      let primExps = map (`M.lookup` primExpTable) deps
      let inscrutable = any (uncurry isInscrutable) (zip primExps counters)

      -- Check if we want to manifest this array with the permutation
      if lastIdxIsInvariant || inscrutable || commonPermutationEliminators perm nest
        then Nothing
        else Just perm

isInscrutable :: Maybe (Maybe (PrimExp VName)) -> Bool -> Bool
isInscrutable maybeOp@(Just (Just op@(BinOpExp {}))) counter =
  if counter
    then -- Calculate stride and offset for loop-counters and thread-IDs
    case reduceStrideAndOffset op of
      -- Maximum allowable stride and offset
      Just (s, _) -> s > 8 -- TODO: Tune these values
      Nothing -> True
    else isInscrutableRec maybeOp
isInscrutable maybeOp _ = isInscrutableRec maybeOp

isInscrutableRec :: Maybe (Maybe (PrimExp VName)) -> Bool
isInscrutableRec Nothing = True
isInscrutableRec (Just Nothing) = True
isInscrutableRec (Just (Just (LeafExp _ _))) = False
isInscrutableRec (Just (Just (ValueExp _))) = False
isInscrutableRec (Just (Just (BinOpExp _ a b))) =
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

  | UnOpExp Not _ <- op = Nothing
  | UnOpExp (Complement _) _  <- op = Nothing
  | UnOpExp (Abs _) _ <- op = Nothing
  | UnOpExp _ sub_op <- op = reduceStrideAndOffset sub_op

  | ConvOpExp _ sub_op <- op = reduceStrideAndOffset sub_op


-- Same as above, idk why, im just testing stuff out at this point
reduceStrideAndOffset (BinOpExp oper  op (ValueExp (IntValue v)))
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

  | UnOpExp Not _ <- op = Nothing
  | UnOpExp (Complement _) _  <- op = Nothing
  | UnOpExp (Abs _) _ <- op = Nothing
  | UnOpExp _ sub_op <- op = reduceStrideAndOffset sub_op

  | ConvOpExp _ sub_op <- op = reduceStrideAndOffset sub_op

reduceStrideAndOffset _ = Nothing

multicorePermutation :: PrimExpTable -> SegOpName -> ArrayName -> IndexExprName -> [DimAccess rep] -> Maybe Permutation
multicorePermutation primExpTable _segOpName (_arr_name, nest, arr_layout) _idxName dimAccesses = do
  -- Create a candidate permutation
  let perm = map fst $ sortMC (zip arr_layout dimAccesses)

  -- Check if any of the dependencies are too complex to reason about
  let dimAccesses' = filter (isJust . originalVar) dimAccesses
  let deps = mapMaybe originalVar dimAccesses'
  let counters = concatMap (map (isCounter . varType . snd) . M.toList . dependencies) dimAccesses'
  let primExps = map (`M.lookup` primExpTable) deps
  let inscrutable = any (uncurry isInscrutable) (zip primExps counters)

  -- Check if we want to manifest this array with the permutation
  if inscrutable || commonPermutationEliminators perm nest
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
commonPermutationEliminators :: [Int] -> [BodyType] -> Bool
commonPermutationEliminators perm nest = do
  -- Don't manifest if the permutation is the permutation is invalid
  let isInvalidPerm =
        -- Don't manifest if the permutation is the identity permutation
        perm `L.isPrefixOf` [0 ..]
          -- or is not a transpose.
          || (isNothing . isMapTranspose) perm
          -- or is not a permutation.
          || not (L.sort perm `L.isPrefixOf` [0 ..])
          -- or if the last idx remains last
          || (last perm == length perm - 1)

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
          (Just (iterL, lvlL, originalLevelL))
          (Just (iterR, lvlR, originalLevelR)) = case (iterL, iterR) of
            (ThreadID, ThreadID) -> EQ
            (ThreadID, _) -> GT
            (_, ThreadID) -> LT
            _ -> (lvlL, originalLevelL) `compare` (lvlR, originalLevelR)

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
          (Just (iterL, lvlL, originalLevelL))
          (Just (iterR, lvlR, originalLevelR)) =
            case (iterL, iterR) of
              (ThreadID, ThreadID) -> (lvlL, originalLevelL) `compare` (lvlR, originalLevelR)
              (ThreadID, _) -> LT
              (_, ThreadID) -> GT
              _ -> (lvlL, originalLevelL) `compare` (lvlR, originalLevelR)

        maxIdxPat :: Maybe (VarType, Int, Int) -> Maybe (VarType, Int, Int) -> Maybe (VarType, Int, Int)
        maxIdxPat lhs rhs =
          case cmpIdxPat lhs rhs of
            LT -> rhs
            _ -> lhs

        f og (Dependency lvl varType) = Just (varType, lvl, og)

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
