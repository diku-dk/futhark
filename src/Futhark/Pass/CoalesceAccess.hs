-- | Do various kernel optimisations - mostly related to coalescing.
-- module Futhark.Pass.CoalesceAccess (coalesceAccess, printAST) where
module Futhark.Pass.CoalesceAccess (coalesceAccess, printAST) where

import Control.Monad
import Control.Monad.State.Strict
import Data.IntMap.Strict qualified as S
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Debug.Pretty.Simple
import Futhark.Analysis.AccessPattern
import Futhark.Builder
import Futhark.Construct
import Futhark.IR.Aliases
import Futhark.IR.GPU
import Futhark.IR.GPUMem
import Futhark.IR.MC
import Futhark.IR.MCMem
-- import Futhark.IR.Prop.Rearrange
import Futhark.IR.SOACS
import Futhark.IR.Seq
import Futhark.IR.SeqMem
import Futhark.Pass

printAST :: (RepTypes rep) => Pass rep rep
printAST =
  Pass
    "pretty print ast"
    "Pretty-print the ast at current stage in pipeline"
    $ pure . pTraceShowId

-- | The pass definition.
coalesceAccess :: (Coalesce rep, BuilderOps rep) => Pass rep rep
coalesceAccess =
  Pass
    "coalesce access"
    "Transform kernel input arrays for better performance."
    -- return
    $ \prog -> do
      -- Analyse the program
      let analysisRes = analyzeDimIdxPats prog
      -- Compute permutations to acheive coalescence for all arrays
      let permutationTable = permutationTableFromIndexTable analysisRes
      -- Insert permutations in the AST
      intraproceduralTransformation (onStms permutationTable) prog
  where
    onStms permutationTable scope stms = do
      let m = localScope scope $ transformStms permutationTable mempty stms
      fmap fst $ modifyNameSource $ runState (runBuilderT m M.empty)

class (Analyze rep) => Coalesce rep where
  onOp :: forall m. (Monad m) => SOACMapper rep rep m -> Op rep -> m (Op rep)
  transformOp :: PermutationTable -> ExpMap rep -> Stm rep -> Op rep -> CoalesceM rep (PermutationTable, ExpMap rep)

  -- | Return a coalescing permutation that will be used to create a manifest of the array.
  -- Returns Nothing if the array is already in the optimal layout
  permutationFromMemoryEntry :: SegOpName -> IndexExprName -> ArrayName -> MemoryEntry rep -> Maybe Permutation

type CoalesceM rep = Builder rep

type Permutation = [Int]

type PermutationTable = M.Map SegOpName (M.Map ArrayName (M.Map IndexExprName Permutation))

-- | A map from the name of an expression to the expression that defines it.
type ExpMap rep = M.Map VName (Stm rep)

transformStms :: (Coalesce rep, BuilderOps rep) => PermutationTable -> ExpMap rep -> Stms rep -> CoalesceM rep (Stms rep)
transformStms permTable expmap stms = collectStms_ $ foldM_ transformStm (permTable, expmap) stms

transformStm :: (Coalesce rep, BuilderOps rep) => (PermutationTable, ExpMap rep) -> Stm rep -> CoalesceM rep (PermutationTable, ExpMap rep)
transformStm (permTable, expmap) (Let pat aux (Op op)) = transformOp permTable expmap (Let pat aux (Op op)) op
transformStm (permTable, expmap) (Let pat aux e) = do
  e' <- mapExpM (transform permTable expmap) e
  let stm' = Let pat aux e'
  addStm stm'
  pure (permTable, M.fromList [(name, stm') | name <- patNames pat] <> expmap)

instance Coalesce GPU where
  onOp soacMapper (Futhark.IR.GPU.OtherOp soac) = Futhark.IR.GPU.OtherOp <$> mapSOACM soacMapper soac
  onOp _ op = pure op
  transformOp permTable expmap stm gpuOp
    | (SegOp op) <- gpuOp = transformSegOpGPU permTable expmap stm op
    | _ <- gpuOp = transformRestOp permTable expmap stm

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

instance Coalesce MC where
  onOp soacMapper (Futhark.IR.MC.OtherOp soac) = Futhark.IR.MC.OtherOp <$> mapSOACM soacMapper soac
  onOp _ op = pure op
  transformOp permTable expmap stm mcOp
    | ParOp maybeParSegOp seqSegOp <- mcOp = transformSegOpMC permTable expmap stm maybeParSegOp seqSegOp
    | _ <- mcOp = transformRestOp permTable expmap stm

  permutationFromMemoryEntry _segOpName _idxName (_arrayName, nest) memEntry = do
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

transformSegOpGPU :: PermutationTable -> ExpMap GPU -> Stm GPU -> SegOp SegLevel GPU -> CoalesceM GPU (PermutationTable, ExpMap GPU)
transformSegOpGPU permTable expmap (Let pat aux _) op = do
  let mapper =
        identitySegOpMapper
          { mapOnSegOpBody = case segLevel op of
              -- We don't want to coalesce anything defined inside a SegGroup
              SegGroup {} -> pure
              -- In any other case, we want to coalesce and recurse
              _ -> transformSegThreadKernelBody permTable patternName
          }
  op' <- mapSegOpM mapper op
  let stm' = Let pat aux $ Op $ SegOp op'
  addStm stm'
  pure (permTable, M.fromList [(name, stm') | name <- patNames pat] <> expmap)
  where
    patternName = patElemName . head $ patElems pat

transformSegOpMC :: PermutationTable -> ExpMap MC -> Stm MC -> Maybe (SegOp () MC) -> SegOp () MC -> CoalesceM MC (PermutationTable, ExpMap MC)
transformSegOpMC permTable expmap (Let pat aux _) maybeParSegOp seqSegOp
  | Nothing <- maybeParSegOp = add Nothing
  | Just parSegOp <- maybeParSegOp = do
      -- Map the parallel part of the ParOp
      parSegOp' <- mapSegOpM mapper parSegOp
      add (Just parSegOp')
  where
    add maybeParSegOp' = do
      -- Map the sequential part of the ParOp
      seqSegOp' <- mapSegOpM mapper seqSegOp
      let stm' = Let pat aux $ Op $ ParOp maybeParSegOp' seqSegOp'
      addStm stm'
      pure (permTable, M.fromList [(name, stm') | name <- patNames pat] <> expmap)
    mapper = identitySegOpMapper {mapOnSegOpBody = transformKernelBody permTable expmap patternName}
    patternName = patElemName . head $ patElems pat

transformRestOp :: (Coalesce rep, BuilderOps rep) => PermutationTable -> ExpMap rep -> Stm rep -> CoalesceM rep (PermutationTable, ExpMap rep)
transformRestOp permTable expmap (Let pat aux e) = do
  e' <- mapExpM (transform permTable expmap) e
  let stm' = Let pat aux e'
  addStm stm'
  pure (permTable, M.fromList [(name, stm') | name <- patNames pat] <> expmap)

transform :: (Coalesce rep, BuilderOps rep) => PermutationTable -> ExpMap rep -> Mapper rep rep (CoalesceM rep)
transform permTable expmap =
  identityMapper {mapOnBody = \scope -> localScope scope . transformBody permTable expmap}

-- | Recursively transform the statements in a body.
transformBody :: (Coalesce rep, BuilderOps rep) => PermutationTable -> ExpMap rep -> Body rep -> CoalesceM rep (Body rep)
transformBody permTable expmap (Body b stms res) = do
  stms' <- transformStms permTable expmap stms
  pure $ Body b stms' res

-- | Transform the statements in the body of a SegThread kernel.
transformSegThreadKernelBody :: (Coalesce rep, BuilderOps rep) => PermutationTable -> VName -> KernelBody rep -> CoalesceM rep (KernelBody rep)
transformSegThreadKernelBody permTable seg_name kbody = do
  evalStateT
    ( traverseKernelBodyArrayIndexes
        seg_name
        (ensureCoalescedAccess permTable)
        kbody
    )
    mempty

transformKernelBody :: (Coalesce rep, BuilderOps rep) => PermutationTable -> ExpMap rep -> VName -> KernelBody rep -> CoalesceM rep (KernelBody rep)
transformKernelBody permTable expmap segName (KernelBody b stms res) = do
  stms' <- transformStms permTable expmap stms
  let kbody' = KernelBody b stms' res
  evalStateT
    ( traverseKernelBodyArrayIndexes
        segName
        (ensureCoalescedAccess permTable)
        kbody'
    )
    mempty

traverseKernelBodyArrayIndexes ::
  forall m rep.
  (Monad m, Coalesce rep) =>
  VName -> -- seg_name
  ArrayIndexTransform m ->
  KernelBody rep ->
  m (KernelBody rep)
traverseKernelBodyArrayIndexes seg_name coalesce (KernelBody b kstms kres) =
  KernelBody b . stmsFromList
    <$> mapM onStm (stmsToList kstms)
    <*> pure kres
  where
    onLambda lam =
      (\body' -> lam {lambdaBody = body'})
        <$> onBody (lambdaBody lam)

    onBody (Body bdec stms bres) = do
      stms' <- stmsFromList <$> mapM onStm (stmsToList stms)
      pure $ Body bdec stms' bres

    onStm (Let pat dec (BasicOp (Index arr is))) =
      Let pat dec . oldOrNew <$> coalesce seg_name patternName arr is
      where
        oldOrNew Nothing =
          BasicOp $ Index arr is
        oldOrNew (Just (arr', is')) =
          BasicOp $ Index arr' is'
        patternName = patElemName . head $ patElems pat
    onStm (Let pat dec e) =
      Let pat dec <$> mapExpM mapper e

    soacMapper =
      identitySOACMapper
        { mapOnSOACLambda = onLambda
        }

    mapper =
      (identityMapper @rep)
        { mapOnBody = const onBody,
          mapOnOp = onOp soacMapper
        }

-- | Used to keep track of which pairs of arrays and permutations we have
-- already created manifests for, in order to avoid duplicates.
type Replacements = M.Map (VName, Permutation) VName

type ArrayIndexTransform m =
  VName -> -- seg_name (name of the SegThread expression's pattern)
  VName -> -- idx_name (name of the Index expression's pattern)
  VName -> -- arr (name of the array)
  Slice SubExp -> -- slice
  m (Maybe (VName, Slice SubExp))

ensureCoalescedAccess ::
  (MonadBuilder m) =>
  PermutationTable ->
  ArrayIndexTransform (StateT Replacements m)
ensureCoalescedAccess
  permTable
  seg_name
  idx_name
  arr
  slice = do
    -- Check if the array has the optimal layout in memory.
    -- If it does not, replace it with a manifest to allocate
    -- it with the optimal layout
    case lookupPermutation permTable seg_name idx_name arr of
      Nothing -> pure $ Just (arr, slice)
      Just perm -> do
        seen <- gets $ M.lookup (arr, perm)
        case seen of
          -- Already created a manifest for this array + permutation.
          -- So, just replace the name and don't make a new manifest.
          Just arr' -> pure $ Just (arr', slice)
          Nothing -> do
            replace perm =<< lift (manifest perm arr)
    where
      replace perm arr' = do
        -- Store the fact that we have seen this array + permutation
        -- so we don't make duplicate manifests
        modify $ M.insert (arr, perm) arr'
        -- Return the new manifest
        pure $ Just (arr', slice)

      manifest perm array = do
        letExp (baseString array ++ "_coalesced") $
          BasicOp $
            Manifest perm array

-- | Given an ordering function for `DimIdxPat`, and an IndexTable, return
-- a PermutationTable.
permutationTableFromIndexTable :: (Coalesce rep) => IndexTable rep -> PermutationTable
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

lookupPermutation :: PermutationTable -> VName -> IndexExprName -> VName -> Maybe Permutation
lookupPermutation permTable segName idxName arrayName =
  case M.lookup segName (M.mapKeys vnameFromSegOp permTable) of
    Nothing -> Nothing
    Just arrayNameMap ->
      -- Look for the current array
      case M.lookup arrayName (M.mapKeys fst arrayNameMap) of
        Nothing -> Nothing
        Just idxs -> M.lookup idxName idxs

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

instance Coalesce GPUMem where
  onOp _ _ = error $ notImplementedYet "GPUMem"
  transformOp _ _ _ _ = error $ notImplementedYet "GPUMem"
  permutationFromMemoryEntry = error $ notImplementedYet "GPUMem"

instance Coalesce MCMem where
  onOp _ _ = error $ notImplementedYet "MCMem"
  transformOp _ _ _ _ = error $ notImplementedYet "MCMem"
  permutationFromMemoryEntry = error $ notImplementedYet "MCMem"

instance Coalesce Seq where
  onOp _ _ = error $ notImplementedYet "Seq"
  transformOp _ _ _ _ = error $ notImplementedYet "Seq"
  permutationFromMemoryEntry = error $ notImplementedYet "Seq"

instance Coalesce SeqMem where
  onOp _ _ = error $ notImplementedYet "SeqMem"
  transformOp _ _ _ _ = error $ notImplementedYet "SeqMem"
  permutationFromMemoryEntry = error $ notImplementedYet "SeqMem"

instance Coalesce SOACS where
  onOp _ _ = error $ notImplementedYet "SOACS"
  transformOp _ _ _ _ = error $ notImplementedYet "SOACS"
  permutationFromMemoryEntry = error $ notImplementedYet "SOACS"
