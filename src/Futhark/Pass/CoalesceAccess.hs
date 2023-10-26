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
import Futhark.IR.Syntax
import Futhark.Pass
import Futhark.Util

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
  transformOp :: Ctx -> ExpMap rep -> Stm rep -> Op rep -> CoalesceM rep (Ctx, ExpMap rep)

  -- | Return a coalescing permutation that will be used to create a manifest of the array.
  -- Returns Nothing if the array is already in the optimal layout
  permutationFromMemoryEntry :: SegOpName -> IndexExprName -> ArrayName -> MemoryEntry rep -> Maybe Permutation

type Ctx = PermutationTable

type CoalesceM rep = Builder rep

type Permutation = [Int]

type PermutationTable = M.Map SegOpName (M.Map ArrayName (M.Map IndexExprName Permutation))

type ExpMap rep = M.Map VName (Stm rep)

transformStms :: (Coalesce rep, BuilderOps rep) => Ctx -> ExpMap rep -> Stms rep -> CoalesceM rep (Stms rep)
transformStms ctx expmap stms = collectStms_ $ foldM_ transformStm (ctx, expmap) stms

transformStm :: (Coalesce rep, BuilderOps rep) => (Ctx, ExpMap rep) -> Stm rep -> CoalesceM rep (Ctx, ExpMap rep)
transformStm (ctx, expmap) (Let pat aux (Op op)) = transformOp ctx expmap (Let pat aux (Op op)) op
transformStm (ctx, expmap) (Let pat aux e) = do
  e' <- mapExpM (transform ctx expmap) e
  let stm' = Let pat aux e'
  addStm stm'
  pure (ctx, M.fromList [(name, stm') | name <- patNames pat] <> expmap)

instance Coalesce GPU where
  onOp soacMapper (Futhark.IR.GPU.OtherOp soac) = Futhark.IR.GPU.OtherOp <$> mapSOACM soacMapper soac
  onOp _ op = pure op
  transformOp ctx expmap stm gpuOp
    | (SegOp op) <- gpuOp = transformSegOpGPU ctx expmap stm op
    | _ <- gpuOp = transformRestOp ctx expmap stm
  permutationFromMemoryEntry segOpName idxName (arrayName, nest) memEntry = do
    let perm = (map originalDimension . (sortGPU . dimensions)) memEntry

    let isIdentity = perm `L.isPrefixOf` [0 ..]
    let isNotTranspose = isNothing (isMapTranspose perm)

    let nestSegOps = filter isUndesired nest
    let isInsideUndesired = not (null nestSegOps)

    if -- isInsideUndesired ||
    isIdentity || isNotTranspose
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
  transformOp ctx expmap stm mcOp
    | ParOp maybeParSegOp seqSegOp <- mcOp = transformSegOpMC ctx expmap stm maybeParSegOp seqSegOp
    | _ <- mcOp = transformRestOp ctx expmap stm
  permutationFromMemoryEntry segOpName idxName arrayName memEntry = do
    let perm = (map originalDimension . (sortMC . dimensions)) memEntry
    -- Is the permutation just the identity permutation, or not a transposition?
    if perm `L.isPrefixOf` [0 ..] || isNothing (isMapTranspose perm)
      then Nothing
      else Just perm

transformSegOpGPU :: Ctx -> ExpMap GPU -> Stm GPU -> SegOp SegLevel GPU -> CoalesceM GPU (Ctx, ExpMap GPU)
transformSegOpGPU ctx expmap (Let pat aux _) op = do
  let mapper =
        identitySegOpMapper
          { mapOnSegOpBody = case segLevel op of
              -- We don't want to coalesce anything defined inside a SegGroup
              SegGroup {} -> pure
              -- In any other case, we want to coalesce and recurse
              _ -> transformSegThreadKernelBody ctx patternName
          }
  op' <- mapSegOpM mapper op
  let stm' = Let pat aux $ Op $ SegOp op'
  addStm stm'
  pure (ctx, M.fromList [(name, stm') | name <- patNames pat] <> expmap)
  where
    patternName = patElemName . head $ patElems pat

transformSegOpMC :: Ctx -> ExpMap MC -> Stm MC -> Maybe (SegOp () MC) -> SegOp () MC -> CoalesceM MC (Ctx, ExpMap MC)
transformSegOpMC ctx expmap (Let pat aux _) maybeParSegOp seqSegOp
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
      pure (ctx, M.fromList [(name, stm') | name <- patNames pat] <> expmap)
    mapper = identitySegOpMapper {mapOnSegOpBody = transformKernelBody ctx expmap patternName}
    patternName = patElemName . head $ patElems pat

transformRestOp :: (Coalesce rep, BuilderOps rep) => Ctx -> ExpMap rep -> Stm rep -> CoalesceM rep (Ctx, ExpMap rep)
transformRestOp ctx expmap (Let pat aux e) = do
  e' <- mapExpM (transform ctx expmap) e
  let stm' = Let pat aux e'
  addStm stm'
  pure (ctx, M.fromList [(name, stm') | name <- patNames pat] <> expmap)

transform :: (Coalesce rep, BuilderOps rep) => Ctx -> ExpMap rep -> Mapper rep rep (CoalesceM rep)
transform ctx expmap =
  identityMapper {mapOnBody = \scope -> localScope scope . transformBody ctx expmap}

-- | Recursively transform the statements in a body.
transformBody :: (Coalesce rep, BuilderOps rep) => Ctx -> ExpMap rep -> Body rep -> CoalesceM rep (Body rep)
transformBody ctx expmap (Body b stms res) = do
  stms' <- transformStms ctx expmap stms
  pure $ Body b stms' res

-- | Recursively transform the statements in the body of a SegGroup kernel.
transformSegGroupKernelBody :: (Coalesce rep, BuilderOps rep) => Ctx -> ExpMap rep -> KernelBody rep -> CoalesceM rep (KernelBody rep)
transformSegGroupKernelBody ctx expmap (KernelBody b stms res) = do
  stms' <- transformStms ctx expmap stms
  pure $ KernelBody b stms' res

-- | Transform the statements in the body of a SegThread kernel.
transformSegThreadKernelBody :: (Coalesce rep, BuilderOps rep) => Ctx -> VName -> KernelBody rep -> CoalesceM rep (KernelBody rep)
transformSegThreadKernelBody ctx seg_name kbody = do
  evalStateT
    ( traverseKernelBodyArrayIndexes
        seg_name
        (ensureCoalescedAccess ctx)
        kbody
    )
    mempty

transformKernelBody :: (Coalesce rep, BuilderOps rep) => Ctx -> ExpMap rep -> VName -> KernelBody rep -> CoalesceM rep (KernelBody rep)
transformKernelBody ctx expmap segName (KernelBody b stms res) = do
  stms' <- transformStms ctx expmap stms
  let kbody' = KernelBody b stms' res
  evalStateT
    ( traverseKernelBodyArrayIndexes
        segName
        (ensureCoalescedAccess ctx)
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
  Ctx ->
  ArrayIndexTransform (StateT Replacements m)
ensureCoalescedAccess
  ctx
  seg_name
  idx_name
  arr
  slice = do
    -- Check if the array has the optimal layout in memory.
    -- If it does not, replace it with a manifest to allocate
    -- it with the optimal layout
    case lookupPermutation ctx seg_name idx_name arr of
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
permutationTableFromIndexTable indexTable = do
  -- We effectively convert all the `MemoryEntry`s in the index table,
  -- then, using mapMaybe, we discard all "rows" in the table which:
  -- \* contain no difference in the permutation, or
  -- \* is a permutation that we cannot achieve efficiently using transpositions.
  -- recall that IndexTable is just a mapping of
  -- SegOpName → ArrayName → IndexExprName → [DimIdxPat]
  -- and PermutationTable is just a variation of IndexTable that maps to
  -- a permutation instead of [DimIdxPat]

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
  where

--   ( filterMap null $
--       filterMap null $
--         filterMap predPermutation permutationFromMemoryEntry
--   )
--   indexTable

-- (SegOpName, ArrayName, IndexExprName, Permutation)

-- Wraps a function in a mapMaybe, if the predicate is true then return
-- nothing. When sequenced as above, the "nothings" will then propagate and
-- ensure that we don't have entries in any of the maps that points to empty
-- maps.
-- filterMap :: (b -> Bool) -> (a -> b) -> M.Map k a -> M.Map k b
-- filterMap predicateFunc permFunc =
--   M.mapMaybe
--     ( \val -> do
--         let res = permFunc val
--         if predicateFunc res
--           then Just res
--           else Nothing
--     )

-- -- Is the permutation just the identity permutation, or not a transposition?
-- predPermutation perm =
--   not (perm `L.isPrefixOf` [0 ..] || isNothing (isMapTranspose perm))

lookupPermutation :: Ctx -> VName -> IndexExprName -> VName -> Maybe Permutation
lookupPermutation ctx segName idxName arrayName =
  case M.lookup segName (M.mapKeys vnameFromSegOp ctx) of
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
  onOp _ _ = undefined
  transformOp _ _ _ _ = error $ notImplementedYet "GPUMem"

instance Coalesce MCMem where
  onOp _ _ = undefined
  transformOp _ _ _ _ = error $ notImplementedYet "MCMem"

instance Coalesce Seq where
  onOp _ _ = undefined
  transformOp _ _ _ _ = error $ notImplementedYet "Seq"

instance Coalesce SeqMem where
  onOp _ _ = undefined
  transformOp _ _ _ _ = error $ notImplementedYet "SeqMem"

instance Coalesce SOACS where
  onOp _ _ = undefined
  transformOp _ _ _ _ = error $ notImplementedYet "SOACS"
