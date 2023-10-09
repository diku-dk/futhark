-- | Do various kernel optimisations - mostly related to coalescing.
-- module Futhark.Pass.CoalesceAccess (coalesceAccess, printAST) where
module Futhark.Pass.CoalesceAccess (coalesceAccess, printAST) where

import Control.Monad
import Control.Monad.State.Strict
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence.Internal qualified as S
import Debug.Pretty.Simple
import Futhark.Analysis.AccessPattern
import Futhark.Builder
import Futhark.Construct
import Futhark.IR.GPU
import Futhark.IR.Prop.Rearrange
import Futhark.IR.Syntax
import Futhark.Pass
import Futhark.Tools
import Futhark.Util

printAST :: Pass GPU GPU
printAST =
  Pass
    "pretty print ast"
    "Pretty-print the ast at current stage in pipeline"
    $ pure . pTraceShowId

-- | The pass definition.
coalesceAccess :: Pass GPU GPU
coalesceAccess =
  Pass
    "coalesce access"
    "Transform kernel input arrays for better performance."
    -- return
    $ \prog -> do
      -- Analyse the program
      let analysisRes = analyzeDimIdxPats prog
      -- Compute permutations to acheive coalescence for all arrays
      let permutationTable = permutationTableFromIndexTable compare analysisRes
      -- let permutationTable = permutationTableFromIndexTable (const . const EQ) analysisRes
      -- Insert permutations in the AST
      intraproceduralTransformation (onStms permutationTable) prog
  where
    onStms permutationTable scope stms = do
      let m = localScope scope $ transformStms permutationTable mempty stms
      fmap fst $ modifyNameSource $ runState (runBuilderT m M.empty)

-- fmap fst $ modifyNameSource $ pure stms'
-- pure stms'

type Ctx = PermutationTable

type Coalesce rep = Analyze rep

type CoalesceM = Builder GPU

-- | Resulting permutation to be inserted in the AST. The first element
-- is the permutation to be applied to the array, and the second
-- element is the array name. The second element is used to indicate
-- that we have already inserted a permutation for this array.
type Permutation = [Int]

type PermutationTable =
  M.Map SegOpName (M.Map ArrayName (M.Map IndexExprName Permutation))

type ArrayNameReplacements = M.Map IndexExprName VName

transformStms :: Ctx -> ExpMap -> Stms GPU -> CoalesceM (Stms GPU)
transformStms ctx expmap stms = collectStms_ $ foldM_ transformStm (ctx, expmap) stms

-- | Map from variable names to defining expression.  We use this to
-- hackily determine whether something is transposed or otherwise
-- funky in memory (and we'd prefer it not to be).  If we cannot find
-- it in the map, we just assume it's all good.  HACK and FIXME, I
-- suppose.  We really should do this at the memory level.
type ExpMap = M.Map VName (Stm GPU)

transformStm :: (Ctx, ExpMap) -> Stm GPU -> CoalesceM (Ctx, ExpMap)
transformStm (ctx, expmap) (Let pat aux (Op (SegOp op)))
  -- FIXME: We only make coalescing optimisations for SegThread
  -- SegOps, because that's what the analysis assumes.  For SegGroup
  -- we should probably look at the component SegThreads, but it
  -- apparently hasn't come up in practice yet.
  | SegThread {} <- segLevel op = do
      let mapper =
            identitySegOpMapper
              { mapOnSegOpBody =
                  transformSegThreadKernelBody ctx patternName
              }
      op' <- mapSegOpM mapper op
      let stm' = Let pat aux $ Op $ SegOp op'
      addStm stm'
      pure (ctx, M.fromList [(name, stm') | name <- patNames pat] <> expmap)
  | SegGroup {} <- segLevel op = do
      let mapper =
            identitySegOpMapper
              { mapOnSegOpBody =
                  transformSegThreadKernelBody ctx patternName
              }
      op' <- mapSegOpM mapper op
      let stm' = Let pat aux $ Op $ SegOp op'
      addStm stm'
      pure (ctx, M.fromList [(name, stm') | name <- patNames pat] <> expmap)
  | SegThreadInGroup {} <- segLevel op = undefined -- TODO: Handle this
  where
    patternName = patElemName . head $ patElems pat
transformStm (ctx, expmap) (Let pat aux e) = do
  e' <- mapExpM (transform ctx expmap) e
  let stm' = Let pat aux e'
  addStm stm'
  pure (ctx, M.fromList [(name, stm') | name <- patNames pat] <> expmap)

transform :: Ctx -> ExpMap -> Mapper GPU GPU CoalesceM
transform ctx expmap =
  identityMapper {mapOnBody = \scope -> localScope scope . transformBody ctx expmap}

-- | Recursively transform the statements in a body.
transformBody :: Ctx -> ExpMap -> Body GPU -> CoalesceM (Body GPU)
transformBody ctx expmap (Body () stms res) = do
  stms' <- transformStms ctx expmap stms
  pure $ Body () stms' res

-- | Recursively transform the statements in the body of a SegGroup kernel.
transformSegGroupKernelBody :: Ctx -> ExpMap -> KernelBody GPU -> CoalesceM (KernelBody GPU)
transformSegGroupKernelBody ctx expmap (KernelBody () stms res) = do
  stms' <- transformStms ctx expmap stms
  pure $ KernelBody () stms' res

-- | Transform the statements in the body of a SegThread kernel.
transformSegThreadKernelBody :: Ctx -> VName -> KernelBody GPU -> CoalesceM (KernelBody GPU)
transformSegThreadKernelBody ctx seg_name kbody = do
  evalStateT
    ( traverseKernelBodyArrayIndexes
        seg_name
        (ensureCoalescedAccess ctx)
        kbody
    )
    mempty

traverseKernelBodyArrayIndexes ::
  forall f.
  (Monad f) =>
  VName -> -- seg_name
  ArrayIndexTransform f ->
  KernelBody GPU ->
  f (KernelBody GPU)
traverseKernelBodyArrayIndexes seg_name coalesce (KernelBody () kstms kres) =
  KernelBody () . stmsFromList
    <$> mapM onStm (stmsToList kstms)
    <*> pure kres
  where
    onLambda :: Lambda GPU -> f (Lambda GPU)
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

    onOp :: Op GPU -> f (Op GPU)
    onOp (OtherOp soac) =
      OtherOp <$> mapSOACM soacMapper soac
    onOp op = pure op

    soacMapper =
      (identitySOACMapper @GPU)
        { mapOnSOACLambda = onLambda
        }

    mapper =
      (identityMapper @GPU)
        { mapOnBody = const onBody,
          mapOnOp = onOp
        }

type Replacements = M.Map (VName, Slice SubExp) VName

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
    seen <- gets $ M.lookup (arr, slice)
    case seen of
      -- Already took care of this case elsewhere.
      Just arr' -> pure $ Just (arr', slice)
      -- We have not seen this array before.
      Nothing ->
        -- Check if the array has the optimal layout in memory.
        -- If it does not, replace it with a manifest to allocate
        -- it with the optimal layout
        case lookupPermutation ctx seg_name idx_name arr of
          Nothing -> pTrace "\nNO PERMUTATION\n" $ pTraceShow ctx $ pTrace "\n" $ pure $ Just (arr, slice)
          Just perm -> pTraceShow perm $ pTrace "\n----------------\n" $ replace =<< lift (manifest perm arr)
    where
      -- Just perm -> pure $ Just (arr, slice)

      -- let (optimal, perm) = optimalPermutation arr idx_name seg_name ctx
      -- if optimal
      --   then pure $ Just (arr, slice)
      --   else replace =<< lift (manifest perm arr)

      replace arr' = do
        modify $ M.insert (arr, slice) arr'
        pure $ Just (arr', slice)

      manifest perm array =
        letExp (baseString array ++ "_coalesced") $
          BasicOp $
            Manifest perm array

segOpPermutationSet :: Ctx -> VName -> [(ArrayName, Permutation)]
segOpPermutationSet ctx pat =
  case M.lookup pat (M.mapKeys vnameFromSegOp ctx) of
    Nothing -> []
    (Just arrayNameMap) -> toPermutationSet $ M.toList $ M.map (map snd . M.toList) arrayNameMap
  where
    toPermutationSet arrayToPermutationMap =
      -- use nubOrd to eliminate duplicate permutations
      nubOrd $
        concat
          [ [(arrayName, permutation) | permutation <- permutations]
            | (arrayName, permutations) <- arrayToPermutationMap
          ]

-- | Given an ordering function for `DimIdxPat`, and an IndexTable, return
-- a PermutationTable.
permutationTableFromIndexTable :: (DimIdxPat rep -> DimIdxPat rep -> Ordering) -> IndexTable rep -> PermutationTable
permutationTableFromIndexTable sortCoalescing =
  -- We effectively convert all the `MemoryEntry`s in the index table,
  -- then, using mapMaybe, we discard all "rows" in the table which:
  -- \* contain no difference in the permutation, or
  -- \* is a permutation that we cannot achieve efficiently using transpositions.
  -- recall that IndexTable is just a mapping of
  -- SegOpName → ArrayName → IndexExprName → [DimIdxPat]
  -- and PermutationTable is just a variation of IndexTable that maps to
  -- a permutation instead of [DimIdxPat]
  filterMap null $
    filterMap null $
      filterMap predPermutation convert
  where
    -- wraps a function in a mapMaybe, if the predicate is true then return
    -- nothing. When sequenced as above, the "nothings" will then propagate and
    -- ensure that we don't have entries in any of the maps that points to empty
    -- maps.
    filterMap predicate f =
      M.mapMaybe
        ( \imm ->
            let res = f imm
             in if predicate res
                  then Nothing
                  else Just res
        )

    -- Is the permutation just the identity permutation, or not a transposition?
    predPermutation perm =
      perm `L.isPrefixOf` [0 ..] || isNothing (isMapTranspose perm)

    -- Sorts the dimensions in a given memory entry and returns it as
    -- a permutation.
    convert indices =
      let perm dims =
            map snd
              . L.sortBy (\a b -> sortCoalescing (fst a) (fst b))
              . flip zip [0 :: Int ..]
              $ dimensions dims
       in perm indices

lookupPermutation :: Ctx -> VName -> IndexExprName -> ArrayName -> Maybe Permutation
lookupPermutation ctx segName idxName arrayName =
  case M.lookup segName (M.mapKeys vnameFromSegOp ctx) of
    Nothing -> Nothing
    Just segmap ->
      -- Look for the current array
      case M.lookup arrayName segmap of
        Nothing -> Nothing
        Just idxs -> M.lookup idxName idxs

-- | Apply `f` to second/right part of tuple.
onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (x, y) = (x, f y)
