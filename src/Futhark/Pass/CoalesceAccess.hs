{-# LANGUAGE TypeFamilies #-}

-- | Do various kernel optimisations - mostly related to coalescing.
-- module Futhark.Pass.CoalesceAccess (coalesceAccess, printAST) where
module Futhark.Pass.CoalesceAccess (coalesceAccess, printAST) where

import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence.Internal qualified as S
import Debug.Pretty.Simple
import Futhark.Analysis.AccessPattern
import Futhark.IR.GPU
import Futhark.IR.Prop.Rearrange
import Futhark.Pass
import Futhark.Tools

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
      -- Compute manifests to acheive coalescence for all arrays
      let manifestTable = manifestTableFromIndexTable compare analysisRes
      -- let manifestTable = manifestTableFromIndexTable (const . const EQ) analysisRes
      -- Insert manifests in the AST
      intraproceduralTransformation (onStms manifestTable) prog
  where
    onStms manifestTable _ stms = do
      let (_, stms') = transformStms manifestTable stms
      -- let m = localScope scope $ stms'
      -- fmap fst $ modifyNameSource $ runState (runBuilderT m M.empty)
      -- fmap fst $ modifyNameSource $ pure stms'
      pure stms'

type Ctx = ManifestTable

type Coalesce rep = Analyze rep

-- | Resulting manifest to be inserted in the AST. The first element
-- is the permutation to be applied to the array, and the second
-- element is the array name. The second element is used to indicate
-- that we have already inserted a manifest for this array.
type Manifest = ([Int], Maybe VName)

type ManifestTable = M.Map SegOpName (M.Map ArrayName (M.Map IndexExprName Manifest))

transformStms :: Ctx -> Stms GPU -> (Ctx, Stms GPU)
transformStms ctx stms = do
  let (ctx', stms') =
        foldl
          ( \(ctx', stms') stm' -> do
              let (ctx'', stm'') = transformStm ctx' stm'
              (ctx'', stms' S.|> stm'')
          )
          (ctx, mempty)
          stms
  (ctx', stms')

transformStm :: Ctx -> Stm GPU -> (Ctx, Stm GPU)
transformStm ctx (Let pat aux e) = do
  let (ctx', e') = case e of
        (Match s cases body m) -> transformMatch ctx s cases body m
        (Loop bindings loop body) -> transformLoop ctx bindings loop body
        (Op op) -> transformOp ctx op
        _ -> (ctx, e)
  let stm' = Let pat aux e'
  (ctx', stm')

transformBody :: Ctx -> Body GPU -> (Ctx, Body GPU)
transformBody ctx body = do
  let (ctx', stms') = transformStms ctx (bodyStms body)
  let body' = Body (bodyDec body) stms' (bodyResult body)
  (ctx', body')

transformMatch :: Ctx -> [SubExp] -> [Case (Body GPU)] -> Body GPU -> MatchDec (BranchType GPU) -> (Ctx, Exp GPU)
transformMatch ctx s cases body m = do
  let bodies = map caseBody cases
  let (ctx', body') = transformBody ctx body
  let (ctx'', cases') =
        foldl
          ( \(ctx_acc, cases_acc) case' -> do
              let body'' = caseBody case'
              let (ctx''', body''') = transformBody ctx_acc body''
              let case'' = Case (casePat case') body'''
              (ctx''', cases_acc ++ [case''])
          )
          (ctx', mempty)
          cases
  (ctx'', Match s cases' body' m)

transformLoop :: Ctx -> [(FParam GPU, SubExp)] -> LoopForm -> Body GPU -> (Ctx, Exp GPU)
transformLoop ctx bindings loop body = do
  let (ctx', stms') = transformStms ctx (bodyStms body)
  let body' = Body (bodyDec body) stms' (bodyResult body)
  (ctx', Loop bindings loop body')

transformOp :: Ctx -> Op GPU -> (Ctx, Exp GPU)
transformOp ctx op = case op of
  (SegOp segop) -> transformSegOp ctx segop
  (GPUBody types body) -> transformGPUBody ctx types body
  _ -> (ctx, Op op)

transformGPUBody :: Ctx -> [Type] -> Body GPU -> (Ctx, Exp GPU)
transformGPUBody ctx types body =
  let (ctx', body') = transformBody ctx body
   in (ctx', Op (GPUBody types body'))

transformSegOp :: Ctx -> SegOp lvl GPU -> (Ctx, Exp GPU)
transformSegOp ctx op = do
  let body = segBody op
  -- TODO:
  -- 1. Find all manifests in ManifestTable for this SegOp
  -- 2. Insert manifests in the AST
  undefined

-- transformStm (ctx, expmap) (Let pat aux e) = do
-- e' <- mapExpM (transform ctx expmap) e
-- let stm' = Let pat aux e'
-- addStm stm'
-- pure (ctx, M.fromList [(name, stm') | name <- patNames pat] <> expmap)

-- do
-- let stms = stmsToList . bodyStms $ funDefBody func
-- -- \| Create a context from a list of parameters
-- let ctx = contextFromNames mempty Sequential $ map paramName $ funDefParams func
-- snd $ analyzeStmsPrimitive ctx stms

-- transformStms :: Ctx -> ExpMap -> Stms GPU -> CoalesceM (Stms GPU)
-- transformStms ctx expmap stms = collectStms_ $ foldM_ transformStm (ctx, expmap) stms

-- -- | Map from variable names to defining expression.  We use this to
-- -- hackily determine whether something is transposed or otherwise
-- -- funky in memory (and we'd prefer it not to be).  If we cannot find
-- -- it in the map, we just assume it's all good.  HACK and FIXME, I
-- -- suppose.  We really should do this at the memory level.
-- type ExpMap = M.Map VName (Stm GPU)

-- transformStm :: (Ctx, ExpMap) -> Stm GPU -> CoalesceM (Ctx, ExpMap)
-- transformStm (ctx, expmap) (Let pat aux (Op (SegOp op))) =
--   -- FIXME: We only make coalescing optimisations for SegThread
--   -- SegOps, because that's what the analysis assumes.  For SegGroup
--   -- we should probably look at the component SegThreads, but it
--   -- apparently hasn't come up in practice yet.
--   -- \| SegThread {} <- segLevel op
--   -- do
--   --   let mapper =
--   --         identitySegOpMapper
--   --           { mapOnSegOpBody =
--   --               transformSegThreadKernelBody ctx patternName
--   --           }
--   --   op' <- mapSegOpM mapper op
--   --   let stm' = Let pat aux $ Op $ SegOp op'
--   --   addStm stm'
--   --   pure (ctx, M.fromList [(name, stm') | name <- patNames pat] <> expmap)
--   -- \| SegGroup {} <- segLevel op = do
--   --     let mapper =
--   --           identitySegOpMapper
--   --             { mapOnSegOpBody =
--   --                 transformSegGroupKernelBody ctx expmap
--   --             }
--   --     op' <- mapSegOpM mapper op
--   --     let stm' = Let pat aux $ Op $ SegOp op'
--   --     addStm stm'
--   --     pure (ctx, M.fromList [(name, stm') | name <- patNames pat] <> expmap)
--   -- \| SegThreadInGroup {} <- segLevel op = undefined -- TODO: Handle this
--   -- where
--   --   patternName = patElemName . head $ patElems pat

--   do
--     let mapper =
--           identitySegOpMapper
--             { mapOnSegOpBody =
--                 transformSegThreadKernelBody ctx patternName
--             }
--     op' <- mapSegOpM mapper op
--     let stm' = Let pat aux $ Op $ SegOp op'
--     addStm stm'
--     pure (ctx, M.fromList [(name, stm') | name <- patNames pat] <> expmap)
--   where
--     patternName = patElemName . head $ patElems pat
-- transformStm (ctx, expmap) (Let pat aux e) = do
--   e' <- mapExpM (transform ctx expmap) e
--   let stm' = Let pat aux e'
--   addStm stm'
--   pure (ctx, M.fromList [(name, stm') | name <- patNames pat] <> expmap)

-- transform :: Ctx -> ExpMap -> Mapper GPU GPU CoalesceM
-- transform ctx expmap =
--   identityMapper {mapOnBody = \scope -> localScope scope . transformBody ctx expmap}

-- -- | Recursively transform the statements in a body.
-- transformBody :: Ctx -> ExpMap -> Body GPU -> CoalesceM (Body GPU)
-- transformBody ctx expmap (Body () stms res) = do
--   stms' <- transformStms ctx expmap stms
--   pure $ Body () stms' res

-- -- -- | Recursively transform the statements in the body of a SegGroup kernel.
-- -- transformSegGroupKernelBody :: Ctx -> ExpMap -> KernelBody GPU -> CoalesceM (KernelBody GPU)
-- -- transformSegGroupKernelBody ctx expmap (KernelBody () stms res) = do
-- --   stms' <- transformStms ctx expmap stms
-- --   pure $ KernelBody () stms' res

-- -- | Transform the statements in the body of a SegThread kernel.
-- transformSegThreadKernelBody :: Ctx -> VName -> KernelBody GPU -> CoalesceM (KernelBody GPU)
-- transformSegThreadKernelBody ctx seg_name kbody = do
--   evalStateT
--     ( traverseKernelBodyArrayIndexes
--         seg_name
--         (ensureCoalescedAccess ctx)
--         kbody
--     )
--     mempty

-- traverseKernelBodyArrayIndexes ::
--   forall m.
--   (Monad m) =>
--   VName -> -- seg_name
--   ArrayIndexTransform m ->
--   KernelBody GPU ->
--   m (KernelBody GPU)
-- traverseKernelBodyArrayIndexes seg_name coalesce (KernelBody () kstms kres) =
--   KernelBody () . stmsFromList
--     <$> mapM onStm (stmsToList kstms)
--     <*> pure kres
--   where
--     onLambda :: Lambda GPU -> f (Lambda GPU)
--     onLambda lam =
--       (\body' -> lam {lambdaBody = body'})
--         <$> onBody (lambdaBody lam)

--     onBody (Body bdec stms bres) = do
--       stms' <- stmsFromList <$> mapM onStm (stmsToList stms)
--       pure $ Body bdec stms' bres

--     onStm stm@(Let pat dec (BasicOp (Index arr is))) =
--       Let pat dec . oldOrNew <$> coalesce seg_name patternName arr is
--       where
--         oldOrNew Nothing = BasicOp $ Index arr is
--         oldOrNew (Just (arr', is')) = BasicOp $ Index arr' is'
--         patternName = patElemName . head $ patElems pat
--     onStm (Let pat dec e) =
--       Let pat dec <$> mapExpM mapper e

--     onOp :: Op GPU -> f (Op GPU)
--     onOp (OtherOp soac) =
--       OtherOp <$> mapSOACM soacMapper soac
--     onOp op = pure op

--     soacMapper =
--       (identitySOACMapper @GPU)
--         { mapOnSOACLambda = onLambda
--         }

--     mapper =
--       (identityMapper @GPU)
--         { mapOnBody = const onBody,
--           mapOnOp = onOp
--         }

-- type Replacements = M.Map (VName, Slice SubExp) VName

-- type StateBoi = (Ctx, Replacements)

-- type ArrayIndexTransform m =
--   Ctx -> -- ManifestTable
--   VName -> -- seg_name (name of the SegThread expression's pattern)
--   VName -> -- idx_name (name of the Index expression's pattern)
--   VName -> -- arr (name of the array)
--   Slice SubExp -> -- slice
--   m (Maybe (VName, Slice SubExp))

-- ensureCoalescedAccess ::
--   (MonadBuilder m) =>
--   -- Ctx ->
--   ArrayIndexTransform (StateT StateBoi m)
-- ensureCoalescedAccess
--   ctx
--   seg_name
--   idx_name
--   arr
--   slice = do
--     -- seen <- gets $ M.lookup (arr, slice)
--     -- case seen of
--     --   -- Already took care of this case elsewhere.
--     --   Just arr' -> pure $ Just (arr', slice)
--     --   -- We have not seen this array before.
--     --   Nothing -> do
--     --     -- Check if the array has the optimal layout in memory.
--     --     -- If it does not, replace it with a manifest to allocate
--     --     -- it with the optimal layout
--     --     let (optimal, perm) = optimalPermutation arr idx_name seg_name ctx
--     --     if optimal
--     --       then pure $ Just (arr, slice)
--     --       else replace =<< lift (manifest perm arr)
--     -- where
--     --   replace arr' = do
--     --     modify $ M.insert (arr, slice) arr'
--     --     pure $ Just (arr', slice)

--     -- Check if this array even has a manifest
--     case lookupManifest of
--       Nothing -> pure $ Just (arr, slice)
--       Just (perm, inserted) -> do
--         -- Check if the manifest is already inserted
--         case inserted of
--           -- Array is already inserted
--           Just new_name -> pure $ Just (new_name, slice)
--           -- Array is not inserted yet
--           Nothing -> do
--             -- TODO: Replace and update ctx!!!
--             replace =<< lift (manifest perm arr)
--     where
--       replace arr' = do
--         modify $ M.insert (arr, slice) arr'
--         pure $ Just (arr', slice)

--       manifest perm array =
--         letExp (baseString array ++ "_coalesced") $
--           BasicOp $
--             Manifest perm array

-- optimalPermutation :: VName -> VName -> VName -> Ctx -> (Bool, [Int])
-- optimalPermutation arr_name idx_name seg_name ctx = do
--   -- Example:
--   --   Initial ordering:
--   --     0: [par 1]
--   --     1: [par 2]
--   --     2: []
--   --     3: [par 0 | seq 1]
--   --   Optimal ordering:
--   --     0: []
--   --     1: [par 0 | seq 1]
--   --     2: [par 1]
--   --     3: [par 2]
--   --   perm' = [2,3,0,1]

--   -- If a dimension is dependent on two par with different levels, we take the highest level

-- | Given an ordering function for `DimIdxPat`, and an IndexTable, return
-- a ManifestTable.
manifestTableFromIndexTable :: (DimIdxPat rep -> DimIdxPat rep -> Ordering) -> IndexTable rep -> ManifestTable
manifestTableFromIndexTable sortCoalescing =
  -- We effectively convert all the `MemoryEntry`s in the index table,
  -- then, using mapMaybe, we discard all "rows" in the table which:
  -- \* contain no difference in the permutation, or
  -- \* is a permutation that we cannot achieve efficiently using transpositions.
  -- recall that IndexTable is just a mapping of
  -- SegOpName → ArrayName → IndexExprName → [DimIdxPat]
  -- and ManifestTable is just a variation of IndexTable that maps to
  -- a permutation instead of [DimIdxPat]
  filterMap null $
    filterMap null $
      filterMap predManifest convert
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
    predManifest (perm, _) =
      perm `L.isPrefixOf` [0 ..] || isNothing (isMapTranspose perm)

    -- Sorts the dimensions in a given memory entry and returns it as
    -- a permutation.
    convert indices =
      let perm dims =
            map snd
              . L.sortBy (\a b -> sortCoalescing (fst a) (fst b))
              . flip zip [0 :: Int ..]
              $ dimensions dims
       in (perm indices, Nothing)

lookupManifest :: (Coalesce rep) => ManifestTable -> ArrayName -> [DimIdxPat rep] -> Maybe Manifest
lookupManifest mTable arrayName permutation =
  undefined
