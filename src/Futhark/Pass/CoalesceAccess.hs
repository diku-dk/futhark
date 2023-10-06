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
    onStms permutationTable _ stms = do
      let m = transformStms permutationTable stms
      -- let m = localScope scope $ stms'
      fmap snd $ modifyNameSource $ runState (runBuilderT m M.empty)

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

transformStms :: Ctx -> Stms GPU -> CoalesceM (Ctx, Stms GPU)
transformStms ctx =
  foldM
    ( \(ctx_acc, stms_acc) stm -> do
        (new_ctx, new_stms) <- transformStm ctx_acc stm
        pure (new_ctx, stms_acc S.>< new_stms)
    )
    (ctx, mempty)

transformStm :: Ctx -> Stm GPU -> CoalesceM (Ctx, Stms GPU)
transformStm ctx stm@(Let pat aux e) = do
  case e of
    (Match s cases body m) -> transformMatch stm ctx s cases body m
    (Loop bindings loop body) -> transformLoop stm ctx bindings loop body
    (Op op) -> transformOp stm ctx op
    _ -> pure (ctx, S.singleton stm)

transformMatch ::
  Stm GPU ->
  Ctx ->
  [SubExp] ->
  [Case (Body GPU)] ->
  Body GPU ->
  MatchDec (BranchType GPU) ->
  CoalesceM (Ctx, Stms GPU)
transformMatch (Let pat aux _) ctx s cases body m = do
  undefined

--  let bodies = map caseBody cases
--  let (ctx', body') = transformBody ctx body
--  let (ctx'', cases') =
--        foldl
--          ( \(ctx_acc, cases_acc) case' -> do
--              let body'' = caseBody case'
--              let (new_ctx, new_body) = transformBody ctx_acc body''
--              let new_case = Case (casePat case') new_body
--              (new_ctx, cases_acc ++ [new_case])
--          )
--          (ctx', mempty)
--          cases
--  (ctx'', S.singleton $ Let pat aux (Match s cases' body' m))

transformLoop ::
  Stm GPU ->
  Ctx ->
  [(FParam GPU, SubExp)] ->
  LoopForm ->
  Body GPU ->
  CoalesceM (Ctx, Stms GPU)
transformLoop (Let pat aux _) ctx bindings loop body = do
  (ctx', stms') <- transformStms ctx (bodyStms body)
  let body' = Body (bodyDec body) stms' (bodyResult body)
  pure (ctx', S.singleton $ Let pat aux (Loop bindings loop body'))

transformOp :: Stm GPU -> Ctx -> Op GPU -> CoalesceM (Ctx, Stms GPU)
transformOp (Let pat aux _) ctx op = do
  let patternName = patElemName . head $ patElems pat
  (ctx', new_stms, op') <- case op of
    (SegOp segop) -> transformSegOp patternName ctx segop
    (GPUBody types body) -> transformGPUBody ctx types body
    _ -> pure (ctx, mempty, Op op)
  pure (ctx', new_stms S.|> Let pat aux op')

transformBody :: Ctx -> Body GPU -> CoalesceM (Ctx, Body GPU)
transformBody ctx body = do
  (ctx', stms') <- transformStms ctx (bodyStms body)
  let body' = Body (bodyDec body) stms' (bodyResult body)
  pure (ctx', body')

transformGPUBody :: Ctx -> [Type] -> Body GPU -> CoalesceM (Ctx, Stms GPU, Exp GPU)
transformGPUBody ctx types body = do
  (ctx', body') <- transformBody ctx body
  pure (ctx', mempty, Op (GPUBody types body'))

transformSegOp ::
  VName ->
  Ctx ->
  SegOp SegLevel GPU ->
  CoalesceM (Ctx, Stms GPU, Exp GPU)
transformSegOp pat ctx op = do
  let body = segBody op
  case M.lookup pat (M.mapKeys vnameFromSegOp ctx) of
    Nothing -> pure (ctx, mempty, Op $ SegOp op)
    (Just arrayNameMap) ->
      do
        -- 1. Find all permutations in PermutationTable for this SegOp
        let arrayPermutationTuples =
              toPermutationSet $
                M.toList $
                  M.map (map snd . M.toList) arrayNameMap
        -- create permutation expressions for the segOp
        let permutationExprs =
              mapM manifestExpr arrayPermutationTuples
        pure (ctx, permutationExprs, Op $ SegOp op)
        undefined
  where
    -- TODO:
    -- 2. Insert permutations in the AST

    toPermutationSet arrayToPermutationMap =
      -- use a set to eliminate duplicate permutations
      nubOrd $
        concat
          [ [(arrayName, permutation) | permutation <- permutations]
            | (arrayName, permutations) <- arrayToPermutationMap
          ]

-- =================== Replace array names in AST ===================

replaceArrayNamesInBody :: ArrayNameReplacements -> Body GPU -> Body GPU
replaceArrayNamesInBody arr_map body = do
  let stms' =
        S.fromList $
          map (replaceArrayNamesInStm arr_map) (stmsToList $ bodyStms body)
  Body (bodyDec body) stms' (bodyResult body)

replaceArrayNamesInKernelBody :: ArrayNameReplacements -> KernelBody GPU -> KernelBody GPU
replaceArrayNamesInKernelBody arr_map body = do
  let stms' = S.fromList $ map (replaceArrayNamesInStm arr_map) (stmsToList $ kernelBodyStms body)
  KernelBody (kernelBodyDec body) stms' (kernelBodyResult body)

replaceArrayNamesInStm :: ArrayNameReplacements -> Stm GPU -> Stm GPU
replaceArrayNamesInStm arr_map (Let pat aux e) = do
  let patternName = patElemName . head $ patElems pat
  let e' = case e of
        (Match s cases body m) -> replaceArrayNamesInMatch arr_map s cases body m
        (Loop bindings loop body) -> replaceArrayNamesInLoop arr_map bindings loop body
        (Op op) -> replaceArrayNamesInOp arr_map op
        (BasicOp op) -> replaceArrayName arr_map patternName op
        _ -> e
  Let pat aux e'

replaceArrayNamesInMatch :: ArrayNameReplacements -> [SubExp] -> [Case (Body GPU)] -> Body GPU -> MatchDec (BranchType GPU) -> Exp GPU
replaceArrayNamesInMatch arr_map s cases body m = do
  let bodies = map caseBody cases
  let body' = replaceArrayNamesInBody arr_map body
  let cases' =
        foldl
          ( \cases_acc case' -> do
              let body'' = caseBody case'
              let new_body = replaceArrayNamesInBody arr_map body''
              let new_case = Case (casePat case') new_body
              cases_acc ++ [new_case]
          )
          mempty
          cases
  Match s cases' body' m

replaceArrayNamesInLoop :: ArrayNameReplacements -> [(FParam GPU, SubExp)] -> LoopForm -> Body GPU -> Exp GPU
replaceArrayNamesInLoop arr_map bindings loop body = Loop bindings loop (replaceArrayNamesInBody arr_map body)

replaceArrayNamesInOp :: ArrayNameReplacements -> Op GPU -> Exp GPU
replaceArrayNamesInOp arr_map op = do
  let op' = case op of
        (SegOp segop) -> replaceArrayNamesInSegOp arr_map segop
        (GPUBody types body) -> replaceArrayNamesInGPUBody arr_map types body
        _ -> op
  Op op'

replaceArrayNamesInSegOp :: ArrayNameReplacements -> SegOp SegLevel GPU -> Op GPU
replaceArrayNamesInSegOp arr_map op = do
  let body = segBody op
  let body' = replaceArrayNamesInKernelBody arr_map body
  let op' = case op of
        (SegMap lvl segSpace types body) -> SegMap lvl segSpace types body'
        (SegRed lvl segSpace o types body) -> SegRed lvl segSpace o types body'
        (SegScan lvl segSpace o types body) -> SegScan lvl segSpace o types body'
        (SegHist lvl segSpace o types body) -> SegHist lvl segSpace o types body'
  SegOp op'

replaceArrayNamesInGPUBody :: ArrayNameReplacements -> [Type] -> Body GPU -> Op GPU
replaceArrayNamesInGPUBody arr_map types body = GPUBody types (replaceArrayNamesInBody arr_map body)

replaceArrayName :: ArrayNameReplacements -> VName -> BasicOp -> Exp GPU
replaceArrayName arr_map idx_name op = do
  -- Look up the new array name
  case M.lookup idx_name arr_map of
    Nothing -> BasicOp op
    Just arr_name' -> do
      -- Replace the array name in the BasicOp
      let op' = case op of
            (Index _name slice) -> Index arr_name' slice
            (Update safety _name slice subExp) -> Update safety arr_name' slice subExp
      BasicOp op'

-- TODO:
-- 1. Find all permutations in PermutationTable for this SegOp
-- 2. Insert permutations in the AST

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

lookupPermutation :: (Coalesce rep) => PermutationTable -> ArrayName -> [DimIdxPat rep] -> Maybe Permutation
lookupPermutation mTable arrayName permutation =
  undefined

-- | Apply `f` to second/right part of tuple.
onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (x, y) = (x, f y)
