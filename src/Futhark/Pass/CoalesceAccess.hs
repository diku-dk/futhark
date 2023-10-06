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

type ArrayNameReplacements = M.Map IndexExprName VName

transformStms :: Ctx -> Stms GPU -> (Ctx, Stms GPU)
transformStms ctx stms = do
  let (ctx', stms') =
        foldl
          ( \(ctx_acc, stms_acc) stm -> do
              let (new_ctx, new_stms) = transformStm ctx_acc stm
              (new_ctx, stms_acc S.>< new_stms)
          )
          (ctx, mempty)
          stms
  (ctx', stms')

transformStm :: Ctx -> Stm GPU -> (Ctx, Stms GPU)
transformStm ctx stm@(Let pat aux e) = do
  let (ctx', new_stms) = case e of
        (Match s cases body m) -> transformMatch stm ctx s cases body m
        (Loop bindings loop body) -> transformLoop stm ctx bindings loop body
        (Op op) -> transformOp stm ctx op
        _ -> (ctx, S.singleton stm)
  (ctx', new_stms)

transformMatch :: Stm GPU -> Ctx -> [SubExp] -> [Case (Body GPU)] -> Body GPU -> MatchDec (BranchType GPU) -> (Ctx, Stms GPU)
transformMatch (Let pat aux _) ctx s cases body m = do
  let bodies = map caseBody cases
  let (ctx', body') = transformBody ctx body
  let (ctx'', cases') =
        foldl
          ( \(ctx_acc, cases_acc) case' -> do
              let body'' = caseBody case'
              let (new_ctx, new_body) = transformBody ctx_acc body''
              let new_case = Case (casePat case') new_body
              (new_ctx, cases_acc ++ [new_case])
          )
          (ctx', mempty)
          cases
  (ctx'', S.singleton $ Let pat aux (Match s cases' body' m))

transformLoop :: Stm GPU -> Ctx -> [(FParam GPU, SubExp)] -> LoopForm -> Body GPU -> (Ctx, Stms GPU)
transformLoop (Let pat aux _) ctx bindings loop body = do
  let (ctx', stms') = transformStms ctx (bodyStms body)
  let body' = Body (bodyDec body) stms' (bodyResult body)
  (ctx', S.singleton $ Let pat aux (Loop bindings loop body'))

transformOp :: Stm GPU -> Ctx -> Op GPU -> (Ctx, Stms GPU)
transformOp (Let pat aux _) ctx op = do
  let patternName = patElemName . head $ patElems pat
  let (ctx', new_stms, op') = case op of
        (SegOp segop) -> transformSegOp patternName ctx segop
        (GPUBody types body) -> transformGPUBody ctx types body
        _ -> (ctx, mempty, Op op)
  (ctx', new_stms S.|> Let pat aux op')

transformBody :: Ctx -> Body GPU -> (Ctx, Body GPU)
transformBody ctx body = do
  let (ctx', stms') = transformStms ctx (bodyStms body)
  let body' = Body (bodyDec body) stms' (bodyResult body)
  (ctx', body')

transformGPUBody :: Ctx -> [Type] -> Body GPU -> (Ctx, Stms GPU, Exp GPU)
transformGPUBody ctx types body =
  let (ctx', body') = transformBody ctx body
   in (ctx', mempty, Op (GPUBody types body'))

transformSegOp :: VName -> Ctx -> SegOp SegLevel GPU -> (Ctx, Stms GPU, Exp GPU)
transformSegOp pat ctx op = do
  let body = segBody op
  case M.lookup pat (M.mapKeys vnameFromSegOp ctx) of
    Nothing -> (ctx, mempty, Op $ SegOp op)
    _ -> undefined

-- TODO:
-- 1. Find all manifests in ManifestTable for this SegOp
-- 2. Insert manifests in the AST

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

-- | Apply `f` to second/right part of tuple.
onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (x, y) = (x, f y)
