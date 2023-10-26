{-# LANGUAGE LambdaCase #-}

module Futhark.Analysis.AccessPattern
  ( analyzeDimIdxPats,
    analyzeFunction,
    vnameFromSegOp,
    analysisPropagateByTransitivity,
    Analyze,
    IndexTable,
    ArrayName,
    DimIdxPat (..),
    IndexExprName,
    IterationType (Sequential, Parallel),
    MemoryEntry (..),
    BodyType (..),
    SegOpName (SegmentedMap, SegmentedRed, SegmentedScan, SegmentedHist),
    notImplementedYet,
  )
where

import Data.Either (partitionEithers)
import Data.Foldable
import Data.IntMap.Strict qualified as S
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.IR.GPU
import Futhark.IR.GPUMem
import Futhark.IR.MC
import Futhark.IR.MCMem
import Futhark.IR.SOACS
import Futhark.IR.Seq
import Futhark.IR.SeqMem
import Futhark.Util.Pretty

class Analyze rep where
  analyzeOp :: Op rep -> (Context rep -> [VName] -> (Context rep, IndexTable rep))

-- | Map patterns of Segmented operations on arrays, to index expressions with
-- their index descriptors.
-- segmap(pattern) → A(pattern) → indexExpressionName(pattern) → [DimIdxPat]
type IndexTable rep =
  M.Map SegOpName (M.Map ArrayName (M.Map IndexExprName (MemoryEntry rep)))

-- | SegOpName stores the nested "level" at which it is declared in the AST.
data SegOpName
  = SegmentedMap (Int, VName)
  | SegmentedRed (Int, VName)
  | SegmentedScan (Int, VName)
  | SegmentedHist (Int, VName)
  deriving (Eq, Ord, Show)

type ArrayName = (VName, [BodyType])

type IndexExprName = VName

vnameFromSegOp :: SegOpName -> VName
vnameFromSegOp (SegmentedMap (_, name)) = name
vnameFromSegOp (SegmentedRed (_, name)) = name
vnameFromSegOp (SegmentedScan (_, name)) = name
vnameFromSegOp (SegmentedHist (_, name)) = name

-- | Each element in `dimensions` corresponds to an access to a given dimension
-- in the given array, in the same order of dimensions.
data MemoryEntry rep = MemoryEntry
  { dimensions :: [DimIdxPat rep],
    nest :: [BodyType]
  }
  deriving (Show)

-- | Collect all features of access to a specific dimension of an array.
data DimIdxPat rep = DimIdxPat
  { -- | Set of VNames of gtid's that some access is variant to.
    -- An empty set indicates that the access is invariant.
    -- Tuple of patternName and nested `level` it is created at.
    dependencies :: S.IntMap (VName, Int, IterationType rep),
    originalDimension :: Int
  }
  deriving (Eq, Show)

instance Semigroup (DimIdxPat rep) where
  (<>) :: DimIdxPat rep -> DimIdxPat rep -> DimIdxPat rep
  adeps <> bdeps =
    DimIdxPat (dependencies adeps <> dependencies bdeps) $
      max (originalDimension adeps) (originalDimension bdeps)

instance Monoid (DimIdxPat rep) where
  mempty = DimIdxPat {dependencies = mempty, originalDimension = 0}

-- | Iteration type describes whether the index is iterated in a parallel or
-- sequential way, ie. if the index expression comes from a sequential or
-- parallel construct, like foldl or map.
data IterationType rep = Sequential | Parallel
  deriving (Eq, Show)

data BodyType
  = SegOpName SegOpName
  | LoopBodyName LoopBodyName
  | CondBodyName CondBodyName
  deriving (Show, Ord, Eq)

type LoopBodyName = (Int, VName)

type CondBodyName = (Int, VName)

unionIndexTables :: IndexTable rep -> IndexTable rep -> IndexTable rep
unionIndexTables lhs rhs = do
  M.unionWith (M.unionWith M.union) lhs rhs

-- | Make segops on arrays transitive, ie. if
-- > let A = segmap (..) xs -- A indexes into xs
-- > let B = segmap (..) A  -- B indexes into A
-- Then B also derives all A's array-accesses, like xs.
-- Runs in n²
analysisPropagateByTransitivity :: IndexTable rep -> IndexTable rep
analysisPropagateByTransitivity idxTable =
  M.map
    foldlArrayNameMap
    idxTable
  where
    -- VName -> M.Map ArrayName (M.Map IndexExprName (MemoryEntry rep))
    aggregateResults arrayName =
      maybe
        mempty
        foldlArrayNameMap
        ((M.!?) (M.mapKeys vnameFromSegOp idxTable) arrayName)

    foldlArrayNameMap aMap =
      foldl (M.unionWith M.union) aMap (map aggregateResults $ M.keys $ M.mapKeys fst aMap)

--
-- Helper types and functions to perform the analysis.
--

-- | Used during the analysis to keep track of the dependencies of patterns
-- encountered so far.
data Context rep = Context
  { -- | A mapping from patterns occuring in Let expressions to their dependencies
    --  and iteration types.
    assignments :: M.Map (VName, [BodyType]) (CtxVal rep),
    -- | A set of all DimIndexes of type `Constant`.
    constants :: Names,
    -- | A list of the segMaps encountered during the analysis in the order they
    -- were encountered.
    parents :: [BodyType],
    -- | Current level of recursion
    currentLevel :: Int
  }
  deriving (Show, Eq)

instance Monoid (Context rep) where
  mempty =
    Context
      { assignments = mempty,
        constants = mempty,
        parents = [],
        currentLevel = 0
      }

instance Semigroup (Context rep) where
  (<>)
    (Context ass0 consts0 lastBody0 lvl0)
    (Context ass1 consts1 lastBody1 lvl1) =
      Context
        ((<>) ass0 ass1)
        ((<>) consts0 consts1)
        ((++) lastBody0 lastBody1)
        $ max lvl0 lvl1

-- | Extend a context with another context.
-- We never have to consider the case where VNames clash in the context, since
-- they are unique.
extend :: Context rep -> Context rep -> Context rep
extend = (<>)

allSegMap :: Context rep -> [SegOpName]
allSegMap (Context _ _ bodies _) =
  mapMaybe
    ( \case
        (SegOpName o) -> Just o
        _ -> Nothing
    )
    bodies

-- | Context Value (CtxVal) is the type used in the context to categorize
-- assignments. For example, a pattern might depend on a function parameter, a
-- gtid, or some other pattern.
data CtxVal rep = CtxVal
  { deps :: Names,
    iterationType :: IterationType rep,
    level :: Int
  }
  deriving (Show, Eq)

ctxValFromNames :: Context rep -> Names -> CtxVal rep
ctxValFromNames ctx names =
  CtxVal
    names
    (getIterationType ctx)
    (currentLevel ctx)

-- | Wrapper around the constructur of Context.
oneContext :: VName -> Context rep -> CtxVal rep -> Context rep
oneContext name ctx ctxValue =
  Context
    { assignments = M.singleton (name, parents ctx) ctxValue,
      constants = mempty,
      parents = [],
      currentLevel = 0
    }

-- | Create a singular ctxVal with no dependencies.
ctxValZeroDeps :: Context rep -> IterationType rep -> CtxVal rep
ctxValZeroDeps ctx iterType =
  CtxVal
    { deps = mempty,
      iterationType = iterType,
      level = currentLevel ctx
    }

-- | Create a singular context from a segspace
contextFromNames :: Context rep -> IterationType rep -> [VName] -> Context rep
contextFromNames ctx itertype =
  -- Create context from names in segspace
  foldl' extend ctx
    . map (\n -> oneContext n ctx (ctxValZeroDeps ctx itertype))

--  . zipWith
--    ( \i n ->
--        n
--          `oneContext` ctxValZeroDeps (ctx {currentLevel = currentLevel ctx + i}) itertype
--    )
--    [0 ..]

-- | Analyze each `entry` and accumulate the results.
analyzeDimIdxPats :: (Analyze rep) => Prog rep -> IndexTable rep
analyzeDimIdxPats = foldMap' analyzeFunction . progFuns

-- | Analyze each statement in a function body.
analyzeFunction :: (Analyze rep) => FunDef rep -> IndexTable rep
analyzeFunction func = do
  let stms = stmsToList . bodyStms $ funDefBody func
  -- \| Create a context from a list of parameters
  let ctx = contextFromNames mempty Sequential $ map paramName $ funDefParams func
  snd $ analyzeStmsPrimitive ctx stms

-- | Analyze each statement in a list of statements.
analyzeStmsPrimitive :: (Analyze rep) => Context rep -> [Stm rep] -> (Context rep, IndexTable rep)
analyzeStmsPrimitive ctx =
  -- Fold over statements in body
  foldl'
    (\(c, r) stm -> onSnd (unionIndexTables r) $ analyzeStm c stm)
    (ctx, mempty)

-- | Same as analyzeStmsPrimitive, but change the resulting context into
-- a ctxVal, mapped to pattern.
analyzeStms :: (Analyze rep) => Context rep -> Context rep -> ((Int, VName) -> BodyType) -> [VName] -> [Stm rep] -> (Context rep, IndexTable rep)
analyzeStms ctx tmp_ctx bodyConstructor pats body = do
  -- 0. Recurse into body with ctx
  let (ctx'', aids) = analyzeStmsPrimitive recContext body
  -- 1. We do not want the returned context directly.
  --    however, we do want pat to map to the names what was hit in body.
  --    therefore we need to subtract the old context from the returned one,
  --    and discard all the keys within it.

  -- assignments :: M.Map VName (CtxVal rep),
  let inScopeDependenciesFromBody =
        rmOutOfScopeDeps ctx'' $
          M.difference (M.mapKeys fst $ assignments ctx'') (M.mapKeys fst $ assignments recContext)
  -- 2. We are ONLY interested in the rhs of assignments (ie. the
  --    dependencies of pat :) )
  let ctx' = foldl extend ctx $ concatCtxVal inScopeDependenciesFromBody -- . map snd $ M.toList ctxVals
  -- 3. Now we have the correct context and result
  (ctx', aids)
  where
    -- Extracts and merges `Names` in `CtxVal`s, and makes a new CtxVal. This
    -- MAY throw away needed information, but it was my best guess at a solution
    -- at the time of writing.
    concatCtxVal dependencies =
      map (\pat -> oneContext pat ctx (ctxValFromNames ctx dependencies)) pats

    -- Context used for "recursion" into analyzeStmsPrimitive
    recContext =
      extend ctx $
        tmp_ctx
          { parents = concatMap (\pat -> [bodyConstructor (currentLevel ctx, pat)]) pats,
            currentLevel = currentLevel ctx + 1
          }

    -- Recursively looks up dependencies, until they're in scope or empty set.
    rmOutOfScopeDeps :: Context rep -> M.Map VName (CtxVal rep) -> Names
    rmOutOfScopeDeps ctx' newAssignments = do
      let rmParents = M.mapKeys fst
      let throwawayAssignments = rmParents $ assignments ctx'
      let localAssignments = rmParents $ assignments ctx
      let localConstants = constants ctx
      M.foldlWithKey
        ( \result a ctxval ->
            let dependencies = deps ctxval
             in -- if the VName of the assignment exists in the context, we are good
                if a `M.member` localAssignments || a `nameIn` localConstants
                  then result <> oneName a
                  else -- Otherwise, recurse on its dependencies;
                  -- 0. Add dependencies in ctx to result

                    let (depsInCtx, depsNotInCtx) =
                          partitionEithers
                            $ map
                              ( \d ->
                                  if d `M.member` localAssignments
                                    then Left d
                                    else Right d
                              )
                            $ namesToList dependencies
                     in let depsNotInCtx' =
                              M.fromList $
                                mapMaybe
                                  ( \d -> case M.lookup d throwawayAssignments of
                                      Just ctxval' -> Just (d, ctxval')
                                      _ -> Nothing
                                  )
                                  depsNotInCtx
                         in result
                              <> namesFromList depsInCtx
                              <> rmOutOfScopeDeps
                                ctx'
                                depsNotInCtx'
        )
        mempty
        newAssignments

getDeps :: SubExp -> Names
getDeps subexp =
  case subexp of
    (Var v) -> oneName v
    (Constant _) -> mempty

-- | Analyze a rep statement and return the updated context and array index
-- descriptors.
analyzeStm :: (Analyze rep) => Context rep -> Stm rep -> (Context rep, IndexTable rep)
analyzeStm ctx (Let pats _ e) = do
  -- Get the name of the first element in a pattern
  let patternNames = map patElemName $ patElems pats
  -- Construct the result and Context from the subexpression. If the subexpression
  -- is a body, we recurse into it.
  case e of
    (BasicOp (Index name (Slice ee))) -> analyzeIndex ctx patternNames name ee
    (BasicOp (Update _ name (Slice subexp) _subexp)) -> analyzeIndex ctx patternNames name subexp
    (BasicOp op) -> analyzeBasicOp ctx op patternNames
    (Match conds cases defaultBody _) -> analyzeMatch (contextFromNames ctx (getIterationType ctx) $ concatMap (namesToList . getDeps) conds) patternNames defaultBody $ map caseBody cases
    (Loop bindings loop body) -> analyzeLoop ctx bindings loop body patternNames
    (Apply _name diets _ _) -> analyzeApply ctx patternNames diets
    (WithAcc _ _) -> (ctx, mempty) -- ignored
    (Op op) -> analyzeOp op ctx patternNames

getIndexDependencies :: Context rep -> [DimIndex SubExp] -> Maybe [DimIdxPat rep]
getIndexDependencies _ [] = Nothing
getIndexDependencies ctx dims =
  fst . foldl' (\(a, i) idx -> (a >>= matchDimIndex idx i, i - 1)) (Just [], length dims - 1) $ reverse dims
  where
    matchDimIndex idx i accumulator =
      case idx of
        (DimFix subExpression) ->
          Just $ (consolidate ctx subExpression) {originalDimension = i} : accumulator
        _ -> Nothing

analyzeIndex :: Context rep -> [VName] -> VName -> [DimIndex SubExp] -> (Context rep, IndexTable rep)
analyzeIndex ctx pats arr_name dimIndexes = do
  let dimindices = getIndexDependencies ctx dimIndexes
  case L.find (\((n, _), _) -> n == arr_name) $ M.toList $ assignments ctx of
    Nothing -> error "yee haw"
    (Just (a, _)) ->
      maybe
        (ctx, mempty)
        (analyzeIndex' (analyzeIndexContextFromIndices ctx dimIndexes pats) pats a)
        dimindices

analyzeIndexContextFromIndices :: Context rep -> [DimIndex SubExp] -> [VName] -> Context rep
analyzeIndexContextFromIndices ctx dimIndexes pats = do
  let (subExprs, consts) =
        partitionEithers $
          mapMaybe
            ( \case
                (DimFix subExpression) -> case subExpression of
                  (Var v) -> Just (Left v)
                  (Constant _) -> Just (Right pats)
                (DimSlice _offs _n _stride) -> Nothing
            )
            dimIndexes

  -- Add each non-constant DimIndex as a dependency to the index expression
  let ctxVal = ctxValFromNames ctx $ namesFromList subExprs

  -- Add each constant DimIndex to the context
  -- Extend context with the dependencies and constants index expression
  foldl' extend ctx $ map (\pat -> (oneContext pat ctx ctxVal) {constants = namesFromList $ concat consts}) pats

analyzeIndex' :: Context rep -> [VName] -> (VName, [BodyType]) -> [DimIdxPat rep] -> (Context rep, IndexTable rep)
analyzeIndex' ctx _ _ [_] = (ctx, mempty)
analyzeIndex' ctx pats arr_name dimIndexes = do
  let segmaps = allSegMap ctx
  let memory_entries = MemoryEntry dimIndexes $ parents ctx
  let idx_expr_name = pats --                                                IndexExprName
  let map_ixd_expr = map (`M.singleton` memory_entries) idx_expr_name --     IndexExprName |-> MemoryEntry
  let map_array = map (M.singleton arr_name) map_ixd_expr --   ArrayName |-> IndexExprName |-> MemoryEntry
  let results = concatMap (\ma -> map (`M.singleton` ma) segmaps) map_array
  let res = foldl' unionIndexTables mempty results
  (ctx, res)

analyzeBasicOp :: Context rep -> BasicOp -> [VName] -> (Context rep, IndexTable rep)
analyzeBasicOp ctx expression pats = do
  -- Construct a CtxVal from the subexpressions
  let ctx_val = case expression of
        (SubExp subexp) -> ctxValFromNames ctx $ analyzeSubExpr pats ctx subexp
        (Opaque _ subexp) -> ctxValFromNames ctx $ analyzeSubExpr pats ctx subexp
        (ArrayLit subexps _t) -> concatCtxVals mempty subexps
        (UnOp _ subexp) -> ctxValFromNames ctx $ analyzeSubExpr pats ctx subexp
        (BinOp _ lsubexp rsubexp) -> concatCtxVals mempty [lsubexp, rsubexp]
        (CmpOp _ lsubexp rsubexp) -> concatCtxVals mempty [lsubexp, rsubexp]
        (ConvOp _ subexp) -> ctxValFromNames ctx $ analyzeSubExpr pats ctx subexp
        (Assert subexp _ _) -> ctxValFromNames ctx $ analyzeSubExpr pats ctx subexp
        (Index name _) ->
          error $ "unhandled: Index (This should NEVER happen) into " ++ prettyString name
        (Update _ name _slice _subexp) ->
          error $ "unhandled: Update (This should NEVER happen) onto " ++ prettyString name
        -- Technically, do we need this case?
        (Concat _ _ length_subexp) -> ctxValFromNames ctx $ analyzeSubExpr pats ctx length_subexp
        (Manifest _dim name) -> ctxValFromNames ctx $ oneName name
        (Iota end start stride _) -> concatCtxVals mempty [end, start, stride]
        (Replicate (Shape shape) value') -> concatCtxVals mempty (value' : shape)
        (Scratch _ subexprs) -> concatCtxVals mempty subexprs
        (Reshape _ (Shape shape_subexp) name) -> concatCtxVals (oneName name) shape_subexp
        (Rearrange _ name) -> ctxValFromNames ctx $ oneName name
        (UpdateAcc name lsubexprs rsubexprs) -> concatCtxVals (oneName name) (lsubexprs ++ rsubexprs)
        (FlatIndex name _) -> ctxValFromNames ctx $ oneName name
        (FlatUpdate name _ source) -> ctxValFromNames ctx $ namesFromList [name, source]
  let ctx' = foldl' extend ctx $ map (\n -> oneContext n ctx ctx_val) pats
  (ctx', mempty)
  where
    concatCtxVals ne nn =
      ctxValFromNames
        ctx
        (foldl' (\a -> (<>) a . analyzeSubExpr pats ctx) ne nn)

analyzeMatch :: (Analyze rep) => Context rep -> [VName] -> Body rep -> [Body rep] -> (Context rep, IndexTable rep)
analyzeMatch ctx pats body bodies =
  let ctx'' = ctx {currentLevel = currentLevel ctx - 1}
   in foldl
        ( \(ctx', res) b ->
            -- This Little Maneuver's Gonna Cost Us 51 Years
            onFst constLevel
              . onSnd (unionIndexTables res)
              . analyzeStms ctx' mempty CondBodyName pats
              . stmsToList
              $ bodyStms b
        )
        (ctx'', mempty)
        (body : bodies)
  where
    constLevel context = context {currentLevel = currentLevel ctx - 1}

analyzeLoop :: (Analyze rep) => Context rep -> [(FParam rep, SubExp)] -> LoopForm -> Body rep -> [VName] -> (Context rep, IndexTable rep)
analyzeLoop ctx bindings loop body pats = do
  let nextLevel = currentLevel ctx
  let ctx'' = ctx {currentLevel = nextLevel}
  let ctx' =
        contextFromNames ctx'' Sequential $
          case loop of
            (WhileLoop iterVar) -> iterVar : map (paramName . fst) bindings
            (ForLoop iterVar _ _) -> iterVar : map (paramName . fst) bindings

  -- Extend context with the loop expression
  analyzeStms ctx' mempty LoopBodyName pats $ stmsToList $ bodyStms body

analyzeApply :: Context rep -> [VName] -> [(SubExp, Diet)] -> (Context rep, IndexTable rep)
analyzeApply ctx pats diets =
  onFst
    ( \ctx' ->
        foldl' extend ctx' $ map (\pat -> oneContext pat ctx $ ctxValFromNames ctx' $ foldl' (<>) mempty $ map (getDeps . fst) diets) pats
    )
    (ctx, mempty)

segOpType :: SegOp lvl rep -> (Int, VName) -> SegOpName
segOpType (SegMap {}) = SegmentedMap
segOpType (SegRed {}) = SegmentedRed
segOpType (SegScan {}) = SegmentedScan
segOpType (SegHist {}) = SegmentedHist

analyzeSegOp :: (Analyze rep) => SegOp lvl rep -> Context rep -> [VName] -> (Context rep, IndexTable rep)
analyzeSegOp op ctx pats = do
  let nextLevel = currentLevel ctx + length (unSegSpace $ segSpace op) - 1
  let ctx' = ctx {currentLevel = nextLevel}
  let segSpaceContext =
        foldl' extend ctx'
          . map (\(n, i) -> oneContext n ctx' $ CtxVal mempty Parallel (currentLevel ctx + i))
          . (\segspaceParams -> zip segspaceParams [0 ..])
          -- contextFromNames ctx' Parallel
          . map fst
          . unSegSpace
          $ segSpace op
  -- Analyze statements in the SegOp body
  analyzeStms segSpaceContext mempty (SegOpName . segOpType op) pats . stmsToList . kernelBodyStms $ segBody op

analyzeSizeOp :: SizeOp -> Context rep -> [VName] -> (Context rep, IndexTable rep)
analyzeSizeOp op ctx pats = do
  let subexprsToContext =
        contextFromNames ctx Sequential
          . concatMap (namesToList . analyzeSubExpr pats ctx)
  let ctx' = case op of
        (CmpSizeLe _name _class subexp) -> subexprsToContext [subexp]
        (CalcNumGroups lsubexp _name rsubexp) -> subexprsToContext [lsubexp, rsubexp]
        _ -> ctx
  -- Add sizeOp to context
  let ctx'' = foldl' extend ctx' $ map (\pat -> oneContext pat ctx' $ ctxValZeroDeps ctx Sequential) pats
  (ctx'', mempty)

-- | Analyze statements in a rep body.
analyzeGPUBody :: (Analyze rep) => Body rep -> Context rep -> (Context rep, IndexTable rep)
analyzeGPUBody body ctx =
  analyzeStmsPrimitive ctx $ stmsToList $ bodyStms body

analyzeOtherOp :: Context rep -> [VName] -> (Context rep, IndexTable rep)
analyzeOtherOp ctx _ = (ctx, mempty)

-- | Get the iteration type of the last SegOp encountered in the context.
getIterationType :: Context rep -> IterationType rep
getIterationType (Context _ _ bodies _) =
  getIteration_rec bodies
  where
    getIteration_rec [] = Sequential
    getIteration_rec rec =
      case last rec of
        SegOpName _ -> Parallel
        LoopBodyName _ -> Sequential
        -- We can't really trust cond/match to be sequential/parallel, so
        -- recurse a bit
        CondBodyName _ -> getIteration_rec $ init rec

-- | Returns an intmap of names, to be used as dependencies in construction of
-- CtxVals.
-- Throws an error if SubExp contains a name not in context. This behaviour
-- might be thrown out in the future, as it is mostly just a very verbose way to
-- ensure that we capture all necessary variables in the context at the moment
-- of development.
analyzeSubExpr :: [VName] -> Context rep -> SubExp -> Names
analyzeSubExpr _ _ (Constant _) = mempty
analyzeSubExpr pp ctx (Var v) =
  case M.lookup v (M.mapKeys fst $ assignments ctx) of
    (Just _) -> oneName v
    Nothing ->
      error $
        "Failed to lookup variable \""
          ++ prettyString v
          ++ "\npat: "
          ++ prettyString pp
          ++ "\n\nContext\n"
          ++ show ctx

-- | Reduce a DimFix into its set of dependencies
consolidate :: Context rep -> SubExp -> DimIdxPat rep
consolidate _ (Constant _) = mempty
consolidate ctx (Var v) = reduceDependencies ctx v

-- | Recursively lookup vnames until vars with no deps are reached.
reduceDependencies :: Context rep -> VName -> DimIdxPat rep
reduceDependencies ctx v =
  if v `nameIn` constants ctx
    then mempty -- If v is a constant, then it is not a dependency
    else case M.lookup v (M.mapKeys fst $ assignments ctx) of
      Nothing -> mempty -- error $ "Unable to find " ++ prettyString v
      Just (CtxVal deps itertype lvl) ->
        if null $ namesToList deps
          then DimIdxPat (S.fromList [(baseTag v, (v, lvl, itertype))]) $ currentLevel ctx
          else
            foldl' (<>) mempty $
              map (reduceDependencies ctx) $
                namesToList deps

-- Misc functions

-- | Apply `f` to first/left part of tuple.
onFst :: (a -> c) -> (a, b) -> (c, b)
onFst f (x, y) = (f x, y)

-- | Apply `f` to second/right part of tuple.
onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (x, y) = (x, f y)

-- Instances for AST types that we actually support
instance Analyze GPU where
  analyzeOp gpuOp
    | (SegOp op) <- gpuOp = analyzeSegOp op
    | (SizeOp op) <- gpuOp = analyzeSizeOp op
    | (GPUBody _ body) <- gpuOp = pure . analyzeGPUBody body
    | (Futhark.IR.GPU.OtherOp _) <- gpuOp = analyzeOtherOp

instance Analyze GPUMem where
  analyzeOp _ = error $ notImplementedYet "GPUMem"

instance Analyze MC where
  analyzeOp mcOp
    | ParOp Nothing seqSegop <- mcOp = analyzeSegOp seqSegop
    | ParOp (Just segop) seqSegop <- mcOp = \ctx name -> do
        let (ctx', res') = analyzeSegOp segop ctx name
        let (ctx'', res'') = analyzeSegOp seqSegop ctx name
        (ctx' <> ctx'', unionIndexTables res' res'')
    | Futhark.IR.MC.OtherOp _ <- mcOp = analyzeOtherOp

instance Analyze MCMem where
  analyzeOp _ = error $ notImplementedYet "MCMem"

instance Analyze Seq where
  analyzeOp _ = error $ notImplementedYet "Seq"

instance Analyze SeqMem where
  analyzeOp _ = error $ notImplementedYet "SeqMem"

instance Analyze SOACS where
  analyzeOp _ = error $ notImplementedYet "SOACS"

notImplementedYet :: String -> String
notImplementedYet s = "Access pattern analysis for the " ++ s ++ " backend is not implemented."

instance Pretty (IndexTable rep) where
  pretty = stack . map f . M.toList :: IndexTable rep -> Doc ann
    where
      f (segop, arrayNameToIdxExprMap) = pretty segop <+> colon <+> g arrayNameToIdxExprMap

      g maps = lbrace </> indent 4 (mapprintArray $ M.toList maps) </> rbrace

      mapprintArray :: [(ArrayName, M.Map IndexExprName (MemoryEntry rep))] -> Doc ann
      mapprintArray [] = ""
      mapprintArray [m] = printArrayMap m
      mapprintArray (m : mm) = printArrayMap m </> mapprintArray mm

      printArrayMap (name, maps) =
        "(arr)"
          <+> pretty name
          <+> colon
          <+> lbrace
          </> indent 4 (mapprintIdxExpr (M.toList maps))
          </> rbrace

      mapprintIdxExpr :: [(IndexExprName, MemoryEntry rep)] -> Doc ann
      mapprintIdxExpr [] = ""
      mapprintIdxExpr [m] = printIdxExpMap m
      mapprintIdxExpr (m : mm) = printIdxExpMap m </> mapprintIdxExpr mm

      printIdxExpMap (name, mems) = "(idx)" <+> pretty name <+> ":" </> indent 4 (printMemoryEntry mems)

      printMemoryEntry :: MemoryEntry rep -> Doc ann
      printMemoryEntry (MemoryEntry dims _) =
        stack $ map printDim dims

      printDim m = pretty (originalDimension m) <+> ":" <+> indent 0 (pretty m)

instance Pretty (DimIdxPat rep) where
  pretty dimidx =
    -- Instead of using `brackets $` we manually enclose with `[`s, to add
    -- spacing between the enclosed elements
    "dependencies" <+> equals <+> align (prettyDeps $ dependencies dimidx)
    where
      prettyDeps = braces . commasep . map (printPair . snd) . S.toList
      printPair (name, lvl, itertype) = pretty name <+> pretty lvl <+> pretty itertype

instance Pretty SegOpName where
  pretty (SegmentedMap (_, name)) = "(segmap)" <+> pretty name
  pretty (SegmentedRed (_, name)) = "(segred)" <+> pretty name
  pretty (SegmentedScan (_, name)) = "(segscan)" <+> pretty name
  pretty (SegmentedHist (_, name)) = "(seghist)" <+> pretty name

instance Pretty BodyType where
  pretty (SegOpName (SegmentedMap (_, name))) = pretty name <+> colon <+> "segmap"
  pretty (SegOpName (SegmentedRed (_, name))) = pretty name <+> colon <+> "segred"
  pretty (SegOpName (SegmentedScan (_, name))) = pretty name <+> colon <+> "segscan"
  pretty (SegOpName (SegmentedHist (_, name))) = pretty name <+> colon <+> "seghist"
  pretty (LoopBodyName (_, name)) = pretty name <+> colon <+> "loop"
  pretty (CondBodyName (_, name)) = pretty name <+> colon <+> "cond"

instance Pretty (IterationType rep) where
  pretty Sequential = "seq"
  pretty Parallel = "par"
