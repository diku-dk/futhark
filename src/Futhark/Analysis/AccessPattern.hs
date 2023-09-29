{-# LANGUAGE LambdaCase #-}

module Futhark.Analysis.AccessPattern
  ( analyzeDimIdxPats,
    analyzeFunction,
    Analyze,
    IndexTable,
    ArrayName,
    DimIdxPat (DimIdxPat),
    IndexExprName,
    IterationType (Sequential, Parallel),
    MemoryEntry (..),
    BodyType (..),
    SegOpName (SegmentedMap, SegmentedRed, SegmentedScan, SegmentedHist),
  )
where

import Data.Either (partitionEithers)
import Data.Foldable
import Data.IntMap.Strict qualified as S
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.IR.Aliases
import Futhark.IR.GPU
import Futhark.IR.GPUMem
import Futhark.IR.MC
import Futhark.IR.MCMem
import Futhark.IR.SOACS
import Futhark.IR.Seq
import Futhark.IR.SeqMem

class Analyze rep where
  analyzeOp :: Op rep -> (Context -> VName -> (Context, IndexTable))

-- | Map patterns of Segmented operations on arrays, to index expressions with
-- their index descriptors.
-- segmap(pattern) → A(pattern) → indexExpressionName(pattern) → [DimIdxPat]
type IndexTable =
  M.Map SegOpName (M.Map ArrayName (M.Map IndexExprName MemoryEntry))

data SegOpName
  = SegmentedMap (Int, VName)
  | SegmentedRed (Int, VName)
  | SegmentedScan (Int, VName)
  | SegmentedHist (Int, VName)
  deriving (Eq, Ord, Show)

vnameFromSegOp :: SegOpName -> VName
vnameFromSegOp (SegmentedMap (_, name)) = name
vnameFromSegOp (SegmentedRed (_, name)) = name
vnameFromSegOp (SegmentedScan (_, name)) = name
vnameFromSegOp (SegmentedHist (_, name)) = name

type ArrayName = VName

type IndexExprName = VName

-- | Each element in `dimensions` corresponds to an access to a given dimension
-- in the given array, in the same order of dimensions.
data MemoryEntry = MemoryEntry
  { dimensions :: [DimIdxPat],
    nest :: [BodyType]
  }

-- | Collect all features of access to a specific dimension of an array.
newtype DimIdxPat = DimIdxPat
  { -- | Set of VNames of gtid's that some access is variant to.
    -- An empty set indicates that the access is invariant.
    -- Tuple of patternName and nested `level` it is created at.
    dependencies :: S.IntMap (VName, Int, IterationType)
  }
  deriving (Eq, Show)

data Lvls = Lvls Int Int
  deriving (Eq, Show)

data OrderedDep = OrderedDep VName Lvls IterationType
  deriving (Eq, Show)

instance Ord Lvls where
  compare (Lvls l1 l'1) (Lvls l2 l'2) =
    if l1 == l2
      then l'1 `compare` l'2
      else l1 `compare` l2

instance Ord OrderedDep where
  compare (OrderedDep _ lvls1 it1) (OrderedDep _ lvls2 it2) =
    if it1 == it2
      then lvls1 `compare` lvls2
      else case (it1, it2) of
        (Parallel, Sequential) -> GT
        (Sequential, Parallel) -> LT

instance Ord DimIdxPat where
  compare (DimIdxPat deps1) (DimIdxPat deps2) = do
    let n = VName "" 0
    let lvls = (-1, -1)
    let it = Sequential
    let deps1' = map f (zip (map snd $ S.toList deps1) [0 :: Int ..])
    let deps2' = map f (zip (map snd $ S.toList deps2) [0 :: Int ..])
    let aggr1 = foldl max (n, lvls, it) deps1'
    let aggr2 = foldl max (n, lvls, it) deps2'
    compare aggr1 aggr2
    where
      f ((n, l, it), l') = (n, (l, l'), it)

-- [ [ par (1,1)], [ par (1,0)] ]

instance Semigroup DimIdxPat where
  (<>) :: DimIdxPat -> DimIdxPat -> DimIdxPat
  (<>) (DimIdxPat adeps) (DimIdxPat bdeps) =
    DimIdxPat ((<>) adeps bdeps)

instance Monoid DimIdxPat where
  mempty = DimIdxPat {dependencies = mempty}

-- | Iteration type describes whether the index is iterated in a parallel or
-- sequential way, ie. if the index expression comes from a sequential or
-- parallel construct, like foldl or map.
data IterationType
  = Sequential
  | Parallel
  deriving (Eq, Ord, Show)

data BodyType
  = SegOpName SegOpName
  | LoopBodyName LoopBodyName
  | CondBodyName CondBodyName
  deriving (Show, Ord, Eq)

type LoopBodyName = (Int, VName)

type CondBodyName = (Int, VName)

unionIndexTables :: IndexTable -> IndexTable -> IndexTable
unionIndexTables lhs rhs = do
  -- Find all occurences of ArrayName's from rhs in lhs
  let lhsNames = M.fromList $ map (onFst vnameFromSegOp) $ M.toList lhs
  let rhs' =
        M.map
          ( \arraynames ->
              foldl'
                (M.unionWith M.union)
                arraynames
                . map
                  (fromMaybe mempty . flip M.lookup lhsNames . fst)
                $ M.toList arraynames
          )
          rhs
  M.unionWith (M.unionWith M.union) lhs rhs'

--
-- Helper types and functions to perform the analysis.
--

-- | Used during the analysis to keep track of the dependencies of patterns
-- encountered so far.
data Context = Context
  { -- | A mapping from patterns occuring in Let expressions to their dependencies
    --  and iteration types.
    assignments :: M.Map VName CtxVal,
    -- | A set of all DimIndexes of type `Constant`.
    constants :: Names,
    -- | A list of the segMaps encountered during the analysis in the order they
    -- were encountered.
    parents :: [BodyType],
    -- | Current level of recursion
    currentLevel :: Int
  }
  deriving (Show, Ord, Eq)

instance Monoid Context where
  mempty =
    Context
      { assignments = mempty,
        constants = mempty,
        parents = [],
        currentLevel = 0
      }

instance Semigroup Context where
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
extend :: Context -> Context -> Context
extend = (<>)

allSegMap :: Context -> [SegOpName]
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
data CtxVal = CtxVal
  { deps :: Names,
    iterationType :: IterationType,
    level :: Int
  }
  deriving (Show, Ord, Eq)

ctxValFromNames :: Context -> Names -> CtxVal
ctxValFromNames ctx names =
  CtxVal
    names
    (getIterationType ctx)
    (currentLevel ctx)

-- | Wrapper around the constructur of Context.
oneContext :: VName -> CtxVal -> Context
oneContext name ctxValue =
  Context
    { assignments = M.singleton name ctxValue,
      constants = mempty,
      parents = [],
      currentLevel = 0
    }

-- | Create a singular ctxVal with no dependencies.
ctxValZeroDeps :: Context -> IterationType -> CtxVal
ctxValZeroDeps ctx iterType =
  CtxVal
    { deps = mempty,
      iterationType = iterType,
      level = currentLevel ctx
    }

-- | Create a singular context from a segspace
contextFromNames :: Context -> IterationType -> [VName] -> Context
contextFromNames ctx itertype =
  -- Create context from names in segspace
  foldl' extend ctx
    . map (`oneContext` ctxValZeroDeps ctx itertype)

-- | Analyze each `entry` and accumulate the results.
analyzeDimIdxPats :: (Analyze rep) => Prog rep -> IndexTable
analyzeDimIdxPats = foldMap' analyzeFunction . progFuns

-- | Analyze each statement in a function body.
analyzeFunction :: (Analyze rep) => FunDef rep -> IndexTable
analyzeFunction func = do
  let stms = stmsToList . bodyStms $ funDefBody func
  -- \| Create a context from a list of parameters
  let ctx = contextFromNames mempty Sequential $ map paramName $ funDefParams func
  snd $ analyzeStmsPrimitive ctx stms

-- | Analyze each statement in a list of statements.
analyzeStmsPrimitive :: (Analyze rep) => Context -> [Stm rep] -> (Context, IndexTable)
analyzeStmsPrimitive ctx =
  -- Fold over statements in body
  foldl'
    (\(c, r) stm -> onSnd (unionIndexTables r) $ analyzeStm c stm)
    (ctx, mempty)

-- | Same as analyzeStmsPrimitive, but change the resulting context into
-- a ctxVal, mapped to pattern.
analyzeStms :: (Analyze rep) => Context -> Context -> ((Int, VName) -> BodyType) -> VName -> [Stm rep] -> (Context, IndexTable)
analyzeStms ctx tmp_ctx bodyConstructor pat body = do
  -- 0. Recurse into body with ctx
  let (ctx'', aids) = analyzeStmsPrimitive recContext body
  -- 1. We do not want the returned context directly.
  --    however, we do want pat to map to the names what was hit in body.
  --    therefore we need to subtract the old context from the returned one,
  --    and discard all the keys within it.
  let ctxVals = M.difference (assignments ctx'') (assignments recContext)
  -- 2. We are ONLY interested in the rhs of assignments (ie. the
  --    dependencies of pat :) )
  let ctx' = extend ctx . concatCtxVal . map snd $ M.toList ctxVals
  -- 3. Now we have the correct context and result
  (ctx', aids)
  where
    -- Extracts and merges `Names` in `CtxVal`s, and makes a new CtxVal. This
    -- MAY throw away needed information, but it was my best guess at a solution
    -- at the time of writing.
    concatCtxVal cvals =
      oneContext pat (ctxValFromNames ctx $ foldl' (<>) mempty $ map deps cvals)

    -- Context used for "recursion" into analyzeStmsPrimitive
    recContext =
      extend ctx $
        tmp_ctx
          { parents = [bodyConstructor (currentLevel ctx, pat)],
            currentLevel = currentLevel ctx + 1
          }

getDeps :: SubExp -> Names
getDeps subexp =
  case subexp of
    (Var v) -> oneName v
    (Constant _) -> mempty

-- | Analyze a rep statement and return the updated context and array index
-- descriptors.
analyzeStm :: (Analyze rep) => Context -> Stm rep -> (Context, IndexTable)
analyzeStm ctx (Let pats _ e) = do
  -- Get the name of the first element in a pattern
  let patternName = patElemName . head $ patElems pats
  -- Construct the result and Context from the subexpression. If the subexpression
  -- is a body, we recurse into it.
  case e of
    (BasicOp (Index name (Slice ee))) -> analyzeIndex ctx patternName name ee
    (BasicOp (Update _ name (Slice subexp) _subexp)) -> analyzeIndex ctx patternName name subexp
    (BasicOp op) -> analyzeBasicOp ctx op patternName
    (Match _ cases defaultBody _) -> analyzeMatch ctx patternName defaultBody $ map caseBody cases
    (Loop bindings loop body) -> analyzeLoop ctx bindings loop body patternName
    (Apply _name diets _ _) -> analyzeApply ctx patternName diets
    (WithAcc _ _) -> (ctx, mempty) -- ignored
    (Op op) -> analyzeOp op ctx patternName

getIndexDependencies :: Context -> [DimIndex SubExp] -> Maybe [DimIdxPat]
getIndexDependencies _ [_] = Nothing
getIndexDependencies ctx dims =
  foldl' (\a idx -> a >>= matchDimIndex idx) (Just []) $ reverse dims
  where
    matchDimIndex idx accumulator =
      case idx of
        (DimFix subExpression) ->
          Just $ consolidate ctx subExpression : accumulator
        _ -> Nothing

analyzeIndex :: Context -> VName -> VName -> [DimIndex SubExp] -> (Context, IndexTable)
analyzeIndex ctx pat arr_name dimIndexes = do
  -- TODO: Should we just take the latest segmap?
  let dimindices = getIndexDependencies ctx dimIndexes
  maybe
    (ctx, mempty)
    (analyzeIndex' (analyzeIndexContextFromIndices ctx dimIndexes pat) pat arr_name)
    dimindices

analyzeIndexContextFromIndices :: Context -> [DimIndex SubExp] -> VName -> Context
analyzeIndexContextFromIndices ctx dimIndexes pat = do
  let (subExprs, consts) =
        partitionEithers $
          mapMaybe
            ( \case
                (DimFix subExpression) -> case subExpression of
                  (Var v) -> Just (Left v)
                  (Constant _) -> Just (Right pat)
                (DimSlice _offs _n _stride) -> Nothing
            )
            dimIndexes

  -- Add each non-constant DimIndex as a dependency to the index expression
  let ctxVal = ctxValFromNames ctx $ namesFromList subExprs

  -- Add each constant DimIndex to the context
  -- Extend context with the dependencies and constants index expression
  extend ctx $ (oneContext pat ctxVal) {constants = namesFromList consts}

analyzeIndex' :: Context -> VName -> VName -> [DimIdxPat] -> (Context, IndexTable)
analyzeIndex' ctx pat arr_name dimIndexes = do
  let segmaps = allSegMap ctx
  let memory_entries = MemoryEntry dimIndexes $ parents ctx
  let idx_expr_name = pat --                                         IndexExprName
  let map_ixd_expr = M.singleton idx_expr_name memory_entries --     IndexExprName |-> MemoryEntry
  let map_array = M.singleton arr_name map_ixd_expr -- ArrayName |-> IndexExprName |-> MemoryEntry
  let res = foldl' unionIndexTables mempty $ map (`M.singleton` map_array) segmaps

  (ctx, res)

analyzeBasicOp :: Context -> BasicOp -> VName -> (Context, IndexTable)
analyzeBasicOp ctx expression pat = do
  -- Construct a CtxVal from the subexpressions
  let ctx_val = case expression of
        (SubExp subexp) -> ctxValFromNames ctx $ analyzeSubExpr pat ctx subexp
        (Opaque _ subexp) -> ctxValFromNames ctx $ analyzeSubExpr pat ctx subexp
        (ArrayLit subexps _t) -> concatCtxVals mempty subexps
        (UnOp _ subexp) -> ctxValFromNames ctx $ analyzeSubExpr pat ctx subexp
        (BinOp _ lsubexp rsubexp) -> concatCtxVals mempty [lsubexp, rsubexp]
        (CmpOp _ lsubexp rsubexp) -> concatCtxVals mempty [lsubexp, rsubexp]
        (ConvOp _ subexp) -> ctxValFromNames ctx $ analyzeSubExpr pat ctx subexp
        (Assert subexp _ _) -> ctxValFromNames ctx $ analyzeSubExpr pat ctx subexp
        (Index name _) ->
          error $ "unhandled: Index (This should NEVER happen) into " ++ prettyString name
        (Update _ name _slice _subexp) ->
          error $ "unhandled: Update (This should NEVER happen) onto " ++ prettyString name
        -- Technically, do we need this case?
        (Concat _ _ length_subexp) -> ctxValFromNames ctx $ analyzeSubExpr pat ctx length_subexp
        (Manifest _dim name) -> error $ "unhandled: Manifest for " ++ prettyString name
        (Iota end start stride _) -> concatCtxVals mempty [end, start, stride]
        (Replicate (Shape shape) value') -> concatCtxVals mempty (value' : shape)
        (Scratch _ subexprs) -> concatCtxVals mempty subexprs
        (Reshape _ (Shape shape_subexp) name) -> concatCtxVals (oneName name) shape_subexp
        (Rearrange _ name) -> ctxValFromNames ctx $ oneName name
        (UpdateAcc name lsubexprs rsubexprs) -> concatCtxVals (oneName name) (lsubexprs ++ rsubexprs)
        _ -> error "unhandled: match-all"
  let ctx' = extend ctx $ oneContext pat ctx_val
  (ctx', mempty)
  where
    concatCtxVals ne nn =
      ctxValFromNames
        ctx
        (foldl' (\a -> (<>) a . analyzeSubExpr pat ctx) ne nn)

analyzeMatch :: (Analyze rep) => Context -> VName -> Body rep -> [Body rep] -> (Context, IndexTable)
analyzeMatch ctx pat body bodies =
  let ctx'' = ctx {currentLevel = currentLevel ctx - 1}
   in foldl'
        ( \(ctx', res) b ->
            -- This Little Maneuver's Gonna Cost Us 51 Years
            onFst constLevel
              . onSnd (unionIndexTables res)
              . analyzeStms ctx'' ctx' CondBodyName pat
              . stmsToList
              $ bodyStms b
        )
        (ctx'', mempty)
        (body : bodies)
  where
    constLevel context = context {currentLevel = currentLevel ctx - 1}

analyzeLoop :: (Analyze rep) => Context -> [(FParam rep, SubExp)] -> LoopForm rep -> Body rep -> VName -> (Context, IndexTable)
analyzeLoop ctx bindings loop body pat = do
  let nextLevel = currentLevel ctx
  let ctx'' = ctx {currentLevel = nextLevel}
  let ctx' =
        contextFromNames ctx'' Sequential $
          case loop of
            (WhileLoop iterVar) -> iterVar : map (paramName . fst) bindings
            (ForLoop iterVar _ _ params) ->
              iterVar : map (paramName . fst) bindings ++ map (paramName . fst) params

  -- Extend context with the loop expression
  analyzeStms ctx ctx' LoopBodyName pat $ stmsToList $ bodyStms body

analyzeApply :: Context -> VName -> [(SubExp, Diet)] -> (Context, IndexTable)
analyzeApply ctx pat diets =
  onFst
        ( \ctx' ->
            extend ctx' $ oneContext pat $ ctxValFromNames ctx' $ foldl' (<>) mempty $ map (getDeps . fst) diets
        )
        (ctx, mempty)

segOpType :: SegOp lvl rep -> (Int, VName) -> SegOpName
segOpType (SegMap {}) = SegmentedMap
segOpType (SegRed {}) = SegmentedRed
segOpType (SegScan {}) = SegmentedScan
segOpType (SegHist {}) = SegmentedHist

analyzeSegOp :: (Analyze rep) => SegOp lvl rep -> Context -> VName -> (Context, IndexTable)
analyzeSegOp op ctx pat = do
  let nextLevel = currentLevel ctx + length (unSegSpace $ segSpace op) - 1
  let ctx' = ctx {currentLevel = nextLevel}
  let segSpaceContext =
        foldl' extend ctx'
          . map (\(n, i) -> n `oneContext` CtxVal mempty Parallel (currentLevel ctx + i))
          . (\segspaceParams -> zip segspaceParams [0 ..])
          . map fst
          . unSegSpace
          $ segSpace op
  -- Analyze statements in the SegOp body
  analyzeStms ctx' segSpaceContext (SegOpName . segOpType op) pat . stmsToList . kernelBodyStms $ segBody op

analyzeSizeOp :: SizeOp -> Context -> VName -> (Context, IndexTable)
analyzeSizeOp op ctx pat = do
  let subexprsToContext =
        contextFromNames ctx Sequential
          . concatMap (namesToList . analyzeSubExpr pat ctx)
  let ctx' = case op of
        (CmpSizeLe _name _class subexp) -> subexprsToContext [subexp]
        (CalcNumGroups lsubexp _name rsubexp) -> subexprsToContext [lsubexp, rsubexp]
        _ -> ctx
  -- Add sizeOp to context
  let ctx'' = extend ctx' $ oneContext pat $ ctxValZeroDeps ctx Sequential
  (ctx'', mempty)

-- | Analyze statements in a rep body.
analyzeGPUBody :: (Analyze rep) => Body rep -> Context -> (Context, IndexTable)
analyzeGPUBody body ctx =
  analyzeStmsPrimitive ctx $ stmsToList $ bodyStms body

analyzeOtherOp :: Context -> VName -> (Context, IndexTable)
analyzeOtherOp _ctx _pat = error "UNHANDLED: OtherOp"

-- | Get the iteration type of the last SegOp encountered in the context.
getIterationType :: Context -> IterationType
getIterationType (Context _ _ bodies _) =
  getIteration_rec bodies
  where
    getIteration_rec [] = Sequential
    getIteration_rec rec =
      case last rec of
        SegOpName (SegmentedMap _) -> Parallel
        SegOpName (SegmentedRed _) -> Parallel
        SegOpName (SegmentedScan _) -> Parallel
        SegOpName (SegmentedHist _) -> Parallel
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
analyzeSubExpr :: VName -> Context -> SubExp -> Names
analyzeSubExpr _ _ (Constant _) = mempty
analyzeSubExpr pp ctx (Var v) =
  case M.lookup v (assignments ctx) of
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
consolidate :: Context -> SubExp -> DimIdxPat
consolidate _ (Constant _) = mempty
consolidate ctx (Var v) = reduceDependencies ctx v

-- | Recursively lookup vnames until vars with no deps are reached.
reduceDependencies :: Context -> VName -> DimIdxPat
reduceDependencies ctx v =
  if v `nameIn` constants ctx
    then mempty -- If v is a constant, then it is not a dependency
    else case M.lookup v (assignments ctx) of
      Nothing -> error $ "Unable to find " ++ prettyString v
      Just (CtxVal deps itertype lvl) ->
        if null $ namesToList deps
          then DimIdxPat $ S.fromList [(baseTag v, (v, lvl, itertype))]
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

instance Analyze GPUMem where analyzeOp _ = error $ notImplementedYet "GPUMem"

instance Analyze MC where
  analyzeOp mcOp
    | ParOp Nothing seqSegop <- mcOp = analyzeSegOp seqSegop
    | ParOp (Just segop) seqSegop <- mcOp = \ctx name -> do
        let (ctx', res') = analyzeSegOp segop ctx name
        let (ctx'', res'') = analyzeSegOp seqSegop ctx name
        (ctx' <> ctx'', unionIndexTables res' res'')
    | Futhark.IR.MC.OtherOp _ <- mcOp = analyzeOtherOp

instance Analyze MCMem where analyzeOp _ = error $ notImplementedYet "MCMem"

instance Analyze Seq where analyzeOp _ = error $ notImplementedYet "Seq"

instance Analyze SeqMem where analyzeOp _ = error $ notImplementedYet "SeqMem"

instance Analyze SOACS where analyzeOp _ = error $ notImplementedYet "SOACS"

notImplementedYet :: String -> String
notImplementedYet s = "Access pattern analysis for the " ++ s ++ " backend is not implemented."
