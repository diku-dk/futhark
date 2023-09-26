{-# LANGUAGE LambdaCase #-}

module Futhark.Analysis.AccessPattern
  ( analyzeDimIdxPats,
    analyzeFunction,
    ArrayIndexDescriptors,
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
import Futhark.IR.GPU

-- | Map patterns of Segmented operations on arrays, to index expressions with
-- their index descriptors.
-- segmap(pattern) → A(pattern) → indexExpressionName(pattern) → [DimIdxPat]
type ArrayIndexDescriptors =
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
  deriving (Eq, Ord, Show)

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

unionArrayIndexDescriptors :: ArrayIndexDescriptors -> ArrayIndexDescriptors -> ArrayIndexDescriptors
unionArrayIndexDescriptors lhs rhs = do
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
analyzeDimIdxPats :: Prog GPU -> ArrayIndexDescriptors
analyzeDimIdxPats = foldMap' analyzeFunction . progFuns

-- | Analyze each statement in a function body.
analyzeFunction :: FunDef GPU -> ArrayIndexDescriptors
analyzeFunction func = do
  let stms = stmsToList . bodyStms $ funDefBody func
  -- \| Create a context from a list of parameters
  let ctx = contextFromNames mempty Sequential $ map paramName $ funDefParams func
  snd $ analyzeStmsPrimitive ctx stms

-- | Analyze each statement in a list of statements.
analyzeStmsPrimitive :: Context -> [Stm GPU] -> (Context, ArrayIndexDescriptors)
analyzeStmsPrimitive ctx =
  -- Fold over statements in body
  foldl'
    (\(c, r) stm -> onSnd (unionArrayIndexDescriptors r) $ analyzeStm c stm)
    (ctx, mempty)

-- | Same as analyzeStmsPrimitive, but change the resulting context into
-- a ctxVal, mapped to pattern.
analyzeStms :: Context -> Context -> ((Int, VName) -> BodyType) -> VName -> [Stm GPU] -> (Context, ArrayIndexDescriptors)
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

-- | Analyze a GPU statement and return the updated context and array index
-- descriptors.
analyzeStm :: Context -> Stm GPU -> (Context, ArrayIndexDescriptors)
analyzeStm ctx (Let pats _ e) = do
  -- Get the name of the first element in a pattern
  let patternName = patElemName . head $ patElems pats
  -- Construct the result and Context from the subexpression. If the subexpression
  -- is a body, we recurse into it.
  case e of
    (BasicOp (Index name (Slice ee))) -> analyzeIndex ctx patternName name ee
    (BasicOp op) -> analyzeBasicOp ctx op patternName
    (Match _ cases defaultBody _) -> analyzeMatch ctx patternName defaultBody $ map caseBody cases
    (Loop bindings loop body) -> analyzeLoop ctx bindings loop body patternName
    (Apply _name _ _ _) -> analyzeApply ctx patternName
    (WithAcc _ _) -> analyzeWithAcc ctx patternName
    (Op (SegOp op)) -> analyzeSegOp ctx op patternName
    (Op (SizeOp op)) -> analyzeSizeOp ctx op patternName
    (Op (GPUBody _ body)) -> analyzeGPUBody ctx body
    (Op (OtherOp _)) -> analyzeOtherOp ctx patternName

analyzeIndex :: Context -> VName -> VName -> [DimIndex SubExp] -> (Context, ArrayIndexDescriptors)
analyzeIndex ctx pat arr_name dimIndexes = do
  -- TODO: Should we just take the latest segmap?
  let segmaps = allSegMap ctx
  let memory_entries = MemoryEntry (map f dimIndexes) (parents ctx)
        where
          f dimIndex = case dimIndex of
            (DimFix subExpression) -> consolidate ctx subExpression
            (DimSlice _offs _n _stride) -> mempty
  let idx_expr_name = pat --                                         IndexExprName
  let map_ixd_expr = M.singleton idx_expr_name memory_entries --     IndexExprName |-> MemoryEntry
  let map_array = M.singleton arr_name map_ixd_expr -- ArrayName |-> IndexExprName |-> MemoryEntry
  let res = foldl' unionArrayIndexDescriptors mempty $ map (`M.singleton` map_array) segmaps

  -- Partition the DimIndexes into constants and non-constants
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
  let ctx' = extend ctx $ (oneContext pat ctxVal) {constants = namesFromList consts}
  (ctx', res)

analyzeBasicOp :: Context -> BasicOp -> VName -> (Context, ArrayIndexDescriptors)
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
          error $ "unhandled: Index (Skill issue?) " ++ baseString name
        (Update _ name _slice _subexp) ->
          error $ "unhandled: Update (technically skill issue?)" ++ baseString name
        -- Technically, do we need this case?
        (Concat _ _ length_subexp) -> ctxValFromNames ctx $ analyzeSubExpr pat ctx length_subexp
        (Manifest _dim _name) -> error "unhandled: Manifest"
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

analyzeMatch :: Context -> VName -> Body GPU -> [Body GPU] -> (Context, ArrayIndexDescriptors)
analyzeMatch ctx pat body bodies =
  let ctx' = ctx {currentLevel = currentLevel ctx - 1}
   in foldl'
        ( \(ctx'', res) b ->
            onSnd (unionArrayIndexDescriptors res)
              . analyzeStms ctx' ctx'' CondBodyName pat
              . stmsToList
              $ bodyStms b
        )
        (ctx', mempty)
        (body : bodies)

analyzeLoop :: Context -> [(FParam GPU, SubExp)] -> LoopForm GPU -> Body GPU -> VName -> (Context, ArrayIndexDescriptors)
analyzeLoop ctx bindings loop body pat = do
  let ctx' =
        contextFromNames ctx Sequential $
          case loop of
            (WhileLoop iterVar) -> iterVar : map (paramName . fst) bindings
            (ForLoop iterVar _ _ params) ->
              iterVar : map (paramName . fst) bindings ++ map (paramName . fst) params

  -- Extend context with the loop expression
  analyzeStms ctx ctx' LoopBodyName pat $ stmsToList $ bodyStms body

analyzeApply :: Context -> VName -> (Context, ArrayIndexDescriptors)
analyzeApply _ctx _pat = error "UNHANDLED: Apply"

analyzeWithAcc :: Context -> VName -> (Context, ArrayIndexDescriptors)
analyzeWithAcc _ctx _pat = error "UNHANDLED: WithAcc"

segOpType :: SegOp lvl GPU -> (Int, VName) -> SegOpName
segOpType (SegMap {}) = SegmentedMap
segOpType (SegRed {}) = SegmentedRed
segOpType (SegScan {}) = SegmentedScan
segOpType (SegHist {}) = SegmentedHist

analyzeSegOp :: Context -> SegOp lvl GPU -> VName -> (Context, ArrayIndexDescriptors)
analyzeSegOp ctx op pat = do
  let segSpaceContext =
        extend ctx
          . contextFromNames ctx Parallel
          . map fst
          . unSegSpace
          $ segSpace op
  -- Analyze statements in the SegOp body
  analyzeStms ctx segSpaceContext (SegOpName . segOpType op) pat . stmsToList . kernelBodyStms $ segBody op

analyzeSizeOp :: Context -> SizeOp -> VName -> (Context, ArrayIndexDescriptors)
analyzeSizeOp ctx op pat = do
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

-- | Analyze statements in a GPU body.
analyzeGPUBody :: Context -> Body GPU -> (Context, ArrayIndexDescriptors)
analyzeGPUBody ctx body =
  analyzeStmsPrimitive ctx $ stmsToList $ bodyStms body

analyzeOtherOp :: Context -> VName -> (Context, ArrayIndexDescriptors)
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
      Nothing -> error $ "Unable to find " ++ baseString v
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
