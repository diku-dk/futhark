{-# LANGUAGE LambdaCase #-}

module Futhark.Analysis.AccessPattern
  ( analyzeDimAccesss,
    analyzeFunction,
    vnameFromSegOp,
    analysisPropagateByTransitivity,
    Analyze,
    IndexTable,
    ArrayName,
    DimAccess (..),
    IndexExprName,
    IterationType (Sequential, Parallel),
    BodyType (..),
    SegOpName (SegmentedMap, SegmentedRed, SegmentedScan, SegmentedHist),
    notImplementedYet,
    Complexity (..),
    Context (..),
    analyzeIndex,
    CtxVal (..),
  )
where

import Data.Bifunctor
import Data.Either
import Data.Foldable
import Data.IntMap.Strict qualified as S
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Debug.Pretty.Simple
import Futhark.IR.GPU
import Futhark.IR.GPUMem
import Futhark.IR.MC
import Futhark.IR.MCMem
import Futhark.IR.SOACS
import Futhark.IR.Seq
import Futhark.IR.SeqMem
import Futhark.Util
import Futhark.Util.Pretty

class Analyze rep where
  analyzeOp :: Op rep -> (Context rep -> [VName] -> (Context rep, IndexTable rep))

-- | Map patterns of Segmented operations on arrays, to index expressions with
-- their index descriptors.
-- segmap(pattern) → A(pattern) → indexExpressionName(pattern) → [DimAccess]
-- Each DimAccess element corresponds to an access to a given dimension
-- in the given array, in the same order of the dimensions.
type IndexTable rep =
  M.Map SegOpName (M.Map ArrayName (M.Map IndexExprName [DimAccess rep]))

-- | SegOpName stores the nested "level" at which it is declared in the AST.
data SegOpName
  = SegmentedMap {vnameFromSegOp :: VName}
  | SegmentedRed {vnameFromSegOp :: VName}
  | SegmentedScan {vnameFromSegOp :: VName}
  | SegmentedHist {vnameFromSegOp :: VName}
  deriving (Eq, Ord, Show)

type IndexExprName = VName

type ArrayName = (VName, [BodyType])

data BodyType
  = SegOpName SegOpName
  | LoopBodyName VName
  | CondBodyName VName
  deriving (Show, Ord, Eq)

-- | Collect all features of access to a specific dimension of an array.
data DimAccess rep = DimAccess
  { -- | Set of VNames of gtid's that some access is variant to.
    -- An empty set indicates that the access is invariant.
    -- Tuple of patternName and nested `level` it is created at.
    dependencies :: S.IntMap (VName, Int, IterationType rep),
    originalDimension :: Int,
    complexity :: Complexity
  }
  deriving (Eq, Show)

instance Semigroup (DimAccess rep) where
  (<>) :: DimAccess rep -> DimAccess rep -> DimAccess rep
  adeps <> bdeps =
    DimAccess
      (dependencies adeps <> dependencies bdeps)
      (max (originalDimension adeps) (originalDimension bdeps))
      (max (complexity adeps) (complexity bdeps))

instance Monoid (DimAccess rep) where
  mempty = DimAccess mempty 0 Simple

-- | Iteration type describes whether the index is iterated in a parallel or
-- sequential way, ie. if the index expression comes from a sequential or
-- parallel construct, like foldl or map.
data IterationType rep = Sequential | Parallel
  deriving (Eq, Show)

-- k | v | linear (add/sub) | mul | exp | indirect

-- | The complexity of an expression. Used to determine whether to manifest
-- an array based on its index expressions. If an array is indexed by a
-- constant, a thread ID or a loop counter, then it is `Simple`. If it is
-- indexed by a linear combination of constants, thread IDs or loop counters,
-- then it is `Linear`. If it is indexed by anything else, then it is probably
-- too complex to reason about (inscrutable), so we give up on manifesting it.
data Complexity = Simple | Linear [VName] Int Int | Inscrutable
  deriving (Eq, Show, Ord)

unionIndexTables :: IndexTable rep -> IndexTable rep -> IndexTable rep
unionIndexTables = M.unionWith (M.unionWith M.union)

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
    -- VName -> M.Map ArrayName (M.Map IndexExprName ([DimAccess rep]))
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
    assignments :: M.Map VName (CtxVal rep),
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
allSegMap (Context _ _ parents _) =
  mapMaybe
    ( \case
        (SegOpName o) -> Just o
        _ -> Nothing
    )
    parents

-- | Context Value (CtxVal) is the type used in the context to categorize
-- assignments. For example, a pattern might depend on a function parameter, a
-- gtid, or some other pattern.
data CtxVal rep = CtxVal
  { deps :: Names,
    iterationType :: IterationType rep,
    level :: Int,
    parents_nest :: [BodyType],
    complexity_ctx :: Complexity
  }
  deriving (Show, Eq)

ctxValFromNames :: Context rep -> Names -> CtxVal rep
ctxValFromNames ctx names = do
  let (keys, ctxvals) = unzip $ M.toList $ assignments ctx
  let complexities = mapMaybe (\n -> M.lookup n $ M.fromList (zip keys (map complexity_ctx ctxvals))) (namesToList names)
  let complexity = foldl max Simple complexities
  CtxVal
    names
    (getIterationType ctx)
    (currentLevel ctx)
    (parents ctx)
    complexity

-- | Wrapper around the constructur of Context.
oneContext :: VName -> CtxVal rep -> Context rep
oneContext name ctxValue =
  Context
    { assignments = M.singleton name ctxValue,
      constants = mempty,
      parents = [],
      currentLevel = 0
    }

-- | Create a singular ctxVal with no dependencies.
ctxValZeroDeps :: Context rep -> IterationType rep -> CtxVal rep
ctxValZeroDeps ctx iterType =
  CtxVal
    mempty
    iterType
    (currentLevel ctx)
    (parents ctx)
    Simple

-- | Create a singular context from a segspace
contextFromNames :: Context rep -> IterationType rep -> [VName] -> Context rep
contextFromNames ctx itertype =
  -- Create context from names in segspace
  foldl' extend ctx
    . map (\n -> oneContext n (ctxValZeroDeps ctx itertype))

--  . zipWith
--    ( \i n ->
--        n
--          `oneContext` ctxValZeroDeps (ctx {currentLevel = currentLevel ctx + i}) itertype
--    )
--    [0 ..]

-- | Analyze each `entry` and accumulate the results.
analyzeDimAccesss :: (Analyze rep) => Prog rep -> IndexTable rep
analyzeDimAccesss = foldMap' analyzeFunction . progFuns

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
analyzeStms :: (Analyze rep) => Context rep -> (VName -> BodyType) -> [VName] -> [Stm rep] -> (Context rep, IndexTable rep)
analyzeStms ctx bodyConstructor pats body = do
  -- 0. Recurse into body with ctx
  let (ctx'', indexTable) = analyzeStmsPrimitive recContext body
  -- 1. We do not want the returned context directly.
  --    however, we do want pat to map to the names what was hit in body.
  --    therefore we need to subtract the old context from the returned one,
  --    and discard all the keys within it.

  -- assignments :: M.Map VName (CtxVal rep),
  let inScopeDependenciesFromBody =
        rmOutOfScopeDeps ctx'' $
          M.difference (assignments ctx'') (assignments recContext)
  -- 2. We are ONLY interested in the rhs of assignments (ie. the
  --    dependencies of pat :) )
  let ctx' = foldl extend ctx $ concatCtxVal inScopeDependenciesFromBody -- . map snd $ M.toList ctxVals
  -- 3. Now we have the correct context and result
  (ctx' {parents = parents ctx, currentLevel = currentLevel ctx}, indexTable)
  where
    -- Extracts and merges `Names` in `CtxVal`s, and makes a new CtxVal. This
    -- MAY throw away needed information, but it was my best guess at a solution
    -- at the time of writing.
    concatCtxVal dependencies =
      map (\pat -> oneContext pat (ctxValFromNames ctx dependencies)) pats

    -- Context used for "recursion" into analyzeStmsPrimitive
    recContext =
      ctx
        { parents = parents ctx <> concatMap (\pat -> [bodyConstructor pat]) pats,
          currentLevel = currentLevel ctx + 1
        }

    -- Recursively looks up dependencies, until they're in scope or empty set.
    rmOutOfScopeDeps :: Context rep -> M.Map VName (CtxVal rep) -> Names
    rmOutOfScopeDeps ctx' newAssignments = do
      let throwawayAssignments = assignments ctx'
      let localAssignments = assignments ctx
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

getIndexDependencies :: Context rep -> [DimIndex SubExp] -> Maybe [DimAccess rep]
getIndexDependencies _ [] = Nothing
getIndexDependencies ctx dims =
  fst
    . foldl' (\(a, i) idx -> (a >>= matchDimIndex idx i, i - 1)) (Just [], length dims - 1)
    $ reverse dims
  where
    matchDimIndex idx i accumulator =
      case idx of
        (DimFix subExpression) ->
          Just $ (consolidate ctx subExpression) {originalDimension = i} : accumulator
        _ -> Nothing

analyzeIndex :: Context rep -> [VName] -> VName -> [DimIndex SubExp] -> (Context rep, IndexTable rep)
analyzeIndex ctx pats arr_name dimIndexes = do
  let dependencies = getIndexDependencies ctx dimIndexes
  let ctx' = analyzeIndexContextFromIndices ctx dimIndexes pats
  -- FIXME: Some variables are not added properly to the scope, and will cause
  -- this to return (arr_name, []), we should throw an error instead and fix the
  -- context with the missing variable.
  -- For now it works fine tho.

  -- TODO: document this and the reason for [BodyType] in ArrayName
  let array_name' =
        fromMaybe (arr_name, []) $
          L.find (\(n, _) -> n == arr_name) $
            map (second parents_nest) (M.toList $ assignments ctx')

  maybe
    (ctx', mempty)
    (analyzeIndex' ctx' pats array_name')
    dependencies

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
  -- The pattern will have inextricable complexity.
  let ctxVal' = ctxVal {complexity_ctx = Inscrutable}

  -- Add each constant DimIndex to the context
  -- Extend context with the dependencies and constants index expression
  foldl' extend ctx $ map (\pat -> (oneContext pat ctxVal') {constants = namesFromList $ concat consts}) pats

analyzeIndex' :: Context rep -> [VName] -> (VName, [BodyType]) -> [DimAccess rep] -> (Context rep, IndexTable rep)
analyzeIndex' ctx _ _ [_] = (ctx, mempty)
analyzeIndex' ctx pats arr_name dimIndexes = do
  let segmaps = allSegMap ctx
  let memory_entries = dimIndexes
  let idx_expr_name = pats --                                                IndexExprName
  let map_ixd_expr = map (`M.singleton` memory_entries) idx_expr_name --     IndexExprName |-> [DimAccess]
  let map_array = map (M.singleton arr_name) map_ixd_expr --   ArrayName |-> IndexExprName |-> [DimAccess]
  let results = concatMap (\ma -> map (`M.singleton` ma) segmaps) map_array
  let res = foldl' unionIndexTables mempty results
  (ctx, res)

analyzeBasicOp :: Context rep -> BasicOp -> [VName] -> (Context rep, IndexTable rep)
analyzeBasicOp ctx expression pats = do
  -- Construct a CtxVal from the subexpressions
  let ctx_val = case expression of
        (SubExp subexp) -> (ctxValFromNames ctx $ analyzeSubExpr pats ctx subexp) {complexity_ctx = Simple}
        (Opaque _ subexp) -> (ctxValFromNames ctx $ analyzeSubExpr pats ctx subexp) {complexity_ctx = Inscrutable}
        (ArrayLit subexps _t) -> (concatCtxVals mempty subexps) {complexity_ctx = Inscrutable}
        (UnOp _ subexp) -> (ctxValFromNames ctx $ analyzeSubExpr pats ctx subexp) {complexity_ctx = Inscrutable}
        (BinOp t lsubexp rsubexp) -> analyzeBinOp t lsubexp rsubexp
        (CmpOp _ lsubexp rsubexp) -> (concatCtxVals mempty [lsubexp, rsubexp]) {complexity_ctx = Inscrutable}
        (ConvOp _ subexp) -> (ctxValFromNames ctx $ analyzeSubExpr pats ctx subexp) {complexity_ctx = Inscrutable}
        (Assert subexp _ _) -> (ctxValFromNames ctx $ analyzeSubExpr pats ctx subexp) {complexity_ctx = Inscrutable}
        (Index name _) ->
          error $ "unhandled: Index (This should NEVER happen) into " ++ prettyString name
        (Update _ name _slice _subexp) ->
          error $ "unhandled: Update (This should NEVER happen) onto " ++ prettyString name
        -- Technically, do we need this case?
        (Concat _ _ length_subexp) -> (ctxValFromNames ctx $ analyzeSubExpr pats ctx length_subexp) {complexity_ctx = Inscrutable}
        (Manifest _dim name) -> (ctxValFromNames ctx $ oneName name) {complexity_ctx = Inscrutable}
        (Iota end start stride _) -> (concatCtxVals mempty [end, start, stride]) {complexity_ctx = Inscrutable}
        (Replicate (Shape shape) value') -> (concatCtxVals mempty (value' : shape)) {complexity_ctx = Inscrutable}
        (Scratch _ subexprs) -> (concatCtxVals mempty subexprs) {complexity_ctx = Inscrutable}
        (Reshape _ (Shape shape_subexp) name) -> (concatCtxVals (oneName name) shape_subexp) {complexity_ctx = Inscrutable}
        (Rearrange _ name) -> (ctxValFromNames ctx $ oneName name) {complexity_ctx = Inscrutable}
        (UpdateAcc name lsubexprs rsubexprs) -> (concatCtxVals (oneName name) (lsubexprs ++ rsubexprs)) {complexity_ctx = Inscrutable}
        (FlatIndex name _) -> (ctxValFromNames ctx $ oneName name) {complexity_ctx = Inscrutable}
        (FlatUpdate name _ source) -> (ctxValFromNames ctx $ namesFromList [name, source]) {complexity_ctx = Inscrutable}
  let ctx' = foldl' extend ctx $ map (`oneContext` ctx_val) pats
  (ctx', mempty)
  where
    concatCtxVals ne nn =
      ctxValFromNames
        ctx
        (foldl' (\a -> (<>) a . analyzeSubExpr pats ctx) ne nn)

    analyzeBinOp t lsubexp rsubexp = do
      let lcomplexity = getComplexity lsubexp
      let rcomplexity = getComplexity rsubexp
      let reduced = [reduceConstants lsubexp, reduceConstants rsubexp]
      let complexity = case t of
            (Add _ _) -> case (lcomplexity, rcomplexity) of
              (Linear names s o, Simple) -> Linear (reduceNames $ names ++ rights reduced) s $ o + sum (lefts reduced)
              (Simple, Linear names s o) -> Linear (reduceNames $ names ++ rights reduced) s $ o + sum (lefts reduced)
              (Linear names0 0 o0, Linear names1 _ o1) -> Linear (reduceNames $ names0 ++ names1 ++ rights reduced) 0 $ o0 + o1 + sum (lefts reduced)
              (Linear names0 1 o0, Linear names1 s o1) -> Linear (reduceNames $ names0 ++ names1 ++ rights reduced) s $ o0 + o1 + sum (lefts reduced)
              (Linear names0 _ o0, Linear names1 0 o1) -> Linear (reduceNames $ names0 ++ names1 ++ rights reduced) 0 $ o0 + o1 + sum (lefts reduced)
              (Linear names0 s o0, Linear names1 1 o1) -> Linear (reduceNames $ names0 ++ names1 ++ rights reduced) s $ o0 + o1 + sum (lefts reduced)
              (Simple, Simple) -> case partitionEithers reduced of
                (consts, vars) -> Linear vars 1 (sum consts)
              _ -> Inscrutable -- TODO: Can we handle this case better?
            (Mul _ _) -> case (lcomplexity, rcomplexity) of
              (Linear names s o, Simple) -> Linear (reduceNames $ names ++ rights reduced) (s * product (lefts reduced)) o
              (Simple, Linear names s o) -> Linear (reduceNames $ names ++ rights reduced) (s * product (lefts reduced)) o
              (Simple, Simple) -> case partitionEithers reduced of
                (consts, vars) -> Linear vars (product consts) 0
              _ -> Inscrutable -- TODO: Can we handle this case better?
            _ -> Inscrutable
      (concatCtxVals mempty [lsubexp, rsubexp]) {complexity_ctx = complexity}

    reduceConstants (Constant (IntValue v)) = Left $ valueIntegral v
    reduceConstants (Var v) = Right v

    reduceNames :: [VName] -> [VName]
    reduceNames = nubOrd . concatMap (map (\(a, _, _) -> a) . S.elems . dependencies . reduceDependencies ctx)

    getComplexity (Constant _) = Simple
    getComplexity (Var v) = case M.lookup v (assignments ctx) of
      Nothing -> error "getComplexity: variable not found in context"
      Just assignment -> complexity_ctx assignment

analyzeMatch :: (Analyze rep) => Context rep -> [VName] -> Body rep -> [Body rep] -> (Context rep, IndexTable rep)
analyzeMatch ctx pats body parents =
  let ctx'' = ctx {currentLevel = currentLevel ctx - 1}
   in foldl
        ( \(ctx', res) b ->
            -- This Little Maneuver's Gonna Cost Us 51 Years
            onFst constLevel
              . onSnd (unionIndexTables res)
              . analyzeStms ctx' CondBodyName pats
              . stmsToList
              $ bodyStms b
        )
        (ctx'', mempty)
        (body : parents)
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
  analyzeStms ctx' LoopBodyName pats $ stmsToList $ bodyStms body

analyzeApply :: Context rep -> [VName] -> [(SubExp, Diet)] -> (Context rep, IndexTable rep)
analyzeApply ctx pats diets =
  onFst
    ( \ctx' ->
        foldl' extend ctx' $ map (\pat -> oneContext pat $ ctxValFromNames ctx' $ foldl' (<>) mempty $ map (getDeps . fst) diets) pats
    )
    (ctx, mempty)

segOpType :: SegOp lvl rep -> VName -> SegOpName
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
          . map (\(n, i) -> oneContext n $ CtxVal mempty Parallel (currentLevel ctx + i) (parents ctx') Simple)
          . (\segspaceParams -> zip segspaceParams [0 ..])
          -- contextFromNames ctx' Parallel
          . map fst
          . unSegSpace
          $ segSpace op
  -- Analyze statements in the SegOp body
  analyzeStms segSpaceContext (SegOpName . segOpType op) pats . stmsToList . kernelBodyStms $ segBody op

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
  let ctx'' = foldl' extend ctx' $ map (\pat -> oneContext pat $ (ctxValZeroDeps ctx Sequential) {parents_nest = parents ctx'}) pats
  (ctx'', mempty)

-- | Analyze statements in a rep body.
analyzeGPUBody :: (Analyze rep) => Body rep -> Context rep -> (Context rep, IndexTable rep)
analyzeGPUBody body ctx =
  analyzeStmsPrimitive ctx $ stmsToList $ bodyStms body

analyzeOtherOp :: Context rep -> [VName] -> (Context rep, IndexTable rep)
analyzeOtherOp ctx _ = (ctx, mempty)

-- | Get the iteration type of the last SegOp encountered in the context.
getIterationType :: Context rep -> IterationType rep
getIterationType (Context _ _ parents _) =
  getIteration_rec parents
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
consolidate :: Context rep -> SubExp -> DimAccess rep
consolidate _ (Constant _) = mempty
consolidate ctx (Var v) = reduceDependencies ctx v

-- | Recursively lookup vnames until vars with no deps are reached.
reduceDependencies :: Context rep -> VName -> DimAccess rep
reduceDependencies ctx v =
  if v `nameIn` constants ctx
    then mempty -- If v is a constant, then it is not a dependency
    else case M.lookup v (assignments ctx) of
      Nothing -> error $ "Unable to find " ++ prettyString v
      Just (CtxVal deps itertype lvl parents_nest complexity) ->
        if null $ namesToList deps
          then DimAccess (S.fromList [(baseTag v, (v, lvl, itertype))]) (currentLevel ctx) complexity
          else
            foldl' (<>) (mempty {complexity = complexity}) $
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
  analyzeOp mcOp = error "Unexpected?"

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

      mapprintArray :: [(ArrayName, M.Map IndexExprName [DimAccess rep])] -> Doc ann
      mapprintArray [] = ""
      mapprintArray [m] = printArrayMap m
      mapprintArray (m : mm) = printArrayMap m </> mapprintArray mm

      printArrayMap :: (ArrayName, M.Map IndexExprName [DimAccess rep]) -> Doc ann
      printArrayMap ((name, []), maps) =
        "(arr)"
          <+> pretty name
          <+> colon
          <+> lbrace
          </> indent 4 (mapprintIdxExpr (M.toList maps))
          </> rbrace
      printArrayMap (name, maps) =
        "(arr)"
          <+> pretty name
          <+> colon
          <+> lbrace
          </> indent 4 (mapprintIdxExpr (M.toList maps))
          </> rbrace

      mapprintIdxExpr :: [(IndexExprName, [DimAccess rep])] -> Doc ann
      mapprintIdxExpr [] = ""
      mapprintIdxExpr [m] = printIdxExpMap m
      mapprintIdxExpr (m : mm) = printIdxExpMap m </> mapprintIdxExpr mm

      printIdxExpMap (name, mems) = "(idx)" <+> pretty name <+> ":" </> indent 4 (printDimAccess mems)

      printDimAccess :: [DimAccess rep] -> Doc ann
      printDimAccess dimAccesses =
        stack $ map printDim dimAccesses

      printDim m = pretty (originalDimension m) <+> ":" <+> indent 0 (pretty m)

instance Pretty (DimAccess rep) where
  pretty dimidx =
    -- Instead of using `brackets $` we manually enclose with `[`s, to add
    -- spacing between the enclosed elements
    "dependencies" <+> equals <+> align (prettyDeps $ dependencies dimidx) <+> pretty (complexity dimidx)
    where
      prettyDeps = braces . commasep . map (printPair . snd) . S.toList
      printPair (name, lvl, itertype) = pretty name <+> pretty lvl <+> pretty itertype

instance Pretty SegOpName where
  pretty (SegmentedMap name) = "(segmap)" <+> pretty name
  pretty (SegmentedRed name) = "(segred)" <+> pretty name
  pretty (SegmentedScan name) = "(segscan)" <+> pretty name
  pretty (SegmentedHist name) = "(seghist)" <+> pretty name

instance Pretty BodyType where
  pretty (SegOpName (SegmentedMap name)) = pretty name <+> colon <+> "segmap"
  pretty (SegOpName (SegmentedRed name)) = pretty name <+> colon <+> "segred"
  pretty (SegOpName (SegmentedScan name)) = pretty name <+> colon <+> "segscan"
  pretty (SegOpName (SegmentedHist name)) = pretty name <+> colon <+> "seghist"
  pretty (LoopBodyName name) = pretty name <+> colon <+> "loop"
  pretty (CondBodyName name) = pretty name <+> colon <+> "cond"

instance Pretty (IterationType rep) where
  pretty Sequential = "seq"
  pretty Parallel = "par"

instance Pretty Complexity where
  pretty Simple = "ez"
  pretty (Linear names stride offset) = "stride" <+> pretty names <+> pretty stride <+> "offset" <+> pretty offset
  pretty Inscrutable = "idk"
