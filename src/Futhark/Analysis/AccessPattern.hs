{-# LANGUAGE LambdaCase #-}

module Futhark.Analysis.AccessPattern
  ( analyzeDimAccesss,
    analyzeFunction,
    vnameFromSegOp,
    analysisPropagateByTransitivity,
    isInvariant,
    Analyze,
    IndexTable,
    ArrayName,
    DimAccess (..),
    IndexExprName,
    IterationType (Sequential, Parallel),
    BodyType (..),
    SegOpName (SegmentedMap, SegmentedRed, SegmentedScan, SegmentedHist),
    notImplementedYet,
    Context (..),
    analyzeIndex,
    CtxVal (..),
    VarType (..),
  )
where

import Data.Bifunctor
import Data.Either
import Data.Foldable
import Data.IntMap.Strict qualified as S
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.IR.GPU (GPU, HostOp (..), KernelBody (..), SegOp (..), SizeOp (..), segBody, segSpace, unSegSpace)
import Futhark.IR.GPUMem (GPUMem)
import Futhark.IR.MC (MC, MCOp (..))
import Futhark.IR.MCMem (MCMem)
import Futhark.IR.Prop.Names
import Futhark.IR.SOACS (SOACS)
import Futhark.IR.Seq (Seq)
import Futhark.IR.SeqMem (SeqMem)
import Futhark.IR.Syntax
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
    -- Tuple of patternName and nested `level` it index occurred at, as well as
    -- what the actual iteration type is.
    dependencies :: S.IntMap (VName, VName, Int, IterationType rep, VarType),
    originalDimension :: Int
  }
  deriving (Eq, Show)

instance Semigroup (DimAccess rep) where
  (<>) :: DimAccess rep -> DimAccess rep -> DimAccess rep
  adeps <> bdeps =
    DimAccess
      (dependencies adeps <> dependencies bdeps)
      (max (originalDimension adeps) (originalDimension bdeps))

instance Monoid (DimAccess rep) where
  mempty = DimAccess mempty 0

isInvariant :: DimAccess rep -> Bool
isInvariant = null . dependencies

-- | Iteration type describes whether the index is iterated in a parallel or
-- sequential way, ie. if the index expression comes from a sequential or
-- parallel construct, like foldl or map.
data IterationType rep = Sequential | Parallel
  deriving (Eq, Show)

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
        parents = [],
        currentLevel = 0
      }

instance Semigroup (Context rep) where
  (<>)
    (Context ass0 lastBody0 lvl0)
    (Context ass1 lastBody1 lvl1) =
      Context
        ((<>) ass0 ass1)
        ((++) lastBody0 lastBody1)
        $ max lvl0 lvl1

-- | Extend a context with another context.
-- We never have to consider the case where VNames clash in the context, since
-- they are unique.
extend :: Context rep -> Context rep -> Context rep
extend = (<>)

allSegMap :: Context rep -> [SegOpName]
allSegMap (Context _ parents _) =
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
    variableType :: VarType
  }
  deriving (Show, Eq)

data VarType
  = ConstType
  | Variable
  | ThreadID
  | LoopVar
  deriving (Show, Eq)

ctxValFromNames :: Context rep -> Names -> CtxVal rep
ctxValFromNames ctx names = do
  CtxVal
    names
    (getIterationType ctx)
    (currentLevel ctx)
    (parents ctx)
    Variable

-- | Wrapper around the constructur of Context.
oneContext :: VName -> CtxVal rep -> Context rep
oneContext name ctxValue =
  Context
    { assignments = M.singleton name ctxValue,
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
    Variable -- variable is a very common type

-- | Create a singular context from a segspace
contextFromNames :: Context rep -> CtxVal rep -> [VName] -> Context rep
contextFromNames ctx ctxval =
  -- Create context from names in segspace
  foldl' extend ctx
    . map (`oneContext` ctxval)

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
  let ctx = contextFromNames mempty (ctxValZeroDeps ctx Sequential) $ map paramName $ funDefParams func
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
      M.foldlWithKey
        ( \result a ctxval ->
            let dependencies = deps ctxval
             in -- if the VName of the assignment exists in the context, we are good
                if a `M.member` localAssignments
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
    (BasicOp (Index name (Slice dimSubexp))) -> analyzeIndex ctx patternNames name dimSubexp
    (BasicOp (Update _ name (Slice dimSubexp) _subexp)) -> analyzeIndex ctx patternNames name dimSubexp
    (BasicOp op) -> analyzeBasicOp ctx op patternNames
    (Match conds cases defaultBody _) -> analyzeMatch (contextFromNames ctx (ctxValZeroDeps ctx (getIterationType ctx)) $ concatMap (namesToList . getDeps) conds) patternNames defaultBody $ map caseBody cases
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
        -- | If we encounter a DimSlice, add it to a map of `DimSlice`s and check
        -- result later.
        -- (DimSlice _offset _num_elems _stride) ->
        -- And then what?
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
  let subExprs =
        mapMaybe
          ( \case
              (DimFix subExpression) -> case subExpression of
                (Var v) -> Just v
                (Constant _) -> Nothing
              (DimSlice _offs _n _stride) -> Nothing
          )
          dimIndexes

  -- Add each non-constant DimIndex as a dependency to the index expression
  let ctxVal = ctxValFromNames ctx $ namesFromList subExprs

  -- Extend context with the dependencies index expression
  foldl' extend ctx $ map (`oneContext` ctxVal) pats

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
        (SubExp subexp) -> ctxValFromSubExpr subexp
        (Opaque _ subexp) -> ctxValFromSubExpr subexp
        (ArrayLit subexps _t) -> concatCtxVals mempty subexps
        (UnOp _ subexp) -> ctxValFromSubExpr subexp
        (BinOp _ lsubexp rsubexp) -> concatCtxVals mempty [lsubexp, rsubexp]
        (CmpOp _ lsubexp rsubexp) -> concatCtxVals mempty [lsubexp, rsubexp]
        (ConvOp _ subexp) -> ctxValFromSubExpr subexp
        (Assert subexp _ _) -> ctxValFromSubExpr subexp
        (Index name _) ->
          error $ "unhandled: Index (This should NEVER happen) into " ++ prettyString name
        (Update _ name _slice _subexp) ->
          error $ "unhandled: Update (This should NEVER happen) onto " ++ prettyString name
        -- Technically, do we need this case?
        (Concat _ _ length_subexp) -> ctxValFromSubExpr length_subexp
        (Manifest _dim name) -> ctxValFromNames ctx $ oneName name
        (Iota end start stride _) -> concatCtxVals mempty [end, start, stride]
        (Replicate (Shape shape) value') -> concatCtxVals mempty (value' : shape)
        (Scratch _ subexprs) -> concatCtxVals mempty subexprs
        (Reshape _ (Shape shape_subexp) name) -> concatCtxVals (oneName name) shape_subexp
        (Rearrange _ name) -> ctxValFromNames ctx $ oneName name
        (UpdateAcc name lsubexprs rsubexprs) -> concatCtxVals (oneName name) (lsubexprs ++ rsubexprs)
        (FlatIndex name _) -> ctxValFromNames ctx $ oneName name
        (FlatUpdate name _ source) -> ctxValFromNames ctx $ namesFromList [name, source]
  let ctx' = foldl' extend ctx $ map (`oneContext` ctx_val) pats
  (ctx', mempty)
  where
    concatCtxVals ne nn =
      ctxValFromNames
        ctx
        (foldl' (\a -> (<>) a . analyzeSubExpr pats ctx) ne nn)

    ctxValFromSubExpr (Constant _) = (ctxValFromNames ctx mempty) {variableType = ConstType}
    ctxValFromSubExpr (Var v) =
      case M.lookup v (assignments ctx) of
        (Just _) -> (ctxValFromNames ctx $ oneName v) {variableType = Variable}
        Nothing ->
          error $
            "Failed to lookup variable \""
              ++ prettyString v
              ++ "\npat: "
              ++ prettyString pats
              ++ "\n\nContext\n"
              ++ show ctx

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
        contextFromNames ctx'' ((ctxValZeroDeps ctx Sequential) {variableType = LoopVar}) $
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
          . map (\(n, i) -> oneContext n $ CtxVal mempty Parallel (currentLevel ctx + i) (parents ctx') ThreadID)
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
        contextFromNames ctx (ctxValZeroDeps ctx Sequential)
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
getIterationType (Context _ bodies _) =
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
consolidate ctx (Var v) = reduceDependencies ctx v v

-- | Recursively lookup vnames until vars with no deps are reached.
reduceDependencies :: Context rep -> VName -> VName -> DimAccess rep
reduceDependencies ctx v_src v =
  case M.lookup v (assignments ctx) of
    Nothing -> error $ "Unable to find " ++ prettyString v
    Just (CtxVal deps itertype lvl _parents t) ->
      -- We detect whether it is a threadID or loop counter by checking
      -- whether or not it has any dependencies
      case t of
        -- TODO: is basetag v good enough as a key?
        ThreadID -> DimAccess (S.fromList [(baseTag v, (v, v_src, lvl, itertype, t))]) (currentLevel ctx)
        LoopVar -> DimAccess (S.fromList [(baseTag v, (v, v_src, lvl, itertype, t))]) (currentLevel ctx)
        ConstType -> mempty
        _ ->
          foldl' (<>) mempty $
            map (reduceDependencies ctx v_src) $
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
  analyzeOp _mcOp = error "Unexpected?"

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
    "dependencies" <+> equals <+> align (prettyDeps $ dependencies dimidx) -- <+> (show $ variableType dimidx)
    where
      prettyDeps = braces . commasep . map (printPair . snd) . S.toList
      printPair (name, name_orig, lvl, itertype, vtype) = pretty name_orig <+> pretty name <+> pretty lvl <+> pretty itertype <+> pretty vtype

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

instance Pretty VarType where
  pretty ConstType = "const"
  pretty Variable = "var"
  pretty ThreadID = "tid"
  pretty LoopVar = "iter"
