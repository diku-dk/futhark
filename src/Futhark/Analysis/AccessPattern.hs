{-# LANGUAGE LambdaCase #-}

module Futhark.Analysis.AccessPattern
  ( analyzeDimIdxPats,
    analyzeFunction,
    analyzeStm,
    ArrayIndexDescriptors,
    ArrayName,
    Dependencies,
    DimIdxPat (DimIdxPat),
    IndexExprName,
    IterationType (Sequential, Parallel),
    MemoryEntry,
    SegOpName (SegmentedMap, SegmentedRed, SegmentedScan, SegmentedHist),
  )
where

import Data.Either (partitionEithers)
import Data.Foldable
import Data.IntMap.Strict qualified as S
import Data.List ((\\))
import Data.Map.Strict qualified as M
import Data.Maybe
import Debug.Pretty.Simple
import Futhark.IR.GPU

-- | Iteration type describes whether the index is iterated in a parallel or
-- sequential way, ie. if the index expression comes from a sequential or
-- parallel construct, like foldl or map.
data IterationType
  = Sequential
  | Parallel
  deriving (Eq, Ord, Show)

-- | Set of VNames of gtid's that some access is variant to.
-- Tuple of patternName and nested `level` it is created at.
type Dependencies = S.IntMap (VName, Int, IterationType)

type ArrayName = VName

data SegOpName
  = SegmentedMap (Int, VName)
  | SegmentedRed (Int, VName)
  | SegmentedScan (Int, VName)
  | SegmentedHist (Int, VName)
  deriving (Eq, Ord, Show)

type LoopBodyName = (Int, VName)

type CondBodyName = (Int, VName)

type IndexExprName = VName

-- | Collect all features of access to a specific dimension of an array.
newtype DimIdxPat = DimIdxPat
  { -- | Set of gtid's that the access is variant to.
    -- An empty set indicates that the access is invariant.
    dependencies :: Dependencies
  }
  -- \| Whether the access is parallel or sequential

  deriving (Eq, Ord, Show)

-- | Each element in the list corresponds to an access to a dimension in the given array
-- in the order of the dimensions.
type MemoryEntry = [DimIdxPat]

-- | Map variable names of arrays in a segmap to index expressions on those arrays.
-- segmap(pattern) |-> A(pattern) |-> indexExpressionName(pattern) |-> [[DimIdxPat]]
type ArrayIndexDescriptors =
  M.Map SegOpName (M.Map ArrayName (M.Map IndexExprName [MemoryEntry]))

unionArrayIndexDescriptors :: ArrayIndexDescriptors -> ArrayIndexDescriptors -> ArrayIndexDescriptors
unionArrayIndexDescriptors = M.unionWith (M.unionWith (M.unionWith (++)))

-- ================ EXAMPLE 1 ================
-- segmap_0 (p)
--   ...
--   segmap_1 (q)
--     let as_1  = A[q,p,i] + A[x,z,q]
--     let as_2  = A[x+y,z,q]
--     let as_3  = A[0,0,0] + B[1,1,1]
--     let res_2 = as + A[y,0,x]
--     in res_2
--
-- ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
--
-- segmap_0:
--   A:
--     as_1:
--       [q,p,i] -> [i,p,q]
-- ...
-- segmap_1:
--   A:
--     as_1:
--       [q,p,i] -> [i,p,q]
--       [x,z,q]
--     as_2:
--       [[x 0,y 1],z 1337 ,q 1]
--     as_3:
--       [0,0,0]
--     res_2:
--       [y,0,x]
--  B:
--    as_3:
--      [1,1,1]

-- A := ...
-- segmap_0 (x,y)
--  A[y,x]
--  ...
--  segmap_1 (i,j)
--  ...
--    A[j,i]

-- seg (i,j)
--  loop l < n
--    seg (x,y)
--      A[i,j,l,x,y]

-- ================ EXAMPLE 2 ================

-- segmap_0 (x,y):
--  segmap_1 (p):
--    segmap_2 (q):
--      let as_1 = A[q,p]

-- ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

-- ctx: [segmap_0, segmap_1, segmap_2]

-- segmap_0:      -- it is a false positive[VERIFICATION NEEDED], since segmap_0
--   A:           -- does not directly provide variables to index into `as_1`
--     as_1:
--       [q,p]
-- segmap_1:
--   A:
--     as_1:
--       [q,p]
-- segmap_2:
--   A:
--     as_1:
--       [q,p]

-- OKAY, but what if
-- let Bs = segmap () As
-- let Cs = segmap () Bs
-- How is Bs and Cs tracked to their respective segmaps?

-- | Only used during the analysis to keep track of the dependencies of each
-- pattern. For example, a pattern might depend on a function parameter, a
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

insertDep :: CtxVal -> Names -> CtxVal
insertDep (CtxVal lnames itertype lvl) names =
  CtxVal ((<>) lnames names) itertype lvl

data BodyType
  = SegOpName SegOpName
  | LoopBodyName LoopBodyName
  | CondBodyName CondBodyName
  deriving (Show, Ord, Eq)

-- | Used during the analysis to keep track of the dependencies of patterns
-- encountered so far.
data Context = Context
  { -- | A mapping from patterns occuring in Let expressions to their dependencies
    --  and iteration types.
    assignments :: M.Map VName CtxVal,
    -- | TODO: explain
    constants :: Names,
    -- | A list of the segMaps encountered during the analysis in the order they
    -- were encountered.
    lastBodyType :: [BodyType],
    -- | Current level of recursion
    currentLevel :: Int
  }
  deriving (Show, Ord, Eq)

-- | Get the last segmap encountered in the context.
lastSegMap :: Context -> Maybe SegOpName
lastSegMap (Context _ _ [] _) = Nothing
lastSegMap (Context _ _ bodies _) = safeLast $ getSegMaps bodies
  where
    getSegMaps =
      mapMaybe
        ( \case
            (SegOpName s) -> Just s
            _ -> Nothing
        )

    safeLast [] = Nothing
    safeLast [x] = Just x
    safeLast (_ : xs) = safeLast xs

instance Monoid Context where
  mempty =
    Context
      { assignments = mempty,
        constants = mempty,
        lastBodyType = [],
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

instance Semigroup DimIdxPat where
  (<>) :: DimIdxPat -> DimIdxPat -> DimIdxPat
  (<>) (DimIdxPat adeps) (DimIdxPat bdeps) =
    DimIdxPat ((<>) adeps bdeps)

-- | Intersect a context with another context.
-- Only needed for debugging. Delete if not used.
intersection :: Context -> Context -> Context
intersection
  (Context ass0 consts0 lastBody0 lvl0)
  (Context ass1 consts1 lastBody1 lvl1) =
    Context
      (M.intersection ass0 ass1)
      (namesIntersection consts0 consts1)
      ((\\) lastBody0 lastBody1)
      $ max lvl0 lvl1

-- | Extend a context with another context.
-- We never have to consider the case where VNames clash in the context, since
-- they are unique.
extend :: Context -> Context -> Context
extend = (<>)

oneContext :: VName -> CtxVal -> Names -> [BodyType] -> Context
oneContext name ctxValue consts segmaps =
  Context
    { assignments = M.singleton name ctxValue,
      constants = consts,
      lastBodyType = segmaps,
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
contextFromSegSpace :: Context -> SegOpName -> SegSpace -> Context
contextFromSegSpace ctx segspaceName segspace =
  -- TODO? make proper assignments in context depending on subexpr
  foldl' (\acc (name, _subexpr) -> extend acc $ oneContext name ctxVal mempty []) mempty $
    unSegSpace segspace
  where
    -- ctxVal = CtxVal (oneName $ snd segspaceName) Parallel $ currentLevel ctx
    ctxVal = ctxValZeroDeps ctx Parallel

-- | Create a singular context from a parameter
contextFromParam :: IterationType -> FParam GPU -> CtxVal -> Context
contextFromParam _i p v = oneContext (paramName p) v mempty []

-- | Create a context from a list of parameters
contextFromParams :: IterationType -> [FParam GPU] -> CtxVal -> Context
contextFromParams iterType pats name =
  foldl extend mempty $
    map (\pat -> contextFromParam iterType pat name) pats

-- | Analyze each `entry` and accumulate the results.
analyzeDimIdxPats :: Prog GPU -> ArrayIndexDescriptors
analyzeDimIdxPats = snd . (foldMap' analyzeFunction . progFuns)

-- | Analyze each statement in a function body.
analyzeFunction :: FunDef GPU -> (Context, ArrayIndexDescriptors)
analyzeFunction func = do
  let stms = stmsToList . bodyStms $ funDefBody func
  let ctx =
        contextFromParams Sequential (funDefParams func) $
          -- All entries are "sequential" in nature.
          ctxValZeroDeps mempty Sequential
  analyzeStmsPrimitive ctx stms

-- | Analyze each statement in a list of statements.
analyzeStmsPrimitive :: Context -> [Stm GPU] -> (Context, ArrayIndexDescriptors)
analyzeStmsPrimitive ctx =
  -- Fold over statements in body
  foldl'
    (\(c, r) stm -> let (c', r') = analyzeStm c stm in (c', unionArrayIndexDescriptors r r'))
    (ctx, mempty)

-- | Same as analyzeStmsPrimitive, but change the resulting context into
-- a ctxVal, mapped to pattern.
analyzeStms :: Context -> Context -> ((Int, VName) -> BodyType) -> Pat dec -> [Stm GPU] -> (Context, ArrayIndexDescriptors)
analyzeStms ctx tmp_ctx bodyConstructor pats body = do
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
    pat = firstPatElemName pats

    -- Extracts and merges `Names` in `CtxVal`s, and makes a new CtxVal. This MAY throw away needed
    -- information, but it was my best guess at a solution at the time of
    -- writing.
    concatCtxVal cvals =
      oneContext pat (ctxValFromNames ctx $ foldl' (<>) mempty $ map deps cvals) mempty []

    recContext =
      extend ctx $
        extend tmp_ctx $
          Context
            { assignments = mempty,
              constants = mempty,
              lastBodyType = [bodyConstructor (currentLevel ctx, pat)],
              currentLevel = currentLevel ctx + 1
            }

-- | Analyze a GPU statement and return the updated context and array index
-- descriptors.
analyzeStm :: Context -> Stm GPU -> (Context, ArrayIndexDescriptors)
analyzeStm ctx (Let pats _ e) =
  case e of
    (BasicOp (Index name (Slice ee))) -> analyzeIndex ctx pats name ee
    (BasicOp op) -> analyzeBasicOp ctx op pats
    (Match _ cases defaultBody _) -> analyzeMatch ctx pats defaultBody $ map caseBody cases
    (Loop bindings loop body) -> analyzeLoop ctx bindings loop body pats
    (Apply _name _ _ _) -> analyzeApply ctx pats
    (WithAcc _ _) -> analyzeWithAcc ctx pats
    (Op (SegOp op)) -> analyzeSegOp ctx op pats
    (Op (SizeOp op)) -> analyzeSizeOp ctx op pats
    (Op (GPUBody _ body)) -> analyzeGPUBody ctx pats body
    (Op (OtherOp _)) -> analyzeOtherOp ctx pats

-- | Get the name of the first element in a pattern
firstPatElemName :: Pat dec -> VName
firstPatElemName pat = patElemName . head $ patElems pat

analyzeIndex :: Context -> Pat dec -> VName -> [DimIndex SubExp] -> (Context, ArrayIndexDescriptors)
analyzeIndex ctx pats arr_name dimIndexes = do
  let pat = firstPatElemName pats
  -- TODO: Should we just take the latest segmap?
  let last_segmap = lastSegMap ctx
  let memory_entries = [mapMaybe f dimIndexes]
        where
          f dimIndex = case dimIndex of
            (DimFix subExpression) -> Just $ DimIdxPat $ consolidate ctx subExpression
            (DimSlice _offs _n _stride) -> Nothing -- TODO: How should we handle slices?
  let idx_expr_name = pat --                                         IndexExprName
  let map_ixd_expr = M.singleton idx_expr_name memory_entries --     IndexExprName |-> [MemoryEntry]
  let map_array = M.singleton arr_name map_ixd_expr -- ArrayName |-> IndexExprName |-> [MemoryEntry]
  let res = case last_segmap of --      SegOpName |-> ArrayName |-> IndexExprName |-> [MemoryEntry]
  -- If the index is outside of a segmap/loop, then there's no
  -- result to add.
        Nothing -> mempty
        (Just segmap) -> M.singleton segmap map_array

  -- Partition the DimIndexes into constants and non-constants
  let (subExprs, consts) =
        partitionEithers $
          mapMaybe
            ( \case
                (DimFix subExpression) -> case subExpression of
                  (Constant _) -> Just (Right pat)
                  (Var v) -> Just (Left v)
                (DimSlice _offs _n _stride) -> Nothing
            )
            dimIndexes

  -- Add each non-constant DimIndex as a dependency to the index expression
  let ctxVal =
        ctxValFromNames ctx $
          namesFromList subExprs

  -- Add each constant DimIndex to the context
  let constants = namesFromList consts

  -- Extend context with the dependencies and constants index expression
  let ctx' = extend ctx $ oneContext pat ctxVal constants []
  (ctx', res)

analyzeBasicOp :: Context -> BasicOp -> Pat dec -> (Context, ArrayIndexDescriptors)
analyzeBasicOp ctx expression pats = do
  let pat = firstPatElemName pats
  -- TODO: Lookup basicOp
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
          error $ "unhandled: Index (Skill issue?) " ++ baseString name
        (Update _ name _slice _subexp) ->
          error $ "unhandled: Update (technically skill issue?)" ++ baseString name
        -- Technically, do we need this case?
        (Concat _ _ length_subexp) -> ctxValFromNames ctx $ analyzeSubExpr pats ctx length_subexp
        (Manifest _dim _name) -> error "unhandled: Manifest"
        (Iota end start stride _) -> concatCtxVals mempty [end, start, stride]
        (Replicate (Shape shape) value') -> concatCtxVals mempty (value' : shape)
        (Scratch _ subexprs) -> concatCtxVals mempty subexprs
        (Reshape _ (Shape shape_subexp) name) -> concatCtxVals (oneName name) shape_subexp
        (Rearrange _ name) -> ctxValFromNames ctx $ oneName name
        (UpdateAcc name lsubexprs rsubexprs) -> concatCtxVals (oneName name) (lsubexprs ++ rsubexprs)
        _ -> error "unhandled: match-all"
  let ctx' = extend ctx $ oneContext pat ctx_val mempty []
  (ctx', mempty)
  where
    concatCtxVals ne nn =
      ctxValFromNames
        ctx
        (foldl' (\a -> (<>) a . analyzeSubExpr pats ctx) ne nn)

analyzeMatch :: Context -> Pat dec -> Body GPU -> [Body GPU] -> (Context, ArrayIndexDescriptors)
analyzeMatch ctx pats body bodies =
  let ctx' =
        Context
          { assignments = assignments ctx,
            constants = constants ctx,
            lastBodyType = lastBodyType ctx,
            currentLevel = currentLevel ctx - 1
          }
   in foldl'
        ( \(ctx'', res) b ->
            onSnd (unionArrayIndexDescriptors res)
              . analyzeStms ctx' ctx'' CondBodyName pats
              . stmsToList
              $ bodyStms b
        )
        (ctx', mempty)
        (body : bodies)

analyzeLoop :: Context -> [(FParam GPU, SubExp)] -> LoopForm GPU -> Body GPU -> Pat dec -> (Context, ArrayIndexDescriptors)
analyzeLoop ctx bindings loop body pats = do
  let pat = firstPatElemName pats
  let fromBindings itervar (param, subexpr) =
        oneContext (paramName param) (CtxVal ((<>) (oneName itervar) (analyzeSubExpr pats ctx subexpr)) Sequential $ currentLevel ctx) mempty []
  let ctx' =
        case loop of
          (WhileLoop iterVar) ->
            (<>)
              (foldl' (<>) mempty $ map (fromBindings iterVar) bindings)
              (oneContext iterVar (ctxValZeroDeps ctx Sequential) mempty [])
          (ForLoop iterVar _ _ params) -> do
            let neutralElem = ctxValZeroDeps ctx Sequential
            let fromParam (param, vname) =
                  oneContext (paramName param) (CtxVal ((<>) (oneName iterVar) (oneName vname)) Sequential $ currentLevel ctx) mempty []
            (<>)
              (foldl' (<>) mempty $ map (fromBindings iterVar) bindings)
              (foldl' (<>) (oneContext iterVar neutralElem mempty []) (map fromParam params))

  let ctxVal = ctxValZeroDeps ctx Sequential -- TODO: There should be deps right?

  -- Extend context with the loop expression
  let ctx'' = extend ctx' $ oneContext pat ctxVal mempty []
  analyzeStms ctx ctx'' LoopBodyName pats $ stmsToList $ bodyStms body

analyzeApply :: Context -> Pat dec -> (Context, ArrayIndexDescriptors)
analyzeApply ctx pats = error "UNHANDLED: Apply"

analyzeWithAcc :: Context -> Pat dec -> (Context, ArrayIndexDescriptors)
analyzeWithAcc ctx pats = error "UNHANDLED: WithAcc"

segOpType :: SegOp lvl GPU -> (Int, VName) -> SegOpName
segOpType (SegMap {}) = SegmentedMap
segOpType (SegRed {}) = SegmentedRed
segOpType (SegScan {}) = SegmentedScan
segOpType (SegHist {}) = SegmentedHist

analyzeSegOp :: Context -> SegOp lvl GPU -> Pat dec -> (Context, ArrayIndexDescriptors)
analyzeSegOp ctx op pats = do
  let pat = firstPatElemName pats
  let segSpaceContext =
        extend ctx $
          contextFromSegSpace ctx (segOpType op (currentLevel ctx, pat)) $
            segSpace op
  -- Analyze statements in the SegOp body
  analyzeStms ctx segSpaceContext (SegOpName . segOpType op) pats . stmsToList . kernelBodyStms $ segBody op

analyzeSizeOp :: Context -> SizeOp -> Pat dec -> (Context, ArrayIndexDescriptors)
analyzeSizeOp ctx op pats = do
  let subexprsToContext = extend ctx . concatCtxVal . map (analyzeSubExpr pats ctx)
  let ctx' = case op of
        (CmpSizeLe _name _class subexp) -> subexprsToContext [subexp]
        (CalcNumGroups lsubexp _name rsubexp) -> subexprsToContext [lsubexp, rsubexp]
        _ -> ctx
  -- Add sizeOp to context
  let ctxVal = ctxValZeroDeps ctx Sequential
  let ctx'' = extend ctx' $ oneContext pat ctxVal mempty []
  (ctx'', mempty)
  where
    pat = firstPatElemName pats
    concatCtxVal :: [Names] -> Context
    concatCtxVal [] = mempty
    concatCtxVal (ne : remainder) = oneContext pat (ctxValFromNames ctx $ foldl' (<>) ne remainder) mempty []

-- | Analyze statements in a GPU body.
analyzeGPUBody :: Context -> Pat dec -> Body GPU -> (Context, ArrayIndexDescriptors)
analyzeGPUBody ctx pats body =
  -- TODO: Add dependencies to pat in ctx
  analyzeStmsPrimitive ctx $ stmsToList $ bodyStms body

analyzeOtherOp :: Context -> Pat dec -> (Context, ArrayIndexDescriptors)
analyzeOtherOp ctx pats = error "UNHANDLED: OtherOp"

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
        -- recurse a bit ya kno
        CondBodyName _ -> getIteration_rec $ init rec

-- Returns an intmap of names, to be used as dependencies in construction of
-- CtxVals.
-- Throws an error if SubExp contains a name not in context. This behaviour
-- might be thrown out in the future, as it is mostly just a very verbose way to
-- ensure that we capture all necessary variables in the context at the moment
-- of development.
analyzeSubExpr :: Pat d -> Context -> SubExp -> Names
analyzeSubExpr _ _ (Constant _) = mempty
analyzeSubExpr p ctx (Var v) =
  let pp = firstPatElemName p
   in case M.lookup v (assignments ctx) of
        Nothing -> error $ "Failed to lookup variable \"" ++ baseString v ++ "_" ++ show (baseTag v) ++ "\npat: " ++ show pp ++ "\n\nContext\n" ++ show ctx
        (Just _) -> oneName v

-- | Consolidates a dimfix into a set of dependencies
consolidate :: Context -> SubExp -> Dependencies
consolidate _ctx (Constant _) = S.fromList []
consolidate ctx (Var v) = reduceDependencies ctx v

-- | Recursively lookup vnames until vars with no deps are reached.
reduceDependencies :: Context -> VName -> Dependencies
reduceDependencies ctx v =
  if nameIn v (constants ctx)
    then mempty -- If v is a constant, then it is not a dependency
    else case M.lookup v (assignments ctx) of
      Nothing -> error $ "Unable to find " ++ baseString v
      Just (CtxVal deps itertype lvl) ->
        if null $ namesToList deps
          then S.fromList [(baseTag v, (v, lvl, itertype))]
          else
            foldl' S.union mempty $
              map (reduceDependencies ctx) $
                namesToList deps

-- Apply `f` to second/right part of tuple.
onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (x, y) = (x, f y)
