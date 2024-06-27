{-# LANGUAGE LambdaCase #-}

module Futhark.Analysis.AccessPattern
  ( analyseDimAccesses,
    analyseFunction,
    vnameFromSegOp,
    analysisPropagateByTransitivity,
    isInvariant,
    Analyse,
    IndexTable,
    ArrayName,
    DimAccess (..),
    IndexExprName,
    BodyType (..),
    SegOpName (SegmentedMap, SegmentedRed, SegmentedScan, SegmentedHist),
    Context (..),
    analyseIndex,
    VariableInfo (..),
    VarType (..),
    isCounter,
    Dependency (..),
  )
where

import Data.Bifunctor
import Data.Foldable
import Data.List qualified as L
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
import Futhark.Util.Pretty

-- | Name of a SegOp, used to identify the SegOp that an array access is
-- contained in.
data SegOpName
  = SegmentedMap {vnameFromSegOp :: VName}
  | SegmentedRed {vnameFromSegOp :: VName}
  | SegmentedScan {vnameFromSegOp :: VName}
  | SegmentedHist {vnameFromSegOp :: VName}
  deriving (Eq, Ord, Show)

-- | Name of an array indexing expression. Taken from the pattern of
-- the expression.
type IndexExprName = VName

data BodyType
  = SegOpName SegOpName
  | LoopBodyName VName
  | CondBodyName VName
  deriving (Show, Ord, Eq)

-- | Stores the name of an array, the nest of loops, kernels,
-- conditionals in which it is constructed, and the existing layout of
-- the array. The latter is currently largely unused and not
-- trustworthy, but might be useful in the future.
type ArrayName = (VName, [BodyType], [Int])

-- | Tuple of patternName and nested `level` it index occurred at, as well as
-- what the actual iteration type is.
data Dependency = Dependency
  { lvl :: Int,
    varType :: VarType
  }
  deriving (Eq, Show)

-- | Collect all features of access to a specific dimension of an array.
data DimAccess rep = DimAccess
  { -- | Set of VNames of iteration variables (gtids, loop counters, etc.)
    -- that some access is variant to.
    -- An empty set indicates that the access is invariant.
    dependencies :: M.Map VName Dependency,
    -- | Used to store the name of the original expression from which `dependencies`
    -- was computed. `Nothing` if it is a constant.
    originalVar :: Maybe VName
  }
  deriving (Eq, Show)

instance Semigroup (DimAccess rep) where
  adeps <> bdeps =
    DimAccess
      (dependencies adeps <> dependencies bdeps)
      ( case originalVar adeps of
          Nothing -> originalVar bdeps
          _ -> originalVar adeps
      )

instance Monoid (DimAccess rep) where
  mempty = DimAccess mempty Nothing

isInvariant :: DimAccess rep -> Bool
isInvariant = null . dependencies

-- | For each array access in a program, this data structure stores the
-- dependencies of each dimension in the access, the array name, and the
-- name of the SegOp that the access is contained in.
-- Each DimAccess element corresponds to an access to a given dimension
-- in the given array, in the same order of the dimensions.
type IndexTable rep =
  M.Map SegOpName (M.Map ArrayName (M.Map IndexExprName [DimAccess rep]))

unionIndexTables :: IndexTable rep -> IndexTable rep -> IndexTable rep
unionIndexTables = M.unionWith (M.unionWith M.union)

-- | Make segops on arrays transitive, ie. if
-- > let A = segmap (..) xs -- A indexes into xs
-- > let B = segmap (..) A  -- B indexes into A
-- Then B also derives all A's array-accesses, like xs.
-- Runs in n²
analysisPropagateByTransitivity :: IndexTable rep -> IndexTable rep
analysisPropagateByTransitivity idx_table =
  M.map foldlArrayNameMap idx_table
  where
    aggregateResults arr_name =
      maybe
        mempty
        foldlArrayNameMap
        (M.mapKeys vnameFromSegOp idx_table M.!? arr_name)

    foldlArrayNameMap aMap =
      foldl (M.unionWith M.union) aMap $
        map (aggregateResults . \(a, _, _) -> a) $
          M.keys aMap

--
-- Helper types and functions to perform the analysis.
--

-- | Used during the analysis to keep track of the dependencies of patterns
-- encountered so far.
data Context rep = Context
  { -- | A mapping from patterns occuring in Let expressions to their dependencies
    --  and iteration types.
    assignments :: M.Map VName (VariableInfo rep),
    -- | Maps from sliced arrays to their respective access patterns.
    slices :: M.Map IndexExprName (ArrayName, [VName], [DimAccess rep]),
    -- | A list of the segMaps encountered during the analysis in the order they
    -- were encountered.
    parents :: [BodyType],
    -- | Current level of recursion, also just `length parents`
    currentLevel :: Int
  }
  deriving (Show, Eq)

instance Monoid (Context rep) where
  mempty =
    Context
      { assignments = mempty,
        slices = mempty,
        parents = [],
        currentLevel = 0
      }

instance Semigroup (Context rep) where
  Context ass0 slices0 lastBody0 lvl0 <> Context ass1 slices1 lastBody1 lvl1 =
    Context
      (ass0 <> ass1)
      (slices0 <> slices1)
      (lastBody0 <> lastBody1)
      (max lvl0 lvl1)

-- | Extend a context with another context.
-- We never have to consider the case where VNames clash in the context, since
-- they are unique.
extend :: Context rep -> Context rep -> Context rep
extend = (<>)

allSegMap :: Context rep -> [SegOpName]
allSegMap (Context _ _ parents _) = mapMaybe f parents
  where
    f (SegOpName o) = Just o
    f _ = Nothing

-- | Context Value (VariableInfo) is the type used in the context to categorize
-- assignments. For example, a pattern might depend on a function parameter, a
-- gtid, or some other pattern.
data VariableInfo rep = VariableInfo
  { deps :: Names,
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

isCounter :: VarType -> Bool
isCounter LoopVar = True
isCounter ThreadID = True
isCounter _ = False

varInfoFromNames :: Context rep -> Names -> VariableInfo rep
varInfoFromNames ctx names = do
  VariableInfo names (currentLevel ctx) (parents ctx) Variable

-- | Wrapper around the constructur of Context.
oneContext :: VName -> VariableInfo rep -> Context rep
oneContext name var_info =
  Context
    { assignments = M.singleton name var_info,
      slices = mempty,
      parents = [],
      currentLevel = 0
    }

-- | Create a singular varInfo with no dependencies.
varInfoZeroDeps :: Context rep -> VariableInfo rep
varInfoZeroDeps ctx =
  VariableInfo mempty (currentLevel ctx) (parents ctx) Variable

-- | Create a singular context from a segspace
contextFromNames :: Context rep -> VariableInfo rep -> [VName] -> Context rep
contextFromNames ctx var_info = foldl' extend ctx . map (`oneContext` var_info)

-- | A representation where we can analyse access patterns.
class Analyse rep where
  -- | Analyse the op for this representation.
  analyseOp :: Op rep -> Context rep -> [VName] -> (Context rep, IndexTable rep)

-- | Analyse each `entry` and accumulate the results.
analyseDimAccesses :: (Analyse rep) => Prog rep -> IndexTable rep
analyseDimAccesses = foldMap' analyseFunction . progFuns

-- | Analyse each statement in a function body.
analyseFunction :: (Analyse rep) => FunDef rep -> IndexTable rep
analyseFunction func =
  let stms = stmsToList . bodyStms $ funDefBody func
      -- Create a context containing the function parameters
      ctx = contextFromNames mempty (varInfoZeroDeps ctx) $ map paramName $ funDefParams func
   in snd $ analyseStmsPrimitive ctx stms

-- | Analyse each statement in a list of statements.
analyseStmsPrimitive :: (Analyse rep) => Context rep -> [Stm rep] -> (Context rep, IndexTable rep)
analyseStmsPrimitive ctx =
  -- Fold over statements in body
  foldl'
    (\(c, r) stm -> second (unionIndexTables r) $ analyseStm c stm)
    (ctx, mempty)

-- | Same as analyseStmsPrimitive, but change the resulting context into
-- a varInfo, mapped to pattern.
analyseStms :: (Analyse rep) => Context rep -> (VName -> BodyType) -> [VName] -> [Stm rep] -> (Context rep, IndexTable rep)
analyseStms ctx body_constructor pats body = do
  -- 0. Recurse into body with ctx
  let (ctx'', indexTable) = analyseStmsPrimitive recContext body

  -- 0.1 Get all new slices
  let slices_new = M.difference (slices ctx'') (slices ctx)
  -- 0.2 Make "IndexExpressions" of the slices
  let slices_indices =
        foldl unionIndexTables indexTable
          $ mapMaybe
            ( uncurry $ \_idx_expression (array_name, patterns, dim_indices) ->
                Just . snd $
                  -- Should we use recContex instead of ctx''?
                  analyseIndex' ctx'' patterns array_name dim_indices
            )
          $ M.toList slices_new

  -- 1. We do not want the returned context directly.
  --    however, we do want pat to map to the names what was hit in body.
  --    therefore we need to subtract the old context from the returned one,
  --    and discard all the keys within it.

  -- assignments :: M.Map VName (VariableInfo rep),
  let in_scope_dependencies_from_body =
        rmOutOfScopeDeps ctx'' $
          M.difference (assignments ctx'') (assignments recContext)

  -- 2. We are ONLY interested in the rhs of assignments (ie. the
  --    dependencies of pat :) )
  let ctx' = foldl extend ctx $ concatVariableInfo in_scope_dependencies_from_body -- . map snd $ M.toList varInfos
  -- 3. Now we have the correct context and result
  (ctx' {parents = parents ctx, currentLevel = currentLevel ctx, slices = slices ctx}, slices_indices)
  where
    -- Extracts and merges `Names` in `VariableInfo`s, and makes a new VariableInfo. This
    -- MAY throw away needed information, but it was my best guess at a solution
    -- at the time of writing.
    concatVariableInfo dependencies =
      map (\pat -> oneContext pat (varInfoFromNames ctx dependencies)) pats

    -- Context used for "recursion" into analyseStmsPrimitive
    recContext =
      ctx
        { parents = parents ctx <> concatMap (\pat -> [body_constructor pat]) pats,
          currentLevel = currentLevel ctx + 1
        }

    -- Recursively looks up dependencies, until they're in scope or empty set.
    rmOutOfScopeDeps :: Context rep -> M.Map VName (VariableInfo rep) -> Names
    rmOutOfScopeDeps ctx' new_assignments =
      let throwaway_assignments = assignments ctx'
          local_assignments = assignments ctx
          f result a var_info =
            -- if the VName of the assignment exists in the context, we are good
            if a `M.member` local_assignments
              then result <> oneName a
              else -- Otherwise, recurse on its dependencies;
              -- 0. Add dependencies in ctx to result

                let (deps_in_ctx, deps_not_in_ctx) =
                      L.partition (`M.member` local_assignments) $
                        namesToList (deps var_info)
                    deps_not_in_ctx' =
                      M.fromList $
                        mapMaybe
                          (\d -> (d,) <$> M.lookup d throwaway_assignments)
                          deps_not_in_ctx
                 in result
                      <> namesFromList deps_in_ctx
                      <> rmOutOfScopeDeps ctx' deps_not_in_ctx'
       in M.foldlWithKey f mempty new_assignments

-- | Analyse a rep statement and return the updated context and array index
-- descriptors.
analyseStm :: (Analyse rep) => Context rep -> Stm rep -> (Context rep, IndexTable rep)
analyseStm ctx (Let pats _ e) = do
  -- Get the name of the first element in a pattern
  let pattern_names = map patElemName $ patElems pats

  -- Construct the result and Context from the subexpression. If the
  -- subexpression is a body, we recurse into it.
  case e of
    BasicOp (Index name (Slice dim_subexp)) ->
      analyseIndex ctx pattern_names name dim_subexp
    BasicOp (Update _ name (Slice dim_subexp) _subexp) ->
      analyseIndex ctx pattern_names name dim_subexp
    BasicOp op ->
      analyseBasicOp ctx op pattern_names
    Match conds cases default_body _ ->
      analyseMatch ctx' pattern_names default_body $ map caseBody cases
      where
        ctx' =
          contextFromNames ctx (varInfoZeroDeps ctx) $
            concatMap (namesToList . freeIn) conds
    Loop bindings loop body ->
      analyseLoop ctx bindings loop body pattern_names
    Apply _name diets _ _ ->
      analyseApply ctx pattern_names diets
    WithAcc _ _ ->
      (ctx, mempty) -- ignored
    Op op ->
      analyseOp op ctx pattern_names

-- If left, this is just a regular index. If right, a slice happened.
getIndexDependencies :: Context rep -> [DimIndex SubExp] -> Either [DimAccess rep] [DimAccess rep]
getIndexDependencies ctx dims =
  fst $
    foldr
      ( \idx (a, i) ->
          ( either (matchDimIndex idx) (either Right Right . matchDimIndex idx) a,
            i - 1
          )
      )
      (Left [], length dims - 1)
      dims
  where
    matchDimIndex (DimFix subExpression) accumulator =
      Left $ consolidate ctx subExpression : accumulator
    -- If we encounter a DimSlice, add it to a map of `DimSlice`s and check
    -- result later.
    matchDimIndex (DimSlice offset num_elems stride) accumulator =
      -- We assume that a slice is iterated sequentially, so we have to
      -- create a fake dependency for the slice.
      let dimAccess' = DimAccess (M.singleton (VName "slice" 0) $ Dependency (currentLevel ctx) LoopVar) (Just $ VName "slice" 0)
          cons = consolidate ctx
          dimAccess = dimAccess' <> cons offset <> cons num_elems <> cons stride
       in Right $ dimAccess : accumulator

-- | Gets the dependencies of each dimension and either returns a result, or
-- adds a slice to the context.
analyseIndex :: Context rep -> [VName] -> VName -> [DimIndex SubExp] -> (Context rep, IndexTable rep)
analyseIndex ctx pats arr_name dim_indices =
  -- Get the dependendencies of each dimension
  let dependencies = getIndexDependencies ctx dim_indices
      -- Extend the current context with current pattern(s) and its deps
      ctx' = analyseIndexContextFromIndices ctx dim_indices pats

      -- The bodytype(s) are used in the result construction
      array_name' =
        -- For now, we assume the array is in row-major-order, hence the
        -- identity permutation. In the future, we might want to infer its
        -- layout, for example, if the array is the result of a transposition.
        let layout = [0 .. length dim_indices - 1]
         in -- 2. If the arrayname was not in assignments, it was not an immediately
            --    allocated array.
            fromMaybe (arr_name, [], layout)
              -- 1. Maybe find the array name, and the "stack" of body types that the
              -- array was allocated in.
              . L.find (\(n, _, _) -> n == arr_name)
              -- 0. Get the "stack" of bodytypes for each assignment
              $ map (\(n, vi) -> (n, parents_nest vi, layout)) (M.toList $ assignments ctx')
   in either (index ctx' array_name') (slice ctx' array_name') dependencies
  where
    slice :: Context rep -> ArrayName -> [DimAccess rep] -> (Context rep, IndexTable rep)
    slice context array_name dims =
      (context {slices = M.insert (head pats) (array_name, pats, dims) $ slices context}, mempty)

    index :: Context rep -> ArrayName -> [DimAccess rep] -> (Context rep, IndexTable rep)
    index context array_name@(name, _, _) dim_access =
      -- If the arrayname is a `DimSlice` we want to fixup the access
      case M.lookup name $ slices context of
        Nothing -> analyseIndex' context pats array_name dim_access
        Just (arr_name', pats', slice_access) ->
          analyseIndex'
            context
            pats'
            arr_name'
            (init slice_access ++ [head dim_access <> last slice_access] ++ drop 1 dim_access)

analyseIndexContextFromIndices :: Context rep -> [DimIndex SubExp] -> [VName] -> Context rep
analyseIndexContextFromIndices ctx dim_accesses pats =
  let subexprs =
        mapMaybe
          ( \case
              DimFix (Var v) -> Just v
              DimFix (Constant _) -> Nothing
              DimSlice _offs _n _stride -> Nothing
          )
          dim_accesses

      -- Add each non-constant DimIndex as a dependency to the index expression
      var_info = varInfoFromNames ctx $ namesFromList subexprs
   in -- Extend context with the dependencies index expression
      foldl' extend ctx $ map (`oneContext` var_info) pats

analyseIndex' ::
  Context rep ->
  [VName] ->
  ArrayName ->
  [DimAccess rep] ->
  (Context rep, IndexTable rep)
analyseIndex' ctx _ _ [] = (ctx, mempty)
analyseIndex' ctx _ _ [_] = (ctx, mempty)
analyseIndex' ctx pats arr_name dim_accesses =
  -- Get the name of all segmaps in the current "callstack"
  let segmaps = allSegMap ctx
      idx_expr_name = pats --                                                IndexExprName
      -- For each pattern, create a mapping to the dimensional indices
      map_ixd_expr = map (`M.singleton` dim_accesses) idx_expr_name --       IndexExprName |-> [DimAccess]
      -- For each pattern -> [DimAccess] mapping, create a mapping from the array
      -- name that was indexed.
      map_array = map (M.singleton arr_name) map_ixd_expr --   ArrayName |-> IndexExprName |-> [DimAccess]
      -- ∀ (arr_name -> IdxExp -> [DimAccess]) mappings, create a mapping from all
      -- segmaps in current callstack (segThread & segGroups alike).
      results = concatMap (\ma -> map (`M.singleton` ma) segmaps) map_array

      res = foldl' unionIndexTables mempty results
   in (ctx, res)

analyseBasicOp :: Context rep -> BasicOp -> [VName] -> (Context rep, IndexTable rep)
analyseBasicOp ctx expression pats =
  -- Construct a VariableInfo from the subexpressions
  let ctx_val = case expression of
        SubExp se -> varInfoFromSubExp se
        Opaque _ se -> varInfoFromSubExp se
        ArrayLit ses _t -> concatVariableInfos mempty ses
        UnOp _ se -> varInfoFromSubExp se
        BinOp _ lsubexp rsubexp -> concatVariableInfos mempty [lsubexp, rsubexp]
        CmpOp _ lsubexp rsubexp -> concatVariableInfos mempty [lsubexp, rsubexp]
        ConvOp _ se -> varInfoFromSubExp se
        Assert se _ _ -> varInfoFromSubExp se
        Index name _ ->
          error $ "unhandled: Index (This should NEVER happen) into " ++ prettyString name
        Update _ name _slice _subexp ->
          error $ "unhandled: Update (This should NEVER happen) onto " ++ prettyString name
        -- Technically, do we need this case?
        Concat _ _ length_subexp -> varInfoFromSubExp length_subexp
        Manifest _dim name -> varInfoFromNames ctx $ oneName name
        Iota end start stride _ -> concatVariableInfos mempty [end, start, stride]
        Replicate (Shape shape) value' -> concatVariableInfos mempty (value' : shape)
        Scratch _ sers -> concatVariableInfos mempty sers
        Reshape _ (Shape shape_subexp) name -> concatVariableInfos (oneName name) shape_subexp
        Rearrange _ name -> varInfoFromNames ctx $ oneName name
        UpdateAcc _ name lsubexprs rsubexprs ->
          concatVariableInfos (oneName name) (lsubexprs ++ rsubexprs)
        FlatIndex name _ -> varInfoFromNames ctx $ oneName name
        FlatUpdate name _ source -> varInfoFromNames ctx $ namesFromList [name, source]
      ctx' = foldl' extend ctx $ map (`oneContext` ctx_val) pats
   in (ctx', mempty)
  where
    concatVariableInfos ne nn =
      varInfoFromNames ctx (ne <> mconcat (map (analyseSubExp pats ctx) nn))

    varInfoFromSubExp (Constant _) = (varInfoFromNames ctx mempty) {variableType = ConstType}
    varInfoFromSubExp (Var v) =
      case M.lookup v (assignments ctx) of
        Just _ -> (varInfoFromNames ctx $ oneName v) {variableType = Variable}
        Nothing ->
          error $
            "Failed to lookup variable \""
              ++ prettyString v
              ++ "\npat: "
              ++ prettyString pats
              ++ "\n\nContext\n"
              ++ show ctx

analyseMatch :: (Analyse rep) => Context rep -> [VName] -> Body rep -> [Body rep] -> (Context rep, IndexTable rep)
analyseMatch ctx pats body parents =
  let ctx'' = ctx {currentLevel = currentLevel ctx - 1}
   in foldl
        ( \(ctx', res) b ->
            -- This Little Maneuver's Gonna Cost Us 51 Years
            bimap constLevel (unionIndexTables res)
              . analyseStms ctx' CondBodyName pats
              . stmsToList
              $ bodyStms b
        )
        (ctx'', mempty)
        (body : parents)
  where
    constLevel context = context {currentLevel = currentLevel ctx - 1}

analyseLoop :: (Analyse rep) => Context rep -> [(FParam rep, SubExp)] -> LoopForm -> Body rep -> [VName] -> (Context rep, IndexTable rep)
analyseLoop ctx bindings loop body pats = do
  let next_level = currentLevel ctx
  let ctx'' = ctx {currentLevel = next_level}
  let ctx' =
        contextFromNames ctx'' ((varInfoZeroDeps ctx) {variableType = LoopVar}) $
          case loop of
            WhileLoop iv -> iv : map (paramName . fst) bindings
            ForLoop iv _ _ -> iv : map (paramName . fst) bindings

  -- Extend context with the loop expression
  analyseStms ctx' LoopBodyName pats $ stmsToList $ bodyStms body

analyseApply :: Context rep -> [VName] -> [(SubExp, Diet)] -> (Context rep, IndexTable rep)
analyseApply ctx pats diets =
  ( foldl' extend ctx $ map (\pat -> oneContext pat $ varInfoFromNames ctx $ mconcat $ map (freeIn . fst) diets) pats,
    mempty
  )

segOpType :: SegOp lvl rep -> VName -> SegOpName
segOpType (SegMap {}) = SegmentedMap
segOpType (SegRed {}) = SegmentedRed
segOpType (SegScan {}) = SegmentedScan
segOpType (SegHist {}) = SegmentedHist

analyseSegOp :: (Analyse rep) => SegOp lvl rep -> Context rep -> [VName] -> (Context rep, IndexTable rep)
analyseSegOp op ctx pats =
  let next_level = currentLevel ctx + length (unSegSpace $ segSpace op) - 1
      ctx' = ctx {currentLevel = next_level}
      segspace_context =
        foldl' extend ctx'
          . map (\(n, i) -> oneContext n $ VariableInfo mempty (currentLevel ctx + i) (parents ctx') ThreadID)
          . (\segspace_params -> zip segspace_params [0 ..])
          -- contextFromNames ctx' Parallel
          . map fst
          . unSegSpace
          $ segSpace op
   in -- Analyse statements in the SegOp body
      analyseStms segspace_context (SegOpName . segOpType op) pats . stmsToList . kernelBodyStms $ segBody op

analyseSizeOp :: SizeOp -> Context rep -> [VName] -> (Context rep, IndexTable rep)
analyseSizeOp op ctx pats =
  let ctx' = case op of
        CmpSizeLe _name _class subexp -> subexprsToContext [subexp]
        CalcNumBlocks lsubexp _name rsubexp -> subexprsToContext [lsubexp, rsubexp]
        _ -> ctx
      -- Add sizeOp to context
      ctx'' =
        foldl' extend ctx' $
          map
            (\pat -> oneContext pat $ (varInfoZeroDeps ctx) {parents_nest = parents ctx'})
            pats
   in (ctx'', mempty)
  where
    subexprsToContext =
      contextFromNames ctx (varInfoZeroDeps ctx)
        . concatMap (namesToList . analyseSubExp pats ctx)

-- | Analyse statements in a rep body.
analyseGPUBody :: (Analyse rep) => Body rep -> Context rep -> (Context rep, IndexTable rep)
analyseGPUBody body ctx =
  analyseStmsPrimitive ctx $ stmsToList $ bodyStms body

analyseOtherOp :: Context rep -> [VName] -> (Context rep, IndexTable rep)
analyseOtherOp ctx _ = (ctx, mempty)

-- | Returns an intmap of names, to be used as dependencies in construction of
-- VariableInfos.
analyseSubExp :: [VName] -> Context rep -> SubExp -> Names
analyseSubExp _ _ (Constant _) = mempty
analyseSubExp _ _ (Var v) = oneName v

-- | Reduce a DimFix into its set of dependencies
consolidate :: Context rep -> SubExp -> DimAccess rep
consolidate _ (Constant _) = mempty
consolidate ctx (Var v) = DimAccess (reduceDependencies ctx v) (Just v)

-- | Recursively lookup vnames until vars with no deps are reached.
reduceDependencies :: Context rep -> VName -> M.Map VName Dependency
reduceDependencies ctx v =
  case M.lookup v (assignments ctx) of
    Nothing -> mempty -- Means a global.
    Just (VariableInfo deps lvl _parents t) ->
      -- We detect whether it is a threadID or loop counter by checking
      -- whether or not it has any dependencies
      case t of
        ThreadID -> M.fromList [(v, Dependency lvl t)]
        LoopVar -> M.fromList [(v, Dependency lvl t)]
        Variable -> mconcat $ map (reduceDependencies ctx) $ namesToList deps
        ConstType -> mempty

-- Misc functions

-- Instances for AST types that we actually support
instance Analyse GPU where
  analyseOp gpu_op
    | (SegOp op) <- gpu_op = analyseSegOp op
    | (SizeOp op) <- gpu_op = analyseSizeOp op
    | (GPUBody _ body) <- gpu_op = pure . analyseGPUBody body
    | (Futhark.IR.GPU.OtherOp _) <- gpu_op = analyseOtherOp

instance Analyse MC where
  analyseOp mc_op
    | ParOp Nothing seq_segop <- mc_op = analyseSegOp seq_segop
    | ParOp (Just segop) seq_segop <- mc_op = \ctx name -> do
        let (ctx', res') = analyseSegOp segop ctx name
        let (ctx'', res'') = analyseSegOp seq_segop ctx' name
        (ctx'', unionIndexTables res' res'')
    | Futhark.IR.MC.OtherOp _ <- mc_op = analyseOtherOp

-- Unfortunately we need these instances, even though they may never appear.
instance Analyse GPUMem where
  analyseOp _ = error $ notImplementedYet "GPUMem"

instance Analyse MCMem where
  analyseOp _ = error "Unexpected?"

instance Analyse Seq where
  analyseOp _ = error $ notImplementedYet "Seq"

instance Analyse SeqMem where
  analyseOp _ = error $ notImplementedYet "SeqMem"

instance Analyse SOACS where
  analyseOp _ = error $ notImplementedYet "SOACS"

notImplementedYet :: String -> String
notImplementedYet s = "Access pattern analysis for the " ++ s ++ " backend is not implemented."

instance Pretty (IndexTable rep) where
  pretty = stack . map f . M.toList :: IndexTable rep -> Doc ann
    where
      f (segop, arrNameToIdxExprMap) = pretty segop <+> colon <+> g arrNameToIdxExprMap

      g maps = lbrace </> indent 4 (mapprintArray $ M.toList maps) </> rbrace

      mapprintArray :: [(ArrayName, M.Map IndexExprName [DimAccess rep])] -> Doc ann
      mapprintArray [] = ""
      mapprintArray [m] = printArrayMap m
      mapprintArray (m : mm) = printArrayMap m </> mapprintArray mm

      printArrayMap :: (ArrayName, M.Map IndexExprName [DimAccess rep]) -> Doc ann
      printArrayMap ((name, _, layout), maps) =
        "(arr)"
          <+> pretty name
          <+> colon
          <+> pretty layout
          <+> lbrace
          </> indent 4 (mapprintIdxExpr (M.toList maps))
          </> rbrace

      mapprintIdxExpr :: [(IndexExprName, [DimAccess rep])] -> Doc ann
      mapprintIdxExpr [] = ""
      mapprintIdxExpr [m] = printIdxExpMap m
      mapprintIdxExpr (m : mm) = printIdxExpMap m </> mapprintIdxExpr mm

      printIdxExpMap (name, mems) = "(idx)" <+> pretty name <+> ":" </> indent 4 (printDimAccess mems)

      printDimAccess :: [DimAccess rep] -> Doc ann
      printDimAccess dim_accesses = stack $ zipWith (curry printDim) [0 ..] dim_accesses

      printDim :: (Int, DimAccess rep) -> Doc ann
      printDim (i, m) = pretty i <+> ":" <+> indent 0 (pretty m)

instance Pretty (DimAccess rep) where
  pretty dim_access =
    -- Instead of using `brackets $` we manually enclose with `[`s, to add
    -- spacing between the enclosed elements
    if case originalVar dim_access of
      Nothing -> True
      Just n ->
        length (dependencies dim_access) == 1 && n == head (map fst $ M.toList $ dependencies dim_access)
        -- Only print the original name if it is different from the first (and single) dependency
      then
        "dependencies"
          <+> equals
          <+> align (prettyDeps $ dependencies dim_access)
      else
        "dependencies"
          <+> equals
          <+> pretty (originalVar dim_access)
          <+> "->"
          <+> align (prettyDeps $ dependencies dim_access)
    where
      prettyDeps = braces . commasep . map printPair . M.toList
      printPair (name, Dependency lvl vtype) = pretty name <+> pretty lvl <+> pretty vtype

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

instance Pretty VarType where
  pretty ConstType = "const"
  pretty Variable = "var"
  pretty ThreadID = "tid"
  pretty LoopVar = "iter"
