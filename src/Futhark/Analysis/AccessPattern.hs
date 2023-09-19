module Futhark.Analysis.AccessPattern
  ( analyzeDimIdxPats,
    analyzeFunction,
    analyzeStm,
    ArrayIndexDescriptors,
    IterationType,
    Variance,
    ArrayName,
    SegMapName,
    IndexExprName,
    DimIdxPat,
  )
where

import Data.Foldable
import Data.IntMap.Strict qualified as S
import Data.Map.Strict qualified as M
import Futhark.IR.GPU
import Futhark.Util.Pretty

-- | Iteration type describes whether the index is iterated in a parallel or
-- sequential way, ie. if the index expression comes from a sequential or
-- parallel construct, like foldl or map.
data IterationType
  = Sequential
  | Parallel
  deriving (Eq, Ord, Show)

-- | Set of VNames of gtid's that some access is variant to.
-- Tuple of patternName and nested `level` it is created at.
type Variance = S.IntMap (VName, Int)

type ArrayName = VName

type SegMapName = (Int, VName)

type LoopBodyName = (Int, VName)

type CondBodyName = (Int, VName)

type IndexExprName = VName

-- | Collect all features of access to a specific dimension of an array.
data DimIdxPat = DimIdxPat
  { -- | Set of gtid's that the access is variant to.
    -- An empty set indicates that the access is invariant.
    variances :: Variance,
    -- | Whether the acess is parallel or sequential
    iterType :: IterationType
  }
  deriving (Eq, Ord, Show)

isInv :: DimIdxPat -> Bool
isInv (DimIdxPat vars _) = S.null vars

isVar :: DimIdxPat -> Bool
isVar = not . isInv

-- | Each element in the list corresponds to an access to a dimension in the given array
-- in the order of the dimensions.
type MemoryEntry = [DimIdxPat]

-- | Map variable names of arrays in a segmap to index expressions on those arrays.
type ArrayIndexDescriptors =
  M.Map SegMapName (M.Map ArrayName (M.Map IndexExprName [MemoryEntry]))

-- segmap(pattern) => A(pattern) => indexExpressionName(pattern) => [DimIdxPat]
--

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
    iterationType :: IterationType
  }

(><) :: Context -> CtxVal -> CtxVal -> CtxVal
(><) _ctx (CtxVal lnames _ltype) (CtxVal rnames rtype) =
  -- TODO: Do some lookups in context
  -- TODO: Consider: do we need to do some combination of ltype and rtype?
  CtxVal ((<>) lnames rnames) rtype

data BodyType
  = SegMapName SegMapName
  | LoopBodyName LoopBodyName
  | CondBodyName CondBodyName

-- | Used during the analysis to keep track of the dependencies of patterns
-- encountered so far.
data Context = Context
  { -- | A mapping from patterns occuring in Let expressions to their dependencies
    --  and iteration types.
    assignments :: M.Map VName CtxVal,
    -- | A list of the segMaps encountered during the analysis in the order they
    -- were encountered.
    lastBodyType :: [BodyType],
    -- | Current level of recursion
    currentLevel :: Int
  }

instance Monoid Context where
  mempty =
    Context
      { assignments = mempty,
        lastBodyType = [],
        currentLevel = 0
      }

instance Semigroup Context where
  (<>)
    (Context ass0 lastSegMap0 lvl0)
    (Context ass1 lastSegMap1 lvl1) =
      Context ((<>) ass0 ass1) ((++) lastSegMap0 lastSegMap1) $ max lvl0 lvl1

instance Semigroup DimIdxPat where
  (<>) :: DimIdxPat -> DimIdxPat -> DimIdxPat
  (<>)
    (DimIdxPat avars atypes)
    (DimIdxPat bvars btypes)
      | atypes == btypes =
          DimIdxPat ((<>) avars bvars) atypes
      | otherwise =
          error "Oh no!"

-- | Extend a context with another context.
-- We never have to consider the case where VNames clash in the context, since
-- they are unique.
extend :: Context -> Context -> Context
extend = (<>)

oneContext :: VName -> CtxVal -> [BodyType] -> Context
oneContext name ctxValue segmaps =
  Context
    { assignments = M.singleton name ctxValue,
      lastBodyType = segmaps,
      currentLevel = 0
    }

-- | Create a singular context from a parameter
contextFromParam :: IterationType -> FParam GPU -> CtxVal -> Context
contextFromParam _i p v = oneContext (paramName p) v []

-- | Create a singular context from a segspace
contextFromSegSpace :: SegMapName -> SegSpace -> Context
contextFromSegSpace segspaceName segspace =
  foldl' (\acc (name, _subexpr) -> extend acc $ oneContext name ctxVal []) mempty $
    unSegSpace segspace
  where
    ctxVal = CtxVal (oneName $ snd segspaceName) Parallel

-- | Create a context from a list of parameters
contextFromParams :: IterationType -> [FParam GPU] -> CtxVal -> Context
contextFromParams iterType pats name =
  foldl extend mempty $
    map (\pat -> contextFromParam iterType pat name) pats

-- | Analyze each `entry` and accumulate the results.
analyzeDimIdxPats :: Prog GPU -> ArrayIndexDescriptors
analyzeDimIdxPats = foldMap analyzeFunction . progFuns

-- | Analyze each statement in a function body.
analyzeFunction :: FunDef GPU -> ArrayIndexDescriptors
analyzeFunction func =
  let stms = stmsToList . bodyStms $ funDefBody func
   in let ctx =
            contextFromParams Sequential (funDefParams func) $
              -- All entries are "sequential" in nature.
              CtxVal {deps = mempty, iterationType = Sequential}
       in analyzeStms ctx stms

-- | Analyze each statement in a list of statements.
analyzeStms :: Context -> [Stm GPU] -> ArrayIndexDescriptors
analyzeStms ctx (stm : stms) =
  let (stm_ctx, res) = analyzeStm ctx stm
   in M.union res $ analyzeStms stm_ctx stms
analyzeStms _ [] = M.empty

-- | Analyze a statement
analyzeStm :: Context -> Stm GPU -> (Context, ArrayIndexDescriptors)
analyzeStm ctx (Let pats _ e) =
  case e of
    (BasicOp (Index name (Slice _e))) -> do
      let last_segmap = VName "REEEEE" 0
      let test_n = VName "gtid" 1337
      let memory_entries =
            [ [ DimIdxPat (S.fromList [(0, (VName "gtid" 1337, 42)), (1, (VName "gtid" 1234, 0)), (2, (VName "gtid" 0101, 5))]) $ getIterationType ctx,
                DimIdxPat (S.fromList [(0, (VName "i" 1337, 0)), (1, (VName "j" 1337, 1)), (2, (VName "k" 1337, 2))]) $ getIterationType ctx,
                DimIdxPat (S.fromList [(0, (VName "i" 1337, 2)), (1, (VName "j" 1337, 0)), (2, (VName "k" 1337, 1))]) $ getIterationType ctx
              ]
            ]
      -- Index expression name
      let idx_expr_name = pat
      -- Map from index expression name to memory entries
      let map_ixd_expr = M.singleton idx_expr_name memory_entries <> M.singleton (VName "some_idx" 0) memory_entries
      -- Map from array name to index expression map
      let map_array = M.singleton name map_ixd_expr <> M.singleton (VName "some_arr" 0) map_ixd_expr
      -- Map from segmap name to array map
      let res = M.singleton (42, last_segmap) map_array
      (ctx, res)

    -- error "UNHANDLED: Index"
    (BasicOp op) ->
      -- TODO: Lookup basicOp
      let ctx' = extend ctx $ oneContext pat (analyzeBasicOp ctx op) []
       in (ctx', mempty)
    (Match _subexps _cases _defaultBody _) -> error "UNHANDLED: Match"
    (Loop _bindings loop body) -> -- error "UNHANDLED: Loop"
      do
        let ctx' = case loop of
                  (WhileLoop iterVar) -> bindCtxVal iterVar
                  (ForLoop iterVar _ numIter loopParams) ->
                    bindCtxVal iterVar
        foldBody ctx' body

      where
        bindCtxVal name =
          Context {
            assignments = M.singleton name (CtxVal (oneName pat) Sequential),
            lastBodyType = [LoopBodyName (currentLevel ctx, pat)],
            currentLevel = currentLevel ctx + 1
          }
    (Apply _name _ _ _) -> error "UNHANDLED: Apply"
    (WithAcc _ _) -> error "UNHANDLED: With"
    -- \| analyzeSegOp does not extend ctx
    (Op (SegOp op)) -> (ctx, analyzeSegOp ctx pat op)
    (Op (SizeOp sizeop)) -> do
      let subexprsToContext =
            extend ctx . concatCtxVal . map (analyzeSubExpr ctx)
      let ctx' =
            case sizeop of
              (CmpSizeLe _name _class subexp) -> subexprsToContext [subexp]
              (CalcNumGroups lsubexp _name rsubexp) -> subexprsToContext [lsubexp, rsubexp]
              _ -> ctx
      (ctx', mempty)
    (Op (GPUBody _ body)) -> foldBody ctx body
    (Op (OtherOp _)) -> error "UNHANDLED: OtherOp"
  where
    pat = patElemName . head $ patElems pats

    concatCtxVal [] = mempty :: Context
    concatCtxVal (ne : cvals) = oneContext pat (foldl' (ctx ><) ne cvals) []

    foldBody ctxExtended body =
      do
        -- 0. Recurse into body with ctx
        (ctx'', aids) <-
          pure
            -- TODO: This does essentially the same thing as analyzeStms, but
            -- without discarding the result. A nice fix would be to refactor
            -- analyzeStms s.t. we can just reuse it here.
            . foldl'
              (\(c, r) stm -> let (c', r') = analyzeStm c stm in (c', M.union r r'))
              (ctxExtended, mempty)
            $ stmsToList
            $ bodyStms body
        -- 1. We do not want the returned context directly.
        --    however, we do want pat to map to the names what was hit in body.
        --    therefore we need to subtract the old context from the returned one,
        --    and discard all the keys within it.
        let ctxVals = M.difference (assignments ctx'') (assignments ctxExtended)
        -- 2. We are ONLY interested in the rhs of assignments (ie. the
        --    dependencies of pat :) )
        let ctx' = concatCtxVal . map snd $ M.toList ctxVals
        -- 3. Now we have the correct context and result
        (ctx', aids)

-- | Analyze a SegOp
analyzeSegOp :: Context -> VName -> SegOp lvl GPU -> ArrayIndexDescriptors
analyzeSegOp ctx segmapname (SegMap _lvl idxSpace _types kbody) =
  -- Add segmapname to ctx
  do
    let ctx'' =
          Context
            { assignments = mempty,
              lastBodyType = [SegMapName (currentLevel ctx, segmapname)],
              currentLevel = currentLevel ctx + 1
            }
    let segSpaceContext = contextFromSegSpace (currentLevel ctx, segmapname) idxSpace
    -- `extend` is associative, so the order matters. (in regards to `lastBodyType`)
    let ctx' = extend ctx $ extend ctx'' segSpaceContext
    analyzeStms ctx' $ stmsToList $ kernelBodyStms kbody
-- In all other cases we just recurse, with extended context of the segspace
-- TODO: Future improvement: Add other segmented operations to the context, to
-- reveal more nested parallel patterns
analyzeSegOp ctx segmapname segop =
  let segSpaceContext =
        contextFromSegSpace (currentLevel ctx, segmapname) $
          segSpace segop
   in analyzeStms (extend ctx segSpaceContext) . stmsToList . kernelBodyStms $ segBody segop

getIterationType :: Context -> IterationType
getIterationType (Context _ bodies _) =
  getIteration_rec bodies
  where
    getIteration_rec [] = Sequential
    getIteration_rec rec =
      case last rec of
        SegMapName _ -> Parallel
        LoopBodyName _ -> Sequential
        -- We can't really trust cond/match to be sequential/parallel, so
        -- recurse a bit ya kno
        CondBodyName _ -> getIteration_rec $ init rec

analyzeSubExpr :: Context -> SubExp -> CtxVal
analyzeSubExpr ctx (Constant _) = CtxVal mempty $ getIterationType ctx
analyzeSubExpr ctx (Var v) =
  case M.lookup v (assignments ctx) of
    Nothing -> error $ "Failed to lookup variable \"" ++ baseString v
    -- If variable is found, the dependenies must be the superset
    (Just (CtxVal deps _)) -> CtxVal (oneName v <> deps) $ getIterationType ctx

analyzeBasicOp :: Context -> BasicOp -> CtxVal
analyzeBasicOp ctx expression =
  case expression of
    (SubExp subexp) -> analyzeSubExpr ctx subexp
    (Opaque _ subexp) -> analyzeSubExpr ctx subexp
    (ArrayLit subexps _t) -> concatCtxVals mempty subexps
    (UnOp _ subexp) -> analyzeSubExpr ctx subexp
    (BinOp _ lsubexp rsubexp) -> concatCtxVals mempty [lsubexp, rsubexp]
    (CmpOp _ lsubexp rsubexp) -> concatCtxVals mempty [lsubexp, rsubexp]
    (ConvOp _ subexp) -> analyzeSubExpr ctx subexp
    (Assert subexp _ _) -> analyzeSubExpr ctx subexp
    (Index name _) ->
      error $ "unhandled: Index (Skill issue?) " ++ baseString name
    (Update _ name _slice _subexp) ->
      error $ "unhandled: Update (technically skill issue?)" ++ baseString name
    -- Technically, do we need this case?
    (Concat _ _ length_subexp) -> analyzeSubExpr ctx length_subexp
    (Manifest _dim _name) ->
      error "unhandled: Manifest"
    (Iota end_subexp start_subexp stride_subexp _) -> concatCtxVals mempty [end_subexp, start_subexp, stride_subexp]
    (Replicate (Shape shape_subexp) subexp) -> concatCtxVals mempty (subexp : shape_subexp)
    (Scratch _ subexprs) -> concatCtxVals mempty subexprs
    (Reshape _ (Shape shape_subexp) name) -> concatCtxVals (oneName name) shape_subexp
    (Rearrange _ name) -> CtxVal (oneName name) $ getIterationType ctx
    (UpdateAcc name lsubexprs rsubexprs) -> concatCtxVals (oneName name) (lsubexprs ++ rsubexprs)
    _ -> error "unhandled: match-all"
  where
    concatCtxVals ne =
      foldl' (\a -> (><) ctx a . analyzeSubExpr ctx) (CtxVal ne $ getIterationType ctx)

-- Pretty printing

instance Pretty ArrayIndexDescriptors where
  pretty = stack . map f . M.toList :: ArrayIndexDescriptors -> Doc ann
    where
      f ((_, name), maps) = "(segmap)" <+> pretty name <+> ": {" </> indent 4 (mapprintArray $ M.toList maps) </> "}"

      mapprintArray :: [(ArrayName, M.Map IndexExprName [MemoryEntry])] -> Doc ann
      mapprintArray [] = ""
      mapprintArray [m] = printArrayMap m
      mapprintArray (m : mm) = printArrayMap m </> mapprintArray mm

      printArrayMap (name, maps) = "(arr)" <+> pretty (baseName name) <+> ": {" </> indent 4 (mapprintIdxExpr (M.toList maps)) </> "}"

      mapprintIdxExpr :: [(IndexExprName, [MemoryEntry])] -> Doc ann
      mapprintIdxExpr [] = ""
      mapprintIdxExpr [m] = printIdxExpMap m
      mapprintIdxExpr (m : mm) = printIdxExpMap m </> mapprintIdxExpr mm

      printIdxExpMap (name, mems) = "(idx)" <+> pretty (baseName name) <+> ":" </> indent 4 (printMemoryEntryList mems)

      printMemoryEntryList :: [MemoryEntry] -> Doc ann
      printMemoryEntryList [] = ""
      printMemoryEntryList [m] = printMemoryEntry m 0
      printMemoryEntryList (m : mm) = printMemoryEntry m 0 </> printMemoryEntryList mm

      printMemoryEntry :: MemoryEntry -> Int -> Doc ann
      printMemoryEntry [] _ = ""
      printMemoryEntry [m] idx = printDim m idx
      printMemoryEntry (m : mm) idx = printDim m idx </> printMemoryEntry mm (idx + 1)

      printDim m idx = pretty idx <+> ":" <+> indent 0 (pretty m)

instance Pretty DimIdxPat where
  pretty (DimIdxPat variances iterType) =
    -- Instead of using `brackets $` we manually enclose with `[`s, to add
    -- spacing between the enclosed elements
    "variances =" <+> "[" <+> pretty variances <+> "]" </> "iterType  =" <+> pretty iterType

instance Pretty IterationType where
  pretty Sequential = "seq"
  pretty Parallel = "par"

instance Pretty Variance where
  pretty = (hsep . punctuate " |") . map (printPair . snd) . S.toList
    where
      printPair (name, lvl) = pretty name <+> pretty lvl
