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

-- | Set of VNames of gtid's that some access is variant to
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
isInv (DimIdxPat n _) = S.null n

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
(><) ctx (CtxVal lnames ltype) (CtxVal rnames rtype) =
  -- TODO: Do some lookups in context
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

zeroContext :: Context
zeroContext =
  Context
    { assignments = mempty,
      lastBodyType = [],
      currentLevel = 0
    }

-- | Create a singular context from a parameter
contextFromParam :: IterationType -> FParam GPU -> CtxVal -> Context
contextFromParam _i p v = oneContext (paramName p) v []

-- | Create a singular context from a segspace
contextFromSegSpace :: SegMapName -> SegSpace -> Context
contextFromSegSpace segspaceName s =
  foldl' (\acc n -> extend acc $ oneContext (fst n) ctxVal []) zeroContext $
    unSegSpace s
  where
    ctxVal = CtxVal (oneName $ snd segspaceName) Parallel

-- | Create a context from a list of parameters
contextFromParams :: IterationType -> [FParam GPU] -> CtxVal -> Context
contextFromParams i pp n =
  foldl extend zeroContext $
    map (\p -> contextFromParam i p n) pp

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
analyzeStm ctx (Let p _ e) =
  case e of
    (BasicOp (Index _n (Slice _e))) -> error "UNHANDLED: Index"
    (BasicOp op) ->
      -- TODO: Lookup basicOp
      let ctx' = extend ctx $ oneContext pat (analyzeBasicOp ctx op) []
       in (ctx', mempty)
    (Match _subexps _cases _defaultBody _) -> error "UNHANDLED: Match"
    (Loop _bindings _loop _body) -> error "UNHANDLED: Loop"
    (Apply _name _ _ _) -> error "UNHANDLED: Apply"
    (WithAcc _ _) -> error "UNHANDLED: With"
    -- \| analyzeSegOp does not extend ctx
    (Op (SegOp o)) -> (ctx, analyzeSegOp ctx pat o)
    (Op (SizeOp _)) -> error "UNHANDLED: SizeOp"
    (Op (GPUBody _ _)) -> error "UNHANDLED: GPUBody"
    (Op (OtherOp _)) -> error "UNHANDLED: OtherOp"
  where
    pat = patElemName . head $ patElems p

-- | Analyze a SegOp
analyzeSegOp :: Context -> VName -> SegOp lvl GPU -> ArrayIndexDescriptors
analyzeSegOp ctx segmapname (SegMap _lvl idxSpace _types kbody) =
  -- Add segmapname to ctx
  let ctx'' =
        Context
          { assignments = mempty,
            lastBodyType = [SegMapName (currentLevel ctx, segmapname)],
            currentLevel = currentLevel ctx + 1
          }
   in let segSpaceContext =
            contextFromSegSpace (currentLevel ctx, segmapname) idxSpace
       in -- `extend` is associative, so the order matters. (in regards to `lastBodyType`)
          let ctx' = extend ctx $ extend ctx'' segSpaceContext
           in analyzeStms ctx' $ stmsToList $ kernelBodyStms kbody
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
analyzeSubExpr c (Constant _) = CtxVal mempty $ getIterationType c
analyzeSubExpr c (Var v) =
  case M.lookup v (assignments c) of
    Nothing -> error $ "Failed to lookup variable \"" ++ baseString v
    -- If variable is found, the dependenies must be the superset
    (Just (CtxVal deps _)) -> CtxVal (oneName v <> deps) $ getIterationType c

analyzeBasicOp :: Context -> BasicOp -> CtxVal
analyzeBasicOp c expression =
  case expression of
    (SubExp subexp) -> analyzeSubExpr c subexp
    (Opaque _ subexp) -> analyzeSubExpr c subexp
    (ArrayLit subexps _t) -> concatCtxVals mempty subexps
    (UnOp _ subexp) -> analyzeSubExpr c subexp
    (BinOp _ lsubexp rsubexp) -> concatCtxVals mempty [lsubexp, rsubexp]
    (CmpOp _ lsubexp rsubexp) -> concatCtxVals mempty [lsubexp, rsubexp]
    (ConvOp _ subexp) -> analyzeSubExpr c subexp
    (Assert subexp _ _) -> analyzeSubExpr c subexp
    (Index name _) -> error $ "unhandled: Index (Skill issue?)" ++ baseString name
    (Update _ name _slice _subexp) -> error $ "unhandled: Update (technically skill issue?)" ++ baseString name
    -- Technically, do we need this case?
    (Concat _ _ length_subexp) -> analyzeSubExpr c length_subexp
    (Manifest _dim _name) -> error "unhandled: Manifest"
    (Iota end_subexp start_subexp stride_subexp _) -> concatCtxVals mempty [end_subexp, start_subexp, stride_subexp]
    (Replicate (Shape shape_subexp) subexp) -> concatCtxVals mempty (subexp : shape_subexp)
    (Scratch _ subexprs) -> concatCtxVals mempty subexprs
    (Reshape _ (Shape shape_subexp) name) -> concatCtxVals (oneName name) shape_subexp
    (Rearrange _ name) -> CtxVal (oneName name) $ getIterationType c
    (UpdateAcc name lsubexprs rsubexprs) -> concatCtxVals (oneName name) (lsubexprs ++ rsubexprs)
    _ -> error "unhandled: match-all"
  where
    concatCtxVals ne =
      foldl' (\a -> (><) c a . analyzeSubExpr c) (CtxVal ne $ getIterationType c)

-- Pretty printing

instance Pretty ArrayIndexDescriptors where
  pretty = stack . map f . M.toList :: ArrayIndexDescriptors -> Doc ann
    where
      mapprint :: [(ArrayName, e)] -> Doc ann
      mapprint [] = ""
      mapprint [m] = memoryEntryPrint m
      mapprint (m : mm) = memoryEntryPrint m </> mapprint mm

      -- memoryEntryPrint = hsep . map pretty
      memoryEntryPrint (name, _b) = pretty $ baseName name
      f (name, maps) = pretty name </> indent 2 (mapprint $ M.toList maps)

instance Pretty DimIdxPat where
  pretty (DimIdxPat variances iterType) =
    -- Instead of using `brackets $` we manually enclose with `[`s, to add
    -- spacing between the enclosed elements
    "[\n variances = " <+> pretty variances <+> "\n iterType:" <+> pretty iterType <+> "]"

instance Pretty IterationType where
  pretty Sequential = "seq"
  pretty Parallel = "par"

instance Pretty Variance where
  pretty = stack . map pretty . S.toList
