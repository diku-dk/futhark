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

type SegMapName = VName

type IndexExprName = VName

-- | Collect all features of memory access together
data DimIdxPat = DimIdxPat
  { -- | Set of gtid's that the access is variant to.
    -- | Empty set means that the access is invariant.
    variances :: Variance,
    -- | Whether the acess is parallel or sequential
    iterType :: IterationType
  }
  deriving (Eq, Ord, Show)

isInv :: DimIdxPat -> Bool
isInv (DimIdxPat n _) = S.null n

isVar :: DimIdxPat -> Bool
isVar = not . isInv

-- | Each element in the list corresponds to a dimension in the given array
type MemoryEntry = [DimIdxPat]

-- | We map variable names of arrays to lists of memory access patterns.
type ArrayIndexDescriptors =
  M.Map SegMapName (M.Map ArrayName (M.Map IndexExprName [MemoryEntry]))

-- segmap(pattern) => A(pattern) => indexExpressionName(pattern) => [DimIdxPat]
--

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
-- ...
--  segmap_1:
--   A:
--     as_1:
--       [q,p,i] -> [i,p,q]
--       [x,z,q]
--     as_2:
--       [[x,y],z,q]
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

-- | Only used during the analysis to keep track of the dependencies of each
-- pattern. For example, a pattern might depend on a function parameter, a
-- gtid, or some other pattern.
data CtxVal = CtxVal
  { deps :: Names,
    iterationType :: IterationType
  }

-- | A mapping from patterns occuring in Let expressions to their corresponding
-- variance.
type Context = M.Map VName CtxVal

instance Semigroup DimIdxPat where
  (<>) :: DimIdxPat -> DimIdxPat -> DimIdxPat
  (<>)
    (DimIdxPat avars atypes)
    (DimIdxPat bvars btypes)
      | atypes == btypes =
          DimIdxPat ((<>) avars bvars) atypes
      | otherwise =
          error "REEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE"

-- | Extend a context with another context.
-- We never have to consider the case where VNames clash in the context, since
-- they are unique.
extend :: Context -> Context -> Context
extend = M.union

-- | Create a singular context from a parameter
contextFromParam :: IterationType -> FParam GPU -> CtxVal -> Context
contextFromParam _i p = M.singleton (paramName p)

-- type t = loop | gpuOp | funcdef

-- | Create a context from a list of parameters
contextFromParams :: IterationType -> [FParam GPU] -> CtxVal -> Context
contextFromParams i pp n =
  foldl (<>) mempty $
    map (\p -> contextFromParam i p n) pp

-- | TODO: CHANGE THIS COMMENT
-- | For each `entry` we return a tuple of (function-name and AIDs)
analyzeDimIdxPats :: Prog GPU -> ArrayIndexDescriptors
analyzeDimIdxPats = foldMap analyzeFunction . progFuns

analyzeFunction :: FunDef GPU -> ArrayIndexDescriptors
analyzeFunction func =
  let stms = stmsToList . bodyStms $ funDefBody func
   in let ctx =
            contextFromParams Sequential (funDefParams func) $
              -- All entries are "sequential" in nature.
              CtxVal {deps = mempty, iterationType = Sequential}
       in analyzeStms ctx stms

analyzeStms :: Context -> [Stm GPU] -> ArrayIndexDescriptors
analyzeStms ctx ((Let pats _aux expr) : stms) =
  let (stm_ctx, res) = analyzeStm ctx expr
   in let ctx' = maybe ctx (extend ctx . M.singleton (patElemName . head $ patElems pats)) stm_ctx
       in M.union res $ analyzeStms ctx' stms
analyzeStms _ [] = M.empty

analyzeStm :: Context -> Exp GPU -> (Maybe CtxVal, ArrayIndexDescriptors)
analyzeStm _c (BasicOp (Index _n (Slice _e))) = error "UNHANDLED: Index"
analyzeStm _c (BasicOp _) = error "UNHANDLED: BasicOp"
analyzeStm _c (Match _subexps _cases _defaultBody _) = error "UNHANDLED: Match"
analyzeStm _c (Loop _bindings _loop _body) = error "UNHANDLED: Loop"
analyzeStm _c (Apply _name _ _ _) = error "UNHANDLED: Apply"
analyzeStm _c (WithAcc _ _) = error "UNHANDLED: With"
analyzeStm ctx (Op (SegOp o)) = analyzeSegOp ctx o
analyzeStm _c (Op (SizeOp _)) = error "sizeop"
analyzeStm _c (Op (GPUBody _ _)) = error "gpubody"
analyzeStm _c (Op (OtherOp _)) = error "otherop"

analyzeSegOp :: Context -> SegOp lvl GPU -> (Maybe CtxVal, ArrayIndexDescriptors)
analyzeSegOp ctx (SegMap lvl SegSpace _types _kbody) = error "case SegMap"
analyzeSegOp ctx (SegRed lvl SegSpace [SegBinOp rep] _types _kbody) = error "case SegRed"
analyzeSegOp ctx (SegScan lvl SegSpace [SegBinOp rep] _types _kbody) = error "case SegScan"
analyzeSegOp ctx (SegHist lvl SegSpace [HistOp rep] _types _kbody) = error "case SegHist"

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
