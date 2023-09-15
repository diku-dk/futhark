module Futhark.Analysis.AccessPattern
  ( analyzeMemoryAccessPatterns,
    analyzeFunction,
    analyzeStm,
    ArrayIndexDescriptors,
    IterationType,
    Variance,
    ArrayName,
    SegMapName,
    IndexExprName,
    MemoryAccessPattern,
    names,
  )
where

import Control.Monad
import Data.IntMap.Strict qualified as S
-- import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Futhark.IR.GPU
import Futhark.IR.Prop.Names
import Futhark.Util.Pretty

-- | Iteration type describes whether the index is iterated in a parallel or
-- sequential way, ie. if the index expression comes from a sequential or
-- parallel construct, like foldl or map.
data IterationType
  = Sequential
  | Parallel
  deriving (Eq, Ord, Show)

-- | Set of VNames of gtid's that some access is variant to
newtype Variance = Variance {names :: Names}
  deriving (Eq, Ord, Show)

type ArrayName = VName

type SegMapName = VName

type IndexExprName = VName

instance Semigroup Variance where
  Variance v0 <> Variance v1 = Variance $ v0 <> v1

instance Monoid Variance where
  mempty = Variance mempty

-- | Collect all features of memory access together
data MemoryAccessPattern = MemoryAccessPattern
  { -- | Set of gtid's that the access is variant to.
    -- | Empty set means that the access is invariant.
    variances :: Variance,
    -- | Whether the acess is parallel or sequential
    iterType :: IterationType
  }
  deriving (Eq, Ord, Show)

isInv :: MemoryAccessPattern -> Bool
isInv (MemoryAccessPattern (Variance n) _) = S.null $ namesIntMap n

isVar :: MemoryAccessPattern -> Bool
isVar = not . isInv

-- | Each element in the list corresponds to a dimension in the given array
type MemoryEntry = [MemoryAccessPattern]

-- | We map variable names of arrays to lists of memory access patterns.
type ArrayIndexDescriptors =
  M.Map SegMapName (M.Map ArrayName (M.Map IndexExprName [MemoryEntry]))

-- segmap(pattern) => A(pattern) => indexExpressionName(pattern) => [MemoryAccessPattern]
--
-- segmap_0 (x,y)
--   let as_1  = A[x,y,0]
--   let res_2 = as + A[y,0,x]
--   in res_2
--
-- ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
--
-- {
--   "segmap_0": {
--     "A": {
--        as_1: [x,y,0],
--        res_2 : [y,0,x]
--     }
--   }
-- }

data Entry = Entry

data CtxVal = CtxVal
  { deps :: Either Names Entry,
    iterrationType :: IterationType
  }

-- | A mapping from patterns occuring in Let expressions to their corresponding
-- variance.
type Context = M.Map VName CtxVal

instance Semigroup MemoryAccessPattern where
  (<>) :: MemoryAccessPattern -> MemoryAccessPattern -> MemoryAccessPattern
  (<>)
    (MemoryAccessPattern (Variance avars) atypes)
    (MemoryAccessPattern (Variance bvars) btypes)
      | atypes == btypes =
          MemoryAccessPattern (Variance $ (<>) avars bvars) atypes
      | otherwise =
          undefined

-- | Extend a context with another context.
-- We never have to consider the case where VNames clash in the context, since
-- they are unique.
extend :: Context -> Context -> Context
extend = M.union

-- | Create a singular context from a parameter
contextFromParam :: IterationType -> FParam GPU -> CtxVal -> Context
contextFromParam i p = M.singleton (paramName p)

-- type t = loop | gpuOp | funcdef

-- | Create a context from a list of parameters
contextFromParams :: IterationType -> [FParam GPU] -> CtxVal -> Context
contextFromParams i pp n =
  foldl (<>) mempty $
    map (\p -> contextFromParam i p n) pp

-- | For each `entry` we return a tuple of (function-name and AIDs)
analyzeMemoryAccessPatterns :: Prog GPU -> ArrayIndexDescriptors
analyzeMemoryAccessPatterns = foldMap analyzeFunction . progFuns

analyzeFunction :: FunDef GPU -> ArrayIndexDescriptors
analyzeFunction func =
  let stms = stmsToList . bodyStms $ funDefBody func
   in let ctx =
            contextFromParams Sequential (funDefParams func) $ CtxVal {deps = Right Entry, iterrationType = Sequential}
       in analyzeStm ctx stms

analyzeStm :: Context -> [Stm GPU] -> ArrayIndexDescriptors
analyzeStm = undefined

instance Pretty ArrayIndexDescriptors where
  pretty = stack . map f . M.toList :: ArrayIndexDescriptors -> Doc ann
    where
      mapprint :: [(ArrayName, e)] -> Doc ann
      mapprint [] = ""
      mapprint [m] = memoryEntryPrint m
      mapprint (m : mm) = memoryEntryPrint m </> mapprint mm

      -- memoryEntryPrint = hsep . map pretty
      memoryEntryPrint (name, b) = pretty $ baseName name
      f (name, maps) = pretty name </> indent 2 (mapprint $ M.toList maps)

instance Pretty MemoryAccessPattern where
  pretty (MemoryAccessPattern variances iterType) =
    -- Instead of using `brackets $` we manually enclose with `[`s, to add
    -- spacing between the enclosed elements
    "[\n variances = " <+> pretty variances <+> "\n iterType:" <+> pretty iterType <+> "]"

instance Pretty IterationType where
  pretty Sequential = "seq"
  pretty Parallel = "par"

instance Pretty Variance where
  pretty (Variance names) = pretty names