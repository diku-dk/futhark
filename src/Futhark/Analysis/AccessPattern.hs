{-# LANGUAGE LambdaCase #-}

module Futhark.Analysis.AccessPattern
  ( analyzeMemoryAccessPatterns,
    analyseStm,
    ArrayIndexDescriptors,
  )
where

import Data.Foldable
import Data.Map.Strict qualified as M
import Futhark.IR.GPU
import Futhark.Util.Pretty

-- | Iteration type describes whether the index is iterated in a parallel or
-- sequential way, ie. if the index expression comes from a sequential or
-- parallel construct, like foldl or map.
data IterationType = Sequential | Parallel
  deriving (Eq, Ord, Show)

-- | Pattern determines what kind of physical pattern is used to index.
-- This is subject to get removed.
data Pattern = Linear | Random
  deriving (Eq, Ord, Show)

-- | Variance represents whether the index is variant, or invariant to the outer
-- kernel/iteration function.
data Variance
  = Variant
  | Invariant
  deriving (Eq, Ord, Show)

newtype DimIndexExpression = DimIndexExpression (Either VName PrimValue)
  deriving (Eq, Ord, Show)

-- | Collect all features of memory access together
data MemoryAccessPattern = MemoryAccessPattern
  { -- | Expression reference that is used to index into a given dimension
    dimIdxExpr :: DimIndexExpression,
    iterationType :: IterationType,
    pattern' :: Pattern,
    variance :: Variance
  }
  deriving (Eq, Ord, Show)

-- | Each element in the list corresponds to a dimension in the given array
type MemoryEntry = [MemoryAccessPattern]

-- | We map variable names of arrays to lists of memory access patterns.
type ArrayIndexDescriptors = M.Map VName [MemoryEntry]

type FunAids = M.Map Name ArrayIndexDescriptors

-- | For each `entry` we return a tuple of (function-name and AIDs)
analyzeMemoryAccessPatterns :: Prog GPU -> FunAids -- FunAids -- M.Map VName ArrayIndexDescriptors
-- analyzeMemoryAccessPatterns (Prog{progTypes = _, progConsts = _, progFuns = funs}) = M.empty
analyzeMemoryAccessPatterns prog =
  -- We map over the program functions (usually always entries)
  -- Then fold them together to a singular map.
  -- foldl' mergeAids M.empty .
  foldl' M.union M.empty $ getAids <$> progFuns prog

getAids :: FunDef GPU -> FunAids
getAids f = M.singleton fdname aids
  where
    fdname = funDefName f
    aids =
      flip analyseStm M.empty
        -- functionBody -> [stm]
        . stmsToList
        . bodyStms
        . funDefBody
        $ f

analyseStm :: [Stm GPU] -> M.Map VName BasicOp -> ArrayIndexDescriptors
analyseStm (stm : ss) m = do
  case stm of
    -- TODO: investigate whether we need the remaining patterns in (Pat (p:_))
    (Let (Pat (p : _)) _ (BasicOp o)) ->
      let res = M.singleton (patElemName p) [analyseOp o m]
       in -- TODO: Extend `m` with access of current let expr.
          let m' = M.union m (M.singleton (patElemName p) o)
           in M.unionWith (++) res $ analyseStm ss m'
    (Let _ _ (Op (SegOp o))) ->
      analyseStm ((stmsToList . kernelBodyStms $ segBody o) ++ ss) m
    -- TODO:
    -- Add patterns here
    _ ->
      analyseStm ss m
analyseStm _ _ = M.empty

accesssPatternOfVName :: VName -> IterationType -> Pattern -> Variance -> MemoryAccessPattern
accesssPatternOfVName n = MemoryAccessPattern $ DimIndexExpression $ Left n

accesssPatternOfPrimValue :: PrimValue -> IterationType -> Pattern -> Variance -> MemoryAccessPattern
accesssPatternOfPrimValue n = MemoryAccessPattern $ DimIndexExpression $ Right n

analyseOp :: BasicOp -> M.Map VName BasicOp -> [MemoryAccessPattern]
analyseOp (Index name (Slice unslice)) m =
  map
    ( \case
        (DimFix (Constant primvalue)) ->
          accesssPatternOfPrimValue primvalue Sequential Linear Invariant
        (DimFix (Var n)) ->
          case n of
            VName "gtid" _ ->
              accesssPatternOfVName n Parallel Linear Variant
            VName "tmp" id' ->
              let nn = VName "tmp" id'
               in if varContainsGtid nn m
                    then accesssPatternOfVName nn Parallel Linear Variant
                    else accesssPatternOfVName nn Sequential Linear Variant
            _ ->
              accesssPatternOfVName n Sequential Linear Variant
        -- FIXME with more patterns?
        _ ->
          accesssPatternOfVName name Sequential Linear Variant
    )
    unslice
analyseOp _ _ = []

varContainsGtid :: VName -> M.Map VName BasicOp -> Bool
varContainsGtid name m =
  maybe False checkGtid (M.lookup name m)
  where
    checkGtid (BinOp _ _ (Var (VName "gtid" _))) = True
    checkGtid (BinOp _ (Var (VName "gtid" _)) _) = True
    checkGtid (UnOp _ (Var (VName "gtid" _))) = True
    -- checkGtid (SegOp _ _ _ _) = True
    -- TODO: Handle more cases?
    -- TODO: Can BinOp contain another BinOp?
    checkGtid _ = False

-- Pretty printing stuffs
instance Pretty FunAids where
  pretty = stack . map f . M.toList :: FunAids -> Doc ann
    where
      f (entryName, aids) = pretty entryName </> indent 2 (pretty aids) -- pretty arr

instance Pretty ArrayIndexDescriptors where
  pretty = stack . map f . M.toList :: ArrayIndexDescriptors -> Doc ann
    where
      mapprint [] = ""
      mapprint (e : ee) = pretty e <+> mapprint ee

      f (n, maps) = pretty n <+> mapprint maps

instance Pretty MemoryAccessPattern where
  pretty (MemoryAccessPattern idx t _p v) =
    brackets $ pretty idx <+> "|" <+> pretty v <+> "|" <+> pretty t

instance Pretty DimIndexExpression where
  pretty (DimIndexExpression (Left n)) = "σ" <+> pretty n -- sigma since it exists in our store
  pretty (DimIndexExpression (Right c)) = "τ" <+> pretty c -- tau for a term

instance Pretty IterationType where
  pretty Sequential = "seq"
  pretty Parallel = "par"

instance Pretty Variance where
  pretty Variant = "ν" -- v for variant
  pretty Invariant = "ψ" -- v dashed for invariant
