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

-- | Collect all features of memory access together
data MemoryAccessPattern = MemoryAccessPattern
  { -- | Expression reference that is used to index into a given dimension
    idxExpr :: VName,
    iterationType :: IterationType,
    pattern :: Pattern,
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
      -- merge results
      foldl' mergeMemAccTable M.empty
        -- map analyzation over stmts
        . fmap analyseStm
        -- functionBody -> [stm]
        . stmsToList
        . bodyStms
        . funDefBody
        $ f

-- Concat the list off array access (note, access != dimensions)
mergeMemAccTable :: ArrayIndexDescriptors -> ArrayIndexDescriptors -> ArrayIndexDescriptors
mergeMemAccTable = M.unionWith (++)

-- TODO:
-- Add patterns here
analyseStm :: Stm GPU -> ArrayIndexDescriptors
-- Recurse into cases / conditional branches
analyseStm (Let _ _ (Match _ _ b _)) =
  foldl' mergeMemAccTable M.empty $
    fmap analyseStm . stmsToList . bodyStms $
      b
-- TODO: investigate whether we need the remaining patterns in (Pat (p:_))
analyseStm (Let (Pat (p : _)) _ (BasicOp o)) = M.singleton (patElemName p) [analyseOp o]
analyseStm (Let _ _ (Op (SegOp o))) =
  foldl' mergeMemAccTable M.empty
    . fmap analyseStm
    . stmsToList
    . kernelBodyStms
    $ segBody o
-- analyseStm (Let (Pat pp) _ _) =
--  foldl' mergeMemAccTable M.empty
--    $ map (\n -> M.singleton (patElemName n) []) pp

analyseStm _ = M.empty

analyseOp :: BasicOp -> [MemoryAccessPattern]
analyseOp (Index name (Slice unslice)) =
  map
    ( \case
        (DimFix (Constant _)) ->
          MemoryAccessPattern
            (VName "const" 0)
            Sequential
            Linear
            Invariant
        (DimFix (Var n)) ->
          MemoryAccessPattern
            n
            Sequential
            Linear
            Variant
        -- FIXME
        _ ->
          MemoryAccessPattern
            name
            Sequential
            Linear
            Variant
    )
    unslice
analyseOp _ = []

-- Pretty printing stuffs
instance Pretty FunAids where
  pretty = stack . map f . M.toList :: FunAids -> Doc ann
    where
      f (entryName, aids) = pretty entryName </> indent 2 (pretty aids) -- pretty arr

instance Pretty ArrayIndexDescriptors where
  pretty = stack . map f . M.toList :: ArrayIndexDescriptors -> Doc ann
    where
      f (n, maps) = pretty n <+> pretty maps

instance Pretty MemoryAccessPattern where
  pretty (MemoryAccessPattern idx _t _p v) =
    let var = case v of
          Variant -> "VAR"
          Invariant -> "INV"
     in brackets $ pretty idx <+> "|" <+> var
