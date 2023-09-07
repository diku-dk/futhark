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

-- | Variance represents whether the index is variant, or invariant to the outer
-- kernel/iteration function.
data Variance
  = Variant IterationType
  | Invariant
  deriving (Eq, Ord, Show)

-- | DimFix'es can either be a constant expression (IntValue) or a VName to some
-- pattern.
newtype DimIndexExpression = DimIndexExpression (Either VName PrimValue)
  deriving (Eq, Ord, Show)

-- | Collect all features of memory access together
data MemoryAccessPattern = MemoryAccessPattern
  { -- | Expression reference that is used to index into a given dimension
    dimIdxExpr :: DimIndexExpression,
    variance :: Variance
  }
  deriving (Eq, Ord, Show)

-- | Each element in the list corresponds to a dimension in the given array
type MemoryEntry = [MemoryAccessPattern]

-- | We map variable names of arrays to lists of memory access patterns.
type ArrayIndexDescriptors = M.Map VName [MemoryEntry]

-- | Map `Pat` to array index descriptors
type StmtsAids = M.Map VName ArrayIndexDescriptors

-- | Map Entries to array index descriptors (mostly used for debugging)
type FunAids = M.Map Name StmtsAids

type AnalyzeCtx = M.Map VName BasicOp

-- | For each `entry` we return a tuple of (function-name and AIDs)
analyzeMemoryAccessPatterns :: Prog GPU -> FunAids
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

analyseStm :: [Stm GPU] -> AnalyzeCtx -> StmtsAids
analyseStm (stm : ss) ctx = do
  case stm of
    -- TODO: investigate whether we need the remaining patterns in (Pat (p:_))
    (Let (Pat pp) _ (BasicOp o)) ->
      let res = M.fromList $ zip (map patElemName pp) $ -- get vnames of patterns
                replicate (length pp) $
                  analyseOp o ctx -- create a result for each pattern
       in let ctx'' = M.fromList $ zip (map patElemName pp) $ replicate (length pp) o
       in let ctx' = M.union ctx ctx''
           in M.union res $ analyseStm ss ctx'
    (Let _ _ (Op (SegOp o))) ->
      analyseStm ((stmsToList . kernelBodyStms $ segBody o) ++ ss) ctx
    -- TODO:
    -- Add patterns here
    _ ->
      analyseStm ss ctx
analyseStm _ _ = M.empty

accesssPatternOfVName :: VName -> Variance -> MemoryAccessPattern
accesssPatternOfVName n = MemoryAccessPattern $ DimIndexExpression $ Left n

accesssPatternOfPrimValue :: PrimValue -> MemoryAccessPattern
accesssPatternOfPrimValue n = MemoryAccessPattern (DimIndexExpression $ Right n) Invariant

analyseOp :: BasicOp -> AnalyzeCtx -> ArrayIndexDescriptors
-- analyseOp (Index name (Slice unslice)) m =
--  map
--    ( \case
--        (DimFix (Constant primvalue)) ->
--          accesssPatternOfPrimValue primvalue Sequential
--        (DimFix (Var n)) ->
--          case n of
--            -- TODO: This is a very abusive and improper way of doing it.
--            -- We need to check the scope in which the expression was created,
--            -- and if the expression is constant.
--            VName "gtid" _ ->
--              accesssPatternOfVName n Parallel Variant
--            VName "tmp" id' ->
--              let nn = VName "tmp" id'
--               in if varContainsGtid nn m
--                    then accesssPatternOfVName nn (Variant Parallel) Variant
--                    else accesssPatternOfVName nn (Variant Sequential) Variant
--            _ ->
--              accesssPatternOfVName n Sequential Variant
--        -- FIXME with more patterns?
--        _ ->
--          accesssPatternOfVName name Sequential Variant
--    )
--    unslice
analyseOp _ _ = M.empty

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

instance Pretty StmtsAids where
  pretty = stack . map f . M.toList :: StmtsAids -> Doc ann
    where
      f (entryName, aids) = pretty entryName </> indent 2 (pretty aids) -- pretty arr

instance Pretty ArrayIndexDescriptors where
  pretty = stack . map f . M.toList :: ArrayIndexDescriptors -> Doc ann
    where
      mapprint [] = ""
      mapprint (e : ee) = pretty e <+> mapprint ee

      f (n, maps) = pretty n <+> mapprint maps

instance Pretty MemoryAccessPattern where
  pretty (MemoryAccessPattern d v) =
    brackets $ pretty d <+> "|" <+> pretty v

instance Pretty DimIndexExpression where
  pretty (DimIndexExpression (Left n)) = "σ" <+> pretty n -- sigma since it exists in our store
  pretty (DimIndexExpression (Right c)) = "τ" <+> pretty c -- tau for a term

instance Pretty IterationType where
  pretty Sequential = "seq"
  pretty Parallel = "par"

instance Pretty Variance where
  pretty (Variant t) = "ν" <+> pretty t -- v for variant
  pretty Invariant = "ψ" -- v dashed for invariant
