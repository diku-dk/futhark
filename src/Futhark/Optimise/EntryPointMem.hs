{-# LANGUAGE TypeFamilies #-}

-- | We require that entry points return arrays with zero offset in
-- row-major order.  "Futhark.Pass.ExplicitAllocations" is
-- conservative and inserts copies to ensure this is the case.  After
-- simplification, it may turn out that those copies are redundant.
-- This pass removes them.  It's a pretty simple pass, as it only has
-- to look at the top level of entry points.
module Futhark.Optimise.EntryPointMem
  ( entryPointMemGPU,
    entryPointMemMC,
    entryPointMemSeq,
  )
where

import Data.List (find)
import Data.Map.Strict qualified as M
import Futhark.IR.GPUMem (GPUMem)
import Futhark.IR.MCMem (MCMem)
import Futhark.IR.Mem
import Futhark.IR.SeqMem (SeqMem)
import Futhark.Pass
import Futhark.Pass.ExplicitAllocations.GPU ()
import Futhark.Transform.Substitute

type Table rep = M.Map VName (Stm rep)

mkTable :: Stms rep -> Table rep
mkTable = foldMap f
  where
    f stm = M.fromList $ map (,stm) (patNames (stmPat stm))

varInfo :: (Mem rep inner) => VName -> Table rep -> Maybe (LetDecMem, Exp rep)
varInfo v table = do
  Let pat _ e <- M.lookup v table
  PatElem _ info <- find ((== v) . patElemName) (patElems pat)
  Just (letDecMem info, e)

optimiseFun :: (Mem rep inner) => Table rep -> FunDef rep -> FunDef rep
optimiseFun consts_table fd =
  fd {funDefBody = onBody $ funDefBody fd}
  where
    table = consts_table <> mkTable (bodyStms (funDefBody fd))
    mkSubst (Var v0)
      | Just (MemArray _ _ _ (ArrayIn mem0 lmad0), BasicOp (Manifest _ v1)) <-
          varInfo v0 table,
        Just (MemArray _ _ _ (ArrayIn mem1 lmad1), _) <-
          varInfo v1 table,
        lmad0 == lmad1 =
          M.fromList [(mem0, mem1), (v0, v1)]
    mkSubst _ = mempty
    onBody (Body dec stms res) =
      let substs = mconcat $ map (mkSubst . resSubExp) res
       in Body dec stms $ substituteNames substs res

entryPointMem :: (Mem rep inner) => Pass rep rep
entryPointMem =
  Pass
    { passName = "Entry point memory optimisation",
      passDescription = "Remove redundant copies of entry point results.",
      passFunction = intraproceduralTransformationWithConsts pure onFun
    }
  where
    onFun consts fd = pure $ optimiseFun (mkTable consts) fd

-- | The pass for GPU representation.
entryPointMemGPU :: Pass GPUMem GPUMem
entryPointMemGPU = entryPointMem

-- | The pass for MC representation.
entryPointMemMC :: Pass MCMem MCMem
entryPointMemMC = entryPointMem

-- | The pass for Seq representation.
entryPointMemSeq :: Pass SeqMem SeqMem
entryPointMemSeq = entryPointMem
