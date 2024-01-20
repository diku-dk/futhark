{-# LANGUAGE TypeFamilies #-}

-- | We require that entry points return arrays in row-major order.
-- "Futhark.Pass.ExplicitAllocations" is conservative and inserts
-- copies to ensure this is the case.  After simplification, it may
-- turn out that those copies are redundant.  This pass removes them.
-- It's a pretty simple pass, as it only has to look at the top level
-- of entry points.
module Futhark.Optimise.EntryPointMem
  ( entryPointMemGPU,
    entryPointMemMC,
    entryPointMemSeq,
  )
where

import Data.List (find)
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.IR.GPUMem (GPUMem)
import Futhark.IR.MCMem (MCMem)
import Futhark.IR.Mem
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.IR.SeqMem (SeqMem)
import Futhark.Pass
import Futhark.Pass.ExplicitAllocations.GPU ()

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

toSubExp :: TPrimExp t VName -> Maybe SubExp
toSubExp (TPrimExp (LeafExp v _)) = Just $ Var v
toSubExp (TPrimExp (ValueExp v)) = Just $ Constant v
toSubExp _ = Nothing

optimiseFun :: (Mem rep inner) => Table rep -> FunDef rep -> FunDef rep
optimiseFun consts_table fd =
  fd {funDefBody = onBody $ funDefBody fd}
  where
    fun_ret = funDefRetType fd
    table = consts_table <> mkTable (bodyStms (funDefBody fd))
    -- XXX: we are assuming that a given offset and memory return is
    -- only used for a single array, and not for anything else.
    mkSubst i (Var arr0) (MemArray _ _ _ (ReturnsNewBlock _ mem_ext lmad0))
      | Just (MemArray _ _ _ (ArrayIn _ _), BasicOp (Manifest _ arr1)) <-
          varInfo arr0 table,
        Just (MemArray _ _ _ (ArrayIn mem2 lmad2), _) <-
          varInfo arr1 table,
        Just (TPrimExp (LeafExp (Ext offset_ext) _)) <-
          LMAD.isDirect lmad0,
        Just offset <-
          toSubExp =<< LMAD.isDirect lmad2 =
          [(mem_ext, Var mem2), (offset_ext, offset), (i, Var arr1)]
    mkSubst _ _ _ = mempty
    onBody (Body dec stms res) =
      let substs =
            mconcat . zipWith3 mkSubst [0 ..] (map resSubExp res) $
              map fst fun_ret
          applySubst i r =
            r {resSubExp = fromMaybe (resSubExp r) $ lookup i substs}
       in Body dec stms $ zipWith applySubst [0 ..] res

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
