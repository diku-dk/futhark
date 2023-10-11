{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Futhark.Optimise.IntraSeq (intraSeq) where

import Futhark.Pass
import Futhark.IR.GPU
import Futhark.Builder.Class

import Control.Monad.Reader
import Control.Monad.State
import Debug.Trace
import Futhark.Construct
import Futhark.Analysis.PrimExp.Convert
import Debug.Pretty.Simple

type SeqM = ReaderT (Scope GPU) (State VNameSource)
-- HELPERS
seqFactor :: SubExp
seqFactor = intConst Int64 4

-- read in seq factor elements for a given thread
getChunk :: (MonadBuilder m) => VName -> SubExp -> m VName
getChunk arr tid = do
  arr_t <- lookupType arr
  slice_offset <- letSubExp "slice_offset" =<< toExp (pe64 tid * pe64 seqFactor)
  let slice = DimSlice slice_offset seqFactor (intConst Int64 1)
  letExp "chunk" $ BasicOp $ Index arr $ fullSlice arr_t [slice]

-- bind the new group size
bindNewGroupSize :: SubExp -> Builder GPU VName
bindNewGroupSize group_size = do
  name <- newVName "group_size"
  letBindNames [name] $ BasicOp $ BinOp (SDivUp Int64 Unsafe) group_size seqFactor
  pure name

-- NOTE uncomment this for pretty printing AST
-- intraSeq :: Pass GPU GPU
  -- intraSeq = Pass "test" "desc" printAst
  -- printAst :: Prog GPU -> PassM (Prog GPU)
-- printAst prog = pTrace (show prog) (pure prog)

-- TODO handle when last thread does not have seqFactor elements to process
intraSeq :: Pass GPU GPU
intraSeq =
    Pass "name" "description" $
      intraproceduralTransformation onStms
    where
      onStms scope stms =
        modifyNameSource $
          runState $
            runReaderT (seqStms stms) scope

seqStms :: Stms GPU -> SeqM (Stms GPU)
seqStms stms =
  localScope (scopeOf stms) $
    mconcat <$> mapM seqStm (stmsToList stms)

seqBody :: KernelBody GPU -> SeqM (KernelBody GPU)
seqBody (KernelBody dec stms result) = do
  stms' <- seqStms stms
  pure $ KernelBody dec stms' result

-- seq outer map, divide group size by sequentialization factor
seqStm :: Stm GPU -> SeqM (Stms GPU)
seqStm (Let pat aux (Op (SegOp (SegMap
        lvl@(SegGroup virt (Just (KernelGrid num_groups (Count group_size))))
        space ts kbody)))) = do
  (gr_name, gr_stm) <- runBuilder $ bindNewGroupSize group_size
  let lvl' = SegGroup virt (Just $ KernelGrid num_groups $ Count (Var gr_name))
  kbody' <- seqBody kbody
  pure $ gr_stm <> oneStm (Let pat aux (Op (SegOp (SegMap lvl' space ts kbody'))))

seqStm (Let pat aux (Op (SegOp (SegRed lvl@SegThread {} space binops ts kbody)))) = do
  -- add segmap to perform the per thread reduction
  map_name <- newVName "inner_map"
  -- let map_stm = oneStm $ Let map_name aux (Op (SegOp (SegMap lvl space ts kbody)))


  pure $ oneStm $ Let pat aux (Op (SegOp (SegRed lvl space binops ts kbody)))

seqStm (Let pat aux (Op (SegOp (SegScan lvl@SegThread {} space binops ts kbody)))) = do
  pure $ oneStm $ Let pat aux (Op (SegOp (SegScan lvl space binops ts kbody)))

seqStm (Let pat aux (Op (SegOp (SegHist lvl@SegThread {} space binops ts kbody)))) = do
  pure $ oneStm $ Let pat aux (Op (SegOp (SegHist lvl space binops ts kbody)))

seqStm stm = pure $ oneStm stm


