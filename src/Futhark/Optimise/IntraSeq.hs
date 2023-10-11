{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Futhark.Optimise.IntraSeq (intraSeq) where

import Language.Futhark.Core
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
            runReaderT (seqStms (VName "" 1 ) stms) scope

seqStms :: VName -> Stms GPU -> SeqM (Stms GPU)
seqStms seq_name stms =
  localScope (scopeOf stms) $
    mconcat <$> mapM (seqStm seq_name) (stmsToList stms)

seqBody :: VName -> KernelBody GPU -> SeqM (KernelBody GPU)
seqBody seq_name (KernelBody dec stms result) = do
  stms' <- seqStms seq_name stms
  pure $ KernelBody dec stms' result

-- seq outer map, divide group size by sequentialization factor
seqStm :: VName -> Stm GPU -> SeqM (Stms GPU)
seqStm seq_name (Let pat aux (Op (SegOp (SegMap
        lvl@(SegGroup virt (Just (KernelGrid num_groups (Count group_size))))
        space ts kbody)))) = do
  (gr_name, gr_stm) <- runBuilder $ bindNewGroupSize group_size
  let lvl' = SegGroup virt (Just $ KernelGrid num_groups $ Count (Var gr_name))
  kbody' <- seqBody seq_name kbody
  pure $ gr_stm <> oneStm (Let pat aux (Op (SegOp (SegMap lvl' space ts kbody'))))

seqStm seq_name (Let pat aux (Op (SegOp (SegRed lvl@SegThread {} space binops ts kbody)))) = do
  -- add segmap to perform the per thread reduction
  -- TODO handle other TypeBase cases?? Should always be array tho
  let (Prim tp) = head ts
  map_ident <- newIdent "inner_map" (Array tp (Shape [Var seq_name]) mempty)
  let map_stm = mkLet' [map_ident] aux (Op (SegOp (SegMap lvl space ts kbody)))
  

  pure $ stmsFromList[map_stm, Let pat aux (Op (SegOp (SegRed lvl space binops ts kbody)))]
  -- runBuilder_ $ do
  -- let SegBinOp comm lamb neut _ = head binops
  -- wrap lambda body inside thread local reduce over chunk elements
  -- might need to change types
  -- vname <- newVName "iterator"
  -- the body is just load in chunk and run the lambda body from binops i suppose
  -- loopbody <- runBodyBuilder $ 
  -- kbody' <- letExp "inner_red" $ 
  --   Loop [] (ForLoop vname Int64 seqFactor []) loopbody 
  -- add map that loops over 1/4
  -- tid_map <- letExp "tid_map" $ Op (SegOp (SegMap lvl space ts kbody'))

seqStm seq_name (Let pat aux (Op (SegOp (SegScan lvl@SegThread {} space binops ts kbody)))) = do
  pure $ oneStm $ Let pat aux (Op (SegOp (SegScan lvl space binops ts kbody)))

seqStm seq_name (Let pat aux (Op (SegOp (SegHist lvl@SegThread {} space binops ts kbody)))) = do
  pure $ oneStm $ Let pat aux (Op (SegOp (SegHist lvl space binops ts kbody)))

seqStm seq_name stm = pure $ oneStm stm


