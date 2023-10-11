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
import qualified Data.Map as M

type SeqM = ReaderT (Scope GPU) (State VNameSource)
type Env = M.Map SubExp SubExp
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
            runReaderT (seqStms M.empty stms) scope

seqStms :: Env -> Stms GPU -> SeqM (Stms GPU)
seqStms env stms =
  localScope (scopeOf stms) $
    mconcat <$> mapM (seqStm env) (stmsToList stms)

seqBody :: Env -> KernelBody GPU -> SeqM (KernelBody GPU)
seqBody env (KernelBody dec stms result) = do
  stms' <- seqStms env stms
  pure $ KernelBody dec stms' result

-- seq outer map, divide group size by sequentialization factor
seqStm :: Env -> Stm GPU -> SeqM (Stms GPU)
seqStm env (Let pat aux (Op (SegOp (SegMap
        lvl@(SegGroup virt (Just (KernelGrid num_groups (Count group_size))))
        space ts kbody)))) = do
  (gr_name, gr_stm) <- runBuilder $ bindNewGroupSize group_size
  let lvl' = SegGroup virt (Just $ KernelGrid num_groups $ Count (Var gr_name))
  kbody' <- seqBody (M.insert group_size (Var gr_name) env) kbody
  pure $ gr_stm <> oneStm (Let pat aux (Op (SegOp (SegMap lvl' space ts kbody'))))

seqStm env (Let pat aux (Op (SegOp (SegRed lvl@SegThread {} space binops ts kbody)))) = do
  -- add segmap to perform the per thread reduction
  -- TODO handle other cases for arrays below, currently we always get head 
  let (Prim tp) = head ts
  let (SegSpace phys ((gtid, bound):ps)) = space
  -- TODO monads??
  phys' <- newName phys
  gtid' <- newName gtid
  let env' = M.insert (Var gtid) (Var gtid') env
  -- TODO handle nothing
  let Just sh_size = M.lookup bound env
  let map_space = SegSpace phys' ((gtid', sh_size) : ps)
  map_ident <- newIdent "inner_map" (Array tp (Shape [sh_size]) mempty)

  -- TODO handle and update kernelbody
  let map_stm = mkLet' [map_ident] aux (Op (SegOp (SegMap lvl map_space ts kbody)))
  
  let red_space = SegSpace phys ((gtid, sh_size) : ps)
  let red_stm = Let pat aux (Op (SegOp (SegRed lvl red_space binops ts kbody)))
  
  -- TODO let a = count number of statements in kernelbody
  -- let b = subtract accumulators from arugments to reduce lambda
  -- kernelbody stms not used in reduce = a - b 

  pure $ stmsFromList [map_stm, red_stm]
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

seqStm env (Let pat aux (Op (SegOp (SegScan lvl@SegThread {} space binops ts kbody)))) = do
  pure $ oneStm $ Let pat aux (Op (SegOp (SegScan lvl space binops ts kbody)))

seqStm env (Let pat aux (Op (SegOp (SegHist lvl@SegThread {} space binops ts kbody)))) = do
  pure $ oneStm $ Let pat aux (Op (SegOp (SegHist lvl space binops ts kbody)))

seqStm env stm = pure $ oneStm stm


