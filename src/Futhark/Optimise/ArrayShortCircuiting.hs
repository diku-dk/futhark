{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Playground for work on merging memory blocks
module Futhark.Optimise.ArrayShortCircuiting (printArrayShortCircuiting, printArrayShortCircuitingGPU, optimiseSeqMem, optimiseGPUMem) where

import Control.Monad.Reader
import Data.Function ((&))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Futhark.Analysis.Alias as AnlAls
import Futhark.IR.Aliases
import Futhark.IR.GPUMem
import Futhark.IR.Mem
import Futhark.IR.Mem.IxFun (substituteInIxFun)
import Futhark.IR.SeqMem
import Futhark.MonadFreshNames
import Futhark.Optimise.ArrayShortCircuiting.ArrayCoalescing
import Futhark.Optimise.ArrayShortCircuiting.DataStructs
import Futhark.Pass (Pass (..))
import qualified Futhark.Pass as Pass
import Futhark.Pipeline
import Futhark.Util

----------------------------------------------------------------
--- Printer/Tester Main Program
----------------------------------------------------------------

-- run it with:  `futhark dev --cpu --merge-mem test.fut`
printArrayShortCircuiting :: Prog SeqMem -> FutharkM ()
printArrayShortCircuiting prg = do
  coaltab <- mkCoalsTab $ AnlAls.aliasAnalysis prg
  liftIO $ putStrLn $ "COALESCING RESULT:" ++ pretty (length coaltab) ++ "\n" ++ pretty coaltab

-- run it with:  `futhark dev --gpu-mem --merge-mem test.fut`
printArrayShortCircuitingGPU :: Prog GPUMem -> FutharkM ()
printArrayShortCircuitingGPU prg = do
  coaltab <- mkCoalsTabGPU $ AnlAls.aliasAnalysis prg
  liftIO $ putStrLn $ "COALESCING RESULT:" ++ pretty (length coaltab) ++ "\n" ++ pretty coaltab

data Env inner = Env
  { envCoalesceTab :: M.Map VName Coalesced,
    onInner :: inner -> ReplaceM inner inner
  }

type ReplaceM inner a = Reader (Env inner) a

optimiseSeqMem :: Pass SeqMem SeqMem
optimiseSeqMem = pass "short-circuit" "Array Short-Circuiting" mkCoalsTab return

optimiseGPUMem :: Pass GPUMem GPUMem
optimiseGPUMem = pass "short-circuit-gpu" "Array Short-Circuiting (GPU)" mkCoalsTabGPU replaceInHostOp

pass ::
  (Mem rep inner, LetDec rep ~ LetDecMem, CanBeAliased inner) =>
  String ->
  String ->
  (Prog (Aliases rep) -> Pass.PassM CoalsTab) ->
  (inner -> ReplaceM inner inner) ->
  Pass rep rep
pass flag desc mk on_inner =
  Pass flag desc $ \prog -> do
    coaltab <- foldMap vartab . M.elems <$> mk (AnlAls.aliasAnalysis prog)
    Pass.intraproceduralTransformation (onStms coaltab) prog
  where
    onStms coaltab _ stms =
      return $ runReader (mapM replaceInStm stms) (Env coaltab on_inner)

replaceInStm :: (Mem rep inner, LetDec rep ~ LetDecMem) => Stm rep -> ReplaceM inner (Stm rep)
replaceInStm (Let (Pat elems) d e) = do
  elems' <- mapM replaceInPatElem elems
  e' <- replaceInExp elems' e
  return $ Let (Pat elems') d e'
  where
    replaceInPatElem :: PatElemT LetDecMem -> ReplaceM inner (PatElemT LetDecMem)
    replaceInPatElem p@(PatElem vname (MemArray _ _ u _)) =
      fromMaybe p <$> lookupAndReplace vname PatElem u
    replaceInPatElem p = return p

replaceInExp :: (Mem rep inner, LetDec rep ~ LetDecMem) => [PatElemT LetDecMem] -> Exp rep -> ReplaceM inner (Exp rep)
replaceInExp _ e@(BasicOp _) = return e
replaceInExp pat_elems (If se then_body else_body dec) = do
  then_body' <- replaceInIfBody then_body
  else_body' <- replaceInIfBody else_body
  if_rets <- zipWithM (generalizeIxfun pat_elems) pat_elems $ ifReturns dec
  let dec' = dec {ifReturns = if_rets}
  return $ If se then_body' else_body' dec'
replaceInExp _ (DoLoop loop_inits loop_form (Body dec stms res)) = do
  loop_inits' <- mapM (replaceInFParam . fst) loop_inits
  stms' <- mapM replaceInStm stms
  return $ DoLoop (zip loop_inits' $ map snd loop_inits) loop_form $ Body dec stms' res
replaceInExp _ e@(Op (Alloc _ _)) = return e
replaceInExp _ e@(Op (Inner i)) = do
  on_op <- asks onInner
  Op . Inner <$> on_op i
replaceInExp _ (Op _) = error "Unreachable" -- This shouldn't be possible?
replaceInExp _ e@WithAcc {} = return e
replaceInExp _ e@Apply {} = return e

replaceInHostOp :: HostOp GPUMem () -> ReplaceM (HostOp GPUMem ()) (HostOp GPUMem ())
replaceInHostOp (SegOp (SegMap lvl sp tps body)) = do
  stms <- mapM replaceInStm $ kernelBodyStms body
  return $ SegOp $ SegMap lvl sp tps $ body {kernelBodyStms = stms}
replaceInHostOp (SegOp (SegRed lvl sp binops tps body)) = do
  stms <- mapM replaceInStm $ kernelBodyStms body
  return $ SegOp $ SegRed lvl sp binops tps body
replaceInHostOp (SegOp (SegScan lvl sp binops tps body)) = do
  stms <- mapM replaceInStm $ kernelBodyStms body
  return $ SegOp $ SegScan lvl sp binops tps body
replaceInHostOp (SegOp (SegHist lvl sp hist_ops tps body)) = do
  stms <- mapM replaceInStm $ kernelBodyStms body
  return $ SegOp $ SegHist lvl sp hist_ops tps body
replaceInHostOp op = return op

generalizeIxfun :: [PatElemT dec] -> PatElemT LetDecMem -> BodyReturns -> ReplaceM inner BodyReturns
generalizeIxfun
  pat_elems
  (PatElem vname (MemArray _ _ _ (ArrayIn mem ixfun)))
  m@(MemArray pt shp u _) = do
    coaltab <- asks envCoalesceTab
    if vname `M.member` coaltab
      then
        existentialiseIxFun (map patElemName pat_elems) ixfun
          & ReturnsInBlock mem
          & MemArray pt shp u
          & return
      else return m
generalizeIxfun _ _ m = return m

replaceInIfBody :: (Mem rep inner, LetDec rep ~ LetDecMem) => Body rep -> ReplaceM inner (Body rep)
replaceInIfBody b@(Body _ stms _) = do
  stms' <- mapM replaceInStm stms
  return $ b {bodyStms = stms'}

replaceInFParam :: Param FParamMem -> ReplaceM inner (Param FParamMem)
replaceInFParam p@(Param _ vname (MemArray _ _ u _)) = do
  fromMaybe p <$> lookupAndReplace vname (Param mempty) u
replaceInFParam p = return p

lookupAndReplace ::
  VName ->
  (VName -> MemBound u -> a) ->
  u ->
  ReplaceM inner (Maybe a)
lookupAndReplace vname f u = do
  coaltab <- asks envCoalesceTab
  case M.lookup vname coaltab of
    Just (Coalesced _ (MemBlock pt shp mem ixfun) subs) ->
      ixfun
        & fixPoint (substituteInIxFun subs)
        & ArrayIn mem
        & MemArray pt shp u
        & f vname
        & Just
        & return
    Nothing -> return Nothing
