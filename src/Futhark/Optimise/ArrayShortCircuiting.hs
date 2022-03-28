{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Playground for work on merging memory blocks
module Futhark.Optimise.ArrayShortCircuiting (optimiseSeqMem, optimiseGPUMem) where

import Control.Monad.Reader
import Data.Function ((&))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Futhark.Analysis.Alias as AnlAls
import Futhark.IR.Aliases
import Futhark.IR.GPUMem
import Futhark.IR.Mem.IxFun (substituteInIxFun)
import Futhark.IR.SeqMem
import Futhark.Optimise.ArrayShortCircuiting.ArrayCoalescing
import Futhark.Optimise.ArrayShortCircuiting.DataStructs
import Futhark.Pass (Pass (..))
import qualified Futhark.Pass as Pass
import Futhark.Util

----------------------------------------------------------------
--- Printer/Tester Main Program
----------------------------------------------------------------

data Env inner = Env
  { envCoalesceTab :: M.Map VName Coalesced,
    onInner :: inner -> ReplaceM inner inner
  }

type ReplaceM inner a = Reader (Env inner) a

optimiseSeqMem :: Pass SeqMem SeqMem
optimiseSeqMem = pass "short-circuit" "Array Short-Circuiting" mkCoalsTab return replaceInParams

optimiseGPUMem :: Pass GPUMem GPUMem
optimiseGPUMem = pass "short-circuit-gpu" "Array Short-Circuiting (GPU)" mkCoalsTabGPU replaceInHostOp replaceInParams

replaceInParams :: CoalsTab -> [Param FParamMem] -> (Names, [Param FParamMem])
replaceInParams coalstab fparams =
  let (mem_allocs_to_remove, fparams') =
        foldl
          ( \(to_remove, acc) (Param attrs name dec) ->
              case dec of
                MemMem DefaultSpace
                  | Just entry <- M.lookup name coalstab ->
                      (oneName (dstmem entry) <> to_remove, Param attrs (dstmem entry) dec : acc)
                MemArray pt shp u (ArrayIn m ixf)
                  | Just entry <- M.lookup m coalstab ->
                      (to_remove, Param attrs name (MemArray pt shp u $ ArrayIn (dstmem entry) ixf) : acc)
                _ -> (to_remove, Param attrs name dec : acc)
          )
          (mempty, mempty)
          fparams
   in (mem_allocs_to_remove, reverse fparams')

removeStms :: Names -> Body rep -> Body rep
removeStms to_remove (Body dec stms res) =
  Body dec (stmsFromList $ filter (not . flip nameIn to_remove . head . patNames . stmPat) $ stmsToList stms) res

pass ::
  (Mem rep inner, LetDec rep ~ LetDecMem, CanBeAliased inner) =>
  String ->
  String ->
  (FunDef (Aliases rep) -> Pass.PassM CoalsTab) ->
  (inner -> ReplaceM inner inner) ->
  (CoalsTab -> [FParam (Aliases rep)] -> (Names, [FParam (Aliases rep)])) ->
  Pass rep rep
pass flag desc mk on_inner on_fparams =
  Pass flag desc $
    Pass.intraproceduralTransformationWithConsts return $ \_ f -> do
      coaltab <- mk (AnlAls.analyseFun f)
      let (mem_allocs_to_remove, new_fparams) = on_fparams coaltab $ funDefParams f
      return $
        f
          { funDefBody =
              onBody (foldMap vartab $ M.elems coaltab) $
                removeStms mem_allocs_to_remove $
                  funDefBody f,
            funDefParams = new_fparams
          }
  where
    onBody coaltab body =
      body {bodyStms = runReader (mapM replaceInStm $ bodyStms body) (Env coaltab on_inner)}

replaceInStm :: (Mem rep inner, LetDec rep ~ LetDecMem) => Stm rep -> ReplaceM inner (Stm rep)
replaceInStm (Let (Pat elems) d e) = do
  elems' <- mapM replaceInPatElem elems
  e' <- replaceInExp elems' e
  return $ Let (Pat elems') d e'
  where
    replaceInPatElem :: PatElem LetDecMem -> ReplaceM inner (PatElem LetDecMem)
    replaceInPatElem p@(PatElem vname (MemArray _ _ u _)) =
      fromMaybe p <$> lookupAndReplace vname PatElem u
    replaceInPatElem p = return p

replaceInExp :: (Mem rep inner, LetDec rep ~ LetDecMem) => [PatElem LetDecMem] -> Exp rep -> ReplaceM inner (Exp rep)
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
replaceInExp _ (Op (Inner i)) = do
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
  return $ SegOp $ SegRed lvl sp binops tps $ body {kernelBodyStms = stms}
replaceInHostOp (SegOp (SegScan lvl sp binops tps body)) = do
  stms <- mapM replaceInStm $ kernelBodyStms body
  return $ SegOp $ SegScan lvl sp binops tps $ body {kernelBodyStms = stms}
replaceInHostOp (SegOp (SegHist lvl sp hist_ops tps body)) = do
  stms <- mapM replaceInStm $ kernelBodyStms body
  return $ SegOp $ SegHist lvl sp hist_ops tps $ body {kernelBodyStms = stms}
replaceInHostOp op = return op

generalizeIxfun :: [PatElem dec] -> PatElem LetDecMem -> BodyReturns -> ReplaceM inner BodyReturns
generalizeIxfun
  pat_elems
  (PatElem vname (MemArray _ _ _ (ArrayIn mem ixf)))
  m@(MemArray pt shp u _) = do
    coaltab <- asks envCoalesceTab
    if vname `M.member` coaltab
      then
        existentialiseIxFun (map patElemName pat_elems) ixf
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
    Just (Coalesced _ (MemBlock pt shp mem ixf) subs) ->
      ixf
        & fixPoint (substituteInIxFun subs)
        & ArrayIn mem
        & MemArray pt shp u
        & f vname
        & Just
        & return
    Nothing -> return Nothing
