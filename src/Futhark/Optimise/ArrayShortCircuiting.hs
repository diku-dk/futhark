{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Perform array short circuiting
module Futhark.Optimise.ArrayShortCircuiting (optimiseSeqMem, optimiseGPUMem) where

import Control.Monad.Reader
import Data.Function ((&))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Futhark.Analysis.Alias qualified as AnlAls
import Futhark.IR.Aliases
import Futhark.IR.GPUMem
import Futhark.IR.Mem.IxFun (substituteInIxFun)
import Futhark.IR.SeqMem
import Futhark.Optimise.ArrayShortCircuiting.ArrayCoalescing
import Futhark.Optimise.ArrayShortCircuiting.DataStructs
import Futhark.Pass (Pass (..))
import Futhark.Pass qualified as Pass
import Futhark.Util

----------------------------------------------------------------
--- Printer/Tester Main Program
----------------------------------------------------------------

data Env inner = Env
  { envCoalesceTab :: CoalsTab,
    onInner :: inner -> ReplaceM inner inner
  }

type ReplaceM inner a = Reader (Env inner) a

optimiseSeqMem :: Pass SeqMem SeqMem
optimiseSeqMem = pass "short-circuit" "Array Short-Circuiting" mkCoalsTab pure replaceInParams

optimiseGPUMem :: Pass GPUMem GPUMem
optimiseGPUMem = pass "short-circuit-gpu" "Array Short-Circuiting (GPU)" mkCoalsTabGPU replaceInHostOp replaceInParams

replaceInParams :: CoalsTab -> [Param FParamMem] -> (Names, [Param FParamMem])
replaceInParams coalstab fparams =
  let (mem_allocs_to_remove, fparams') =
        foldl
          replaceInParam
          (mempty, mempty)
          fparams
   in (mem_allocs_to_remove, reverse fparams')
  where
    replaceInParam (to_remove, acc) (Param attrs name dec) =
      case dec of
        MemMem DefaultSpace
          | Just entry <- M.lookup name coalstab ->
              (oneName (dstmem entry) <> to_remove, Param attrs (dstmem entry) dec : acc)
        MemArray pt shp u (ArrayIn m ixf)
          | Just entry <- M.lookup m coalstab ->
              (to_remove, Param attrs name (MemArray pt shp u $ ArrayIn (dstmem entry) ixf) : acc)
        _ -> (to_remove, Param attrs name dec : acc)

removeStms :: Names -> Body rep -> Body rep
removeStms to_remove (Body dec stms res) =
  Body dec (stmsFromList $ filter (not . flip nameIn to_remove . head . patNames . stmPat) $ stmsToList stms) res

pass ::
  (Mem rep inner, LetDec rep ~ LetDecMem, CanBeAliased inner) =>
  String ->
  String ->
  (Prog (Aliases rep) -> Pass.PassM (M.Map Name CoalsTab)) ->
  (inner -> ReplaceM inner inner) ->
  (CoalsTab -> [FParam (Aliases rep)] -> (Names, [FParam (Aliases rep)])) ->
  Pass rep rep
pass flag desc mk on_inner on_fparams =
  Pass flag desc $ \prog -> do
    coaltabs <- mk $ AnlAls.aliasAnalysis prog
    Pass.intraproceduralTransformationWithConsts pure (onFun coaltabs) prog
  where
    onFun coaltabs _ f = do
      let coaltab = coaltabs M.! funDefName f
      let (mem_allocs_to_remove, new_fparams) = on_fparams coaltab $ funDefParams f
      pure $
        f
          { funDefBody =
              onBody coaltab $
                removeStms mem_allocs_to_remove $
                  funDefBody f,
            funDefParams = new_fparams
          }

    onBody coaltab body =
      body
        { bodyStms =
            runReader
              (mapM replaceInStm $ bodyStms body)
              (Env coaltab on_inner),
          bodyResult = map (replaceResMem coaltab) $ bodyResult body
        }

replaceResMem :: CoalsTab -> SubExpRes -> SubExpRes
replaceResMem coaltab res =
  case flip M.lookup coaltab =<< subExpResVName res of
    Just entry -> res {resSubExp = Var $ dstmem entry}
    Nothing -> res

replaceInStm :: (Mem rep inner, LetDec rep ~ LetDecMem) => Stm rep -> ReplaceM inner (Stm rep)
replaceInStm (Let (Pat elems) d e) = do
  elems' <- mapM replaceInPatElem elems
  e' <- replaceInExp elems' e
  pure $ Let (Pat elems') d e'
  where
    replaceInPatElem :: PatElem LetDecMem -> ReplaceM inner (PatElem LetDecMem)
    replaceInPatElem p@(PatElem vname (MemArray _ _ u _)) =
      fromMaybe p <$> lookupAndReplace vname PatElem u
    replaceInPatElem p = pure p

replaceInExp :: (Mem rep inner, LetDec rep ~ LetDecMem) => [PatElem LetDecMem] -> Exp rep -> ReplaceM inner (Exp rep)
replaceInExp _ e@(BasicOp _) = pure e
replaceInExp pat_elems (Match cond_ses cases defbody dec) = do
  defbody' <- replaceInIfBody defbody
  cases' <- mapM (\(Case p b) -> Case p <$> replaceInIfBody b) cases
  case_rets <- zipWithM (generalizeIxfun pat_elems) pat_elems $ matchReturns dec
  let dec' = dec {matchReturns = case_rets}
  pure $ Match cond_ses cases' defbody' dec'
replaceInExp _ (DoLoop loop_inits loop_form (Body dec stms res)) = do
  loop_inits' <- mapM (replaceInFParam . fst) loop_inits
  stms' <- mapM replaceInStm stms
  coalstab <- asks envCoalesceTab
  let res' = map (replaceResMem coalstab) res
  pure $ DoLoop (zip loop_inits' $ map snd loop_inits) loop_form $ Body dec stms' res'
replaceInExp _ (Op op) =
  case op of
    Inner i -> do
      on_op <- asks onInner
      Op . Inner <$> on_op i
    _ -> pure $ Op op
replaceInExp _ e@WithAcc {} = pure e
replaceInExp _ e@Apply {} = pure e

replaceInHostOp :: HostOp GPUMem () -> ReplaceM (HostOp GPUMem ()) (HostOp GPUMem ())
replaceInHostOp (SegOp (SegMap lvl sp tps body)) = do
  stms <- mapM replaceInStm $ kernelBodyStms body
  pure $ SegOp $ SegMap lvl sp tps $ body {kernelBodyStms = stms}
replaceInHostOp (SegOp (SegRed lvl sp binops tps body)) = do
  stms <- mapM replaceInStm $ kernelBodyStms body
  pure $ SegOp $ SegRed lvl sp binops tps $ body {kernelBodyStms = stms}
replaceInHostOp (SegOp (SegScan lvl sp binops tps body)) = do
  stms <- mapM replaceInStm $ kernelBodyStms body
  pure $ SegOp $ SegScan lvl sp binops tps $ body {kernelBodyStms = stms}
replaceInHostOp (SegOp (SegHist lvl sp hist_ops tps body)) = do
  stms <- mapM replaceInStm $ kernelBodyStms body
  pure $ SegOp $ SegHist lvl sp hist_ops tps $ body {kernelBodyStms = stms}
replaceInHostOp op = pure op

generalizeIxfun :: [PatElem dec] -> PatElem LetDecMem -> BodyReturns -> ReplaceM inner BodyReturns
generalizeIxfun
  pat_elems
  (PatElem vname (MemArray _ _ _ (ArrayIn mem ixf)))
  m@(MemArray pt shp u _) = do
    coaltab <- asks envCoalesceTab
    if any (M.member vname . vartab) coaltab
      then
        existentialiseIxFun (map patElemName pat_elems) ixf
          & ReturnsInBlock mem
          & MemArray pt shp u
          & pure
      else pure m
generalizeIxfun _ _ m = pure m

replaceInIfBody :: (Mem rep inner, LetDec rep ~ LetDecMem) => Body rep -> ReplaceM inner (Body rep)
replaceInIfBody b@(Body _ stms res) = do
  coaltab <- asks envCoalesceTab
  stms' <- mapM replaceInStm stms
  pure $ b {bodyStms = stms', bodyResult = map (replaceResMem coaltab) res}

replaceInFParam :: Param FParamMem -> ReplaceM inner (Param FParamMem)
replaceInFParam p@(Param _ vname (MemArray _ _ u _)) = do
  fromMaybe p <$> lookupAndReplace vname (Param mempty) u
replaceInFParam p = pure p

lookupAndReplace ::
  VName ->
  (VName -> MemBound u -> a) ->
  u ->
  ReplaceM inner (Maybe a)
lookupAndReplace vname f u = do
  coaltab <- asks envCoalesceTab
  case M.lookup vname $ foldMap vartab coaltab of
    Just (Coalesced _ (MemBlock pt shp mem ixf) subs) ->
      ixf
        & fixPoint (substituteInIxFun subs)
        & ArrayIn mem
        & MemArray pt shp u
        & f vname
        & Just
        & pure
    Nothing -> pure Nothing
