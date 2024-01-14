{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Perform array short circuiting
module Futhark.Optimise.ArrayShortCircuiting
  ( optimiseSeqMem,
    optimiseGPUMem,
    optimiseMCMem,
  )
where

import Control.Monad
import Control.Monad.Reader
import Data.Function ((&))
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Futhark.Analysis.Alias qualified as AnlAls
import Futhark.IR.Aliases
import Futhark.IR.GPUMem
import Futhark.IR.MCMem
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.IR.SeqMem
import Futhark.Optimise.ArrayShortCircuiting.ArrayCoalescing
import Futhark.Optimise.ArrayShortCircuiting.DataStructs
import Futhark.Pass (Pass (..))
import Futhark.Pass qualified as Pass
import Futhark.Util

data Env inner = Env
  { envCoalesceTab :: CoalsTab,
    onInner :: inner -> UpdateM inner inner,
    memAllocsToRemove :: Names
  }

type UpdateM inner a = Reader (Env inner) a

optimiseSeqMem :: Pass SeqMem SeqMem
optimiseSeqMem = pass "short-circuit" "Array Short-Circuiting" mkCoalsTab pure replaceInParams

optimiseGPUMem :: Pass GPUMem GPUMem
optimiseGPUMem = pass "short-circuit-gpu" "Array Short-Circuiting (GPU)" mkCoalsTabGPU replaceInHostOp replaceInParams

optimiseMCMem :: Pass MCMem MCMem
optimiseMCMem = pass "short-circuit-mc" "Array Short-Circuiting (MC)" mkCoalsTabMC replaceInMCOp replaceInParams

replaceInParams :: CoalsTab -> [Param FParamMem] -> (Names, [Param FParamMem])
replaceInParams coalstab fparams =
  let (mem_allocs_to_remove, fparams') =
        foldl replaceInParam (mempty, mempty) fparams
   in (mem_allocs_to_remove, reverse fparams')
  where
    replaceInParam (to_remove, acc) (Param attrs name dec) =
      case dec of
        MemMem _
          | Just entry <- M.lookup name coalstab ->
              (oneName (dstmem entry) <> to_remove, Param attrs (dstmem entry) dec : acc)
        MemArray pt shp u (ArrayIn m ixf)
          | Just entry <- M.lookup m coalstab ->
              (to_remove, Param attrs name (MemArray pt shp u $ ArrayIn (dstmem entry) ixf) : acc)
        _ -> (to_remove, Param attrs name dec : acc)

removeAllocsInStms :: Stms rep -> UpdateM inner (Stms rep)
removeAllocsInStms stms = do
  to_remove <- asks memAllocsToRemove
  stmsToList stms
    & filter (not . flip nameIn to_remove . head . patNames . stmPat)
    & stmsFromList
    & pure

pass ::
  (Mem rep inner, LetDec rep ~ LetDecMem, AliasableRep rep) =>
  String ->
  String ->
  (Prog (Aliases rep) -> Pass.PassM (M.Map Name CoalsTab)) ->
  (inner rep -> UpdateM (inner rep) (inner rep)) ->
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
          { funDefBody = onBody coaltab mem_allocs_to_remove $ funDefBody f,
            funDefParams = new_fparams
          }

    onBody coaltab mem_allocs_to_remove body =
      body
        { bodyStms =
            runReader
              (updateStms $ bodyStms body)
              (Env coaltab on_inner mem_allocs_to_remove),
          bodyResult = map (replaceResMem coaltab) $ bodyResult body
        }

replaceResMem :: CoalsTab -> SubExpRes -> SubExpRes
replaceResMem coaltab res =
  case flip M.lookup coaltab =<< subExpResVName res of
    Just entry -> res {resSubExp = Var $ dstmem entry}
    Nothing -> res

updateStms ::
  (Mem rep inner, LetDec rep ~ LetDecMem) =>
  Stms rep ->
  UpdateM (inner rep) (Stms rep)
updateStms stms = do
  stms' <- mapM replaceInStm stms
  removeAllocsInStms stms'

replaceInStm ::
  (Mem rep inner, LetDec rep ~ LetDecMem) =>
  Stm rep ->
  UpdateM (inner rep) (Stm rep)
replaceInStm (Let (Pat elems) (StmAux c a d) e) = do
  elems' <- mapM replaceInPatElem elems
  e' <- replaceInExp elems' e
  entries <- asks (M.elems . envCoalesceTab)
  let c' = case filter (\entry -> (map patElemName elems `L.intersect` M.keys (vartab entry)) /= []) entries of
        [] -> c
        entries' -> c <> foldMap certs entries'
  pure $ Let (Pat elems') (StmAux c' a d) e'
  where
    replaceInPatElem :: PatElem LetDecMem -> UpdateM inner (PatElem LetDecMem)
    replaceInPatElem p@(PatElem vname (MemArray _ _ u _)) =
      fromMaybe p <$> lookupAndReplace vname PatElem u
    replaceInPatElem p = pure p

replaceInExp ::
  (Mem rep inner, LetDec rep ~ LetDecMem) =>
  [PatElem LetDecMem] ->
  Exp rep ->
  UpdateM (inner rep) (Exp rep)
replaceInExp _ e@(BasicOp _) = pure e
replaceInExp pat_elems (Match cond_ses cases defbody dec) = do
  defbody' <- replaceInIfBody defbody
  cases' <- mapM (\(Case p b) -> Case p <$> replaceInIfBody b) cases
  case_rets <- zipWithM (generalizeIxfun pat_elems) pat_elems $ matchReturns dec
  let dec' = dec {matchReturns = case_rets}
  pure $ Match cond_ses cases' defbody' dec'
replaceInExp _ (Loop loop_inits loop_form (Body dec stms res)) = do
  loop_inits' <- mapM (replaceInFParam . fst) loop_inits
  stms' <- updateStms stms
  coalstab <- asks envCoalesceTab
  let res' = map (replaceResMem coalstab) res
  pure $ Loop (zip loop_inits' $ map snd loop_inits) loop_form $ Body dec stms' res'
replaceInExp _ (Op op) =
  case op of
    Inner i -> do
      on_op <- asks onInner
      Op . Inner <$> on_op i
    _ -> pure $ Op op
replaceInExp _ e@WithAcc {} = pure e
replaceInExp _ e@Apply {} = pure e

replaceInSegOp ::
  (Mem rep inner, LetDec rep ~ LetDecMem) =>
  SegOp lvl rep ->
  UpdateM (inner rep) (SegOp lvl rep)
replaceInSegOp (SegMap lvl sp tps body) = do
  stms <- updateStms $ kernelBodyStms body
  pure $ SegMap lvl sp tps $ body {kernelBodyStms = stms}
replaceInSegOp (SegRed lvl sp binops tps body) = do
  stms <- updateStms $ kernelBodyStms body
  pure $ SegRed lvl sp binops tps $ body {kernelBodyStms = stms}
replaceInSegOp (SegScan lvl sp binops tps body) = do
  stms <- updateStms $ kernelBodyStms body
  pure $ SegScan lvl sp binops tps $ body {kernelBodyStms = stms}
replaceInSegOp (SegHist lvl sp hist_ops tps body) = do
  stms <- updateStms $ kernelBodyStms body
  pure $ SegHist lvl sp hist_ops tps $ body {kernelBodyStms = stms}

replaceInHostOp :: HostOp NoOp GPUMem -> UpdateM (HostOp NoOp GPUMem) (HostOp NoOp GPUMem)
replaceInHostOp (SegOp op) = SegOp <$> replaceInSegOp op
replaceInHostOp op = pure op

replaceInMCOp :: MCOp NoOp MCMem -> UpdateM (MCOp NoOp MCMem) (MCOp NoOp MCMem)
replaceInMCOp (ParOp par_op op) =
  ParOp <$> traverse replaceInSegOp par_op <*> replaceInSegOp op
replaceInMCOp op = pure op

generalizeIxfun :: [PatElem dec] -> PatElem LetDecMem -> BodyReturns -> UpdateM inner BodyReturns
generalizeIxfun
  pat_elems
  (PatElem vname (MemArray _ _ _ (ArrayIn mem ixf)))
  m@(MemArray pt shp u _) = do
    coaltab <- asks envCoalesceTab
    if any (M.member vname . vartab) coaltab
      then
        existentialiseLMAD (map patElemName pat_elems) ixf
          & ReturnsInBlock mem
          & MemArray pt shp u
          & pure
      else pure m
generalizeIxfun _ _ m = pure m

replaceInIfBody :: (Mem rep inner, LetDec rep ~ LetDecMem) => Body rep -> UpdateM (inner rep) (Body rep)
replaceInIfBody b@(Body _ stms res) = do
  coaltab <- asks envCoalesceTab
  stms' <- updateStms stms
  pure $ b {bodyStms = stms', bodyResult = map (replaceResMem coaltab) res}

replaceInFParam :: Param FParamMem -> UpdateM inner (Param FParamMem)
replaceInFParam p@(Param _ vname (MemArray _ _ u _)) = do
  fromMaybe p <$> lookupAndReplace vname (Param mempty) u
replaceInFParam p = pure p

lookupAndReplace ::
  VName ->
  (VName -> MemBound u -> a) ->
  u ->
  UpdateM inner (Maybe a)
lookupAndReplace vname f u = do
  coaltab <- asks envCoalesceTab
  case M.lookup vname $ foldMap vartab coaltab of
    Just (Coalesced _ (MemBlock pt shp mem ixf) subs) ->
      ixf
        & fixPoint (LMAD.substitute subs)
        & ArrayIn mem
        & MemArray pt shp u
        & f vname
        & Just
        & pure
    Nothing -> pure Nothing
