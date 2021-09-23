{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Playground for work on merging memory blocks
module Futhark.Optimise.MemBlockCoalesce (printMemoryBlockMerging, coalesceSeqMem) where

import Control.Monad.Reader
import Data.Function ((&))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Futhark.Analysis.Alias as AnlAls
import Futhark.IR.Aliases
import Futhark.IR.Mem
import Futhark.IR.Mem.IxFun (substituteInIxFun)
import Futhark.IR.SeqMem
import Futhark.Optimise.MemBlockCoalesce.ArrayCoalescing
import Futhark.Optimise.MemBlockCoalesce.DataStructs
import Futhark.Pass (Pass (..))
import qualified Futhark.Pass as Pass
import Prelude

----------------------------------------------------------------
--- Printer/Tester Main Program
----------------------------------------------------------------

-- run it with:  `futhark dev --cpu --merge-mem test.fut`
printMemoryBlockMerging :: Prog SeqMem -> IO ()
printMemoryBlockMerging prg = do
  mapM_ lookAtFunction (progFuns prg)

  let coaltab = mkCoalsTab $ AnlAls.aliasAnalysis prg
  putStrLn $ "COALESCING RESULT:" ++ pretty (length coaltab) ++ "\n" ++ pretty coaltab

lookAtFunction :: FunDef SeqMem -> IO ()
lookAtFunction fdef = do
  putStrLn $ "Function:\n" ++ pretty fdef

data Env inner = Env
  { envCoalesceTab :: M.Map VName Coalesced,
    onInner :: inner -> ReplaceM inner (Maybe inner)
  }

type ReplaceM inner a = Reader (Env inner) a

coalesceSeqMem :: Pass SeqMem SeqMem
coalesceSeqMem = pass mkCoalsTab (return . const Nothing)

pass ::
  (Mem rep inner, LetDec rep ~ LetDecMem, CanBeAliased inner) =>
  (Prog (Aliases rep) -> CoalsTab) ->
  (inner -> ReplaceM inner (Maybe inner)) ->
  Pass rep rep
pass mk on_inner =
  Pass "Memory Coalescing" "memory coalescing" $ \prog ->
    let coaltab = foldMap vartab $ M.elems $ mk $ AnlAls.aliasAnalysis prog
     in Pass.intraproceduralTransformation (onStms coaltab) prog
  where
    onStms coaltab _ stms = do
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
  maybe e (Op . Inner) <$> on_op i
replaceInExp _ (Op _) = error "Unreachable" -- This shouldn't be possible?
replaceInExp _ e@WithAcc {} = return e
replaceInExp _ e@Apply {} = return e

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
replaceInFParam p@(Param vname (MemArray _ _ u _)) = do
  fromMaybe p <$> lookupAndReplace vname Param u
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
      substituteInIxFun subs ixfun
        & ArrayIn mem
        & MemArray pt shp u
        & f vname
        & Just
        & return
    Nothing -> return Nothing
