{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Playground for work on merging memory blocks
module Futhark.Optimise.MemBlockCoalesce (memoryBlockMerging, pass) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor (first)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Futhark.Analysis.Alias as AnlAls
import Futhark.Builder.Class
import Futhark.Construct
import Futhark.IR.Aliases
import Futhark.IR.Mem
import Futhark.IR.Mem.IxFun (substituteInIxFun)
import Futhark.IR.SeqMem
import Futhark.Optimise.MemBlockCoalesce.ArrayCoalescing
import Futhark.Optimise.MemBlockCoalesce.DataStructs
import Futhark.Pass (Pass (..), PassM)
import qualified Futhark.Pass as Pass
import Prelude

----------------------------------------------------------------
--- Printer/Tester Main Program
----------------------------------------------------------------

-- run it with:  `futhark dev --cpu --merge-mem test.fut`
memoryBlockMerging :: Prog SeqMem -> IO ()
memoryBlockMerging prg = do
  mapM_ lookAtFunction (progFuns prg)

  let coaltab = mkCoalsTab $ AnlAls.aliasAnalysis prg
  putStrLn $ "COALESCING RESULT:" ++ pretty (length coaltab) ++ "\n" ++ pretty coaltab

lookAtFunction :: FunDef SeqMem -> IO ()
lookAtFunction fdef = do
  putStrLn $ "Function:\n" ++ pretty fdef

pass :: Pass SeqMem SeqMem
pass =
  Pass "Memory Coalescing" "memory coalescing" $ \prog ->
    let coaltab = foldMap vartab $ M.elems $ mkCoalsTab $ AnlAls.aliasAnalysis prog
     in Pass.intraproceduralTransformation (onStms coaltab) prog
  where
    onStms :: M.Map VName Coalesced -> Scope SeqMem -> Stms SeqMem -> PassM (Stms SeqMem)
    onStms coaltab scope stms = do
      let m = localScope scope $ replaceDecorations coaltab stms
      fmap fst $ modifyNameSource $ runState $ runBuilderT m mempty

replaceDecorations :: LocalScope SeqMem m => M.Map VName Coalesced -> Stms SeqMem -> m (Stms SeqMem)
replaceDecorations coaltab = mapM $ replaceInStm coaltab

replaceInStm :: LocalScope SeqMem m => M.Map VName Coalesced -> Stm SeqMem -> m (Stm SeqMem)
replaceInStm coaltab stm@(Let (Pat elems) d e) =
  inScopeOf stm $ do
    e' <- replaceInExp coaltab e
    return $ Let (Pat elems') d e'
  where
    elems' = fmap replaceInPatElem elems

    replaceInPatElem :: PatElem SeqMem -> PatElem SeqMem
    replaceInPatElem p@(PatElem vname (MemArray _ _ u _)) =
      fromMaybe p $ lookupAndReplace vname coaltab PatElem u
    replaceInPatElem p = p

    replaceInExp :: LocalScope SeqMem m => M.Map VName Coalesced -> Exp SeqMem -> m (Exp SeqMem)
    replaceInExp _ (BasicOp _) = return e
    replaceInExp tab (If se then_body else_body dec) = do
      then_body' <- replaceInIfBody tab then_body
      else_body' <- replaceInIfBody tab else_body
      let dec' = dec {ifReturns = zipWith (curry generalizeIxfun) elems' $ ifReturns dec}
      return $ If se then_body' else_body' dec'
    replaceInExp tab (DoLoop loop_inits loop_form (Body dec stms res)) = do
      let loop_inits' = map (first $ replaceInFParam tab) loop_inits
      stms' <- mapM (replaceInStm tab) stms
      return $ DoLoop loop_inits' loop_form $ Body dec stms' res
    replaceInExp _ (Op (Alloc _ _)) = return e
    replaceInExp _ (Op (Inner ())) = return e
    replaceInExp _ WithAcc {} = return e
    replaceInExp _ Apply {} = return e

    generalizeIxfun :: (PatElem SeqMem, MemInfo ExtSize NoUniqueness MemReturn) -> MemInfo ExtSize NoUniqueness MemReturn
    generalizeIxfun (PatElem vname (MemArray _ _ _ (ArrayIn mem ixfun)), MemArray pt shp u _)
      | vname `M.member` coaltab =
        MemArray pt shp u $ ReturnsInBlock mem $ existentialiseIxFun (map patElemName elems) ixfun
    generalizeIxfun (_, m) = m

replaceInIfBody :: LocalScope SeqMem m => M.Map VName Coalesced -> Body SeqMem -> m (Body SeqMem)
replaceInIfBody tab b@(Body _ stms _) = do
  stms' <- mapM (replaceInStm tab) stms
  return $ b {bodyStms = stms'}

replaceInFParam :: M.Map VName Coalesced -> FParam SeqMem -> FParam SeqMem
replaceInFParam tab p@(Param vname (MemArray _ _ u _)) =
  fromMaybe p $ lookupAndReplace vname tab Param u
replaceInFParam _ p = p

lookupAndReplace :: VName -> M.Map VName Coalesced -> (VName -> MemInfo SubExp u MemBind -> a) -> u -> Maybe a
lookupAndReplace vname coaltab f u =
  case M.lookup vname coaltab of
    Just (Coalesced InPlaceCoal (MemBlock pt shp mem ixfun) subs) ->
      Just $ f vname $ MemArray pt shp u $ ArrayIn mem $ substituteInIxFun subs ixfun
    Nothing -> Nothing
    _ -> undefined
