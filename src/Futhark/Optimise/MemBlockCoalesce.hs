{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Playground for work on merging memory blocks
module Futhark.Optimise.MemBlockCoalesce (memoryBlockMerging, pass) where

import Data.Bifunctor (first)
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
    onStms coaltab _ stms = do
      return $ replaceDecorations coaltab stms

replaceDecorations :: M.Map VName Coalesced -> Stms SeqMem -> Stms SeqMem
replaceDecorations coaltab = fmap $ replaceInStm coaltab

replaceInStm :: M.Map VName Coalesced -> Stm SeqMem -> Stm SeqMem
replaceInStm coaltab (Let (Pat elems) d e) =
  let e' = replaceInExp coaltab e
   in Let (Pat elems') d e'
  where
    elems' = fmap replaceInPatElem elems

    replaceInPatElem :: PatElem SeqMem -> PatElem SeqMem
    replaceInPatElem p@(PatElem vname (MemArray _ _ u _)) =
      fromMaybe p $ lookupAndReplace vname coaltab PatElem u
    replaceInPatElem p = p

    replaceInExp :: M.Map VName Coalesced -> Exp SeqMem -> Exp SeqMem
    replaceInExp _ (BasicOp _) = e
    replaceInExp tab (If se then_body else_body dec) =
      let then_body' = replaceInIfBody tab then_body
          else_body' = replaceInIfBody tab else_body
          dec' = dec {ifReturns = zipWith (curry generalizeIxfun) elems' $ ifReturns dec}
       in If se then_body' else_body' dec'
    replaceInExp tab (DoLoop loop_inits loop_form (Body dec stms res)) =
      let loop_inits' = map (first $ replaceInFParam tab) loop_inits
          stms' = fmap (replaceInStm tab) stms
       in DoLoop loop_inits' loop_form $ Body dec stms' res
    replaceInExp _ (Op (Alloc _ _)) = e
    replaceInExp _ (Op (Inner ())) = e
    replaceInExp _ WithAcc {} = e
    replaceInExp _ Apply {} = e

    generalizeIxfun :: (PatElem SeqMem, BodyReturns) -> BodyReturns
    generalizeIxfun
      ( PatElem vname (MemArray _ _ _ (ArrayIn mem ixfun)),
        MemArray pt shp u _
        )
        | vname `M.member` coaltab =
          existentialiseIxFun (map patElemName elems) ixfun
            & ReturnsInBlock mem
            & MemArray pt shp u
    generalizeIxfun (_, m) = m

replaceInIfBody :: M.Map VName Coalesced -> Body SeqMem -> Body SeqMem
replaceInIfBody tab b@(Body _ stms _) =
  let stms' = fmap (replaceInStm tab) stms
   in b {bodyStms = stms'}

replaceInFParam :: M.Map VName Coalesced -> FParam SeqMem -> FParam SeqMem
replaceInFParam tab p@(Param vname (MemArray _ _ u _)) =
  fromMaybe p $ lookupAndReplace vname tab Param u
replaceInFParam _ p = p

lookupAndReplace ::
  VName ->
  M.Map VName Coalesced ->
  (VName -> MemInfo SubExp u MemBind -> a) ->
  u ->
  Maybe a
lookupAndReplace vname coaltab f u =
  case M.lookup vname coaltab of
    Just (Coalesced InPlaceCoal (MemBlock pt shp mem ixfun) subs) ->
      substituteInIxFun subs ixfun
        & ArrayIn mem
        & MemArray pt shp u
        & f vname
        & Just
    Nothing -> Nothing
    _ -> undefined
