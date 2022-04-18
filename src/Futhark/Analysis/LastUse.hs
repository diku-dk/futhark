{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Provides last-use analysis for Futhark programs.
module Futhark.Analysis.LastUse (LastUseMap, Used, analyseGPUMem, analyseSeqMem) where

import Control.Monad.Reader
import Data.Bifunctor (first)
import Data.Foldable
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Tuple
import Futhark.Analysis.Alias (aliasAnalysis)
import Futhark.IR.Aliases
import Futhark.IR.GPUMem
import Futhark.IR.SeqMem

-- | `LastUseMap` tells which names were last used in a given statement.
-- Statements are uniquely identified by the `VName` of the first value
-- parameter in the statement pattern. `Names` is the set of names last used.
type LastUseMap = Map VName Names

-- | `LastUse` is a mapping from a `VName` to the statement identifying it's
-- last use. `LastUseMap` is the inverse of `LastUse`.
type LastUse = Map VName VName

-- | `Used` is the set of `VName` that were used somewhere in a statement, body
-- or otherwise.
type Used = Names

type LastUseOp rep =
  VName -> (LastUse, Used) -> Op (Aliases rep) -> LastUseM rep

newtype Env rep = Env {envLastUseOp :: LastUseOp rep}

type LastUseM rep = Reader (Env rep) (LastUse, Used)

analyseGPUMem :: Prog GPUMem -> (LastUseMap, Used)
analyseGPUMem = analyseProg analyseGPUOp

analyseSeqMem :: Prog SeqMem -> (LastUseMap, Used)
analyseSeqMem = analyseProg analyseSeqOp

analyseGPUOp :: LastUseOp GPUMem
analyseGPUOp pat_name (lumap, used) (Alloc se sp) = do
  let nms = (freeIn se <> freeIn sp) `namesSubtract` used
  pure (insertNames pat_name nms lumap, used <> nms)
analyseGPUOp pat_name (lumap, used) (Inner (SizeOp sop)) = do
  let nms = freeIn sop `namesSubtract` used
  pure (insertNames pat_name nms lumap, used <> nms)
analyseGPUOp _ (lumap, used) (Inner (OtherOp ())) =
  pure (lumap, used)
analyseGPUOp pat_name (lumap, used) (Inner (SegOp (SegMap lvl _ tps body))) = do
  (lumap', used') <- analyseKernelBody (lumap, used) body
  let nms = (freeIn lvl <> freeIn tps) `namesSubtract` used'
  pure (insertNames pat_name nms lumap', used' <> nms)
analyseGPUOp pat_name (lumap, used) (Inner (SegOp (SegRed lvl _ binops tps body))) =
  segOpHelper pat_name lumap used lvl binops tps body
analyseGPUOp pat_name (lumap, used) (Inner (SegOp (SegScan lvl _ binops tps body))) =
  segOpHelper pat_name lumap used lvl binops tps body
analyseGPUOp pat_name (lumap, used) (Inner (SegOp (SegHist lvl _ binops tps body))) = do
  (lumap', used') <- foldM analyseHistOp (lumap, used) $ reverse binops
  (lumap'', used'') <- analyseKernelBody (lumap', used') body
  let nms = (freeIn lvl <> freeIn tps) `namesSubtract` used''
  pure (insertNames pat_name nms lumap'', used'' <> nms)
analyseGPUOp pat_name (lumap, used) (Inner (GPUBody ts body)) = do
  (lumap', used') <- analyseBody lumap used body
  let nms = freeIn ts
  pure (insertNames pat_name nms lumap', used' <> nms)

segOpHelper ::
  (FreeIn (OpWithAliases (Op rep)), ASTRep rep) =>
  VName ->
  LastUse ->
  Used ->
  SegLevel ->
  [SegBinOp (Aliases rep)] ->
  [Type] ->
  KernelBody (Aliases rep) ->
  LastUseM rep
segOpHelper pat_name lumap used lvl binops tps body = do
  (lumap', used') <- foldM analyseSegBinOp (lumap, used) $ reverse binops
  (lumap'', used'') <- analyseKernelBody (lumap', used') body
  let nms = (freeIn lvl <> freeIn tps) `namesSubtract` used''
  pure (insertNames pat_name nms lumap'', used'' <> nms)

analyseSeqOp :: LastUseOp SeqMem
analyseSeqOp pat_name (lumap, used) (Alloc se sp) = do
  let nms = (freeIn se <> freeIn sp) `namesSubtract` used
  pure (insertNames pat_name nms lumap, used <> nms)
analyseSeqOp _ (lumap, used) (Inner ()) =
  pure (lumap, used)

-- | Analyses a program to return a last-use map, mapping each simple statement
-- in the program to the values that were last used within that statement, and
-- the set of all `VName` that were used inside.
analyseProg :: (CanBeAliased (Op rep), Mem rep inner) => LastUseOp rep -> Prog rep -> (LastUseMap, Used)
analyseProg onOp prog =
  runReader helper (Env onOp)
  where
    helper = do
      let consts =
            progConsts prog
              & concatMap (toList . fmap patElemName . patElems . stmPat)
              & namesFromList
          funs = progFuns $ aliasAnalysis prog
      (lus, used) <- mconcat <$> mapM (analyseFun mempty consts) funs
      pure (flipMap lus, used)

analyseFun :: (FreeIn (OpWithAliases (Op rep)), ASTRep rep) => LastUse -> Used -> FunDef (Aliases rep) -> LastUseM rep
analyseFun lumap used fun = do
  (lumap', used') <- analyseBody lumap used $ funDefBody fun
  pure (lumap', used' <> freeIn (funDefParams fun))

analyseStms :: (FreeIn (OpWithAliases (Op rep)), ASTRep rep) => LastUse -> Used -> Stms (Aliases rep) -> LastUseM rep
analyseStms lumap used stms = foldM analyseStm (lumap, used) $ reverse $ stmsToList stms

analyseStm :: (FreeIn (OpWithAliases (Op rep)), ASTRep rep) => (LastUse, Used) -> Stm (Aliases rep) -> LastUseM rep
analyseStm (lumap0, used0) (Let pat _ e) = do
  let (lumap', used') = patElems pat & foldl helper (lumap0, used0)
  analyseExp (lumap', used') e
  where
    helper (lumap_acc, used_acc) (PatElem name (aliases, _)) =
      -- Any aliases of `name` should have the same last-use as `name`
      ( case M.lookup name lumap_acc of
          Just name' ->
            insertNames name' (unAliases aliases) lumap_acc
          Nothing -> lumap_acc,
        used_acc <> unAliases aliases
      )

    pat_name = patElemName $ head $ patElems pat

    analyseExp (lumap, used) (BasicOp _) = do
      let nms = freeIn e `namesSubtract` used
      pure (insertNames pat_name nms lumap, used <> nms)
    analyseExp (lumap, used) (Apply _ args _ _) = do
      let nms = freeIn $ map fst args
      pure (insertNames pat_name nms lumap, used <> nms)
    analyseExp (lumap, used) (If cse then_body else_body dec) = do
      (lumap_then, used_then) <- analyseBody lumap used then_body
      (lumap_else, used_else) <- analyseBody lumap used else_body
      let used' = used_then <> used_else
          nms = (freeIn cse <> freeIn dec) `namesSubtract` used'
      pure (insertNames pat_name nms (lumap_then <> lumap_else), used' <> nms)
    analyseExp (lumap, used) (DoLoop merge form body) = do
      (lumap', used') <- analyseBody lumap used body
      let nms = (freeIn merge <> freeIn form) `namesSubtract` used'
      pure (insertNames pat_name nms lumap', used' <> nms)
    analyseExp (lumap, used) (Op op) = do
      onOp <- asks envLastUseOp
      onOp pat_name (lumap, used) op
    analyseExp (lumap, used) (WithAcc _ l) =
      analyseLambda (lumap, used) l

analyseBody :: (FreeIn (OpWithAliases (Op rep)), ASTRep rep) => LastUse -> Used -> Body (Aliases rep) -> LastUseM rep
analyseBody lumap used (Body _ stms result) = do
  let used' = used <> freeIn result
  analyseStms lumap used' stms

analyseKernelBody ::
  (FreeIn (OpWithAliases (Op rep)), ASTRep rep) =>
  (LastUse, Used) ->
  KernelBody (Aliases rep) ->
  LastUseM rep
analyseKernelBody (lumap, used) (KernelBody _ stms result) =
  let used' = used <> freeIn result
   in analyseStms lumap used' stms

analyseSegBinOp ::
  (FreeIn (OpWithAliases (Op rep)), ASTRep rep) =>
  (LastUse, Used) ->
  SegBinOp (Aliases rep) ->
  LastUseM rep
analyseSegBinOp (lumap, used) (SegBinOp _ lambda neutral shp) = do
  (lumap', used') <- analyseLambda (lumap, used) lambda
  let nms = (freeIn neutral <> freeIn shp) `namesSubtract` used'
  pure (lumap', used' <> nms)

analyseHistOp ::
  (FreeIn (OpWithAliases (Op rep)), ASTRep rep) =>
  (LastUse, Used) ->
  HistOp (Aliases rep) ->
  LastUseM rep
analyseHistOp (lumap, used) (HistOp width race dest neutral shp lambda) = do
  (lumap', used') <- analyseLambda (lumap, used) lambda
  let nms =
        ( freeIn width <> freeIn race <> freeIn dest <> freeIn neutral
            <> freeIn shp
        )
          `namesSubtract` used'
  pure (lumap', used' <> nms)

analyseLambda :: (FreeIn (OpWithAliases (Op rep)), ASTRep rep) => (LastUse, Used) -> Lambda (Aliases rep) -> LastUseM rep
analyseLambda (lumap, used) (Lambda params body ret) = do
  (lumap', used') <- analyseBody lumap used body
  let used'' = used' <> freeIn params <> freeIn ret
  pure (lumap', used'')

flipMap :: Map VName VName -> Map VName Names
flipMap m =
  M.toList m
    & fmap (swap . first oneName)
    & foldr (uncurry $ M.insertWith (<>)) mempty

insertNames :: VName -> Names -> LastUse -> LastUse
insertNames name names lumap =
  foldr (flip (M.insertWith $ \_ x -> x) name) lumap $ namesToList names
