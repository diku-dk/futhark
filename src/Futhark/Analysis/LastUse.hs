{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Provides last-use analysis for Futhark programs.
module Futhark.Analysis.LastUse (LastUseMap, analyseProg) where

import Data.Bifunctor (first)
import Data.Foldable
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Tuple
import Futhark.Analysis.Alias (aliasAnalysis)
import Futhark.IR.Aliases
import Futhark.IR.GPUMem

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

-- | Analyses a program to return a last-use map, mapping each simple statement
-- in the program to the values that were last used within that statement, and
-- the set of all `VName` that were used inside.
analyseProg :: Prog GPUMem -> (LastUseMap, Used)
analyseProg prog =
  let consts =
        progConsts prog
          & concatMap (toList . fmap patElemName . patElems . stmPat)
          & namesFromList
      funs = progFuns $ aliasAnalysis prog
      (lus, used) = foldMap (analyseFun mempty consts) funs
   in (flipMap lus, used)

analyseFun :: LastUse -> Used -> FunDef (Aliases GPUMem) -> (LastUse, Used)
analyseFun lumap used fun =
  let (lumap', used') = analyseBody lumap used $ funDefBody fun
   in (lumap', used' <> freeIn (funDefParams fun))

analyseStms :: LastUse -> Used -> Stms (Aliases GPUMem) -> (LastUse, Used)
analyseStms lumap used stms = foldr analyseStm (lumap, used) $ stmsToList stms

analyseStm :: Stm (Aliases GPUMem) -> (LastUse, Used) -> (LastUse, Used)
analyseStm (Let pat _ e) (lumap0, used0) =
  let (lumap', used') = patElems pat & foldl helper (lumap0, used0)
   in analyseExp (lumap', used') e
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
    analyseExp :: (LastUse, Used) -> Exp (Aliases GPUMem) -> (LastUse, Used)
    analyseExp (lumap, used) (BasicOp _) =
      let nms = freeIn e `namesSubtract` used
       in (insertNames pat_name nms lumap, used <> nms)
    analyseExp (lumap, used) (Apply _ args _ _) =
      let nms = freeIn $ map fst args
       in (insertNames pat_name nms lumap, used <> nms)
    analyseExp (lumap, used) (If cse then_body else_body dec) =
      let (lumap_then, used_then) = analyseBody lumap used then_body
          (lumap_else, used_else) = analyseBody lumap used else_body
          used' = used_then <> used_else
          nms = ((freeIn cse <> freeIn dec) `namesSubtract` used')
       in (insertNames pat_name nms (lumap_then <> lumap_else), used' <> nms)
    analyseExp (lumap, used) (DoLoop merge form body) =
      let (lumap', used') = analyseBody lumap used body
          nms = (freeIn merge <> freeIn form) `namesSubtract` used'
       in (insertNames pat_name nms lumap', used' <> nms)
    analyseExp (lumap, used) (Op (Alloc se sp)) =
      let nms = (freeIn se <> freeIn sp) `namesSubtract` used
       in (insertNames pat_name nms lumap, used <> nms)
    analyseExp (lumap, used) (Op (Inner (SizeOp sop))) =
      let nms = freeIn sop `namesSubtract` used
       in (insertNames pat_name nms lumap, used <> nms)
    analyseExp (lumap, used) (Op (Inner (OtherOp ()))) =
      (lumap, used)
    analyseExp (lumap, used) (Op (Inner (SegOp (SegMap lvl _ tps body)))) =
      let (lumap', used') = analyseKernelBody (lumap, used) body
          nms = (freeIn lvl <> freeIn tps) `namesSubtract` used'
       in (insertNames pat_name nms lumap', used' <> nms)
    analyseExp (lumap, used) (Op (Inner (SegOp (SegRed lvl _ binops tps body)))) =
      segOpHelper lumap used lvl binops tps body
    analyseExp (lumap, used) (Op (Inner (SegOp (SegScan lvl _ binops tps body)))) =
      segOpHelper lumap used lvl binops tps body
    analyseExp (lumap, used) (Op (Inner (SegOp (SegHist lvl _ binops tps body)))) =
      let (lumap', used') = foldr analyseHistOp (lumap, used) binops
          (lumap'', used'') = analyseKernelBody (lumap', used') body
          nms = (freeIn lvl <> freeIn tps) `namesSubtract` used''
       in (insertNames pat_name nms lumap'', used'' <> nms)
    analyseExp (lumap, used) (WithAcc _ l) =
      analyseLambda (lumap, used) l
    segOpHelper lumap used lvl binops tps body =
      let (lumap', used') = foldr analyseSegBinOp (lumap, used) binops
          (lumap'', used'') = analyseKernelBody (lumap', used') body
          nms = (freeIn lvl <> freeIn tps) `namesSubtract` used''
       in (insertNames pat_name nms lumap'', used'' <> nms)

analyseBody :: LastUse -> Used -> Body (Aliases GPUMem) -> (LastUse, Used)
analyseBody lumap used (Body _ stms result) =
  let used' = used <> freeIn result
   in analyseStms lumap used' stms

analyseKernelBody ::
  (LastUse, Used) ->
  KernelBody (Aliases GPUMem) ->
  (LastUse, Used)
analyseKernelBody (lumap, used) (KernelBody _ stms result) =
  let used' = used <> freeIn result
   in analyseStms lumap used' stms

analyseSegBinOp ::
  SegBinOp (Aliases GPUMem) ->
  (LastUse, Used) ->
  (LastUse, Used)
analyseSegBinOp (SegBinOp _ lambda neutral shp) (lumap, used) =
  let (lumap', used') = analyseLambda (lumap, used) lambda
      nms = (freeIn neutral <> freeIn shp) `namesSubtract` used'
   in (lumap', used' <> nms)

analyseHistOp ::
  HistOp (Aliases GPUMem) ->
  (LastUse, Used) ->
  (LastUse, Used)
analyseHistOp (HistOp width race dest neutral shp lambda) (lumap, used) =
  let (lumap', used') = analyseLambda (lumap, used) lambda
      nms =
        ( freeIn width <> freeIn race <> freeIn dest <> freeIn neutral
            <> freeIn shp
        )
          `namesSubtract` used'
   in (lumap', used' <> nms)

analyseLambda :: (LastUse, Used) -> Lambda (Aliases GPUMem) -> (LastUse, Used)
analyseLambda (lumap, used) (Lambda params body ret) =
  let (lumap', used') = analyseBody lumap used body
      used'' = used' <> freeIn params <> freeIn ret
   in (lumap', used'')

flipMap :: Map VName VName -> Map VName Names
flipMap m =
  M.toList m
    & fmap (swap . first oneName)
    & foldr (uncurry $ M.insertWith (<>)) mempty

insertNames :: VName -> Names -> LastUse -> LastUse
insertNames name names lumap =
  foldr (flip (M.insertWith $ \_ x -> x) name) lumap $ namesToList names
