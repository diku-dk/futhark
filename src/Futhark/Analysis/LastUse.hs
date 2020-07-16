{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Futhark.Analysis.LastUse (LastUseMap, analyseProg) where

import Control.Arrow (first)
import Data.Foldable
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple
import Futhark.Analysis.Alias (aliasAnalysis)
import Futhark.IR.Aliases
import Futhark.IR.KernelsMem

-- | LastUseMap tells which names were last used in a given statement.
-- Statements are uniquely identified by the 'VName' of the first value
-- parameter in the statement pattern. 'Names' is the set of names last used.
type LastUseMap = Map VName Names

type LastUse = Map VName VName

type Used = Names

analyseProg :: Prog KernelsMem -> (LastUseMap, Used)
analyseProg prog =
  let consts =
        progConsts prog
          & concatMap (toList . fmap patElemName . patternValueElements . stmPattern)
          & namesFromList
      funs = progFuns $ aliasAnalysis prog
      (lus, used) = foldMap (analyseFun mempty consts) funs
   in (flipMap lus, used)

analyseFun :: LastUse -> Used -> FunDef (Aliases KernelsMem) -> (LastUse, Used)
analyseFun lumap used fun =
  let (lumap', used') = analyseBody lumap used $ funDefBody fun
   in (lumap', used' <> freeIn (funDefParams fun))

analyseStms :: LastUse -> Used -> Stms (Aliases KernelsMem) -> (LastUse, Used)
analyseStms lumap used stms = foldr analyseStm (lumap, used) $ stmsToList stms

analyseStm :: Stm (Aliases KernelsMem) -> (LastUse, Used) -> (LastUse, Used)
analyseStm (Let pat _ e) (lumap0, used0) =
  let (lumap', used') =
        patternValueElements pat
          & foldl
            ( \(lumap_acc, used_acc) (PatElem name (aliases, _)) ->
                -- Any aliases of `name` should have the same last-use as `name`
                ( case Map.lookup name lumap_acc of
                    Just name' ->
                      insertNames name' (unAliases aliases) lumap_acc
                    Nothing -> lumap_acc,
                  used_acc <> unAliases aliases
                )
            )
            (lumap0, used0)
   in analyseExp (lumap', used') e
  where
    pat_name = patElemName $ head $ patternValueElements pat
    analyseExp :: (LastUse, Used) -> Exp (Aliases KernelsMem) -> (LastUse, Used)
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
    analyseExp (lumap, used) (DoLoop ctx vals form body) =
      let (lumap', used') = analyseBody lumap used body
          nms = (freeIn ctx <> freeIn vals <> freeIn form) `namesSubtract` used'
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
    segOpHelper lumap used lvl binops tps body =
      let (lumap', used') = foldr analyseSegBinOp (lumap, used) binops
          (lumap'', used'') = analyseKernelBody (lumap', used') body
          nms = (freeIn lvl <> freeIn tps) `namesSubtract` used''
       in (insertNames pat_name nms lumap'', used'' <> nms)

analyseBody :: LastUse -> Used -> Body (Aliases KernelsMem) -> (LastUse, Used)
analyseBody lumap used (Body _ stms result) =
  let used' = used <> freeIn result
   in analyseStms lumap used' stms

analyseKernelBody ::
  (LastUse, Used) ->
  KernelBody (Aliases KernelsMem) ->
  (LastUse, Used)
analyseKernelBody (lumap, used) (KernelBody _ stms result) =
  let used' = used <> freeIn result
   in analyseStms lumap used' stms

analyseSegBinOp ::
  SegBinOp (Aliases KernelsMem) ->
  (LastUse, Used) ->
  (LastUse, Used)
analyseSegBinOp (SegBinOp _ lambda neutral shp) (lumap, used) =
  let (lumap', used') = analyseLambda (lumap, used) lambda
      nms = (freeIn neutral <> freeIn shp) `namesSubtract` used'
   in (lumap', used' <> nms)

analyseHistOp ::
  HistOp (Aliases KernelsMem) ->
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

analyseLambda :: (LastUse, Used) -> Lambda (Aliases KernelsMem) -> (LastUse, Used)
analyseLambda (lumap, used) (Lambda params body ret) =
  let (lumap', used') = analyseBody lumap used body
      used'' = used' <> freeIn params <> freeIn ret
   in (lumap', used'')

flipMap :: Map VName VName -> Map VName Names
flipMap m =
  Map.toList m
    & fmap (swap . first oneName)
    & foldr (uncurry $ Map.insertWith (<>)) mempty

insertNames :: VName -> Names -> LastUse -> LastUse
insertNames name names lumap =
  foldr (flip (Map.insertWith $ flip const) name) lumap $ namesToList names
