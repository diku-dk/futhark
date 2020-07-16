{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.Analysis.Interference (Graph, analyse, analyseKernels) where

import Control.Monad.Reader
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Futhark.Analysis.LastUse (LastUseMap)
import qualified Futhark.Analysis.LastUse as LastUse
import Futhark.IR.KernelsMem

type InUse = Names

type LastUsed = Names

type Graph a = Set (a, a)

insertEdge :: Ord a => a -> a -> Graph a -> Graph a
insertEdge v1 v2 g
  | v1 == v2 = g
  | otherwise = Set.insert (min v1 v2, max v1 v2) g

cartesian :: Names -> Names -> Graph VName
cartesian ns1 ns2 =
  [(min x y, max x y) | x <- namesToList ns1, y <- namesToList ns2]
    & foldr (uncurry insertEdge) mempty

analyseStm ::
  LocalScope KernelsMem m =>
  LastUseMap ->
  InUse ->
  Stm KernelsMem ->
  m (InUse, LastUsed, Graph VName)
analyseStm lumap inuse0 stm =
  inScopeOf stm $ do
    let pat_name = patElemName $ head $ patternValueElements $ stmPattern stm

    new_mems <-
      stmPattern stm
        & patternValueElements
        & mapM (memInfo . patElemName)
        <&> catMaybes
        <&> namesFromList

    -- `new_mems` should interfere with any mems inside the statement expression
    let inuse_outside = inuse0 <> new_mems

    -- `inuse` is the set of memory blocks that are inuse at the end of any code
    -- bodies inside the expression. `lus` is the set of all memory blocks that
    -- have reached their last use in any code bodies inside the
    -- expression. `graph` is the interference graph computed for any code
    -- bodies inside the expression.
    (inuse, lus, graph) <- analyseExp lumap inuse_outside (stmExp stm)

    last_use_mems <-
      Map.lookup pat_name lumap
        & fromMaybe mempty
        & namesToList
        & mapM memInfo
        <&> catMaybes
        <&> namesFromList
        <&> namesIntersection inuse_outside

    return
      ( (inuse_outside `namesSubtract` last_use_mems `namesSubtract` lus)
          <> new_mems,
        (lus <> last_use_mems) `namesSubtract` new_mems,
        graph
          <> ( inuse_outside
                 `cartesian` (inuse_outside <> inuse <> lus <> last_use_mems)
             )
      )

analyseExp ::
  LocalScope KernelsMem m =>
  LastUseMap ->
  InUse ->
  Exp KernelsMem ->
  m (InUse, LastUsed, Graph VName)
analyseExp lumap inuse_outside expr =
  case expr of
    If _ then_body else_body _ -> do
      res1 <- analyseBody lumap inuse_outside then_body
      res2 <- analyseBody lumap inuse_outside else_body
      return $ res1 <> res2
    DoLoop _ _ _ body -> do
      analyseBody lumap inuse_outside body
    Op (Inner (SegOp segop)) -> do
      analyseSegOp lumap inuse_outside segop
    _ ->
      return mempty

analyseKernelBody ::
  LocalScope KernelsMem m =>
  LastUseMap ->
  InUse ->
  KernelBody KernelsMem ->
  m (InUse, LastUsed, Graph VName)
analyseKernelBody lumap inuse body = analyseStms lumap inuse $ kernelBodyStms body

analyseBody ::
  LocalScope KernelsMem m =>
  LastUseMap ->
  InUse ->
  Body KernelsMem ->
  m (InUse, LastUsed, Graph VName)
analyseBody lumap inuse body = analyseStms lumap inuse $ bodyStms body

analyseStms ::
  LocalScope KernelsMem m =>
  LastUseMap ->
  InUse ->
  Stms KernelsMem ->
  m (InUse, LastUsed, Graph VName)
analyseStms lumap inuse0 stms = do
  inScopeOf stms $ do
    foldM
      ( \(inuse, lus, graph) stm -> do
          (inuse', lus', graph') <- analyseStm lumap inuse stm
          return (inuse', lus' <> lus, graph' <> graph)
      )
      (inuse0, mempty, mempty)
      $ stmsToList stms

analyseSegOp ::
  LocalScope KernelsMem m =>
  LastUseMap ->
  InUse ->
  SegOp lvl KernelsMem ->
  m (InUse, LastUsed, Graph VName)
analyseSegOp lumap inuse (SegMap _ _ _ body) =
  analyseKernelBody lumap inuse body
analyseSegOp lumap inuse (SegRed _ _ binops _ body) =
  segWithBinOps lumap inuse binops body
analyseSegOp lumap inuse (SegScan _ _ binops _ body) = do
  segWithBinOps lumap inuse binops body
analyseSegOp lumap inuse (SegHist _ _ histops _ body) = do
  (inuse', lus', graph) <- analyseKernelBody lumap inuse body
  (inuse'', lus'', graph') <- mconcat <$> mapM (analyseHistOp lumap inuse') histops
  return (inuse'', lus' <> lus'', graph <> graph')

segWithBinOps ::
  LocalScope KernelsMem m =>
  LastUseMap ->
  InUse ->
  [SegBinOp KernelsMem] ->
  KernelBody KernelsMem ->
  m (InUse, LastUsed, Graph VName)
segWithBinOps lumap inuse binops body = do
  (inuse', lus', graph) <- analyseKernelBody lumap inuse body
  (inuse'', lus'', graph') <- mconcat <$> mapM (analyseSegBinOp lumap inuse') binops
  return (inuse'', lus' <> lus'', graph <> graph')

analyseSegBinOp ::
  LocalScope KernelsMem m =>
  LastUseMap ->
  InUse ->
  SegBinOp KernelsMem ->
  m (InUse, LastUsed, Graph VName)
analyseSegBinOp lumap inuse (SegBinOp _ lambda _ _) =
  analyseLambda lumap inuse lambda

analyseHistOp ::
  LocalScope KernelsMem m =>
  LastUseMap ->
  InUse ->
  HistOp KernelsMem ->
  m (InUse, LastUsed, Graph VName)
analyseHistOp lumap inuse histop =
  analyseLambda lumap inuse (histOp histop)

analyseLambda ::
  LocalScope KernelsMem m =>
  LastUseMap ->
  InUse ->
  Lambda KernelsMem ->
  m (InUse, LastUsed, Graph VName)
analyseLambda lumap inuse (Lambda _ body _) =
  analyseBody lumap inuse body

analyseKernels ::
  LocalScope KernelsMem m =>
  LastUseMap ->
  Stms KernelsMem ->
  m (InUse, LastUsed, Graph VName)
analyseKernels lumap stms =
  mconcat . toList <$> mapM helper stms
  where
    helper ::
      LocalScope KernelsMem m =>
      Stm KernelsMem ->
      m (InUse, LastUsed, Graph VName)
    helper stm@Let {stmExp = Op (Inner (SegOp segop))} =
      inScopeOf stm $ analyseSegOp lumap mempty segop
    helper stm@Let {stmExp = If _ then_body else_body _} =
      inScopeOf stm $ do
        res1 <- analyseKernels lumap (bodyStms then_body)
        res2 <- analyseKernels lumap (bodyStms else_body)
        return (res1 <> res2)
    helper stm@Let {stmExp = DoLoop _ _ _ body} =
      inScopeOf stm $
        analyseKernels lumap $ bodyStms body
    helper stm =
      inScopeOf stm $ return mempty

analyse :: Prog KernelsMem -> Graph VName
analyse prog =
  let (lumap, _) = LastUse.analyseProg prog
      (_, _, graph) =
        foldMap
          ( \f ->
              runReader (analyseKernels lumap (bodyStms $ funDefBody f)) $
                scopeOf f
          )
          $ progFuns prog
   in graph

nameInfoToMemInfo :: Mem lore => NameInfo lore -> MemBound NoUniqueness
nameInfoToMemInfo info =
  case info of
    FParamName summary -> noUniquenessReturns summary
    LParamName summary -> summary
    LetName summary -> summary
    IndexName it -> MemPrim $ IntType it

memInfo :: LocalScope KernelsMem m => VName -> m (Maybe VName)
memInfo vname = do
  summary <- asksScope (fmap nameInfoToMemInfo . Map.lookup vname)
  case summary of
    Just (MemArray _ _ _ (ArrayIn mem _)) ->
      return $ Just mem
    _ ->
      return Nothing
