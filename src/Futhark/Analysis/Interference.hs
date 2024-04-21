{-# LANGUAGE TypeFamilies #-}

-- | Interference analysis for Futhark programs.
module Futhark.Analysis.Interference (Graph, analyseProgGPU) where

import Control.Monad
import Control.Monad.Reader
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Futhark.Analysis.Alias qualified as AnlAls
import Futhark.Analysis.LastUse (LUTabFun)
import Futhark.Analysis.LastUse qualified as LastUse
import Futhark.Analysis.MemAlias qualified as MemAlias
import Futhark.IR.GPUMem
import Futhark.Util (cartesian, invertMap)

-- | The set of 'VName' currently in use.
type InUse = Names

-- | The set of 'VName' that are no longer in use.
type LastUsed = Names

-- | An interference graph. An element @(x, y)@ in the set means that there is
-- an undirected edge between @x@ and @y@, and therefore the lifetimes of @x@
-- and @y@ overlap and they "interfere" with each other. We assume that pairs
-- are always normalized, such that @x@ < @y@, before inserting. This should
-- prevent any duplicates. We also don't allow any pairs where @x == y@.
type Graph a = Set (a, a)

-- | Insert an edge between two values into the graph.
makeEdge :: (Ord a) => a -> a -> Graph a
makeEdge v1 v2
  | v1 == v2 = mempty
  | otherwise = S.singleton (min v1 v2, max v1 v2)

analyseStm ::
  (LocalScope GPUMem m) =>
  LUTabFun ->
  InUse ->
  Stm GPUMem ->
  m (InUse, LastUsed, Graph VName)
analyseStm lumap inuse0 stm =
  inScopeOf stm $ do
    let pat_name = patElemName $ head $ patElems $ stmPat stm

    new_mems <-
      stmPat stm
        & patElems
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
      M.lookup pat_name lumap
        & fromMaybe mempty
        & namesToList
        & mapM memInfo
        <&> catMaybes
        <&> namesFromList
        <&> namesIntersection inuse_outside

    pure
      ( (inuse_outside `namesSubtract` last_use_mems `namesSubtract` lus)
          <> new_mems,
        (lus <> last_use_mems) `namesSubtract` new_mems,
        graph
          <> cartesian
            makeEdge
            (namesToList inuse_outside)
            (namesToList $ inuse_outside <> inuse <> lus <> last_use_mems)
      )

-- We conservatively treat all memory arguments to a Loop to
-- interfere with each other, as well as anything used inside the
-- loop.  This could potentially be improved by looking at the
-- interference computed by the loop body wrt. the loop arguments, but
-- probably very few programs would benefit from this.
analyseLoopParams ::
  [(FParam GPUMem, SubExp)] ->
  (InUse, LastUsed, Graph VName) ->
  (InUse, LastUsed, Graph VName)
analyseLoopParams merge (inuse, lastused, graph) =
  (inuse, lastused, cartesian makeEdge mems (mems <> inner_mems) <> graph)
  where
    mems = mapMaybe isMemArg merge
    inner_mems = namesToList lastused <> namesToList inuse
    isMemArg (Param _ _ MemMem {}, Var v) = Just v
    isMemArg _ = Nothing

analyseExp ::
  (LocalScope GPUMem m) =>
  LUTabFun ->
  InUse ->
  Exp GPUMem ->
  m (InUse, LastUsed, Graph VName)
analyseExp lumap inuse_outside expr =
  case expr of
    Match _ cases defbody _ ->
      fmap mconcat $
        mapM (analyseBody lumap inuse_outside) $
          defbody : map caseBody cases
    Loop merge _ body ->
      analyseLoopParams merge <$> analyseBody lumap inuse_outside body
    Op (Inner (SegOp segop)) -> do
      analyseSegOp lumap inuse_outside segop
    _ ->
      pure mempty

analyseKernelBody ::
  (LocalScope GPUMem m) =>
  LUTabFun ->
  InUse ->
  KernelBody GPUMem ->
  m (InUse, LastUsed, Graph VName)
analyseKernelBody lumap inuse body = analyseStms lumap inuse $ kernelBodyStms body

analyseBody ::
  (LocalScope GPUMem m) =>
  LUTabFun ->
  InUse ->
  Body GPUMem ->
  m (InUse, LastUsed, Graph VName)
analyseBody lumap inuse body = analyseStms lumap inuse $ bodyStms body

analyseStms ::
  (LocalScope GPUMem m) =>
  LUTabFun ->
  InUse ->
  Stms GPUMem ->
  m (InUse, LastUsed, Graph VName)
analyseStms lumap inuse0 stms = do
  inScopeOf stms $ foldM helper (inuse0, mempty, mempty) $ stmsToList stms
  where
    helper (inuse, lus, graph) stm = do
      (inuse', lus', graph') <- analyseStm lumap inuse stm
      pure (inuse', lus' <> lus, graph' <> graph)

analyseSegOp ::
  (LocalScope GPUMem m) =>
  LUTabFun ->
  InUse ->
  SegOp lvl GPUMem ->
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
  pure (inuse'', lus' <> lus'', graph <> graph')

segWithBinOps ::
  (LocalScope GPUMem m) =>
  LUTabFun ->
  InUse ->
  [SegBinOp GPUMem] ->
  KernelBody GPUMem ->
  m (InUse, LastUsed, Graph VName)
segWithBinOps lumap inuse binops body = do
  (inuse', lus', graph) <- analyseKernelBody lumap inuse body
  (inuse'', lus'', graph') <-
    mconcat
      <$> mapM
        (analyseSegBinOp lumap inuse')
        binops
  pure (inuse'', lus' <> lus'', graph <> graph')

analyseSegBinOp ::
  (LocalScope GPUMem m) =>
  LUTabFun ->
  InUse ->
  SegBinOp GPUMem ->
  m (InUse, LastUsed, Graph VName)
analyseSegBinOp lumap inuse (SegBinOp _ lambda _ _) =
  analyseLambda lumap inuse lambda

analyseHistOp ::
  (LocalScope GPUMem m) =>
  LUTabFun ->
  InUse ->
  HistOp GPUMem ->
  m (InUse, LastUsed, Graph VName)
analyseHistOp lumap inuse = analyseLambda lumap inuse . histOp

analyseLambda ::
  (LocalScope GPUMem m) =>
  LUTabFun ->
  InUse ->
  Lambda GPUMem ->
  m (InUse, LastUsed, Graph VName)
analyseLambda lumap inuse = analyseBody lumap inuse . lambdaBody

analyseProgGPU :: Prog GPUMem -> Graph VName
analyseProgGPU prog = onConsts (progConsts prog) <> foldMap onFun (progFuns prog)
  where
    (consts_aliases, funs_aliases) = MemAlias.analyzeGPUMem prog
    (lumap_consts, lumap) = LastUse.lastUseGPUMem $ AnlAls.aliasAnalysis prog
    onFun f =
      applyAliases (fromMaybe mempty $ M.lookup (funDefName f) funs_aliases) $
        runReader (analyseGPU (lumap M.! funDefName f) $ bodyStms $ funDefBody f) $
          scopeOf f
    onConsts stms =
      applyAliases consts_aliases $
        runReader (analyseGPU lumap_consts stms) (mempty :: Scope GPUMem)

applyAliases :: MemAlias.MemAliases -> Graph VName -> Graph VName
applyAliases aliases =
  -- For each pair @(x, y)@ in graph, all memory aliases of x should interfere with all memory aliases of y
  foldMap
    ( \(x, y) ->
        let xs = MemAlias.aliasesOf aliases x <> oneName x
            ys = MemAlias.aliasesOf aliases y <> oneName y
         in cartesian makeEdge (namesToList xs) (namesToList ys)
    )

-- | Perform interference analysis on the given statements. The result is a
-- triple of the names currently in use, names that hit their last use somewhere
-- within, and the resulting graph.
analyseGPU ::
  (LocalScope GPUMem m) =>
  LUTabFun ->
  Stms GPUMem ->
  m (Graph VName)
analyseGPU lumap stms = do
  (_, _, graph) <- analyseGPU' lumap stms
  -- We need to insert edges between memory blocks which differ in size, if they
  -- are in DefaultSpace. The problem is that during memory expansion,
  -- DefaultSpace arrays in kernels are interleaved. If the element sizes of two
  -- merged memory blocks are different, threads might try to read and write to
  -- overlapping memory positions. More information here:
  -- https://munksgaard.me/technical-diary/2020-12-30.html#org210775b
  spaces <- M.filter (== DefaultSpace) <$> memSpaces stms
  inv_size_map <-
    memSizes stms
      <&> flip M.restrictKeys (S.fromList $ M.keys spaces)
      <&> invertMap
  let new_edges =
        cartesian
          (\x y -> if x /= y then cartesian makeEdge x y else mempty)
          inv_size_map
          inv_size_map
  pure $ graph <> new_edges

-- | Return a mapping from memory blocks to their element sizes in the given
-- statements.
memSizes :: (LocalScope GPUMem m) => Stms GPUMem -> m (Map VName Int)
memSizes stms =
  inScopeOf stms $ fmap mconcat <$> mapM memSizesStm $ stmsToList stms
  where
    memSizesStm :: (LocalScope GPUMem m) => Stm GPUMem -> m (Map VName Int)
    memSizesStm (Let pat _ e) = do
      arraySizes <- fmap mconcat <$> mapM memElemSize $ patNames pat
      arraySizes' <- memSizesExp e
      pure $ arraySizes <> arraySizes'
    memSizesExp :: (LocalScope GPUMem m) => Exp GPUMem -> m (Map VName Int)
    memSizesExp (Op (Inner (SegOp segop))) =
      let body = segBody segop
       in inScopeOf (kernelBodyStms body)
            $ fmap mconcat
              <$> mapM memSizesStm
            $ stmsToList
            $ kernelBodyStms body
    memSizesExp (Match _ cases defbody _) = do
      mconcat <$> mapM (memSizes . bodyStms) (defbody : map caseBody cases)
    memSizesExp (Loop _ _ body) =
      memSizes $ bodyStms body
    memSizesExp _ = pure mempty

-- | Return a mapping from memory blocks to the space they are allocated in.
memSpaces :: (LocalScope GPUMem m) => Stms GPUMem -> m (Map VName Space)
memSpaces stms =
  pure $ foldMap getSpacesStm stms
  where
    getSpacesStm :: Stm GPUMem -> Map VName Space
    getSpacesStm (Let (Pat [PatElem name _]) _ (Op (Alloc _ sp))) =
      M.singleton name sp
    getSpacesStm (Let _ _ (Op (Alloc _ _))) = error "impossible"
    getSpacesStm (Let _ _ (Op (Inner (SegOp segop)))) =
      foldMap getSpacesStm $ kernelBodyStms $ segBody segop
    getSpacesStm (Let _ _ (Match _ cases defbody _)) =
      foldMap (foldMap getSpacesStm . bodyStms) $ defbody : map caseBody cases
    getSpacesStm (Let _ _ (Loop _ _ body)) =
      foldMap getSpacesStm (bodyStms body)
    getSpacesStm _ = mempty

analyseGPU' ::
  (LocalScope GPUMem m) =>
  LUTabFun ->
  Stms GPUMem ->
  m (InUse, LastUsed, Graph VName)
analyseGPU' lumap stms =
  mconcat . toList <$> mapM helper stms
  where
    helper ::
      (LocalScope GPUMem m) =>
      Stm GPUMem ->
      m (InUse, LastUsed, Graph VName)
    helper stm@Let {stmExp = Op (Inner (SegOp segop))} =
      inScopeOf stm $ analyseSegOp lumap mempty segop
    helper stm@Let {stmExp = Match _ cases defbody _} =
      inScopeOf stm $
        mconcat
          <$> mapM (analyseGPU' lumap . bodyStms) (defbody : map caseBody cases)
    helper stm@Let {stmExp = Loop merge _ body} =
      fmap (analyseLoopParams merge) . inScopeOf stm $
        analyseGPU' lumap $
          bodyStms body
    helper stm =
      inScopeOf stm $ pure mempty

memInfo :: (LocalScope GPUMem m) => VName -> m (Maybe VName)
memInfo vname = do
  summary <- asksScope (fmap nameInfoToMemInfo . M.lookup vname)
  case summary of
    Just (MemArray _ _ _ (ArrayIn mem _)) ->
      pure $ Just mem
    _ ->
      pure Nothing

-- | Returns a mapping from memory block to element size. The input is the
-- `VName` of a variable (supposedly an array), and the result is a mapping from
-- the memory block of that array to element size of the array.
memElemSize :: (LocalScope GPUMem m) => VName -> m (Map VName Int)
memElemSize vname = do
  summary <- asksScope (fmap nameInfoToMemInfo . M.lookup vname)
  case summary of
    Just (MemArray pt _ _ (ArrayIn mem _)) ->
      pure $ M.singleton mem (primByteSize pt)
    _ ->
      pure mempty
