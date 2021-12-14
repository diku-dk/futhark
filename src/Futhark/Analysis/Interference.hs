{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Interference analysis for Futhark programs.
module Futhark.Analysis.Interference (Graph, analyseGPU) where

import Control.Monad.Reader
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Futhark.Analysis.LastUse (LastUseMap)
import Futhark.IR.GPUMem
import Futhark.Util (invertMap)

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
makeEdge :: Ord a => a -> a -> Graph a
makeEdge v1 v2
  | v1 == v2 = mempty
  | otherwise = S.singleton (min v1 v2, max v1 v2)

-- | Compute the cartesian product of two foldable collections, using the given
-- combinator function.
cartesian :: (Monoid m, Foldable t) => (a -> a -> m) -> t a -> t a -> m
cartesian f xs ys =
  [(x, y) | x <- toList xs, y <- toList ys]
    & foldMap (uncurry f)

analyseStm ::
  LocalScope GPUMem m =>
  LastUseMap ->
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

    return
      ( (inuse_outside `namesSubtract` last_use_mems `namesSubtract` lus)
          <> new_mems,
        (lus <> last_use_mems) `namesSubtract` new_mems,
        graph
          <> cartesian
            makeEdge
            (namesToList inuse_outside)
            (namesToList $ inuse_outside <> inuse <> lus <> last_use_mems)
      )

-- We conservatively treat all memory arguments to a DoLoop to
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
  LocalScope GPUMem m =>
  LastUseMap ->
  InUse ->
  Exp GPUMem ->
  m (InUse, LastUsed, Graph VName)
analyseExp lumap inuse_outside expr =
  case expr of
    If _ then_body else_body _ -> do
      res1 <- analyseBody lumap inuse_outside then_body
      res2 <- analyseBody lumap inuse_outside else_body
      return $ res1 <> res2
    DoLoop merge _ body ->
      analyseLoopParams merge <$> analyseBody lumap inuse_outside body
    Op (Inner (SegOp segop)) -> do
      analyseSegOp lumap inuse_outside segop
    _ ->
      return mempty

analyseKernelBody ::
  LocalScope GPUMem m =>
  LastUseMap ->
  InUse ->
  KernelBody GPUMem ->
  m (InUse, LastUsed, Graph VName)
analyseKernelBody lumap inuse body = analyseStms lumap inuse $ kernelBodyStms body

analyseBody ::
  LocalScope GPUMem m =>
  LastUseMap ->
  InUse ->
  Body GPUMem ->
  m (InUse, LastUsed, Graph VName)
analyseBody lumap inuse body = analyseStms lumap inuse $ bodyStms body

analyseStms ::
  LocalScope GPUMem m =>
  LastUseMap ->
  InUse ->
  Stms GPUMem ->
  m (InUse, LastUsed, Graph VName)
analyseStms lumap inuse0 stms = do
  inScopeOf stms $ foldM helper (inuse0, mempty, mempty) $ stmsToList stms
  where
    helper (inuse, lus, graph) stm = do
      (inuse', lus', graph') <- analyseStm lumap inuse stm
      return (inuse', lus' <> lus, graph' <> graph)

analyseSegOp ::
  LocalScope GPUMem m =>
  LastUseMap ->
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
  return (inuse'', lus' <> lus'', graph <> graph')

segWithBinOps ::
  LocalScope GPUMem m =>
  LastUseMap ->
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
  return (inuse'', lus' <> lus'', graph <> graph')

analyseSegBinOp ::
  LocalScope GPUMem m =>
  LastUseMap ->
  InUse ->
  SegBinOp GPUMem ->
  m (InUse, LastUsed, Graph VName)
analyseSegBinOp lumap inuse (SegBinOp _ lambda _ _) =
  analyseLambda lumap inuse lambda

analyseHistOp ::
  LocalScope GPUMem m =>
  LastUseMap ->
  InUse ->
  HistOp GPUMem ->
  m (InUse, LastUsed, Graph VName)
analyseHistOp lumap inuse histop =
  analyseLambda lumap inuse (histOp histop)

analyseLambda ::
  LocalScope GPUMem m =>
  LastUseMap ->
  InUse ->
  Lambda GPUMem ->
  m (InUse, LastUsed, Graph VName)
analyseLambda lumap inuse (Lambda _ body _) =
  analyseBody lumap inuse body

-- | Perform interference analysis on the given statements. The result is a
-- triple of the names currently in use, names that hit their last use somewhere
-- within, and the resulting graph.
analyseGPU ::
  LocalScope GPUMem m =>
  LastUseMap ->
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
  return $ graph <> new_edges

-- | Return a mapping from memory blocks to their element sizes in the given
-- statements.
memSizes :: LocalScope GPUMem m => Stms GPUMem -> m (Map VName Int)
memSizes stms =
  inScopeOf stms $ fmap mconcat <$> mapM memSizesStm $ stmsToList stms
  where
    memSizesStm :: LocalScope GPUMem m => Stm GPUMem -> m (Map VName Int)
    memSizesStm (Let pat _ e) = do
      arraySizes <- fmap mconcat <$> mapM memElemSize $ patNames pat
      arraySizes' <- memSizesExp e
      return $ arraySizes <> arraySizes'
    memSizesExp :: LocalScope GPUMem m => Exp GPUMem -> m (Map VName Int)
    memSizesExp (Op (Inner (SegOp segop))) =
      let body = segBody segop
       in inScopeOf (kernelBodyStms body) $
            fmap mconcat
              <$> mapM memSizesStm
              $ stmsToList $ kernelBodyStms body
    memSizesExp (If _ then_body else_body _) = do
      then_res <- memSizes $ bodyStms then_body
      else_res <- memSizes $ bodyStms else_body
      return $ then_res <> else_res
    memSizesExp (DoLoop _ _ body) =
      memSizes $ bodyStms body
    memSizesExp _ = return mempty

-- | Return a mapping from memory blocks to the space they are allocated in.
memSpaces :: LocalScope GPUMem m => Stms GPUMem -> m (Map VName Space)
memSpaces stms =
  return $ foldMap getSpacesStm stms
  where
    getSpacesStm :: Stm GPUMem -> Map VName Space
    getSpacesStm (Let (Pat [PatElem name _]) _ (Op (Alloc _ sp))) =
      M.singleton name sp
    getSpacesStm (Let _ _ (Op (Alloc _ _))) = error "impossible"
    getSpacesStm (Let _ _ (Op (Inner (SegOp segop)))) =
      foldMap getSpacesStm $ kernelBodyStms $ segBody segop
    getSpacesStm (Let _ _ (If _ then_body else_body _)) =
      foldMap getSpacesStm (bodyStms then_body)
        <> foldMap getSpacesStm (bodyStms else_body)
    getSpacesStm (Let _ _ (DoLoop _ _ body)) =
      foldMap getSpacesStm (bodyStms body)
    getSpacesStm _ = mempty

analyseGPU' ::
  LocalScope GPUMem m =>
  LastUseMap ->
  Stms GPUMem ->
  m (InUse, LastUsed, Graph VName)
analyseGPU' lumap stms =
  mconcat . toList <$> mapM helper stms
  where
    helper ::
      LocalScope GPUMem m =>
      Stm GPUMem ->
      m (InUse, LastUsed, Graph VName)
    helper stm@Let {stmExp = Op (Inner (SegOp segop))} =
      inScopeOf stm $ analyseSegOp lumap mempty segop
    helper stm@Let {stmExp = If _ then_body else_body _} =
      inScopeOf stm $ do
        res1 <- analyseGPU' lumap (bodyStms then_body)
        res2 <- analyseGPU' lumap (bodyStms else_body)
        return (res1 <> res2)
    helper stm@Let {stmExp = DoLoop merge _ body} =
      fmap (analyseLoopParams merge) . inScopeOf stm $
        analyseGPU' lumap $ bodyStms body
    helper stm =
      inScopeOf stm $ return mempty

nameInfoToMemInfo :: Mem rep inner => NameInfo rep -> MemBound NoUniqueness
nameInfoToMemInfo info =
  case info of
    FParamName summary -> noUniquenessReturns summary
    LParamName summary -> summary
    LetName summary -> letDecMem summary
    IndexName it -> MemPrim $ IntType it

memInfo :: LocalScope GPUMem m => VName -> m (Maybe VName)
memInfo vname = do
  summary <- asksScope (fmap nameInfoToMemInfo . M.lookup vname)
  case summary of
    Just (MemArray _ _ _ (ArrayIn mem _)) ->
      return $ Just mem
    _ ->
      return Nothing

-- | Returns a mapping from memory block to element size. The input is the
-- `VName` of a variable (supposedly an array), and the result is a mapping from
-- the memory block of that array to element size of the array.
memElemSize :: LocalScope GPUMem m => VName -> m (Map VName Int)
memElemSize vname = do
  summary <- asksScope (fmap nameInfoToMemInfo . M.lookup vname)
  case summary of
    Just (MemArray pt _ _ (ArrayIn mem _)) ->
      return $ M.singleton mem (primByteSize pt)
    _ ->
      return mempty
