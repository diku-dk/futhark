{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.Optimise.ArrayShortCircuiting.MemRefAggreg
  ( recordMemRefUses,
    freeVarSubstitutions,
    translateAccessSummary,
    aggSummaryLoopTotal,
    aggSummaryLoopPartial,
    aggSummaryMapPartial,
    aggSummaryMapTotal,
    noMemOverlap,
  )
where

import Control.Monad
import Data.Function ((&))
import Data.List (intersect, partition, uncons)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.Analysis.AlgSimplify
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Aliases
import Futhark.IR.Mem
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.MonadFreshNames
import Futhark.Optimise.ArrayShortCircuiting.DataStructs
import Futhark.Optimise.ArrayShortCircuiting.TopdownAnalysis
import Futhark.Util

-----------------------------------------------------
-- Some translations of Accesses and Ixfuns        --
-----------------------------------------------------

-- | Checks whether the index function can be translated at the current program
-- point and also returns the substitutions.  It comes down to answering the
-- question: "can one perform enough substitutions (from the bottom-up scalar
-- table) until all vars appearing in the index function are defined in the
-- current scope?"
freeVarSubstitutions ::
  (FreeIn a) =>
  ScopeTab rep ->
  ScalarTab ->
  a ->
  Maybe FreeVarSubsts
freeVarSubstitutions scope0 scals0 indfun =
  freeVarSubstitutions' mempty $ namesToList $ freeIn indfun
  where
    freeVarSubstitutions' :: FreeVarSubsts -> [VName] -> Maybe FreeVarSubsts
    freeVarSubstitutions' subs [] = Just subs
    freeVarSubstitutions' subs0 fvs =
      let fvs_not_in_scope = filter (`M.notMember` scope0) fvs
       in case mapAndUnzipM getSubstitution fvs_not_in_scope of
            -- We require that all free variables can be substituted
            Just (subs, new_fvs) ->
              freeVarSubstitutions' (subs0 <> mconcat subs) $ concat new_fvs
            Nothing -> Nothing
    getSubstitution v
      | Just pe <- M.lookup v scals0,
        IntType _ <- primExpType pe =
          Just (M.singleton v $ TPrimExp pe, namesToList $ freeIn pe)
    getSubstitution _v = Nothing

-- | Translates free variables in an access summary
translateAccessSummary :: ScopeTab rep -> ScalarTab -> AccessSummary -> AccessSummary
translateAccessSummary _ _ Undeterminable = Undeterminable
translateAccessSummary scope0 scals0 (Set slmads)
  | Just subs <- freeVarSubstitutions scope0 scals0 slmads =
      slmads
        & S.map (LMAD.substitute subs)
        & Set
translateAccessSummary _ _ _ = Undeterminable

-- | This function computes the written and read memory references for the current statement
getUseSumFromStm ::
  (Op rep ~ MemOp inner rep, HasMemBlock (Aliases rep)) =>
  TopdownEnv rep ->
  CoalsTab ->
  Stm (Aliases rep) ->
  -- | A pair of written and written+read memory locations, along with their
  -- associated array and the index function used
  Maybe ([(VName, VName, LMAD)], [(VName, VName, LMAD)])
getUseSumFromStm td_env coal_tab (Let _ _ (BasicOp (Index arr (Slice slc))))
  | Just (MemBlock _ shp _ _) <- getScopeMemInfo arr (scope td_env),
    length slc == length (shapeDims shp) && all isFix slc = do
      (mem_b, mem_arr, ixfn_arr) <- getDirAliasedIxfn td_env coal_tab arr
      let new_ixfn = LMAD.slice ixfn_arr $ Slice $ map (fmap pe64) slc
      pure ([], [(mem_b, mem_arr, new_ixfn)])
  where
    isFix DimFix {} = True
    isFix _ = False
getUseSumFromStm _ _ (Let Pat {} _ (BasicOp Index {})) = Just ([], []) -- incomplete slices
getUseSumFromStm _ _ (Let Pat {} _ (BasicOp FlatIndex {})) = Just ([], []) -- incomplete slices
getUseSumFromStm td_env coal_tab (Let (Pat pes) _ (BasicOp (ArrayLit ses _))) =
  let rds = mapMaybe (getDirAliasedIxfn td_env coal_tab) $ mapMaybe seName ses
      wrts = mapMaybe (getDirAliasedIxfn td_env coal_tab . patElemName) pes
   in Just (wrts, wrts ++ rds)
  where
    seName (Var a) = Just a
    seName (Constant _) = Nothing
-- In place update @x[slc] <- a@. In the "in-place update" case,
--   summaries should be added after the old variable @x@ has
--   been added in the active coalesced table.
getUseSumFromStm td_env coal_tab (Let (Pat [x']) _ (BasicOp (Update _ _x (Slice slc) a_se))) = do
  (m_b, m_x, x_ixfn) <- getDirAliasedIxfn td_env coal_tab (patElemName x')
  let x_ixfn_slc = LMAD.slice x_ixfn $ Slice $ map (fmap pe64) slc
      r1 = (m_b, m_x, x_ixfn_slc)
  case a_se of
    Constant _ -> Just ([r1], [r1])
    Var a -> case getDirAliasedIxfn td_env coal_tab a of
      Nothing -> Just ([r1], [r1])
      Just r2 -> Just ([r1], [r1, r2])
getUseSumFromStm td_env coal_tab (Let (Pat [y]) _ (BasicOp (Replicate (Shape []) (Var x)))) = do
  -- y = copy x
  wrt <- getDirAliasedIxfn td_env coal_tab $ patElemName y
  rd <- getDirAliasedIxfn td_env coal_tab x
  pure ([wrt], [wrt, rd])
getUseSumFromStm _ _ (Let Pat {} _ (BasicOp (Replicate (Shape []) _))) =
  error "Impossible"
getUseSumFromStm td_env coal_tab (Let (Pat ys) _ (BasicOp (Concat _i (a :| bs) _ses))) =
  -- concat
  let ws = mapMaybe (getDirAliasedIxfn td_env coal_tab . patElemName) ys
      rs = mapMaybe (getDirAliasedIxfn td_env coal_tab) (a : bs)
   in Just (ws, ws ++ rs)
getUseSumFromStm td_env coal_tab (Let (Pat ys) _ (BasicOp (Manifest _perm x))) =
  let ws = mapMaybe (getDirAliasedIxfn td_env coal_tab . patElemName) ys
      rs = mapMaybe (getDirAliasedIxfn td_env coal_tab) [x]
   in Just (ws, ws ++ rs)
getUseSumFromStm td_env coal_tab (Let (Pat ys) _ (BasicOp (Replicate _shp se))) =
  let ws = mapMaybe (getDirAliasedIxfn td_env coal_tab . patElemName) ys
   in case se of
        Constant _ -> Just (ws, ws)
        Var x -> Just (ws, ws ++ mapMaybe (getDirAliasedIxfn td_env coal_tab) [x])
getUseSumFromStm td_env coal_tab (Let (Pat [x]) _ (BasicOp (FlatUpdate _ (FlatSlice offset slc) v)))
  | Just (m_b, m_x, x_ixfn) <- getDirAliasedIxfn td_env coal_tab (patElemName x) = do
      let x_ixfn_slc =
            LMAD.flatSlice x_ixfn $ FlatSlice (pe64 offset) $ map (fmap pe64) slc
      let r1 = (m_b, m_x, x_ixfn_slc)
      case getDirAliasedIxfn td_env coal_tab v of
        Nothing -> Just ([r1], [r1])
        Just r2 -> Just ([r1], [r1, r2])
-- getUseSumFromStm td_env coal_tab (Let (Pat ys) _ (BasicOp bop)) =
--   let wrt = mapMaybe (getDirAliasedIxfn td_env coal_tab . patElemName) ys
--    in trace ("getUseBla: " <> show bop) $ pure (wrt, wrt)
getUseSumFromStm td_env coal_tab (Let (Pat ys) _ (BasicOp Iota {})) =
  let wrt = mapMaybe (getDirAliasedIxfn td_env coal_tab . patElemName) ys
   in pure (wrt, wrt)
getUseSumFromStm _ _ (Let Pat {} _ BasicOp {}) = Just ([], [])
getUseSumFromStm _ _ (Let Pat {} _ (Op (Alloc _ _))) = Just ([], [])
getUseSumFromStm _ _ _ =
  -- if-then-else, loops are supposed to be treated separately,
  -- calls are not supported, and Ops are not yet supported
  Nothing

-- | This function:
--     1. computes the written and read memory references for the current statement
--          (by calling @getUseSumFromStm@)
--     2. fails the entries in active coalesced table for which the write set
--          overlaps the uses of the destination (to that point)
recordMemRefUses ::
  (AliasableRep rep, Op rep ~ MemOp inner rep, HasMemBlock (Aliases rep)) =>
  TopdownEnv rep ->
  BotUpEnv ->
  Stm (Aliases rep) ->
  (CoalsTab, InhibitTab)
recordMemRefUses td_env bu_env stm =
  let active_tab = activeCoals bu_env
      inhibit_tab = inhibit bu_env
      active_etries = M.toList active_tab
   in case getUseSumFromStm td_env active_tab stm of
        Nothing ->
          M.toList active_tab
            & foldl
              ( \state (m_b, entry) ->
                  if not $ null $ patNames (stmPat stm) `intersect` M.keys (vartab entry)
                    then markFailedCoal state m_b
                    else state
              )
              (active_tab, inhibit_tab)
        Just use_sums ->
          let (mb_wrts, prev_uses, mb_lmads) =
                map (checkOverlapAndExpand use_sums active_tab) active_etries
                  & unzip3

              -- keep only the entries that do not overlap with the memory
              -- blocks defined in @pat@ or @inner_free_vars@.
              -- the others must be recorded in @inhibit_tab@ because
              -- they violate the 3rd safety condition.
              active_tab1 =
                M.fromList
                  $ map
                    ( \(wrts, (uses, prev_use, (k, etry))) ->
                        let mrefs' = (memrefs etry) {dstrefs = prev_use}
                            etry' = etry {memrefs = mrefs'}
                         in (k, addLmads wrts uses etry')
                    )
                  $ mapMaybe (\(x, y) -> (,y) <$> x) -- only keep successful coals
                  $ zip mb_wrts
                  $ zip3 mb_lmads prev_uses active_etries
              failed_tab =
                M.fromList $
                  map snd $
                    filter (isNothing . fst) $
                      zip mb_wrts active_etries
              (_, inhibit_tab1) = foldl markFailedCoal (failed_tab, inhibit_tab) $ M.keys failed_tab
           in (active_tab1, inhibit_tab1)
  where
    checkOverlapAndExpand (stm_wrts, stm_uses) active_tab (m_b, etry) =
      let alias_m_b = getAliases mempty m_b
          stm_uses' = filter ((`notNameIn` alias_m_b) . tupFst) stm_uses
          all_aliases = foldl getAliases mempty $ namesToList $ alsmem etry
          ixfns = map tupThd $ filter ((`nameIn` all_aliases) . tupSnd) stm_uses'
          lmads' = mapMaybe mbLmad ixfns
          lmads'' =
            if length lmads' == length ixfns
              then Set $ S.fromList lmads'
              else Undeterminable
          wrt_ixfns = map tupThd $ filter ((`nameIn` alias_m_b) . tupFst) stm_wrts
          wrt_tmps = mapMaybe mbLmad wrt_ixfns
          prev_use =
            translateAccessSummary (scope td_env) (scalarTable td_env) $
              (dstrefs . memrefs) etry
          wrt_lmads' =
            if length wrt_tmps == length wrt_ixfns
              then Set $ S.fromList wrt_tmps
              else Undeterminable
          original_mem_aliases =
            fmap tupFst stm_uses
              & uncons
              & fmap fst
              & (=<<) (`M.lookup` active_tab)
              & maybe mempty alsmem
          (wrt_lmads'', lmads) =
            if m_b `nameIn` original_mem_aliases
              then (wrt_lmads' <> lmads'', Set mempty)
              else (wrt_lmads', lmads'')
          no_overlap = noMemOverlap td_env (lmads <> prev_use) wrt_lmads''
          wrt_lmads =
            if no_overlap
              then Just wrt_lmads''
              else Nothing
       in (wrt_lmads, prev_use, lmads)

    tupFst (a, _, _) = a
    tupSnd (_, b, _) = b
    tupThd (_, _, c) = c
    getAliases acc m =
      oneName m
        <> acc
        <> fromMaybe mempty (M.lookup m (m_alias td_env))
    mbLmad indfun
      | Just subs <- freeVarSubstitutions (scope td_env) (scals bu_env) indfun =
          Just $ LMAD.substitute subs indfun
    mbLmad _ = Nothing
    addLmads wrts uses etry =
      etry {memrefs = MemRefs uses wrts <> memrefs etry}

-- | Check for memory overlap of two access summaries.
--
-- This check is conservative, so unless we can guarantee that there is no
-- overlap, we return 'False'.
noMemOverlap :: (AliasableRep rep) => TopdownEnv rep -> AccessSummary -> AccessSummary -> Bool
noMemOverlap _ _ (Set mr)
  | mr == mempty = True
noMemOverlap _ (Set mr) _
  | mr == mempty = True
noMemOverlap td_env (Set is0) (Set js0)
  | Just non_negs <- mapM (primExpFromSubExpM (vnameToPrimExp (scope td_env) (scalarTable td_env)) . Var) $ namesToList $ nonNegatives td_env =
      let (_, not_disjoints) =
            partition
              ( \i ->
                  all
                    ( \j ->
                        LMAD.disjoint less_thans (nonNegatives td_env) i j
                          || LMAD.disjoint2 () () less_thans (nonNegatives td_env) i j
                          || LMAD.disjoint3 (typeOf <$> scope td_env) asserts less_thans non_negs i j
                    )
                    js
              )
              is
       in null not_disjoints
  where
    less_thans = map (fmap $ fixPoint $ substituteInPrimExp $ scalarTable td_env) $ knownLessThan td_env
    asserts = map (fixPoint (substituteInPrimExp $ scalarTable td_env) . primExpFromSubExp Bool) $ td_asserts td_env
    is = map (fixPoint (LMAD.substitute $ TPrimExp <$> scalarTable td_env)) $ S.toList is0
    js = map (fixPoint (LMAD.substitute $ TPrimExp <$> scalarTable td_env)) $ S.toList js0
noMemOverlap _ _ _ = False

-- | Computes the total aggregated access summary for a loop by expanding the
-- access summary given according to the iterator variable and bounds of the
-- loop.
--
-- Corresponds to:
--
-- \[
--   \bigcup_{j=0}^{j<n} Access_j
-- \]
aggSummaryLoopTotal ::
  (MonadFreshNames m) =>
  ScopeTab rep ->
  ScopeTab rep ->
  ScalarTab ->
  Maybe (VName, (TPrimExp Int64 VName, TPrimExp Int64 VName)) ->
  AccessSummary ->
  m AccessSummary
aggSummaryLoopTotal _ _ _ _ Undeterminable = pure Undeterminable
aggSummaryLoopTotal _ _ _ _ (Set l)
  | l == mempty = pure $ Set mempty
aggSummaryLoopTotal scope_bef scope_loop scals_loop _ access
  | Set ls <- translateAccessSummary scope_loop scals_loop access,
    nms <- foldl (<>) mempty $ map freeIn $ S.toList ls,
    all inBeforeScope $ namesToList nms = do
      pure $ Set ls
  where
    inBeforeScope v =
      case M.lookup v scope_bef of
        Nothing -> False
        Just _ -> True
aggSummaryLoopTotal _ _ scalars_loop (Just (iterator_var, (lower_bound, upper_bound))) (Set lmads) =
  concatMapM
    ( aggSummaryOne iterator_var lower_bound upper_bound
        . fixPoint (LMAD.substitute $ fmap TPrimExp scalars_loop)
    )
    (S.toList lmads)
aggSummaryLoopTotal _ _ _ _ _ = pure Undeterminable

-- | For a given iteration of the loop $i$, computes the aggregated loop access
-- summary of all later iterations.
--
-- Corresponds to:
--
-- \[
--   \bigcup_{j=i+1}^{j<n} Access_j
-- \]
aggSummaryLoopPartial ::
  (MonadFreshNames m) =>
  ScalarTab ->
  Maybe (VName, (TPrimExp Int64 VName, TPrimExp Int64 VName)) ->
  AccessSummary ->
  m AccessSummary
aggSummaryLoopPartial _ _ Undeterminable = pure Undeterminable
aggSummaryLoopPartial _ Nothing _ = pure Undeterminable
aggSummaryLoopPartial scalars_loop (Just (iterator_var, (_, upper_bound))) (Set lmads) = do
  -- map over each index function in the access summary
  --   Substitube a fresh variable k for the loop iterator
  --   if k is in stride or span of ixfun: fall back to total
  --   new_stride = old_offset - old_offset (where k+1 is substituted for k)
  --   new_offset = old_offset where k = lower bound of iteration
  --   new_span = upper bound of iteration
  concatMapM
    ( aggSummaryOne
        iterator_var
        (isInt64 (LeafExp iterator_var $ IntType Int64) + 1)
        (upper_bound - typedLeafExp iterator_var - 1)
        . fixPoint (LMAD.substitute $ fmap TPrimExp scalars_loop)
    )
    (S.toList lmads)

-- | For a given map with $k$ dimensions and an index $i$ for each dimension,
-- compute the aggregated access summary of all other threads.
--
-- For the innermost dimension, this corresponds to
--
-- \[
--   \bigcup_{j=0}^{j<i} Access_j \cup \bigcup_{j=i+1}^{j<n} Access_j
-- \]
--
-- where $Access_j$ describes the point accesses in the map. As we move up in
-- dimensionality, the previous access summaries are kept, in addition to the
-- total aggregation of the inner dimensions. For outer dimensions, the equation
-- is the same, the point accesses in $Access_j$ are replaced with the total
-- aggregation of the inner dimensions.
aggSummaryMapPartial :: (MonadFreshNames m) => ScalarTab -> [(VName, SubExp)] -> LmadRef -> m AccessSummary
aggSummaryMapPartial _ [] = const $ pure mempty
aggSummaryMapPartial scalars dims =
  helper mempty (reverse dims) . Set . S.singleton -- Reverse dims so we work from the inside out
  where
    helper acc [] _ = pure acc
    helper Undeterminable _ _ = pure Undeterminable
    helper _ _ Undeterminable = pure Undeterminable
    helper (Set acc) ((gtid, size) : rest) (Set as) = do
      partial_as <- aggSummaryMapPartialOne scalars (gtid, size) (Set as)
      total_as <-
        concatMapM
          (aggSummaryOne gtid 0 (TPrimExp $ primExpFromSubExp (IntType Int64) size))
          (S.toList as)
      helper (Set acc <> partial_as) rest total_as

-- | Given an access summary $a$, a thread id $i$ and the size $n$ of the
-- dimension, compute the partial map summary.
--
-- Corresponds to
--
-- \[
--   \bigcup_{j=0}^{j<i} a_j \cup \bigcup_{j=i+1}^{j<n} a_j
-- \]
aggSummaryMapPartialOne :: (MonadFreshNames m) => ScalarTab -> (VName, SubExp) -> AccessSummary -> m AccessSummary
aggSummaryMapPartialOne _ _ Undeterminable = pure Undeterminable
aggSummaryMapPartialOne _ (_, Constant n) (Set _) | oneIsh n = pure mempty
aggSummaryMapPartialOne scalars (gtid, size) (Set lmads0) =
  concatMapM
    helper
    [ (0, isInt64 (LeafExp gtid $ IntType Int64)),
      ( isInt64 (LeafExp gtid $ IntType Int64) + 1,
        isInt64 (primExpFromSubExp (IntType Int64) size) - isInt64 (LeafExp gtid $ IntType Int64) - 1
      )
    ]
  where
    lmads = map (fixPoint (LMAD.substitute $ fmap TPrimExp scalars)) $ S.toList lmads0
    helper (x, y) = concatMapM (aggSummaryOne gtid x y) lmads

-- | Computes to total access summary over a multi-dimensional map.
aggSummaryMapTotal :: (MonadFreshNames m) => ScalarTab -> [(VName, SubExp)] -> AccessSummary -> m AccessSummary
aggSummaryMapTotal _ [] _ = pure mempty
aggSummaryMapTotal _ _ (Set lmads)
  | lmads == mempty = pure mempty
aggSummaryMapTotal _ _ Undeterminable = pure Undeterminable
aggSummaryMapTotal scalars segspace (Set lmads0) =
  foldM
    ( \as' (gtid', size') -> case as' of
        Set lmads' ->
          concatMapM
            ( aggSummaryOne gtid' 0 $
                TPrimExp $
                  primExpFromSubExp (IntType Int64) size'
            )
            (S.toList lmads')
        Undeterminable -> pure Undeterminable
    )
    (Set lmads)
    (reverse segspace)
  where
    lmads =
      S.fromList $
        map (fixPoint (LMAD.substitute $ fmap TPrimExp scalars)) $
          S.toList lmads0

-- | Helper function that aggregates the accesses of single LMAD according to a
-- given iterator value, a lower bound and a span.
--
-- If successful, the result is an index function with an extra outer
-- dimension. The stride of the outer dimension is computed by taking the
-- difference between two points in the index function.
--
-- The function returns 'Underterminable' if the iterator is free in the output
-- LMAD or the dimensions of the input LMAD .
aggSummaryOne :: (MonadFreshNames m) => VName -> TPrimExp Int64 VName -> TPrimExp Int64 VName -> LmadRef -> m AccessSummary
aggSummaryOne iterator_var lower_bound spn lmad@(LMAD.LMAD offset0 dims0)
  | iterator_var `nameIn` freeIn dims0 = pure Undeterminable
  | iterator_var `notNameIn` freeIn offset0 = pure $ Set $ S.singleton lmad
  | otherwise = do
      new_var <- newVName "k"
      let offset = replaceIteratorWith (typedLeafExp new_var) offset0
          offsetp1 = replaceIteratorWith (typedLeafExp new_var + 1) offset0
          new_stride = TPrimExp $ constFoldPrimExp $ simplify $ untyped $ offsetp1 - offset
          new_offset = replaceIteratorWith lower_bound offset0
          new_lmad =
            LMAD.LMAD new_offset $ LMAD.LMADDim new_stride spn : dims0
      if new_var `nameIn` freeIn new_lmad
        then pure Undeterminable
        else pure $ Set $ S.singleton new_lmad
  where
    replaceIteratorWith se = TPrimExp . substituteInPrimExp (M.singleton iterator_var $ untyped se) . untyped

-- | Takes a 'VName' and converts it into a 'TPrimExp' with type 'Int64'.
typedLeafExp :: VName -> TPrimExp Int64 VName
typedLeafExp vname = isInt64 $ LeafExp vname (IntType Int64)
