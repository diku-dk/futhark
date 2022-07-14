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
import Control.Monad.IO.Class
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intersect, uncons)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Futhark.Analysis.AlgSimplify2
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Aliases
import Futhark.IR.Mem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.MonadFreshNames
import Futhark.Optimise.ArrayShortCircuiting.DataStructs
import Futhark.Optimise.ArrayShortCircuiting.TopDownAn
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
  FreeIn a =>
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
       in case unzip <$> mapM getSubstitution fvs_not_in_scope of
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
        & S.map (IxFun.substituteInLMAD subs)
        & Set
translateAccessSummary _ _ _ = Undeterminable

-- | This function computes the written and read memory references for the current statement
getUseSumFromStm ::
  (Op rep ~ MemOp inner, HasMemBlock (Aliases rep)) =>
  TopDnEnv rep ->
  CoalsTab ->
  Stm (Aliases rep) ->
  -- | A pair of written and written+read memory locations, along with their
  -- associated array and the index function used
  Maybe ([(VName, VName, IxFun)], [(VName, VName, IxFun)])
getUseSumFromStm td_env coal_tab (Let _ _ (BasicOp (Index arr (Slice slc))))
  | Just (MemBlock _ shp _ _) <- getScopeMemInfo arr (scope td_env),
    length slc == length (shapeDims shp) && all isFix slc = do
      (mem_b, mem_arr, ixfn_arr) <- getDirAliasedIxfn td_env coal_tab arr
      let new_ixfn = IxFun.slice ixfn_arr $ Slice $ map (fmap pe64) slc
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
  let x_ixfn_slc = IxFun.slice x_ixfn $ Slice $ map (fmap pe64) slc
      r1 = (m_b, m_x, x_ixfn_slc)
  case a_se of
    Constant _ -> Just ([r1], [r1])
    Var a -> case getDirAliasedIxfn td_env coal_tab a of
      Nothing -> Just ([r1], [r1])
      Just r2 -> Just ([r1], [r1, r2])
getUseSumFromStm td_env coal_tab (Let (Pat [y]) _ (BasicOp (Copy x))) = do
  -- y = copy x
  wrt <- getDirAliasedIxfn td_env coal_tab $ patElemName y
  rd <- getDirAliasedIxfn td_env coal_tab x
  pure ([wrt], [wrt, rd])
getUseSumFromStm _ _ (Let Pat {} _ (BasicOp Copy {})) = error "Impossible"
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
  | Just (m_b, m_x, x_ixfn) <- getDirAliasedIxfn td_env coal_tab (patElemName x) =
      let x_ixfn_slc = IxFun.flatSlice x_ixfn $ FlatSlice (pe64 offset) $ map (fmap pe64) slc
          r1 = (m_b, m_x, x_ixfn_slc)
       in case getDirAliasedIxfn td_env coal_tab v of
            Nothing -> Just ([r1], [r1])
            Just r2 -> Just ([r1], [r1, r2])
getUseSumFromStm _ _ (Let Pat {} _ BasicOp {}) = Just ([], [])
getUseSumFromStm _ _ (Let Pat {} _ (Op (Alloc _ _))) = Just ([], [])
getUseSumFromStm _ _ _ =
  -- if-then-else, loops are supposed to be treated separately,
  -- calls are not supported, and Ops are not yet supported
  -- error
  --   ("In MemRefAggreg.hs, getUseSumFromStm, unsuported case of stm being: " ++ pretty stm)
  Nothing

-- | This function:
--     1. computes the written and read memory references for the current statement
--          (by calling @getUseSumFromStm@)
--     2. fails the entries in active coalesced table for which the write set
--          overlaps the uses of the destination (to that point)
--   ToDo: a) the @noMemOverlap@ test probably requires the @scals@ field
--            from @BotUpEnv@ so that it translates the index functions.
--         b) it will also require a dictionary of ranges, e.g., for loop
--            indices, and positive constants, such as loop counts. This
--            should be computed on the top-down pass.
recordMemRefUses ::
  (CanBeAliased (Op rep), RepTypes rep, Op rep ~ MemOp inner, HasMemBlock (Aliases rep)) =>
  TopDnEnv rep ->
  BotUpEnv ->
  Stm (Aliases rep) ->
  IO (CoalsTab, InhibitTab)
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
            & pure
        Just (stm_wrts, stm_uses) -> do
          (mb_wrts, prev_uses, mb_lmads) <-
            fmap unzip3 $
              liftIO $
                mapM
                  ( \(m_b, etry) -> do
                      let alias_m_b = getAliases mempty m_b
                          stm_uses' = filter (not . (`nameIn` alias_m_b) . tupFst) stm_uses
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
                      no_overlap <- noMemOverlap td_env prev_use wrt_lmads''
                      let wrt_lmads =
                            if no_overlap
                              then Just wrt_lmads''
                              else Nothing
                      pure (wrt_lmads, prev_use, lmads)
                  )
                  active_etries

          -- keep only the entries that do not overlap with the memory
          -- blocks defined in @pat@ or @inner_free_vars@.
          -- the others must be recorded in @inhibit_tab@ because
          -- they violate the 3rd safety condition.
          let active_tab1 =
                M.fromList
                  $ map
                    ( \(wrts, (uses, prev_use, (k, etry))) ->
                        let mrefs' = (memrefs etry) {dstrefs = prev_use}
                            etry' = etry {memrefs = mrefs'}
                         in (k, addLmads wrts uses etry')
                    )
                  $ filter (isJust . fst)
                  $ zip mb_wrts
                  $ zip3 mb_lmads prev_uses active_etries
              failed_tab =
                M.fromList $
                  map snd $
                    filter (isNothing . fst) $
                      zip mb_wrts active_etries
              (_, inhibit_tab1) = foldl markFailedCoal (failed_tab, inhibit_tab) $ M.keys failed_tab
          pure (active_tab1, inhibit_tab1)
  where
    tupFst (a, _, _) = a
    tupSnd (_, b, _) = b
    tupThd (_, _, c) = c
    getAliases acc m =
      oneName m
        <> acc
        <> fromMaybe mempty (M.lookup m (m_alias td_env))
    mbLmad indfun
      | Just subs <- freeVarSubstitutions (scope td_env) (scals bu_env) indfun,
        (IxFun.IxFun (lmad :| []) _ _) <- IxFun.substituteInIxFun subs indfun =
          Just lmad
    mbLmad _ = Nothing
    addLmads (Just wrts) uses etry =
      etry {memrefs = MemRefs uses wrts <> memrefs etry}
    addLmads _ _ _ =
      error "Impossible case reached because we have filtered Nothings before"

-------------------------------------------------------------
-- Helper Functions for Partial and Total Loop Aggregation --
--  of memory references. Please implement as precise as   --
--  possible. Currently the implementations are dummy      --
--  aiming to be useful only when one of the sets is empty.--
-------------------------------------------------------------
noMemOverlap :: (CanBeAliased (Op rep), RepTypes rep) => TopDnEnv rep -> AccessSummary -> AccessSummary -> IO Bool
noMemOverlap _ _ (Set mr)
  | mr == mempty = pure True
noMemOverlap _ (Set mr) _
  | mr == mempty = pure True
noMemOverlap td_env (Set is0) (Set js0)
  | Just non_negs <- mapM (primExpFromSubExpM (basePMconv (scope td_env) (scalarTable td_env)) . Var) $ namesToList $ nonNegatives td_env = do
      (_, not_disjoints) <-
        partitionM
          ( \i ->
              allM
                ( \j ->
                    pure (IxFun.disjoint less_thans (nonNegatives td_env) i j)
                      ||^ pure (IxFun.disjoint2 less_thans (nonNegatives td_env) i j)
                      ||^ IxFun.disjoint3 (typeOf <$> scope td_env) asserts less_thans non_negs i j
                )
                js
          )
          is
      pure $ null not_disjoints
  where
    less_thans = map (fmap $ fixPoint $ substituteInPrimExp $ scalarTable td_env) $ knownLessThan td_env
    asserts = map (fixPoint (substituteInPrimExp $ scalarTable td_env) . primExpFromSubExp Bool) $ td_asserts td_env
    is = map (fixPoint (IxFun.substituteInLMAD $ TPrimExp <$> scalarTable td_env)) $ S.toList is0
    js = map (fixPoint (IxFun.substituteInLMAD $ TPrimExp <$> scalarTable td_env)) $ S.toList js0
noMemOverlap _ _ _ = pure False

-- | Suppossed to aggregate the iteration-level summaries
--     across a loop of index i = 0 .. n-1
--   The current implementation is naive, in that it
--     treats the case when the summary is loop-invariant,
--     and returns Undeterminable in the other cases.
--   The current implementation is good for while, but needs
--     to be refined for @for i < n@ loops
aggSummaryLoopTotal ::
  MonadFreshNames m =>
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
  mconcat
    <$> mapM
      ( aggSummaryOne iterator_var lower_bound upper_bound
          . fixPoint (IxFun.substituteInLMAD $ fmap TPrimExp scalars_loop)
      )
      (S.toList lmads)
aggSummaryLoopTotal _ _ _ _ _ = pure Undeterminable

-- | Partially aggregate the iteration-level summaries
--     across a loop of index i = 0 .. n-1. This means that it
--     computes the summary: Union_{j=i+1..n} Access_j
aggSummaryLoopPartial ::
  MonadFreshNames m =>
  ScopeTab rep ->
  ScopeTab rep ->
  ScalarTab ->
  Maybe (VName, (TPrimExp Int64 VName, TPrimExp Int64 VName)) ->
  AccessSummary ->
  m AccessSummary
aggSummaryLoopPartial _ _ _ _ Undeterminable = pure Undeterminable
aggSummaryLoopPartial _ _ _ Nothing _ = pure Undeterminable
aggSummaryLoopPartial _ _ scalars_loop (Just (iterator_var, (_, upper_bound))) (Set lmads) = do
  -- map over each index function in the access summary
  --   Substitube a fresh variable k for the loop iterator
  --   if k is in stride or span of ixfun: fall back to total
  --   new_stride = old_offset - old_offset (where k+1 is substituted for k)
  --   new_offset = old_offset where k = lower bound of iteration
  --   new_span = upper bound of iteration
  mconcat
    <$> mapM
      ( aggSummaryOne
          iterator_var
          (isInt64 (LeafExp iterator_var $ IntType Int64) + 1)
          (upper_bound - typedLeafExp iterator_var - 1)
          . fixPoint (IxFun.substituteInLMAD $ fmap TPrimExp scalars_loop)
      )
      (S.toList lmads)

aggSummaryMapPartial :: MonadFreshNames m => ScalarTab -> [(VName, SubExp)] -> AccessSummary -> m AccessSummary
aggSummaryMapPartial _ [] _ = pure mempty
aggSummaryMapPartial _ _ (Set lmads)
  | lmads == mempty = pure mempty
aggSummaryMapPartial scalars ((gtid, size) : rest) as = do
  total_inner <-
    foldM
      ( \as' (gtid', size') -> case as' of
          Set lmads ->
            mconcat
              <$> mapM
                ( aggSummaryOne gtid' 0 $
                    TPrimExp $
                      primExpFromSubExp (IntType Int64) size'
                )
                (S.toList lmads)
          Undeterminable -> pure Undeterminable
      )
      as
      $ reverse rest
  this_dim <- aggSummaryMapPartialOne scalars (gtid, size) total_inner
  rest' <- aggSummaryMapPartial scalars rest as
  pure $ this_dim <> rest'

aggSummaryMapPartialOne :: MonadFreshNames m => ScalarTab -> (VName, SubExp) -> AccessSummary -> m AccessSummary
aggSummaryMapPartialOne _ _ Undeterminable = pure Undeterminable
aggSummaryMapPartialOne _ (_, Constant n) (Set _) | oneIsh n = pure mempty
aggSummaryMapPartialOne scalars (gtid, size) (Set lmads0) =
  mapM
    helper
    [ (0, isInt64 (LeafExp gtid $ IntType Int64)),
      ( isInt64 (LeafExp gtid $ IntType Int64) + 1,
        isInt64 (primExpFromSubExp (IntType Int64) size) - isInt64 (LeafExp gtid $ IntType Int64) - 1
      )
    ]
    <&> mconcat
  where
    lmads = map (fixPoint (IxFun.substituteInLMAD $ fmap TPrimExp scalars)) $ S.toList lmads0
    helper (x, y) = mconcat <$> mapM (aggSummaryOne gtid x y) lmads

aggSummaryMapTotal :: MonadFreshNames m => ScalarTab -> [(VName, SubExp)] -> AccessSummary -> m AccessSummary
aggSummaryMapTotal _ [] _ = pure mempty
aggSummaryMapTotal _ _ (Set lmads)
  | lmads == mempty = pure mempty
aggSummaryMapTotal _ _ Undeterminable = pure Undeterminable
aggSummaryMapTotal scalars segspace (Set lmads0) =
  foldM
    ( \as' (gtid', size') -> case as' of
        Set lmads' ->
          mconcat
            <$> mapM
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
        map (fixPoint (IxFun.substituteInLMAD $ fmap TPrimExp scalars)) $
          S.toList lmads0

aggSummaryOne :: MonadFreshNames m => VName -> TPrimExp Int64 VName -> TPrimExp Int64 VName -> LmadRef -> m AccessSummary
aggSummaryOne iterator_var lower_bound spn lmad@(IxFun.LMAD offset0 dims0)
  | iterator_var `nameIn` freeIn dims0 = pure Undeterminable
  | not $ iterator_var `nameIn` freeIn offset0 = pure $ Set $ S.singleton lmad
  | otherwise = do
      new_var <- newVName "k"
      let offset = replaceIteratorWith (typedLeafExp new_var) offset0
          offsetp1 = replaceIteratorWith (typedLeafExp new_var + 1) offset0
          new_stride = TPrimExp $ constFoldPrimExp $ simplify $ untyped $ offsetp1 - offset
          new_offset = replaceIteratorWith lower_bound offset0
          new_lmad =
            IxFun.LMAD new_offset $
              IxFun.LMADDim new_stride 0 spn 0 IxFun.Inc : map incPerm dims0
      if new_var `nameIn` freeIn new_lmad
        then pure Undeterminable
        else pure $ Set $ S.singleton new_lmad
  where
    incPerm dim = dim {IxFun.ldPerm = IxFun.ldPerm dim + 1}
    replaceIteratorWith se = TPrimExp . substituteInPrimExp (M.singleton iterator_var $ untyped se) . untyped

typedLeafExp :: VName -> TPrimExp Int64 VName
typedLeafExp vname = isInt64 $ LeafExp vname (IntType Int64)
