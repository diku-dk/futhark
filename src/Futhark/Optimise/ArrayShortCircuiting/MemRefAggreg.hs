{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.Optimise.ArrayShortCircuiting.MemRefAggreg
  ( recordMemRefUses,
    freeVarSubstitutions,
    aggSummaryLoopTotal,
    aggSummaryLoopPartial,
    noMemOverlap,
  )
where

import Data.Foldable
import Data.Function ((&))
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace
import Futhark.Analysis.AlgSimplify2
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Aliases
import Futhark.IR.Mem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.IR.Prop.Names
import Futhark.MonadFreshNames
import Futhark.Optimise.ArrayShortCircuiting.DataStructs
import Futhark.Optimise.ArrayShortCircuiting.TopDownAn
import Futhark.Transform.Substitute
import Futhark.Util

traceWith s a = trace (s <> ": " <> pretty a) a

traceWith' s a = trace (s <> ": " <> show a) a

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
            _ -> Nothing
    getSubstitution v
      | Just pe <- M.lookup v scals0,
        IntType Int64 <- primExpType pe =
        Just (M.singleton v $ isInt64 pe, namesToList $ freeIn pe)
    getSubstitution _v = Nothing

-- | Translates free variables in an access summary
translateAccessSummary :: ScopeTab rep -> ScalarTab -> AccessSummary -> AccessSummary
translateAccessSummary _ _ Undeterminable = Undeterminable
translateAccessSummary scope0 scals0 (Set slmads)
  | Just subs <- freeVarSubstitutions scope0 scals0 slmads =
    slmads
      & S.map (IxFun.substituteInLMAD $ traceWith "freeVarSubs" subs)
      & Set
translateAccessSummary _ _ _ = Undeterminable

-- | This function computes the written and read memory references for the current statement
getUseSumFromStm ::
  (CanBeAliased inner, ASTRep rep, Op rep ~ MemOp inner, HasMemBlock (Aliases rep)) =>
  TopDnEnv rep ->
  CoalsTab ->
  Stm (Aliases rep) ->
  -- | A pair of written and written+read memory locations, along with their
  -- associated array and the index function used
  ([(VName, VName, IxFun)], [(VName, VName, IxFun)])
getUseSumFromStm td_env coal_tab (Let Pat {} _ (BasicOp (Index arr (Slice slc))))
  | Just (MemBlock _ shp _ _) <- getScopeMemInfo arr (scope td_env),
    length slc == length (shapeDims shp) && all isFix slc =
    case getDirAliasedIxfn td_env coal_tab arr of
      Nothing -> error "impossible"
      Just (mem_b, mem_arr, ixfn_arr) ->
        let new_ixfn = IxFun.slice ixfn_arr $ Slice $ map (fmap pe64) slc
         in ([], [(mem_b, mem_arr, new_ixfn)])
  where
    isFix DimFix {} = True
    isFix _ = False
getUseSumFromStm _ _ (Let Pat {} _ (BasicOp Index {})) = ([], []) -- incomplete slices
getUseSumFromStm td_env coal_tab (Let (Pat pes) _ (BasicOp (ArrayLit ses _))) =
  let rds = mapMaybe (getDirAliasedIxfn td_env coal_tab) $ mapMaybe seName ses
      wrts = mapMaybe (getDirAliasedIxfn td_env coal_tab . patElemName) pes
   in (wrts, wrts ++ rds)
  where
    seName (Var a) = Just a
    seName (Constant _) = Nothing
-- In place update @x[slc] <- a@. In the "in-place update" case,
--   summaries should be added after the old variable @x@ has
--   been added in the active coalesced table.
getUseSumFromStm td_env coal_tab (Let (Pat [x']) _ (BasicOp (Update _ _x (Slice slc) a_se)))
  | Just (m_b, m_x, x_ixfn) <- getDirAliasedIxfn td_env coal_tab (patElemName x') =
    let x_ixfn_slc = IxFun.slice x_ixfn $ Slice $ map (fmap pe64) slc
        r1 = (m_b, m_x, x_ixfn_slc)
     in case a_se of
          Constant _ -> ([r1], [r1])
          Var a -> case getDirAliasedIxfn td_env coal_tab a of
            Nothing -> ([r1], [r1])
            Just r2 -> ([r1], [r1, r2])
getUseSumFromStm _ _ (Let Pat {} _ (BasicOp Update {})) = error "impossible"
getUseSumFromStm td_env coal_tab (Let (Pat [y]) _ (BasicOp (Copy x))) =
  -- y = copy x
  let wrt = getDirAliasedIxfn td_env coal_tab $ patElemName y
      rd = getDirAliasedIxfn td_env coal_tab x
   in case (wrt, rd) of
        (Just w, Just r) -> ([w], [w, r])
        _ -> error $ "Impossible, " ++ pretty x ++ " should be an array"
getUseSumFromStm _ _ (Let Pat {} _ (BasicOp Copy {})) = error "Impossible"
getUseSumFromStm td_env coal_tab (Let (Pat ys) _ (BasicOp (Concat _i a bs _ses))) =
  -- concat
  let ws = mapMaybe (getDirAliasedIxfn td_env coal_tab . patElemName) ys
      rs = mapMaybe (getDirAliasedIxfn td_env coal_tab) (a : bs)
   in (ws, ws ++ rs)
getUseSumFromStm td_env coal_tab (Let (Pat ys) _ (BasicOp (Manifest _perm x))) =
  let ws = mapMaybe (getDirAliasedIxfn td_env coal_tab . patElemName) ys
      rs = mapMaybe (getDirAliasedIxfn td_env coal_tab) [x]
   in (ws, ws ++ rs)
getUseSumFromStm td_env coal_tab (Let (Pat ys) _ (BasicOp (Replicate _shp se))) =
  let ws = mapMaybe (getDirAliasedIxfn td_env coal_tab . patElemName) ys
   in case se of
        Constant _ -> (ws, ws)
        Var x -> (ws, ws ++ mapMaybe (getDirAliasedIxfn td_env coal_tab) [x])
getUseSumFromStm td_env coal_tab (Let (Pat [x]) _ (BasicOp (FlatUpdate _ (FlatSlice offset slc) v)))
  | Just (m_b, m_x, x_ixfn) <- getDirAliasedIxfn td_env coal_tab (patElemName x) =
    let x_ixfn_slc = IxFun.flatSlice x_ixfn $ FlatSlice (pe64 offset) $ map (fmap pe64) slc
        r1 = (m_b, m_x, x_ixfn_slc)
     in case getDirAliasedIxfn td_env coal_tab v of
          Nothing -> ([r1], [r1])
          Just r2 -> ([r1], [r1, r2])
getUseSumFromStm _ _ (Let Pat {} _ BasicOp {}) = ([], [])
getUseSumFromStm _ _ (Let Pat {} _ (Op (Alloc _ _))) = ([], [])
getUseSumFromStm _ _ stm =
  -- if-then-else, loops are supposed to be treated separately,
  -- calls are not supported, and Ops are not yet supported
  error ("In MemRefAggreg.hs, getUseSumFromStm, unsuported case of stm being: " ++ pretty stm)

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
  (CanBeAliased inner, ASTRep rep, Op rep ~ MemOp inner, HasMemBlock (Aliases rep)) =>
  TopDnEnv rep ->
  BotUpEnv ->
  Stm (Aliases rep) ->
  (CoalsTab, InhibitTab)
recordMemRefUses td_env bu_env stm =
  let active_tab = activeCoals bu_env
      inhibit_tab = inhibit bu_env
      (stm_wrts, stm_uses) = traceWith ("stm: " <> pretty stm <> "\nuseSumFromStm") $ getUseSumFromStm td_env active_tab stm
      active_etries = M.toList active_tab

      (mb_wrts, prev_uses, mb_lmads) =
        unzip3 $
          map
            ( \(m_b, etry) ->
                let alias_m_b = getAliases mempty m_b
                    stm_uses' = filter (not . (`nameIn` alias_m_b) . tupFst) stm_uses
                    all_aliases = foldl getAliases mempty $ namesToList $ alsmem etry
                    ixfns = map tupThd $ filter ((`nameIn` all_aliases) . tupSnd) stm_uses'
                    lmads' = mapMaybe mbLmad ixfns
                    lmads'' =
                      if length lmads' == length ixfns
                        then Set $ S.fromList $ lmads'
                        else Undeterminable

                    wrt_ixfns = map tupThd $ filter ((`nameIn` alias_m_b) . tupFst) stm_wrts
                    wrt_tmps = mapMaybe mbLmad wrt_ixfns
                    prev_use =
                      translateAccessSummary (scope td_env) (scals bu_env) $
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
                        & fmap alsmem
                        & fromMaybe mempty
                    (wrt_lmads'', lmads) =
                      if m_b `nameIn` original_mem_aliases
                        then (wrt_lmads' <> lmads'', Set mempty)
                        else (wrt_lmads', lmads'')
                    no_overlap = noMemOverlap td_env prev_use wrt_lmads''
                    wrt_lmads =
                      if no_overlap
                        then Just wrt_lmads''
                        else Nothing
                 in trace
                      ( "Philip m_b: " <> pretty m_b
                          <> "\nalias_m_b: "
                          <> pretty alias_m_b
                          <> "\ndestmem entry: "
                          <> pretty (dstmem etry)
                          <> "\nstm_uses': "
                          <> pretty stm_uses'
                          <> "\norig_use: "
                          <> pretty original_mem_aliases
                          <> "\nall_aliases: "
                          <> pretty all_aliases
                          <> "\nstatement: "
                          <> pretty stm
                          <> "\nixfns: "
                          <> pretty ixfns
                          <> "\nwrt_lmads: "
                          <> pretty wrt_lmads
                          <> "\nprev_use: "
                          <> pretty prev_use
                          <> "\nlmads: "
                          <> pretty lmads
                      )
                      (wrt_lmads, prev_use, lmads)
            )
            active_etries

      -- keep only the entries that do not overlap with the memory
      -- blocks defined in @pat@ or @inner_free_vars@.
      -- the others must be recorded in @inhibit_tab@ because
      -- they violate the 3rd safety condition.
      active_tab1 =
        M.fromList $
          map
            ( \(wrts, (uses, prev_use, (k, etry))) ->
                let mrefs' = (memrefs etry) {dstrefs = prev_use}
                    etry' = etry {memrefs = mrefs'}
                 in (k, addLmads wrts uses etry')
            )
            $ filter okMemRef $
              zip mb_wrts $ zip3 mb_lmads prev_uses active_etries
      failed_tab =
        M.fromList $
          map snd $
            filter (not . okMemRef) $
              zip mb_wrts active_etries
      (_, inhibit_tab1) = foldl markFailedCoal (failed_tab, inhibit_tab) (M.keys failed_tab)
   in trace "END OF RECORDMEMREFUSES" (active_tab1, inhibit_tab1)
  where
    tupFst (a, _, _) = a
    tupSnd (_, b, _) = b
    tupThd (_, _, c) = c
    okMemRef (Nothing, _) = False
    okMemRef (_, _) = True
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
noMemOverlap :: TopDnEnv rep -> AccessSummary -> AccessSummary -> Bool
noMemOverlap _ _ (Set mr)
  | mr == mempty = traceWith "empty 1" True
noMemOverlap _ (Set mr) _
  | mr == mempty = traceWith "empty 2" True
-- noMemOverlap _ (Set ml) (Set mr)
--   | mr == ml = False || False
noMemOverlap _ Undeterminable _ = False
noMemOverlap _ _ Undeterminable = False
noMemOverlap _ (Set is) (Set js) =
  -- TODO Expand this to be able to handle eg. nw
  traceWith "all" $ all (\i -> all (IxFun.disjoint i) $ traceWith "js" $ S.toList js) $ traceWith "is" $ S.toList is

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
aggSummaryLoopTotal _ _ _ _ Undeterminable = return Undeterminable
aggSummaryLoopTotal _ _ _ _ (Set l)
  | l == mempty = return $ Set mempty
aggSummaryLoopTotal scope_bef scope_loop scals_loop _ access
  | Set ls <- traceWith "translateAccesSummary" $ translateAccessSummary scope_loop scals_loop $ traceWith "access" access,
    nms <- foldl (<>) mempty $ map freeIn $ S.toList ls,
    traceWith "all" $ all inBeforeScope $ traceWith "names" $ namesToList nms = do
    return $ Set ls
  where
    inBeforeScope v =
      case M.lookup v scope_bef of
        Nothing -> False
        Just _ -> True
aggSummaryLoopTotal scope_before scope_loop scalars_loop (Just (iterator_var, (lower_bound, upper_bound))) (Set lmads) =
  mconcat
    <$> mapM
      (aggSummaryOne (traceWith ("aggSummaaryLoopTotal.\nlower_bound: " <> pretty lower_bound <> "\nupper_bound: " <> pretty upper_bound <> "\niterator_var") iterator_var) lower_bound upper_bound)
      (S.toList lmads)
aggSummaryLoopTotal _ _ _ _ _ = return Undeterminable

-- | Suppossed to partially aggregate the iteration-level summaries
--     across a loop of index i = 0 .. n-1. This means that it
--     computes the summary: Union_{j=i+1..n} Access_j
--   The current implementation is naive in that it treats only
--     the loop invariant case (same as function @aggSummaryLoopTotal@).
aggSummaryLoopPartial ::
  MonadFreshNames m =>
  ScopeTab rep ->
  ScopeTab rep ->
  ScalarTab ->
  Maybe (VName, (TPrimExp Int64 VName, TPrimExp Int64 VName)) ->
  AccessSummary ->
  m AccessSummary
aggSummaryLoopPartial _ _ _ _ Undeterminable = return Undeterminable
aggSummaryLoopPartial _ _ _ Nothing _ = return Undeterminable
aggSummaryLoopPartial scope_before scope_loop scalars_loop (Just (iterator_var, (lower_bound, upper_bound))) (Set lmads) = do
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
      )
      (map (fixPoint $ IxFun.substituteInLMAD $ fmap TPrimExp scalars_loop) $ S.toList lmads)

aggSummaryOne :: MonadFreshNames m => VName -> TPrimExp Int64 VName -> TPrimExp Int64 VName -> LmadRef -> m AccessSummary
aggSummaryOne iterator_var lower_bound upper_bound (IxFun.LMAD _ dims) | iterator_var `nameIn` freeIn dims = return Undeterminable
aggSummaryOne iterator_var lower_bound span lmad@(IxFun.LMAD offset0 dims0) = do
  new_var <- newVName "k"
  let offset = traceWith "offset" $ replaceIteratorWith (typedLeafExp new_var) offset0
      offsetp1 = traceWith "offsetp1" $ replaceIteratorWith (typedLeafExp new_var + 1) offset0
      new_stride = traceWith "new_stride" $ TPrimExp $ constFoldPrimExp $ simplify $ untyped $ offsetp1 - offset
      new_offset = replaceIteratorWith lower_bound offset0
      new_lmad =
        traceWith ("old_lmad: " <> pretty lmad <> "\nnew_lmad") $
          IxFun.LMAD new_offset $
            IxFun.LMADDim new_stride 0 span 0 IxFun.Inc : map incPerm dims0
  if new_var `nameIn` freeIn new_lmad
    then return Undeterminable
    else return $ Set $ S.singleton new_lmad
  where
    incPerm dim = dim {IxFun.ldPerm = IxFun.ldPerm dim + 1}
    replaceIteratorWith se = TPrimExp . substituteInPrimExp (M.singleton iterator_var $ untyped se) . untyped

typedLeafExp :: VName -> TPrimExp Int64 VName
typedLeafExp vname = isInt64 $ LeafExp vname (IntType Int64)
