{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.Optimise.MemBlockCoalesce.MemRefAggreg
  ( recordMemRefUses,
    freeVarSubstitutions,
    aggSummaryLoopTotal,
    aggSummaryLoopPartial,
    noMemOverlap,
  )
where

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
--import Debug.Trace

import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Aliases
import qualified Futhark.IR.Mem.IxFun as IxFun
import qualified Futhark.IR.SeqMem as ExpMem
import Futhark.Optimise.MemBlockCoalesce.DataStructs
import Futhark.Optimise.MemBlockCoalesce.TopDownAn
import Prelude

-----------------------------------------------------
-- Some translations of Accesses and Ixfuns        --
-----------------------------------------------------

-- | Checks whether the index function can be translated at the
--     current program point and also returns the substitutions.
--     It comes down to answering the question: "can one perform
--     enough substitutions (from the bottom-up scalar table) until
--     all vars appearing in the index function are defined in the
--     current scope?"
freeVarSubstitutions ::
  FreeIn a =>
  ScopeTab ->
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

-- | Translates free variables in an access sum
translateAccess :: ScopeTab -> ScalarTab -> AccsSum -> AccsSum
translateAccess _ _ Top = Top
translateAccess scope0 scals0 (Over slmads)
  | Just subs <- freeVarSubstitutions scope0 scals0 slmads =
    slmads
      & S.map (IxFun.substituteInLMAD subs)
      & Over
translateAccess _ _ _ = Top

-- | This function computes the written and read memory references for the current statement
getUseSumFromStm ::
  TopDnEnv ->
  CoalsTab ->
  Stm (Aliases ExpMem.SeqMem) ->
  ([(VName, VName, ExpMem.IxFun)], [(VName, VName, ExpMem.IxFun)])
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
-- y = copy x
getUseSumFromStm td_env coal_tab (Let (Pat [y]) _ (BasicOp (Copy x))) =
  let wrt = getDirAliasedIxfn td_env coal_tab $ patElemName y
      rd = getDirAliasedIxfn td_env coal_tab x
   in case (wrt, rd) of
        (Just w, Just r) -> ([w], [w, r])
        _ -> error $ "Impossible, " ++ pretty x ++ " should be an array"
getUseSumFromStm _ _ (Let Pat {} _ (BasicOp Copy {})) = error "Impossible"
-- concat
getUseSumFromStm td_env coal_tab (Let (Pat ys) _ (BasicOp (Concat _i a bs _ses))) =
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
-- UnAcc, UpdateAcc are not supported
--getUseSumFromStm td_env coal_tab (BasicOp (UnAcc{})) =
--  error "UnAcc is not supported yet!"
--getUseSumFromStm td_env coal_tab (BasicOp (UpdateAcc{})) =
--  error "UpdateAcc is not supported yet!"
-- SubExp, Scratch, reshape, rearrange, rotate and scalar ops
--   do not require an array access.
getUseSumFromStm _ _ (Let Pat {} _ BasicOp {}) = ([], [])
getUseSumFromStm _ _ (Let Pat {} _ (Op (ExpMem.Alloc _ _))) = ([], [])
-- if-then-else, loops are supposed to be treated separately,
-- calls are not supported, and Ops are not yet supported
getUseSumFromStm _ _ stm =
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
  TopDnEnv ->
  BotUpEnv ->
  Stm (Aliases ExpMem.SeqMem) ->
  (CoalsTab, InhibitTab)
recordMemRefUses td_env bu_env stm =
  let active_tab = activeCoals bu_env
      inhibit_tab = inhibit bu_env
      (stm_wrts, stm_uses) = getUseSumFromStm td_env active_tab stm
      active_etries = M.toList active_tab

      (mb_wrts, prev_uses) =
        unzip $
          map
            ( \(m_b, etry) ->
                if null stm_wrts
                  then (Just $ Over mempty, dstrefs (memrefs etry))
                  else
                    let alias_m_b = getAliases mempty m_b
                        ixfns = map tupThd $ filter ((`nameIn` alias_m_b) . tupFst) stm_wrts
                        wrt_tmps = mapMaybe mbLmad ixfns
                        wrt_lmads =
                          if length wrt_tmps == length ixfns
                            then Over $ S.fromList wrt_tmps
                            else Top
                        prev_use =
                          translateAccess (scope td_env) (scals bu_env) $
                            (dstrefs . memrefs) etry
                        no_overlap = noMemOverlap td_env prev_use wrt_lmads
                     in if no_overlap
                          then (Just wrt_lmads, prev_use)
                          else --trace ("No overlap fails: "++pretty (m_b, dstmem etry, wrt_lmads)) $
                            (Nothing, prev_use)
            )
            active_etries

      mb_lmads =
        map
          ( \(m_b, etry) ->
              if null stm_uses
                then Over mempty
                else
                  let -- get rid of the read and write to m_b, those are not relevant
                      alias_m_b = getAliases mempty m_b
                      stm_uses' = filter (not . (`nameIn` alias_m_b) . tupFst) stm_uses
                      all_aliases = foldl getAliases mempty $ namesToList $ alsmem etry
                      ixfns = map tupThd $ filter ((`nameIn` all_aliases) . tupSnd) stm_uses'
                      lmads =
                        --trace ("MemRefUses for: "++pretty (m_b,dstmem etry)++" "++pretty stm_uses') $
                        mapMaybe mbLmad ixfns
                   in if length lmads == length ixfns
                        then Over (S.fromList lmads)
                        else Top
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
   in (active_tab1, inhibit_tab1)
  where
    tupFst (a, _, _) = a
    tupSnd (_, b, _) = b
    tupThd (_, _, c) = c
    okMemRef (Nothing, _) = False
    okMemRef (_, _) = True
    getAliases acc m =
      oneName m
        <> case M.lookup m (m_alias td_env) of
          Nothing -> acc
          Just nms -> nms <> acc
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
noMemOverlap :: TopDnEnv -> AccsSum -> AccsSum -> Bool
noMemOverlap _ _ (Over mr)
  | mr == mempty = True
noMemOverlap _ (Over mr) _
  | mr == mempty = True
noMemOverlap _ Top _ = False
noMemOverlap _ _ Top = False
noMemOverlap _ (Over _) (Over _) =
  False

-- TODO add non-trivial implementation, please

-- | Suppossed to aggregate the iteration-level summaries
--     across a loop of index i = 0 .. n-1
--   The current implementation is naive, in that it
--     treats the case when the summary is loop-invariant,
--     and returns Top in the other cases.
--   The current implementation is good for while, but needs
--     to be refined for @for i < n@ loops
aggSummaryLoopTotal ::
  ScopeTab ->
  ScopeTab ->
  ScalarTab ->
  Maybe (VName, (ExpMem.TPrimExp Int64 VName, ExpMem.TPrimExp Int64 VName)) ->
  AccsSum ->
  AccsSum
aggSummaryLoopTotal _ _ _ _ Top = Top
aggSummaryLoopTotal _ _ _ _ (Over l)
  | l == mempty = Over mempty
aggSummaryLoopTotal scope_bef scope_loop scals_loop _ access
  | Over ls <- translateAccess scope_loop scals_loop access,
    nms <- foldl (<>) mempty $ map freeIn $ S.toList ls,
    all inBeforeScope (namesToList nms) =
    Over ls
  where
    inBeforeScope v =
      case M.lookup v scope_bef of
        Nothing -> False
        Just _ -> True
aggSummaryLoopTotal _ _ _ _ _ = Top

-- | Suppossed to partially aggregate the iteration-level summaries
--     across a loop of index i = 0 .. n-1. This means that it
--     computes the summary: Union_{j=0..i-1} Access_j
--   The current implementation is naive in that it treats only
--     the loop invariant case (same as function @aggSummaryLoopTotal@).
aggSummaryLoopPartial ::
  ScopeTab ->
  ScopeTab ->
  ScalarTab ->
  Maybe (VName, (ExpMem.TPrimExp Int64 VName, ExpMem.TPrimExp Int64 VName)) ->
  AccsSum ->
  AccsSum
aggSummaryLoopPartial = aggSummaryLoopTotal
