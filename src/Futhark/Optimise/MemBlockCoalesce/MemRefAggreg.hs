{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Futhark.Optimise.MemBlockCoalesce.MemRefAggreg
       ( recordMemRefUses, markFailedCoal, translateIndFunFreeVar )
       where


import Prelude
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Control.Exception.Base as Exc
import qualified Data.List.NonEmpty as NE

--import Debug.Trace

import Futhark.IR.Aliases
import Futhark.Analysis.PrimExp.Convert
import qualified Futhark.IR.SeqMem as ExpMem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.Optimise.MemBlockCoalesce.DataStructs
import Futhark.Optimise.MemBlockCoalesce.TopDownAn

-- | This function computes the written and read memory references for the current statement
getUseSumFromStm :: TopDnEnv -> CoalsTab
                 -> Stm (Aliases ExpMem.SeqMem)
                 -> ([(VName, VName, ExpMem.IxFun)],[(VName, VName, ExpMem.IxFun)])
getUseSumFromStm td_env coal_tab (Let (Pattern{}) _ (BasicOp (Index arr slc)))
  | Just (MemBlock _ shp _ _) <- getScopeMemInfo arr (scope td_env),
    length slc == length (shapeDims shp) && all isFix slc =
      case getDirAliasedIxfn td_env coal_tab arr of
        Nothing -> ([],[])
        Just (_, mem_arr, ixfn_arr) ->
            ([],[(arr, mem_arr, IxFun.slice ixfn_arr $ map (fmap pe64) slc)])
  where
    isFix (DimFix{}) = True
    isFix _ = False
getUseSumFromStm _ _ (Let (Pattern{}) _ (BasicOp (Index{}))) = ([],[]) -- incomplete slices
getUseSumFromStm td_env coal_tab (Let (Pattern [] pes) _ (BasicOp (ArrayLit ses _))) =
  let rds  = mapMaybe (getDirAliasedIxfn td_env coal_tab) $ mapMaybe seName ses
      wrts = mapMaybe (getDirAliasedIxfn td_env coal_tab) $ map patElemName pes
  in  (wrts, wrts++rds)
  where
    seName (Var a)      = Just a
    seName (Constant _) = Nothing
-- In place update @x[slc] <- a@. In the "in-place update" case,
--   summaries should be added after the old variable @x@ has
--   been added in the active coalesced table.
getUseSumFromStm td_env coal_tab (Let (Pattern [] [x']) _ (BasicOp (Update x slc a_se)))
  | Just (_, m_x, x_ixfn) <- getDirAliasedIxfn td_env coal_tab (patElemName x') =
    let x_ixfn_slc = IxFun.slice x_ixfn $ map (fmap pe64) slc
        r1 = (x, m_x, x_ixfn_slc)
    in  case a_se of
          Constant _ -> ([r1], [r1])
          Var a -> case getDirAliasedIxfn td_env coal_tab a of
                    Nothing -> ([r1], [r1])
                    Just r2 -> ([r1], [r1, r2])
getUseSumFromStm _ _ (Let (Pattern{}) _ (BasicOp (Update{}))) = Exc.assert False ([],[])
-- y = copy x
getUseSumFromStm td_env coal_tab (Let (Pattern [] [y]) _  (BasicOp (Copy x))) =
  let wrt = getDirAliasedIxfn td_env coal_tab $ patElemName y
      rd  = getDirAliasedIxfn td_env coal_tab x
  in  case (wrt, rd) of
        (Just w, Just r) -> ([w], [w, r])
        _ -> Exc.assert False ([],[]) -- should not happen as @x@ should be an array
getUseSumFromStm _ _ (Let (Pattern{}) _ (BasicOp (Copy{}))) = Exc.assert False ([],[])
-- concat
getUseSumFromStm td_env coal_tab (Let (Pattern _ ys) _  (BasicOp (Concat _i a bs _ses))) =
  let ws = mapMaybe (getDirAliasedIxfn td_env coal_tab) $ map patElemName ys
      rs = mapMaybe (getDirAliasedIxfn td_env coal_tab) (a : bs)
  in  (ws, ws++rs)
getUseSumFromStm td_env coal_tab (Let (Pattern _ ys) _  (BasicOp (Manifest _perm x))) =
  let ws = mapMaybe (getDirAliasedIxfn td_env coal_tab) $ map patElemName ys
      rs = mapMaybe (getDirAliasedIxfn td_env coal_tab) [x]
  in  (ws, ws++rs)
getUseSumFromStm td_env coal_tab (Let (Pattern _ ys) _  (BasicOp (Replicate _shp se))) =
  let ws = mapMaybe (getDirAliasedIxfn td_env coal_tab) $ map patElemName ys
  in  case se of
        Constant _ -> (ws, ws)
        Var x -> (ws, ws ++ mapMaybe (getDirAliasedIxfn td_env coal_tab) [x])
-- UnAcc, UpdateAcc are not supported
--getUseSumFromStm td_env coal_tab (BasicOp (UnAcc{})) =
--  error "UnAcc is not supported yet!"
--getUseSumFromStm td_env coal_tab (BasicOp (UpdateAcc{})) =
--  error "UpdateAcc is not supported yet!"
-- SubExp, Scratch, reshape, rearrange, rotate and scalar ops
--   do not require an array access.
getUseSumFromStm _ _ (Let (Pattern{}) _ (BasicOp{})) = ([],[])
-- if-then-else, loops are supposed to be treated separately,
-- calls are not supported, and Ops are not yet supported
getUseSumFromStm _ _ stm =
  error ("In MemRefAggreg.hs, getUseSumFromStm, unsuported case of stm being: " ++ pretty stm)

-- | This function:
--     1. computes the written and read memory references for the current statement
--          (by calling getUseSumFromStm)
--     2. fails the entries in active coalesced table for which the write set
--          overlaps the uses of the destination (to that point)
--   ToDo: a) the @noMemOverlap@ test probably requires the @scals@ field
--            from @BotUpEnv@ so that it translates the index functions.
--         b) it will also require a dictionary of ranges, e.g., for loop
--            indices, and positive constants, such as loop counts. This
--            should be computed on the top-down pass.
recordMemRefUses :: TopDnEnv -> BotUpEnv
                 -> Stm (Aliases ExpMem.SeqMem)
                 -> (CoalsTab,InhibitTab)
recordMemRefUses td_env bu_env stm =
  let (active_tab, inhibit_tab) = (activeCoals bu_env, inhibit bu_env)
      (stm_wrts0, stm_uses0) = getUseSumFromStm td_env active_tab stm
      (stm_wrts, stm_uses) = (map getLast2 stm_wrts0, map getLast2 stm_uses0)
      active_etries = M.toList active_tab

      (mb_wrts, prev_uses) = unzip $ map
        (\(m_b,etry) ->
          let all_aliases = addNames mempty m_b
              ixfns = map snd $ filter ((`nameIn` all_aliases) . fst) stm_wrts
              wrt_tmps = mapMaybe mbLmad ixfns
              wrt_lmads=
                if length wrt_tmps == length ixfns
                then Over $ S.fromList wrt_tmps
                else Top
              prev_use = mbAccess $ (dstrefs . memrefs) etry
              no_overlap = noMemOverlap (ranges td_env) prev_use wrt_lmads
          in  if no_overlap
              then (Just wrt_lmads, prev_use)
              else (Nothing, prev_use)
        ) active_etries

      mb_lmads = map
        (\(_,etry) -> 
          let all_aliases = foldl addNames mempty $ namesToList $ alsmem etry
              ixfns = map snd $ filter ((`nameIn` all_aliases) . fst) stm_uses
              lmads = mapMaybe mbLmad ixfns
          in  if length lmads == length ixfns
              then Over (S.fromList lmads)
              else Top
        ) active_etries

      -- keep only the entries that do not overlap with the memory
      -- blocks defined in @pat@ or @inner_free_vars@.
      -- the others must be recorded in @inhibit_tab@ because
      -- they violate the 3rd safety condition.
      active_tab1 = M.fromList $
                    map (\(wrts, (uses, prev_use, (k,etry))) ->
                          let mrefs' = (memrefs etry) { dstrefs = prev_use }
                              etry' = etry { memrefs = mrefs' }
                          in  (k, addLmads wrts uses etry')
                        ) $
                    filter okMemRef $
                    zip mb_wrts $ zip3 mb_lmads prev_uses active_etries
      failed_tab  = M.fromList $ map snd $ filter (not . okMemRef) $
                      zip mb_wrts active_etries
      (_,inhibit_tab1) = foldl markFailedCoal (failed_tab,inhibit_tab) (M.keys failed_tab)
  in  (active_tab1, inhibit_tab1)
  where
    okMemRef (Nothing, _) = False
    okMemRef (_, _)       = True
    getLast2 (_, a, b) = (a,b) 
    addNames acc m =
      case M.lookup m (m_alias td_env) of
        Nothing -> acc
        Just nms-> nms <> acc
    mbLmad indfun
      | (True, subst) <- translateIndFunFreeVar (scope td_env) (scals bu_env) (Just indfun),
        (IxFun.IxFun (lmad NE.:| []) _ _) <- IxFun.substituteInIxFun subst indfun =
          Just lmad
    mbLmad _ = Nothing
    mbAccess Top = Top
    mbAccess (Over l)
      | l == mempty = Over mempty
    mbAccess (Over slmads)
      | lmad:lmads <- S.toList slmads,
        fake_ixfn <- IxFun.IxFun (lmad NE.:| lmads) ([pe64 $ intConst Int64 1]) False,
        (True, subst) <- translateIndFunFreeVar (scope td_env) (scals bu_env) (Just fake_ixfn) =
        Over $ S.fromList $ NE.toList $ IxFun.ixfunLMADs $ IxFun.substituteInIxFun subst fake_ixfn
    mbAccess _ = Top
    addLmads (Just wrts) uses etry =
      etry { memrefs = unionMemRefs (MemRefs uses wrts) (memrefs etry) }
    addLmads _ _ _ =
        error "Impossible case reached because we have filtered Nothings before"

-- ToDo: implement this function as precise as possible
noMemOverlap :: RangeTab -> AccsSum -> AccsSum -> Bool
noMemOverlap _ _ (Over mr)
  | mr == mempty = True
noMemOverlap _ (Over mr) _
  | mr == mempty = True
noMemOverlap _ Top _ = False
noMemOverlap _ _ Top = False
noMemOverlap _ (Over _) (Over _) =
  False
  -- ^ add non-trivial implementation, please

-- | Checks whether the index function can be translated at the
--     current program point and also returns the substitutions.
--     It comes down to answering the question: "can one perform
--     enough substitutions (from the bottom-up scalar table) until
--     all vars appearing in the index function are defined in the
--     current scope?"
--   Please fix: needs a form of fix-point iteration!
translateIndFunFreeVar :: ScopeTab -> ScalarTab -> (Maybe ExpMem.IxFun)
                       -> (Bool,FreeVarSubsts)
translateIndFunFreeVar _ _ Nothing = (True, M.empty)
translateIndFunFreeVar scope0 scals0 (Just indfun) =
  translateHelper M.empty $ freeIn indfun
  where
    translateHelper :: FreeVarSubsts -> Names -> (Bool,FreeVarSubsts)
    translateHelper substs nms
      | mempty == nms = (True, substs)
    translateHelper subst0 cur_fvs =
      let fv_trans_vars = filter (\x -> not $ M.member x scope0) $ namesToList cur_fvs
          (subs, new_fvs_2) = unzip $ mapMaybe getSubst fv_trans_vars
          new_fvs = foldl (<>) mempty new_fvs_2
      in  if length fv_trans_vars == length subs
          then translateHelper (M.union subst0 $ M.fromList subs) new_fvs
          else (False, M.empty)
    getSubst v
      | Just pe <- M.lookup v scals0,
        IntType Int64 <- primExpType pe =
          Just ((v,isInt64 pe), freeIn pe)
    getSubst _ = Nothing

-- | Memory-block removal from active-coalescing table
--   should only be handled via this function, it is easy
--   to run into infinite execution problem; i.e., the
--   fix-pointed iteration of coalescing transformation
--   assumes that whenever a coalescing fails it is
--   recorded in the @inhibit@ table.
markFailedCoal :: (CoalsTab, InhibitTab)
               -> VName
               -> (CoalsTab, InhibitTab)
markFailedCoal (coal_tab,inhb_tab) src_mem =
  case M.lookup src_mem coal_tab of
         Nothing   -> (coal_tab,inhb_tab)
         Just coale->
           let failed_set = case M.lookup src_mem inhb_tab of
                              Nothing  -> oneName (dstmem coale)
                              Just fld -> fld <> oneName (dstmem coale)
           in  ( M.delete src_mem coal_tab
               , M.insert src_mem failed_set inhb_tab )
