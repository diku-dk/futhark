{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Futhark.Optimise.MemBlockCoalesce.MemRefAggreg
       ( recordMemRefUses, markFailedCoal )
       where


import Prelude
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Control.Exception.Base as Exc
import Data.List.NonEmpty (NonEmpty (..))

--import Debug.Trace

import Futhark.IR.Aliases
import Futhark.Analysis.PrimExp.Convert
import qualified Futhark.IR.SeqMem as ExpMem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.Optimise.MemBlockCoalesce.DataStructs
import Futhark.Optimise.MemBlockCoalesce.TopDownAn

mem_empty :: MemRefs
mem_empty = MemRefs mempty mempty


-- This function computes the written and read memory references for the current statement
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
--     2. fails the entries in active coalesced table for which the 
recordMemRefUses:: TopDnEnv -> CoalsTab -> InhibitTab
              -> Stm (Aliases ExpMem.SeqMem)
              -> (CoalsTab,InhibitTab)
recordMemRefUses td_env active_tab inhibit_tab stm =
  let (stm_wrts0, stm_uses0) = getUseSumFromStm td_env active_tab stm
      (stm_wrts,  stm_uses)  = (map getLast2 stm_wrts0, map getLast2 stm_uses0)

      active_etries = M.toList active_tab

      mb_wrts = map
        (\(m_b,etry) ->
          let all_aliases = addNames mempty m_b
              ixfns = map snd $ filter ((`nameIn` all_aliases) . fst) stm_wrts
              wrt_lmads = mapMaybe mbLmad ixfns
              prev_uses = (dstrefs . memrefs) etry
              no_overlap = noMemOverlap prev_uses $ S.fromList wrt_lmads
          in  if length wrt_lmads == length ixfns && no_overlap
              then Just (S.fromList wrt_lmads)
              else Nothing
        ) active_etries

      mb_lmads = map
        (\(_,etry) -> 
          let all_aliases = foldl addNames mempty $ namesToList $ alsmem etry
              ixfns = map snd $ filter ((`nameIn` all_aliases) . fst) stm_uses
              lmads = mapMaybe mbLmad ixfns
          in  if length lmads == length ixfns
              then Just (S.fromList lmads)
              else Nothing
        ) active_etries

      -- keep only the entries that do not overlap with the memory
      -- blocks defined in @pat@ or @inner_free_vars@.
      -- the others must be recorded in @inhibit_tab@ because
      -- they violate the 3rd safety condition.
      active_tab1 = M.fromList $ map (\(wrts, uses, (k,etry)) -> (k,addLmads wrts uses etry)) $
                      filter okMemRef $ zip3 mb_wrts mb_lmads active_etries
      failed_tab  = M.fromList $ map thrd $ filter (not . okMemRef) $
                      zip3 mb_wrts mb_lmads active_etries
      (_,inhibit_tab1) = foldl markFailedCoal (failed_tab,inhibit_tab) (M.keys failed_tab)
  in  (active_tab1, inhibit_tab1)
  where
    thrd (_, _, c) = c
    okMemRef (Nothing, _, _) = False
    okMemRef (_, Nothing, _) = False
    okMemRef (_, _, _)       = True
    getLast2 (_, a, b) = (a,b) 
    addNames acc m =
      case M.lookup m (m_alias td_env) of
        Nothing -> acc
        Just nms-> nms <> acc
    mbLmad (IxFun.IxFun (lmad :| []) _ _) = Just lmad
    mbLmad _ = Nothing
    addLmads (Just wrts) (Just uses) etry =
      let memref = memrefs etry
          memrefs' = memref { srcwrts = wrts `S.union` (srcwrts memref)
                            , dstrefs = uses `S.union` (dstrefs memref)
                            }
      in  etry { memrefs = memrefs'}
    addLmads _ _ _ =
        error "Impossible case reached because we have filtered Nothings before"

-- ToDo: implement this function as precise as possible
noMemOverlap :: S.Set LmadRef -> S.Set LmadRef -> Bool
noMemOverlap mr1 mr2 =
  if mr1 == mempty || mr2 == mempty
  then True
  else False

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
