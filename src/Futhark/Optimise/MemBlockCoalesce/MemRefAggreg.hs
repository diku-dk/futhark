{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Futhark.Optimise.MemBlockCoalesce.MemRefAggreg
       ( recordDstUses )
       where


import Prelude
import Data.Maybe
import qualified Data.Map.Strict as M
--import qualified Data.Set as S
import qualified Control.Exception.Base as Exc

--import Debug.Trace

import Futhark.IR.Aliases
import Futhark.Analysis.PrimExp.Convert
import qualified Futhark.IR.SeqMem as ExpMem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.Optimise.MemBlockCoalesce.DataStructs
import Futhark.Optimise.MemBlockCoalesce.TopDownAn

mem_empty :: MemRefs
mem_empty = MemRefs mempty mempty


{--
addName :: VName -> Maybe (VName, ExpMem.IxFun)
        -> Maybe (VName, VName, ExpMem.IxFun)
addName _ Nothing = Nothing
addName x (Just (a, b)) = Just (x, a, b)
--}

getUseSumFromExp :: TopDnEnv -> CoalsTab
                 -> Exp (Aliases ExpMem.SeqMem)
                 -> [(VName, VName, ExpMem.IxFun)]
getUseSumFromExp td_env coal_tab (BasicOp (Index arr slc))
  | Just (MemBlock _ shp _ _) <- getScopeMemInfo arr (scope td_env),
    length slc == length (shapeDims shp) && all isFix slc =
      case getDirAliasedIxfn td_env coal_tab arr of
        Nothing -> []
        Just (_, mem_arr, ixfn_arr) ->
            [(arr, mem_arr, IxFun.slice ixfn_arr $ map (fmap pe64) slc)]
  where
    isFix (DimFix{}) = True
    isFix _ = False
getUseSumFromExp _ _ (BasicOp (Index{})) = [] -- incomplete slices
getUseSumFromExp td_env coal_tab (BasicOp (ArrayLit ses _)) =
  mapMaybe (getDirAliasedIxfn td_env coal_tab) $ mapMaybe seName ses
  where
    seName (Var a)      = Just a
    seName (Constant _) = Nothing
-- In place update @x[slc] <- a@. In the "in-place update" case,
--   summaries should be added after the old variable @x@ has
--   been added in the active coalesced table.
getUseSumFromExp td_env coal_tab (BasicOp (Update x slc a_se))
  | Just (_, m_x, x_ixfn) <- getDirAliasedIxfn td_env coal_tab x =
    let x_ixfn_slc = IxFun.slice x_ixfn $ map (fmap pe64) slc
        r1 = (x, m_x, x_ixfn_slc)
    in  case a_se of
          Constant _ -> [r1]
          Var a -> case getDirAliasedIxfn td_env coal_tab a of
                    Nothing -> [r1]
                    Just r2 -> [r1, r2]
getUseSumFromExp _ _ (BasicOp (Update{})) = Exc.assert False []
-- copy case
getUseSumFromExp td_env coal_tab (BasicOp (Copy x)) =
  case getDirAliasedIxfn td_env coal_tab x of
    Just r -> [r]
    Nothing -> Exc.assert False [] -- should not happen as @x@ should be an array
-- concat
getUseSumFromExp td_env coal_tab (BasicOp (Concat _i a bs _ses)) =
  mapMaybe (getDirAliasedIxfn td_env coal_tab) (a : bs)
getUseSumFromExp td_env coal_tab (BasicOp (Manifest _perm x)) =
  mapMaybe (getDirAliasedIxfn td_env coal_tab) [x]
getUseSumFromExp td_env coal_tab (BasicOp (Replicate _shp (Var x))) =
  mapMaybe (getDirAliasedIxfn td_env coal_tab) [x]
-- UnAcc, UpdateAcc are not supported
--getUseSumFromExp td_env coal_tab (BasicOp (UnAcc{})) =
--  error "UnAcc is not supported yet!"
--getUseSumFromExp td_env coal_tab (BasicOp (UpdateAcc{})) =
--  error "UpdateAcc is not supported yet!"
-- SubExp, Scratch, reshape, rearrange, rotate and scalar ops
--   do not require an array access.
getUseSumFromExp _ _ (BasicOp{}) = []
-- if-then-else, loops are supposed to be treated separately,
-- calls are not supported, and Ops are not yet supported
getUseSumFromExp _ _ e =
  error ("In MemRefAggreg.hs, getUseSumFromExp, unsuported case of exp being: " ++ pretty e)

{--
getUseSumFromExp :: Exp (Aliases ExpMem.SeqMem)
                 -> M.Map VName Names
                 -> [(VName, VName, ExpMem.IxFun)]
getUseSumFromExp (Index arr slc) v_alias m_alias 
  | Just (MemBlock tp shp m ixfn) <- getScopeMemInfo arr scope_tab =
    if length slc == length (shapeDims shp) && all isFix slc
    then case M.lookup m actv_tab of
            Just info@(CoalsEntry x_mem _ _ vtab _ _) ->
                case M.lookup arr (vartab info) of
                    Just (Coalesced k mblk@(MemBlock _ _ _ new_indfun) _) ->
                        [(arr, dstmem info, new_indfun)]
                    Nothing -> -- search in v_alias
                        case getTransitiveAlias v_alias (vartab info) arr id of
                            Nothing -> error "can't happen ??"
            Nothing ->
    else [] -- not full slice, so the actual access manifests later
    where
      isFix (DimFix _) = True
      isFix _ = False
getUseSumFromExp (Index arr slc) m_alias =
  error "Can't Happen in MemRefAggreg.hs, fun getUseSumFromExp"
--}



{--
getMemRefsStm :: TopDnEnv -> BotUpEnv -> Stm (Aliases ExpMem.SeqMem) -> MemRefs
getMemRefsStm td_env bu_env (Let (Pattern [] [pe]) _ (BasicOp (SubExp (Var v)))) = mem_empty
getMemRefsStm td_env bu_env (Let (Pattern [] [pe]) _ (BasicOp (Opaque (Var v)))) = mem_empty
getMemRefsStm td_env bu_env (Let (Pattern [] [pe]) _ (BasicOp (ArrayLit ses _tp))) =
  let nms = mapMaybe se2Maybe ses
      mb_ixfs = map (\(MemBlock _ _ m ixf) -> (m,ixf)) $
      				mapMaybe (`getScopeMemInfo` (scope td_env)) nms
  	  act_tab' = M.map (\etry -> ) act_tab 
  where
  	se2Maybe (Var nm) = Just nm
  	se2Maybe (Constant _) = Nothing
  	updateCoalEtry :: [(VName,ExpMem.IxFun)] -> CoalsEntry -> CoalsEntry
  	updateCoalEtry mb_ixfs etry =

--  | Just primexp <- primExpFromExp (basePMconv (scope td_env) (scals bu_env)) e =

getMemIxfun x scope_td act_tab
  | Nothing <- getScopeMemInfo x scope_td = Nothing
getMemIxfun x
  | Just (MemBlock _ptp _shp m ixfn) <- getScopeMemInfo x scope_td,
  	Just coal_etry <- M.lookup m act_tab =
--}

recordDstUses :: Pattern (Aliases ExpMem.SeqMem)
              -> Exp (Aliases ExpMem.SeqMem)
              -> TopDnEnv -> CoalsTab -> InhibitTab
              -> (CoalsTab,InhibitTab)
recordDstUses pat e td_env active_tab inhibit_tab =
  let inner_free_vars = freeIn e
      uses = getUseSumFromExp td_env active_tab
      e_mems  = mapMaybe (`getScopeMemInfo` (scope td_env)) $ namesToList inner_free_vars
      -- ToDo: understand why the memory block of pattern must be filtered:
      --       this definitely needs to be relaxed, e.g., because the src
      --       and sink can be created in the same loop.
      memasoc = getArrMemAssoc pat
      -- get memory-block names that are used in the current stmt.
      stm_mems= namesFromList $ map (\(MemBlock _ _ mnm _) -> mnm) $
                e_mems ++ map snd memasoc

      -- BIG BUG/ToDo: take the aliasing transitive closure of @all_mems@
      all_mems = stm_mems

      -- keep only the entries that do not overlap with the memory
      -- blocks defined in @pat@ or @inner_free_vars@.
      -- the others must be recorded in @inhibit_tab@ because
      -- they violate the 3rd safety condition.
      active_tab1 = M.filter (\etry -> mempty == namesIntersection all_mems (alsmem etry)) active_tab
      failed_tab  = M.difference active_tab active_tab1
      (_,inhibit_tab1) = foldl markFailedCoal (failed_tab,inhibit_tab) (M.keys failed_tab)

  in  (active_tab1, inhibit_tab1)


-- redundant, also in ArrayCoalescing.hs; please get rid of it
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
