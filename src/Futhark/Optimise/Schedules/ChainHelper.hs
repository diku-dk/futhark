{-# LANGUAGE Strict #-}

-- | This module implements enabling transformation for ultimately
--   applying a chain of schedules, e.g., hoisting the stms the
--   schedule depends on vefore the SOAC/WithAcc producer.
module Futhark.Optimise.Schedules.ChainHelper
  ( ChainEntry(..)
  , ChainsEnv(..)
  , schedKind
  , freshChainsEnv
  , findScheduleChain
  )
where

import Data.Maybe
import Data.List qualified as L
import Data.Map.Strict qualified as M
--import Data.Maybe
import Futhark.IR.SOACS -- hiding (SOAC (..))
--import Futhark.Tools
--import Futhark.IR.Mem.LMAD qualified as LMAD
-- import Futhark.Util.Pretty hiding (line, sep, (</>))
import Futhark.Optimise.Schedules.ScheduleM( ScheduleM )
import Futhark.Optimise.Schedules.EnvUtils( TopEnv(..) )
import Futhark.Optimise.Schedules.SchedUtils( HLSched(..), parseSchedule )
-- import Futhark.SoP.SoP

-- | Assume the following schedule chain:
--   @ r0 = soac ... @
--   @ r1 = transpose r0 @
--   @ r2 = sched1 r0 @
--   @ r3 = fuse r2 @
--   @ r4 = sched2 r3 @
-- In principle we aim to support schedule chains of length
--   at most 3, of the form above, i.e., sched o fuse o sched.
-- We enforce that the results of the chain (@r0 - r3@) are
--   not used in any of the other stms between @r0 - r4@.
-- This guarantees that moving these other stms across the SOAC
--   is safe modulo in-place updates of arrays used in SOAC.
--   To satisfy this we need to check that the array variables
--   used in SOACS are not used in the chain, which should not
--   happen.
data ChainEntry = ChainEntry
  { schedule :: HLSched
    -- ^ the closest schedule to the SOAC producer
  , sched_stms :: Stms SOACS
    -- ^ Denotes he last part of the schedule chain, where
    --    the last stm is the earliest schedule, and the
    --    other are rearrange stms, e.g., @[r1_stm, r2_stm]@.
    --   The rearrange are intended to modify the schedule
    --    but are not supported yet.
    --   If the schedule does not fire, these stms need to
    --    be inserted back as close to the schedule, i.e.,
    --    just before the (WithAcc / SOAC) producer.
  , sched_tc_syms :: Names
    -- ^ Denotes the transitive closure of the symbols used
    --   in the definition of the schedule stm, until just
    --   after the SOAC / WithAcc producer.  
  , hoist_stms :: Stms SOACS
    -- These are the stms that the schedule depends on and
    --   need to be hoisted above the schedule definition;
    --   typically simple stms producing scalar and array literals.
    -- Sasfety check for moving @hoist_stms@ above producer SOAC:
    --   the set of variables denoting arrays in @sched_tc_syms@
    --   does not overlap with the array variables used in the
    --   SOAC => hence we cannot hoist an in-place update after
    --   the SOAC before it (referring to an array read in SOAC).
  , sched_kind :: Int
    -- ^ the kind of the schedule, see encoding in function @schedKind@
  , chain_len  :: Int
    -- ^ the length of the emtire schedule chain, max 3.
  , last_name  :: VName
    -- ^ the last name of the chain: it is safe to start substitution there
    --   (it is not essential, currently redundant)
  }

data ChainsEnv = ChainsEnv
  { chainTab :: M.Map VName ChainEntry
  , active_sched_name :: Maybe VName
  -- ^ only one schedule chain can be active at a time
  --   updates are done to the corresponding entry in @chainTab@
  , names_used_so_far :: Names
  , sched_res_names   :: Names
  }

freshChainsEnv :: ChainsEnv
freshChainsEnv = ChainsEnv mempty Nothing mempty mempty

schedKind :: String -> Int
schedKind str
 | L.isPrefixOf "fuseSched" str = 1
 | L.isPrefixOf "hlSched" str = 2
 | L.isPrefixOf "cmpctSched" str = 3
 | True = 0

-- | ToDo: Implement me properly. The code below is absolete,
--         needs significant rethinking and adjustments.
findScheduleChain :: TopEnv -> ChainsEnv -> Stm SOACS -> ScheduleM ChainsEnv
findScheduleChain _ env _stm
  | Just tgt_nm <- active_sched_name env,
    isNothing (M.lookup tgt_nm (chainTab env)) =
  error $ "findScheduleChain, active schedule chain: " ++
          prettyString tgt_nm ++ " misses an entry in chainTab!"
findScheduleChain td_env env stm@(Let (Pat [pat_el]) _aux e)
  | Apply fnm args_diet _rtp _ <- e,
    0 < schedKind (nameToString fnm),
    Var arr_nm : _ <- (reverse . fst . unzip) args_diet,
    nameIn arr_nm (names_used_so_far env) =
  error $ "Intermediate Scheduled Array : " ++ prettyString arr_nm ++
          " has been used outside a schedule statement!"
  | Apply fnm _args_diet _rtp _ <- e,
    0 < schedKind (nameToString fnm),
    -- Var arr_nm : _ <- (reverse . fst . unzip) args_diet,
    Just act_sched_nm <- active_sched_name env,
    not (act_sched_nm == patElemName pat_el) =
  error $ "Error: found overlap of distinct schedules: " ++
          prettyString (act_sched_nm, patElemName pat_el)
  | Apply fnm args_diet _rtp _ <- e,
    k <- schedKind $ nameToString fnm,
    k > 0,
    Var arr_nm : _ <- (reverse . fst . unzip) args_diet = do
  -- (VName, PatElem (LetDec SOACS), HLSched, LMAD)
  let (_, _, sched, _) =
        parseSchedule fnm td_env (fst $ unzip args_diet) $ Pat [pat_el]
      nms_so_far = namesSubtract (freeIn e <> names_used_so_far env) $ oneName arr_nm
  pure $
    env { active_sched_name = Just arr_nm
        , names_used_so_far = nms_so_far
        , sched_res_names = oneName arr_nm <> (sched_res_names env)
        , chainTab = updateSchedTab arr_nm k sched
        }
  where
 -- ChainEntry schedule sched_stms sched_tc_syms hoist_stms sched_kind chain_len last_name
    updateSchedTab :: VName -> Int -> HLSched -> M.Map VName ChainEntry
    updateSchedTab _arr_nm k _sched
      | Just entry <- M.lookup (patElemName pat_el) (chainTab env),
        s_c <- chain_len entry,
        s_k <- sched_kind entry,
        s_c > 2 || (s_k > 1 && k > 1) || (s_k == 1 && k == 1) || (s_c == 2 && k == 1) =
      error $ "Illegal Schedule Chain detected at: " ++ prettyString stm
    updateSchedTab arr_nm k sched =
      let patel_nm = patElemName pat_el
          entry0 = ChainEntry sched (oneStm stm) (freeIn stm) mempty k (1::Int) patel_nm
      in  case M.lookup patel_nm (chainTab env) of
            Nothing -> M.insert arr_nm entry0 (chainTab env)
            Just entry ->
              let entry' =
                   entry0 { sched_stms = oneStm stm <> sched_stms entry
                          , chain_len  = 1 + chain_len entry
                          , last_name  = (last_name entry)
                          }
              in  M.insert arr_nm entry' $ M.delete patel_nm (chainTab env)
-- The case of a Rearrange (e.g., transposition) of a scheduled SOAC
--   can in principle be supported by transforming the schedule's permutation;
-- Not implemented yet.
findScheduleChain _ env (Let (Pat [pat_el]) _aux e)
  | BasicOp (Rearrange arr_nm _) <- e,
    Just cur_arr <- active_sched_name env,
    cur_arr == patElemName pat_el,
    nameIn arr_nm  (names_used_so_far env) ||
      nameIn cur_arr (names_used_so_far env) =
  error $ "In findScheduleChain transpose case: rescheduled name used outside scheduling stms "
            ++ prettyString (cur_arr, arr_nm)
findScheduleChain _ env stm@(Let (Pat [pat_el]) _aux e)
  | BasicOp (Rearrange arr_nm sigma) <- e,
    Just cur_arr <- active_sched_name env,
    cur_arr == patElemName pat_el,
    Just entry <- M.lookup arr_nm (chainTab env) = do
    let schedule' = applyRearrangeOnSchedule sigma $ schedule entry -- HLSched
        entry' = entry { schedule = schedule', sched_stms = oneStm stm <> sched_stms entry }
        tab' = M.insert arr_nm entry' $ M.delete cur_arr (chainTab env)
        nms_so_far = namesSubtract (freeIn e <> names_used_so_far env) $ oneName arr_nm
    pure $
      env { active_sched_name = Just arr_nm
          , names_used_so_far = nms_so_far
          , sched_res_names = oneName arr_nm <> (sched_res_names env)
          , chainTab = tab'
          }
    where
      -- Currently unimplemented. If implemented, a fusion schedule yields an error.
      applyRearrangeOnSchedule _sigma sched =
        error $ "Rearranging the schedule is not implemented yet. Stm: " ++
                prettyString stm ++ " schedule: " ++ prettyString sched
--
-- The case of a root WithAcc or SOAC, i.e., target to rescheduling
--   1. turn off @active_sched_name@ in environment -- root was found
--   2. check that the rescheduled result has not been used outside the chain
--   2. check that hoisting the statements that the schedule depends on
--        is safe, i.e., no overlap in array names
findScheduleChain _ env stm@(Let pat _aux e)
  | isSOACorWithAcc e,
    Just tgt_arr <- active_sched_name env,
    pat_nms <- map patElemName (patElems pat),
    nameIn tgt_arr (namesFromList pat_nms) =
    if nameIn tgt_arr (names_used_so_far env)
    then error $ "In findScheduleChain, Root SOAC/WithAcc case: " ++
                 "rescheduled name was used outside scheduling stms " ++
                 prettyString tgt_arr ++ " used-so-far: " ++
                 prettyString (names_used_so_far env)
    else checkHoistAndTurnOffSched tgt_arr
  where
    isSOACorWithAcc (Op{}) = True
    isSOACorWithAcc (WithAcc{}) = True
    isSOACorWithAcc _ = False
    --
    checkHoistAndTurnOffSched tgt_arr = do
      case M.lookup tgt_arr (chainTab env) of
        Nothing -> error $ "In findScheduleChain, target SOAC/WithAcc case: empty entry for "
                           ++ prettyString tgt_arr
        Just entry -> do
          let common_names = namesIntersection (freeIn e) $ sched_tc_syms entry
          common_tps <- mapM lookupType $ namesToList common_names
          if any hasArrayOrAccType common_tps
          then error $ "Schedule stms cannot be hoisted across SOAC. Syms: " ++
                 prettyString common_names ++ " are used in SOAC: " ++ prettyString stm
          else pure $ env { active_sched_name = Nothing
                          , names_used_so_far = freeIn e <> (names_used_so_far env)
                          , sched_res_names = mempty
                          }
    --
    hasArrayOrAccType (Array{}) = True
    hasArrayOrAccType (Acc{}) = True
    hasArrayOrAccType _ = False
--
-- A statement that is not part of the scheduling chain should not
--   the intermediate names of a scheduled array
findScheduleChain _ env stm@(Let _pat _aux e)
  | namesIntersect (sched_res_names env) (freeIn e) =
  error $ "An Intermediate Name of a Scheduled Array " ++
          prettyString (sched_res_names env) ++
          " is used outside the schedule chain in stm: " ++
          prettyString stm
-- The case of a statement whose result is used by the schedule and
--   needs to be hoisted before the WithAcc/SOAC target to rescheduling.
findScheduleChain _ env stm@(Let pat _aux e)
  | pat_nms <- map patElemName (patElems pat),
    Just tgt_arr_nm <- active_sched_name env,
    Just entry <- M.lookup tgt_arr_nm (chainTab env),
    namesIntersect (namesFromList pat_nms) (sched_tc_syms entry) = do
  let entry' = entry { hoist_stms = oneStm stm <> hoist_stms entry
                     , sched_tc_syms = freeIn e <> sched_tc_syms entry
                     }
  pure $ env { names_used_so_far = freeIn e <> names_used_so_far env
             , chainTab = M.insert tgt_arr_nm entry' (chainTab env)
             }
-- default:
findScheduleChain _ env (Let _ _ e) =
  pure $ env { names_used_so_far = freeIn e <> names_used_so_far env }


-------------------------------------------
--- Grabage: please clean up eventually ---
-------------------------------------------

{--
-- | This is aimed to find a chain of maximum three schedules:
--     a. ordered as Schedule2 o Fusion o Schedule1, where
--        Schedule1,2 are non-fusion schedules. Schedule1 can possibly
--        be applied at once, but Schedule2 is delayed after fusion.
--     b. consecutive non-fusion schedules are illegal and
--        consecutive fusion schedules are illegal as well.
--     c. the intermediate result names in the chain cannot be used
--          elsewhere in the code (other than in schedule calls)
-- ToDo: this is still unsafe since the plan is to codegen the SOAC
--       at the position of its schedule. The latter is not possible
--       if a variable used by the target SOAC is consumed between
--       the SOAC and the schedule definition. An "easy" fix is to
--       allow only "simple, effect-free" scalar instructions between them.
findScheduleChain :: SchedChainTab -> Stm SOACS -> SchedChainTab
findScheduleChain (used_names, sched_names, tab) stm@(Let (Pat [pat_el]) _aux e)
  | Apply fnm args_diet _rtp _ <- e,
    0 < schedKind (nameToString fnm),
    Var arr_nm : _ <- (reverse . fst . unzip) args_diet,
    nameIn arr_nm used_names =
  error $ "Intermediate Scheduled Array : " ++ prettyString arr_nm ++
          " has been used outside a schedule statement!"
  | Apply fnm args_diet _rtp _ <- e,
    k <- schedKind $ nameToString fnm,
    k > 0,
    Var arr_nm : _ <- (reverse . fst . unzip) args_diet =
  ( used_names, oneName arr_nm <> sched_names, updateSchedTab arr_nm k )
  where
    updateSchedTab arr_nm k
      | pat_el_nm <- patElemName pat_el,
        Just (_, (s_k, s_c), last_nm) <- M.lookup pat_el_nm tab =
      let mstm = if k > 1 then Just stm else Nothing
      in if s_c > 2 || (s_k > 1 && k > 1) || (s_k == 1 && k == 1) || (s_c == 2 && k == 1)
         then error $ "Illegal Schedule Chain detected at: " ++ prettyString stm
         else M.insert arr_nm (mstm, (k, s_c+1), last_nm) $
              M.delete pat_el_nm tab
      | True = -- i.e., Nothing <- M.lookup (patElemName pat_el) tab
      let mstm = if k > 1 then Just stm else Nothing
      in  M.insert arr_nm (mstm, (k,1), patElemName pat_el) tab

--
findScheduleChain (used_names, sched_names, tab) (Let _pat _aux e)
  | common_nms <- namesIntersection sched_names (freeIn e),
    not (common_nms == mempty) =
  error $ "Intermediate Scheduled Arrays : " ++ prettyString common_nms ++
          " have been used outside a schedule statement!"
  | True = (freeIn e <> used_names, sched_names, tab)
--}
