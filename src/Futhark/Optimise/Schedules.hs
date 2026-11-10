{-# LANGUAGE TypeFamilies #-}

-- | This module consists of performing a sequence of
--     code transformations that are defined by users
--     by means of high-level schedules.
--   ... more explanation is comming ...

module Futhark.Optimise.Schedules
  ( applySchedules ) where

import Control.Monad (foldM)
--import Control.Monad.Reader
--import Control.Monad.State hiding (state)
import Control.Monad (forM)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Sq
import Data.Maybe
--import Futhark.Builder
import Futhark.IR.SOACS
-- import Futhark.Optimise.TileLoops.Shared
import Futhark.Pass
--import Futhark.Tools
--import Futhark.Transform.Rename
--import Futhark.Analysis.PrimExp.Convert
--import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.Optimise.Schedules.ScheduleM
  (ScheduleEnv(..), ScheduleM, runScheduleM, rollbackAlgEnv)
import Futhark.Optimise.Schedules.EnvUtils
  (TopEnv (..), freshTopEnv, updateTopdownEnv)
import Futhark.Optimise.Schedules.SchedUtils
  ( BotEnv(..), freshBotEnv, parseSchedule, oneFullyConsumedMapRed )
import Futhark.Optimise.Schedules.ChainHelper
  (ChainEntry(..), ChainsEnv(..), schedKind, findScheduleChain, freshChainsEnv)
import Futhark.Optimise.Schedules.Safety(checkValidSched)
import Futhark.Optimise.Schedules.Stripmine(applyStripmining)
import Futhark.Optimise.Schedules.Permute(applyPermute)
import Futhark.Optimise.Schedules.AdjustResult(manifestResult)
--import Futhark.SoP.Monad (AlgEnv (..), MonadSoP (..))
import Debug.Trace

-----------------------------------------
--- The Definition of the Pass
----------------------------------------

type Env = ((TopEnv, BotEnv), ChainsEnv)

chainEnv :: Env -> ChainsEnv
chainEnv = snd

scalrEnv :: Env -> TopEnv
scalrEnv = fst . fst

schedEnv :: Env -> BotEnv
schedEnv = snd . fst

{--
se2VName :: SubExp -> Maybe VName
se2VName (Var nm) = Just nm
se2VName _ = Nothing
--}

runSchedules :: TopEnv -> Scope SOACS -> Stms SOACS -> PassM (Stms SOACS)
runSchedules topenv scope stms =
  runScheduleM
    scope
    (ScheduleEnv { vNameSource = mempty, algEnv = mempty })
    (fmap snd (applySchedOnStms ((topenv, freshBotEnv), freshChainsEnv) stms))

applySchedOnFun :: Stms SOACS -> FunDef SOACS -> PassM (FunDef SOACS)
applySchedOnFun consts fun = do
  let fun_bdy = funDefBody fun
      topenv  = freshTopEnv { bdy_res = map resSubExp $ bodyResult fun_bdy }
      scope   = scopeOf fun <> scopeOf consts
  fun_stms' <- runSchedules topenv scope $ bodyStms fun_bdy
  trace ("End Function: " ++ show (funDefName fun) ) $
    pure fun { funDefBody = (funDefBody fun) { bodyStms = fun_stms' } }

-- | The pass definition.
{-# NOINLINE applySchedules #-}
applySchedules :: Pass SOACS SOACS
applySchedules =
  Pass
    { passName = "High-level schedules"
    , passDescription = "The entry-point for applying high-level schedules"
    , passFunction = \ prog -> do
        trace ("\nProg before pass: "++prettyString prog++"\n") $
          intraproceduralTransformationWithConsts pure applySchedOnFun prog
    }

-----------------------------------------
--- Analysis
----------------------------------------

{--
-- | Traversal structure
applySchedOnStms :: Env -> Stms SOACS -> ScheduleM (Stms SOACS)
applySchedOnStms (scal_env, sched_env0) stmts = do
  let active_schedules = chainTab $
        foldl findScheduleChain freshChainsEnv $ reverse $ stmsToList stmts
      active_soac_nms = namesFromList $ M.keys active_schedules
      sched_env = sched_env0 { target_soac_names =  active_soac_nms }
  (_env', stms) <- foldM handleStm ((scal_env, sched_env), mempty) $ stmsToList stmts
  -- ToDo: check that no change-of-indexing caused by rescheduling
  --       is pending in the @bdy_res@ field of @fst env'@ result.
  pure $ stms
  where
    handleStm (env, gen_stms) stm = do
      td_env' <- updateTopdownEnv (fst env) stm
      (bu_env', stms) <- applySchedOnStm (td_env', snd env) stm
      pure ((td_env', bu_env'), gen_stms <> stms)
--}

-- | Traversal structure
applySchedOnStms :: Env -> Stms SOACS -> ScheduleM (BotEnv, Stms SOACS)
applySchedOnStms env0 stmts = do
  scope <- askScope
  localScope (scope <> scopeOf stmts) $
    rollbackAlgEnv $
      fmap snd (traverseStms env0 stmts)
  where
    traverseStms :: Env -> Stms SOACS -> ScheduleM (ChainsEnv, (BotEnv, Stms SOACS))
    traverseStms env (Sq.Empty) =
      pure ( chainEnv env, (schedEnv env, mempty) )
    traverseStms env (stm Sq.:<| stms) = do
      -- Compute @td_env@ top down
      scalar_env' <- updateTopdownEnv (scalrEnv env) stm
      -- Compute @chenv@ bottom up
      (chenv, (_, stms')) <- traverseStms ((scalar_env', schedEnv env), chainEnv env) stms
      -- Compute @bu_env@ top down
      chenv' <- findScheduleChain scalar_env' chenv stm
      (sched_env', cur_stms) <- applySchedOnStm ((scalar_env', schedEnv env), chenv') stm
      pure ( chenv', (sched_env', cur_stms <> stms') )

-- | Applies schedules in a body of statements:
--   1. sets the body result in the top-down env and
--   2. processes the statements by means of @applySchedOnStms@
applySchedOnBody :: Env -> Body SOACS -> ScheduleM (BotEnv, Body SOACS)
applySchedOnBody ((td_env, sched_env), chnenv) body = do
  let td_env' = td_env { bdy_res = map resSubExp $ bodyResult body}
  -- scope <- askScope
  (sched_env', stms') <-
    -- localScope (scope <> scopeOf (bodyStms body)) $
      applySchedOnStms ((td_env', sched_env), chnenv) $ bodyStms body
  pure (sched_env', body { bodyStms = stms' })

-- | Applies schedules in a lambda, essentially by calling
--     @applySchedOnBody@
applySchedOnLambda :: Env -> Lambda SOACS -> ScheduleM (BotEnv, Lambda SOACS)
applySchedOnLambda env lam = do
  scope <- askScope
  (sched_env', bdy) <-
    localScope (scope <> scopeOf lam) $
      applySchedOnBody env $ lambdaBody lam
  pure (sched_env', lam { lambdaBody = bdy })

----------------------------------------
--- CORE FUNCTION is applySchedOnStm ---
----------------------------------------

-- | Core function that transforms a SOAC according to its
--     high-level schedule. In case the schedule modifies
--     the layout of the SOAC's result, it also performs the
--     index/layout changes in the rest of the code.
applySchedOnStm :: Env -> Stm SOACS -> ScheduleM (BotEnv, Stms SOACS)
---------------------------
--- Threatment of SOACS ---
---------------------------
-- A fusion schedule is treated later by a fusion pass;
--  currently, just adds the hoisted stms and the
--  fusion schedule arround the recursive codegen.
applySchedOnStm (env, chenv) soac_stm
  | Just (key_nm, entry) <- legalSchedForSoacStm chenv soac_stm,
    sched_kind entry == 1 = do
  let chenv' = chenv { chainTab = M.delete key_nm (chainTab chenv) }
  (sched_env, stms') <- applySchedOnStm (env, chenv') soac_stm
  pure (sched_env, hoist_stms entry <> stms' <> sched_stms entry)
--
-- The Core SOAC Case is a fully fused redomap
applySchedOnStm env soac_stm@(Let soac_pat aux e)
  | Op (Screma w inp_nms (ScremaForm map_lam [] reds)) <- e,
    Just (_, entry) <- legalSchedForSoacStm (chainEnv env) soac_stm = do
    -- 1. update the scalar environment with the hoisted stms
    scal_env' <- foldM updateTopdownEnv (scalrEnv env) $ hoist_stms entry
    -- 2. Apply the schedules inside the lambda
    (_sched_env_lam, map_lam') <- applySchedOnLambda ((scal_env', freshBotEnv), freshChainsEnv) map_lam
    let soac_stm' = Let soac_pat aux $ Op $ Screma w inp_nms (ScremaForm map_lam' [] reds)
    --
    -- 1. check the validity of the schedule; in addition find out
    --    whether the rescheduling result needs to be adjusted and
    --    the operator and neutral element for the manifested accumulation,
    --    i.e., the case of a generalized reduction hoisted out.
    (_valid, per_dim_kind, _mb_accum_lam_ne) <-
          checkValidSched scal_env' (schedule entry) soac_stm'
  
    let new_sched_stms = mkFirstSchedACpyStm $ sched_stms entry
    -- error "After LMAD Sanity, Terminating!"
    trace ("\nTop-Down Env:" ++
           "\n\tFwdTileTab: " ++ prettyString (M.toList $ appTilesFwd $ scalrEnv env) ++ 
           "\n\tInvTileTab: " ++ prettyString (M.toList $ appTilesInv $ scalrEnv env) ++
           "\n\tIotaTab: "    ++ prettyString (M.toList $ iotas $ scalrEnv env) ++
           "\n\n\nHL-Sched: " ++ prettyString (schedule entry) ++
           "\n\n\nHoist-Stms:\n" ++ prettyString (hoist_stms entry) ++
           "\n\n\nOld-Sched-Stms:\n" ++ prettyString (sched_stms entry) ++
           "\n\n\nNew-Sched-Stms: " ++ prettyString new_sched_stms ++
           "\n\n\nDim Kind: " ++ show per_dim_kind
          ) $ error "Terminating After LMAD Sanity !!!" --
            -- $ pure (schedEnv env, hoist_stms entry <> oneStm soac_stm' <> sched_stms entry)
{--
    --
    -- 2. perform the split of original dimnesions, according schedule's @dimlens@
    --    this refers to simple stripmining, as in the case of matrix multiplication,
    --    or to introducing redundant computation by means of overlapping lmads.
    (prologue, env', e_soac') <- applyStripmining scal_env' (schedule entry) soac_stm'
    -- 3. adjusting the result has 3 cases according to the @whatres@ field of the schedule:
    --    (a) @ExactRes@      -> nothing to be done
    --    (b) @ManifestRes@   -> generate accumulator code that manifest the original result
    --    (c) @SubstituteRes@ -> substitute the result of the rescheduled recurrence in the
    --                             rest of the code
    (_perm_patel, init_stms, withacc_stm) <-
        manifestResult env' (schedule entry) soac_pat
    -- 4. permute the nest
    permuted_res_stms <- applyPermute env' (schedule entry) aux e_soac'

    trace ("\nTop-Down Env:" ++
           "\n\tFwdTileTab: " ++ prettyString (M.toList $ appTilesFwd env') ++ 
           "\n\tInvTileTab: " ++ prettyString (M.toList $ appTilesInv env') ++
           "\n\tIotaTab: "    ++ prettyString (M.toList $ iotas env') ++
           "\n\n\nHL-Sched: "   ++ prettyString (schedule entry) ++
           "\n\n\nORIGINAL SOAC STMT:" ++
           "\nPattern: "    ++ prettyString soac_pat++" = "++
           "\n" ++ prettyString e ++
           "\n\n\nStripmined SOAC:\n"++ prettyString e_soac' ++
           "\n\n\nStripmined Prol:\n"++ prettyString prologue ++
           "\n\n\ninit-withacc:\n" ++ prettyString init_stms ++
           "\n\n\nManifestedRes:\n"++ prettyString withacc_stm ++
           "\n\n\nPermuted SOAC:\n"++ prettyString permuted_res_stms ++
           "\n\nDim Kind: " ++ show per_dim_kind
          ) $ error "Partially Transformed Schedule, Terminating!"
--}
applySchedOnStm env soac_stm@(Let _ _ (Op{}))
  | Just (_key_nm, _entry) <- legalSchedForSoacStm (chainEnv env) soac_stm =
  error $ "This SOACS is not yet supported for scheduling: " ++
            prettyString soac_stm
--
-- Treatment for WithAccs:
--   - if a schedule is applied to a non-accumulated result => Error
--   - if multiple schedules are applied to accumulator results => Error
--   - if the schedule stms is not exactly one schedule call => Error,
--       e.g., Rearranges are not permitted because a schedule for an
--       accumulated results will not change the shape of the result.
--   - Otherwise, fill in the ScalarEnv with the info of the hoisted stms,
--       create a ChainEnv to carry the outstanding schedule inside the
--       withAcc and apply the schedule in the WithAcc lambda.
--   - Since the shape of the accumulated result is preserved by the
--       schedule, no change of indexing needs to be propagated. 
applySchedOnStm env stm@(Let pat aux (WithAcc wacc_inps lam))
  | pat_nms <- map patElemName (patElems pat),
    lam_tps <- lambdaReturnType lam,
    (acc_pat_nms, _) <- unzip $ filter isAccType (zip pat_nms lam_tps),
    reg_nms <- namesSubtract (namesFromList pat_nms) (namesFromList acc_pat_nms),
    sched_tgt_nms <- namesFromList (M.keys $ chainTab $ chainEnv env),
    common_reg_nms <- namesIntersection sched_tgt_nms reg_nms,
    sched_active_nms <- filter (\ x -> nameIn x sched_tgt_nms) pat_nms,
    length sched_active_nms > 1,
    not (common_reg_nms == mempty) =
  error $ "Error In applySchedOnStm (WithAcc): scheduled name not an accumulator: " ++
          prettyString common_reg_nms ++ " or more than one active schedules: " ++
          prettyString sched_active_nms
  | pat_nms <- map patElemName (patElems pat),
    res_ses <- map resSubExp (bodyResult (lambdaBody lam)),
    sched_tgt_nms <- namesFromList (M.keys $ chainTab $ chainEnv env),
    [(sched_tgt_nm, res_nm)] <- mapMaybe (resInNames sched_tgt_nms) (zip pat_nms res_ses),
    -- ^ equiv with: nameIn sched_tgt_nm (namesFromList pat_nms),
    Just entry <- M.lookup sched_tgt_nm (chainTab (chainEnv env)),
    sched_kind entry > 1,
    checkSchedStms (sched_stms entry) = do
  let (_, new_tc_nms) = unzip $ mapMaybe (resInNames (sched_tc_syms entry)) $ zip pat_nms res_ses
      entry' = entry { sched_stms = mempty
                     , hoist_stms = mempty
                     , sched_tc_syms = namesFromList new_tc_nms
                     }
      chain_env =
        freshChainsEnv
          { chainTab = M.insert res_nm entry' mempty
          , active_sched_name = Just res_nm
          , names_used_so_far = mempty
          , sched_res_names = oneName res_nm
          }
  scal_env' <- foldM updateTopdownEnv (scalrEnv env) $ hoist_stms entry
  (sched_env', lam') <- applySchedOnLambda ((scal_env', schedEnv env), chain_env) lam
  let withacc_stm = Let pat aux $ WithAcc wacc_inps lam'
      new_sched_stms =
        if isNothing $ M.lookup res_nm $ soac_tab sched_env'
        then sched_stms entry
        else mkFirstSchedACpyStm $ sched_stms entry
  pure ( schedEnv env, (hoist_stms entry) <> oneStm withacc_stm <> new_sched_stms )
  -- ^ for an accumulator result, so no change-of-indexing is
  --   neccessary in the remaining code.
  where
    isAccType (_, Acc{}) = True
    isAccType _ = False
    --
    checkSchedStms stms
      | oneStm stm == stms = True
    checkSchedStms stms =
      error $ "Error: multiple scheduling stms for an accumulator result: "
               ++ prettyString stms
    --
    resInNames names (pat_nm, (Var res_nm))
      | nameIn pat_nm names = Just (pat_nm, res_nm)
    resInNames _ _ = Nothing
-- default treatment of WithAccs, i.e., no scheduling
applySchedOnStm env (Let pat aux e)
  | WithAcc wacc_inps lam <- e = do
  (_, lam') <- applySchedOnLambda env lam
  let stm' = Let pat aux $ WithAcc wacc_inps lam'
  pure (schedEnv env, oneStm stm')
--
-- Stms that are part of (or are used in) the schedule chain
--   are removed from their current location, i.e., will be hoisted.
applySchedOnStm env stm
  | Just tgt_sched_nm <- active_sched_name (chainEnv env),
    Just entry <- M.lookup tgt_sched_nm (chainTab $ chainEnv env),
    isJust (Sq.elemIndexL stm (hoist_stms entry)) ||
      isJust (Sq.elemIndexL stm (sched_stms entry)) =
  pure (schedEnv env, mempty)
--
-- treatment for If statements
applySchedOnStm env (Let pat aux e)
  | Match ses cases def_bdy matchdec <- e = do
  env_case_bodies <- forM cases $ \ c -> applySchedOnBody env $ caseBody c
  let (_, case_bodies) = unzip env_case_bodies
  (_, def_bdy') <- applySchedOnBody env def_bdy
  let cases' = zipWith (\c b -> c { caseBody = b } ) cases case_bodies 
      stm' = Let pat aux $ Match ses cases' def_bdy' matchdec
  pure (schedEnv env, oneStm stm')
-- treatment for loops
applySchedOnStm env (Let pat aux e)
  | Loop par_inis lform bdy <- e = do
  (_, bdy') <- applySchedOnBody env bdy
  let stm' = Let pat aux $ Loop par_inis lform bdy'
  pure (schedEnv env, oneStm stm')
-- treatment for Ops
applySchedOnStm env (Let pat aux (Op soac))
  | Screma w nms (ScremaForm map_lam scans reds) <- soac = do
    (_, map_lam') <- applySchedOnLambda env map_lam
    scans'   <- forM scans applySchedOnScan
    reds'    <- forM reds  applySchedOnReduce
    let stm' = Let pat aux $ Op $ Screma w nms $
                ScremaForm map_lam' scans' reds'
    pure (schedEnv env, oneStm stm')
  where
    applySchedOnScan (Scan lam ne) = do
      (_, lam') <- applySchedOnLambda env lam
      pure $ Scan lam' ne
    applySchedOnReduce (Reduce com lam ne) = do
      (_, lam') <- applySchedOnLambda env lam
      pure $ Reduce com lam' ne
applySchedOnStm env (Let pat aux (Op e))
  | Stream w nms ses lam <- e = do
  (_, lam') <- applySchedOnLambda env lam
  let stm' = Let pat aux $ Op $ Stream w nms ses lam'
  pure (schedEnv env, oneStm stm')
applySchedOnStm env (Let pat aux (Op e))
  | Scatter w nms spec lam <- e = do
  (_, lam') <- applySchedOnLambda env lam
  let stm' = Let pat aux $ Op $ Scatter w nms spec lam'
  pure (schedEnv env, oneStm stm')
applySchedOnStm env (Let pat aux (Op e))
  | Hist w nms bops lam <- e = do
  (_, lam') <- applySchedOnLambda env lam
  let stm' = Let pat aux $ Op $ Hist w nms bops lam'
  pure (schedEnv env, oneStm stm')
applySchedOnStm env (Let pat aux (Op e))
  | JVP ses1 ses2 lam <- e = do
  (_, lam') <- applySchedOnLambda env lam
  let stm' = Let pat aux $ Op $ JVP ses1 ses2 lam'
  pure (schedEnv env, oneStm stm')
applySchedOnStm env (Let pat aux (Op e))
  | VJP ses1 ses2 lam <- e = do
  (_, lam') <- applySchedOnLambda env lam
  let stm' = Let pat aux $ Op $ VJP ses1 ses2 lam'
  pure (schedEnv env, oneStm stm')
-- default case
applySchedOnStm env stm =
  pure (schedEnv env, oneStm stm)

--------------------------------
--- Various Helper Functions ---
--------------------------------

mkCopyStm :: Stm SOACS -> Stm SOACS
mkCopyStm stm@(Let _ _ e_sched)
  | Apply fnm _args_diet _rtp _ <- e_sched,
    schedKind (nameToString fnm) == 1 = stm
mkCopyStm (Let pat aux e_sched)
  | Apply fnm args_diet _rtp _ <- e_sched,
    schedKind (nameToString fnm) > 1,
    Var tgt_arr_nm : _ <- fst $ unzip $ reverse args_diet =
  Let pat aux $ BasicOp $ SubExp $ Var tgt_arr_nm
mkCopyStm stm@(Let _ _ (Apply{})) =
  error $ "In mkCopyStm, input stm is not a scheduling stm: " ++ prettyString stm
mkCopyStm (Let pat aux (BasicOp (Rearrange arr_nm _sigma))) =
  Let pat aux $ BasicOp $ SubExp $ Var arr_nm
mkCopyStm stm =
  error $ "In mkCopyStm, illegal input stm: " ++ prettyString stm

mkFirstSchedACpyStm :: Stms SOACS -> Stms SOACS
mkFirstSchedACpyStm stms =
  stmsFromList $ reverse $ snd $ foldl ff (0::Int, mempty) $ stmsToList stms
  where
  ff (0, acc_stms) stm@(Let _ _ (Apply fnm _args_diet _rtp _))
    | 1 == schedKind (nameToString fnm) = (2, stm : acc_stms)
    | 1 < schedKind (nameToString fnm)  = (1, mkCopyStm stm : acc_stms)
  ff (0, _) stm =
    error $ "In mkFirstSchedACpyStm, first stmt is not a non-fuse schedule: "
             ++ prettyString stm
  ff (1, acc_stms) stm@(Let _ _ (Apply{})) = (2, stm : acc_stms)
  ff (1, acc_stms) stm = (1, mkCopyStm stm : acc_stms)
  ff (_, acc_stms) stm = (2, stm : acc_stms)

legalSchedForSoacStm :: ChainsEnv -> Stm SOACS -> Maybe (VName, ChainEntry)
legalSchedForSoacStm env stm@(Let pat _ e)
  | Op _ <- e,
    pat_nms <- map patElemName (patElems pat),
    sched_tgt_nms <- namesFromList (M.keys $ chainTab env),
    sched_nm : sched_nms <- namesToList $ namesIntersection sched_tgt_nms (namesFromList pat_nms),
    Just entry <- M.lookup sched_nm (chainTab env) =
  if null sched_nms
  then Just (sched_nm, entry)
  else error $ "Only one-result SOACS can be currently rescheduled; target stm has several: " ++
               prettyString (sched_nm : sched_nms) ++ " stm: " ++ prettyString stm
legalSchedForSoacStm _ _ = Nothing

------------------------------
--- Commented Garbage Code ---
------------------------------

{--
applySchedOnStm :: Env -> Stm SOACS -> ScheduleM (BotEnv, Stms SOACS)
-- | Core function that transforms a SOAC according to its
--     high-level schedule. In case the schedule modifies
--     the layout of the SOAC's result, it also performs the
--     index/layout changes in the rest of the code.
applySchedOnStm :: Env -> Stm SOACS -> ScheduleM (BotEnv, Stms SOACS)
applySchedOnStm env stm@(Let pat _ _)
  | sched_env <- schedEnv env,
    pat_nms <- (namesFromList . map patElemName) (patElems pat),
    com_nms <- namesIntersection pat_nms (target_soac_names sched_env),
    tgt_nm : _ <- namesToList com_nms =
    pure ( sched_env { soac_tab = M.insert tgt_nm stm (soac_tab sched_env) }, mempty )
-- Codegen for rescheduled SOACs:

applySchedOnStm ((td_env, bu_env),_) sched_stm@(Let pat_sched _ e_sched)
  | Apply fnm args_diet _rtp _ <- e_sched,
    2 == schedKind (nameToString fnm),
    (args, _diet) <- unzip args_diet,
    (arr_nm, _pat_el, sched, _lmad) <- parseSchedule fnm td_env args pat_sched,
    Just soac_stm <- M.lookup arr_nm (soac_tab bu_env),
    Let soac_pat _aux e <- soac_stm,
    length (patElems soac_pat) == 1,
    Op (Screma _m _inp_nms form@(ScremaForm _map_lam [] reds )) <- e,
    null reds || isJust (oneFullyConsumedMapRed form) = do
    --
    -- 0. Don't forget to apply schedules on the lambda, and to fuse the code.
    --
    -- 1. check the validity of the schedule; in addition find out
    --    whether the rescheduling result needs to be adjusted and
    --    the operator and neutral element for the manifested accumulation,
    --    i.e., the case of a generalized reduction hoisted out.
    let (_valid, per_dim_kind, _mb_accum_lam_ne) = checkValidSched td_env sched soac_stm
    
    -- error "After LMAD Sanity, Terminating!"
    trace ("\nTop-Down Env:" ++
            "\n\tFwdTileTab: " ++ prettyString (M.toList $ appTilesFwd td_env) ++ 
            "\n\tInvTileTab: " ++ prettyString (M.toList $ appTilesInv td_env) ++
            "\n\tIotaTab: "    ++ prettyString (M.toList $ iotas td_env) ++
            "\n\n\nHL-Sched: " ++ prettyString sched ++
            "\n\n\nDim Kind: " ++ show per_dim_kind
          ) -- $ error "Terminating After LMAD Sanity !!!" --
            $ pure (bu_env, stmsFromList [soac_stm, sched_stm])
{--
    -- 2. perform the split of original dimnesions, according schedule's @dimlens@
    --    this refers to simple stripmining, as in the case of matrix multiplication,
    --    or to introducing redundant computation by means of overlapping lmads.
    (prologue, env', e_soac') <- applyStripmining td_env sched soac_stm
    -- 3. adjusting the result has 3 cases according to the @whatres@ field of the schedule:
    --    (a) @ExactRes@      -> nothing to be done
    --    (b) @ManifestRes@   -> generate accumulator code that manifest the original result
    --    (c) @SubstituteRes@ -> substitute the result of the rescheduled recurrence in the
    --                             rest of the code
    (_perm_patel, init_stms, withacc_stm) <-
        manifestResult env' sched $ pat_sched
    -- 4. permute the nest
    permuted_res_stms <- applyPermute env' sched aux e_soac'

    trace ("\nTop-Down Env:" ++
           "\n\tFwdTileTab: " ++ prettyString (M.toList $ appTilesFwd env') ++ 
           "\n\tInvTileTab: " ++ prettyString (M.toList $ appTilesInv env') ++
           "\n\tIotaTab: "    ++ prettyString (M.toList $ iotas env') ++
           "\n\n\nHL-Sched: "   ++ prettyString sched ++
           "\n\n\nORIGINAL SOAC STMT:" ++
           "\nPattern: "    ++ prettyString soac_pat++" = "++
           "\n" ++ prettyString e ++
           "\n\n\nStripmined SOAC:\n"++ prettyString e_soac' ++
           "\n\n\nStripmined Prol:\n"++ prettyString prologue ++
           "\n\n\ninit-withacc:\n" ++ prettyString init_stms ++
           "\n\n\nManifestedRes:\n"++ prettyString withacc_stm ++
           "\n\n\nPermuted SOAC:\n"++ prettyString permuted_res_stms
          ) $ error "Partially Transformed Schedule, Terminating!"
--} 
applySchedOnStm ((_, bu_env),_) (Let _pat _aux e_sched)
  | Apply fnm args_diet _rtp _ <- e_sched,
    schedKind (nameToString fnm) > 0,
    (args, _) <- unzip args_diet,
    Var arr_nm : _ <- reverse args,
    Just soac_stm <- M.lookup arr_nm (soac_tab bu_env) =
  error ("Implementation shortcomming: scheduling is supported on soacs"++
         " whose top reccurence is a map and producing one array result"++
         "; given stm target to scheduling is:\n:" ++ prettyString soac_stm )
--}

--
-------------------------------------------------------------------------
-- Core Implementation: case of a redomap soac target to a schedule!  ---
-------------------------------------------------------------------------
{--  
applySchedOnStm (td_env, bu_env) stm@(Let soac_pat aux e)
  | Op soac@(Screma _m _inp_nms form@(ScremaForm _map_lam [] reds )) <- e,
    [pat_el] <- patElems soac_pat,
    Just (patel_sched, sched, _lmad, _stms) <-
      M.lookup (patElemName pat_el) (schedules bu_env),
    null reds || isJust (oneFullyConsumedMapRed form) = do
    -- 0. Don't forget to apply schedules on the lambda, and to fuse the code.
    --
    -- 1. check the validity of the schedule; in addition find out
    --    whether the rescheduling result needs to be adjusted and
    --    the operator and neutral element for the manifested accumulation,
    --    i.e., the case of a generalized reduction hoisted out.
    let (_valid, per_dim_kind, _mb_accum_lam_ne) = checkValidSched td_env sched stm
    
    trace ("\nTop-Down Env:" ++
           "\n\tFwdTileTab: " ++ prettyString (M.toList $ appTilesFwd td_env) ++ 
           "\n\tInvTileTab: " ++ prettyString (M.toList $ appTilesInv td_env) ++
           "\n\tIotaTab: "    ++ prettyString (M.toList $ iotas td_env) ++
           "\n\n\nHL-Sched: " ++ prettyString sched ++
           "\n\n\nDim Kind: " ++ show per_dim_kind
--           "\n\n\nORIGINAL SOAC STMT:" ++
--           "\nPattern: "    ++ prettyString soac_pat++" = "++
--           "\n" ++ prettyString soac ++
--           "\n\n\nStripmined SOAC:\n"++ prettyString e_soac' ++
--           "\n\n\nStripmined Prol:\n"++ prettyString prologue ++
--           "\n\n\ninit-withacc:\n" ++ prettyString init_stms ++
--           "\n\n\nManifestedRes:\n"++ prettyString withacc_stm ++
--           "\n\n\nPermuted SOAC:\n"++ prettyString permuted_res_stms
          ) $ error "After LMAD Sanity, Terminating!" 
  
    -- 2. perform the split of original dimnesions, according schedule's @dimlens@
    --    this refers to simple stripmining, as in the case of matrix multiplication,
    --    or to introducing redundant computation by means of overlapping lmads.
    (prologue, env', e_soac') <- applyStripmining td_env sched stm
    -- 3. adjusting the result has 3 cases according to the @whatres@ field of the schedule:
    --    (a) @ExactRes@      -> nothing to be done
    --    (b) @ManifestRes@   -> generate accumulator code that manifest the original result
    --    (c) @SubstituteRes@ -> substitute the result of the rescheduled recurrence in the
    --                             rest of the code
    (_perm_patel, init_stms, withacc_stm) <-
        manifestResult env' sched $ Pat [patel_sched]
    -- 4. permute the nest
    permuted_res_stms <- applyPermute env' sched aux e_soac'

    trace ("\nTop-Down Env:" ++
           "\n\tFwdTileTab: " ++ prettyString (M.toList $ appTilesFwd env') ++ 
           "\n\tInvTileTab: " ++ prettyString (M.toList $ appTilesInv env') ++
           "\n\tIotaTab: "    ++ prettyString (M.toList $ iotas env') ++
           "\n\n\nHL-Sched: "   ++ prettyString sched ++
           "\n\n\nORIGINAL SOAC STMT:" ++
           "\nPattern: "    ++ prettyString soac_pat++" = "++
           "\n" ++ prettyString soac ++
           "\n\n\nStripmined SOAC:\n"++ prettyString e_soac' ++
           "\n\n\nStripmined Prol:\n"++ prettyString prologue ++
           "\n\n\ninit-withacc:\n" ++ prettyString init_stms ++
           "\n\n\nManifestedRes:\n"++ prettyString withacc_stm ++
           "\n\n\nPermuted SOAC:\n"++ prettyString permuted_res_stms
          ) $ error "Partially Transformed Schedule, Terminating!"
 
--    scope <- askScope
--    body' <- fuseInBody scope $ lambdaBody map_lam
--    error "unimplemented"
applySchedOnStm (_, bu_env) (Let pat _aux e)
  | any isJust $ map ((`M.lookup` (schedules bu_env)) . patElemName) $ patElems pat =
    error ("Implementation shortcomming: scheduling is supported on soacs"++
           " whose top reccurence is a map and producing one array result"++
           "; given expression target to scheduling is:\n:" ++ prettyString e )
--}

