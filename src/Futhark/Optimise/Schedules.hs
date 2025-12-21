{-# LANGUAGE TypeFamilies #-}

-- | This module consists of performing a sequence of
--     code transformations that are defined by users
--     by means of high-level schedules.
--   ... more explanation is comming ...

module Futhark.Optimise.Schedules
  ( applySchedules ) where

--import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Sq
import Data.Maybe
import Futhark.Builder
import Futhark.IR.SOACS
-- import Futhark.Optimise.TileLoops.Shared
import Futhark.Pass
import Futhark.Tools
--import Futhark.Transform.Rename
--import Futhark.Analysis.PrimExp.Convert
import Futhark.Optimise.Fusion(fuseInBody)
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.Optimise.Schedules.EnvUtils
  (TopEnv (..), freshTopEnv, addTileBinding, addIota2Env, peFromSe)
import Futhark.Optimise.Schedules.SchedUtils
  ( HLSched(..), BotEnv(..), AdjustRes(..), freshBotEnv )
import Futhark.Optimise.Schedules.Safety(checkValidSched)
import Futhark.Optimise.Schedules.Stripmine(applyStripmining)
import Futhark.Optimise.Schedules.Permute(applyPermute)
import Futhark.Optimise.Schedules.AdjustResult(manifestResult)

import Debug.Trace

----------------------------------------
--- Environments
----------------------------------------

-- type SchedM = ReaderT (Scope SOACS) (State VNameSource)

type Env = (TopEnv, BotEnv)

-----------------------------------------
--- The Definition of the Pass
----------------------------------------

runSchedules :: Env -> Scope SOACS -> Stms SOACS -> PassM (Stms SOACS)
runSchedules env scope stms =
  modifyNameSource $
    runState $
      runReaderT (applySchedOnStms env stms) scope

applySchedOnFun :: Stms SOACS -> FunDef SOACS -> PassM (FunDef SOACS)
applySchedOnFun consts fun = do
  let fun_bdy = funDefBody fun
      topenv  = freshTopEnv { bdy_res = map resSubExp $ bodyResult fun_bdy }
      scope   = scopeOf fun <> scopeOf consts
  fun_stms' <- runSchedules (topenv, freshBotEnv) scope $ bodyStms fun_bdy
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

-- | Traversal structure
applySchedOnStms :: (LocalScope SOACS m, MonadFreshNames m) =>
  Env -> Stms SOACS -> m (Stms SOACS)
applySchedOnStms env stmts = do
  bu_env <- traverseStms env stmts
  pure $ optstms bu_env
  where
    traverseStms (_, bu_env) Sq.Empty = pure bu_env
    traverseStms (td_env, bu_env) (stm Sq.:<| stms) = do
      -- Compute @td_env@ top down
      let td_env' = updateTopdownEnv td_env stm
      -- Compute @bu_env@ bottom up
      bu_env' <- traverseStms (td_env', bu_env) stms
      applySchedOnStm (td_env', bu_env') stm

-- | The top-down pass records the scalar expansion, iotas,
--     tiling calls and the like.
--   ToDos:
--     1. maybe record also the change-of-layout transformations
--        into an LMAD, i.e., in the @arrTransf@ filed of @TopEnv@
updateTopdownEnv :: TopEnv -> Stm SOACS -> TopEnv
updateTopdownEnv env (Let pat _aux (Apply fnm arg_diets _ _))
  | any (`L.isPrefixOf` (nameToString fnm)) tileFuns,
    [(size_se, _)] <- arg_diets,
    ptp:_ <- map (elemType . patElemDec)  $ patElems pat =
  addTileBinding env (peFromSe env ptp size_se) $
    map patElemName $ patElems pat
  where
    tileFuns = ["strip1", "strip2"]
updateTopdownEnv env (Let (Pat [PatElem pat_nm pat_tp]) _aux e)
  | BasicOp (Iota w start stride Int64) <- e,
    start == se0 && stride == se1,
    pe_w <- peFromSe env i64ptp w =
    addIota2Env env (pat_nm, w, pe_w)
  | BasicOp (BinOp bop s1 s2) <- e,
    Prim ptp <- pat_tp,
    (p1, p2) <- (peFromSe env ptp s1, peFromSe env ptp s2) =
    env { scalars = M.insert pat_nm (BinOpExp bop p1 p2) (scalars env) }
  | BasicOp (UnOp unop se) <- e,
    Prim ptp <- pat_tp,
    pe <- peFromSe env ptp se =
    env { scalars = M.insert pat_nm (UnOpExp unop pe) (scalars env) }
  | BasicOp (ArrayLit arg_ses (Prim ptp)) <- e =
    env { arrayLits = M.insert pat_nm (map (peFromSe env ptp) arg_ses) (arrayLits env) }
  | BasicOp (Scratch _ptp [se]) <- e,
    se == se0 =
    env { arrayLits = M.insert pat_nm [] (arrayLits env) }
  | BasicOp (Replicate shp se) <- e,
    [d] <- shapeDims shp,
    Constant (IntValue (Int64Value n)) <- d =
    let pe_se = peFromSe env (elemType pat_tp) se
        arr_pe_se = replicate (fromIntegral n) pe_se
    in  env { arrayLits = M.insert pat_nm arr_pe_se (arrayLits env) }
  | otherwise = env
  where
    se0 = Constant $ IntValue $ Int64Value 0
    se1 = Constant $ IntValue $ Int64Value 1
    i64ptp = IntType Int64
updateTopdownEnv env _ = env

-- | Core function:
--    1. it identifies and records schedule calls
--    2. transforms SOACS matching existing schedules
applySchedOnStm :: (LocalScope SOACS m, MonadFreshNames m) =>
  Env -> Stm SOACS -> m BotEnv
applySchedOnStm (td_env, bu_env) stm@(Let pat _aux e)
  | Apply fnm args_diet _rtp _ <- e,
    L.isPrefixOf "hlSched2D" $ nameToString fnm,
    [pat_el] <- patElems pat,
    Array _ptp shp _ <- patElemDec pat_el,
    (args, _diet) <- unzip args_diet,
    arr : fuse : pad : fps : ms_res : virt : strds : sigs : perm : oinds : ns : _ <- L.reverse args = do
    let sched =
          HLS { dimlens = getPrimExpLit ns
              , origids = getIntegerLit oinds
              , sigma   = getIntegerLit perm
              , signals = getIntegerLit sigs
              , strides = getPrimExpLit strds
              , virthds = getPrimExpLit virt
              , whatres = getAdjustRes ms_res
              , permres = getIntegerLit fps
              , padinner= getPrimExpLit pad
              , fuselevs= getIntegerLit fuse
              }
        shp_pes = map (isInt64 . peFromSe td_env (IntType Int64)) $ shapeDims shp 
        lmad = LMAD.iotaStrided (isInt64 pe0) (isInt64 pe1) shp_pes
        sched_entry = trace ("\n\nFOUND SCHEDULE: "++prettyString sched) (pat_el, sched, lmad, Sq.singleton stm)
    pure $ bu_env
      { optstms = stm Sq.<| (optstms bu_env)
      , schedules = M.insert (getVName arr) sched_entry $ schedules bu_env
      }
    -- ^ much of this code should be moved as a helper function in SchedUtils
  where
    pe0 = ValueExp $ IntValue $ Int64Value 0
    pe1 = ValueExp $ IntValue $ Int64Value 1
    --
    getVName (Var nm) = nm
    getVName se = error ("Trying to read the soac-result name, but got: "++prettyString se)
    --
    getPrimExpLit (Constant _) =
      error ("The result of an array literal cannot be a constant")
    getPrimExpLit (Var nm)
      | Just pes <- M.lookup nm (arrayLits td_env) = pes
      | otherwise = error ("something went wrong with tracking literals of " ++ prettyString nm)
    getIntegerLit nm =
      map toInt $ getPrimExpLit nm
    --
    getAdjustRes (Var _) = error "Illegal var name denoting AdjustResult"
    getAdjustRes (Constant pval)
      | pval == IntValue (Int64Value 0) = ExactRes
      | pval == IntValue (Int64Value 1) = ManifestRes
      | pval == IntValue (Int64Value 2) = SubstituteRes
      | otherwise = error "Illegal integer for the kind of AdjustResult"
    --
    toInt :: PrimExp VName -> Int
    toInt (ValueExp (IntValue iv)) = valueIntegral iv
    toInt pe = error ("This was supposed to be a constant int value; instead is "++prettyString pe)
--
-- The case of transposition (just for demo)
applySchedOnStm (_, bu_env) stm@(Let (Pat [pat_el]) _aux e)
  | BasicOp (Rearrange soac_nm perm) <- e,
    pat_nm <- patElemName pat_el,
    Just (schd_nm, sched, lmad, sched_stms) <- M.lookup pat_nm (schedules bu_env),
    stm' Sq.:<| Sq.Empty <- Sq.filter (nameIn pat_nm . freeIn) $ optstms bu_env,
    Just _ <- Sq.elemIndexL stm' sched_stms = do
    let lmad' = LMAD.permute lmad perm
        sched_entry = (schd_nm, sched, lmad', stm Sq.<| sched_stms)
    pure $ bu_env 
      { optstms = stm Sq.<| (optstms bu_env)
      , schedules = M.insert soac_nm sched_entry $ M.delete pat_nm $ schedules bu_env
      }
-------------------------------------------------------------------------
-- Core Implementation: the case of a soac target to a schedule!
-------------------------------------------------------------------------
applySchedOnStm (td_env, bu_env) stm@(Let soac_pat aux e)
  | Op soac@(Screma _m _inp_nms (ScremaForm _map_lam [] [] )) <- e,
    [pat_el] <- patElems soac_pat,
    Just (patel_sched, sched, _lmad, _stms) <-
      M.lookup (patElemName pat_el) (schedules bu_env) = do
    -- 0. Don't forget to apply schedules on the lambda, and to fuse the code.
    --
    -- 1. check the validity of the schedule; in addition find out
    --    whether the rescheduling result needs to be adjusted and
    --    the operator and neutral element for the manifested accumulation,
    --    i.e., the case of a generalized reduction hoisted out.
    let (_valid, _mb_accum_lam_ne) = checkValidSched sched $ Op soac
    -- 2. perform the split of original dimnesions, according schedule's @dimlens@
    --    this refers to simple stripmining, as in the case of matrix multiplication,
    --    or to introducing redundant computation by means of overlapping lmads.
    (prologue, env', soac') <- applyStripmining td_env sched stm
    -- 3. adjusting the result has 3 cases according to the @whatres@ field of the schedule:
    --    (a) @ExactRes@      -> nothing to be done
    --    (b) @ManifestRes@   -> generate accumulator code that manifest the original result
    --    (c) @SubstituteRes@ -> substitute the result of the rescheduled recurrence in the
    --                             rest of the code
    (_perm_patel, init_stms, withacc_stm) <- trace ("Stripmined succeeds!!!") $
        manifestResult env' sched $ Pat [patel_sched]
    -- 4. permute the nest
    permuted_res_stms <- trace ("AdjustResult succeeds!!!") $
        applyPermute env' sched aux soac'

    trace ("\nTop-Down Env:" ++
           "\n\tFwdTileTab: " ++ prettyString (M.toList $ appTilesFwd env') ++ 
           "\n\tInvTileTab: " ++ prettyString (M.toList $ appTilesInv env') ++
           "\n\tIotaTab: "    ++ prettyString (M.toList $ iotas env') ++
           "\n\n\nHL-Sched: "   ++ prettyString sched ++
           "\n\n\nORIGINAL SOAC STMT:" ++
           "\nPattern: "    ++ prettyString soac_pat++" = "++
           "\n" ++ prettyString soac ++
           "\n\n\nStripmined SOAC:\n"++ prettyString soac' ++
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
-- Just (env0, soac_node@(_, SoacNode out_trsfs soac_pat soac soac_aux), sched, patel_sched)
applySchedOnStm (_, bu_env) stm =
  pure $ bu_env { optstms = stm Sq.<| (optstms bu_env) }

