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
import Futhark.Builder
import Futhark.IR.SOACS
-- import Futhark.Optimise.TileLoops.Shared
import Futhark.Pass
import Futhark.Tools
--import Futhark.Transform.Rename
--import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.Optimise.Schedules.EnvUtils(TopEnv (..), freshTopEnv, addTileBinding, peFromSe)
import Futhark.Optimise.Schedules.SchedUtils

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
    env { iotas = M.insert pat_nm (w, pe_w) (iotas env) }
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
        sched_entry = (patElemName pat_el, sched, lmad, Sq.singleton stm)
    pure $ bu_env
      { optstms = stm Sq.<| (optstms bu_env)
      , schedules = M.insert (getVName arr) sched_entry $ schedules bu_env
      }
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
      | otherwise = error "something went wrong with tracking literals"
    getIntegerLit nm =
      map toInt $ getPrimExpLit nm
    --
    getAdjustRes (Var _) = error "Illegal var name denoting AdjustResult"
    getAdjustRes (Constant pval)
      | pval == IntValue (Int64Value 0) = ManifestRes
      | pval == IntValue (Int64Value 1) = SubstituteRes
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
--
-- Core Implementation: the case of a soac target to a schedule!
applySchedOnStm _ _ = error "unimplemented"    

