-- | This module tries to switch the default allocation
--   of fast arrays from shared memory to registers.

module Futhark.Optimise.IntraShm2Reg
  ( applyShm2Reg ) where

import Control.Monad (forM)
import Data.Sequence qualified as Sq
import Futhark.IR.GPU
import Futhark.Pass
import Futhark.Optimise.IntraShm2Reg.SymTabs
import Futhark.Optimise.IntraShm2Reg.IntraBndAn
import Futhark.Optimise.IntraShm2Reg.CodeGen(fixRegKerResults)

{--
--import Control.Monad
import Control.Monad.Reader
import Control.Monad.State hiding (state)
import Control.Monad (forM)
import Data.List qualified as L
import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Sq
import Data.Maybe
import Futhark.Builder
import Futhark.IR.GPU
import Futhark.IR.GPU.Simplify (simplifyGPU)
-- import Futhark.Optimise.TileLoops.Shared
import Futhark.Pass
import Futhark.Tools
--import Futhark.Transform.Rename
--import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.SoP.Monad (AlgEnv (..), MonadSoP (..))
import Futhark.Optimise.IntraShm2Reg.OutBndAn
import Futhark.Optimise.IntraShm2Reg.SymTabs
--}
import Debug.Trace

-----------------------------------------
--- The Definition of the Pass
----------------------------------------

runShm2Reg :: TopEnv -> Scope GPU -> Stms GPU -> PassM (Stms GPU)
runShm2Reg env scope stms =
  runShm2RegM
    scope
    (Shm2RegEnv { vNameSource = mempty, algEnv = mempty })
    (shm2RegOnOutStms env stms)

applyShm2RegOnFun :: Stms GPU -> FunDef GPU -> PassM (FunDef GPU)
applyShm2RegOnFun consts fun = do
  let fun_bdy = funDefBody fun
      topenv  = freshTopEnv { bdy_res = map resSubExp $ bodyResult fun_bdy }
      scope   = scopeOf fun <> scopeOf consts
  fun_stms' <- runShm2Reg topenv scope $ bodyStms fun_bdy
  pure fun { funDefBody = (funDefBody fun) { bodyStms = fun_stms' } }

-- | The pass definition.
{-# NOINLINE applyShm2Reg #-}
applyShm2Reg :: Pass GPU GPU
applyShm2Reg =
  Pass
    { passName = "Enabling Register Mapping of Arrays in IntraGroup Kernels"
    , passDescription = "Attempts to change the default allocation of fast arrays in intra-group kernels from shared memory to registers."
    , passFunction = \ prog -> do
        -- trace ("\nProg before pass: "++prettyString prog++"\n") $
        intraproceduralTransformationWithConsts pure applyShm2RegOnFun prog
    }

--------------------------
--- The Outer Analysis ---
--------------------------

-- | Applies the analysis in a body of statements
shm2RegOnOutBody :: TopEnv -> Body GPU -> Shm2RegM (Body GPU)
shm2RegOnOutBody env body = do
  scope <- askScope
  stms' <- localScope (scope <> scopeOf (bodyStms body)) $
             shm2RegOnOutStms env $ bodyStms body
  pure $ body { bodyStms = stms' }

-- | Applies the analysis in a lambda, essentially by calling
--     @shm2RegOnOutBody@
applySchedOnOutLambda :: TopEnv -> Lambda GPU -> Shm2RegM (Lambda GPU)
applySchedOnOutLambda env lam = do
  scope <- askScope
  bdy <- localScope (scope <> scopeOf lam) $
           shm2RegOnOutBody env $ lambdaBody lam
  pure $ lam { lambdaBody = bdy }

-- | Applies the analysis on a sequence of statements
shm2RegOnOutStms :: TopEnv -> Stms GPU -> Shm2RegM (Stms GPU)
shm2RegOnOutStms _env Sq.Empty =
  pure mempty
shm2RegOnOutStms env (stm Sq.:<| stms) = do
  (env', stm') <- onOutStm env stm
  stms' <- shm2RegOnOutStms env' stms
  pure $ stm' Sq.:<| stms'

onOutStm :: TopEnv -> Stm GPU -> Shm2RegM (TopEnv, Stm GPU)
-- Core entrypoint refers to a statement denoting an
--   intra-group kernel: this is the target for analysis,
--   i.e., both for remapping arrays to registers and also
--         possibly for efficient sequentialization.
onOutStm env stm@( Let pat aux ( Op ( SegOp ( SegMap lvl space ts kbody ) ) ) )
  | (par_idxs, par_dims) <- unzip (unSegSpace space),
    -- length par_idxs == 1,
    SegBlock _virt (Just _grid) <- lvl,
    Body yyy stms kres <- kbody = do
    -- shouldSequentialize (stmAuxAttrs aux) = do
  let attrs = stmAuxAttrs aux
      par_dims_pe = map (peFromSe env (IntType Int64)) par_dims
      fvs_block = freeIn stm
      bu_env = freshBotEnv (zip3 par_idxs par_dims par_dims_pe) fvs_block
  let env_intra = (env, bu_env)
      ker_idxs_params = map (\nm -> Param mempty nm (Prim (IntType Int64))) par_idxs
  scope <- askScope
  kbody' <-
    localScope (scope <> scopeOfLParams ker_idxs_params <> scopeOf (bodyStms kbody)) $ do
      (bu_env', stms') <-
        --trace ("Attributes Intragroup Ker: " ++ prettyString attrs) $
          shm2RegOnIntraStms env_intra stms
      (kres_fix_stms, kres') <- fixRegKerResults bu_env' kres
      pure $ Body yyy (stms' <> kres_fix_stms) kres'
  pure (env, Let pat aux ( Op ( SegOp ( SegMap lvl space ts kbody' ) ) ) )
-- Loop
onOutStm env (Let pat aux e)
  | Loop fpar_ses loop_form loop_body <- e,
    (fpars, _init_ses) <- unzip fpar_ses = do
    scope <- askScope
    bdy'  <- localScope (scope <> scopeOfFParams fpars) $
              shm2RegOnOutBody env loop_body
    let e' = Loop fpar_ses loop_form bdy'
    pure (env, Let pat aux e')
-- WithAcc
onOutStm env (Let pat aux e)
  | WithAcc inps lam <- e = do
    lam' <- applySchedOnOutLambda env lam
    pure $ (env, Let pat aux (WithAcc inps lam'))
-- Ifs
onOutStm env (Let pat aux e)
  | Match ses cases def_bdy dec_match <- e = do
    cases' <- forM cases $ \ (Case cpat cbdy) -> do
      cbdy' <- shm2RegOnOutBody env cbdy
      pure $ Case cpat cbdy'
    def_bdy' <- shm2RegOnOutBody env def_bdy
    let e' = Match ses cases' def_bdy' dec_match
    pure (env, Let pat aux e')
-- any other outer statement possibly updates
--     the top-down environment
onOutStm env stm = do
  pure (updateTopdownEnv env stm, stm)

