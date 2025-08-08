{-# LANGUAGE Strict #-}

-- | This module performs the stripmining part
--     of a high-level schedule
module Futhark.Optimise.Fusion.HLsched.Permute
  ( permuteHSOAC
  )
where

import Control.Monad
--import Data.Graph.Inductive.Graph qualified as G
import Data.Sequence qualified as Sq
import Data.Map.Strict qualified as M
import Data.Set qualified as S
--import Data.Maybe
import Futhark.Analysis.HORep.SOAC qualified as H
import Futhark.Construct
import Futhark.IR.SOACS hiding (SOAC (..))
import Futhark.IR.SOACS qualified as F
--import Futhark.Optimise.Fusion.GraphRep
--import Futhark.Tools
--import Futhark.Transform.Rename
--import Futhark.Transform.Substitute
--import Futhark.Analysis.PrimExp
import Futhark.Transform.Rename (renameLambda)
import Futhark.IR.SOACS.Simplify (simplifyLambda)
import Futhark.Optimise.Fusion.HLsched.Env
import Futhark.Optimise.Fusion.HLsched.SchedUtils
--import Futhark.Util.Pretty hiding (line, sep, (</>))
import Futhark.Analysis.PrimExp.Convert
--import Futhark.Optimise.TileLoops.Shared

import Debug.Trace

-- | Fusion entrypoint for permuting the dimensions of a SOAC nest,
--     i.e., HORep.SOAC argument
permuteHSOAC ::
    (LocalScope SOACS m, MonadFreshNames m) =>
    FuseEnv m -> Env -> HLSched ->
    H.SOAC SOACS -> m (H.SOAC SOACS)
permuteHSOAC fenv env sched (H.Screma mm inps (H.ScremaForm map_lam [] reduces))
  | not (any hasTransform inps) = do
    let soac = F.Screma mm (map nameFromInp inps) $ F.ScremaForm map_lam [] reduces
    perm_soac <- permuteSOAC fenv 0 env sched soac
    trace ( "Stripmined soac:\n" ++ prettyString soac ++
            "Permuted   soac:\n" ++ prettyString perm_soac
          ) $
      pure perm_soac
    where
      hasTransform (H.Input transf _ _) = not $ H.nullTransforms transf
      nameFromInp (H.Input _ nm _) = nm
permuteHSOAC _ _ _ hsoac =
  error ("permuteHSOAC support only fully-fused map-reduce SOACs; given: "++prettyString hsoac)

-- | Assumptions:
--   1. The results of each intermediate recurrence in the nest is
--        either of scalar type or it constitutes the body result
--
--   ToDo: 
--   1. Add sanity checks, e.g., that the permutation length <= the depth of the soac nest
--
permuteSOAC ::
    (LocalScope SOACS m, MonadFreshNames m) =>
    FuseEnv m -> Env -> HLSched ->
    H.SOAC SOACS -> m (F.SOAC SOACS)
permuteSOAC fenv env sched soac
  | identityPerm (sigma sched) = pure soac
  -- ^ if the permutation is the identity, the task has been accomplished
  --   the empty list is considered identity as well
  --
  | [_] <- sigma sched = pure soac
  -- ^ one element schedule is always permuted in the right way
  --
  | not (null (sigma sched)) && 0 == head (sigma sched) = do
  -- ^ if the first element of the permutation is 0 then it reduces
  --   to permuting the last recurrence of the current soac. 
    let (code_bef, rec_stm, code_after, res_ses) = separateLastRec soac
        Let pat_rec aux_rec e_rec = rec_stm
        -- ^ check that either `pat_rec` is of scalar type or it is not used by code_after
        sched_tail = tailOfSched sched
        sched' = sched_tail { sigma = map (\x -> x- 1) (sigma sched_tail) }
    last_rec' <- permuteSOAC fenv env sched' last_rec
    mkSoacWithBody code_bef last_rec' code_after
    -- ^ this also need to fix the code_after to use the result of last_rec'
    --     which is potentially of a different shape now (?)
    --
  | s0 : s1 : _rest <- sigma sched,
    s0 < s1 = do
  -- ^ s0 cannot be directly interchanged. Treated in two steps:
  --   1. build a permutation for (s1:_rest) nest by subtracting 1 from each
  --        element greater than s0; call the resulted permutation `sigma_rec`
  --   2. recursively permute the (s1:_rest) according to `sigma_rec`
  --   3. place s0 at the correct position by recursively applying the
  --        permutation s0 ++ sort(s1:_rest) to the whole nest
    let (code_bef, rec_stm, code_after, res_ses) = separateLastRec soac
        Let pat_rec aux_rec e_rec = rec_stm
        sigma_rec = map (\x -> if x > s0 then x-1 else x) (sigma sched)
        sched_rec = (tailOfSched sched) { sigma = sigma_rec}
    soac_rec <- permuteSOAC fenv env sched last_rec
    let sched' = append2Sched (headOfSched sched) $ sortByPerm $ tailOfSched sched
    soac' <- mkSoacWithBody code_bef soac_rec code_after -- as before
    permuteSOAC fenv env sched' soac'
  --
  | s0 : s1 : _rest <- sigma sched,
    s0 > s1 = do
  -- ^ interchange the front dimensions, then call recursively on the top soac
    -- let (code_bef, last_rec, code_after) = separateLastRec soac
    soac' <- interchangeTopOfNest fenv env soac
    permuteSOAC fenv env (interchangeTop sched) soac'

--permuteSOAC fenv env sched (F.Screma mm inps (F.ScremaForm map_lam [] reduces)) = do
--  
--permuteSOAC _ _ _ soac =
--  error ("permuteSOAC support only fully-fused map-reduce SOACs; given: "++prettyString soac)