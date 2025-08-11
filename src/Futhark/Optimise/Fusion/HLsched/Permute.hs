{-# LANGUAGE Strict #-}

-- | This module performs the stripmining part
--     of a high-level schedule
module Futhark.Optimise.Fusion.HLsched.Permute
  ( permuteNest
  )
where

--import Control.Monad
--import Data.Graph.Inductive.Graph qualified as G
import Data.Sequence qualified as Sq
--import Data.Map.Strict qualified as M
--import Data.Set qualified as S
--import Data.Maybe
--import Futhark.Analysis.HORep.SOAC qualified as H
import Futhark.Construct
--import Futhark.IR.SOACS hiding (SOAC (..))
import Futhark.IR.SOACS
--import Futhark.IR.SOACS qualified as F
--import Futhark.Optimise.Fusion.GraphRep
--import Futhark.Tools
--import Futhark.Transform.Rename
--import Futhark.Transform.Substitute
--import Futhark.Analysis.PrimExp
--import Futhark.Transform.Rename (renameLambda)
--import Futhark.IR.SOACS.Simplify (simplifyLambda)
import Futhark.Optimise.Fusion.HLsched.Env
import Futhark.Optimise.Fusion.HLsched.SchedUtils
--import Futhark.Util.Pretty hiding (line, sep, (</>))
--import Futhark.Analysis.PrimExp.Convert
--import Futhark.Optimise.TileLoops.Shared

import Debug.Trace

-- | Assumptions:
--   1. The results of each intermediate recurrence in the nest is
--        either of scalar type or it constitutes the body result
--   2. 
--   ToDo: 
--   1. Add sanity checks, e.g., that the permutation length <= the depth of the soac nest
--
permuteNest ::
    (LocalScope SOACS m, MonadFreshNames m) =>
    FuseEnv m -> Env -> HLSched ->
    Stm SOACS -> m (Stms SOACS)
permuteNest fenv env sched stm_nest@(Let _pat _aux e_soac)
  | identityPerm (sigma sched) = trace ("Permute Base Case (Identity)") $ pure $ oneStm stm_nest
  -- ^ if the permutation is the identity, the task has been accomplished
  --   the empty list is considered identity as well
  --
  | [_] <- sigma sched = trace ("Permute Base Case (Size 1)") $ pure $ oneStm stm_nest
  -- ^ one element schedule is always permuted in the right way
  --
  | not (null (sigma sched)) && 0 == head (sigma sched),
  -- ^ if the first element of the permutation is 0 then it reduces
  --   to permuting the last recurrence of the current soac. 
    Just (code_bef, rec_stm, code_after, res_ses) <- separateLastRec e_soac = do
    -- Let _pat_rec _aux_rec e_rec <- rec_stm = do
    -- ^ check that either `_pat_rec` is of scalar type or it is not used by code_after
    let sched_tail = tailOfSched sched
        sched' = sched_tail { sigma = map (\x -> x - 1) (sigma sched_tail) }
    last_rec_stms <- trace ("Permute, Case head==0, tail-sched: "++show (sigma sched')++" orig_sched: "++show (sigma sched)) $
      permuteNest fenv env sched' rec_stm
    mkRecWithBody stm_nest (code_bef <> last_rec_stms <> code_after) res_ses >>= pure . oneStm
    -- ^ this also need to fix the code_after to use the result of last_rec'
    --     which is potentially of a different shape now (?)
    --
  | s0 : s1 : _rest <- sigma sched,
    s0 < s1,
    Just (code_bef, rec_stm, code_after, res_ses) <- separateLastRec e_soac = do
  -- ^ s0 cannot be directly interchanged. Treated in two steps:
  --   1. build a permutation for (s1:_rest) nest by subtracting 1 from each
  --        element greater than s0; call the resulted permutation `sigma_rec`
  --   2. recursively permute the (s1:_rest) according to `sigma_rec`
  --   3. place s0 at the correct position by recursively applying the
  --        permutation s0 ++ sort(s1:_rest) to the whole nest
    let tail_sched = tailOfSched sched
        sigma_rec = map (\x -> if x > s0 then x-1 else x) (sigma tail_sched)
        sched_rec = tail_sched { sigma = sigma_rec}
    last_rec_stms <- trace ("Permute Case 1.0, s0 < s1, rec-sched: "++show (sigma sched_rec)++" orig_sched: "++show (sigma sched)) $
      permuteNest fenv env sched_rec rec_stm
    let sched' = append2Sched (headOfSched sched) $ sortByPerm $ tailOfSched sched
    rec_stm' <- mkRecWithBody stm_nest (code_bef <> last_rec_stms <> code_after) res_ses
    trace ("Permute Case 1.1, s0 < s1, repositionaing first dim: "++show (sigma sched')++" orig_sched: "++show (sigma sched)++" sorted sched: "++show (sigma $ sortByPerm $ tailOfSched sched)) $
      permuteNest fenv env sched' rec_stm'
  --
  | s0 : s1 : _rest <- sigma sched,
    s0 > s1 = do
  -- ^ interchange the front dimensions, then call recursively on the top soac
    -- let (code_bef, last_rec, code_after) = separateLastRec soac
    (before_stms, last_rec_stm, after_stms) <- trace ("Permute Case 2.0, s0 > s1, transpose top"++" orig_sched: "++show (sigma sched)) $
      transposeTopOfNest fenv env sched stm_nest
    last_rec_stms' <- trace ("Permute Case 2.0, s0 > s1, recurse on sched: "++show (sigma (interchangeTop sched))++" orig_sched: "++show (sigma sched)) $
      permuteNest fenv env (interchangeTop sched) last_rec_stm
    pure $ before_stms <> last_rec_stms' <> after_stms
    -- ^ something is not OK here: how do you identify the result?
    where
      interchangeTop schd =
        append2Sched (headOfSched (tailOfSched schd)) $
        append2Sched (headOfSched schd) $
        (tailOfSched (tailOfSched schd))
permuteNest _ _ _ nest_stm =
  error ("Unsupported (nest) statement in permuteNest: "++prettyString nest_stm)

--permuteNest fenv env sched (F.Screma mm inps (F.ScremaForm map_lam [] reduces)) = do
--  
--permuteNest _ _ _ soac =
--  error ("permuteNest support only fully-fused map-reduce SOACs; given: "++prettyString soac)

-------------------------------------------------
--- Helper functions: so far unimplemented
-------------------------------------------------

separateLastRec :: Exp SOACS -> Maybe (Stms SOACS, Stm SOACS, Stms SOACS, [SubExp])
separateLastRec (Op (Screma _mm _inps (ScremaForm map_lam [] [])))
  | (code_bef Sq.:|> last_stm) <- bodyStms $ lambdaBody map_lam =
  Just (code_bef, last_stm, mempty, map resSubExp $ bodyResult $ lambdaBody map_lam)
separateLastRec e =
  error ("Unimplementated case for function separateLastRec in Permute.hs, expression: "++prettyString e)

mkRecWithBody :: (LocalScope SOACS m, MonadFreshNames m) =>
    Stm SOACS -> Stms SOACS -> [SubExp] -> m (Stm SOACS)
mkRecWithBody orig_nest_stm bdy_stms res_ses
  | Let pat aux (Op (Screma m inp_nms (ScremaForm map_lam [] []))) <- orig_nest_stm = do
  scope <- askScope
  map_lam' <- runLambdaBuilder (lambdaParams map_lam) $ localScope scope $ do
    addStms bdy_stms
    pure $ map subExpRes res_ses
--  new_bdy <- runBodyBuilder $ localScope scope $ do
--    addStms bdy_stms
--    pure res_ses
  pure $ Let pat aux $ Op $ Screma m inp_nms $ ScremaForm map_lam' [] []
--
mkRecWithBody orig_nest_stm _bdy_stms _res_ses =
  error ("Yet unsuported case for function mkRecWithBody in Permute.hs, stmt: "++prettyString orig_nest_stm)

transposeTopOfNest :: (LocalScope SOACS m, MonadFreshNames m) =>
    FuseEnv m -> Env -> HLSched -> Stm SOACS -> m (Stms SOACS, Stm SOACS, Stms SOACS)
transposeTopOfNest _fenv _env _sched orig_nest_stm@(Let _pat _ _) =
  pure (mempty, orig_nest_stm, mempty)
  -- map (Var . patElemName) $ patElems pat
