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
import Data.Maybe
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
--   2. Treat special functions such as pad1D as recurrences, e.g., in `separateLastRec`,
--        `mkRecWithBody`, `transposeTopOfNest`
--
permuteNest ::
    (LocalScope SOACS m, MonadFreshNames m) =>
    FuseEnv m -> Env -> HLSched ->
    Stm SOACS -> m (Stms SOACS)
permuteNest fenv env sched orig_nest_stm@(Let _pat _aux e_soac)
  | identityPerm (sigma sched) = trace ("Permute Base Case (Identity)") $ pure $ oneStm orig_nest_stm
  -- ^ if the permutation is the identity, the task has been accomplished
  --   the empty list is considered identity as well
  --
  | [_] <- sigma sched = trace ("Permute Base Case (Size 1)") $ pure $ oneStm orig_nest_stm
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
    mkRecWithBody orig_nest_stm (code_bef, last_rec_stms, code_after, res_ses) >>= pure . oneStm
    -- ^ this also need to fix the code_after to use the result of last_rec'
    --     which is potentially of a different shape now (?)
    --
  | s0 : s1 : _rest <- sigma sched,
    s0 < s1,
    Just (code_bef, rec_stm, code_after, res_ses) <- separateLastRec e_soac = do
  -- ^ s0 cannot be directly interchanged. Treated in two steps:
  --   1. build a permutation for (s1:_rest) nest by subtracting 1 from each
  --       element greater than s0; call the resulted permutation `sigma_rec`
  --   2. recursively permute the (s1:_rest) according to `sigma_rec`
  --   3. place s0 at the correct position by recursively applying the
  --        permutation s0 ++ sort(s1:_rest) to the whole nest
    let tail_sched = tailOfSched sched
        sigma_rec = map (\x -> if x > s0 then x-1 else x) (sigma tail_sched)
        sched_rec = tail_sched { sigma = sigma_rec}
    last_rec_stms <- trace ("Permute Case 1.0, s0 < s1, rec-sched: "++show (sigma sched_rec)++" orig_sched: "++show (sigma sched)) $
      permuteNest fenv env sched_rec rec_stm
    rec_stm' <- mkRecWithBody orig_nest_stm (code_bef, last_rec_stms, code_after, res_ses)
    --
    let sched' = append2Sched (headOfSched sched) $ sortByPerm $ tailOfSched sched
    trace ("Permute Case 1.1, s0 < s1, repositionaing first dim: "++show (sigma sched')++" orig_sched: "++show (sigma sched)++" sorted sched: "++show (sigma $ sortByPerm $ tailOfSched sched)) $
      permuteNest fenv env sched' rec_stm'
  --
  | s0 : s1 : _rest <- sigma sched,
    s0 > s1 = do
  -- ^ interchange the front dimensions, then call recursively on the top soac
    -- let (code_bef, last_rec, code_after) = separateLastRec soac
    (before_stms, last_rec_stm, after_stms) <- trace ("Permute Case 2.0, s0 > s1, transpose top"++" orig_sched: "++show (sigma sched)) $
      transposeTopOfNest fenv env sched orig_nest_stm
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

----------------------------------------------------------------
--- Simple Helper functions: separateLastRec & mkRecWithBody ---
----------------------------------------------------------------

separateLastRec :: Exp SOACS -> Maybe (Stms SOACS, Stm SOACS, Stms SOACS, [SubExp])
separateLastRec (Op (Screma _mm _inps (ScremaForm map_lam [] _reds))) =
  sepLastRecBody $ lambdaBody map_lam
separateLastRec (Loop _ ForLoop{} body ) =
  sepLastRecBody body
separateLastRec e =
  error ("Unimplementated case for function separateLastRec in Permute.hs, expression: "++prettyString e)

sepLastRecBody :: Body SOACS -> Maybe (Stms SOACS, Stm SOACS, Stms SOACS, [SubExp])
sepLastRecBody body
  | Sq.null (bodyStms body) = Nothing
  | True =
    case sepLastRecStms (Sq.reverse (bodyStms body)) of
      (bef, Just recc, aft) ->
        Just (bef, recc, aft, map resSubExp $ bodyResult body)
      _ -> Nothing
  where
    sepLastRecStms rev_stms =
      foldl processStm (mempty, Nothing, mempty) rev_stms
    processStm (bef, mrec, aft) curr_stm
      | isJust mrec = (curr_stm Sq.<| bef, mrec, aft)
      | isNothing mrec && isSupportedRec curr_stm =
          (bef, Just curr_stm, aft)
      | True = -- isNothing mrec && not (isSupportedRec curr_stm) =
          (bef, Nothing, curr_stm Sq.<| aft)
    isSupportedRec (Let _ _ (Loop _ ForLoop{} _)) = True
    isSupportedRec (Let _ _ (Op (Screma _ _ (ScremaForm _ [] [])))) = True
    isSupportedRec (Let _ _ (Op (Screma _ _ form))) =
      isJust $ oneFullyConsumedMapRed form
    isSupportedRec _ = False
  
-- | This is supposed to make a body from `stms_bef`, `new_stms_rec` and
--     `stms_aft`, where `new_stms_rec` are the statements generated by
--     a `permuteNest` call on an inner recurrence `stm_rec`.
--   The current implementation is WRONG because it does not adjust
--     `stms_aft` and the body result to the type of the permuted nest.
--   To fix: pass also the schedule as an argument, or at least the
--     permutation of the schdule and perform the necessary substitutions
--     in `stms_aft` and the body result.
--   
mkRecWithBody :: (LocalScope SOACS m, MonadFreshNames m) =>
    Stm SOACS -> (Stms SOACS, Stms SOACS, Stms SOACS, [SubExp]) -> m (Stm SOACS)
mkRecWithBody orig_nest_stm (stms_bef, stms_perm_rec, stms_aft, res_ses) -- body_stms
  | Let pat aux (Op (Screma m inp_nms (ScremaForm map_lam [] reduces))) <- orig_nest_stm = do
  scope <- askScope
  map_lam' <- runLambdaBuilder (lambdaParams map_lam) $ localScope scope $ do
    addStms (stms_bef <> stms_perm_rec <> stms_aft); pure $ map subExpRes res_ses
  pure $ Let pat aux $ Op $ Screma m inp_nms $ ScremaForm map_lam' [] reduces
mkRecWithBody orig_nest_stm (stms_bef, stms_perm_rec, stms_aft, res_ses)
  | Let pat aux (Loop par_inis (ForLoop i inttp mm) _body) <- orig_nest_stm,
    loop_pars <- map fromFParam (fst (unzip par_inis)) = do
  let i_param = Param (Attrs mempty) i $ Prim $ IntType inttp
  scope <- askScope
  loop_body' <- runBodyBuilder $ localScope (scope <> scopeOfLParams (i_param:loop_pars))$ do
    addStms (stms_bef <> stms_perm_rec <> stms_aft); pure $ map subExpRes res_ses
  pure $ Let pat aux $ Loop par_inis (ForLoop i inttp mm) loop_body'
--
mkRecWithBody orig_nest_stm _ =
  error ("Yet unsuported case for function mkRecWithBody in Permute.hs, stmt: "++prettyString orig_nest_stm)

------------------------------------------------------------------
--- Core Function that performs one transposition
------------------------------------------------------------------

transposeTopOfNest :: (LocalScope SOACS m, MonadFreshNames m) =>
    FuseEnv m -> Env -> HLSched -> Stm SOACS -> m (Stms SOACS, Stm SOACS, Stms SOACS)
transposeTopOfNest _fenv _env _sched orig_nest_stm
  | Let _pat _aux (Op (Screma _m _inp_nms (ScremaForm map_lam [] _reduces))) <- orig_nest_stm,
    Just (stms_bef, rec_stm, stms_aft, res_ses) <- sepLastRecBody $ lambdaBody map_lam = do
    pure (mempty, orig_nest_stm, mempty)
  -- map (Var . patElemName) $ patElems pat
transposeTopOfNest _fenv _env _sched orig_nest_stm =
  error ("Yet unsuported case for function transposeTopOfNest in Permute.hs, stmt: "++prettyString orig_nest_stm)