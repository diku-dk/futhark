{-# LANGUAGE Strict #-}

-- | This module consists of performing a sequence of
--     code transformations that are defined by users
--     by means of high-level schedules.
--   ... more explanation is comming ...
module Futhark.Optimise.Fusion.HLsched
  ( applyHLsched
  )
where

import Data.List qualified as L
--import Control.Monad
import Data.Graph.Inductive.Graph qualified as G
--import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.HORep.SOAC qualified as H
import Futhark.Construct
import Futhark.IR.SOACS hiding (SOAC (..))
--import Futhark.IR.SOACS qualified as F
import Futhark.Optimise.Fusion.GraphRep
--import Futhark.Tools
--import Futhark.Transform.Rename
--import Futhark.Transform.Substitute
--import Futhark.Analysis.PrimExp
import Futhark.Optimise.Fusion.HLsched.Utils
--import Futhark.Util.Pretty hiding (line, sep, (</>))
-- import Futhark.Analysis.PrimExp.Convert

import Debug.Trace

-----------------------------------
--- core function
-----------------------------------

applyHLsched ::
  (HasScope SOACS m, MonadFreshNames m) =>
  DepNode ->
  DepGraph ->
  m (Maybe DepGraph)
applyHLsched node_to_fuse dg@DepGraph {dgGraph = g} = do
  mb_sched <- parseHLSched node_to_fuse dg
  case mb_sched of
    Just _  -> pure Nothing
    Nothing -> pure Nothing
{--  
    sched_nodeT <- snd node_to_fuse,
    sched_node_id <- nodeFromLNode node_to_fuse,
    StmNode stmt <- sched_nodeT,
    Let pat _aux e <- stmt,
    [pat_el] <- patElems pat, 
    Apply fnm args_diet _rtp _ <- e,
    (args, _diet) <- unzip args_diet,
    strides_arg : signals_arg : sigma_arg : orig_arg : ns_arg : offs : soac_res : _ <- L.reverse args,
    trace ("Debug HLSched: "++ prettyString fnm ++
           "\n\t res-arr:  "++ prettyString (patElemName pat_el) ++
           "\n\t res_type: "++ prettyString (patElemDec  pat_el) ++
           "\n\targs: "++prettyString args ++ 
           "\n\t stmt: "++prettyString stmt ++
           "\n\t soac res name: "++ prettyString soac_res
          ) True,
    --
    sched_ctx <- G.context g sched_node_id,
    (out_deps, _, _, inp_deps) <- sched_ctx,
    indrc_deps <- filter (isInd   . fst) inp_deps,
    indrc_ctxs <- map (G.context g . snd) indrc_deps,
    indrc_ndTs <- map getNodeTfromCtx indrc_ctxs,
    ([soac_nT], arrlit_nTs) <- L.partition isSOACNodeT indrc_ndTs,
    SoacNode node_out_trsfs soac_pat soac _soac_aux <- soac_nT,
    H.nullTransforms node_out_trsfs,
    [soac_patel] <- patElems soac_pat,
    soac_res == Var (patElemName soac_patel),
    stmts <- mapMaybe getStmNode arrlit_nTs,
    trace ("soac:\nlet " ++ prettyString soac_pat ++ " inp-deps: "++show inp_deps) True,
    trace ("other " ++ show (length stmts) ++ " stms:\n" ++ prettyString stmts) True
    -- trace("indrct-deps: "++show indrc_deps) True
    --trace("graph: "++show g) True
  = do
  -- scope <- askScope
  let env0 = Env mempty mempty
  (env, lmad_off_pe) <- expandScalar env0 dg inp_deps offs
  trace ("lmad offset is: "++ prettyString lmad_off_pe ++ 
         "\n\t FwdTileTab: " ++ prettyString (appTilesFwd env) ++ 
         "\n\t InvTileTab: " ++ prettyString (appTilesInv env)
        ) $
    pure Nothing
  where
    getNodeTfromCtx (_, _, nT, _) = nT

--
applyHLsched _ _ = do
  pure Nothing
--}

