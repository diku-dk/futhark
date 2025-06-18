{-# LANGUAGE Strict #-}

-- | This module consists of performing a sequence of
--     code transformations that are defined by users
--     by means of high-level schedules.
--   ... more explanation is comming ...
module Futhark.Optimise.Fusion.HLsched
  ( applyHLsched
  )
where

import Control.Monad
import Data.Graph.Inductive.Graph qualified as G
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.HORep.SOAC qualified as H
import Futhark.Construct
import Futhark.IR.SOACS hiding (SOAC (..))
import Futhark.IR.SOACS qualified as F
import Futhark.Optimise.Fusion.GraphRep
import Futhark.Tools
import Futhark.Transform.Rename
import Futhark.Transform.Substitute

import Debug.Trace

applyHLsched ::
  (HasScope SOACS m, MonadFreshNames m) =>
  DepNode ->
  DepGraph ->
  m (Maybe DepGraph)
applyHLsched node_to_fuse dg@DepGraph {dgGraph = g}
  | soac_nodeT <- snd node_to_fuse,
    scat_node_id <- trace ("Debug1") $ nodeFromLNode node_to_fuse,
    StmNode stmt <- soac_nodeT,
    Let pat _aux e <- stmt,
    Apply fnm args rtp _ <- e,
    trace ("Debug HLSched: "++prettyString stmt) True
  = do
  pure Nothing
--
applyHLsched _ _ = do
  pure Nothing
