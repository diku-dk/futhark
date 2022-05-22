{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

-- | Perform horizontal and vertical fusion of SOACs.  See the paper
-- /A T2 Graph-Reduction Approach To Fusion/ for the basic idea (some
-- extensions discussed in /Design and GPGPU Performance of Futhark’s
-- Redomap Construct/).
module Futhark.Optimise.Fusion (fuseSOACs) where

import Control.Monad.State
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.DFS as Q
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace
import qualified Futhark.Analysis.Alias as Alias
import qualified Futhark.Analysis.HORep.SOAC as H
import Futhark.Construct
import Futhark.IR.Prop.Aliases
import Futhark.IR.SOACS hiding (SOAC (..))
import qualified Futhark.IR.SOACS as Futhark
import Futhark.Optimise.Fusion.GraphRep
import Futhark.Optimise.Fusion.LoopKernel (mergeForms, pullOutputTransforms, pushRearrange, tryFusion)
import Futhark.Pass
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util (splitAt3)

-- extra util - scans reduces are "a->a->a" - so half of those are the amount of inputs
scanInput :: [Scan SOACS] -> Int
scanInput l = flip div 2 $ sum (map (length . lambdaParams . scanLambda) l)

redInput :: [Reduce rep] -> Int
redInput l = flip div 2 $ sum (map (length . lambdaParams . redLambda) l)

doFuseScans :: FusionEnvM a -> FusionEnvM a
doFuseScans m = do
  fs <- gets fuseScans
  modify (\s -> s {fuseScans = True})
  r <- m
  modify (\s -> s {fuseScans = fs})
  pure r

dontFuseScans :: FusionEnvM a -> FusionEnvM a
dontFuseScans m = do
  fs <- gets fuseScans
  modify (\s -> s {fuseScans = False})
  r <- m
  modify (\s -> s {fuseScans = fs})
  pure r

-- lazy version of fuse graph - removes inputs from the graph that are not arrays
fuseGraphLZ :: [Stm SOACS] -> Result -> [Param DeclType] -> FusionEnvM [Stm SOACS]
fuseGraphLZ stms results inputs = fuseGraph stms resNames inputNames
  where
    resNames = namesFromRes results
    inputNames = map paramName $ filter isArray inputs

-- main fusion function.
fuseGraph :: [Stm SOACS] -> [VName] -> [VName] -> FusionEnvM [Stm SOACS]
fuseGraph stms results inputs = localScope (scopeOf stms) $ do
  old_mappings <- gets producerMapping
  graph_not_fused <- mkDepGraph stms results inputs

  let graph_not_fused' = trace (pprg graph_not_fused) graph_not_fused
  graph_fused <- doAllFusion graph_not_fused'
  let graph_fused' = trace (pprg graph_fused) graph_fused

  stms_new <- linearizeGraph graph_fused'
  modify (\s -> s {producerMapping = old_mappings})
  pure $ trace (unlines (map pretty stms_new)) stms_new

unreachableEitherDir :: DepGraph -> G.Node -> G.Node -> FusionEnvM Bool
unreachableEitherDir g a b = do
  b1 <- reachable g a b
  b2 <- reachable g b a
  pure $ not (b1 || b2)

reachable :: DepGraph -> G.Node -> G.Node -> FusionEnvM Bool
reachable g source target = pure $ target `elem` Q.reachable source g

linearizeGraph :: DepGraph -> FusionEnvM [Stm SOACS]
linearizeGraph g = do
  stms <- mapM finalizeNode $ reverse $ Q.topsort' g
  pure $ concat stms

doAllFusion :: DepGraphAug
doAllFusion =
  applyAugs
    [ keepTrying doVerticalFusion,
      doHorizontalFusion,
      removeUnusedOutputs,
      makeCopiesOfConsAliased,
      runInnerFusion
    ]

doVerticalFusion :: DepGraphAug
doVerticalFusion g = applyAugs (map tryFuseNodeInGraph $ G.labNodes g) g

doHorizontalFusion :: DepGraphAug
doHorizontalFusion g = applyAugs (map horizontalFusionOnNode (G.nodes g)) g

-- for each node, find what came before, attempt to fuse
horizontalFusionOnNode :: G.Node -> DepGraphAug
horizontalFusionOnNode node g = tryFuseAll incoming_nodes g
  where
    (incoming_nodes, _) = unzip $ filter (isDep . snd) $ G.lpre g node

tryFuseAll :: [G.Node] -> DepGraphAug
tryFuseAll nodes_list = applyAugs (map (uncurry hTryFuseNodesInGraph) pairs)
  where
    pairs = [(x, y) | x <- nodes_list, y <- nodes_list, x < y]

vFusionFeasability :: DepGraph -> G.Node -> G.Node -> FusionEnvM Bool
vFusionFeasability g n1 n2 =
  do
    let b2 = not (any isInf (edgesBetween g n1 n2))
    reach <- mapM (reachable g n2) (filter (/= n2) (G.pre g n1))
    pure $ b2 && all not reach

hFusionFeasability :: DepGraph -> G.Node -> G.Node -> FusionEnvM Bool
hFusionFeasability = unreachableEitherDir

tryFuseNodeInGraph :: DepNode -> DepGraphAug
tryFuseNodeInGraph node_to_fuse g =
  if G.gelem node_to_fuse_id g
    then applyAugs (map (vTryFuseNodesInGraph node_to_fuse_id) fuses_with) g
    else pure g
  where
    fuses_with = map fst $ filter (isDep . snd) $ G.lpre g (nodeFromLNode node_to_fuse)
    node_to_fuse_id = nodeFromLNode node_to_fuse

vTryFuseNodesInGraph :: G.Node -> G.Node -> DepGraphAug
-- find the neighbors -> verify that fusion causes no cycles -> fuse
vTryFuseNodesInGraph node_1 node_2 g
  | not (G.gelem node_1 g && G.gelem node_2 g) = pure g
  | otherwise =
      do
        b <- vFusionFeasability g node_1 node_2
        if b
          then do
            let (ctx1, ctx2) = mapT (G.context g) (node_1, node_2)
            fres <- vFuseContexts edgs infusable_nodes ctx1 ctx2
            case fres of
              Just (inputs, _, nodeT, outputs) -> do
                nodeT' <-
                  if null fusedC
                    then pure nodeT
                    else do
                      let (_, _, _, deps_1) = ctx1
                      let (_, _, _, deps_2) = ctx2
                      -- make copies of everything that was not previously consumed
                      let old_cons = map (getName . fst) $ filter (isCons . fst) (deps_1 <> deps_2)
                      makeCopiesOfFusedExcept old_cons nodeT
                g' <- contractEdge node_2 (inputs, node_1, nodeT', outputs) g
                if null trEdgs
                  then pure g'
                  else updateTrEdges node_1 g'
              Nothing -> pure g
          else pure g
  where
    edgs = map G.edgeLabel $ edgesBetween g node_1 node_2
    fusedC = map getName $ filter isCons edgs
    trEdgs = map getName $ filter isTrDep edgs
    infusable_nodes =
      map
        depsFromEdge
        (concatMap (edgesBetween g node_1) (filter (/= node_2) $ G.pre g node_1))

hTryFuseNodesInGraph :: G.Node -> G.Node -> DepGraphAug
hTryFuseNodesInGraph node_1 node_2 g
  | not (G.gelem node_1 g && G.gelem node_2 g) = pure g
  | otherwise = do
      b <- hFusionFeasability g node_1 node_2
      fres <- hFuseContexts (G.context g node_1) (G.context g node_2)
      if b
        then case fres of
          Just new_Context -> contractEdge node_2 new_Context g
          Nothing -> pure g
        else pure g

hFuseContexts :: DepContext -> DepContext -> FusionEnvM (Maybe DepContext)
hFuseContexts
  c1@(_, _, nodeT1, _)
  c2@(_, _, nodeT2, _) =
    do
      fres <- hFuseNodeT nodeT1 nodeT2
      case fres of
        Just nodeT -> pure $ Just (mergedContext nodeT c1 c2)
        Nothing -> pure Nothing
hFuseContexts _ _ = pure Nothing

vFuseContexts :: [EdgeT] -> [VName] -> DepContext -> DepContext -> FusionEnvM (Maybe DepContext)
vFuseContexts
  edgs
  infusable
  c1@(_i1, n1, nodeT1, o1)
  c2@(_i2, _n2, nodeT2, o2) =
    do
      fres <- fuseNodeT edgs infusable (nodeT1, map fst o1) (nodeT2, map fst $ filter ((/=) n1 . snd) o2)
      case fres of
        Just nodeT -> pure $ Just (mergedContext nodeT c1 c2)
        Nothing -> pure Nothing

pullRearrangeNodeT :: H.ArrayTransforms -> NodeT -> FusionEnvM (Maybe NodeT)
pullRearrangeNodeT ts nodeT = case nodeT of
  SoacNode soac outputs aux -> do
    scope <- askScope
    -- note: tryFusion does not actually do any fusion - its just a monad-runner
    maybeSoac <- tryFusion (pullOutputTransforms soac ts) scope
    case maybeSoac of
      -- plausible source of bugs
      Just (s2, _ts) -> pure $ Just $ SoacNode s2 (map (H.addInitialTransforms ts) outputs) aux
      _ -> pure Nothing
  _ -> pure Nothing

pushRearrangeNodeT :: H.ArrayTransforms -> NodeT -> FusionEnvM (Maybe NodeT)
pushRearrangeNodeT trs nodeT = case nodeT of
  SoacNode soac outputs aux -> do
    scope <- askScope
    -- possible bug: only 1-1 fusion
    let soac' = trace (show trs) $ H.setInputs (map (internalizeOutput . H.setInputTransforms trs) (H.inputs soac)) soac
    maybeSoac <- tryFusion (pushRearrange (map H.inputArray (H.inputs soac)) soac' H.noTransforms) scope
    case maybeSoac of
      Just (s2, ts)
        | all (H.nullTransforms . H.inputTransforms) (H.inputs s2) ->
            pure $ Just $ SoacNode s2 (map (internalizeAndAdd ts) outputs) aux
      _ -> pure Nothing
  _ -> pure Nothing

makeCopiesOfFusedExcept :: [VName] -> NodeT -> FusionEnvM NodeT
makeCopiesOfFusedExcept noCopy (SoacNode soac pats aux) =
  do
    let lam = H.lambda soac
    let fused_inner = namesToList $ consumedByLambda $ Alias.analyseLambda mempty lam
    lam' <- makeCopiesInLambda (fused_inner L.\\ noCopy) lam
    pure $ SoacNode (H.setLambda lam' soac) pats aux
makeCopiesOfFusedExcept _ nodeT = pure nodeT

makeCopiesInLambda :: [VName] -> Lambda SOACS -> FusionEnvM (Lambda SOACS)
makeCopiesInLambda toCopy lam =
  do
    (copies, nameMap) <- localScope (scopeOf lam) $ makeCopyStms toCopy
    let l_body = lambdaBody lam
    let newBody = insertStms (stmsFromList copies) (substituteNames nameMap l_body)
    let newLambda = lam {lambdaBody = newBody}
    pure newLambda

makeCopyStms :: [VName] -> FusionEnvM ([Stm SOACS], M.Map VName VName)
makeCopyStms toCopy = do
  newNames <- mapM makeNewName toCopy
  copies <- mapM (\(name, name_fused) -> mkLetNames [name_fused] (BasicOp $ Copy name)) (zip toCopy newNames)
  pure (copies, makeMap toCopy newNames)
  where
    makeNewName name = newVName $ baseString name <> "_copy"

fuseNodeT :: [EdgeT] -> [VName] -> (NodeT, [EdgeT]) -> (NodeT, [EdgeT]) -> FusionEnvM (Maybe NodeT)
fuseNodeT edgs infusible (s1, e1s) (s2, e2s) =
  case (s1, s2) of
    ( _,
      IfNode stm2 dfused
      )
        | isRealNode s1,
          null infusible ->
            pure $ Just $ IfNode stm2 $ (s1, e1s) : dfused
    -- ( _,
    --   DoNode stm2 dfused) | isRealNode s1, null infusible
    --     -> pure $ Just $ DoNode stm2 $ (s1, e1s) : dfused
    -- requirements for this type of fusion should be really tough
    (SoacNode {}, SoacNode {})
      | null infusible,
        ns <- map getName $ filter isTrDep edgs,
        (not . null) ns,
        not $ any isDep e2s,
        [ts] <- L.nub $ map (\x -> findTransformsBetween x s1 s2) ns ->
          do
            let edgs' = trace (show (filter (not . isTrDep) edgs)) filter (not . isTrDep) edgs
            newS1m <- pullRearrangeNodeT ts s1
            case newS1m of
              Just newS1 -> fuseNodeT edgs' infusible (newS1, e1s) (s2, e2s)
              _ -> do
                newS2m <- pushRearrangeNodeT ts s2
                case newS2m of
                  Just newS2 -> fuseNodeT edgs' infusible (s1, e1s) (newS2, e2s)
                  _ -> pure Nothing
    (_, _) | any isTrDep edgs -> pure Nothing
    ( SoacNode soac1 pats1 aux1,
      SoacNode soac2 pats2 aux2
      ) ->
        let (o1, o2) = mapT (map H.inputArray) (pats1, pats2)
         in let aux = (aux1 <> aux2)
             in case (soac1, soac2) of
                  -- Screma-Screma fusion
                  ( H.Screma s_exp1 (ScremaForm scans_1 red_1 lam_1) i1,
                    H.Screma s_exp2 (ScremaForm scans_2 red_2 lam_2) i2
                    )
                      | s_exp1 == s_exp2,
                        not (any isScanRed edgs) ->
                          let soac = H.Screma s_exp2 (ScremaForm (scans_1 ++ scans_2) (red_1 ++ red_2) lam) fused_inputs
                           in pure $ Just $ SoacNode soac ids (aux1 <> aux2)
                      where
                        (lam_1_inputs, lam_2_inputs) = mapT boundByLambda (lam_1, lam_2)
                        (lam_1_output, lam_2_output) = mapT (namesFromRes . resFromLambda) (lam_1, lam_2)

                        fused_inputs = fuseInputs o1 i1 i2
                        lparams =
                          changeAll
                            (i1 ++ i2)
                            (lambdaParams lam_1 ++ lambdaParams lam_2)
                            fused_inputs

                        (scan_in_size_1, scan_in_size_2) = mapT scanInput (scans_1, scans_2)
                        (red_in_size_1, red_in_size_2) = mapT redInput (red_1, red_2)

                        (scan_inputs_1, red_inputs_1, lambda_outputs_1) = splitAt3 scan_in_size_1 red_in_size_1 lam_1_output
                        (scan_inputs_2, red_inputs_2, lambda_outputs_2) = splitAt3 scan_in_size_2 red_in_size_2 lam_2_output

                        fused_lambda_outputs =
                          concat
                            [ scan_inputs_1 ++ scan_inputs_2,
                              red_inputs_1 ++ red_inputs_2,
                              lambda_outputs_1 ++ lambda_outputs_2
                            ]

                        (types, body_res) =
                          unzip $
                            changeAll
                              (lam_1_output ++ lam_2_output)
                              ( zip (lambdaReturnType lam_1) (resFromLambda lam_1)
                                  ++ zip (lambdaReturnType lam_2) (resFromLambda lam_2)
                              )
                              fused_lambda_outputs

                        (scan_outputs_1, red_outputs_1, lambda_used_outputs_1) = splitAt3 (Futhark.scanResults scans_1) (Futhark.redResults red_1) o1
                        (scan_outputs_2, red_outputs_2, lambda_used_outputs_2) = splitAt3 (Futhark.scanResults scans_2) (Futhark.redResults red_2) o2

                        fused_outputs =
                          concat
                            [ scan_outputs_1,
                              scan_outputs_2,
                              red_outputs_1,
                              red_outputs_2,
                              lambda_used_outputs_1,
                              lambda_used_outputs_2
                            ]

                        ids = changeAll (o1 ++ o2) (pats1 ++ pats2) fused_outputs

                        fused_inputs_inner = changeAll (i1 ++ i2) (lam_1_inputs ++ lam_2_inputs) fused_inputs

                        map1 = makeMap (lam_1_inputs ++ lam_2_inputs) (map H.inputArray (i1 ++ i2))
                        map2 = makeMap o1 lam_1_output
                        map4 = makeMap (map H.inputArray fused_inputs) fused_inputs_inner
                        map3 = fuseMaps map1 (M.union map2 map4)

                        lam' = fuseLambda lam_1 lam_2
                        lam =
                          substituteNames map3 $
                            lam'
                              { lambdaParams = lparams,
                                lambdaReturnType = types,
                                lambdaBody = (lambdaBody lam') {bodyResult = body_res}
                              }
                  -- vertical map-scatter fusion
                  ( H.Screma s_exp1 (ScremaForm [] [] lam_1) i1,
                    H.Scatter s_exp2 lam_2 i2 other
                    )
                      | L.null infusible -- only if map outputs are used exclusivly by the scatter
                          && s_exp1 == s_exp2 ->
                          let soac = H.Scatter s_exp2 lam fused_inputs other
                           in pure $ Just $ SoacNode soac pats2 (aux1 <> aux2)
                      where
                        (lam, fused_inputs) = vFuseLambdas [] lam_1 i1 o1 lam_2 i2 o2
                  ( H.Screma s_exp1 (ScremaForm [] [] lam_1) i1,
                    H.Hist s_exp2 other lam_2 i2
                    )
                      | L.null infusible -- only if map outputs are used exclusivly by the hist
                          && s_exp1 == s_exp2 ->
                          let soac = H.Hist s_exp2 other lam fused_inputs
                           in pure $ Just $ SoacNode soac pats2 (aux1 <> aux2)
                      where
                        (lam, fused_inputs) = vFuseLambdas [] lam_1 i1 o1 lam_2 i2 o2
                  -- attempt fusing by turning other soac into a stream
                  ( H.Screma s_e1 _ _i1,
                    H.Stream s_e2 _ _ _ _
                    )
                      | s_e1 == s_e2 -> do
                          (stream1, is_extra_1) <- H.soacToStream soac1
                          if stream1 /= soac1
                            then do
                              is_extra_1' <- mapM (newIdent "unused" . identType) is_extra_1
                              fuseNodeT
                                edgs
                                infusible
                                (SoacNode stream1 (map H.identInput is_extra_1' <> pats1) aux1, e1s)
                                (s2, e2s)
                            else pure Nothing
                  ( H.Stream s_e1 _ _ _ _,
                    H.Screma s_e2 _ _i2
                    )
                      | s_e2 == s_e1 -> do
                          (stream2, is_extra_2) <- H.soacToStream soac2
                          if stream2 /= soac2
                            then do
                              is_extra_2' <- mapM (newIdent "unused" . identType) is_extra_2
                              fuseNodeT
                                edgs
                                infusible
                                (s1, e1s)
                                (SoacNode stream2 (map H.identInput is_extra_2' <> pats2) aux2, e2s)
                            else pure Nothing
                  ( H.Screma s_exp1 sform1 _i1,
                    H.Screma s_exp2 sform2 _i2
                    )
                      | Just _ <- isScanomapSOAC sform1,
                        Just _ <- isScanomapSOAC sform2,
                        s_exp1 == s_exp2,
                        any isScanRed edgs ->
                          do
                            doFusion <- gets fuseScans
                            if not doFusion
                              then pure Nothing
                              else do
                                (stream1, is_extra_1) <- H.soacToStream soac1
                                (stream2, is_extra_2) <- H.soacToStream soac2
                                is_extra_1' <- mapM (newIdent "unused" . identType) is_extra_1
                                is_extra_2' <- mapM (newIdent "unused" . identType) is_extra_2
                                fuseNodeT
                                  edgs
                                  infusible
                                  (SoacNode stream1 (map H.identInput is_extra_1' <> pats1) aux1, e1s)
                                  (SoacNode stream2 (map H.identInput is_extra_2' <> pats2) aux2, e2s)
                  -- ( H.Stream s_exp1 sform1 nes1 lam1 i1,
                  --   H.Stream s_exp2 sform2 nes2 lam2 i2)
                  --   | getStreamOrder sform1 /= getStreamOrder sform2 ->
                  --     let s1' = toSeqStream soac1 in
                  --     let s2' = toSeqStream soac2 in
                  --     fuseNodeT edgs infusible
                  --       (SoacNode s1' pats1 aux1, e1s)
                  --       (SoacNode s2' pats2 aux2, e2s)
                  ( H.Stream _w1 sform1 _lam1 _nes1 _i1,
                    H.Stream _w2 sform2 _lam2 _nes2 _i2
                    )
                      | (sform1 == Sequential) /= (sform2 == Sequential) ->
                          pure Nothing
                  ( H.Stream w1 sform1 lam1 nes1 i1,
                    H.Stream w2 sform2 lam2 nes2 i2
                    ) | w1 == w2 -> do
                      let chunk1 = head $ lambdaParams lam1
                      let chunk2 = head $ lambdaParams lam2
                      let mmap = makeMap [paramName chunk2] [paramName chunk1]

                      let (lam1Rps, lam1ps) = splitAt (length nes1) $ tail $ lambdaParams lam1
                      let (lam1Rts, lam1ts) = splitAt (length nes1) $ lambdaReturnType lam1
                      let (lam1Rrs, lam1rs) = splitAt (length nes1) $ bodyResult $ lambdaBody lam1

                      let (lam2Rps, lam2ps) = splitAt (length nes2) $ tail $ lambdaParams lam2
                      let (lam2Rts, lam2ts) = splitAt (length nes2) $ lambdaReturnType lam2
                      let (lam2Rrs, lam2rs) = splitAt (length nes2) $ bodyResult $ lambdaBody lam2

                      let lam1' = lam1 {lambdaParams = lam1ps, lambdaReturnType = lam1ts, lambdaBody = (lambdaBody lam1) {bodyResult = lam1rs}}
                      let lam2' = lam2 {lambdaParams = lam2ps, lambdaReturnType = lam2ts, lambdaBody = (lambdaBody lam2) {bodyResult = lam2rs}}
                      let (lam, is_new) = vFuseLambdas infusible lam1' i1 (drop (length nes1) o1) lam2' i2 (drop (length nes2) o2)
                      let lam' = lam {lambdaParams = chunk1 : lam1Rps ++ lam2Rps ++ lambdaParams lam}
                      let lam'' = lam' {lambdaBody = (lambdaBody lam') {bodyResult = lam1Rrs ++ lam2Rrs ++ bodyResult (lambdaBody lam')}}
                      let lam''' = lam'' {lambdaReturnType = lam1Rts <> lam2Rts <> lambdaReturnType lam''}

                      let toKeep = filter (\x -> H.inputArray x `elem` infusible) (drop (length nes1) pats1)
                      let pats = trace ("look: " ++ pretty pats2) $ take (length nes1) pats1 ++ take (length nes2) pats2 ++ toKeep ++ drop (length nes2) pats2

                      let soac = H.Stream w1 (mergeForms sform1 sform2) lam''' (nes1 <> nes2) is_new
                      pure $ Just $ substituteNames mmap $ SoacNode soac pats aux
                  _ -> pure Nothing -- not fusable soac combos
    _ -> pure Nothing -- not op statements

vFuseLambdas ::
  [VName] ->
  Lambda SOACS ->
  [H.Input] ->
  [VName] ->
  Lambda SOACS ->
  [H.Input] ->
  [VName] ->
  (Lambda SOACS, [H.Input])
vFuseLambdas infusible lam_1 i1 o1 lam_2 i2 _o2 = (lam, fused_inputs)
  where
    (lam_1_inputs, lam_2_inputs) = mapT boundByLambda (lam_1, lam_2)
    (lam_1_output, _) = mapT (namesFromRes . resFromLambda) (lam_1, lam_2)

    fused_inputs = fuseInputs o1 i1 i2
    fused_inputs_inner = changeAll (i1 ++ i2) (lam_1_inputs ++ lam_2_inputs) fused_inputs

    map1 = makeMap (lam_1_inputs ++ lam_2_inputs) (map H.inputArray (i1 ++ i2))
    map2 = makeMap (reverse o1) (reverse lam_1_output)
    map4 = makeMap (map H.inputArray fused_inputs) fused_inputs_inner
    map3 = fuseMaps map1 (M.union map2 map4)

    res_toChange = zip (lambdaReturnType lam_1) (bodyResult (lambdaBody lam_1))
    (_, other) = unzip $ filter (\(x, _) -> x `elem` infusible) (zip o1 res_toChange)
    (types, results) = unzip other

    lparams =
      changeAll
        (i1 ++ i2)
        (lambdaParams lam_1 ++ lambdaParams lam_2)
        fused_inputs

    lam' = fuseLambda lam_1 lam_2
    lam =
      substituteNames
        map3
        ( lam'
            { lambdaParams = lparams,
              lambdaReturnType = types ++ lambdaReturnType lam_2,
              lambdaBody = (lambdaBody lam') {bodyResult = results ++ bodyResult (lambdaBody lam')}
            }
        )

changeAll :: Ord b => [b] -> [a] -> [b] -> [a]
changeAll orig_names orig_other = mapMaybe (mapping M.!)
  where
    mapping = M.map Just $ makeMap orig_names orig_other

resFromLambda :: Lambda rep -> Result
resFromLambda = bodyResult . lambdaBody

hFuseNodeT :: NodeT -> NodeT -> FusionEnvM (Maybe NodeT)
hFuseNodeT s1 s2
  | Just inputs1 <- H.inputs <$> getSoac s1,
    Just inputs2 <- H.inputs <$> getSoac s2,
    hasNoDifferingInputs inputs1 inputs2 =
      case (s1, s2) of
        ( SoacNode soac1 pats1 aux1,
          SoacNode soac2 pats2 aux2
          ) -> case (soac1, soac2) of
            ( H.Screma {},
              H.Screma {}
              ) -> fuseNodeT [] (map H.inputArray pats1) (s1, []) (s2, [])
            ( H.Scatter s_exp1 lam_1 i1 outputs1,
              H.Scatter s_exp2 lam_2 i2 outputs2
              )
                | s_exp1 == s_exp2 ->
                    let soac = H.Scatter s_exp2 lam fused_inputs outputs
                     in pure $ Just $ SoacNode soac pats aux
                where
                  pats = pats1 <> pats2
                  aux = aux1 <> aux2
                  outputs = outputs1 <> outputs2
                  -- o1 = patNames pats1

                  (lam_1_inputs, lam_2_inputs) = mapT boundByLambda (lam_1, lam_2)
                  (lam_1_output, lam_2_output) = mapT resFromLambda (lam_1, lam_2)

                  fused_inputs = fuseInputs [] i1 i2
                  fused_inputs_inner = changeAll (i1 ++ i2) (lam_1_inputs ++ lam_2_inputs) fused_inputs

                  map1 = makeMap (lam_1_inputs ++ lam_2_inputs) (i1 ++ i2)
                  map4 = makeMap fused_inputs fused_inputs_inner
                  map3 = fuseMaps map1 map4

                  lam' = fuseLambda lam_1 lam_2

                  lparams =
                    changeAll
                      (i1 ++ i2)
                      (lambdaParams lam_1 ++ lambdaParams lam_2)
                      fused_inputs

                  (types1, types2) = mapT lambdaReturnType (lam_1, lam_2)
                  (res1, res2) = mapT resFromLambda (lam_1, lam_2)

                  (ids1, vals1) = splitScatterResults outputs1 (zip3 types1 res1 lam_1_output)
                  (ids2, vals2) = splitScatterResults outputs2 (zip3 types2 res2 lam_2_output)
                  (types, res, _) = unzip3 $ ids1 ++ ids2 ++ vals1 ++ vals2

                  lam =
                    substituteNames map3 $
                      lam'
                        { lambdaParams = lparams,
                          lambdaReturnType = types,
                          lambdaBody = (lambdaBody lam') {bodyResult = res}
                        }
            ( H.Hist s_exp1 ops_1 lam_1 i1,
              H.Hist s_exp2 ops_2 lam_2 i2 -- pretty much copied too
              )
                | s_exp1 == s_exp2 -> do
                    let num_buckets_2 = length ops_2
                    let num_buckets_1 = length ops_1
                    let (body_2, body_1) = (lambdaBody lam_2, lambdaBody lam_1)
                    let body' =
                          Body
                            { bodyDec = bodyDec body_1, -- body_p and body_c have the same decorations
                              bodyStms = bodyStms body_2 <> bodyStms body_1,
                              bodyResult =
                                take num_buckets_1 (bodyResult body_1)
                                  ++ take num_buckets_2 (bodyResult body_2)
                                  ++ drop num_buckets_1 (bodyResult body_1)
                                  ++ drop num_buckets_2 (bodyResult body_2)
                            }
                    let lam' =
                          Lambda
                            { lambdaParams = lambdaParams lam_1 ++ lambdaParams lam_2,
                              lambdaBody = body',
                              lambdaReturnType =
                                replicate (num_buckets_1 + num_buckets_2) (Prim int64)
                                  ++ drop num_buckets_1 (lambdaReturnType lam_1)
                                  ++ drop num_buckets_2 (lambdaReturnType lam_2)
                            }
                    -- success (outNames ker ++ returned_outvars) $
                    let soac = H.Hist s_exp1 (ops_1 <> ops_2) lam' (i1 <> i2)
                    pure $ Just $ SoacNode soac (pats1 <> pats2) (aux1 <> aux2)
            _ -> pure Nothing
        _ -> pure Nothing
hFuseNodeT _ _ = pure Nothing

fuseInputs :: [VName] -> [H.Input] -> [H.Input] -> [H.Input]
fuseInputs fusing inputs1 inputs2 =
  L.nub $ inputs1 `L.union` filter ((`notElem` fusing) . H.inputArray) inputs2

fuseLambda :: Lambda SOACS -> Lambda SOACS -> Lambda SOACS
fuseLambda lam_1 lam_2 =
  lam_2 {lambdaBody = l_body_new}
  where
    l_body_1 = lambdaBody lam_1
    l_body_2 = lambdaBody lam_2
    l_body_new = insertStms (bodyStms l_body_1) l_body_2

removeUnusedOutputs :: DepGraphAug
removeUnusedOutputs = mapAcross removeUnusedOutputsFromContext

vNameFromAdj :: G.Node -> (EdgeT, G.Node) -> VName
vNameFromAdj n1 (edge, n2) = depsFromEdge (n2, n1, edge)

removeUnusedOutputsFromContext :: DepContext -> FusionEnvM DepContext
removeUnusedOutputsFromContext (incoming, n1, nodeT, outgoing) =
  pure (incoming, n1, nodeT', outgoing)
  where
    toKeep = map (vNameFromAdj n1) incoming
    nodeT' = removeOutputsExcept toKeep nodeT
removeUnusedOutputsFromContext c = pure c

removeOutputsExcept :: [VName] -> NodeT -> NodeT
removeOutputsExcept toKeep s = case s of
  SoacNode soac@(H.Screma _ (ScremaForm scans_1 red_1 lam_1) _) pats1 aux1 ->
    SoacNode (H.setLambda lam_new soac) (pats_unchanged ++ pats_new) aux1
    where
      scan_input_size = scanInput scans_1
      red_input_size = redInput red_1
      scan_output_size = Futhark.scanResults scans_1
      red_outputs_size = Futhark.redResults red_1

      (pats_unchanged, pats_toChange) = splitAt (scan_output_size + red_outputs_size) pats1
      (res_unchanged, res_toChange) = splitAt (scan_input_size + red_input_size) (zip (resFromLambda lam_1) (lambdaReturnType lam_1))

      (pats_new, other) = unzip $ filter (\(x, _) -> H.inputArray x `elem` toKeep) (zip pats_toChange res_toChange)
      (results, types) = unzip (res_unchanged ++ other)
      lam_new =
        trace
          ("getshere: " ++ show toKeep)
          lam_1
            { lambdaReturnType = types,
              lambdaBody = (lambdaBody lam_1) {bodyResult = results}
            }
  node -> node

runInnerFusion :: DepGraphAug -- do fusion on the inner lambdas
runInnerFusion = mapAcross runInnerFusionOnContext

runInnerFusionOnContext :: DepContext -> FusionEnvM DepContext
runInnerFusionOnContext c@(incomming, node, nodeT, outgoing) = case nodeT of
  DoNode (Let pat aux (DoLoop params form body)) toFuse ->
    doFuseScans $
      localScope (scopeOfFParams (map fst params) <> scopeOf form) $ do
        let extra_is = map (paramName . fst) (filter (isArray . fst) params)
        b <- doFusionWithDelayed body extra_is toFuse
        pure (incomming, node, DoNode (Let pat aux (DoLoop params form b)) [], outgoing)
  IfNode (Let pat aux (If sz b1 b2 dec)) toFuse -> doFuseScans $ do
    b1' <- doFusionWithDelayed b1 [] toFuse
    b2' <- doFusionWithDelayed b2 [] toFuse
    rb2' <- renameBody b2'
    pure (incomming, node, IfNode (Let pat aux (If sz b1' rb2' dec)) [], outgoing)
  SoacNode soac pats aux -> do
    let lam = H.lambda soac
    newbody <- localScope (scopeOf lam) $ case soac of
      H.Stream {} -> dontFuseScans $ doFusionInner (lambdaBody lam) (lambdaParams lam)
      _ -> doFuseScans $ doFusionInner (lambdaBody lam) (lambdaParams lam)
    let newLam = lam {lambdaBody = newbody}
    let newNode = SoacNode (H.setLambda newLam soac) pats aux
    pure (incomming, node, newNode, outgoing)
  _ -> pure c
  where
    doFusionWithDelayed :: Body SOACS -> [VName] -> [(NodeT, [EdgeT])] -> FusionEnvM (Body SOACS)
    doFusionWithDelayed b extraInputs extraNodes = localScope (scopeOf stms) $
      do
        let g = emptyGraph stms results (inputs <> extraInputs)
        -- highly temporary and non-thought-out
        g' <- applyAugs [handleNodes extraNodes, makeMapping, initialGraphConstruction, printgraph, doAllFusion] g
        new_stms <- trace (pprg g') $ linearizeGraph g'
        pure b {bodyStms = stmsFromList new_stms}
      where
        inputs = map (vNameFromAdj node) outgoing ++ extraInputs
        stms = stmsToList (bodyStms b)
        results = namesFromRes (bodyResult b)
    doFusionInner :: Body SOACS -> [LParam SOACS] -> FusionEnvM (Body SOACS)
    doFusionInner b inp =
      do
        new_stms <- fuseGraph stms results inputs
        pure b {bodyStms = stmsFromList new_stms}
      where
        lambda_inputs = map paramName (filter isArray2 inp)
        other_inputs = map (vNameFromAdj node) $ filter (not . isDep . fst) outgoing
        inputs = other_inputs ++ lambda_inputs
        stms = stmsToList (bodyStms b)
        results = namesFromRes (bodyResult b)

-- inserting for delayed fusion
handleNodes :: [(NodeT, [EdgeT])] -> DepGraphAug
handleNodes ns g = do
  let nodes = G.newNodes (length ns) g
  let (nodeTs, edgs) = unzip ns
  let depNodes = zip nodes nodeTs
  let g' = G.insNodes depNodes g
  _ <- makeMapping g'
  applyAugs (zipWith (curry addEdgesToGraph) depNodes edgs) g'

addEdgesToGraph :: (DepNode, [EdgeT]) -> DepGraphAug
addEdgesToGraph (n, edgs) = genEdges [n] (const edgs')
  where
    f e = (getName e, e)
    edgs' = map f edgs

isAlias :: EdgeT -> Bool
isAlias (Alias _) = True
isAlias _ = False

isFake :: EdgeT -> Bool
isFake (Fake _) = True
isFake _ = False

makeCopiesOfConsAliased :: DepGraphAug
makeCopiesOfConsAliased g = mapAcrossWithSE copyAlised g
  where
    copyAlised :: DepNode -> DepGraphAug
    copyAlised (n, nt) _ = do
      let (incoming, _, _, outgoing) = G.context g n
      let incoming' = map getName $ filter isFake (map fst incoming)
      let outgoing' = map getName $ filter isAlias (map fst outgoing)
      let toMakeCopies = incoming' `L.intersect` outgoing'
      if not $ null toMakeCopies
        then do
          (new_stms, nameMapping) <- makeCopyStms toMakeCopies
          let newNode = FinalNode new_stms (substituteNames nameMapping nt) []
          updateNode n (const (Just newNode)) g
        else pure g
    copyAlised _ _ = pure g

fuseConsts :: [VName] -> Stms SOACS -> PassM (Stms SOACS)
fuseConsts outputs stms =
  stmsFromList
    <$> runFusionEnvM
      (scopeOf stms)
      freshFusionEnv
      (fuseGraphLZ (stmsToList stms) (varsRes outputs) [])

fuseFun :: Stms SOACS -> FunDef SOACS -> PassM (FunDef SOACS)
fuseFun consts fun = do
  fun_stms' <-
    runFusionEnvM
      (scopeOf fun <> scopeOf consts)
      freshFusionEnv
      (fuseGraphLZ fun_stms fun_res (funDefParams fun))
  pure fun {funDefBody = fun_body {bodyStms = stmsFromList fun_stms'}}
  where
    fun_body = funDefBody fun
    fun_stms = stmsToList $ bodyStms fun_body
    fun_res = bodyResult fun_body

-- | The pass definition.
fuseSOACs :: Pass SOACS SOACS
fuseSOACs =
  Pass
    { passName = "Fuse SOACs",
      passDescription = "Perform higher-order optimisation, i.e., fusion.",
      passFunction = \p ->
        intraproceduralTransformationWithConsts
          (fuseConsts (namesToList $ freeIn (progFuns p)))
          fuseFun
          p
    }
