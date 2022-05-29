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
import Futhark.IR.SOACS.Simplify (simplifyLambda)
import Futhark.Optimise.Fusion.GraphRep
import qualified Futhark.Optimise.Fusion.LoopKernel as LK
import Futhark.Pass
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util (isEnvVarAtLeast)

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

isArray :: Typed t => Param t -> Bool
isArray p = case paramType p of
  Array {} -> True
  _ -> False

-- lazy version of fuse graph - removes inputs from the graph that are not arrays
fuseGraphLZ :: Stms SOACS -> Result -> [Param DeclType] -> FusionEnvM (Stms SOACS)
fuseGraphLZ stms results inputs =
  fuseGraph
    stms
    (freeIn results)
    (namesFromList $ map paramName $ filter isArray inputs)

-- main fusion function.
fuseGraph :: Stms SOACS -> Names -> Names -> FusionEnvM (Stms SOACS)
fuseGraph stms results inputs = localScope (scopeOf stms) $ do
  old_mappings <- gets producerMapping
  graph_not_fused <- mkDepGraph stms results inputs

  let graph_not_fused'
        | isEnvVarAtLeast "FUTHARK_COMPILER_DEBUGGING" 2 =
            trace (pprg graph_not_fused) graph_not_fused
        | otherwise = graph_not_fused
  graph_fused <- doAllFusion graph_not_fused'
  let graph_fused'
        | isEnvVarAtLeast "FUTHARK_COMPILER_DEBUGGING" 2 =
            trace (pprg graph_fused) graph_fused
        | otherwise = graph_fused

  stms_new <- linearizeGraph graph_fused'
  modify (\s -> s {producerMapping = old_mappings})
  pure stms_new

unreachableEitherDir :: DepGraph -> G.Node -> G.Node -> FusionEnvM Bool
unreachableEitherDir g a b = do
  b1 <- reachable g a b
  b2 <- reachable g b a
  pure $ not (b1 || b2)

reachable :: DepGraph -> G.Node -> G.Node -> FusionEnvM Bool
reachable g source target = pure $ target `elem` Q.reachable source g

isNotVarInput :: [H.Input] -> [H.Input]
isNotVarInput = filter (isNothing . H.isVarInput)

finalizeNode :: NodeT -> FusionEnvM (Stms SOACS)
finalizeNode nt = case nt of
  StmNode stm -> pure $ oneStm stm
  SoacNode ots outputs soac aux -> runBuilder_ $ do
    untransformed_outputs <- mapM newName $ patNames outputs
    auxing aux $ letBindNames untransformed_outputs . Op =<< H.toSOAC soac
    forM_ (zip (patNames outputs) untransformed_outputs) $ \(output, v) ->
      letBindNames [output] . BasicOp . SubExp . Var =<< H.applyTransforms ots v
  RNode _ -> pure mempty
  InNode _ -> pure mempty
  DoNode stm lst -> do
    stmsNotFused <- mapM (finalizeNode . fst) lst
    pure $ mconcat stmsNotFused <> oneStm stm
  IfNode stm lst -> do
    stmsNotFused <- mapM (finalizeNode . fst) lst
    pure $ mconcat stmsNotFused <> oneStm stm
  FinalNode stms1 nt' stms2 -> do
    stms' <- finalizeNode nt'
    pure $ stms1 <> stms' <> stms2

linearizeGraph :: DepGraph -> FusionEnvM (Stms SOACS)
linearizeGraph g = do
  stms <- mapM finalizeNode $ reverse $ Q.topsort' g
  pure $ mconcat stms

fusedSomething :: NodeT -> FusionEnvM (Maybe NodeT)
fusedSomething x = do
  modify $ \s -> s {fusedAnything = True}
  pure $ Just x

-- Fixed-point
keepTrying :: DepGraphAug FusionEnvM -> DepGraphAug FusionEnvM
keepTrying f g = do
  prev_fused <- gets fusedAnything
  modify $ \s -> s {fusedAnything = False}
  g' <- f g
  fused <- gets fusedAnything
  (if fused then keepTrying f g' else pure g')
    <* modify (\s -> s {fusedAnything = prev_fused || fused})

doAllFusion :: DepGraphAug FusionEnvM
doAllFusion =
  applyAugs
    [ keepTrying . applyAugs $
        [ doVerticalFusion,
          doHorizontalFusion,
          runInnerFusion
        ],
      removeUnusedOutputs
    ]

doVerticalFusion :: DepGraphAug FusionEnvM
doVerticalFusion g = applyAugs (map tryFuseNodeInGraph $ reverse $ G.labNodes g) g

doHorizontalFusion :: DepGraphAug FusionEnvM
doHorizontalFusion g = applyAugs (map horizontalFusionOnNode (G.nodes g)) g

-- | For each node, find what came before, attempt to fuse them
-- horizontally.  This means we only perform horizontal fusion for
-- SOACs that use the same input in some way.
horizontalFusionOnNode :: G.Node -> DepGraphAug FusionEnvM
horizontalFusionOnNode node g =
  applyAugs (map (uncurry hTryFuseNodesInGraph) pairs) g
  where
    incoming_nodes = map fst $ filter (isDep . snd) $ G.lpre g node
    pairs = [(x, y) | x <- incoming_nodes, y <- incoming_nodes, x < y]

vFusionFeasability :: DepGraph -> G.Node -> G.Node -> FusionEnvM Bool
vFusionFeasability g n1 n2 = do
  let b2 = not (any isInf (edgesBetween g n1 n2))
  reach <- mapM (reachable g n2) (filter (/= n2) (G.pre g n1))
  pure $ b2 && all not reach

hFusionFeasability :: DepGraph -> G.Node -> G.Node -> FusionEnvM Bool
hFusionFeasability = unreachableEitherDir

tryFuseNodeInGraph :: DepNode -> DepGraphAug FusionEnvM
tryFuseNodeInGraph node_to_fuse g =
  if G.gelem node_to_fuse_id g
    then applyAugs (map (vTryFuseNodesInGraph node_to_fuse_id) fuses_with) g
    else pure g
  where
    fuses_with = map fst $ filter (isDep . snd) $ G.lpre g (nodeFromLNode node_to_fuse)
    node_to_fuse_id = nodeFromLNode node_to_fuse

vTryFuseNodesInGraph :: G.Node -> G.Node -> DepGraphAug FusionEnvM
-- find the neighbors -> verify that fusion causes no cycles -> fuse
vTryFuseNodesInGraph node_1 node_2 g
  | not (G.gelem node_1 g && G.gelem node_2 g) = pure g
  | otherwise = do
      b <- vFusionFeasability g node_1 node_2
      if b
        then do
          let (ctx1, ctx2) = (G.context g node_1, G.context g node_2)
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
              contractEdge node_2 (inputs, node_1, nodeT', outputs) g
            Nothing -> pure g
        else pure g
  where
    edgs = map G.edgeLabel $ edgesBetween g node_1 node_2
    fusedC = map getName $ filter isCons edgs
    infusable_nodes =
      map
        depsFromEdge
        (concatMap (edgesBetween g node_1) (filter (/= node_2) $ G.pre g node_1))

hTryFuseNodesInGraph :: G.Node -> G.Node -> DepGraphAug FusionEnvM
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
  c2@(_, _, nodeT2, _) = do
    fres <- hFuseNodeT nodeT1 nodeT2
    case fres of
      Just nodeT -> pure $ Just (mergedContext nodeT c1 c2)
      Nothing -> pure Nothing

vFuseContexts :: [EdgeT] -> [VName] -> DepContext -> DepContext -> FusionEnvM (Maybe DepContext)
vFuseContexts
  edgs
  infusable
  c1@(i1, n1, nodeT1, o1)
  c2@(_i2, n2, nodeT2, o2) = do
    fres <-
      vFuseNodeT
        edgs
        infusable
        (nodeT1, map fst $ filter ((/=) n2 . snd) i1, map fst o1)
        (nodeT2, map fst $ filter ((/=) n1 . snd) o2)
    case fres of
      Just nodeT -> pure $ Just (mergedContext nodeT c1 c2)
      Nothing -> pure Nothing

makeMap :: Ord a => [a] -> [b] -> M.Map a b
makeMap x y = M.fromList $ zip x y

fuseMaps :: Ord b => M.Map a b -> M.Map b c -> M.Map a c
fuseMaps m1 m2 = M.mapMaybe (`M.lookup` m2) m1

makeCopiesOfFusedExcept :: [VName] -> NodeT -> FusionEnvM NodeT
makeCopiesOfFusedExcept noCopy (SoacNode ots pats soac aux) = do
  let lam = H.lambda soac
  let fused_inner = namesToList $ consumedByLambda $ Alias.analyseLambda mempty lam
  lam' <- makeCopiesInLambda (fused_inner L.\\ noCopy) lam
  pure $ SoacNode ots pats (H.setLambda lam' soac) aux
makeCopiesOfFusedExcept _ nodeT = pure nodeT

makeCopiesInLambda :: [VName] -> Lambda SOACS -> FusionEnvM (Lambda SOACS)
makeCopiesInLambda toCopy lam = do
  (copies, nameMap) <- localScope (scopeOf lam) $ makeCopyStms toCopy
  let l_body = lambdaBody lam
      newBody = insertStms copies (substituteNames nameMap l_body)
      newLambda = lam {lambdaBody = newBody}
  pure newLambda

makeCopyStms :: [VName] -> FusionEnvM (Stms SOACS, M.Map VName VName)
makeCopyStms toCopy = do
  newNames <- mapM makeNewName toCopy
  copies <- forM (zip toCopy newNames) $ \(name, name_fused) ->
    mkLetNames [name_fused] (BasicOp $ Copy name)
  pure (stmsFromList copies, makeMap toCopy newNames)
  where
    makeNewName name = newVName $ baseString name <> "_copy"

okToFuseProducer :: H.SOAC SOACS -> FusionEnvM Bool
okToFuseProducer (H.Screma _ form _) = do
  let is_scan = isJust $ Futhark.isScanomapSOAC form
  gets $ (not is_scan ||) . fuseScans
okToFuseProducer _ = pure True

-- First node is producer, second is consumer.
vFuseNodeT :: [EdgeT] -> [VName] -> (NodeT, [EdgeT], [EdgeT]) -> (NodeT, [EdgeT]) -> FusionEnvM (Maybe NodeT)
vFuseNodeT _ infusible (s1, _, e1s) (IfNode stm2 dfused, _)
  | isRealNode s1,
    null infusible =
      pure $ Just $ IfNode stm2 $ (s1, e1s) : dfused
vFuseNodeT _ infusible (StmNode stm1, _, _) (SoacNode ots2 pats2 soac2 aux2, _)
  | null infusible,
    [stm1_out] <- patNames $ stmPat stm1,
    Just (stm1_in, tr) <-
      H.transformFromExp (stmAuxCerts (stmAux stm1)) (stmExp stm1) = do
      stm1_in_t <- lookupType stm1_in
      let onInput inp
            | H.inputArray inp == stm1_out =
                H.Input (tr H.<| H.inputTransforms inp) stm1_in stm1_in_t
            | otherwise =
                inp
          soac2' = map onInput (H.inputs soac2) `H.setInputs` soac2
      pure $ Just $ SoacNode ots2 pats2 soac2' aux2
vFuseNodeT
  _
  _
  (SoacNode ots1 pats1 soac1 aux1, i1s, _e1s)
  (SoacNode ots2 pats2 soac2 aux2, _e2s) = do
    scope <- askScope
    let ker =
          LK.FusedKer
            { LK.fsoac = soac2,
              LK.kernelScope = scope,
              LK.outputTransform = ots2,
              LK.outNames = patNames pats2
            }
        preserveEdge InfDep {} = True
        preserveEdge e = isDep e
        preserve = namesFromList $ map getName $ filter preserveEdge i1s
    ok <- okToFuseProducer soac1
    r <-
      if ok && ots1 == mempty
        then LK.attemptFusion preserve (patNames pats1) soac1 ker
        else pure Nothing
    case r of
      Just ker' -> do
        when (isEnvVarAtLeast "FUTHARK_COMPILER_DEBUGGING" 1) $
          traceM $
            unlines
              [ show preserve,
                "vfused",
                pretty soac1,
                "outputs",
                pretty (pats1, show ots1),
                "and",
                pretty soac2,
                "got",
                pretty (LK.fsoac <$> r),
                "outputs",
                pretty (LK.outNames <$> r)
              ]

        let pats2' =
              zipWith PatElem (LK.outNames ker') (H.typeOf (LK.fsoac ker'))
        fusedSomething $
          SoacNode
            (LK.outputTransform ker')
            (Pat pats2')
            (LK.fsoac ker')
            (aux1 <> aux2)
      Nothing -> pure Nothing
vFuseNodeT _ _ _ _ = pure Nothing

changeAll :: Ord b => [b] -> [a] -> [b] -> [a]
changeAll orig_names orig_other = mapMaybe (mapping M.!)
  where
    mapping = M.map Just $ makeMap orig_names orig_other

resFromLambda :: Lambda rep -> Result
resFromLambda = bodyResult . lambdaBody

hasNoDifferingInputs :: [H.Input] -> [H.Input] -> Bool
hasNoDifferingInputs is1 is2 =
  let (vs1, vs2) = (isNotVarInput is1, isNotVarInput $ is2 L.\\ is1)
   in null $ vs1 `L.intersect` vs2

hFuseNodeT :: NodeT -> NodeT -> FusionEnvM (Maybe NodeT)
hFuseNodeT (SoacNode ots1 pats1 soac1 aux1) (SoacNode ots2 pats2 soac2 aux2)
  | inputs1 <- H.inputs soac1,
    inputs2 <- H.inputs soac2,
    ots1 == mempty,
    ots2 == mempty,
    hasNoDifferingInputs inputs1 inputs2 = do
      scope <- askScope
      case (soac1, soac2) of
        ( H.Screma {},
          H.Screma {}
          ) -> do
            let ker =
                  LK.FusedKer
                    { LK.fsoac = soac2,
                      LK.kernelScope = scope,
                      LK.outputTransform = mempty,
                      LK.outNames = patNames pats2
                    }
                preserve = namesFromList $ patNames pats1
            r <- LK.attemptFusion preserve (patNames pats1) soac1 ker
            when (isEnvVarAtLeast "FUTHARK_COMPILER_DEBUGGING" 1) $
              traceM $
                unlines
                  [ show preserve,
                    "hfused",
                    pretty soac1,
                    "outputs",
                    pretty pats1,
                    "and",
                    pretty soac2,
                    "got",
                    pretty (LK.fsoac <$> r),
                    "outputs",
                    pretty (LK.outNames <$> r)
                  ]
            case r of
              Just ker' -> do
                let pats2' =
                      zipWith PatElem (LK.outNames ker') (H.typeOf (LK.fsoac ker'))
                pure $ Just $ SoacNode mempty (Pat pats2') (LK.fsoac ker') (aux1 <> aux2)
              Nothing -> pure Nothing
        ( H.Scatter w1 lam_1 i1 outputs1,
          H.Scatter w2 lam_2 i2 outputs2
          )
            | w1 == w2 ->
                let soac = H.Scatter w2 lam fused_inputs outputs
                 in pure $ Just $ SoacNode mempty pats soac aux
            where
              pats = pats1 <> pats2
              aux = aux1 <> aux2
              outputs = outputs1 <> outputs2

              (lam_1_inputs, lam_2_inputs) = (boundByLambda lam_1, boundByLambda lam_2)
              (lam_1_output, lam_2_output) = (resFromLambda lam_1, resFromLambda lam_2)

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

              (types1, types2) = (lambdaReturnType lam_1, lambdaReturnType lam_2)
              (res1, res2) = (resFromLambda lam_1, resFromLambda lam_2)

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
        ( H.Hist w1 ops_1 lam_1 i1,
          H.Hist w2 ops_2 lam_2 i2 -- pretty much copied too
          )
            | w1 == w2 -> do
                let num_buckets_2 = length ops_2
                    num_buckets_1 = length ops_1
                    (body_2, body_1) = (lambdaBody lam_2, lambdaBody lam_1)
                    body' =
                      Body
                        { bodyDec = bodyDec body_1, -- body_p and body_c have the same decorations
                          bodyStms = bodyStms body_2 <> bodyStms body_1,
                          bodyResult =
                            take num_buckets_1 (bodyResult body_1)
                              ++ take num_buckets_2 (bodyResult body_2)
                              ++ drop num_buckets_1 (bodyResult body_1)
                              ++ drop num_buckets_2 (bodyResult body_2)
                        }
                    lam' =
                      Lambda
                        { lambdaParams = lambdaParams lam_1 ++ lambdaParams lam_2,
                          lambdaBody = body',
                          lambdaReturnType =
                            replicate (num_buckets_1 + num_buckets_2) (Prim int64)
                              ++ drop num_buckets_1 (lambdaReturnType lam_1)
                              ++ drop num_buckets_2 (lambdaReturnType lam_2)
                        }
                    soac = H.Hist w1 (ops_1 <> ops_2) lam' (i1 <> i2)
                pure $ Just $ SoacNode mempty (pats1 <> pats2) soac (aux1 <> aux2)
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

removeUnusedOutputs :: DepGraphAug FusionEnvM
removeUnusedOutputs = mapAcross removeUnusedOutputsFromContext

vNameFromAdj :: G.Node -> (EdgeT, G.Node) -> VName
vNameFromAdj n1 (edge, n2) = depsFromEdge (n2, n1, edge)

removeUnusedOutputsFromContext :: DepContext -> FusionEnvM DepContext
removeUnusedOutputsFromContext (incoming, n1, nodeT, outgoing) =
  pure (incoming, n1, nodeT', outgoing)
  where
    toKeep = map (vNameFromAdj n1) incoming
    nodeT' = removeOutputsExcept toKeep nodeT

removeOutputsExcept :: [VName] -> NodeT -> NodeT
removeOutputsExcept toKeep s = case s of
  SoacNode ots (Pat pats1) soac@(H.Screma _ (ScremaForm scans_1 red_1 lam_1) _) aux1 ->
    SoacNode ots (Pat $ pats_unchanged <> pats_new) (H.setLambda lam_new soac) aux1
    where
      scan_output_size = Futhark.scanResults scans_1
      red_output_size = Futhark.redResults red_1

      (pats_unchanged, pats_toChange) = splitAt (scan_output_size + red_output_size) pats1
      (res_unchanged, res_toChange) = splitAt (scan_output_size + red_output_size) (zip (resFromLambda lam_1) (lambdaReturnType lam_1))

      (pats_new, other) = unzip $ filter (\(x, _) -> patElemName x `elem` toKeep) (zip pats_toChange res_toChange)
      (results, types) = unzip (res_unchanged ++ other)
      lam_new =
        lam_1
          { lambdaReturnType = types,
            lambdaBody = (lambdaBody lam_1) {bodyResult = results}
          }
  node -> node

runInnerFusion :: DepGraphAug FusionEnvM -- do fusion on the inner lambdas
runInnerFusion = mapAcross runInnerFusionOnContext

runInnerFusionOnContext :: DepContext -> FusionEnvM DepContext
runInnerFusionOnContext c@(incoming, node, nodeT, outgoing) = case nodeT of
  DoNode (Let pat aux (DoLoop params form body)) toFuse ->
    doFuseScans $
      localScope (scopeOfFParams (map fst params) <> scopeOf form) $ do
        let extra_is = namesFromList $ map (paramName . fst) (filter (isArray . fst) params)
        b <- doFusionWithDelayed body extra_is toFuse
        pure (incoming, node, DoNode (Let pat aux (DoLoop params form b)) [], outgoing)
  IfNode (Let pat aux (If sz b1 b2 dec)) toFuse -> doFuseScans $ do
    b1' <- doFusionWithDelayed b1 mempty toFuse
    b2' <- doFusionWithDelayed b2 mempty toFuse
    rb2' <- renameBody b2'
    pure (incoming, node, IfNode (Let pat aux (If sz b1' rb2' dec)) [], outgoing)
  SoacNode ots pat soac aux -> do
    -- To clean up previous instances of fusion.
    lam <- simplifyLambda $ H.lambda soac
    newbody <- localScope (scopeOf lam) $ case soac of
      H.Stream _ Sequential {} _ _ _ ->
        dontFuseScans $ doFusionInner (lambdaBody lam) (lambdaParams lam)
      _ ->
        doFuseScans $ doFusionInner (lambdaBody lam) (lambdaParams lam)
    -- To clean up any inner fusion.
    lam' <- simplifyLambda $ lam {lambdaBody = newbody}
    let nodeT' = SoacNode ots pat (H.setLambda lam' soac) aux
    pure (incoming, node, nodeT', outgoing)
  _ -> pure c
  where
    doFusionWithDelayed :: Body SOACS -> Names -> [(NodeT, [EdgeT])] -> FusionEnvM (Body SOACS)
    doFusionWithDelayed b extraInputs extraNodes = localScope (scopeOf stms) $ do
      let g = emptyGraph stms results inputs
      -- highly temporary and non-thought-out
      stm_node <- mapM (finalizeNode . fst) extraNodes
      g' <-
        applyAugs
          [ handleNodes extraNodes,
            makeMapping,
            makeAliasTable (mconcat stm_node <> stms),
            initialGraphConstruction,
            doAllFusion
          ]
          g
      new_stms <- linearizeGraph g'
      pure b {bodyStms = new_stms}
      where
        inputs = namesFromList (map (vNameFromAdj node) outgoing) <> extraInputs
        stms = bodyStms b
        results = freeIn (bodyResult b)
    doFusionInner :: Body SOACS -> [LParam SOACS] -> FusionEnvM (Body SOACS)
    doFusionInner b inp = do
      new_stms <- fuseGraph stms results inputs
      pure b {bodyStms = new_stms}
      where
        lambda_inputs = map paramName (filter isArray inp)
        other_inputs = map (vNameFromAdj node) $ filter (not . isDep . fst) outgoing
        inputs = namesFromList $ other_inputs ++ lambda_inputs
        stms = bodyStms b
        results = freeIn (bodyResult b)

-- inserting for delayed fusion
handleNodes :: [(NodeT, [EdgeT])] -> DepGraphAug FusionEnvM
handleNodes ns g = do
  let nodes = G.newNodes (length ns) g
  let (nodeTs, edgs) = unzip ns
  let depNodes = zip nodes nodeTs
  let g' = G.insNodes depNodes g
  _ <- makeMapping g'
  applyAugs (zipWith (curry addEdgesToGraph) depNodes edgs) g'

addEdgesToGraph :: (DepNode, [EdgeT]) -> DepGraphAug FusionEnvM
addEdgesToGraph (n, edgs) = genEdges [n] (const edgs')
  where
    f e = (getName e, e)
    edgs' = map f edgs

fuseConsts :: [VName] -> Stms SOACS -> PassM (Stms SOACS)
fuseConsts outputs stms =
  runFusionEnvM
    (scopeOf stms)
    freshFusionEnv
    (fuseGraphLZ stms (varsRes outputs) [])

fuseFun :: Stms SOACS -> FunDef SOACS -> PassM (FunDef SOACS)
fuseFun consts fun = do
  fun_stms' <-
    runFusionEnvM
      (scopeOf fun <> scopeOf consts)
      freshFusionEnv
      (fuseGraphLZ (bodyStms fun_body) (bodyResult fun_body) (funDefParams fun))
  pure fun {funDefBody = fun_body {bodyStms = fun_stms'}}
  where
    fun_body = funDefBody fun

-- | The pass definition.
{-# NOINLINE fuseSOACs #-}
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
