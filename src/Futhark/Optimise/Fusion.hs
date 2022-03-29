-- | Perform horizontal and vertical fusion of SOACs.  See the paper
-- /A T2 Graph-Reduction Approach To Fusion/ for the basic idea (some
-- extensions discussed in /Design and GPGPU Performance of Futhark’s
-- Redomap Construct/).
module Futhark.Optimise.Fusion (fuseSOACs) where

-- import Control.Monad.Except
-- import Control.Monad.Reader
-- import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map.Strict as M

import Data.Maybe
--import qualified Data.Set as S
import qualified Futhark.Analysis.Alias as Alias
--import qualified Futhark.Analysis.HORep.SOAC as SOAC
import Futhark.Construct
--import qualified Futhark.IR.Aliases as Aliases
import Futhark.IR.Prop.Aliases
import Futhark.IR.SOACS hiding (SOAC (..))
import qualified Futhark.IR.SOACS as Futhark
--import Futhark.IR.SOACS.Simplify
-- import Futhark.Optimise.Fusion.LoopKernel
import Futhark.Pass
--import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util (splitAt3)

import Futhark.Optimise.GraphRep
import qualified Data.Graph.Inductive.Query.DFS as Q
--import qualified Data.Graph.Inductive.Query.TransClos as TC
import Data.Graph.Inductive.Graph
-- import qualified Control.Monad.Identity as Control.Monad

import Debug.Trace
--import Futhark.IR.Aliases (VName(VName))
--import Futhark.Optimise.Fusion.LoopKernel (FusedKer(fusedVars))
--import Data.Tuple (swap)
--import Data.List (deleteFirstsBy)
--import Data.FileEmbed (bsToExp)
--import GHC.TopHandler (runNonIO)
--import qualified Futhark.Util as L
--import Control.Monad (foldM)
import Data.Foldable (foldlM)
import Control.Monad.State
import Futhark.IR.SOACS.SOAC (SOAC(Screma))


-- unofficial TODO
-- rename variables that used to be transpose results.
-- insert transpose statements at the end.


-- scatter fusion/histogram fusion if input arrays match


-- extra util - scans reduces are "a->a->a" - so half of those are the amount of inputs
scanInput :: [Scan SOACS] -> Int
scanInput l = flip div 2 $ sum (map (length . lambdaParams . scanLambda) l)
redInput :: [Reduce rep] -> Int
redInput l = flip div 2 $ sum (map (length . lambdaParams . redLambda) l)






-- | The pass definition.
fuseSOACs :: Pass SOACS SOACS
fuseSOACs =
  Pass
    { passName = "Fuse SOACs",
      passDescription = "Perform higher-order optimisation, i.e., fusion.",
      passFunction = \p -> intraproceduralTransformationWithConsts
          (fuseConsts (namesToList $ freeIn (progFuns p)))
          fuseFun p
          -- (\y x -> pure x)
    }



fuseConsts :: [VName] -> Stms SOACS -> PassM (Stms SOACS)
fuseConsts outputs stms =
  do
    new_stms <- runFusionEnvM (fuseGraphLZ stmList results []) $ freshFusionEnv $ scopeOf stms
    return $ stmsFromList new_stms
  where
    stmList = stmsToList stms
    results = varsRes outputs



-- some sort of functional decorator pattern
fuseFun :: Stms SOACS -> FunDef SOACS -> PassM (FunDef SOACS)
fuseFun _stmts fun = do
  new_stms <- runFusionEnvM (fuseGraphLZ stms res (funDefParams  fun)) $ freshFusionEnv $ scopeOf fun
  let body = (funDefBody fun) {bodyStms = stmsFromList new_stms}
  return fun {funDefBody = body}
    where
      b   = funDefBody fun
      stms = trace (ppr (bodyStms b)) $ stmsToList $ bodyStms b
      res = bodyResult b

-- lazy version of fuse graph - removes inputs from the graph that are not arrays
fuseGraphLZ :: [Stm SOACS] -> Result -> [Param DeclType] -> FusionEnvM [Stm SOACS]
fuseGraphLZ stms results inputs = fuseGraph stms resNames inputNames
  where
    resNames = namesFromRes results
    inputNames = map paramName $ filter isArray inputs

-- main fusion function.
fuseGraph :: [Stm SOACS] -> [VName] -> [VName] -> FusionEnvM [Stm SOACS]
fuseGraph stms results inputs = do
    -- old_reachabilityG <- trace (unlines (map ppr stms)) $ gets reachabilityG
    old_mappings <- gets producerMapping
    graph_not_fused <- mkDepGraph stms results (trace (show inputs) inputs)
 -- I have no idea why this is neccessary

    oldScope <- gets scope
    modify (\s -> s {scope = M.union (scopeOf stms) oldScope})
    -- modify (\s -> s {reachabilityG = TC.tc $ nmap (const ()) graph_not_fused})
    -- rg' <- gets reachabilityG
    let graph_not_fused' = trace (pprg graph_not_fused) graph_not_fused
    graph_fused <-  doAllFusion graph_not_fused'
    let graph_fused' = trace (pprg graph_fused) graph_fused
    -- rg <- gets reachabilityG
    let stms_new = linearizeGraph graph_fused'
    modify (\s -> s{producerMapping=old_mappings} )
    return $ trace (unlines (map ppr stms_new)) stms_new


unreachableEitherDir :: DepGraph -> Node -> Node -> FusionEnvM Bool
unreachableEitherDir g a b = do
  b1 <- reachable g a b
  b2 <- reachable g b a
  pure $ not (b1 || b2)

reachable :: DepGraph -> Node -> Node -> FusionEnvM Bool
reachable g source target = pure $ target `elem` Q.reachable source g


linearizeGraph :: DepGraph -> [Stm SOACS]
linearizeGraph g = concatMap stmFromNode $ reverse $ Q.topsort' g

doAllFusion :: DepGraphAug
doAllFusion = applyAugs [keepTrying doMapFusion, doHorizontalFusion, removeUnusedOutputs, makeCopiesOfConsAliased, runInnerFusion]

-- doInnerFusion :: DepGraphAug
-- doInnerFusion g = pure g


-- map-fusion part




doMapFusion :: DepGraphAug
-- go through each node and attempt a map fusion
doMapFusion g = applyAugs (map tryFuseNodeInGraph $ labNodes g) g

doHorizontalFusion :: DepGraphAug
doHorizontalFusion g = applyAugs (map  horizontalFusionOnNode (nodes g)) g

  -- for each node, find what came before, attempt to fuse

horizontalFusionOnNode :: Node -> DepGraphAug
horizontalFusionOnNode node g = tryFuseAll incoming_nodes g
  where
    (incoming_nodes, _) = unzip $ lpre g node


tryFuseAll :: [Node] -> DepGraphAug
tryFuseAll nodes_list = applyAugs (map (uncurry tryFuseNodesInGraph2) pairs)
  where
    pairs = [(x, y) | x <- nodes_list, y <- nodes_list,  x < y]



-- contextFromLNode :: DepGraph -> DepNode -> DepContext
-- contextFromLNode g lnode = context g $ nodeFromLNode lnode

-- isRes :: (Node, EdgeT) -> Bool
-- isRes (_, Res _) = True
-- isRes _ = False

isDep :: EdgeT -> Bool
isDep (Dep _) = True
isDep (InfDep _) = True
isDep (Res _) = True -- unintuitive, but i think it works
isDep _ = False

isInf :: (Node, Node, EdgeT) -> Bool
isInf (_,_,e) = case e of
  InfDep _ -> True
  Cons _ -> False -- you would think this sholud be true - but mabye this works
  Fake _ -> True -- this is infusible to avoid simultaneous cons/dep edges
  _ -> False

isCons :: EdgeT -> Bool
isCons (Cons _) = True
isCons _ = False


-- how to check that no other successor of source reaches target:
-- all mapM (not reachable) (suc graph source - target)
vFusionFeasability :: DepGraph -> Node -> Node -> FusionEnvM Bool
vFusionFeasability g n1 n2 =
  do
    --let b1 = all isDep edges || all (==n2) nodesN1
    let b2 = not (any isInf (edgesBetween g n1 n2))
    reach <- mapM (reachable g n2) (filter (/=n2) (pre g n1))
    pure $ b2 && all not reach

-- horizontally_fusible_groups :: DepGraph -> [[DepNode]]
-- horizontally_fusible_groups g = let hfg =(horizontally_fusible_groups g) in (not $ null hfg, hfg)

-- horizontally_fusible_groups :: DepGraph -> [[DepNode]]
-- horizontally_fusible_groups g =
--   map (\l -> concatMap snd $ M.toList $ M.fromListWith (++) [(l, [v]) | v@(_,SNode (Let _ _ (Op (Futhark.Screma l _ _)))) <- l]) scremas
--   where
--     ins = map (map (lNodeFromNode g) . pre g) (nodes g)
--     scremas =
--       filter (\x -> length x > 1) $
--       map (catMaybes . filter (\case
--         Just (_,SNode (Let _ _ (Op Futhark.Screma{}))) -> True
--         _ -> False
--       )) ins

-- fuse_horizontally :: DepGraphAug
-- fuse_horizontally g =
--   let (fusible, groups) = horizontally_fusible g in
--   if not fusible then g else
--     let (((n1, SNode s1):(n2, SNode s2):rest):_) = groups in
--     case fuseStms [] s1 s2 of

-- todo: add length check
hFusionFeasability :: DepGraph -> Node -> Node -> FusionEnvM Bool
hFusionFeasability = unreachableEitherDir
  -- lessThanOneDep n1 && lessThanOneDep n2
  -- where
  --   lessThanOneDep n =
  --     let (nodes, _) = unzip $ filter (not . isRes) $ lsuc g n
  --     in (<=1) $ length (L.nub nodes)

  -- what are the rules here?
  --  - they have an input in common
  --  - they only have that input? -


tryFuseNodeInGraph :: DepNode -> DepGraphAug
tryFuseNodeInGraph node_to_fuse g =
  if gelem node_to_fuse_id g
  then applyAugs (map (tryFuseNodesInGraph node_to_fuse_id) fuses_with) g
  else pure g
  where
    fuses_with = map fst $ filter (isDep . snd) $ lpre g (nodeFromLNode node_to_fuse)
    node_to_fuse_id = nodeFromLNode node_to_fuse

tryFuseNodesInGraph :: Node -> Node -> DepGraphAug
-- find the neighbors
-- check that they have no other dependencies
-- fuse them
tryFuseNodesInGraph node_1 node_2 g
  | not (gelem node_1 g && gelem node_2 g) = pure g
  | otherwise =
    do
      b <- vFusionFeasability g node_1 node_2
      if b then
        case fuseContexts infusable_nodes (context g node_1) (context g node_2) of
          Just newcxt@(inputs, _, SNode stm _, outputs) ->
            if null fused then contractEdge node_2 newcxt g
            else do
              new_stm <- makeCopies stm
              contractEdge node_2 (inputs, node_1, SNode new_stm mempty, outputs) g
          Just _ -> error "fuseContexts did not return an SNode"
          Nothing -> pure g
      else pure g
    where
      -- sorry about this
      fused = concatMap depsFromEdge $ filter (isCons . edgeLabel) $ edgesBetween g node_1 node_2
      infusable_nodes = concatMap depsFromEdge
        (concatMap (edgesBetween g node_1) (filter (/=node_2) $ pre g node_1))
                                  -- L.\\ edges_between g node_1 node_2)


-- insertAndCopy :: DepContext -> DepGraphAug
-- insertAndCopy (inputs, node, SNode stm, outputs) g =
--   do
--     new_stm <- trace "I get here!!!\n" $ makeCopies stm
--     let new_g = (&) (inputs, node, SNode new_stm, outputs) g
--     -- genEdges [(node, SNode new_stm)] (\x -> zip newly_fused $ map Cons newly_fused)  new_g
--     pure new_g
--   where
--     newly_fused = namesToList . consumedInStm . Alias.analyseStm mempty $ stm
-- insertAndCopy c g = pure $ (&) c g


-- tal -> (x -> (x, tal))

-- (x -> (x, tal)) -> (tal -> (x -> (x, tal))) -> (x -> (x, tal))



-- (tal, x) -> (tal, x)

makeCopies :: Stm SOACS -> FusionEnvM (Stm SOACS)
makeCopies s@(Let _ _ (Op (Futhark.Screma _ _  (ScremaForm _ _ lam)))) =
  do
    let fused_inner = namesToList $ consumedByLambda $ Alias.analyseLambda mempty lam
    makeCopiesInStm fused_inner s


makeCopies stm = pure stm

makeCopiesInStm :: [VName] -> Stm SOACS -> FusionEnvM (Stm SOACS)
makeCopiesInStm toCopy (Let sz os (Op st@(Screma szi is (Futhark.ScremaForm scan red lam)))) =
  do
    newLam <- makeCopiesInLambda toCopy lam
    pure $ Let sz os (Op (Screma szi is (Futhark.ScremaForm scan red newLam)))
makeCopiesInStm _ s = pure s


makeCopiesInLambda :: [VName] -> Lambda SOACS -> FusionEnvM (Lambda SOACS)
makeCopiesInLambda toCopy lam =
  do
    oldScope <- gets scope
    modify (\s -> s {scope = M.union (scopeOf lam) oldScope})
    (copies, nameMap) <- makeCopyStms toCopy
    let l_body = lambdaBody lam
    let newBody = insertStms (stmsFromList copies) (substituteNames nameMap l_body)
    let newLambda = lam {lambdaBody = newBody}
    pure newLambda


makeCopyStms :: [VName] -> FusionEnvM ([Stm SOACS], M.Map VName VName)
makeCopyStms toCopy = do
  newNames <- mapM makeNewName toCopy
  copies  <-  mapM (\ (name, name_fused) -> mkLetNames [name_fused] (BasicOp $ Copy name)) (zip toCopy newNames)
  pure (copies, makeMap toCopy newNames)
    where
    makeNewName name = newVName $ baseString name <> "_copy"





-- for horizontal fusion
tryFuseNodesInGraph2 :: Node -> Node -> DepGraphAug
tryFuseNodesInGraph2 node_1 node_2 g
  | not (gelem node_1 g && gelem node_2 g) = pure g
  | otherwise = do
    b <- hFusionFeasability g node_1 node_2
    if b then case fuseContexts2 (context g node_1) (context g node_2) of
      Just new_Context -> contractEdge node_2 new_Context g
      Nothing -> pure g
    else pure g


fuseContexts2 :: DepContext -> DepContext -> Maybe DepContext
-- fuse the nodes / contexts
fuseContexts2 c1@(_, _, SNode s1 _, _)
              c2@(_, _, SNode s2 _, _)
            = case hFuseStms s1 s2 of
              Just s3 -> Just (mergedContext (SNode s3 mempty) c1 c2)
              Nothing -> Nothing
fuseContexts2 _ _ = Nothing


fuseContexts :: [VName] -> DepContext -> DepContext -> Maybe DepContext
-- fuse the nodes / contexts
fuseContexts infusable
            c1@(_, _, SNode s1 _, _)
            c2@(_, _, SNode s2 _, _)
            = case fuseStms infusable s1 s2 of
              Just s3 -> Just (mergedContext (SNode s3 mempty) c1 c2)
              Nothing -> Nothing
fuseContexts _ _ _ = Nothing


fuseStms :: [VName] ->  Stm SOACS -> Stm SOACS -> Maybe (Stm SOACS)
fuseStms infusible s1 s2 =
  case (s1, s2) of
    (Let pats1 aux1 (Op (Futhark.Screma  s_exp1 i1  (ScremaForm scans_1 red_1 lam_1))),
     Let pats2 aux2 (Op (Futhark.Screma  s_exp2 i2  (ScremaForm scans_2 red_2 lam_2))))
     | s_exp1 == s_exp2
     ->
          Just $ Let (basicPat ids)  (aux1 <> aux2) (Op (Futhark.Screma s_exp2 fused_inputs
            (ScremaForm (scans_1 ++ scans_2) (red_1 ++ red_2) lam)))
      where
        (o1, o2) = mapT (patNames . stmPat) (s1, s2)
        (lam_1_inputs, lam_2_inputs) = mapT boundByLambda (lam_1, lam_2)
        (lam_1_output, lam_2_output) = mapT (namesFromRes . resFromLambda) (lam_1, lam_2)

        fused_inputs = fuseInputs2 o1 i1 i2
        lparams = changeAll (i1 ++ i2)
          (lambdaParams lam_1 ++ lambdaParams lam_2)
          fused_inputs


        (scan_in_size_1, scan_in_size_2) = mapT scanInput (scans_1, scans_2)
        (red_in_size_1, red_in_size_2) = mapT redInput (red_1, red_2)

        (scan_inputs_1, red_inputs_1, lambda_outputs_1) = splitAt3 scan_in_size_1 red_in_size_1 lam_1_output
        (scan_inputs_2, red_inputs_2, lambda_outputs_2) = splitAt3 scan_in_size_2 red_in_size_2 lam_2_output

        fused_lambda_outputs = concat [
          scan_inputs_1 ++ scan_inputs_2,
          red_inputs_1 ++ red_inputs_2,
          lambda_outputs_1 ++  lambda_outputs_2]

        (types, body_res) = unzip $ changeAll (lam_1_output ++ lam_2_output) (
          zip (lambdaReturnType lam_1) (resFromLambda lam_1) ++
          zip (lambdaReturnType lam_2) (resFromLambda lam_2)) fused_lambda_outputs
          -- fuseOutputs2 infusible lambda_outputs_1 lambda_outputs_2

        (scan_outputs_1, red_outputs_1, lambda_used_outputs_1) = splitAt3 (Futhark.scanResults scans_1) (Futhark.redResults red_1) o1
        (scan_outputs_2, red_outputs_2, lambda_used_outputs_2) = splitAt3 (Futhark.scanResults scans_2) (Futhark.redResults red_2) o2

        fused_outputs = concat [
          scan_outputs_1,        scan_outputs_2,
          red_outputs_1,         red_outputs_2,
          lambda_used_outputs_1, lambda_used_outputs_2]


        ids = changeAll (o1 ++ o2) (patIdents pats1 ++ patIdents pats2) fused_outputs

        -- (scan_res_1, red_res_1, map_res_1)  = splitAt3 (Futhark.scanResults scans_1) Int ([a]) [, Futhark.redResults red_1, length lam_1_output]
        -- out_sizes_2  = [Futhark.scanResults scans_2, Futhark.redResults red_2, length lam_2_output]

        -- fused_outputs = interweave (fuseOutputs2 infusible)
        --           (chunks out_sizes_1 o1)
        --           (chunks out_sizes_2 o2)


        -- (ids, types, body_res) = unzip3 $ change_all (o1 ++ o2) (
        --   zip3 (patIdents pats1) (lambdaReturnType lam_1) (resFromLambda lam_1) ++
        --   zip3 (patIdents pats2) (lambdaReturnType lam_2) (resFromLambda lam_2))
        --   fused_outputs

        fused_inputs_inner = changeAll (i1 ++ i2) (lam_1_inputs ++ lam_2_inputs) fused_inputs


        map1 = makeMap (lam_1_inputs ++ lam_2_inputs) (i1 ++ i2)
        map2 = makeMap o1 lam_1_output
        map4 = makeMap fused_inputs fused_inputs_inner
        map3 = fuseMaps map1 (M.union map2 map4)


        lam' = fuseLambda lam_1 lam_2
        lam = substituteNames map3 $ lam' {
          lambdaParams = lparams,
          lambdaReturnType = types,
          lambdaBody = (lambdaBody lam') {bodyResult = body_res}
          }
    -- vertical map-scatter fusion
    ( Let _ aux1 (Op (Futhark.Screma  s_exp1 i1 (ScremaForm [] [] lam_1))),
      Let pats2 aux2 (Op (Futhark.Scatter s_exp2 i2 lam_2 other)))
      | L.null infusible -- only if map outputs are used exclusivly by the scatter
      && s_exp1 == s_exp2
      -> Just $ Let pats2  (aux1 <> aux2) (Op (Futhark.Scatter s_exp2 fused_inputs lam other))
        where
        (o1, o2) = mapT (patNames . stmPat) (s1, s2)
        (lam, fused_inputs) = vFuseLambdas lam_1 i1 o1 lam_2 i2 o2
    -- vertical map-histogram fusion
    ( Let _ aux1 (Op (Futhark.Screma s_exp1 i1 (ScremaForm [] [] lam_1))),
      Let pats2 aux2 (Op (Futhark.Hist   s_exp2 i2 other lam_2)))
      | L.null infusible -- only if map outputs are used exclusivly by the hist
      && s_exp1 == s_exp2
        -> Just $ Let pats2 (aux1 <> aux2) (Op (Futhark.Hist s_exp2 fused_inputs other lam))
          where
            (o1, o2) = mapT (patNames . stmPat) (s1, s2)
            (lam, fused_inputs) = vFuseLambdas lam_1 i1 o1 lam_2 i2 o2
    _ -> Nothing

-- should handle all fusions of lambda at some ponit - now its only perfect fusion
vFuseLambdas :: Lambda SOACS -> [VName] -> [VName] ->
                Lambda SOACS -> [VName] -> [VName]
                -> (Lambda SOACS, [VName])
vFuseLambdas lam_1 i1 o1 lam_2 i2 _ = (lam , fused_inputs)
  where
    (lam_1_inputs, lam_2_inputs) = mapT boundByLambda (lam_1, lam_2)
    (lam_1_output, _) = mapT (namesFromRes . resFromLambda) (lam_1, lam_2)

    fused_inputs = trace ("things: " ++ show (fuseInputs2 o1 i1 i2)) fuseInputs2 o1 i1 i2
    fused_inputs_inner = changeAll (i1 ++ i2) (lam_1_inputs ++ lam_2_inputs) fused_inputs

    map1 = makeMap (lam_1_inputs ++ lam_2_inputs) (i1 ++ i2)
    map2 = makeMap o1 lam_1_output
    map4 = makeMap fused_inputs fused_inputs_inner
    map3 = fuseMaps map1 (M.union map2 map4)



    lparams = changeAll (i1 ++ i2)
      (lambdaParams lam_1 ++ lambdaParams lam_2)
      fused_inputs

    lam' = fuseLambda lam_1 lam_2
    lam = substituteNames map3 (lam' {
      lambdaParams = lparams
      })



makeMap :: Ord a => [a] -> [b] -> M.Map a b
makeMap x y = M.fromList $ zip x y

fuseMaps :: Ord b => M.Map a b -> M.Map b c -> M.Map a c
fuseMaps m1 m2 = M.mapMaybe (`M.lookup` m2 ) m1


changeAll :: Ord b => [b] -> [a] -> [b] -> [a]
changeAll orig_names orig_other = mapMaybe (mapping M.!)
  where
      mapping = M.map Just $ makeMap orig_names orig_other




resFromLambda :: Lambda rep -> Result
resFromLambda =  bodyResult . lambdaBody


hFuseStms :: Stm SOACS -> Stm SOACS -> Maybe (Stm SOACS)
hFuseStms s1 s2 = case (s1, s2) of
  (Let pats1 _ (Op Futhark.Screma {}),
   Let _     _ (Op Futhark.Screma {})) -> fuseStms (patNames pats1) s1 s2
  (Let pats1 aux1 (Op (Futhark.Scatter s_exp1 i1 lam_1 outputs1)),
   Let pats2 aux2 (Op (Futhark.Scatter s_exp2 i2 lam_2 outputs2)))
   | s_exp1 == s_exp2 ->
     Just $ Let pats aux (Op (Futhark.Scatter s_exp2 fused_inputs lam outputs))
      where
        pats = pats1 <> pats2
        aux = aux1 <> aux2
        outputs = outputs1 <> outputs2
        --o1 = patNames pats1

        (lam_1_inputs, lam_2_inputs) = mapT boundByLambda (lam_1, lam_2)
        (lam_1_output, lam_2_output) = mapT resFromLambda (lam_1, lam_2)

        fused_inputs = fuseInputs2 [] i1 i2
        fused_inputs_inner = changeAll (i1 ++ i2) (lam_1_inputs ++ lam_2_inputs) fused_inputs

        map1 = makeMap (lam_1_inputs ++ lam_2_inputs) (i1 ++ i2)
        map4 = makeMap fused_inputs fused_inputs_inner
        map3 = fuseMaps map1 map4

        lam' = fuseLambda lam_1 lam_2

        lparams = changeAll (i1 ++ i2)
          (lambdaParams lam_1 ++ lambdaParams lam_2)
          fused_inputs

        (types1, types2) = mapT lambdaReturnType (lam_1, lam_2)
        (res1, res2) = mapT resFromLambda (lam_1, lam_2)

        (ids1, vals1) = splitScatterResults outputs1 (zip3 types1 res1 lam_1_output)
        (ids2, vals2) = splitScatterResults outputs2 (zip3 types2 res2 lam_2_output)
        (types, res, _) = unzip3 $ ids1 ++ ids2 ++ vals1 ++ vals2

        lam = substituteNames map3 $ lam' {
          lambdaParams = lparams,
          lambdaReturnType = types,
          lambdaBody = (lambdaBody lam') {bodyResult = res}
          }


  _ -> Nothing



fuseInputs2 :: [VName] -> [VName] -> [VName] -> [VName]
fuseInputs2 fusing inputs1 inputs2 =
   L.nub $ inputs1 `L.union` filter (`notElem` fusing) inputs2

-- fuseOutputs2 :: [VName] -> [VName] -> [VName] -> [VName]
-- fuseOutputs2 infusible outputs1 outputs2 =
--    outputs2 `L.union` filtered_outputs
--   where
--     filtered_outputs = filter (`elem` infusible) outputs1


fuseLambda :: Lambda SOACS  -> -- [VName] -> [VName] ->
              Lambda SOACS -- ->[VName]
              -> Lambda SOACS
fuseLambda lam_1 lam_2  =
  -- Lambda inputs_new body_new output_types_new
  lam_2 {lambdaBody = l_body_new}
  where
    l_body_1 = lambdaBody lam_1
    l_body_2 = lambdaBody lam_2
    l_body_new = insertStms (bodyStms l_body_1) l_body_2

    -- lam_1_inputs = boundByLambda lam_1
    -- lam_2_inputs = boundByLambda lam_2
    -- lam_1_output = namesFromRes $ bodyResult $ lambdaBody lam_1

    -- map1 = makeMap lam_2_inputs i2
    -- map2 = makeMap o1 lam_1_output
    -- map3 = makeMap i1 lam_1_inputs
    -- map4 = fuse_maps map1 (M.union map2 map3)

    -- fuse_maps :: M.Map a b -> M.Map b c -> M.Map a c
    -- fuse_maps m1 m2 = M.mapMaybe (m2 `M.lookup`) m1

    -- makeMap :: [a] -> [b] -> M.Map a b
    -- makeMap x y = M.fromList $ zip x y

-- the same as that fixed-point function in util
keepTrying :: DepGraphAug -> DepGraphAug
keepTrying f g =
  do
  r  <- f g
  r2 <- f r
  if equal r r2 then pure r
  else keepTrying f r2

-- substituteNames


-- getstms

removeUnusedOutputs :: DepGraphAug
removeUnusedOutputs = mapAcross removeUnusedOutputsFromContext

vNameFromAdj :: Node -> (EdgeT, Node) -> [VName]
vNameFromAdj n1 (edge, n2) = depsFromEdge (n2,n1, edge)


removeUnusedOutputsFromContext :: DepContext  -> FusionEnvM DepContext
removeUnusedOutputsFromContext (incoming, n1, SNode s outputTs, outgoing) =
  pure (incoming, n1, SNode new_stm outputTs, outgoing)
  where
    new_stm = removeOutputsExcept (concatMap (vNameFromAdj n1) incoming) s
removeUnusedOutputsFromContext c = pure c

removeOutputsExcept :: [VName] -> Stm SOACS -> Stm SOACS
removeOutputsExcept toKeep s = case s of
  (Let pats1 aux1 (Op (Futhark.Screma size_exp i1  (ScremaForm scans_1 red_1 lam_1)))) ->
     Let (basicPat (pats_unchanged ++ pats_new)) aux1 (Op (Futhark.Screma size_exp i1  (ScremaForm scans_1 red_1 lam_new)))
        where
          scan_input_size = scanInput scans_1
          red_input_size = redInput red_1
          scan_output_size = Futhark.scanResults scans_1
          red_outputs_size = Futhark.redResults red_1

          (pats_unchanged, pats_toChange) = splitAt (scan_output_size + red_outputs_size) (patIdents pats1)
          (res_unchanged, res_toChange) = splitAt (scan_input_size + red_input_size) (zip (resFromLambda lam_1) (lambdaReturnType lam_1))

          (pats_new, other) = unzip $ filter (\(x, _) -> identName x  `elem` toKeep) (zip pats_toChange res_toChange)
          (results, types) = unzip (res_unchanged ++ other)
          lam_new = trace ("getshere: " ++ show toKeep) lam_1 {
            lambdaReturnType = types,
            lambdaBody = (lambdaBody lam_1) {bodyResult = results}
            }
  stm -> stm


mapAcross :: (DepContext -> FusionEnvM DepContext) -> DepGraphAug
mapAcross f g =
  do
    let ns = nodes g
    foldlM (flip helper) g ns
    where
      helper :: Node -> DepGraphAug
      helper n gr = case match n gr of
        (Just c, g') ->
          do
            c' <- f c
            return $ c' & g'
        (Nothing, g') -> pure g'


-- do fusion on the inner nodes -
runInnerFusion :: DepGraphAug
runInnerFusion = mapAcross runInnerFusionOnContext

runInnerFusionOnContext :: DepContext -> FusionEnvM DepContext
runInnerFusionOnContext c@(incomming, node, nodeT, outgoing) = case nodeT of
  SNode (Let pats aux (If size_exp b1 b2 branchType )) _ ->
    do
      b1_new <- doFusionInner b1 []
      b2_new <- doFusionInner b2 []
      return (incomming, node, SNode (Let pats aux (If size_exp b1_new b2_new branchType)) mempty, outgoing)
  SNode (Let pats aux (DoLoop params form body)) _ ->
    do
      oldScope <- gets scope
      modify (\s -> s {scope = M.union (scopeOfFParams (map fst params)) oldScope})
      b_new <- doFusionInner body (map (paramName . fst) params)
      return (incomming, node, SNode (Let pats aux (DoLoop params form b_new)) mempty, outgoing)
  SNode (Let pats aux (Op (Futhark.Screma is sz (ScremaForm [] [] lambda)))) _ ->
    do
      newbody <- doFusionInner (lambdaBody lambda) []
      let newLam = lambda {lambdaBody = newbody}
      let nodeNew = SNode (Let pats aux (Op (Futhark.Screma is sz (ScremaForm [] [] newLam)))) mempty
      pure (incomming, node, nodeNew, outgoing)
  _ -> return c
  where
    doFusionInner :: Body SOACS -> [VName] -> FusionEnvM (Body SOACS)
    doFusionInner b extraInputs =
      do
        new_stms <- fuseGraph stms results inputs
        return b {bodyStms = stmsFromList new_stms}
      where
        inputs = concatMap (vNameFromAdj node) outgoing ++ extraInputs
        stms = stmsToList (bodyStms b)
        results = namesFromRes (bodyResult b)

-- what about inner lambdas??????


isAlias :: EdgeT -> Bool
isAlias (Alias _) = True
isAlias _ = False

isFake :: EdgeT -> Bool
isFake (Fake _) = True
isFake _ = False

makeCopiesOfConsAliased :: DepGraphAug
makeCopiesOfConsAliased = mapAcross copyAlised
  where
    copyAlised :: DepContext -> FusionEnvM DepContext
    copyAlised c@(incoming, node, SNode s _, outgoing) = do
      let incoming' = concatMap depsFromEdgeT $ filter isFake (map fst incoming)
      let outgoing' = concatMap depsFromEdgeT $ filter isAlias (map fst outgoing)
      let toMakeCopies =  incoming' `L.intersect` outgoing'
      if not $ null toMakeCopies then do
        (new_stms, nameMapping) <- makeCopyStms toMakeCopies
        return (incoming, node, FinalNode (new_stms ++ [substituteNames nameMapping s]), outgoing)
      else pure c
    copyAlised c = pure c

-- make the context to context function and add that on top d




-- makeCopy :: MonadFreshNames m => DepGraph -> m DepGraph
-- makeCopy g =
--   return g


-- makeCopyContext :: MonadFreshNames m => DepContext -> m DepContext
-- makeCopyContext c = return c


-- Problems with copying fusion
-- need new Cons-edges + fake edges
--
