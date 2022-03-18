{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use intercalate" #-}
{-# LANGUAGE LambdaCase #-}

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
import qualified Data.Set as S
import qualified Futhark.Analysis.Alias as Alias
import qualified Futhark.Analysis.HORep.SOAC as SOAC
import Futhark.Construct
import qualified Futhark.IR.Aliases as Aliases
import Futhark.IR.Prop.Aliases
import Futhark.IR.SOACS hiding (SOAC (..))
import qualified Futhark.IR.SOACS as Futhark
import Futhark.IR.SOACS.Simplify
-- import Futhark.Optimise.Fusion.LoopKernel
import Futhark.Pass
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util (maxinum, chunks, splitAt3)

import Futhark.Optimise.GraphRep
import qualified Data.Graph.Inductive.Query.DFS as Q
import Data.Graph.Inductive.Graph
-- import qualified Control.Monad.Identity as Control.Monad

import Debug.Trace
import Futhark.IR.Aliases (VName(VName))
import Futhark.Optimise.Fusion.LoopKernel (FusedKer(fusedVars))
import Data.Tuple (swap)
import Data.List (deleteFirstsBy)
import Data.FileEmbed (bsToExp)
import GHC.TopHandler (runNonIO)
import qualified Futhark.Util as L
import Control.Monad (foldM)
import Data.Foldable (foldlM)


-- unofficial TODO
-- lengths of inputs
-- name generations is going to have to occur


-- extra util
scan_input :: [Scan SOACS] -> Int
scan_input l = flip div 2 $ sum (map (length . lambdaParams . scanLambda) l)
red_input :: [Reduce rep] -> Int
red_input l = flip div 2 $ sum (map (length . lambdaParams . redLambda) l)






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
    new_stms <- fuseGraph stmList results []
    return $ stmsFromList new_stms
  where
    stmList = stmsToList stms
    results = varsRes outputs



-- some sort of functional decorator pattern
fuseFun :: Stms SOACS -> FunDef SOACS -> PassM (FunDef SOACS)
fuseFun _stmts fun = do
  new_stms <- fuseGraph stms res (funDefParams  fun)
  let body = (funDefBody fun) {bodyStms = stmsFromList new_stms}
  return fun {funDefBody = body}
    where
      b   = funDefBody fun
      stms = trace (ppr (bodyStms b)) $ stmsToList $ bodyStms b
      res = bodyResult b

fuseGraph :: [Stm SOACS] -> Result -> [FParam SOACS] -> PassM [Stm SOACS]
fuseGraph stms results inputs =
  do
    graph_not_fused <- mkDepGraph stms results (trace (show inputs) inputs)
    let graph_not_fused' = trace (pprg graph_not_fused) graph_not_fused
    graph_fused <-  doAllFusion graph_not_fused'
    let graph_fused' = trace (pprg graph_fused) graph_fused
    let stms_new = linearizeGraph graph_fused'
    return $ trace (unlines (map ppr stms_new)) stms_new



linearizeGraph :: DepGraph -> [Stm SOACS]
linearizeGraph g = reverse $ mapMaybe stmFromNode $ Q.topsort' g

doAllFusion :: DepGraphAug
doAllFusion = applyAugs [keeptrying doMapFusion, doHorizontalFusion, removeUnusedOutputs, runInnerFusion]

doInnerFusion :: DepGraphAug
doInnerFusion g = pure g


-- map-fusion part

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




doMapFusion :: DepGraphAug
-- go through each node and attempt a map fusion
doMapFusion g = applyAugs (map tryFuseNodeInGraph $ labNodes g) g

doHorizontalFusion :: DepGraphAug
doHorizontalFusion g = applyAugs (map  horizontalFusionOnNode (nodes g)) g

  -- for each node, find what came before, attempt to fuse

horizontalFusionOnNode :: Node -> DepGraphAug
horizontalFusionOnNode node g = try_fuse_all incoming_nodes g
  where
    (incoming_nodes, _) = unzip $ lpre g node


try_fuse_all :: [Node] -> DepGraphAug
try_fuse_all nodes = applyAugs (map (uncurry tryFuseNodesInGraph2) pairs)
  where
    pairs = [(x, y) | x <- nodes, y <- nodes,  x < y]



-- contextFromLNode :: DepGraph -> DepNode -> DepContext
-- contextFromLNode g lnode = context g $ nodeFromLNode lnode

isRes :: (Node, EdgeT) -> Bool
isRes (_,Res _) = True
isRes _ = False

isDep :: EdgeT -> Bool
isDep (Dep _) = True
isDep (InfDep _) = True
isDep (Res _) = True -- unintuitive, but i think it works
isDep _ = False

isInf :: (Node, Node, EdgeT) -> Bool
isInf (_,_,e) = case e of
  InfDep vn -> True
  (Cons _) -> False -- you would think this sholud be true - but mabye this works
  Fake -> True
  _ -> False

v_fusion_feasability :: DepGraph -> Node -> Node -> Bool
v_fusion_feasability g n1 n2 =
  (all isDep edges || all (==n2) nodesN1 )&&
  not (any isInf (edgesBetween g n1 n2)) &&
  (all (==n2) nodesN1 || all (==n1) nodesN2)
  where
    (nodesN2, _) = unzip $ filter (not . isRes) $ lsuc g n2
    (nodesN1, edges) = unzip $ filter (not . isRes) $ lpre g n1



d_fusion_feasability :: DepGraph -> Node -> Node -> Bool
d_fusion_feasability g n1 n2 = lessThanOneDep n1 && lessThanOneDep n2
  where
    lessThanOneDep n =
      let (nodes, _) = unzip $ filter (not . isRes) $ lsuc g n
      in (<=1) $ length (L.nub nodes)
  -- what are the rules here?
  --  - they have an input in common
  --  - they only have that input? -


tryFuseNodeInGraph :: DepNode -> DepGraphAug
tryFuseNodeInGraph node_to_fuse g =
  if gelem node_to_fuse_id g
  then applyAugs (map (tryFuseNodesInGraph node_to_fuse_id) fuses_with) g
  else pure g
  where
    fuses_with = map nodeFromLNode $ output g node_to_fuse
    node_to_fuse_id = nodeFromLNode node_to_fuse

tryFuseNodesInGraph :: Node -> Node -> DepGraphAug
-- find the neighbors
-- check that they have no other dependencies
-- fuse them
tryFuseNodesInGraph node_1 node_2 g
  | not (gelem node_1 g && gelem node_2 g) = pure g
  | v_fusion_feasability g node_1 node_2 =
    case fuseContexts infusable_nodes (context g node_1) (context g node_2) of
      Just new_Context -> pure $ (&) new_Context $ delNodes [node_1, node_2] g
      Nothing -> pure g
  | otherwise = pure g
    where
      -- sorry about this
      infusable_nodes = concatMap depsFromEdge
        (concatMap (edgesBetween g node_1) (filter (/=node_2) $ pre g node_1))
                                  -- L.\\ edges_between g node_1 node_2)

-- for horizontal fusion
tryFuseNodesInGraph2 :: Node -> Node -> DepGraphAug
tryFuseNodesInGraph2 node_1 node_2 g
  | not (gelem node_1 g && gelem node_2 g) = pure g
  | d_fusion_feasability g node_1 node_2 =
    case fuseContexts infusable_nodes (context g node_1) (context g node_2) of
      Just new_Context -> pure $ (&) new_Context $ delNodes [node_1, node_2] g
      Nothing -> pure  g
  | otherwise = pure g
    where
      -- sorry about this
      infusable_nodes = concatMap depsFromEdge
        (concatMap (edgesBetween g node_1) (filter (/=node_2) $ pre g node_1))




fuseContexts :: [VName] -> DepContext -> DepContext -> Maybe DepContext
-- fuse the nodes / contexts
fuseContexts infusable
            (inp1, n1, SNode s1, outp1)
            (inp2, n2, SNode s2, outp2)
            = case fuseStms infusable s1 s2 of
              Just s3 -> Just (new_inp, n1, SNode s3, new_outp)
              Nothing -> Nothing
  where
    new_inp  = L.nub $ filter (\n -> snd n /= n1 && snd n /= n2) (inp1  `L.union` inp2)
    new_outp = L.nub $ filter (\n -> snd n /= n1 && snd n /= n2) (outp1 `L.union` outp2)
fuseContexts _ _ _ = Nothing


fuseStms :: [VName] ->  Stm SOACS -> Stm SOACS -> Maybe (Stm SOACS)
fuseStms infusible s1 s2 =
  case (s1, s2) of
    (Let pats1 aux1 (Op (Futhark.Screma  _     i1  (ScremaForm scans_1 red_1 lam_1))),
     Let pats2 aux2 (Op (Futhark.Screma  s_exp i2  (ScremaForm scans_2 red_2 lam_2)))) ->
          Just $ Let (basicPat ids) aux2 (Op (Futhark.Screma s_exp fused_inputs
            (ScremaForm (scans_1 ++ scans_2) (red_1 ++ red_2) lam)))
      where
        (o1, o2) = mapT (patNames . stmPat) (s1, s2)
        (lam_1_inputs, lam_2_inputs) = mapT boundByLambda (lam_1, lam_2)
        (lam_1_output, lam_2_output) = mapT (namesFromRes . res_from_lambda) (lam_1, lam_2)

        fused_inputs = fuse_inputs2 o1 i1 i2
        lparams = trace (show fused_inputs ++ "vs" ++ show i1) $ change_all (i1 ++ i2)
          (lambdaParams lam_1 ++ lambdaParams lam_2)
          fused_inputs


        (scan_in_size_1, scan_in_size_2) = mapT scan_input (scans_1, scans_2)
        (red_in_size_1, red_in_size_2) = mapT red_input (red_1, red_2)

        (scan_inputs_1, red_inputs_1, lambda_outputs_1) = splitAt3 scan_in_size_1 red_in_size_1 lam_1_output
        (scan_inputs_2, red_inputs_2, lambda_outputs_2) = splitAt3 scan_in_size_2 red_in_size_2 lam_2_output

        fused_lambda_outputs = concat [
          scan_inputs_1 ++ scan_inputs_2,
          red_inputs_1 ++ red_inputs_2,
          lambda_outputs_1 ++  lambda_outputs_2]

        (types, body_res) = unzip $ change_all (lam_1_output ++ lam_2_output) (
          zip (lambdaReturnType lam_1) (res_from_lambda lam_1) ++
          zip (lambdaReturnType lam_2) (res_from_lambda lam_2)) fused_lambda_outputs
          -- fuse_outputs2 infusible lambda_outputs_1 lambda_outputs_2

        (scan_outputs_1, red_outputs_1, lambda_used_outputs_1) = splitAt3 (Futhark.scanResults scans_1) (Futhark.redResults red_1) o1
        (scan_outputs_2, red_outputs_2, lambda_used_outputs_2) = splitAt3 (Futhark.scanResults scans_2) (Futhark.redResults red_2) o2

        fused_outputs = concat [
          scan_outputs_1 ++ scan_outputs_2,
          red_outputs_1 ++ red_outputs_2,
          lambda_used_outputs_1 ++  lambda_used_outputs_2]


        ids = change_all (o1 ++ o2) (patIdents pats1 ++ patIdents pats2) fused_outputs

        -- (scan_res_1, red_res_1, map_res_1)  = splitAt3 (Futhark.scanResults scans_1) Int ([a]) [, Futhark.redResults red_1, length lam_1_output]
        -- out_sizes_2  = [Futhark.scanResults scans_2, Futhark.redResults red_2, length lam_2_output]

        -- fused_outputs = interweave (fuse_outputs2 infusible)
        --           (chunks out_sizes_1 o1)
        --           (chunks out_sizes_2 o2)


        -- (ids, types, body_res) = unzip3 $ change_all (o1 ++ o2) (
        --   zip3 (patIdents pats1) (lambdaReturnType lam_1) (res_from_lambda lam_1) ++
        --   zip3 (patIdents pats2) (lambdaReturnType lam_2) (res_from_lambda lam_2))
        --   fused_outputs

        map1 = makeMap (lam_1_inputs ++ lam_2_inputs) (i1 ++ i2)
        map2 = makeMap o1 lam_1_output
        map4 = makeMap i1 lam_1_inputs
        map3 = fuse_maps map1 (M.union map2 map4)


        lam' = fuseLambda lam_1 lam_2
        lam = substituteNames map3 $ lam' {
          lambdaParams = lparams,
          lambdaReturnType = types,
          lambdaBody = (lambdaBody lam') {bodyResult = body_res}
          }
    -- vertical map-scatter fusion
    ( Let pats1 aux1 (Op (Futhark.Screma  _ i1 (ScremaForm [] [] lam_1))),
      Let pats2 aux2 (Op (Futhark.Scatter exp i2 lam_2 other)))
      | L.null infusible -- only if map outputs are used exclusivly by the scatter
      -> Just $ Let pats2 aux2 (Op (Futhark.Scatter exp fused_inputs lam other))
        where
        (o1, o2) = mapT (patNames . stmPat) (s1, s2)
        (lam, fused_inputs) = vFuseLambdas lam_1 i1 o1 lam_2 i2 o2
    -- vertical map-histogram fusion
    ( Let pats1 aux1 (Op (Futhark.Screma  _ i1 (ScremaForm [] [] lam_1))),
      Let pats2 aux2 (Op (Futhark.Hist exp i2 other lam_2)))
      | L.null infusible -- only if map outputs are used exclusivly by the scatter
        -> Just $ Let pats2 aux2 (Op (Futhark.Hist exp fused_inputs other lam))
          where
            (o1, o2) = mapT (patNames . stmPat) (s1, s2)
            (lam, fused_inputs) = vFuseLambdas lam_1 i1 o1 lam_2 i2 o2
    _ -> Nothing

-- should handle all fusions of lambda at some ponit - now its only perfect fusion
vFuseLambdas :: Lambda SOACS -> [VName] -> [VName] ->
                Lambda SOACS -> [VName] -> [VName]
                -> (Lambda SOACS, [VName])
vFuseLambdas lam_1 i1 o1 lam_2 i2 o2 = (lam , fused_inputs)
  where
    (lam_1_inputs, lam_2_inputs) = mapT boundByLambda (lam_1, lam_2)
    (lam_1_output, lam_2_output) = mapT (namesFromRes . res_from_lambda) (lam_1, lam_2)

    map1 = makeMap lam_2_inputs i2
    map2 = makeMap o1 lam_1_output
    map4 = makeMap i1 lam_1_inputs
    map3 = fuse_maps map1 (M.union map2 map4)

    fused_inputs = fuse_inputs2 o1 i1 i2

    lparams = change_all (i1 ++ i2)
      (lambdaParams lam_1 ++ lambdaParams lam_2)
      fused_inputs

    lam' = fuseLambda lam_1 lam_2
    lam = substituteNames map3 $ lam' {
      lambdaParams = lparams
      }



makeMap :: Ord a => [a] -> [b] -> M.Map a b
makeMap x y = M.fromList $ zip x y

fuse_maps :: Ord b => M.Map a b -> M.Map b c -> M.Map a c
fuse_maps m1 m2 = M.mapMaybe (`M.lookup` m2 ) m1


change_all :: Ord b => [b] -> [a] -> [b] -> [a]
change_all orig_names orig_other = mapMaybe (mapping M.!)
  where
      mapping = M.map Just $ makeMap orig_names orig_other


mapT :: (a -> b) -> (a,a) -> (b,b)
mapT f (a,b) = (f a, f b)

res_from_lambda :: Lambda rep -> Result
res_from_lambda =  bodyResult . lambdaBody


-- nvm horizontal fusion on its own


fuse_inputs2 :: [VName] -> [VName] -> [VName] -> [VName]
fuse_inputs2 fusing inputs1 inputs2 =
   L.nub $ inputs1 `L.union` filter (`notElem` fusing) inputs2

fuse_outputs2 :: [VName] -> [VName] -> [VName] -> [VName]
fuse_outputs2 infusible outputs1 outputs2 =
   outputs2 `L.union` filtered_outputs
  where
    filtered_outputs = filter (`elem` infusible) outputs1


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
keeptrying :: DepGraphAug -> DepGraphAug
keeptrying f g =
  do
  r  <- f g
  r2 <- f r
  if equal r r2 then pure r
  else keeptrying f r2

-- substituteNames


-- getstms

removeUnusedOutputs :: DepGraphAug
removeUnusedOutputs g = pure $ gmap (removeUnusedOutputsFromContext g) g

vNameFromAdj :: Node -> (EdgeT, Node) -> [VName]
vNameFromAdj n1 (edge, n2) = depsFromEdge (n2,n1, edge)


removeUnusedOutputsFromContext :: DepGraph -> DepContext  -> DepContext
removeUnusedOutputsFromContext g (incoming, n1, SNode s, outgoing) =
  (incoming, n1, SNode new_stm, outgoing)
  where
    new_stm = removeOutputsExcept (concatMap (vNameFromAdj n1) incoming) s
removeUnusedOutputsFromContext _ context = context

removeOutputsExcept :: [VName] -> Stm SOACS -> Stm SOACS
removeOutputsExcept toKeep s = case s of
  (Let pats1 aux1 (Op (Futhark.Screma  size i1  (ScremaForm scans_1 red_1 lam_1)))) ->
     Let (basicPat (pats_unchanged ++ pats_new)) aux1 (Op (Futhark.Screma  size i1  (ScremaForm scans_1 red_1 lam_new)))
        where
          scan_input_size = trace (show toKeep) $ scan_input scans_1
          red_input_size = red_input red_1
          scan_output_size = Futhark.scanResults scans_1
          red_outputs_size = Futhark.redResults red_1

          (pats_unchanged, pats_toChange) = splitAt (scan_output_size + red_outputs_size) (patIdents pats1)
          (res_unchanged, res_toChange) = splitAt (scan_input_size + red_input_size) (zip (res_from_lambda lam_1) (lambdaReturnType lam_1))

          (pats_new, other) = unzip $ filter (\(x, _) -> identName x  `elem` toKeep) (zip pats_toChange res_toChange)
          (results, types) = unzip (res_unchanged ++ other)
          lam_new = lam_1 {
            lambdaReturnType = types,
            lambdaBody = (lambdaBody lam_1) {bodyResult = results}
            }
  s -> s


mapAcross :: (DepContext -> PassM DepContext) -> DepGraphAug
mapAcross f g =
  do
    let ns = nodes g
    foldlM (flip helper) g ns
    where
      helper :: Node -> DepGraphAug
      helper n g = case match n g of
        (Just c, g') ->
          do
            c' <- f c
            return $ c' & g'
        (Nothing, g') -> pure g'


-- do fusion on the inner nodes -
runInnerFusion :: DepGraphAug
runInnerFusion = mapAcross runInnerFusionOnContext

runInnerFusionOnContext :: DepContext -> PassM DepContext
runInnerFusionOnContext c@(incomming, node, nodeT, outgoing) = case nodeT of
  SNode (Let pats aux (If size b1 b2 branchType )) ->
    do
      b1_new <- doFusionInner b1
      b2_new <- doFusionInner b2
      return (incomming, node, SNode (Let pats aux (If size b1_new b2_new branchType)), outgoing)
  SNode (Let pats aux (DoLoop params form body)) ->
    do
      b_new <- doFusionInner body
      return (incomming, node, SNode (Let pats aux (DoLoop params form b_new)), outgoing)
  _ -> return c
  where
    doFusionInner :: Body SOACS -> PassM (Body SOACS)
    doFusionInner b =
      do
        graph <- mkDepGraphInner stms results inputs
        fused_graph <- doAllFusion graph
        let new_stms = linearizeGraph graph
        return b {bodyStms = stmsFromList new_stms}
      where
        inputs = concatMap (vNameFromAdj node) incomming
        stms = stmsToList (bodyStms b)
        results = namesFromRes (bodyResult b)



-- what about inner lambdas??????













-- copyNewlyConsumed ::
--   Names ->
--   Futhark.SOAC (Aliases.Aliases SOACS) ->
--   Builder SOACS (Futhark.SOAC SOACS)
-- copyNewlyConsumed was_consumed soac =
--   case soac of
--     Futhark.Screma w arrs (Futhark.ScremaForm scans reds map_lam) -> do
--       -- Copy any arrays that are consumed now, but were not in the
--       -- constituents.
--       arrs' <- mapM copyConsumedArr arrs
--       -- Any consumed free variables will have to be copied inside the
--       -- lambda, and we have to substitute the name of the copy for
--       -- the original.
--       map_lam' <- copyFreeInLambda map_lam

--       let scans' =
--             map
--               ( \scan ->
--                   scan
--                     { scanLambda =
--                         Aliases.removeLambdaAliases
--                           (scanLambda scan)
--                     }
--               )
--               scans

--       let reds' =
--             map
--               ( \red ->
--                   red
--                     { redLambda =
--                         Aliases.removeLambdaAliases
--                           (redLambda red)
--                     }
--               )
--               reds

--       return $ Futhark.Screma w arrs' $ Futhark.ScremaForm scans' reds' map_lam'
--     _ -> return $ removeOpAliases soac
--   where
--     consumed = consumedInOp soac
--     newly_consumed = consumed `namesSubtract` was_consumed

--     copyConsumedArr a
--       | a `nameIn` newly_consumed =
--         letExp (baseString a <> "_copy") $ BasicOp $ Copy a
--       | otherwise = return a

--     copyFreeInLambda lam = do
--       let free_consumed =
--             consumedByLambda lam
--               `namesSubtract` namesFromList (map paramName $ lambdaParams lam)
--       (stms, subst) <-
--         foldM copyFree (mempty, mempty) $ namesToList free_consumed
--       let lam' = Aliases.removeLambdaAliases lam
--       return $
--         if null stms
--           then lam'
--           else
--             lam'
--               { lambdaBody =
--                   insertStms stms $
--                     substituteNames subst $ lambdaBody lam'
--               }

--     copyFree (stms, subst) v = do
--       v_copy <- newVName $ baseString v <> "_copy"
--       copy <- mkLetNamesM [v_copy] $ BasicOp $ Copy v
--       return (oneStm copy <> stms, M.insert v v_copy subst)


makeCopy :: MonadFreshNames m => DepGraph -> m DepGraph
makeCopy g =
  return g


makeCopyContext :: MonadFreshNames m => DepContext -> m DepContext
makeCopyContext c = return c
