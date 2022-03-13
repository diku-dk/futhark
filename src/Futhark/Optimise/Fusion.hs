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
import Futhark.Util (maxinum, chunks)

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


-- unofficial TODO
-- lengths of inputs
-- order outputs in fusion
-- make fusion stm->stm prettier


-- | The pass definition.
fuseSOACs :: Pass SOACS SOACS
fuseSOACs =
  Pass
    { passName = "Fuse SOACs",
      passDescription = "Perform higher-order optimisation, i.e., fusion.",
      passFunction = intraproceduralTransformationWithConsts
          pure
          fuseFun
          -- (\y x -> pure x)
    }


-- some sort of functional decorator pattern
fuseFun :: Stms SOACS -> FunDef SOACS -> PassM (FunDef SOACS)
fuseFun _stmts fun = do
  let body = (funDefBody fun) {bodyStms = stmsFromList stms_new'}
  return fun {funDefBody = body}
    where
      b   = funDefBody fun
      stms = trace (ppr (bodyStms b)) $ stmsToList $ bodyStms b
      res = bodyResult b
      graph_not_fused = mkDepGraph stms res (trace (show $ funDefParams  fun) (funDefParams  fun))
      graph_not_fused' = trace (pprg graph_not_fused) graph_not_fused
      graph_fused = doHorizontalFusion $ keeptrying doMapFusion graph_not_fused'
      graph_fused' = trace (pprg graph_fused) graph_fused
      stms_new = reverse $ concatMap stmFromNode $ Q.topsort' graph_fused'
      stms_new' = trace (unlines (map ppr stms_new)) stms_new

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
doMapFusion g = applyAugs g (map tryFuseNodeInGraph $ labNodes g)

doHorizontalFusion :: DepGraphAug
doHorizontalFusion g = applyAugs g (map  horizontalFusionOnNode (nodes g))

  -- for each node, find what came before, attempt to fuse

horizontalFusionOnNode :: Node -> DepGraphAug
horizontalFusionOnNode node g = try_fuse_all incoming_nodes g
  where
    (incoming_nodes, _) = unzip $ lpre g node


try_fuse_all :: [Node] -> DepGraphAug
try_fuse_all nodes g = applyAugs g (map (uncurry tryFuseNodesInGraph2) pairs)
  where
    pairs = [(x, y) | x <- nodes, y <- nodes,  x < y]



-- contextFromLNode :: DepGraph -> DepNode -> DepContext
-- contextFromLNode g lnode = context g $ nodeFromLNode lnode

isRes :: (Node, EdgeT) -> Bool
isRes (_,Res) = True
isRes _ = False

isDep :: EdgeT -> Bool
isDep (Dep _) = True
isDep (InfDep _) = True
isDep Res = True -- unintuitive, but i think it works
isDep _ = False

isInf :: (Node, Node, EdgeT) -> Bool
isInf (_,_,e) = case e of
  InfDep vn -> True
  Cons -> True
  Fake -> True
  _ -> False

v_fusion_feasability :: DepGraph -> Node -> Node -> Bool
v_fusion_feasability g n1 n2 =
  all isDep edges &&
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
  then applyAugs g (map (tryFuseNodesInGraph node_to_fuse_id) fuses_with)
  else g
  where
    fuses_with = map nodeFromLNode $ output g node_to_fuse
    node_to_fuse_id = nodeFromLNode node_to_fuse

tryFuseNodesInGraph :: Node -> Node -> DepGraphAug
-- find the neighbors
-- check that they have no other dependencies
-- fuse them
tryFuseNodesInGraph node_1 node_2 g
  | not (gelem node_1 g && gelem node_2 g) = g
  | v_fusion_feasability g node_1 node_2 =
    case fuseContexts infusable_nodes (context g node_1) (context g node_2) of
      Just new_Context -> (&) new_Context $ delNodes [node_1, node_2] g
      Nothing -> g
  | otherwise = g
    where
      -- sorry about this
      infusable_nodes = concatMap (depsFromEdge g)
        (concatMap (edgesBetween g node_1) (filter (/=node_2) $ pre g node_1))
                                  -- L.\\ edges_between g node_1 node_2)

-- for horizontal fusion
tryFuseNodesInGraph2 :: Node -> Node -> DepGraphAug
tryFuseNodesInGraph2 node_1 node_2 g
  | not (gelem node_1 g && gelem node_2 g) = g
  | d_fusion_feasability g node_1 node_2 =
    case fuseContexts infusable_nodes (context g node_1) (context g node_2) of
      Just new_Context -> (&) new_Context $ delNodes [node_1, node_2] g
      Nothing -> g
  | otherwise = g
    where
      -- sorry about this
      infusable_nodes = concatMap (depsFromEdge g)
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
        lparams = change_all (i1 ++ i2)
          (lambdaParams lam_1 ++ lambdaParams lam_2)
          fused_inputs

        out_sizes_1  = [Futhark.scanResults scans_1, Futhark.redResults red_1, length lam_1_output]
        out_sizes_2  = [Futhark.scanResults scans_2, Futhark.redResults red_2, length lam_2_output]

        fused_outputs = interweave (fuse_outputs2 infusible)
                  (chunks out_sizes_1 o1)
                  (chunks out_sizes_2 o2)

        (ids, types, body_res) = unzip3 $ change_all (o1 ++ o2) (
          zip3 (patIdents pats1) (lambdaReturnType lam_1) (res_from_lambda lam_1) ++
          zip3 (patIdents pats2) (lambdaReturnType lam_2) (res_from_lambda lam_2))
          fused_outputs


        map1 = makeMap lam_2_inputs i2
        map2 = makeMap o1 lam_1_output
        map4 = makeMap i1 lam_1_inputs
        map3 =  fuse_maps map1 (M.union map2 map4)




        lam' = fuseLambda lam_1 lam_2
        lam = substituteNames map3 $ lam' {
          lambdaParams = lparams,
          lambdaReturnType = types,
          lambdaBody = (lambdaBody lam') {bodyResult = body_res}
          }
    -- -- vertical map-scatter fusion
    ( Let pats1 aux1 (Op (Futhark.Screma  _ i1 (ScremaForm [] [] lam_1))),
      Let pats2 aux2 (Op (Futhark.Scatter exp i2 lam_2 other)))
      | L.null infusible -- only if map outputs are used exclusivly by the scatter
      -> Just $ Let pats2 aux2 (Op (Futhark.Scatter exp fused_inputs lam other))
        where
        (o1, o2) = mapT (patNames . stmPat) (s1, s2)
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
    _ -> Nothing

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

interweave :: ([a] -> [a] -> [a]) -> [[a]] -> [[a]] -> [a]
interweave f (a : as) (b : bs) = f a b ++ interweave f as bs
interweave f (a : as) [] = f a [] ++ interweave f as []
interweave f [] (b : bs) = f [] b ++ interweave f [] bs
interweave f [] [] = []



-- fuse_inputs :: [VName] -> [(VName, LParam SOACS)] ->
--                           [(VName, LParam SOACS)] -> [(VName, LParam SOACS)]
-- fuse_inputs fusing inputs1 inputs2 = L.unionBy (\a b -> fst a == fst b) inputs1 filtered_inputs_2
--   where
--     filtered_inputs_2 = filter (\x -> fst x `notElem` fusing) inputs2

fuse_inputs2 :: [VName] -> [VName] -> [VName] -> [VName]
fuse_inputs2 fusing inputs1 inputs2 =
   inputs1 `L.union` filter (`notElem` fusing) inputs2

-- fuse_outputs :: [VName] ->
--   [(Ident, Type, SubExpRes)] ->
--   [(Ident, Type, SubExpRes)] ->
--   [(Ident, Type, SubExpRes)]
-- fuse_outputs infusible outputs1 outputs2 =
--    outputs2 `L.union` filtered_outputs
--   where
--     filtered_outputs = filter (\(x, _, _) -> identName x `elem` infusible) outputs1


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
  let r = f g in
  let r2 = f r in
    if equal r r2 then r
    else keeptrying f r2

-- substituteNames


-- getstms
