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
import Futhark.Util (maxinum)

import Futhark.Optimise.GraphRep
import qualified Data.Graph.Inductive.Query.DFS as Q
import Data.Graph.Inductive.Graph
-- import qualified Control.Monad.Identity as Control.Monad

import Debug.Trace
import Futhark.IR.Aliases (VName(VName))
import Futhark.Optimise.Fusion.LoopKernel (FusedKer(fusedVars))
import Data.Tuple (swap)
import Data.List (deleteFirstsBy)


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
      graph_fused = keeptrying doMapFusion graph_not_fused'
      graph_fused' = trace (pprg graph_fused) graph_fused
      stms_new = reverse $ concatMap stmFromNode $ Q.topsort' graph_fused'
      stms_new' = trace (unlines (map ppr stms_new)) stms_new

-- map-fusion part

-- horizontally_fusible :: DepGraph -> (Bool, [[DepNode]])
-- horizontally_fusible g = let hfg =(horizontally_fusible_groups g) in (not $ null hfg, hfg)

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



-- try_fuse_all :: [DepNode] -> DepGraphAug
-- try_fuse_all depsnodes g = applyAugs g (map (uncurry tryFuseNodesInGraph) pairs)
--   where
--     nodes = map nodeFromLNode depsnodes
--     pairs = [(x, y) | x <- nodes, y <- nodes,  x < y]

-- -- fuse_all === applyAugs g try_fuse_all listlist

doMapFusion :: DepGraphAug
-- go through each node and attempt a map fusion
doMapFusion g = applyAugs g (map tryFuseNodeInGraph $ labNodes g)


-- contextFromLNode :: DepGraph -> DepNode -> DepContext
-- contextFromLNode g lnode = context g $ nodeFromLNode lnode

isRes :: (Node, EdgeT) -> Bool
isRes (_,Res) = True
isRes _ = False


v_fusion_feasability :: DepGraph -> Node -> Node -> Bool
v_fusion_feasability g n1 n2 = all is_deps edges && (all (==n2) nodes || all (==n1) nodes2)
  where
    (nodes2, _) = unzip $ filter (not . isRes) $ lsuc g n2
    (nodes, edges) = unzip $ filter (not . isRes) $ lpre g n1
    is_deps (Dep _) = True
    is_deps (InfDep _) = True
    -- is_deps Res = True -- unintuitive, but i think it works
    is_deps _ = False




d_fusion_feasability :: DepGraph -> Node -> Node -> Bool
d_fusion_feasability _ _ _ = True


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

  --- | otherwise = case fuseContexts infusable_nodes (context g node_1) (context g node_2) of
  --     Just new_Context -> (&) new_Context $ delNodes [node_1, node_2] g
  --     Nothing -> g
  --   where
  --     -- sorry about this
  --     infusable_nodes = concatMap deps_from_edge
  --       (concatMap (edgesBetween g node_1) (pre g node_1)
  --                                 L.\\ edgesBetween g node_1 node_2)





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
          Just $ Let (basicPat ids) aux2 (Op (Futhark.Screma s_exp names
            (ScremaForm (scans_1 ++ scans_2) (red_1 ++ red_2) lam)))
      where
        lam_1_inputs = boundByLambda lam_1
        lam_2_inputs = boundByLambda lam_2
       -- inputs_2 = i2
        stm_1_output = (patNames . stmPat) s1
        lam_1_output = namesFromRes $ bodyResult $ lambdaBody lam_1

        map1 = M.fromList $ zip lam_2_inputs i2
        map2 = M.fromList $ zip stm_1_output lam_1_output
        map4 = M.fromList $ zip i1 lam_1_inputs-- I1 II1

        map3 = M.mapMaybe (\x -> M.lookup x (M.union map2 map4)) map1

        lam_2' = substituteNames map3 lam_2

        (names, lam_inputs) = unzip $ fuse_inputs stm_1_output
                                                (zip i1 (lambdaParams lam_1))
                                                (zip i2 (lambdaParams lam_2))


        res_from_lambda =  bodyResult . lambdaBody

        (ids, types, body_res) = unzip3 $ fuse_outputs infusible outputs1 outputs2
        outputs1 = zip3 (patIdents pats1) (lambdaReturnType lam_1) (res_from_lambda lam_1)
        outputs2 = zip3 (patIdents pats2) (lambdaReturnType lam_2) (res_from_lambda lam_2)


        lam' = fuseLambda lam_1 lam_2'
        lam = lam' {
          lambdaParams = lam_inputs,
          lambdaReturnType = types,
          lambdaBody = (lambdaBody lam') {bodyResult = substituteNames map3 body_res}
          }
    -- vertical map-scatter fusion
    ( Let pats1 aux1 (Op (Futhark.Screma  _ i1 (ScremaForm scans_1 red_1 lam_1))),
      Let pats2 aux2 (Op (Futhark.Scatter exp i2 lam_2 other)))
      | L.null infusible -- only if map outputs are used exclusivly by the scatter
      -> Just $ Let pats2 aux2 (Op (Futhark.Scatter exp names lam other))
        where
          -- fuse inputs - outputs will not change, ordering of indecies may break,
        lam_1_inputs = boundByLambda lam_1
        lam_2_inputs = boundByLambda lam_2
       -- inputs_2 = i2
        stm_1_output = (patNames . stmPat) s1
        lam_1_output = namesFromRes $ bodyResult $ lambdaBody lam_1

        map1 = M.fromList $ zip lam_2_inputs i2
        map2 = M.fromList $ zip stm_1_output lam_1_output
        map4 = M.fromList $ zip i1 lam_1_inputs-- I1 II1

        map3 = M.mapMaybe (\x -> M.lookup x (M.union map2 map4)) map1

        lam_2' = substituteNames map3 lam_2

        (names, lam_inputs) = unzip $ fuse_inputs stm_1_output
                                                (zip i1 (lambdaParams lam_1))
                                                (zip i2 (lambdaParams lam_2))


        res_from_lambda =  bodyResult . lambdaBody

        (ids, types, body_res) = unzip3 $ fuse_outputs infusible outputs1 outputs2
        outputs1 = zip3 (patIdents pats1) (lambdaReturnType lam_1) (res_from_lambda lam_1)
        outputs2 = zip3 (patIdents pats2) (lambdaReturnType lam_2) (res_from_lambda lam_2)


        lam' = fuseLambda lam_1 lam_2'
        lam = lam' {
          lambdaParams = lam_inputs,
          lambdaReturnType = lambdaReturnType lam_2,
          lambdaBody = (lambdaBody lam') {bodyResult = substituteNames map3 $ res_from_lambda lam_2}
          }

    -- problem - literally copy pasted code here.
    -- also, in cases where the ordering matters on the inputs and outputs,
    --  not sure how to fix


    _ -> Nothing





-- fuseStms_horrizontal :: [VName] ->  Stm SOACS -> Stm SOACS -> Maybe (Stm SOACS)
-- fuseStms_horrizontal infusible s1 s2 =
--   case (s1, s2) of
--     (Let pats1 aux1 (Op (Futhark.Screma  _     i1  (ScremaForm [] reds_1 lam_1))),
--      Let pats2 aux2 (Op (Futhark.Screma  s_exp i2  (ScremaForm [] reds_2 lam_2)))) ->
--           Just $ Let (basicPat ids) aux2 (Op (Futhark.Screma s_exp names
--             (mapSOAC lam)))
--       where
--         lam_1_inputs = boundByLambda lam_1
--         lam_2_inputs = boundByLambda lam_2
--        -- inputs_2 = i2
--         stm_1_output = (patNames . stmPat) s1
--         lam_1_output = namesFromRes $ bodyResult $ lambdaBody lam_1

--         map1 = M.fromList $ zip lam_2_inputs i2
--         map2 = M.fromList $ zip stm_1_output lam_1_output
--         map4 = M.fromList $ zip i1 lam_1_inputs-- I1 II1

--         map3 = M.mapMaybe (\x -> M.lookup x (M.union map2 map4)) map1

--         lam_2' = substituteNames map3 lam_2

--         (names, lam_inputs) = unzip $ fuse_inputs stm_1_output
--                                                 (zip i1 (lambdaParams lam_1))
--                                                 (zip i2 (lambdaParams lam_2))


--         res_from_lambda =  bodyResult . lambdaBody


--         lam' = fuseLambda lam_1 lam_2'
--         lam = lam' {
--           lambdaParams = lam_inputs,
--           lambdaReturnType = lambdaReturnType lam_2,
--           lambdaBody = (lambdaBody lam') {bodyResult = substituteNames map3 $ res_from_lambda lam_2}
--           }

--     _ -> Nothing








fuse_inputs :: [VName] -> [(VName, LParam SOACS)] ->
                          [(VName, LParam SOACS)] -> [(VName, LParam SOACS)]
fuse_inputs fusing inputs1 inputs2 = L.unionBy (\a b -> fst a == fst b) inputs1 filtered_inputs_2
  where
    filtered_inputs_2 = filter (\x -> fst x `notElem` fusing) inputs2

fuse_outputs :: [VName] ->
  [(Ident, Type, SubExpRes)] ->
  [(Ident, Type, SubExpRes)] ->
  [(Ident, Type, SubExpRes)]
fuse_outputs infusible outputs1 outputs2 =
   trace ("infusible: " ++ show infusible) outputs2 `L.union` filtered_outputs
  where
    filtered_outputs = filter (\(x, _, _) -> identName x `elem` infusible) outputs1




fuseLambda :: Lambda SOACS -> Lambda SOACS -> Lambda SOACS
fuseLambda (Lambda inputs l_body_1 _) (Lambda _ l_body_2 output_types) =
  Lambda inputs l_body_new output_types
  where
    l_body_new = insertStms (bodyStms l_body_1) l_body_2


keeptrying :: DepGraphAug -> DepGraphAug
keeptrying f g =
  let r = f g in
  let r2 = f r in
    if equal r r2 then r
    else keeptrying f r2

-- substituteNames


-- getstms
