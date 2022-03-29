-- TODO: Move to IR/Graph.hs at some point, for now keeping in Optimise/

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module Futhark.Optimise.GraphRep (module Futhark.Optimise.GraphRep)
  -- (FusionEnvM,
  -- FusionEnv,
  -- runFusionEnvM,
  -- freshFusionEnv,
  -- mkDepGraph,
  -- isArray,
  -- pprg)
  where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
-- import Data.Maybe
import Futhark.IR.SOACS hiding (SOAC (..))
import qualified Futhark.IR.SOACS as Futhark
import qualified Futhark.Analysis.Alias as Alias
import Futhark.IR.Prop.Aliases
import qualified Futhark.Analysis.HORep.SOAC as HOREPSOAC

--import qualified Data.Graph.Inductive.Query.DFS as Q
import qualified Data.Graph.Inductive.Tree as G
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Dot
--import Futhark.IR.Pretty as PP
import qualified Futhark.Util.Pretty as PP

--import Debug.Trace
import Futhark.Builder (MonadFreshNames (putNameSource), VNameSource, getNameSource, modifyNameSource, blankNameSource)
--import Futhark.Pass
import Data.Foldable (foldlM)
import Control.Monad.State




-- TODO: Move to IR/Graph.hs at some point, for now keeping in Optimise/

-- SNode: Stm [InputTransforms] [OutputTransforms]
data EdgeT = Alias VName | InfDep VName | Dep VName | Cons VName | Fake VName | Res VName deriving (Eq, Ord)
data NodeT = SNode (Stm SOACS) HOREPSOAC.ArrayTransforms | RNode VName | InNode VName | FinalNode [Stm SOACS]
  deriving (Eq, Ord)


instance Show EdgeT where
  show (Dep vName) = "Dep " <> ppr vName
  show (InfDep vName) = "iDep " <> ppr vName
  show (Cons _) = "Cons"
  show (Fake _) = "Fake"
  show (Res _) = "Res"
  show (Alias _) = "Alias"

-- inputs could have their own edges - to facilitate fusion


-- nodeT_to_str
instance Show NodeT where
    show (SNode (Let pat _ _) _) = ppr $ L.intercalate ", " $ map ppr $ patNames pat -- show (namesToList $ freeIn stm)
    show (FinalNode stms) = concatMap ppr stms
    show (RNode name)  = ppr $ "Res: "   ++ ppr name
    show (InNode name) = ppr $ "Input: " ++ ppr name

-- PrettyPrinter
ppr :: PP.Pretty m => m -> String
ppr k = PP.prettyDoc 80 (PP.ppr k)

pprg :: DepGraph -> String
pprg = showDot . fglToDotString . nemap show show

type DepNode = LNode NodeT
type DepEdge = LEdge EdgeT
type DepContext = Context NodeT EdgeT
type DepGraph = G.Gr NodeT EdgeT

-- depGenerators can be used to make edgeGenerators
type DepGenerator = Stm SOACS -> [VName]
-- for each node, what producer should the node depend on and what type
type EdgeGenerator = NodeT -> [(VName, EdgeT)]

-- monadic state environment for fusion.
data FusionEnv = FusionEnv
  {
    -- nodeMap :: M.Map VName [VName],
    vNameSource :: VNameSource,
    scope :: Scope SOACS,
    --reachabilityG :: G.Gr () (),
    producerMapping :: M.Map VName Node
  }

freshFusionEnv :: Scope SOACS -> FusionEnv
freshFusionEnv stms_scope = FusionEnv {vNameSource = blankNameSource, scope = stms_scope, producerMapping = M.empty}

newtype FusionEnvM a = FusionEnvM (State FusionEnv a)
  deriving
    (
      Monad,
      Applicative,
      Functor,
      MonadState FusionEnv
    )

instance MonadFreshNames FusionEnvM where
  getNameSource = gets vNameSource
  putNameSource source =
    modify (\env -> env {vNameSource = source})

instance HasScope SOACS FusionEnvM where
  askScope = gets scope


runFusionEnvM ::
  MonadFreshNames m =>
  FusionEnvM a ->
  FusionEnv ->
  m a
runFusionEnvM (FusionEnvM a) env =
  modifyNameSource $ \src -> let (new_a, new_env) = runState a (env {vNameSource = src}) in (new_a, vNameSource new_env)


-- most everything is going to be a graph augmentation g -> M g.
-- these can be efficiently strung together using applyAugs
type DepGraphAug = DepGraph -> FusionEnvM DepGraph


-- transform functions for fusing over transforms
-- appendTransformations :: DepGraphAug
-- appendTransformations g = applyAugs (map appendTransform $ labNodes g) g


-- appendTransform :: DepNode -> DepGraphAug
-- appendTransform node_to_fuse g =
--   if gelem (nodeFromLNode node_to_fuse) g
--   then applyAugs (map (appendT node_to_fuse_id) fuses_to) g
--   else pure g
--   where
--     fuses_to = map nodeFromLNode $ input g node_to_fuse
--     node_to_fuse_id = nodeFromLNode node_to_fuse


-- --- Graph Construction ---

-- appendT :: Node -> Node -> DepGraphAug
-- appendT transformNode to g
--   | not (gelem transformNode g && gelem to g) = pure g
--   | outdeg g to == 1 =
--     case mapT (lFromNode g) (transformNode, to) of
--       (SNode (Let _ aux_1 exp_1) _, SNode s@(Let _ aux_2 (Op (Futhark.Screma sub_exp ouputs scremaform))) outTrans) ->
--         case (HOREPSOAC.transformFromExp (stmAuxCerts aux_1) exp_1, isMapSOAC scremaform) of
--           (Just (vn,transform), Just lam) -> do
--             let newNodeL = SNode s (outTrans HOREPSOAC.|> transform)
--             let newContext = mergedContext newNodeL (context g to) (context g transformNode)
--             contractEdge transformNode newContext g
--           _ -> pure g
--       _ -> pure g
--   | otherwise = pure g





      -- (SNode (Let _ aux exp) _ _) ->
      --   case transformFromExp (stmAuxCerts aux) exp of
      --     Just (vn, transform) ->
      --       if lnodeFromNode $ to g
      --       contractEdge transformNode (context to) g
      --     Nothing -> pure g


    -- gen_names_map :: [DepNode] -> M.Map VName Node
    -- gen_names_map s = M.fromList $ concatMap gen_dep_list s

emptyG2 :: [Stm SOACS] -> [VName] -> [VName] -> DepGraph
emptyG2 stms res inputs = mkGraph (label_nodes (snodes ++ rnodes ++ inNodes)) []
  where
    label_nodes = zip [0..]
    snodes = map (`SNode` mempty) stms
    rnodes = map RNode res
    inNodes= map InNode inputs

isArray :: FParam SOACS -> Bool
isArray p = case paramDec p of
  Array {} -> True
  _ -> False

mkDepGraph :: [Stm SOACS] -> [VName] -> [VName] -> FusionEnvM DepGraph
mkDepGraph stms res inputs = do
  let g = emptyG2 stms res inputs
  _ <- makeMapping g
  addDepEdges g

addDepEdges :: DepGraphAug
addDepEdges = applyAugs
  [addDeps2, makeScanInfusible, addInfDeps, addCons, addExtraCons, addResEdges, addAliases] --, appendTransformations]


makeMapping :: DepGraphAug
makeMapping g = do
  let mapping = M.fromList $ concatMap gen_dep_list (labNodes g)
  modify (\s -> s{producerMapping = mapping})
  pure g
    where
      gen_dep_list :: DepNode -> [(VName, Node)]
      gen_dep_list (i, node) = [(name, i) | name <- getOutputs node]

-- creates deps for the given nodes on the graph using the edgeGenerator
genEdges :: [DepNode] -> EdgeGenerator -> DepGraphAug
genEdges l_stms edge_fun g = do
  name_map <- gets producerMapping
  depGraphInsertEdges (concatMap (gen_edge name_map) l_stms) g
  where
    -- statements -> mapping from declared array names to soac index
    gen_edge ::  M.Map VName Node -> DepNode -> [LEdge EdgeT]
    gen_edge name_map (from, node) = [toLEdge (from,to) edgeT  | (dep, edgeT) <- edge_fun node,
                                              Just to <- [M.lookup dep name_map]]

depGraphInsertEdges :: [DepEdge] -> DepGraphAug
depGraphInsertEdges edgs g = return $ mkGraph (labNodes g) (edgs ++ labEdges g)

applyAugs :: [DepGraphAug] -> DepGraphAug
applyAugs augs g = foldlM (flip ($)) g augs

--- /Graph Construction

--- Extracting Nodes/Edges ---

label :: DepNode -> NodeT
label = snd

stmFromNode :: NodeT -> [Stm SOACS]
stmFromNode (SNode x _) = [x]
stmFromNode (FinalNode x) = x
stmFromNode _ = []


-- possibly should be combined with the copy aliased
-- -- started this - but seems unreasonably hard.
-- finalizeStmFromNode :: NodeT -> FusionEnvM [Stm SOACS]
-- finalizeStmFromNode (SNode stm transforms)
--   | HOREPSOAC.nullTransforms transforms = pure [stm]
--   | otherwise = case stm of
--     Let pat sa (Op (Futhark.Screma size inputs  (ScremaForm [] [] lam))) ->
--       let names = patNames pat



--       pure []
--     _ -> error "transformations applied to non-map"
-- finalizeStmFromNode _ = pure []


nodeFromLNode :: DepNode -> Node
nodeFromLNode = fst

lNodeFromNode :: DepGraph -> Node -> DepNode
lNodeFromNode g n = labNode' (context g n)

lFromNode :: DepGraph -> Node -> NodeT
lFromNode g n = label $ lNodeFromNode g n


labFromEdge :: DepGraph -> DepEdge -> DepNode
labFromEdge g (n1, _, _) = lNodeFromNode g n1

depsFromEdgeT :: EdgeT -> [VName]
depsFromEdgeT e = case e of
  Dep name    -> [name]
  InfDep name -> [name]
  Res name    -> [name]
  Cons name   -> [name]
  Fake name   -> [name]
  Alias name  -> [name]

depsFromEdge ::  DepEdge -> [VName]
depsFromEdge = depsFromEdgeT . edgeLabel


input :: DepGraph -> DepNode -> [DepNode]
input g node = map (labNode' . context g) $ suc g $ nodeFromLNode node

output :: DepGraph -> DepNode -> [DepNode]
output g node = map (labNode' . context g) $ pre g $ nodeFromLNode node

edgesBetween :: DepGraph -> Node -> Node -> [DepEdge]
edgesBetween g n1 n2 = labEdges $ subgraph [n1,n2] g

--- Extracting Nodes/Edges ---

--- Augmentations ---

-- Utility func for augs
augWithFun :: EdgeGenerator -> DepGraphAug
augWithFun f g = genEdges (labNodes g) f g


toAlias :: DepGenerator -> EdgeGenerator
toAlias f stmt = map (\vname ->  (vname, Alias vname)) (concatMap f (stmFromNode stmt))

toDep :: DepGenerator -> EdgeGenerator
toDep f stmt = map (\vname ->  (vname, Dep vname)) (concatMap f (stmFromNode stmt))

addDeps2 :: DepGraphAug
addDeps2 = augWithFun $ toDep fusableInputs

toInfDep :: DepGenerator -> EdgeGenerator
toInfDep f stmt = map (\vname ->  (vname, InfDep vname)) (concatMap f (stmFromNode stmt))

addInfDeps :: DepGraphAug
addInfDeps = augWithFun $ toInfDep infusableInputs


addAliases :: DepGraphAug
addAliases = augWithFun $ toAlias aliasInputs
-- --unused?
-- addDeps :: DepGraphAug
-- addDeps = augWithFun getStmDeps

addCons :: DepGraphAug
addCons = augWithFun getStmCons


-- Merges two contexts
mergedContext :: (Eq b) => a -> Context a b -> Context a b -> Context a b
mergedContext mergedlabel (inp1, n1, _, out1) (inp2, n2, _, out2) =
  let new_inp = L.nub $ filter (\n -> snd n /= n1 && snd n /= n2) (inp1  `L.union` inp2) in
  let new_out = L.nub $ filter (\n -> snd n /= n1 && snd n /= n2) (out1 `L.union` out2)
  in (new_inp, n1, mergedlabel, new_out)
  -- update keys of gen n2 with n1


-- n1 remains
contractEdge :: Node -> DepContext -> DepGraphAug
contractEdge n2 cxt g = do
  let n1 = node' cxt

  -- -- Modify reachabilityG
  -- rg <- gets reachabilityG
  -- let newContext = mergedContext () (context rg n1) (context rg n2)
  -- modify (\s -> s {reachabilityG = (&) newContext $ delNodes [n1, n2] rg})

  pure $ (&) cxt $ delNodes [n1, n2] g
-- BUG: should modify name_mappings



-- extra dependencies mask the fact that consuming nodes "depend" on all other
-- nodes coming before it
addExtraCons :: DepGraphAug
addExtraCons g = depGraphInsertEdges new_edges g
  where
    new_edges = concatMap make_edge (labEdges g)
    make_edge :: DepEdge -> [DepEdge]
    make_edge (from, to, Cons cname) = [toLEdge (from, to2) (Fake cname) | (to2, _) <- filter (\(tonode,toedge)->
      tonode /= from
      && cname `elem` depsFromEdgeT toedge
      ) $ lpre g to]
    make_edge _ = []

addResEdges :: DepGraphAug
addResEdges = augWithFun getStmRes

-- and reduce, actually
makeScanInfusible :: DepGraphAug
makeScanInfusible g = return $ emap change_node_to_idep g
  where
    find_scan_results :: Stm SOACS -> [VName]
    find_scan_results  (Let pat _ (Op (Futhark.Screma  _ _ (ScremaForm scns rdcs _)))) =
      let resLen = scanResults scns + redResults rdcs
      in take resLen (patNames pat)
    -- find_scan_results  (Let pat _ (Op Futhark.Scatter {})) = patNames pat
    -- find_scan_results  (Let pat _ (Op Futhark.Hist {})) = patNames pat
    find_scan_results _ = []

    scan_res_set :: S.Set VName
    scan_res_set = S.fromList (concatMap find_scan_results (concatMap (stmFromNode . label) (labNodes g)))

    is_scan_res :: VName -> Bool
    is_scan_res name = S.member name scan_res_set

    change_node_to_idep :: EdgeT -> EdgeT
    change_node_to_idep (Dep name) = if is_scan_res name
      then InfDep name
      else Dep name
    change_node_to_idep e = e

-- Utils for fusibility/infusibility
-- find dependencies - either fusable or infusable. edges are generated based on these


fusableInputs :: Stm SOACS -> [VName]
fusableInputs (Let _ _ expr) = fusableInputsFromExp expr

fusableInputsFromExp :: Exp SOACS -> [VName]
fusableInputsFromExp (Op soac) = case soac of
  Futhark.Screma  _ is _     -> is
  Futhark.Hist    _ is _ _   -> is
  Futhark.Scatter _ is _ _   -> is
  Futhark.Stream  _ is _ _ _ -> is
fusableInputsFromExp _ = []

infusableInputs :: Stm SOACS -> [VName]
infusableInputs (Let _ aux expr) = infusableInputsFromExp expr ++ namesToList (freeIn aux)

infusableInputsFromExp :: Exp SOACS -> [VName]
infusableInputsFromExp (Op soac) = case soac of
  Futhark.Screma  e _ s  ->
    namesToList $ freeIn $ Futhark.Screma e [] s
  Futhark.Hist    e _ histops lam ->
    namesToList $ freeIn $ Futhark.Hist e [] histops lam
  Futhark.Scatter e _ lam other       ->
    namesToList $ freeIn $ Futhark.Scatter e [] lam other
  Futhark.Stream  a1 _ a3 a4 lam     ->
    namesToList $ freeIn $ Futhark.Stream a1 [] a3 a4 lam
-- infusableInputsFromExp op@(BasicOp x) = namesToList $ freeIn op
-- infusableInputsFromExp op@If {} = namesToList $ freeIn op
-- infusableInputsFromExp op@DoLoop {} = namesToList $ freeIn op
infusableInputsFromExp op = namesToList $ freeIn op

aliasInputs :: Stm SOACS -> [VName]
aliasInputs op = case op of
  Let _ _ expr -> concatMap namesToList $ expAliases $ Alias.analyseExp mempty expr

--- /Augmentations ---

--- Inspecting Stms ---

getStmNames :: Stm SOACS -> [VName]
getStmNames s = case s of
  Let pat _ _ -> patNames pat


getStmCons :: EdgeGenerator
getStmCons (SNode s _) = zip names (map Cons names)
  where
    names =  namesToList . consumedInStm . Alias.analyseStm mempty $ s
getStmCons _ = []

getStmRes :: EdgeGenerator
getStmRes (RNode name) = [(name, Res name)]
getStmRes _ = []

-- TODO: Figure out where to put this
namesFromRes :: [SubExpRes] -> [VName]
namesFromRes = concatMap ((\case
     Var z -> [z]
     Constant _ -> []
  ) . resSubExp)
-- THIS IS BUGGY!!!! Constants are yeeted from lambda outputs after fusion


getOutputs :: NodeT -> [VName]
getOutputs node = case node of
  (SNode stm _) -> getStmNames stm
  (RNode _)   -> []
  (InNode name) -> [name]
  (FinalNode stms) -> concatMap getStmNames stms

--- /Inspecting Stms ---




mapT :: (a -> b) -> (a,a) -> (b,b)
mapT f (a,b) = (f a, f b)
