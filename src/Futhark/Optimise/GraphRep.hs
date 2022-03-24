-- TODO: Move to IR/Graph.hs at some point, for now keeping in Optimise/
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.Optimise.GraphRep where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Futhark.IR.SOACS hiding (SOAC (..))
import qualified Futhark.IR.SOACS as Futhark
import qualified Futhark.Analysis.Alias as Alias
import Futhark.IR.Prop.Aliases
import qualified Futhark.Analysis.HORep.SOAC as HOREPSOAC

import qualified Data.Graph.Inductive.Query.DFS as Q
import qualified Data.Graph.Inductive.Tree as G
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Dot
import Futhark.IR.Pretty as PP
import qualified Futhark.Util.Pretty as PP

import Debug.Trace
import Futhark.Builder (MonadFreshNames (putNameSource), VNameSource, getNameSource, modifyNameSource)
import Futhark.Pass
import Data.Foldable (foldlM)
import Control.Monad.State
import Futhark.CodeGen.Backends.CCUDA.Boilerplate (failureSwitch)

-- TODO: Move to IR/Graph.hs at some point, for now keeping in Optimise/

-- SNode: Stm [InputTransforms] [OutputTransforms]
data EdgeT = InfDep VName | Dep VName | Cons VName | Fake | Res VName deriving (Eq, Ord)
data NodeT = SNode (Stm SOACS) HOREPSOAC.ArrayTransforms | RNode VName | InNode VName
  deriving (Eq, Ord)


instance Show EdgeT where
  show (Dep vName) = "Dep " <> ppr vName
  show (InfDep vName) = "iDep " <> ppr vName
  show (Cons _) = "Cons"
  show Fake = "Fake"
  show (Res _) = "Res"

-- inputs could have their own edges - to facilitate fusion


-- nodeT_to_str
instance Show NodeT where
    show (SNode stm@(Let pat aux _) _) = ppr $ L.intercalate ", " $ map ppr $ patNames pat -- show (namesToList $ freeIn stm)
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

type DepGenerator = Stm SOACS -> [VName]
type EdgeGenerator = NodeT -> [(VName, EdgeT)]


data FusionEnv = FusionEnv
  {
    -- nodeMap :: M.Map VName [VName],
    vNameSource :: VNameSource,
    scope :: Scope SOACS,
    reachabilityG :: G.Gr () ()
  }

newtype FusionEnvM a = FusionEnvM {fenv :: State FusionEnv a}
  deriving
    (
      Monad,
      Applicative,
      Functor,
      MonadState FusionEnv
    )

instance MonadFreshNames FusionEnvM where
  getNameSource = gets vNameSource
  putNameSource source = do
    fenv <- get
    put (fenv {vNameSource = source})

instance HasScope SOACS FusionEnvM where
  askScope = gets scope


runFusionEnvM ::
  MonadFreshNames m =>
  FusionEnvM a ->
  FusionEnv ->
  m a
runFusionEnvM (FusionEnvM a) env =
  -- let (a', new_env) =  runState a env
  -- in modifyNameSource (\src ->  (a', vNameSource (new_env {vNameSource = src})))
  modifyNameSource $ \src -> let (new_a, new_env) = runState a (env {vNameSource = src}) in (new_a, vNameSource new_env)



type DepGraphAug = DepGraph -> FusionEnvM DepGraph

--- Graph Construction ---

-- emptyG :: [Stm SOACS] -> Result -> [FParam SOACS] -> DepGraph
-- emptyG stms r inputs = mkGraph (label_nodes (snodes ++ rnodes ++ inNodes)) []
--   where
--     namesFromRes = map ((\(Var x) -> x) . resSubExp)
--     label_nodes = zip [0..]
--     snodes = map SNode stms
--     rnodes = map RNode (namesFromRes r)
--     inNodes= map (InNode . paramName) $ filter isArray inputs -- there might be a better way

appendTransformations :: DepGraphAug
appendTransformations g = do
  applyAugs (map appendTransform $ labNodes g) g


appendTransform :: DepNode -> DepGraphAug
appendTransform node_to_fuse g =
  if gelem (nodeFromLNode node_to_fuse) g
  then applyAugs (map (appendT node_to_fuse_id) fuses_to) g
  else pure g
  where
    fuses_to = map nodeFromLNode $ input g node_to_fuse
    node_to_fuse_id = nodeFromLNode node_to_fuse


-- replaceName :: VName -> VName ->



appendT :: Node -> Node -> DepGraphAug
appendT transformNode to g
  | not (gelem transformNode g && gelem to g) = pure g
  | outdeg g to == 1 =
    case mapT (lFromNode g) (transformNode, to) of
      (SNode (Let _ aux_1 exp_1) _, SNode s@(Let _ aux_2 (Op (Futhark.Screma sub_exp ouputs scremaform))) outTrans) ->
        case (HOREPSOAC.transformFromExp (stmAuxCerts aux_1) exp_1, isMapSOAC scremaform) of
          (Just (vn,transform), Just lam) -> do
            let newNodeL = SNode s (outTrans HOREPSOAC.|> transform)
            let newContext = mergedContext newNodeL (context g to) (context g transformNode)
            contractEdge transformNode newContext g
          _ -> pure g
      _ -> pure g
  | otherwise = pure g





      -- (SNode (Let _ aux exp) _ _) ->
      --   case transformFromExp (stmAuxCerts aux) exp of
      --     Just (vn, transform) ->
      --       if lnodeFromNode $ to g
      --       contractEdge transformNode (context to) g
      --     Nothing -> pure g




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
mkDepGraph stms res inputs = addDepEdges $ emptyG2 stms res inputs


addDepEdges :: DepGraphAug
addDepEdges = applyAugs
  [addDeps2, makeScanInfusible, addInfDeps, addCons, addExtraCons, addResEdges] --, appendTransformations]



genEdges :: [DepNode] -> EdgeGenerator -> DepGraphAug
genEdges l_stms edge_fun g = depGraphInsertEdges (concatMap gen_edge l_stms) g
  where
    name_map = gen_names_map (labNodes g)
    -- statements -> mapping from declared array names to soac index
    gen_names_map :: [DepNode] -> M.Map VName Node
    gen_names_map s = M.fromList $ concatMap gen_dep_list s
      where
        gen_dep_list :: DepNode -> [(VName, Node)]
        gen_dep_list (i, node) = [(name, i) | name <- getOutputs node]-- getStmNames (stmFromNode node)]
    gen_edge :: DepNode -> [LEdge EdgeT]
    gen_edge (from, node) = [toLEdge (from,to) edgeT  | (dep, edgeT) <- edge_fun node,
                                              Just to <- [M.lookup dep name_map]]

depGraphInsertEdges :: [DepEdge] -> DepGraphAug
depGraphInsertEdges edgs g = return $ mkGraph (labNodes g) (edgs ++ labEdges g)

applyAugs :: [DepGraphAug] -> DepGraphAug
applyAugs augs g = foldlM (flip ($)) g augs
--applyAugs g augs = foldl (flip ($)) g (augs ++ [cleanUpGraph])

--- /Graph Construction

--- Extracting Nodes/Edges ---

label :: DepNode -> NodeT
label = snd

stmFromNode :: NodeT -> [Stm SOACS]
stmFromNode (SNode x _) = [x]
stmFromNode _ = []



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

lFromNode g n = label $ lNodeFromNode g n


labFromEdge :: DepGraph -> DepEdge -> DepNode
labFromEdge g (n1, n2, lab) = lNodeFromNode g n1

depsFromEdge ::  DepEdge -> [VName]
depsFromEdge e = case edgeLabel e of
  (Dep name) -> [name]
  (InfDep name) -> [name]
  (Res name) -> [name]
  (Cons name) -> [name]
  _ -> []

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


toDep :: DepGenerator -> EdgeGenerator
toDep f stmt = map (\vname ->  (vname, Dep vname)) (concatMap f (stmFromNode stmt))

addDeps2 :: DepGraphAug
addDeps2 = augWithFun $ toDep fusableInputs

toInfDep :: DepGenerator -> EdgeGenerator
toInfDep f stmt = map (\vname ->  (vname, InfDep vname)) (concatMap f (stmFromNode stmt))

addInfDeps :: DepGraphAug
addInfDeps = augWithFun $ toInfDep infusableInputs

--unused?
addDeps :: DepGraphAug
addDeps = augWithFun getStmDeps

addCons :: DepGraphAug
addCons = augWithFun getStmCons

cleanUpGraph :: DepGraphAug
cleanUpGraph g = undefined

-- Merges two contexts
mergedContext :: (Eq b) => a -> Context a b -> Context a b -> Context a b
mergedContext mergedlabel (inp1, n1, _, out1) (inp2, n2, _, out2) =
  let new_inp  = L.nub $ filter (\n -> snd n /= n1 && snd n /= n2) (inp1  `L.union` inp2) in
  let new_out = L.nub $ filter (\n -> snd n /= n1 && snd n /= n2) (out1 `L.union` out2)
  in (new_inp, n1, mergedlabel, new_out)

-- n1 remains
contractEdge :: Node -> DepContext -> DepGraphAug
contractEdge n2 cxt g = do
  let n1 = node' cxt

  -- Modify reachabilityG
  rg <- gets reachabilityG
  let newContext = mergedContext () (context rg n1) (context rg n2)
  modify (\s -> s {reachabilityG = (&) newContext $ delNodes [n1, n2] rg})

  pure $ (&) cxt $ delNodes [n1, n2] g

-- extra dependencies mask the fact that consuming nodes "depend" on all other
-- nodes coming before it
addExtraCons :: DepGraphAug
addExtraCons g = depGraphInsertEdges new_edges g
  where
    new_edges = concatMap make_edge (labEdges g)
    make_edge :: DepEdge -> [DepEdge]
    make_edge (from, to, Cons _) = [toLEdge (from, to2) Fake | to2 <- filter (/= from) $ pre g to]
    make_edge _ = []

addResEdges :: DepGraphAug
addResEdges = augWithFun getStmRes

makeScanInfusible :: DepGraphAug
makeScanInfusible g = return $ emap change_node_to_idep g
  where
    find_scan_results :: Stm SOACS -> [VName]
    find_scan_results  (Let pat _ (Op (Futhark.Screma  _ _ (ScremaForm scns _ _)))) =
      take (length scns) (patNames pat)
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
fusableInputs :: Stm SOACS -> [VName]
fusableInputs (Let _ _ exp) = fusableInputsFromExp exp

fusableInputsFromExp :: Exp SOACS -> [VName]
fusableInputsFromExp (Op soac) = case soac of
  Futhark.Screma  _ is _     -> is
  Futhark.Hist    _ is _ _   -> is
  Futhark.Scatter _ is _ _   -> is
  Futhark.Stream  _ is _ _ _ -> is
fusableInputsFromExp _ = []

infusableInputs :: Stm SOACS -> [VName]
infusableInputs (Let _ aux exp) = infusableInputsFromExp exp ++ namesToList (freeIn aux)

infusableInputsFromExp :: Exp SOACS -> [VName]
infusableInputsFromExp (Op soac) = case soac of
  Futhark.Screma  e _ s  ->
    namesToList $ freeIn $ Futhark.Screma e [] s
  Futhark.Hist    _ _ _ lam       -> namesToList $ freeIn lam
  Futhark.Scatter _ _ lam _       -> namesToList $ freeIn lam
  Futhark.Stream  _ _ _ _ lam     -> namesToList $ freeIn lam
infusableInputsFromExp op@(BasicOp x) = namesToList $ freeIn op
infusableInputsFromExp op@If {} = namesToList $ freeIn op
infusableInputsFromExp op@DoLoop {} = namesToList $ freeIn op
infusableInputsFromExp _ = []

--- /Augmentations ---

--- Inspecting Stms ---

getStmNames :: Stm SOACS -> [VName]
getStmNames s = case s of
  Let pat _ _ -> patNames pat

getStmDeps :: EdgeGenerator
getStmDeps (SNode s _) = map (\x -> (x, Dep x)) names
  where
    names = (traceShowId . namesToList . freeIn) s
getStmDeps _ = []

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
namesFromRes = concatMap ((\x -> case x of
     Var z -> [z]
     Constant _ -> []
  ) . resSubExp)



getOutputs :: NodeT -> [VName]
getOutputs node = case node of
  (SNode stm _) -> getStmNames stm
  (RNode _)   -> []
  (InNode name) -> [name]

--- /Inspecting Stms ---




mapT :: (a -> b) -> (a,a) -> (b,b)
mapT f (a,b) = (f a, f b)
