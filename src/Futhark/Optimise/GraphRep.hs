-- TODO: Move to IR/Graph.hs at some point, for now keeping in Optimise/
module Futhark.Optimise.GraphRep where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Futhark.IR.SOACS hiding (SOAC (..))
import qualified Futhark.IR.SOACS as Futhark
import qualified Futhark.Analysis.Alias as Alias
import Futhark.IR.Prop.Aliases

import qualified Data.Graph.Inductive.Query.DFS as Q
import qualified Data.Graph.Inductive.Tree as G
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Dot
import Futhark.IR.Pretty as PP
import qualified Futhark.Util.Pretty as PP

import Debug.Trace



data EdgeT = InfDep VName | Dep VName | Cons | Fake | Res deriving (Eq, Ord)
data NodeT = SNode (Stm SOACS) | RNode VName | InNode VName
  deriving (Eq, Ord)


instance Show EdgeT where
  show (Dep vName) = "Dep " <> ppr vName
  show (InfDep vName) = "iDep " <> ppr vName
  show Cons = "Cons"
  show Fake = "Fake"
  show Res  = "Res"




-- nodeT_to_str
instance Show NodeT where
    show (SNode stm@(Let pat _ _)) = ppr $ L.intercalate ", " $ map ppr $ patNames pat -- show (namesToList $ freeIn stm)
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
type DepGraphAug = DepGraph -> DepGraph

type DepGenerator = Stm SOACS -> [VName]
type EdgeGenerator = NodeT -> [(VName, EdgeT)]

--- Graph Construction ---

emptyG :: [Stm SOACS] -> Result -> [FParam SOACS] -> DepGraph
emptyG stms r inputs = mkGraph (label_nodes (snodes ++ rnodes ++ inNodes)) []
  where
    namesFromRes = map ((\(Var x) -> x) . resSubExp)
    label_nodes = zip [0..]
    snodes = map SNode stms
    rnodes = map RNode (namesFromRes r)
    inNodes= map (InNode .paramName) inputs -- there might be a better way


mkDepGraph :: [Stm SOACS] -> Result -> [FParam SOACS] -> DepGraph
mkDepGraph stms res inputs =
    applyAugs (emptyG stms res inputs) [addDeps2, makeScanInfusible, addInfDeps, addCons, addExtraCons, addResEdges]

genEdges :: [DepNode] -> EdgeGenerator -> [LEdge EdgeT]
genEdges l_stms edge_fun = concatMap gen_edge l_stms
  where
    name_map = gen_names_map l_stms
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
depGraphInsertEdges edgs g = mkGraph (labNodes g) (edgs ++ labEdges g)

applyAugs :: DepGraph -> [DepGraphAug] -> DepGraph
applyAugs = foldl (flip ($))
--applyAugs g augs = foldl (flip ($)) g (augs ++ [cleanUpGraph])

--- /Graph Construction

--- Extracting Nodes/Edges ---

label :: DepNode -> NodeT
label = snd

stmFromNode :: NodeT -> [Stm SOACS]
stmFromNode (SNode x) = [x]
stmFromNode _ = []

nodeFromLNode :: DepNode -> Node
nodeFromLNode = fst

lNodeFromNode :: DepGraph -> Node -> DepNode
lNodeFromNode g n = labNode' (context g n)

labFromEdge :: DepGraph -> DepEdge -> DepNode
labFromEdge g (n1, n2, lab) = lNodeFromNode g n1

depsFromEdge :: DepGraph -> DepEdge -> [VName]
depsFromEdge g e = case edgeLabel e of
  (Dep name) -> [name]
  (InfDep name) -> [name]
  Res -> case labFromEdge g e of
    (_, RNode n) -> [n]
    _ -> []
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
augWithFun f g = depGraphInsertEdges new_edges g
  where
    new_edges = genEdges (labNodes g) f

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

-- extra dependencies mask the fact that consuming nodes "depend" on all other
-- nodes coming before it
addExtraCons :: DepGraphAug
addExtraCons g = depGraphInsertEdges new_edges g
  where
    new_edges = concatMap make_edge (labEdges g)
    make_edge :: DepEdge -> [DepEdge]
    make_edge (from, to, Cons) = [toLEdge (from, to2) Fake | to2 <- filter (/= from) $ pre g to]
    make_edge _ = []

addResEdges :: DepGraphAug
addResEdges = augWithFun getStmRes

makeScanInfusible :: DepGraphAug
makeScanInfusible g = emap change_node_to_idep g
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
infusableInputs (Let _ _ exp) = infusableInputsFromExp exp

infusableInputsFromExp :: Exp SOACS -> [VName]
infusableInputsFromExp (Op soac) = case soac of
  Futhark.Screma  e _ s  ->
    namesToList $ freeIn $ Futhark.Screma e [] s
  Futhark.Hist    _ _ _ lam       -> namesToList $ freeIn lam
  Futhark.Scatter _ _ lam _       -> namesToList $ freeIn lam
  Futhark.Stream  _ _ _ _ lam     -> namesToList $ freeIn lam
infusableInputsFromExp op@(BasicOp x) = namesToList $ freeIn op
infusableInputsFromExp _ = []

--- /Augmentations ---

--- Inspecting Stms ---

getStmNames :: Stm SOACS -> [VName]
getStmNames s = case s of
  Let pat _ _ -> patNames pat

getStmDeps :: EdgeGenerator
getStmDeps (SNode s) = map (\x -> (x, Dep x)) names
  where
    names = (traceShowId . namesToList . freeIn) s
getStmDeps _ = []

getStmCons :: EdgeGenerator
getStmCons (SNode s) = zip names (repeat Cons)
  where
    names =  namesToList . consumedInStm . Alias.analyseStm mempty $ s
getStmCons _ = []

getStmRes :: EdgeGenerator
getStmRes (RNode name) = [(name, Res)]
getStmRes _ = []

-- TODO: Figure out where to put this
namesFromRes :: [SubExpRes] -> [VName]
namesFromRes = mapMaybe ((\x -> case x of
    Var z -> Just z
    Constant _ -> Nothing
  ) . resSubExp)

getOutputs :: NodeT -> [VName]
getOutputs node = case node of
  (SNode stm) -> getStmNames stm
  (RNode _)   -> []
  (InNode name) -> [name]

--- /Inspecting Stms ---
