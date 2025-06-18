-- | A graph representation of a sequence of Futhark statements
-- (i.e. a 'Body'), built to handle fusion.  Could perhaps be made
-- more general.  An important property is that it does not handle
-- "nested bodies" (e.g. 'Match'); these are represented as single
-- nodes.
--
-- This is all implemented on top of the graph representation provided
-- by the @fgl@ package ("Data.Graph.Inductive").  The graph provided
-- by this package allows nodes and edges to have arbitrarily-typed
-- "labels".  It is these labels ('EdgeT', 'NodeT') that we use to
-- contain Futhark-specific information.  An edge goes *from*
-- consumers to producers.  There are also edges that do not represent
-- normal data dependencies, but other things.  This means that a node
-- can have multiple edges for the same name, indicating different
-- kinds of dependencies.
module Futhark.Optimise.Fusion.GraphRep
  ( -- * Data structure
    EdgeT (..),
    NodeT (..),
    DepContext,
    DepGraphAug,
    DepGraph (..),
    DepNode,

    -- * Queries
    getName,
    nodeFromLNode,
    mergedContext,
    mapAcross,
    edgesBetween,
    reachable,
    applyAugs,
    depsFromEdge,
    contractEdge,
    isRealNode,
    isCons,
    isDep,
    isInf,

    -- * Construction
    mkDepGraph,
    mkDepGraphForFun,
    pprg,
    isHLschedNodeT,
    isWithAccNodeT,
    isWithAccNodeId,
    vFusionFeasability,
    hFusionFeasability,
  )
where

import Control.Monad.Reader
import Data.Bifunctor (bimap)
import Data.Foldable (foldlM)
import Data.Graph.Inductive.Dot qualified as G
import Data.Graph.Inductive.Graph qualified as G
import Data.Graph.Inductive.Query.DFS qualified as Q
import Data.Graph.Inductive.Tree qualified as G
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Futhark.Analysis.Alias qualified as Alias
import Futhark.Analysis.HORep.SOAC qualified as H
import Futhark.IR.Prop.Aliases
import Futhark.IR.SOACS hiding (SOAC (..))
import Futhark.IR.SOACS qualified as Futhark
import Futhark.Util (nubOrd)

-- | Information associated with an edge in the graph.
data EdgeT
  = Alias VName
  | InfDep VName
  | Dep VName
  | Cons VName
  | Fake VName
  | Res VName
  deriving (Eq, Ord)

-- | Information associated with a node in the graph.
data NodeT
  = StmNode (Stm SOACS)
  | SoacNode H.ArrayTransforms (Pat Type) (H.SOAC SOACS) (StmAux (ExpDec SOACS))
  | -- | First 'VName' is result; last is input.
    TransNode VName H.ArrayTransform VName
  | -- | Node corresponding to a result of the entire computation
    -- (i.e. the 'Result' of a body).  Any node that is not
    -- transitively reachable from one of these can be considered
    -- dead.
    ResNode VName
  | -- | Node corresponding to a free variable.  These are used to
    -- safely handle consumption, which also means we don't have to
    -- create a node for every free single variable.
    FreeNode VName
  | MatchNode (Stm SOACS) [(NodeT, [EdgeT])]
  | DoNode (Stm SOACS) [(NodeT, [EdgeT])]
  deriving (Eq)

instance Show EdgeT where
  show (Dep vName) = "Dep " <> prettyString vName
  show (InfDep vName) = "iDep " <> prettyString vName
  show (Cons _) = "Cons"
  show (Fake _) = "Fake"
  show (Res _) = "Res"
  show (Alias _) = "Alias"

instance Show NodeT where
  show (StmNode (Let pat _ _)) = L.intercalate ", " $ map prettyString $ patNames pat
  show (SoacNode _ pat _ _) = prettyString pat
  show (TransNode _ tr _) = prettyString tr
  show (ResNode name) = prettyString $ "Res: " ++ prettyString name
  show (FreeNode name) = prettyString $ "Input: " ++ prettyString name
  show (MatchNode stm _) = "Match: " ++ L.intercalate ", " (map prettyString $ stmNames stm)
  show (DoNode stm _) = "Do: " ++ L.intercalate ", " (map prettyString $ stmNames stm)

-- | The name that this edge depends on.
getName :: EdgeT -> VName
getName edgeT = case edgeT of
  Alias vn -> vn
  InfDep vn -> vn
  Dep vn -> vn
  Cons vn -> vn
  Fake vn -> vn
  Res vn -> vn

-- | Does the node acutally represent something in the program?  A
-- "non-real" node represents things like fake nodes inserted to
-- express ordering due to consumption.
isRealNode :: NodeT -> Bool
isRealNode ResNode {} = False
isRealNode FreeNode {} = False
isRealNode _ = True

-- | Prettyprint dependency graph.
pprg :: DepGraph -> String
pprg = G.showDot . G.fglToDotString . G.nemap show show . dgGraph

-- | A pair of a 'G.Node' and the node label.
type DepNode = G.LNode NodeT

type DepEdge = G.LEdge EdgeT

-- | A tuple with four parts: inbound links to the node, the node
-- itself, the 'NodeT' "label", and outbound links from the node.
-- This type is used to modify the graph in 'mapAcross'.
type DepContext = G.Context NodeT EdgeT

-- | A dependency graph.  Edges go from *consumers* to *producers*
-- (i.e. from usage to definition).  That means the incoming edges of
-- a node are the dependents of that node, and the outgoing edges are
-- the dependencies of that node.
data DepGraph = DepGraph
  { dgGraph :: G.Gr NodeT EdgeT,
    dgProducerMapping :: ProducerMapping,
    -- | A table mapping VNames to VNames that are aliased to it.
    dgAliasTable :: AliasTable
  }

-- | A "graph augmentation" is a monadic action that modifies the graph.
type DepGraphAug m = DepGraph -> m DepGraph

-- | For each node, what producer should the node depend on and what
-- type is it.
type EdgeGenerator = NodeT -> [(VName, EdgeT)]

-- | A mapping from variable name to the graph node that produces
-- it.
type ProducerMapping = M.Map VName G.Node

makeMapping :: (Monad m) => DepGraphAug m
makeMapping dg@(DepGraph {dgGraph = g}) =
  pure dg {dgProducerMapping = M.fromList $ concatMap gen_dep_list (G.labNodes g)}
  where
    gen_dep_list :: DepNode -> [(VName, G.Node)]
    gen_dep_list (i, node) = [(name, i) | name <- getOutputs node]

-- | Apply several graph augmentations in sequence.
applyAugs :: (Monad m) => [DepGraphAug m] -> DepGraphAug m
applyAugs augs g = foldlM (flip ($)) g augs

-- | Creates deps for the given nodes on the graph using the 'EdgeGenerator'.
genEdges :: (Monad m) => [DepNode] -> EdgeGenerator -> DepGraphAug m
genEdges l_stms edge_fun dg =
  depGraphInsertEdges (concatMap (genEdge (dgProducerMapping dg)) l_stms) dg
  where
    -- statements -> mapping from declared array names to soac index
    genEdge :: M.Map VName G.Node -> DepNode -> [G.LEdge EdgeT]
    genEdge name_map (from, node) = do
      (dep, edgeT) <- edge_fun node
      Just to <- [M.lookup dep name_map]
      pure $ G.toLEdge (from, to) edgeT

depGraphInsertEdges :: (Monad m) => [DepEdge] -> DepGraphAug m
depGraphInsertEdges edgs dg = pure $ dg {dgGraph = G.insEdges edgs $ dgGraph dg}

-- | Monadically modify every node of the graph.
mapAcross :: (Monad m) => (DepContext -> m DepContext) -> DepGraphAug m
mapAcross f dg = do
  g' <- foldlM (flip helper) (dgGraph dg) (G.nodes (dgGraph dg))
  pure $ dg {dgGraph = g'}
  where
    helper n g' = case G.match n g' of
      (Just c, g_new) -> do
        c' <- f c
        pure $ c' G.& g_new
      (Nothing, _) -> pure g'

stmFromNode :: NodeT -> Stms SOACS -- do not use outside of edge generation
stmFromNode (StmNode x) = oneStm x
stmFromNode _ = mempty

-- | Get the underlying @fgl@ node.
nodeFromLNode :: DepNode -> G.Node
nodeFromLNode = fst

-- | Get the variable name that this edge refers to.
depsFromEdge :: DepEdge -> VName
depsFromEdge = getName . G.edgeLabel

-- | Find all the edges connecting the two nodes.
edgesBetween :: DepGraph -> G.Node -> G.Node -> [DepEdge]
edgesBetween dg n1 n2 = G.labEdges $ G.subgraph [n1, n2] $ dgGraph dg

-- | @reachable dg from to@ is true if @to@ is reachable from @from@.
reachable :: DepGraph -> G.Node -> G.Node -> Bool
reachable dg source target = target `elem` Q.reachable source (dgGraph dg)

-- Utility func for augs
augWithFun :: (Monad m) => EdgeGenerator -> DepGraphAug m
augWithFun f dg = genEdges (G.labNodes (dgGraph dg)) f dg

addDeps :: (Monad m) => DepGraphAug m
addDeps = augWithFun toDep
  where
    toDep stmt =
      let (fusible, infusible) =
            bimap (map fst) (map fst)
              . L.partition ((== SOACInput) . snd)
              . S.toList
              $ foldMap stmInputs (stmFromNode stmt)
          mkDep vname = (vname, Dep vname)
          mkInfDep vname = (vname, InfDep vname)
       in map mkDep fusible <> map mkInfDep infusible

addConsAndAliases :: (Monad m) => DepGraphAug m
addConsAndAliases = augWithFun edges
  where
    edges (StmNode s) = consEdges s' <> aliasEdges s'
      where
        s' = Alias.analyseStm mempty s
    edges _ = mempty
    consEdges s = zip names (map Cons names)
      where
        names = namesToList $ consumedInStm s
    aliasEdges =
      map (\vname -> (vname, Alias vname))
        . namesToList
        . mconcat
        . patAliases
        . stmPat

-- extra dependencies mask the fact that consuming nodes "depend" on all other
-- nodes coming before it (now also adds fake edges to aliases - hope this
-- fixes asymptotic complexity guarantees)
addExtraCons :: (Monad m) => DepGraphAug m
addExtraCons dg =
  depGraphInsertEdges (concatMap makeEdge (G.labEdges g)) dg
  where
    g = dgGraph dg
    alias_table = dgAliasTable dg
    mapping = dgProducerMapping dg
    makeEdge (from, to, Cons cname) = do
      let aliases = namesToList $ M.findWithDefault mempty cname alias_table
          to' = mapMaybe (`M.lookup` mapping) aliases
          p (tonode, toedge) =
            tonode /= from && getName toedge `elem` (cname : aliases)
      (to2, _) <- filter p $ concatMap (G.lpre g) to' <> G.lpre g to
      pure $ G.toLEdge (from, to2) (Fake cname)
    makeEdge _ = []

mapAcrossNodeTs :: (Monad m) => (NodeT -> m NodeT) -> DepGraphAug m
mapAcrossNodeTs f = mapAcross f'
  where
    f' (ins, n, nodeT, outs) = do
      nodeT' <- f nodeT
      pure (ins, n, nodeT', outs)

nodeToSoacNode :: (HasScope SOACS m, Monad m) => NodeT -> m NodeT
nodeToSoacNode n@(StmNode s@(Let pat aux op)) = case op of
  Op {} -> do
    maybeSoac <- H.fromExp op
    case maybeSoac of
      Right hsoac -> pure $ SoacNode mempty pat hsoac aux
      Left H.NotSOAC -> pure n
  Loop {} ->
    pure $ DoNode s []
  Match {} ->
    pure $ MatchNode s []
  e
    | [output] <- patNames pat,
      Just (ia, tr) <- H.transformFromExp aux e ->
        pure $ TransNode output tr ia
  _ -> pure n
nodeToSoacNode n = pure n

-- | Construct a graph with only nodes, but no edges.
emptyGraph :: Body SOACS -> DepGraph
emptyGraph body =
  DepGraph
    { dgGraph = G.mkGraph (labelNodes (stmnodes <> resnodes <> inputnodes)) [],
      dgProducerMapping = mempty,
      dgAliasTable = aliases
    }
  where
    labelNodes = zip [0 ..]
    stmnodes = map StmNode $ stmsToList $ bodyStms body
    resnodes = map ResNode $ namesToList $ freeIn $ bodyResult body
    inputnodes = map FreeNode $ namesToList consumed
    (_, (aliases, consumed)) = Alias.analyseStms mempty $ bodyStms body

getStmRes :: EdgeGenerator
getStmRes (ResNode name) = [(name, Res name)]
getStmRes _ = []

addResEdges :: (Monad m) => DepGraphAug m
addResEdges = augWithFun getStmRes

-- | Make a dependency graph corresponding to a 'Body'.
mkDepGraph :: (HasScope SOACS m, Monad m) => Body SOACS -> m DepGraph
mkDepGraph body = applyAugs augs $ emptyGraph body
  where
    augs =
      [ makeMapping,
        addDeps,
        addConsAndAliases,
        addExtraCons,
        addResEdges,
        mapAcrossNodeTs nodeToSoacNode -- Must be done after adding edges
      ]

-- | Make a dependency graph corresponding to a function.
mkDepGraphForFun :: FunDef SOACS -> DepGraph
mkDepGraphForFun f = runReader (mkDepGraph (funDefBody f)) scope
  where
    scope = scopeOfFParams (funDefParams f) <> scopeOf (bodyStms (funDefBody f))

-- | Merges two contexts.
mergedContext :: (Ord b) => a -> G.Context a b -> G.Context a b -> G.Context a b
mergedContext mergedlabel (inp1, n1, _, out1) (inp2, n2, _, out2) =
  let new_inp = filter (\n -> snd n /= n1 && snd n /= n2) (nubOrd (inp1 <> inp2))
      new_out = filter (\n -> snd n /= n1 && snd n /= n2) (nubOrd (out1 <> out2))
   in (new_inp, n1, mergedlabel, new_out)

-- | Remove the given node, and insert the 'DepContext' into the
-- graph, replacing any existing information about the node contained
-- in the 'DepContext'.
contractEdge :: (Monad m) => G.Node -> DepContext -> DepGraphAug m
contractEdge n2 ctx dg = do
  let n1 = G.node' ctx -- n1 remains
  pure $ dg {dgGraph = ctx G.& G.delNodes [n1, n2] (dgGraph dg)}

-- Utils for fusibility/infusibility
-- find dependencies - either fusible or infusible. edges are generated based on these

-- | A classification of a free variable.
data Classification
  = -- | Used as array input to a SOAC (meaning fusible).
    SOACInput
  | -- | Used in some other way.
    Other
  deriving (Eq, Ord, Show)

type Classifications = S.Set (VName, Classification)

freeClassifications :: (FreeIn a) => a -> Classifications
freeClassifications =
  S.fromList . (`zip` repeat Other) . namesToList . freeIn

stmInputs :: Stm SOACS -> Classifications
stmInputs (Let pat aux e) =
  freeClassifications (pat, aux) <> expInputs e

bodyInputs :: Body SOACS -> Classifications
bodyInputs (Body _ stms res) = foldMap stmInputs stms <> freeClassifications res

expInputs :: Exp SOACS -> Classifications
expInputs (Match cond cases defbody attr) =
  foldMap (bodyInputs . caseBody) cases
    <> bodyInputs defbody
    <> freeClassifications (cond, attr)
expInputs (Loop params form b1) =
  freeClassifications (params, form) <> bodyInputs b1
expInputs (Op soac) = case soac of
  Futhark.Screma w is form -> inputs is <> freeClassifications (w, form)
  Futhark.Hist w is ops lam -> inputs is <> freeClassifications (w, ops, lam)
  Futhark.Scatter w is lam iws -> inputs is <> freeClassifications (w, lam, iws)
  Futhark.Stream w is nes lam ->
    inputs is <> freeClassifications (w, nes, lam)
  Futhark.JVP {} -> freeClassifications soac
  Futhark.VJP {} -> freeClassifications soac
  where
    inputs = S.fromList . (`zip` repeat SOACInput)
expInputs e
  | Just (arr, _) <- H.transformFromExp mempty e =
      S.singleton (arr, SOACInput)
        <> freeClassifications (freeIn e `namesSubtract` oneName arr)
  | otherwise = freeClassifications e

stmNames :: Stm SOACS -> [VName]
stmNames = patNames . stmPat

getOutputs :: NodeT -> [VName]
getOutputs node = case node of
  (StmNode stm) -> stmNames stm
  (TransNode v _ _) -> [v]
  (ResNode _) -> []
  (FreeNode name) -> [name]
  (MatchNode stm _) -> stmNames stm
  (DoNode stm _) -> stmNames stm
  (SoacNode _ pat _ _) -> patNames pat

-- | Is there a possibility of fusion?
isDep :: EdgeT -> Bool
isDep (Dep _) = True
isDep (Res _) = True
isDep _ = False

-- | Is this an infusible edge?
isInf :: (G.Node, G.Node, EdgeT) -> Bool
isInf (_, _, e) = case e of
  InfDep _ -> True
  Fake _ -> True -- this is infusible to avoid simultaneous cons/dep edges
  _ -> False

-- | Is this a 'Cons' edge?
isCons :: EdgeT -> Bool
isCons (Cons _) = True
isCons _ = False

-- | Is this a high-level schedule?
isHLschedNodeT :: NodeT -> Bool
isHLschedNodeT (StmNode (Let _ _ (Apply fnm _ _ _))) =
  any (`L.isPrefixOf` (nameToString fnm)) hl_sched_prefixes
  where
    hl_sched_prefixes = ["hlSched2D", "fuseSched2D"]
isHLschedNodeT _ = False

-- | Is this a withAcc?
isWithAccNodeT :: NodeT -> Bool
isWithAccNodeT (StmNode (Let _ _ (WithAcc _ _))) = True
isWithAccNodeT _ = False

isWithAccNodeId :: G.Node -> DepGraph -> Bool
isWithAccNodeId node_id (DepGraph {dgGraph = g}) =
  let (_, _, nT, _) = G.context g node_id
   in isWithAccNodeT nT

unreachableEitherDir :: DepGraph -> G.Node -> G.Node -> Bool
unreachableEitherDir g a b =
  not (reachable g a b || reachable g b a)

vFusionFeasability :: DepGraph -> G.Node -> G.Node -> Bool
vFusionFeasability dg@DepGraph {dgGraph = g} n1 n2 =
  (isWithAccNodeId n2 dg || not (any isInf (edgesBetween dg n1 n2)))
    && not (any (reachable dg n2) (filter (/= n2) (G.pre g n1)))

hFusionFeasability :: DepGraph -> G.Node -> G.Node -> Bool
hFusionFeasability = unreachableEitherDir
