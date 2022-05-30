{-# LANGUAGE FlexibleContexts #-}

-- | A graph representation of a sequence of Futhark statements
-- (i.e. a 'Body'), built to handle fusion.  Could perhaps be made
-- more general.  An important property is that it does not handle
-- "nested bodies" (e.g. 'If'); these are represented as single nodes.
--
-- This is all implemented on top of the graph representation provided
-- by the @fgl@ package ("Data.Graph.Inductive").  The graph provided
-- by this package allows nodes and edges to have arbitrarily-typed
-- "labels".  It is these labels ('EdgeT', 'NodeT') that we use to
-- contain Futhark-specific information.
module Futhark.Optimise.Fusion.GraphRep
  ( -- * Data structure
    EdgeT (..),
    NodeT (..),
    DepContext,
    DepGraphAug,
    DepGraph (..),
    DepNode,

    -- * Construction
    mkDepGraph,
    pprg,

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
  )
where

import Data.Bifunctor (bimap)
import Data.Foldable (foldlM)
import qualified Data.Graph.Inductive.Dot as G
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.DFS as Q
import qualified Data.Graph.Inductive.Tree as G
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Futhark.Analysis.Alias as Alias
import qualified Futhark.Analysis.HORep.SOAC as H
import Futhark.IR.Prop.Aliases
import Futhark.IR.SOACS hiding (SOAC (..))
import qualified Futhark.IR.SOACS as Futhark
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
  | RNode VName
  | InNode VName
  | FinalNode (Stms SOACS) NodeT (Stms SOACS)
  | IfNode (Stm SOACS) [(NodeT, [EdgeT])]
  | DoNode (Stm SOACS) [(NodeT, [EdgeT])]
  deriving (Eq)

instance Show EdgeT where
  show (Dep vName) = "Dep " <> pretty vName
  show (InfDep vName) = "iDep " <> pretty vName
  show (Cons _) = "Cons"
  show (Fake _) = "Fake"
  show (Res _) = "Res"
  show (Alias _) = "Alias"

instance Show NodeT where
  show (StmNode (Let pat _ _)) = L.intercalate ", " $ map pretty $ patNames pat
  show (SoacNode _ pat _ _) = pretty pat
  show (FinalNode _ nt _) = show nt
  show (RNode name) = pretty $ "Res: " ++ pretty name
  show (InNode name) = pretty $ "Input: " ++ pretty name
  show (IfNode stm _) = "If: " ++ L.intercalate ", " (map pretty $ stmNames stm)
  show (DoNode stm _) = "Do: " ++ L.intercalate ", " (map pretty $ stmNames stm)

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
isRealNode RNode {} = False
isRealNode InNode {} = False
isRealNode _ = True

-- | Prettyprint dependency graph.
pprg :: DepGraph -> String
pprg = G.showDot . G.fglToDotString . G.nemap show show . dgGraph

-- | A pair of a 'G.Node' and the node label.
type DepNode = G.LNode NodeT

type DepEdge = G.LEdge EdgeT

-- | A tuple with four parts: inbound links to the node, the node
-- itself, the 'NodeT' "label", and outbound links from the node.
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

-- 'DepGenerator's can be used to make 'EdgeGenerators'.
type DepGenerator = Stm SOACS -> Names

-- | For each node, what producer should the node depend on and what
-- type is it.
type EdgeGenerator = NodeT -> [(VName, EdgeT)]

-- | A mapping from variable name to the graph node that produces
-- it.
type ProducerMapping = M.Map VName G.Node

-- | Construct a graph with only nodes, but no edges.
emptyGraph :: Stms SOACS -> Names -> Names -> DepGraph
emptyGraph stms res inputs =
  DepGraph
    { dgGraph = G.mkGraph (labelNodes (stmnodes <> resnodes <> inputnodes)) [],
      dgProducerMapping = mempty,
      dgAliasTable = mempty
    }
  where
    labelNodes = zip [0 ..]
    stmnodes = map StmNode $ stmsToList stms
    resnodes = map RNode $ namesToList res
    inputnodes = map InNode $ namesToList inputs

makeMapping :: Monad m => DepGraphAug m
makeMapping dg@(DepGraph {dgGraph = g}) =
  pure dg {dgProducerMapping = M.fromList $ concatMap gen_dep_list (G.labNodes g)}
  where
    gen_dep_list :: DepNode -> [(VName, G.Node)]
    gen_dep_list (i, node) = [(name, i) | name <- getOutputs node]

-- make a table to handle transitive aliases
makeAliasTable :: Monad m => Stms SOACS -> DepGraphAug m
makeAliasTable stms dg = do
  let (_, (aliasTable', _)) = Alias.analyseStms mempty stms
  pure $ dg {dgAliasTable = aliasTable'}

-- | Make a dependency graph corresponding to a 'Body' and with the
-- provided "inputs" (i.e. free variables).
mkDepGraph :: (HasScope SOACS m, Monad m) => Body SOACS -> Names -> m DepGraph
mkDepGraph (Body () stms res) inputs = do
  let g = emptyGraph stms (freeIn res) inputs
  applyAugs
    [ makeMapping,
      makeAliasTable stms,
      initialGraphConstruction
    ]
    g

-- | Apply several graph augmentations in sequence.
applyAugs :: Monad m => [DepGraphAug m] -> DepGraphAug m
applyAugs augs g = foldlM (flip ($)) g augs

-- | Add edges for straightforward dependencies to the graph.
addDeps :: Monad m => DepGraphAug m
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

initialGraphConstruction :: (HasScope SOACS m, Monad m) => DepGraphAug m
initialGraphConstruction =
  applyAugs
    [ addDeps,
      addCons,
      addExtraCons,
      addResEdges,
      addAliases,
      convertGraph -- Must be done after adding edges
    ]

-- | Creates deps for the given nodes on the graph using the 'EdgeGenerator'.
genEdges :: Monad m => [DepNode] -> EdgeGenerator -> DepGraphAug m
genEdges l_stms edge_fun dg =
  depGraphInsertEdges (concatMap (genEdge (dgProducerMapping dg)) l_stms) dg
  where
    -- statements -> mapping from declared array names to soac index
    genEdge :: M.Map VName G.Node -> DepNode -> [G.LEdge EdgeT]
    genEdge name_map (from, node) = do
      (dep, edgeT) <- edge_fun node
      Just to <- [M.lookup dep name_map]
      pure $ G.toLEdge (from, to) edgeT

depGraphInsertEdges :: Monad m => [DepEdge] -> DepGraphAug m
depGraphInsertEdges edgs dg = pure $ dg {dgGraph = G.insEdges edgs $ dgGraph dg}

mapAcross :: Monad m => (DepContext -> m DepContext) -> DepGraphAug m
mapAcross f dg = do
  g' <- foldlM (flip helper) (dgGraph dg) (G.nodes (dgGraph dg))
  pure $ dg {dgGraph = g'}
  where
    helper n g' = case G.match n g' of
      (Just c, g_new) -> do
        c' <- f c
        pure $ c' G.& g_new
      (Nothing, _) -> pure g'

mapAcrossNodeTs :: Monad m => (NodeT -> m NodeT) -> DepGraphAug m
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
  DoLoop {} ->
    pure $ DoNode s []
  If {} ->
    pure $ IfNode s []
  _ -> pure n
nodeToSoacNode n = pure n

convertGraph :: (HasScope SOACS m, Monad m) => DepGraphAug m
convertGraph = mapAcrossNodeTs nodeToSoacNode

stmFromNode :: NodeT -> Stms SOACS -- do not use outside of edge generation
stmFromNode (StmNode x) = oneStm x
stmFromNode _ = mempty

nodeFromLNode :: DepNode -> G.Node
nodeFromLNode = fst

depsFromEdge :: DepEdge -> VName
depsFromEdge = getName . G.edgeLabel

edgesBetween :: DepGraph -> G.Node -> G.Node -> [DepEdge]
edgesBetween dg n1 n2 = G.labEdges $ G.subgraph [n1, n2] $ dgGraph dg

-- | @reachable dg from to@ is true if @to@ is reachable from @from@.
reachable :: DepGraph -> G.Node -> G.Node -> Bool
reachable dg source target = target `elem` Q.reachable source (dgGraph dg)

-- Utility func for augs
augWithFun :: Monad m => EdgeGenerator -> DepGraphAug m
augWithFun f dg = genEdges (G.labNodes (dgGraph dg)) f dg

toAlias :: DepGenerator -> EdgeGenerator
toAlias f =
  map (\vname -> (vname, Alias vname)) . namesToList . foldMap f . stmFromNode

addAliases :: Monad m => DepGraphAug m
addAliases = augWithFun $ toAlias aliasInputs

addCons :: Monad m => DepGraphAug m
addCons = augWithFun getStmCons

-- | Merges two contexts.
mergedContext :: Ord b => a -> G.Context a b -> G.Context a b -> G.Context a b
mergedContext mergedlabel (inp1, n1, _, out1) (inp2, n2, _, out2) =
  let new_inp = filter (\n -> snd n /= n1 && snd n /= n2) (nubOrd (inp1 <> inp2))
      new_out = filter (\n -> snd n /= n1 && snd n /= n2) (nubOrd (out1 <> out2))
   in (new_inp, n1, mergedlabel, new_out)

contractEdge :: Monad m => G.Node -> DepContext -> DepGraphAug m
contractEdge n2 ctx dg = do
  let n1 = G.node' ctx -- n1 remains
  pure $ dg {dgGraph = ctx G.& G.delNodes [n1, n2] (dgGraph dg)}

-- extra dependencies mask the fact that consuming nodes "depend" on all other
-- nodes coming before it (now also adds fake edges to aliases - hope this
-- fixes asymptotic complexity guarantees)
addExtraCons :: Monad m => DepGraphAug m
addExtraCons dg = do
  let aliasTab = dgAliasTable dg
      mapping = dgProducerMapping dg
      edges = concatMap (make_edge aliasTab mapping) (G.labEdges g)
  depGraphInsertEdges edges dg
  where
    g = dgGraph dg
    make_edge :: AliasTable -> M.Map VName G.Node -> DepEdge -> [DepEdge]
    make_edge aliasTab mapping (from, to, Cons cname) =
      let aliasses = namesToList $ M.findWithDefault (namesFromList []) cname aliasTab
          to' = map (mapping M.!) aliasses
       in [ G.toLEdge (from, to2) (Fake cname)
            | (to2, _) <-
                filter
                  ( \(tonode, toedge) ->
                      tonode /= from
                        && getName toedge `elem` aliasses <> [cname]
                  )
                  $ concatMap (G.lpre g) to' <> G.lpre g to
          ]
    make_edge _ _ _ = []

addResEdges :: Monad m => DepGraphAug m
addResEdges = augWithFun getStmRes

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

freeClassifications :: FreeIn a => a -> Classifications
freeClassifications =
  S.fromList . (`zip` repeat Other) . namesToList . freeIn

stmInputs :: Stm SOACS -> Classifications
stmInputs (Let pat aux e) =
  freeClassifications (pat, aux) <> expInputs e

bodyInputs :: Body SOACS -> Classifications
bodyInputs (Body _ stms res) = foldMap stmInputs stms <> freeClassifications res

expInputs :: Exp SOACS -> Classifications
expInputs (If cond b1 b2 attr) =
  bodyInputs b1 <> bodyInputs b2 <> freeClassifications (cond, attr)
expInputs (DoLoop params form b1) =
  freeClassifications (params, form) <> bodyInputs b1
expInputs (Op soac) = case soac of
  Futhark.Screma w is form -> inputs is <> freeClassifications (w, form)
  Futhark.Hist w is ops lam -> inputs is <> freeClassifications (w, ops, lam)
  Futhark.Scatter w is lam iws -> inputs is <> freeClassifications (w, lam, iws)
  Futhark.Stream w is form nes lam ->
    inputs is <> freeClassifications (w, form, nes, lam)
  Futhark.JVP {} -> freeClassifications soac
  Futhark.VJP {} -> freeClassifications soac
  where
    inputs = S.fromList . (`zip` repeat SOACInput)
expInputs e
  | Just (arr, _) <- H.transformFromExp mempty e =
      S.singleton (arr, SOACInput)
        <> freeClassifications (freeIn e `namesSubtract` oneName arr)
  | otherwise = freeClassifications e

aliasInputs :: Stm SOACS -> Names
aliasInputs = mconcat . expAliases . Alias.analyseExp mempty . stmExp

stmNames :: Stm SOACS -> [VName]
stmNames = patNames . stmPat

getStmCons :: EdgeGenerator
getStmCons (StmNode s) = zip names (map Cons names)
  where
    names = namesToList . consumedInStm . Alias.analyseStm mempty $ s
getStmCons _ = []

getStmRes :: EdgeGenerator
getStmRes (RNode name) = [(name, Res name)]
getStmRes _ = []

getOutputs :: NodeT -> [VName]
getOutputs node = case node of
  (StmNode stm) -> stmNames stm
  (RNode _) -> []
  (InNode name) -> [name]
  (IfNode stm _) -> stmNames stm
  (DoNode stm _) -> stmNames stm
  FinalNode {} -> error "Final nodes cannot generate edges"
  (SoacNode _ pat _ _) -> patNames pat

isDep :: EdgeT -> Bool -- Is there a possibility of fusion?
isDep (Dep _) = True
isDep (Res _) = True
isDep _ = False

isInf :: (G.Node, G.Node, EdgeT) -> Bool -- No possibility of fusion
isInf (_, _, e) = case e of
  InfDep _ -> True
  Cons _ -> False
  Fake _ -> True -- this is infusible to avoid simultaneous cons/dep edges
  _ -> False

-- | Is this a 'Cons' edge?
isCons :: EdgeT -> Bool
isCons (Cons _) = True
isCons _ = False
