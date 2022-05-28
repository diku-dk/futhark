{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.Optimise.Fusion.GraphRep
  ( EdgeT (..),
    NodeT (..),
    FusionEnvM,
    runFusionEnvM,
    FusionEnv (..),
    freshFusionEnv,
    DepContext,
    DepGraphAug,
    DepGraph,
    mkDepGraph,
    DepNode,
    pprg,
    getName,
    isRealNode,
    nodeFromLNode,
    mergedContext,
    mapAcross,
    genEdges,
    edgesBetween,
    isDep,
    isInf,
    applyAugs,
    makeMapping,
    makeAliasTable,
    initialGraphConstruction,
    emptyGraph,
    depsFromEdge,
    contractEdge,
    isCons,
    makeMap,
    fuseMaps,
    mapAcrossWithSE,
    updateNode,
  )
where

import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State
import Data.Bifunctor (bimap)
import Data.Foldable (foldlM)
import qualified Data.Graph.Inductive.Dot as G
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Tree as G
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Futhark.Analysis.Alias as Alias
import qualified Futhark.Analysis.HORep.SOAC as H
import Futhark.Builder
import Futhark.IR.Prop.Aliases
import Futhark.IR.SOACS hiding (SOAC (..))
import qualified Futhark.IR.SOACS as Futhark
import Futhark.Transform.Substitute
import Futhark.Util (nubOrd)

data EdgeT
  = Alias VName
  | InfDep VName
  | Dep VName
  | Cons VName
  | Fake VName
  | Res VName
  deriving (Eq, Ord)

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

instance Substitute EdgeT where
  substituteNames m edgeT =
    setName (substituteNames m (getName edgeT)) edgeT

instance Substitute NodeT where
  substituteNames m nodeT = case nodeT of
    StmNode stm -> StmNode (f stm)
    SoacNode ots pat soac aux ->
      let soac' = case soac of
            H.Stream se sf la ses ins -> H.Stream (f se) (fStreamForm sf) (f la) (f ses) (f ins)
            H.Scatter se la ins x0 -> H.Scatter (f se) (f la) (f ins) (map fShape x0)
            H.Screma se sf ins -> H.Screma (f se) (fScremaForm sf) (f ins)
            H.Hist se hos la ins -> H.Hist (f se) (map fHistOp hos) (f la) (f ins)
       in SoacNode (f ots) (f pat) soac' (f aux)
    RNode vn -> RNode (f vn)
    InNode vn -> InNode (f vn)
    FinalNode stms1 nt stms2 -> FinalNode (fmap f stms1) (f nt) (fmap f stms2)
    IfNode stm nodes -> IfNode (f stm) (map f nodes)
    DoNode stm nodes -> DoNode (f stm) (map f nodes)
    where
      f :: Substitute a => a -> a
      f = substituteNames m
      fStreamForm :: StreamForm SOACS -> StreamForm SOACS
      fStreamForm (Parallel o comm lam0) =
        Parallel o comm (f lam0)
      fStreamForm s = s
      fShape (shp, i, vn) = (f shp, i, f vn)
      fHistOp (HistOp shape rf op_arrs nes op) =
        HistOp (f shape) (f rf) (f op_arrs) (f nes) (f op)
      fScremaForm (ScremaForm scans reds lam) = ScremaForm (map fScan scans) (map fRed reds) (f lam)
      fScan (Scan red_lam red_nes) = Scan (f red_lam) (map f red_nes)
      fRed (Reduce comm red_lam red_nes) = Reduce comm (f red_lam) (map f red_nes)

getName :: EdgeT -> VName
getName edgeT = case edgeT of
  Alias vn -> vn
  InfDep vn -> vn
  Dep vn -> vn
  Cons vn -> vn
  Fake vn -> vn
  Res vn -> vn

setName :: VName -> EdgeT -> EdgeT
setName vn edgeT = case edgeT of
  Alias _ -> Alias vn
  InfDep _ -> InfDep vn
  Dep _ -> Dep vn
  Cons _ -> Cons vn
  Fake _ -> Fake vn
  Res _ -> Res vn

makeMap :: Ord a => [a] -> [b] -> M.Map a b
makeMap x y = M.fromList $ zip x y

fuseMaps :: Ord b => M.Map a b -> M.Map b c -> M.Map a c
fuseMaps m1 m2 = M.mapMaybe (`M.lookup` m2) m1

-- does the node acutally represent something in the program
-- (non-real nodes are not delayed-fused into other nodes)
isRealNode :: NodeT -> Bool
isRealNode RNode {} = False
isRealNode InNode {} = False
isRealNode _ = True

pprg :: DepGraph -> String
pprg = G.showDot . G.fglToDotString . G.nemap show show

type DepNode = G.LNode NodeT

type DepEdge = G.LEdge EdgeT

type DepContext = G.Context NodeT EdgeT

-- | A dependency graph.  Edges go from *consumers* to *producers*
-- (i.e. from usage to definition).  That means the incoming edges of
-- a node are the dependents of that node, and the outgoing edges are
-- the dependencies of that node.
type DepGraph = G.Gr NodeT EdgeT

type DepGraphAug = DepGraph -> FusionEnvM DepGraph

-- | 'DepGenerator's can be used to make 'EdgeGenerators'.
type DepGenerator = Stm SOACS -> Names

-- | For each node, what producer should the node depend on and what
-- type is it.
type EdgeGenerator = NodeT -> [(VName, EdgeT)]

data FusionEnv = FusionEnv -- monadic state environment for fusion.
  { vNameSource :: VNameSource,
    -- | A mapping from variable name to the graph node that produces
    -- it.
    producerMapping :: M.Map VName G.Node,
    -- a table mapping VNames to VNames that are aliased to it
    aliasTable :: AliasTable,
    -- | Fused anything yet?
    fusedAnything :: Bool,
    fuseScans :: Bool
  }

freshFusionEnv :: FusionEnv
freshFusionEnv =
  FusionEnv
    { vNameSource = blankNameSource,
      producerMapping = M.empty,
      aliasTable = M.empty,
      fusedAnything = False,
      fuseScans = True
    }

newtype FusionEnvM a = FusionEnvM (ReaderT (Scope SOACS) (State FusionEnv) a)
  deriving
    ( Monad,
      Applicative,
      Functor,
      MonadState FusionEnv,
      HasScope SOACS,
      LocalScope SOACS
    )

instance MonadFreshNames FusionEnvM where
  getNameSource = gets vNameSource
  putNameSource source =
    modify (\env -> env {vNameSource = source})

runFusionEnvM :: MonadFreshNames m => Scope SOACS -> FusionEnv -> FusionEnvM a -> m a
runFusionEnvM scope fenv (FusionEnvM a) = modifyNameSource $ \src ->
  let x = runReaderT a scope
      (y, z) = runState x (fenv {vNameSource = src})
   in (y, vNameSource z)

-- | Construct a graph with only nodes, but no edges.
emptyGraph :: Stms SOACS -> Names -> Names -> DepGraph
emptyGraph stms res inputs = G.mkGraph (labelNodes (stmnodes <> resnodes <> inputnodes)) []
  where
    labelNodes = zip [0 ..]
    stmnodes = map StmNode $ stmsToList stms
    resnodes = map RNode $ namesToList res
    inputnodes = map InNode $ namesToList inputs

-- | Construct a mapping from output names to the node that produces
-- it and add it to the 'producerMapping' of the monad.
makeMapping :: DepGraphAug
makeMapping g = do
  let mapping = M.fromList $ concatMap gen_dep_list (G.labNodes g)
  modify (\s -> s {producerMapping = mapping})
  pure g
  where
    gen_dep_list :: DepNode -> [(VName, G.Node)]
    gen_dep_list (i, node) = [(name, i) | name <- getOutputs node]

-- make a table to handle transitive aliases
makeAliasTable :: Stms SOACS -> DepGraphAug
makeAliasTable stms g = do
  let (_, (aliasTable', _)) = Alias.analyseStms mempty stms
  modify (\s -> s {aliasTable = aliasTable'})
  pure g

mkDepGraph :: Stms SOACS -> Names -> Names -> FusionEnvM DepGraph
mkDepGraph stms res inputs = do
  let g = emptyGraph stms res inputs
  applyAugs
    [ makeMapping,
      makeAliasTable stms,
      initialGraphConstruction
    ]
    g

applyAugs :: [DepGraphAug] -> DepGraphAug
applyAugs augs g = foldlM (flip ($)) g augs

-- | Add edges for straightforward dependencies to the graph.
addDeps :: DepGraphAug
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

initialGraphConstruction :: DepGraphAug
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
genEdges :: [DepNode] -> EdgeGenerator -> DepGraphAug
genEdges l_stms edge_fun g = do
  name_map <- gets producerMapping
  depGraphInsertEdges (concatMap (gen_edge name_map) l_stms) g
  where
    -- statements -> mapping from declared array names to soac index
    gen_edge :: M.Map VName G.Node -> DepNode -> [G.LEdge EdgeT]
    gen_edge name_map (from, node) =
      [ G.toLEdge (from, to) edgeT | (dep, edgeT) <- edge_fun node, Just to <- [M.lookup dep name_map]
      ]

depGraphInsertEdges :: [DepEdge] -> DepGraphAug
depGraphInsertEdges edgs g = pure $ G.insEdges edgs g

mapAcross :: (DepContext -> FusionEnvM DepContext) -> DepGraphAug
mapAcross f g = foldlM (flip helper) g (G.nodes g)
  where
    helper :: G.Node -> DepGraphAug
    helper n g' = case G.match n g' of
      (Just c, g_new) -> do
        c' <- f c
        pure $ c' G.& g_new
      (Nothing, _) -> pure g'

mapAcrossNodeTs :: (NodeT -> FusionEnvM NodeT) -> DepGraphAug
mapAcrossNodeTs f = mapAcross f'
  where
    f' (ins, n, nodeT, outs) = do
      nodeT' <- f nodeT
      pure (ins, n, nodeT', outs)

mapAcrossWithSE :: (DepNode -> DepGraphAug) -> DepGraphAug
mapAcrossWithSE f g =
  applyAugs (map f (G.labNodes g)) g

updateNode :: G.Node -> (NodeT -> Maybe NodeT) -> DepGraphAug
updateNode n f g =
  case G.context g n of
    (ins, _, l, outs) ->
      case f l of
        Just newLab ->
          pure $ (ins, n, newLab, outs) G.& G.delNode n g
        Nothing -> pure g

nodeToSoacNode :: NodeT -> FusionEnvM NodeT
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

convertGraph :: DepGraphAug
convertGraph = mapAcrossNodeTs nodeToSoacNode

stmFromNode :: NodeT -> Stms SOACS -- do not use outside of edge generation
stmFromNode (StmNode x) = oneStm x
stmFromNode _ = mempty

nodeFromLNode :: DepNode -> G.Node
nodeFromLNode = fst

depsFromEdge :: DepEdge -> VName
depsFromEdge = getName . G.edgeLabel

edgesBetween :: DepGraph -> G.Node -> G.Node -> [DepEdge]
edgesBetween g n1 n2 = G.labEdges $ G.subgraph [n1, n2] g

-- Utility func for augs
augWithFun :: EdgeGenerator -> DepGraphAug
augWithFun f g = genEdges (G.labNodes g) f g

toAlias :: DepGenerator -> EdgeGenerator
toAlias f =
  map (\vname -> (vname, Alias vname)) . namesToList . foldMap f . stmFromNode

addAliases :: DepGraphAug
addAliases = augWithFun $ toAlias aliasInputs

addCons :: DepGraphAug
addCons = augWithFun getStmCons

-- Merges two contexts
mergedContext :: Ord b => a -> G.Context a b -> G.Context a b -> G.Context a b
mergedContext mergedlabel (inp1, n1, _, out1) (inp2, n2, _, out2) =
  let new_inp = filter (\n -> snd n /= n1 && snd n /= n2) (nubOrd (inp1 <> inp2))
      new_out = filter (\n -> snd n /= n1 && snd n /= n2) (nubOrd (out1 <> out2))
   in (new_inp, n1, mergedlabel, new_out)

contractEdge :: G.Node -> DepContext -> DepGraphAug
contractEdge n2 ctx g = do
  let n1 = G.node' ctx -- n1 remains
  pure $ ctx G.& G.delNodes [n1, n2] g

-- extra dependencies mask the fact that consuming nodes "depend" on all other
-- nodes coming before it (now also adds fake edges to aliases - hope this
-- fixes asymptotic complexity guarantees)
addExtraCons :: DepGraphAug
addExtraCons g = do
  aliasTab <- gets aliasTable
  mapping <- gets producerMapping
  let edges = concatMap (make_edge aliasTab mapping) (G.labEdges g)
  depGraphInsertEdges edges g
  where
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

addResEdges :: DepGraphAug
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

isCons :: EdgeT -> Bool
isCons (Cons _) = True
isCons _ = False
