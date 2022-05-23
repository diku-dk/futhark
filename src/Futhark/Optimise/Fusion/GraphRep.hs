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
    getSoac,
    isScanRed,
    findTransformsBetween,
    isTrDep,
    isRealNode,
    nodeFromLNode,
    internalizeOutput,
    mergedContext,
    mapAcross,
    genEdges,
    edgesBetween,
    keepTrying,
    isDep,
    isInf,
    applyAugs,
    makeMapping,
    initialGraphConstruction,
    emptyGraph,
    isArray,
    isArray2,
    depsFromEdge,
    contractEdge,
    isCons,
    updateTrEdges,
    makeMap,
    fuseMaps,
    internalizeAndAdd,
    mapAcrossWithSE,
    updateNode,
    substituteNamesInNodes,
    substituteNamesInEdges,
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
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Futhark.Analysis.Alias as Alias
import qualified Futhark.Analysis.HORep.SOAC as H
import Futhark.Builder
import Futhark.IR.Prop.Aliases
import Futhark.IR.SOACS hiding (SOAC (..))
import qualified Futhark.IR.SOACS as Futhark
import qualified Futhark.Optimise.Fusion.LoopKernel as LK
import Futhark.Transform.Substitute

data EdgeT
  = Alias VName
  | InfDep VName
  | Dep VName
  | Cons VName
  | Fake VName
  | Res VName
  | ScanRed VName
  | TrDep VName
  deriving (Eq, Ord)

data NodeT
  = StmNode (Stm SOACS)
  | SoacNode (H.SOAC SOACS) [H.Input] (StmAux (ExpDec SOACS))
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
  show (ScanRed vName) = "SR " <> pretty vName
  show (TrDep vName) = "Tr " <> pretty vName

instance Show NodeT where
  show (StmNode (Let pat _ _)) = L.intercalate ", " $ map pretty $ patNames pat
  show (SoacNode _ pat _) = L.intercalate ", " $ map (pretty . H.inputArray) pat
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
    SoacNode so pat sa ->
      let so' = case so of
            H.Stream se sf la ses ins -> H.Stream (f se) (fStreamForm sf) (f la) (f ses) (f ins)
            H.Scatter se la ins x0 -> H.Scatter (f se) (f la) (f ins) (map fShape x0)
            H.Screma se sf ins -> H.Screma (f se) (fScremaForm sf) (f ins)
            H.Hist se hos la ins -> H.Hist (f se) (map fHistOp hos) (f la) (f ins)
       in SoacNode so' (f pat) (f sa)
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

getSoac :: NodeT -> Maybe (H.SOAC SOACS)
getSoac s = case s of
  SoacNode soac _ _ -> Just soac
  _ -> Nothing

getName :: EdgeT -> VName
getName edgeT = case edgeT of
  Alias vn -> vn
  InfDep vn -> vn
  Dep vn -> vn
  Cons vn -> vn
  Fake vn -> vn
  Res vn -> vn
  ScanRed vn -> vn
  TrDep vn -> vn

setName :: VName -> EdgeT -> EdgeT
setName vn edgeT = case edgeT of
  Alias _ -> Alias vn
  InfDep _ -> InfDep vn
  Dep _ -> Dep vn
  Cons _ -> Cons vn
  Fake _ -> Fake vn
  Res _ -> Res vn
  ScanRed _ -> ScanRed vn
  TrDep _ -> TrDep vn

inputFromPat :: Typed rep => Pat rep -> [H.Input]
inputFromPat = map H.identInput . patIdents

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
    fuseScans :: Bool
  }

freshFusionEnv :: FusionEnv
freshFusionEnv =
  FusionEnv
    { vNameSource = blankNameSource,
      producerMapping = M.empty,
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

-- Fixed-point
keepTrying :: DepGraphAug -> DepGraphAug
keepTrying f g = do
  r <- f g
  r2 <- f r
  if G.equal r r2
    then pure r
    else keepTrying f r2

isArray :: FParam SOACS -> Bool
isArray p = case paramDec p of
  Array {} -> True
  _ -> False

isArray2 :: LParam SOACS -> Bool
isArray2 p = case paramDec p of
  Array {} -> True
  _ -> False

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

mkDepGraph :: Stms SOACS -> Names -> Names -> FusionEnvM DepGraph
mkDepGraph stms res inputs = do
  let g = emptyGraph stms res inputs
  _ <- makeMapping g
  initialGraphConstruction g

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

makeScanInfusible :: DepGraphAug
makeScanInfusible g = pure $ G.emap change_node_to_idep g
  where
    find_scan_results :: Stm SOACS -> [VName]
    find_scan_results (Let pat _ (Op (Futhark.Screma _ _ (ScremaForm scns rdcs _)))) =
      let resLen = scanResults scns + redResults rdcs
       in take resLen (patNames pat)
    find_scan_results _ = []

    scan_res_set :: S.Set VName
    scan_res_set = S.fromList (concatMap find_scan_results (foldMap (stmFromNode . label) (G.labNodes g)))

    is_scan_res :: VName -> Bool
    is_scan_res name = S.member name scan_res_set

    change_node_to_idep :: EdgeT -> EdgeT
    change_node_to_idep (Dep name) =
      if is_scan_res name
        then ScanRed name
        else Dep name
    change_node_to_idep e = e

initialGraphConstruction :: DepGraphAug
initialGraphConstruction =
  applyAugs
    [ addDeps,
      makeScanInfusible,
      addCons,
      addExtraCons,
      addResEdges,
      addAliases,
      convertGraph, -- Must be done after adding edges
      keepTrying addTransforms,
      iswim
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

delLEdges :: [DepEdge] -> DepGraphAug
delLEdges edgs g = pure $ foldl (flip ($)) g (map G.delLEdge edgs)

depGraphInsertEdges :: [DepEdge] -> DepGraphAug
depGraphInsertEdges edgs g = pure $ G.insEdges edgs g

updateTrEdges :: G.Node -> DepGraphAug
updateTrEdges n1 g = do
  let (inc, _, _nt, otg) = G.context g n1
  let relevantInc = relNodes inc
  let relevantOtg = relNodes otg
  let augs =
        map (updateTrEdgesBetween n1) relevantInc
          <> map (`updateTrEdgesBetween` n1) relevantOtg
  applyAugs augs g
  where
    relNodes :: G.Adj EdgeT -> [G.Node]
    relNodes adjs = map snd $ filter (isDep . fst) adjs

updateTrEdgesBetween :: G.Node -> G.Node -> DepGraphAug
updateTrEdgesBetween n1 n2 g = do
  let ns = map (getName . G.edgeLabel) $ filter (isDep . G.edgeLabel) $ edgesBetween g n1 n2
  let edgs = mapMaybe insEdge ns
  depGraphInsertEdges edgs =<< delLEdges edgs g
  where
    nt1 = G.lab' $ G.context g n1
    nt2 = G.lab' $ G.context g n2
    insEdge :: VName -> Maybe DepEdge
    insEdge name =
      if H.nullTransforms $ findTransformsBetween name nt1 nt2
        then Nothing
        else Just (n2, n1, TrDep name)

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

transformsFromInputs :: VName -> [H.Input] -> H.ArrayTransforms
transformsFromInputs name1 is = case L.filter filterFun is of
  [] -> error "missing input from list"
  [x] -> H.inputTransforms x
  xs | [ts] <- L.nub (map H.inputTransforms xs) -> ts
  _ -> error "variable with differeing transformations"
  where
    filterFun (H.Input _ name2 _) = name1 == name2

nodeInputTransforms :: VName -> NodeT -> H.ArrayTransforms
nodeInputTransforms vname nodeT = case nodeT of
  SoacNode soac _ _ -> transformsFromInputs vname (H.inputs soac)
  _ -> H.noTransforms

nodeOutputTransforms :: VName -> NodeT -> H.ArrayTransforms
nodeOutputTransforms vname nodeT = case nodeT of
  SoacNode _ outs _ -> transformsFromInputs vname outs
  _ -> H.noTransforms

internalizeOutput :: H.Input -> H.Input
internalizeOutput i@(H.Input ts name _) = H.Input ts name (H.inputType i)

internalizeAndAdd :: H.ArrayTransforms -> H.Input -> H.Input
internalizeAndAdd ts i = H.setInputTransforms newTs $ internalizeOutput temporaryI
  where
    oldTs = H.inputTransforms i
    temporaryI = H.setInputTransforms ts i
    newTs = ts <> oldTs

findTransformsBetween :: VName -> NodeT -> NodeT -> H.ArrayTransforms
findTransformsBetween vname n1 n2 =
  let outs = nodeOutputTransforms vname n1
   in let ins = nodeInputTransforms vname n2
       in outs <> ins

iswim :: DepGraphAug
iswim = mapAcrossWithSE f
  where
    f (n, SoacNode soac ots aux) g = do
      scope <- askScope
      maybeISWIM <- LK.tryFusion (LK.iswim Nothing soac H.noTransforms) scope
      case maybeISWIM of
        Just (newSOAC, newts) ->
          updateNode n (const (Just $ SoacNode newSOAC (map (internalizeAndAdd newts) ots) aux)) g
            >>= updateTrEdges n
            >>= updateContext n
        Nothing -> pure g
    f _ g = pure g

    updateContext :: G.Node -> DepGraphAug
    updateContext n g =
      case G.match n g of
        (Just (ins, _, l, outs), g') -> do
          let newins = map (\(e, n2) -> if isScanRed e then (Dep (getName e), n2) else (e, n2)) ins
          pure $ (newins, n, l, outs) G.& g'
        _ -> pure g

addTransforms :: DepGraphAug
addTransforms orig_g =
  applyAugs (map helper ns) orig_g
  where
    ns = G.nodes orig_g
    helper :: G.Node -> DepGraphAug
    helper n g = case G.lab g n of
      Just (StmNode (Let pat aux e))
        | Just (vn, transform) <- H.transformFromExp (stmAuxCerts aux) e,
          [n'] <- L.nub $ G.suc g n,
          vn
            `notElem` map
              (getName . G.edgeLabel)
              (filter (\(a, _, _) -> a /= n) (G.inn g n')),
          Just (SoacNode soac outps aux2) <- G.lab g n',
          [trName] <- patNames pat -> do
            let sucNodes = G.pre g n
            g' <- depGraphInsertEdges (map (\nd -> (nd, n', TrDep trName)) sucNodes) g
            let outps' =
                  map
                    ( \inp ->
                        if H.inputArray inp /= vn
                          then inp
                          else H.addTransform transform inp
                    )
                    outps
            let mapping = makeMap [vn] [trName]
            let newNode = substituteNames mapping (SoacNode soac outps' (aux <> aux2))
            let ctx = mergedContext newNode (G.context g' n') (G.context g' n)
            contractEdge n ctx g'
      _ -> pure g

substituteNamesInNodes :: M.Map VName VName -> [G.Node] -> DepGraphAug
substituteNamesInNodes submap ns =
  applyAugs (map (substituteNameInNode submap) ns)
  where
    substituteNameInNode :: M.Map VName VName -> G.Node -> DepGraphAug
    substituteNameInNode m n =
      updateNode n (Just . substituteNames m)

substituteNamesInEdges :: M.Map VName VName -> [DepEdge] -> DepGraphAug
substituteNamesInEdges m edgs g =
  let edgs' = mapEdgeT (substituteNames m) edgs
   in pure $ G.insEdges edgs' $ foldl (flip ($)) g (map G.delLEdge edgs)
  where
    mapEdgeT f = map (\(a, b, c) -> (a, b, f c))

updateNode :: G.Node -> (NodeT -> Maybe NodeT) -> DepGraphAug
updateNode n f g =
  case G.context g n of
    (ins, _, l, outs) ->
      case f l of
        Just newLab ->
          pure $ (ins, n, newLab, outs) G.& G.delNode n g
        Nothing -> pure g

nodeToSoacNode :: NodeT -> FusionEnvM NodeT
nodeToSoacNode n@(StmNode s@(Let pats aux op)) = case op of
  Op {} -> do
    maybeSoac <- H.fromExp op
    case maybeSoac of
      Right hsoac -> pure $ SoacNode hsoac (inputFromPat pats) aux
      Left H.NotSOAC -> pure n
  DoLoop {} ->
    pure $ DoNode s []
  If {} ->
    pure $ IfNode s []
  _ -> pure n
nodeToSoacNode n = pure n

convertGraph :: DepGraphAug
convertGraph = mapAcrossNodeTs nodeToSoacNode

label :: DepNode -> NodeT
label = snd

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
toAlias f stmt =
  map (\vname -> (vname, Alias vname)) $
    namesToList $ foldMap f (stmFromNode stmt)

addAliases :: DepGraphAug
addAliases = augWithFun $ toAlias aliasInputs

addCons :: DepGraphAug
addCons = augWithFun getStmCons

-- Merges two contexts
mergedContext :: (Eq b) => a -> G.Context a b -> G.Context a b -> G.Context a b
mergedContext mergedlabel (inp1, n1, _, out1) (inp2, n2, _, out2) =
  let new_inp = L.nub $ filter (\n -> snd n /= n1 && snd n /= n2) (inp1 `L.union` inp2)
   in let new_out = L.nub $ filter (\n -> snd n /= n1 && snd n /= n2) (out1 `L.union` out2)
       in (new_inp, n1, mergedlabel, new_out)

contractEdge :: G.Node -> DepContext -> DepGraphAug
contractEdge n2 ctx g = do
  let n1 = G.node' ctx -- n1 remains
  pure $ ctx G.& G.delNodes [n1, n2] g

-- extra dependencies mask the fact that consuming nodes "depend" on all other
-- nodes coming before it
addExtraCons :: DepGraphAug
addExtraCons g = depGraphInsertEdges new_edges g
  where
    new_edges = concatMap make_edge (G.labEdges g)
    make_edge :: DepEdge -> [DepEdge]
    make_edge (from, to, Cons cname) =
      [ G.toLEdge (from, to2) (Fake cname)
        | (to2, _) <-
            filter
              ( \(tonode, toedge) ->
                  tonode /= from
                    && cname == getName toedge
              )
              $ G.lpre g to
      ]
    make_edge _ = []

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
expInputs e = freeClassifications e

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
  (SoacNode _ outputs _) -> map H.inputArray outputs

isDep :: EdgeT -> Bool -- Is there a possibility of fusion?
isDep (Dep _) = True
isDep (ScanRed _) = True
isDep (Res _) = True
isDep _ = False

isInf :: (G.Node, G.Node, EdgeT) -> Bool -- No possibility of fusion
isInf (_, _, e) = case e of
  InfDep _ -> True
  Cons _ -> False
  Fake _ -> True -- this is infusible to avoid simultaneous cons/dep edges
  TrDep _ -> False
  _ -> False

isCons :: EdgeT -> Bool
isCons (Cons _) = True
isCons _ = False

isScanRed :: EdgeT -> Bool
isScanRed (ScanRed _) = True
isScanRed _ = False

isTrDep :: EdgeT -> Bool
isTrDep (TrDep _) = True
isTrDep _ = False
