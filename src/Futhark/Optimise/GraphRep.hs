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
import qualified Futhark.Analysis.HORep.SOAC as H

--import qualified Data.Graph.Inductive.Query.DFS as Q
import qualified Data.Graph.Inductive.Tree as G
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Dot
--import Futhark.IR.Pretty as PP
import qualified Futhark.Util.Pretty as PP

--import Debug.Trace
--import Debug.Trace
--import Debug.Trace
--import Debug.Trace
--import Debug.Trace
--import Debug.Trace
--import Debug.Trace
--import Debug.Trace
import Futhark.Builder (MonadFreshNames (putNameSource), VNameSource, getNameSource, modifyNameSource, blankNameSource, runBuilder, auxing, letBind)
--import Futhark.Pass
import Data.Foldable (foldlM)
import Control.Monad.State
import Futhark.Transform.Substitute (Substitute (substituteNames), Substitutable)
import Control.Monad.Reader (ReaderT (runReaderT))
import Foreign (bitReverse32)
-- import qualified Futhark.Analysis.HORep.MapNest as HM




-- TODO: Move to IR/Graph.hs at some point, for now keeping in Optimise/

-- SNode: Stm [InputTransforms] [OutputTransforms]
data EdgeT =
    Alias VName
  | InfDep VName
  | Dep VName
  | Cons VName
  | Fake VName
  | Res VName
  | ScanRed VName
  deriving (Eq, Ord)

data NodeT =
    StmNode (Stm SOACS)
  | SoacNode (H.SOAC SOACS) (Pat (LetDec SOACS)) (StmAux (ExpDec SOACS))
  | RNode VName
  | InNode VName
  | FinalNode [Stm SOACS] NodeT
  | IfNode (Stm SOACS) [(NodeT, [EdgeT])]
  | DoNode (Stm SOACS) [(NodeT, [EdgeT])]
  deriving (Eq)


getName :: EdgeT -> VName
getName edgeT = case edgeT of
  Alias vn -> vn
  InfDep vn -> vn
  Dep vn -> vn
  Cons vn -> vn
  Fake vn -> vn
  Res vn -> vn
  ScanRed vn -> vn


setName :: VName -> EdgeT -> EdgeT
setName vn edgeT = case edgeT of
  Alias _ -> Alias vn
  InfDep _ -> InfDep vn
  Dep _ -> Dep vn
  Cons _ -> Cons vn
  Fake _ -> Fake vn
  Res _ -> Res vn
  ScanRed _ -> ScanRed vn



instance Substitute EdgeT where
  substituteNames m edgeT =
    let newName = substituteNames m (getName edgeT)
    in setName newName edgeT


instance Substitute NodeT where
  substituteNames m nodeT = case nodeT of
    StmNode stm -> StmNode (f stm)
    SoacNode so pat sa -> let lam = H.lambda so in
      SoacNode (H.setLambda (f lam) so) (f pat) (f sa) -- this may be ridiculously buggy, just a heads-up
    RNode vn -> RNode (f vn)
    InNode vn -> InNode (f vn)
    FinalNode stms nt -> FinalNode (map f stms) (f nt)
    IfNode stm nodes -> IfNode (f stm) (map f nodes)
    DoNode stm nodes -> DoNode (f stm) (map f nodes)

    where
      f :: Substitute a => a -> a
      f = substituteNames m


instance Show EdgeT where
  show (Dep vName) = "Dep " <> ppr vName
  show (InfDep vName) = "iDep " <> ppr vName
  show (Cons _) = "Cons"
  show (Fake _) = "Fake"
  show (Res _) = "Res"
  show (Alias _) = "Alias"
  show (ScanRed vName) = "SR " <> ppr vName

-- inputs could have their own edges - to facilitate fusion


-- nodeT_to_str
instance Show NodeT where
    show (StmNode (Let pat _ _)) = L.intercalate ", " $ map ppr $ patNames pat
    show (SoacNode _ pat _) = L.intercalate ", " $ map ppr $ patNames pat
    show (FinalNode stms nt) = show nt
    show (RNode name)  = ppr $ "Res: "   ++ ppr name
    show (InNode name) = ppr $ "Input: " ++ ppr name
    show (IfNode stm nodes) =  "If: " ++ L.intercalate ", " (map ppr $ getStmNames stm)
    show (DoNode stm nodes) =  "Do: " ++ L.intercalate ", " (map ppr $ getStmNames stm)


-- does the node acutally represent something in the program
-- (non-real nodes are not delayed-fused into other nodes)
isRealNode :: NodeT -> Bool
isRealNode RNode {} = False
isRealNode InNode {} = False
isRealNode _ = True


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
    --reachabilityG :: G.Gr () (),
    producerMapping :: M.Map VName Node,
    fuseScans :: Bool
  }

freshFusionEnv :: FusionEnv
freshFusionEnv = FusionEnv {vNameSource = blankNameSource,
                            producerMapping = M.empty,
                            fuseScans = True}

newtype FusionEnvM a = FusionEnvM ( ReaderT
          (Scope SOACS)
          (State FusionEnv)
          a
    )
  deriving
    (
      Monad,
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


runFusionEnvM ::  MonadFreshNames m => Scope SOACS -> FusionEnv -> FusionEnvM a -> m a
-- runFusionEnvM scope fenv (FusionEnvM a) = do
--   ns <- getNameSource
--   let r = runReaderT a scope
--   let fenv2 = fenv {vNameSource = ns}
--   return $ evalState r fenv2
runFusionEnvM scope fenv (FusionEnvM a) = modifyNameSource $ \src ->
    let x = runReaderT a scope in
    let (y,z) = runState x (fenv {vNameSource = src}) in
    (y, vNameSource z)
  -- modifyNameSource $



-- runFusionEnvM ::
--   MonadFreshNames m =>
--   FusionEnvM a ->
--   FusionEnv ->
--   m a
-- runFusionEnvM (FusionEnvM a) env =
--   modifyNameSource $ \src -> let (new_a, new_env) = runState a (env {vNameSource = src}) in (new_a, vNameSource new_env)


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
    snodes = map StmNode stms
    rnodes = map RNode res
    inNodes= map InNode inputs

initGraph :: DepGraphAug
initGraph g = do
  _ <- makeMapping g
  addDepEdges g


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
  [addDeps2,
  makeScanInfusible,
  addInfDeps,
  addCons,
  addExtraCons,
  addResEdges,
  addAliases,--, appendTransformations
  convertGraph] -- this one must be done last


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

mapAcross :: (DepContext -> FusionEnvM DepContext) -> DepGraphAug
mapAcross f g =
  do
    let ns = nodes g
    foldlM (flip helper) g ns
    where
      helper :: Node -> DepGraphAug
      helper n g' = case match n g' of
        (Just c, g_new) ->
          do
            c' <- f c
            pure $ c' & g_new
        (Nothing, _) -> pure g'

--
mapAcrossNodeTs :: (NodeT -> FusionEnvM NodeT) -> DepGraphAug
mapAcrossNodeTs f = mapAcross f'
  where
    f' (ins, n, nodeT, outs) =
      do
        nodeT' <- f nodeT
        return (ins, n, nodeT', outs)



nodeToSoacNode :: NodeT -> FusionEnvM NodeT
nodeToSoacNode n@(StmNode s@(Let pats aux op)) = case op of
  Op {} -> do
    maybeSoac <- H.fromExp op
    case maybeSoac of
      Right hsoac -> pure $ SoacNode hsoac pats aux
      Left H.NotSOAC -> pure n
  -- add if, loops, (maybe transformations)
  DoLoop {} -> -- loop-node
    pure $ DoNode s []
  If {} -> do
    pure $ IfNode s []
  _ -> pure n
nodeToSoacNode n = pure n

convertGraph :: DepGraphAug
convertGraph = mapAcrossNodeTs nodeToSoacNode


--- /Graph Construction

--- Extracting Nodes/Edges ---

label :: DepNode -> NodeT
label = snd

-- do not use outside of edge generation
stmFromNode :: NodeT -> [Stm SOACS]
stmFromNode (StmNode x) = [x]
-- stmFromNode SoacNode {} = []
-- stmFromNode (FinalNode x nt) = x ++ stmFromNode nt
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


depsFromEdge ::  DepEdge -> VName
depsFromEdge = getName . edgeLabel


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
      && cname == getName toedge
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
      then ScanRed name
      else Dep name
    change_node_to_idep e = e

-- Utils for fusibility/infusibility
-- find dependencies - either fusable or infusable. edges are generated based on these


fusableInputs :: Stm SOACS -> [VName]
fusableInputs (Let _ _ expr) = fusableInputsFromExp expr

fusableInputsFromExp :: Exp SOACS -> [VName]
fusableInputsFromExp (If _ b1 b2 _) =
  concatMap fusableInputs (bodyStms b1) <>
  concatMap fusableInputs (bodyStms b2)
fusableInputsFromExp (DoLoop _ _ b1) =
  concatMap fusableInputs (bodyStms b1)
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
infusableInputsFromExp (If exp b1 b2 cond) =
  let emptyB = Body mempty mempty mempty :: Body SOACS in
  namesToList (freeIn exp
  <> freeIn cond)
  <> concatMap infusableInputs (bodyStms b1)
  <> concatMap infusableInputs (bodyStms b2)
infusableInputsFromExp (DoLoop exp loopform b1) =
  let emptyB = Body mempty mempty mempty :: Body SOACS in
  concatMap infusableInputs (bodyStms b1)
    <> namesToList (freeIn (DoLoop exp loopform emptyB))
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
getStmCons (StmNode s) = zip names (map Cons names)
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
  (StmNode stm) -> getStmNames stm
  (RNode _)   -> []
  (InNode name) -> [name]
  (IfNode stm nodes) -> getStmNames stm
  (DoNode stm nodes) -> getStmNames stm
  (FinalNode stms _) -> error "Final nodes cannot generate edges" -- concatMap getStmNames stms
  (SoacNode _ pats _) -> patNames pats

--- /Inspecting Stms ---


mapT :: (a -> b) -> (a,a) -> (b,b)
mapT f (a,b) = (f a, f b)



finalizeNode :: NodeT -> FusionEnvM [Stm SOACS]
finalizeNode nt = case nt of
  StmNode stm -> pure [stm]
  SoacNode soac pats aux -> do
    (_, stms) <-  runBuilder $ do
      new_soac <- H.toSOAC soac
      auxing aux $ letBind pats $ Op new_soac
    return $ stmsToList stms
  RNode vn  -> pure []
  InNode vn -> pure []
  DoNode stm lst -> do
    stmsNotFused <- mapM (finalizeNode . fst) lst
    pure $ concat stmsNotFused ++ [stm]
  IfNode stm lst -> do
    stmsNotFused <- mapM (finalizeNode . fst) lst
    pure $ concat stmsNotFused ++ [stm]
  FinalNode stms nt' -> do
    stms' <- finalizeNode nt'
    pure $ stms <> stms'
