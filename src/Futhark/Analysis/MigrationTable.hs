module Futhark.Analysis.MigrationTable (
  -- * Analysis
  analyseProg,

  -- * Query
  MigrationTable,
  moveToDevice,
  usedOnHost,
) where

import Control.Monad
import qualified Control.Monad.Writer.Lazy as W
import Control.Monad.Trans.RWS.Lazy
import Control.Parallel.Strategies (parMap, rpar)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Futhark.Analysis.MigrationGraph hiding (empty, addEdges, get)
import qualified Futhark.Analysis.MigrationGraph as MG
import Futhark.IR.GPU

-- | Where the value bound by a name should be computed.
data MigrationStatus
  -- | The statement that computes the value should be moved to device.
  -- No host usage of the value will be left after the migration.
  = MoveToDevice
  -- | As MoveToDevice but host usage of the value will remain after migration.
  | UsedOnHost
  -- | The statement that computes the value should remain on host.
  | StayOnHost
  deriving (Eq, Ord)

-- | Identifies
--
--     (1) which statements should be moved from host to device to approximately
--         minimize the worst case number of device-host scalar reads.
--
--     (2) which migrated variables that still will be used on the host after
--         all such statements have been moved.
--
newtype MigrationTable = MigrationTable (IM.IntMap MigrationStatus)

statusOf :: VName -> MigrationTable -> MigrationStatus
statusOf v (MigrationTable mt) =
  fromMaybe UsedOnHost $ IM.lookup (baseTag v) mt

-- | Should this whole statement be moved from host to device?
moveToDevice :: Stm GPU -> MigrationTable -> Bool
moveToDevice (Let (Pat ((PatElem v _):_)) _ (BasicOp _)) mt =
  (statusOf v mt) /= StayOnHost
moveToDevice (Let (Pat ((PatElem v _):_)) _ (Apply _ _ _ _)) mt =
  (statusOf v mt) /= StayOnHost
moveToDevice (Let _ _ (If (Var v) _ _ _)) mt =
  (statusOf v mt) == MoveToDevice
moveToDevice (Let _ _ (DoLoop _ (ForLoop _ _ (Var v) _) _)) mt =
  (statusOf v mt) == MoveToDevice
moveToDevice (Let _ _ (DoLoop _ (WhileLoop v) _)) mt =
  (statusOf v mt) == MoveToDevice
-- BasicOp and Apply statements might not bind any variables.
-- If statements might use a constant branch condition.
-- For loop statements might use a constant number of iterations.
-- HostOp statements cannot execute on device.
-- WithAcc statements are never moved in their entirety.
moveToDevice _ _ = False

-- | Will the variable by this name still be used on host after all statements
-- identified by this table have been migrated?
usedOnHost :: VName -> MigrationTable -> Bool
usedOnHost v mt = (statusOf v mt) == UsedOnHost

-- | Merges two migration tables that are assumed to be disjoint.
merge :: MigrationTable -> MigrationTable -> MigrationTable
merge (MigrationTable a) (MigrationTable b) = MigrationTable (a `IM.union` b)

-- | The empty migration table.
empty :: MigrationTable
empty = MigrationTable IM.empty

-- | Analyses a program to return a migration table that covers all its
-- statements and variables.
analyseProg :: Prog GPU -> MigrationTable
analyseProg (Prog consts funs) =
  let hof = hostOnlyFunDefs funs
      mt  = analyseConsts hof consts
      mts = parMap rpar (analyseFunDef hof) funs
  in foldl' merge mt mts

-- | Returns the names of all top-level functions that cannot be called from the
-- device. The application of such a function is host-only.
hostOnlyFunDefs :: [FunDef GPU] -> HostOnlyFuns
hostOnlyFunDefs funs =
  let ns = map funDefName funs
      m  = M.fromList $ zip ns (map checkFunDef funs)
  in (S.fromList ns) \\ (keysToSet $ rmHostOnly m)
  where
    keysToSet = S.fromAscList . M.keys

    rmHostOnly m =
      let (a, b) = M.partition isNothing m 
          done   = M.null a
      in if done then b else
         rmHostOnly $ M.map (test (keysToSet a)) b

    test s1 (Just s2) | s1 `S.disjoint` s2 = Just s2
    test _ _                               = Nothing

-- | 'checkFunDef' returns 'Nothing' if this function definition uses arrays or
-- HostOps. Otherwise it returns the names of all applied functions, which may
-- include user defined functions that could turn out to be host-only.
checkFunDef :: FunDef GPU -> Maybe (Set Name)
checkFunDef fun = do
  checkFParams (funDefParams fun)
  checkRetTypes (funDefRetType fun)
  checkBody (funDefBody fun)
  where
    hostOnly = Nothing
    ok       = Just ()
    check b  = if b then hostOnly else ok
    isArray  = not . primType

    checkFParams ps = check $ any (isArray . typeOf) ps

    checkLParams ps = check $ any (isArray . typeOf . fst) ps
    
    checkRetTypes rs = check $ any isArray rs

    checkPats pats = check $ any (isArray . typeOf) pats

    checkLoopForm (ForLoop _ _ _ (_:_)) = hostOnly
    checkLoopForm _                     = ok

    checkBody = checkStms . bodyStms

    checkStms stms = fmap S.unions (mapM checkStm stms)

    checkStm (Let (Pat pats) _ e) = checkPats pats >> checkExp e

    -- Any expression that produces an array is caught by checkPat
    checkExp (BasicOp (Index _ _))     = hostOnly
    checkExp (BasicOp (FlatIndex _ _)) = hostOnly
    checkExp (WithAcc _ _)             = hostOnly
    checkExp (Op _)                    = hostOnly
    checkExp (Apply n _ _ _)           = Just (S.singleton n)
    checkExp (If _ t f _)              = do s1 <- checkBody t
                                            s2 <- checkBody f
                                            return $ s1 `S.union` s2
    checkExp (DoLoop params form body) = do checkLParams params
                                            checkLoopForm form
                                            checkBody body
    checkExp _                         = Just (S.empty)

-- | Analyses top-level constants.
analyseConsts :: HostOnlyFuns -> Stms GPU -> MigrationTable
analyseConsts hof consts =
  let usage = concatMap f $ M.assocs (scopeOf consts)
  in analyseStms hof usage consts
  where
    f (v, n) | isScalar n = [nameToId v]
    f _                   = []

-- | Analyses a top-level function definition.
analyseFunDef :: HostOnlyFuns -> FunDef GPU -> MigrationTable
analyseFunDef hof fd =
  let body  = funDefBody fd
      usage = concatMap f $ zip (bodyResult body) (funDefRetType fd)
      stms  = bodyStms body
  in analyseStms hof usage stms
  where
    f ((SubExpRes _ (Var v)), t) | isScalarType t = [nameToId v]
    f _                                           = []

isScalar :: Typed t => t -> Bool
isScalar = isScalarType . typeOf

isScalarType :: TypeBase shape u -> Bool
isScalarType (Prim Unit) = False
isScalarType (Prim _)    = True
isScalarType _           = False

-- | HostUsage identifies scalar variables that are used on host.
type HostUsage = [Id]

-- | Analyses statements. The 'HostUsage' list identifies which bound scalar
-- variables that subsequently may be used on host. All free variables such as
-- constants and function parameters are assumed to reside on host.
analyseStms :: HostOnlyFuns -> HostUsage -> Stms GPU -> MigrationTable
analyseStms hof usage stms =
  let (g, srcs, snks1)   = buildGraph hof usage stms
      (routed, unrouted) = srcs
      (snks2, g')        = MG.routeMany unrouted g
  -- TODO: Mark D
  -- TODO: Make reads conditional
  in empty -- TODO

buildGraph :: HostOnlyFuns -> HostUsage -> Stms GPU -> (Graph, Sources, Sinks)
buildGraph hof usage stms =
  let (g, srcs, snks) = execGrapher hof (graphStms stms)
      g' = foldl' (flip MG.connectToSink) g usage
  in (g', srcs, snks)



type Grapher = RWS Env BodyStats State

data Env = Env
  { -- | See 'HostOnlyFuns'.
    envHostOnlyFuns :: HostOnlyFuns,
    -- | How to graph some operation based on previous, non-local information.
    envActionTable :: ActionTable,
    -- | Metadata for the current body being graphed.
    envMeta :: Meta
  }

-- | Identifies top-level function definitions that cannot be run on the
-- device. The application of any such function is host-only.
type HostOnlyFuns = Set Name

-- | From 'Id' to 'GraphAction'. Entries for 'UpdateAcc' expressions are
-- inserted under the token of their associated accumulator.
type ActionTable = IM.IntMap GraphAction

-- | How to graph some operation based on previous, non-local information.
data GraphAction
  = HostOp    -- ^ Operation is host-only.
  | DeviceOp  -- ^ Operation should be moved to device.
  | NeutralOp -- ^ How to handle the operation has not been determined yet.
  deriving (Eq)

instance Semigroup GraphAction where
  _        <> HostOp    = HostOp
  DeviceOp <> DeviceOp  = DeviceOp
  x        <> NeutralOp = x
  x        <> y         = y <> x

instance Monoid GraphAction where
  mempty = NeutralOp

-- | Statistics on the statements within a body and their dependencies.
data BodyStats = BodyStats
  { -- Whether the body contained any host-only statements.
    bodyHostOnly :: Bool,
    -- Whether the body performed any reads.
    bodyReads :: Bool,
    -- All scalar variables represented in the graph that have been used
    -- as operands within the body, including those that have been added
    -- by the body itself. Variables with vertices connected to sinks may
    -- be excluded.
    bodyOperands :: IdSet
  }

instance Semigroup BodyStats where
  (BodyStats ho1 r1 o1) <> (BodyStats ho2 r2 o2) =
    BodyStats { bodyHostOnly = ho1 || ho2,
                bodyReads    = r1 || r2,
                bodyOperands = IS.union o1 o2}

instance Monoid BodyStats where
  mempty = BodyStats { bodyHostOnly = False,
                       bodyReads    = False,
                       bodyOperands = IS.empty}

data State = State
  {
    stateGraph :: Graph,
    stateGraphedScalars :: IdSet,
    stateSources :: Sources,
    stateSinks :: Sinks
  }

-- | All vertices connected from a source, partitioned into those that have
-- been attempted routed and those which have not.
type Sources = ([Id], [Id])

-- | All terminal vertices of routes.
type Sinks = [Id]



execGrapher :: HostOnlyFuns -> Grapher a -> (Graph, Sources, Sinks)
execGrapher hof g =
  let s = fst (execRWS g env st)
  in (stateGraph s, stateSources s, stateSinks s)
  where
    env = Env   hof IM.empty (Meta 0 Nothing)
    st  = State MG.empty IS.empty ([], []) []


-- | Get the graph under construction.
getGraph :: Grapher Graph
getGraph = gets stateGraph

-- | All scalar variables with a vertex representation in the graph.
getGraphedScalars :: Grapher IdSet
getGraphedScalars = gets stateGraphedScalars

-- | All source connected vertices that have been added to the graph so far.
getSources :: Grapher Sources
getSources = gets stateSources


-- | Update graph under construction.
modifyGraph :: (Graph -> Graph) -> Grapher ()
modifyGraph f =
  modify $ \s -> s { stateGraph = f (stateGraph s) }

-- | Update the contents of the graphed scalar set.
modifyGraphedScalars :: (IdSet -> IdSet) -> Grapher ()
modifyGraphedScalars f =
  modify $ \s -> s { stateGraphedScalars = f (stateGraphedScalars s) }

-- | Update the set of source connected vertices.
modifySources :: (Sources -> Sources) -> Grapher ()
modifySources f =
  modify $ \s -> s { stateSources = f (stateSources s) }


-- | Increment the fork depth for variables graphed by this action.
incForkDepthFor :: Grapher a -> Grapher a
incForkDepthFor =
  local $ \env ->
    let meta  = envMeta env
        fd = metaForkDepth meta
    in env { envMeta = meta { metaForkDepth = fd+1 } }

-- | Change the graph id for variables graphed by this action.
graphIdFor :: Id -> Grapher a -> Grapher a
graphIdFor i =
  local $ \env ->
    let meta = envMeta env
    in env { envMeta = meta { metaGraphId = Just i } }

-- | Add these entries to the 'ActionTable' for this action.
withActions :: Grapher a -> ActionTable -> Grapher a
withActions g table = local f g
  where
    f env = env { envActionTable = table <> (envActionTable env) }


-- | Can applications of this function be moved to device?
isHostOnlyFun :: Name -> Grapher Bool
isHostOnlyFun n = asks $ S.member n . envHostOnlyFuns

-- | How to graph updates of the accumulator with the given token.
getAction :: Id -> Grapher GraphAction
getAction i = do
  table <- asks envActionTable
  return (table IM.! i)

-- | Get the 'Meta' corresponding to the current body.
getMeta :: Grapher Meta
getMeta = asks envMeta


graphBody :: BodyT GPU -> Grapher BodyStats
graphBody b = fmap snd $ listen $ graphStms (bodyStms b)

graphStms :: Stms GPU -> Grapher ()
graphStms = mapM_ graphStm

graphStm :: Stm GPU -> Grapher ()
graphStm stm = do
  let b = boundBy stm
  let e = stmExp stm 
  case e of
    BasicOp (Assert {})      -> graphAssert    (one b) e
    BasicOp (Index {})       -> graphRead      (one b) e
    BasicOp (UpdateAcc {})   -> graphUpdateAcc (one b) e
    BasicOp _                -> graphSimple  b e
    Apply n _ _ _            -> graphApply n b e
    If cond tbody fbody _    -> graphIf b cond tbody fbody
    DoLoop params lform body -> graphLoop b params lform body
    WithAcc inputs f         -> graphWithAcc b inputs f
    Op _                     -> graphHostOnly e
  where
    one [x] = x
    one _   = error "type error: unexpected number of pattern elements"

-- The vertex handle for a variable and its type.
type Binding = (Id, Type)

-- Bindings for all pattern elements bound by a statement.
boundBy :: Stm GPU -> [Binding]
boundBy (Let (Pat pes) _ _) = map f pes
  where
    f (PatElem v dec) = (nameToId v, typeOf dec)

graphHostOnly :: Exp GPU -> Grapher ()
graphHostOnly e = do
  -- Connect the vertices of all operands to sinks to mark that they are
  -- required on host. Transitive reads that they depend upon can be delayed
  -- no further.
  ops <- graphedScalarOperands e
  addEdges ToSink ops
  tell $ mempty { bodyHostOnly = True }

graphSimple :: [Binding] -> Exp GPU -> Grapher ()
graphSimple bs e = do
  -- Only add vertices to the graph if they have a transitive dependency to
  -- an array read. Transitive dependencies through variables required on host
  -- (those that are connected to sinks) do not count.
  ops <- graphedScalarOperands e
  if IS.null ops then return ()
  else do
    mapM_ addVertex bs
    addEdges (MG.declareEdges $ fst $ unzip bs) ops

graphAssert :: Binding -> Exp GPU -> Grapher ()
graphAssert b e = do
  -- In the worst case, each assertion moved to device costs an extra read
  -- during next (read) synchronization. To ensure that the number of reads
  -- does not increase by moving assertions, each assertion must in itself
  -- delay one read. By connecting assertion certificates to sinks, a set of
  -- assertions will only be moved to device if they delay a greater number
  -- of reads.
  graphSimple [b] e
  modifyGraph (MG.connectToSink $ fst b)

graphRead :: Binding -> Exp GPU -> Grapher ()
graphRead b e = do
  ops <- graphedScalarOperands e
  addSource b
  addEdges (oneEdge $ fst b) ops
  tell $ mempty { bodyReads = True }

graphUpdateAcc :: Binding -> Exp GPU -> Grapher ()
graphUpdateAcc b e | (_, Acc a _ _ _) <- b = do
  let ai = nameToId a
  act <- getAction ai
  if act == HostOp then graphHostOnly e else do
    ops <- graphedScalarOperands e
    addVertex b
    -- The vertex that represents the accumulator operator is also connected to
    -- the UpdateAcc vertex, such that if the operator is moved to device, all
    -- usage sites will be moved too.
    addEdges (oneEdge $ fst b) (IS.insert ai ops)
graphUpdateAcc _ _ = error -- should never happen
  "type error: UpdateAcc did not produce accumulator typed value"

graphApply :: Name -> [Binding] -> Exp GPU -> Grapher ()
graphApply n b e = do
  hof <- isHostOnlyFun n
  if hof then graphHostOnly e
         else graphSimple b e

graphIf :: [Binding] -> SubExp -> BodyT GPU -> BodyT GPU -> Grapher ()
graphIf bs cond tbody fbody = do
  ho <- incForkDepthFor (do {
          tstats <- graphBody tbody ;
          fstats <- graphBody fbody ;
          return $ (bodyHostOnly tstats) || (bodyHostOnly fstats)
        })
  ci <- case (ho, cond) of
          (True, Var v)  -> do { requiredOnHost (nameToId v); return IS.empty }
          (False, Var v) -> onlyGraphedScalars [v]
          (_, _)         -> return IS.empty
  ret <- zipWithM (f ci) (bodyResult tbody) (bodyResult fbody)
  mapM_ (uncurry createNode) $ zip bs ret
  where
    f ci a b = fmap (ci <>) $ onlyGraphedScalars $ (toSet a) <> (toSet b)

    toSet (SubExpRes _ (Var v)) = S.singleton v
    toSet _                     = S.empty

graphWithAcc :: [Binding]
             -> [(Shape, [VName], Maybe (Lambda GPU, [SubExp]))]
             -> Lambda GPU
             -> Grapher ()
graphWithAcc bs inputs f = do
  let accs = zip (lambdaReturnType f) inputs
  -- Graph each accumulator operator. UpdateAcc statements will be graphed
  -- based on which statements their associated accumulator operators
  -- evaluate, which is captured as 'GraphAction's. The actions are made
  -- available until the associated accumulators no longer are in scope.
  actions <- fmap IM.fromList $ mapM (graph . extract) accs
  let body = lambdaBody f
  (graphStms $ bodyStms body) `withActions` actions
  -- Connect return variables to bound values.
  ret <- mapM (onlyGraphedScalars . toSet) (bodyResult body)
  mapM_ (uncurry createNode) $ zip bs ret
  where
    graph (i, ts, op) = do { a <- graphAccOp i ts op; return (i, a) }

    extract (Acc a _ ts _, (_, _, Nothing))      = (nameToId a, ts, Nothing)
    extract (Acc a _ ts _, (_, _, Just (op, _))) = (nameToId a, ts, Just op)
    extract _ = (0, [], Nothing) -- should never happen

    toSet (SubExpRes _ (Var v)) = S.singleton v
    toSet _                     = S.empty

-- Graphs the operator associated with updating an accumulator.
-- The arguments are the 'Id' for the accumulator token, the element type of
-- the accumulator, and its update operator lambda, if any.
graphAccOp :: Id -> [Type] -> Maybe (Lambda GPU) -> Grapher GraphAction
graphAccOp i ts op = do
  env <- ask
  st  <- get

  -- Graph the accumulator operator to collect statistics about its statements.
  -- If the operator contains no host-only statements then the graphing will be
  -- reversed. Otherwise no reads should be delayed into it, as it may be
  -- evaluated multiple times.
  let env' = env { envMeta = (envMeta env) { metaGraphId = Just i } }
  let (st', bs) = case op of
                    Nothing  -> (st, mempty)
                    Just lmd -> execRWS (graphBody $ lambdaBody lmd) env' st

  -- The operator will be invoked by 'UpdateAcc' statements. Determine how these
  -- statements should be graphed based on collected statistics.
  let act  = if any isScalarType ts then DeviceOp else NeutralOp
  let act' = act <> case bs of
                      BodyStats hostOnly _ _ | hostOnly -> HostOp
                      BodyStats _ didReads _ | didReads -> DeviceOp
                      _                                 -> NeutralOp

  -- Determine which external variables the operator depends upon.
  let g   = stateGraph st
  let ops = IS.filter (\oi -> MG.member oi g) (bodyOperands bs)

  -- Finish graphing the operator based on the determined action type.
  case act' of
    HostOp    -> do { put st'; tell bs;
                      _ <- censorRoutes i IS.empty ops; return () }
    DeviceOp  -> addSource (i, Prim Unit) -- no sink possible ==> no edges
    NeutralOp -> do { addVertex (i, Prim Unit); addEdges (oneEdge i) ops }

  -- Report how 'UpdateAcc' statements that use this operator should be graphed.
  return act'

graphLoop :: [Binding]
          -> [(FParam GPU, SubExp)]
          -> LoopForm GPU
          -> BodyT GPU
          -> Grapher ()
graphLoop b params lform body =
  return () -- TODO





-- | Ids for all variables used as an operand.
type Operands = IdSet

-- | A reach map maps a vertex to all vertices within some set that it can
-- reach within a graph, provided it cannot reach a sink.
type ReachMap = IM.IntMap Reaches

-- | All vertices of interest that can be reached from some vertex.
data Reaches
  -- | A sink can be reached.
  = Sink
  -- | These vertices can be reached.
  | Vars IdSet
  -- | The vertices that can be reached are those that this other vertex
  -- can reach.
  | Ref Id Int

-- | Can a sink be reached?
isSink :: Reaches -> Bool
isSink Sink = True
isSink _    = False

instance Semigroup Reaches where
  Sink <> _ = Sink
  _ <> Sink = Sink
  (Ref r1 d1) <> (Ref r2 d2)
    | d1 < d2   = Ref r1 d1
    | otherwise = Ref r2 d2
  (Ref r d) <> _ = Ref r d
  _ <> (Ref r d) = Ref r d
  (Vars a) <> (Vars b) = Vars (IS.union a b)

instance Monoid Reaches where
  mempty = Vars IS.empty

-- | @censorRoutes si terms ops@ routes all possible routes within the subgraph
-- identified by @si@ and then connects all operands in @ops@ to sinks if they
-- can reach a subgraph sink. Returns a reach map for the operands to the
-- variables in @terms@.
--
-- Assumption: The subgraph with the given id has just been created and no path
-- exists from it to an external sink.
censorRoutes :: Id -> IdSet -> Operands -> Grapher ReachMap
censorRoutes si terms ops = do
  routeSubgraph si
  -- Connect operands to sinks if they can reach a sink within the subgraph.
  -- This ensures additional reads cannot occur by delaying any reads into it.
  g <- getGraph
  let reachMap = IS.foldl' (reachMapper g) IM.empty ops
  let canReachSink = \i -> isSink (reachMap IM.! i)
  mapM_ (\i -> when (canReachSink i) $ requiredOnHost i) (IS.elems ops)
  return reachMap
  where
    reachMapper g rm i
      | Just v  <- MG.get i g
      = fst $ execRWS (traverseVertex 0 g Normal v) () rm

      | otherwise
      = rm -- shouldn't happen

    traverseEdges d g et v =
      MG.searchEdges (traverser d g) et v g

    traverser d g from v
      | gi <- metaGraphId $ vertexMeta v
      , gi /= (Just si)
      = continue -- only traverse subgraph

      | otherwise = do
        let et = maybe Normal snd from
        rm <- get
        case IM.lookup (vertexId v) rm of
          Just rs -> reaches rs
          Nothing -> traverseVertex d g et v
    
    -- Output is:
    -- 1) Current Reaches value, at most one indirection if Ref.
    -- 2) The union of all variables of interest reached so far.
    -- 3) Cycle members.
    reaches rs
      | Ref {} <- rs
      = do { tell (rs, mempty, IS.empty); continue }

      | otherwise
      = do { tell (acyclic rs); continue }

    acyclic rs = (rs, rs, IS.empty)

    traverseVertex d g et v = do
      let i = vertexId v
      rm <- get
      -- The vertex might be the start of a cycle. To handle we prematurely set
      -- its Reaches value to be a self-reference. Before returning from
      -- traverseVertex the value must be updated to either be a non-Ref Reaches
      -- value or a reference to a vertex further up the search tree.
      let rm1       = IM.insert i (Ref i d) rm
      let traversal = (sinkCheck v >> termCheck i >> traverseEdges (d+1) g et v)
      let (rm2, w)  = execRWS traversal () rm1
      let (rm', w') = handleCycle i rm2 w
      let (val, _, _) = w'
      put $ IM.insert i val rm' -- replace self-reference
      tell w'
      continue

    sinkCheck v
      | NoRoute <- vertexRouting v
      , ToSink <- vertexEdges v
      = reaches Sink

      | otherwise
      = continue

    termCheck i
      | IS.member i terms
      = reaches (Vars $ IS.singleton i)

      | otherwise
      = continue

    handleCycle i rm w@(val, rs, vs) =
      let rm' = IS.foldr' (\oi m -> IM.insert oi rs m) rm vs
          w'  = (val, rs, IS.insert i vs)
      in case val of
          Ref ref _
            | ref == i  -> (rm', acyclic rs) -- Exiting cycle
            | otherwise -> (rm,  w')         -- Inside cycle
          _             -> (rm,  w)          -- Outside cycle

-- | Routes all possible routes within the subgraph identified by this id.
--
-- Assumption: The subgraph with the given id has just been created and no path
-- exists from it to an external sink.
routeSubgraph :: Id -> Grapher ()
routeSubgraph si = do
  st <- get
  let g      = stateGraph st
  let (r, u) = stateSources st
  let (ss, u') = span (inSubGraph si g) u
  let (sinks, g') = MG.routeMany ss g
  put $ st { stateGraph   = g',
             stateSources = (ss ++ r, u'),
             stateSinks   = sinks ++ (stateSinks st) }

-- | @inSubGraph si g i@ returns whether @g@ contains a vertex with id @i@ and
-- subgraph id @si@.
inSubGraph :: Id -> Graph -> Id -> Bool
inSubGraph si g i
  | Just v   <- MG.get i g
  , Just mgi <- metaGraphId (vertexMeta v)
  = si == mgi
inSubGraph _ _ _
  = False

-- | Creates a vertex for the given binding, provided that the set of operands
-- is not empty.
createNode :: Binding -> Operands -> Grapher ()
createNode b ops =
  if IS.null ops then return ()
  else do
    addVertex b
    addEdges (oneEdge $ fst b) ops

-- | Adds a vertex to the graph for the given binding.
addVertex :: Binding -> Grapher ()
addVertex (i, t) = do
  meta <- getMeta
  let v = MG.vertex i meta
  when (isScalarType t) $ modifyGraphedScalars (IS.insert i)
  modifyGraph (MG.insert v)

-- | Adds a source connected vertex to the graph for the given binding.
addSource :: Binding -> Grapher ()
addSource b = do
  addVertex b
  modifySources $ \(r, u) -> (r, (fst b):u)

-- | Adds the given edges to each vertex identified by the 'IdSet'. It is
-- assumed that all vertices reside within the body that currently is being
-- graphed.
addEdges :: Edges -> IdSet -> Grapher ()
addEdges ToSink is = do
  modifyGraph $ \g -> IS.foldl' (flip $ MG.connectToSink) g is
  modifyGraphedScalars (\s -> s `IS.difference` is)
addEdges es is = do
  modifyGraph $ \g -> IS.foldl' (flip $ MG.addEdges es) g is
  tell $ mempty { bodyOperands = is }

-- | Connects the vertex of the given id to a sink.
requiredOnHost :: Id -> Grapher ()
requiredOnHost i = do
  modifyGraph (MG.connectToSink i)
  modifyGraphedScalars (IS.delete i)

-- Reduces the variables to just the 'Id's of those that are scalars and which
-- have a vertex representation in the graph, excluding those that have been
-- connected to sinks.
onlyGraphedScalars :: Foldable t => t VName -> Grapher IdSet
onlyGraphedScalars vs = do
  let is = foldl' (\s v -> IS.insert (nameToId v) s) IS.empty vs
  gss <- getGraphedScalars
  return (IS.intersection is gss)

-- Returns all non-kernel scalar operands that have a vertex representation in
-- the graph, excluding those that have been connected to sinks. That is all
-- operands produced by statements that can be moved to device to potentially
-- avoid a read.
graphedScalarOperands :: Exp GPU -> Grapher IdSet
graphedScalarOperands e = onlyGraphedScalars $ fst (collect e)
  where
    none = (S.empty, S.empty) -- scalar operands, accumulator tokens

    captureAcc a = (S.empty, S.singleton a)

    operands vs = (S.fromList vs, S.empty)

    collectSE (Var v) = (S.singleton v, S.empty)
    collectSE _       = none

    -- Note: Plain VName values only refer to arrays.
    collectBasic = W.execWriter . walkExpM subExpWalker
    subExpWalker = identityWalker { walkOnSubExp = W.tell . collectSE }

    collectBody = collectStms . bodyStms
    collectStms = foldMap collectStm

    -- Capture the tokens of accumulators used on host.
    collectStm (Let pat _ ua)
      | BasicOp (UpdateAcc {}) <- ua
      , Pat [pe] <- pat
      , Acc a _ _ _ <- typeOf pe
      = captureAcc a <> collectBasic ua
    collectStm stm = collect (stmExp stm)

    collectLForm (ForLoop _ _ b _) = collectSE b
    collectLForm (WhileLoop _)     = none

    collect b@(BasicOp {})   = collectBasic b
    collect (Apply _ ps _ _) = foldMap (collectSE . fst) ps
    collect (If c t f _)     = collectSE c <> collectBody t <> collectBody f
    collect (DoLoop ps lf b) = foldMap (collectSE . snd) ps <>
                               collectLForm lf <> collectBody b
    collect (WithAcc accs f) = collectWithAcc accs f
    collect (Op op)          = collectHostOp op

    -- Neutral elements of accumulator operators are always used on host but the
    -- collective operands of an operator lambda body are only used on host if
    -- the associated accumulator is used in an UpdateAcc statement outside a
    -- kernel.
    collectWithAcc inputs f =
      let bops = collectBody (lambdaBody f)
          accs = take (length inputs) (lambdaReturnType f)
          used = (flip map) accs $ \(Acc a _ _ _) -> S.member a (snd bops)
      in bops <> foldMap collectAcc (zip used inputs)
    
    collectAcc (_,    (_, _, Nothing))        = none
    collectAcc (used, (_, _, Just (op, nes))) = foldMap collectSE nes <>
                                                if not used then none
                                                else collectBody (lambdaBody op)

    -- SegLevel contains just tunable runtime constants, which are host-only.
    -- SegSpace and Types only refers to array sizes, which always reside o
    -- host. Kernel bodies are explicitly skipped as all those occur on device.
    collectHostOp (SegOp (SegRed _ _ ops _ _))  = foldMap collectSegBinOp ops
    collectHostOp (SegOp (SegScan _ _ ops _ _)) = foldMap collectSegBinOp ops
    collectHostOp (SegOp (SegHist _ _ ops _ _)) = foldMap collectHistOp ops
    collectHostOp (SegOp (SegMap {}))           = none
    collectHostOp (GPUBody _ _)                 = none
    collectHostOp op = operands $ IM.elems $ namesIntMap $ freeIn op

    collectSegBinOp (SegBinOp _ _ nes _)  = foldMap collectSE nes
    collectHistOp (HistOp _ rf _ nes _ _) = collectSE rf <> -- ?
                                            foldMap collectSE nes