module Futhark.Analysis.MigrationTable
  ( -- * Analysis
    analyseProg,

    -- * Query
    MigrationTable,
    MigrationStatus (..),
    statusOf,
    moveToDevice,
    shouldMove,
    usedOnHost,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Trans.State.Strict ()
import Control.Monad.Trans.State.Strict hiding (State)
import Control.Parallel.Strategies (parMap, rpar)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (find, foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import qualified Data.Sequence as SQ
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Futhark.Analysis.MigrationTable.Graph hiding
  ( Graph,
    addEdges,
    connectToSink,
    get,
    none,
  )
import qualified Futhark.Analysis.MigrationTable.Graph as MG
import Futhark.Error
import Futhark.IR.GPU

{- HLINT ignore "Use first" -}
{- HLINT ignore "Use second" -}

-- | Where the value bound by a name should be computed.
data MigrationStatus
  = -- | The statement that computes the value should be moved to device.
    -- No host usage of the value will be left after the migration.
    MoveToDevice
  | -- | As MoveToDevice but host usage of the value will remain after
    -- migration.
    UsedOnHost
  | -- | The statement that computes the value should remain on host.
    StayOnHost
  deriving (Eq, Ord, Show)

-- | Identifies
--
--     (1) which statements should be moved from host to device to reduce the
--         the worst case number of blocking memory transfers, primarily
--         device-host scalar reads.
--
--     (2) which migrated variables that still will be used on the host after
--         all such statements have been moved.
newtype MigrationTable = MigrationTable (IM.IntMap MigrationStatus)

-- | Where should the value bound by this name be computed?
statusOf :: VName -> MigrationTable -> MigrationStatus
statusOf n (MigrationTable mt) =
  fromMaybe StayOnHost $ IM.lookup (baseTag n) mt

-- | Should this whole statement be moved from host to device?
moveToDevice :: Stm GPU -> MigrationTable -> Bool
moveToDevice (Let (Pat ((PatElem n _) : _)) _ (BasicOp (Index _ slice))) mt =
  statusOf n mt == MoveToDevice || any movedOperand slice
  where
    movedOperand (Var op) = statusOf op mt == MoveToDevice
    movedOperand _ = False
moveToDevice (Let (Pat ((PatElem n _) : _)) _ (BasicOp _)) mt =
  statusOf n mt /= StayOnHost
moveToDevice (Let (Pat ((PatElem n _) : _)) _ Apply {}) mt =
  statusOf n mt /= StayOnHost
moveToDevice (Let _ _ (If (Var n) _ _ _)) mt =
  statusOf n mt == MoveToDevice
moveToDevice (Let _ _ (DoLoop _ (ForLoop _ _ (Var n) _) _)) mt =
  statusOf n mt == MoveToDevice
moveToDevice (Let _ _ (DoLoop _ (WhileLoop n) _)) mt =
  statusOf n mt == MoveToDevice
-- BasicOp and Apply statements might not bind any variables (shouldn't happen).
-- If statements might use a constant branch condition.
-- For loop statements might use a constant number of iterations.
-- HostOp statements cannot execute on device.
-- WithAcc statements are never moved in their entirety.
moveToDevice _ _ = False

-- | Should the value bound by this name be computed on device?
shouldMove :: VName -> MigrationTable -> Bool
shouldMove n mt = statusOf n mt /= StayOnHost

-- | Is the value bound by this name used on host?
usedOnHost :: VName -> MigrationTable -> Bool
usedOnHost n mt = statusOf n mt /= MoveToDevice

-- | Merges two migration tables that are assumed to be disjoint.
merge :: MigrationTable -> MigrationTable -> MigrationTable
merge (MigrationTable a) (MigrationTable b) = MigrationTable (a `IM.union` b)

-- | Analyses a program to return a migration table that covers all its
-- statements and variables.
analyseProg :: Prog GPU -> MigrationTable
analyseProg (Prog consts funs) =
  let hof = hostOnlyFunDefs funs
      mt = analyseConsts hof consts
      mts = parMap rpar (analyseFunDef hof) funs
   in foldl' merge mt mts

-- | Returns the names of all top-level functions that cannot be called from the
-- device. The evaluation of such a function is host-only.
hostOnlyFunDefs :: [FunDef GPU] -> HostOnlyFuns
hostOnlyFunDefs funs =
  let names = map funDefName funs
      call_map = M.fromList $ zip names (map checkFunDef funs)
   in S.fromList names \\ keysToSet (removeHostOnly call_map)
  where
    keysToSet = S.fromAscList . M.keys

    removeHostOnly cm =
      let (host_only, cm') = M.partition isHostOnly cm
       in if M.null host_only
            then cm'
            else removeHostOnly $ M.map (checkCalls $ keysToSet host_only) cm'

    isHostOnly = isNothing

    -- A function that calls a host-only function is itself host-only.
    checkCalls hostOnlyFuns (Just calls)
      | hostOnlyFuns `S.disjoint` calls =
        Just calls
    checkCalls _ _ =
      Nothing

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
    ok = Just ()
    check isArr as = if any isArr as then hostOnly else ok
    isArray = not . primType

    checkFParams ps = check (isArray . typeOf) ps

    checkLParams ps = check (isArray . typeOf . fst) ps

    checkRetTypes rs = check isArray rs

    checkPats pats = check (isArray . typeOf) pats

    checkLoopForm (ForLoop _ _ _ (_ : _)) = hostOnly
    checkLoopForm _ = ok

    checkBody = checkStms . bodyStms

    checkStms stms = S.unions <$> mapM checkStm stms

    checkStm (Let (Pat pats) _ e) = checkPats pats >> checkExp e

    -- Any expression that produces an array is caught by checkPats
    checkExp (BasicOp (Index _ _)) = hostOnly
    checkExp (WithAcc _ _) = hostOnly
    checkExp (Op _) = hostOnly
    checkExp (Apply fn _ _ _) = Just (S.singleton fn)
    checkExp (If _ tbranch fbranch _) = do
      calls1 <- checkBody tbranch
      calls2 <- checkBody fbranch
      pure (calls1 <> calls2)
    checkExp (DoLoop params lform body) = do
      checkLParams params
      checkLoopForm lform
      checkBody body
    checkExp _ = Just S.empty

-- | Analyses top-level constants.
analyseConsts :: HostOnlyFuns -> Stms GPU -> MigrationTable
analyseConsts hof consts =
  let usage = M.foldlWithKey f [] (scopeOf consts)
   in analyseStms hof usage consts
  where
    f usage n t | isScalar t = nameToId n : usage
    f usage _ _ = usage

-- | Analyses a top-level function definition.
analyseFunDef :: HostOnlyFuns -> FunDef GPU -> MigrationTable
analyseFunDef hof fd =
  let body = funDefBody fd
      usage = foldl' f [] $ zip (bodyResult body) (funDefRetType fd)
      stms = bodyStms body
   in analyseStms hof usage stms
  where
    f usage (SubExpRes _ (Var n), t) | isScalarType t = nameToId n : usage
    f usage _ = usage

isScalar :: Typed t => t -> Bool
isScalar = isScalarType . typeOf

isScalarType :: TypeBase shape u -> Bool
isScalarType (Prim Unit) = False
isScalarType (Prim _) = True
isScalarType _ = False

-- | HostUsage identifies scalar variables that are used on host.
type HostUsage = [Id]

-- | Analyses statements. The 'HostUsage' list identifies which bound scalar
-- variables that subsequently may be used on host. All free variables such as
-- constants and function parameters are assumed to reside on host.
analyseStms :: HostOnlyFuns -> HostUsage -> Stms GPU -> MigrationTable
analyseStms hof usage stms =
  let (g, srcs, _) = buildGraph hof usage stms
      (routed, unrouted) = srcs
      (_, g') = MG.routeMany unrouted g -- hereby routed
      f st' = MG.fold g' visit st' Normal
      st = foldl' f (initial, MG.none) unrouted
      (vr, vn, tn) = fst $ foldl' f st routed
   in -- TODO: Delay reads into (deeper) branches

      MigrationTable $
        IM.unions
          [ IM.fromSet (const MoveToDevice) vr,
            IM.fromSet (const MoveToDevice) vn,
            -- Read by host if not reached by a reversed edge
            IM.fromSet (const UsedOnHost) tn
          ]
  where
    -- 1) Visited by reversed edge.
    -- 2) Visited by normal edge, no route.
    -- 3) Visited by normal edge, had route; will potentially be read by host.
    initial = (IS.empty, IS.empty, IS.empty)

    visit (vr, vn, tn) Reversed v =
      let vr' = IS.insert (vertexId v) vr
       in (vr', vn, tn)
    visit (vr, vn, tn) Normal v@Vertex {vertexRouting = NoRoute} =
      let vn' = IS.insert (vertexId v) vn
       in (vr, vn', tn)
    visit (vr, vn, tn) Normal v =
      let tn' = IS.insert (vertexId v) tn
       in (vr, vn, tn')

buildGraph :: HostOnlyFuns -> HostUsage -> Stms GPU -> (Graph, Sources, Sinks)
buildGraph hof usage stms =
  let (g, srcs, sinks) = execGrapher hof (graphStms stms)
      g' = foldl' (flip MG.connectToSink) g usage
   in (g', srcs, sinks)

type Grapher = StateT State (R.Reader Env)

data Env = Env
  { -- | See 'HostOnlyFuns'.
    envHostOnlyFuns :: HostOnlyFuns,
    -- | Metadata for the current body being graphed.
    envMeta :: Meta
  }

-- | Identifies top-level function definitions that cannot be run on the
-- device. The application of any such function is host-only.
type HostOnlyFuns = Set Name

-- | Metadata on the environment that a variable is declared within.
data Meta = Meta
  { -- | How many if statement branch bodies the variable binding is nested
    -- within. If a route passes through the edge u->v and the fork depth
    --
    --   1) increases from u to v, then u is within a conditional branch.
    --
    --   2) decreases from u to v, then v binds the result of two or more
    --      branches.
    --
    -- After the graph has been built and routed, this can be used to delay
    -- reads into deeper branches to reduce their likelihood of manifesting.
    metaForkDepth :: Int,
    -- | How many bodies the variable is nested within.
    metaBodyDepth :: Int,
    -- | An id for the subgraph within which the variable exists, defined at
    -- the body level. A read may only be delayed to a point within its own
    -- subgraph.
    metaGraphId :: Maybe Id
  }

-- | Ids for all variables used as an operand.
type Operands = IdSet

-- | Statistics on the statements within a body and their dependencies.
data BodyStats = BodyStats
  { -- | Whether the body contained any host-only statements.
    bodyHostOnly :: Bool,
    -- | Whether the body performed any reads.
    bodyReads :: Bool,
    -- | All scalar variables represented in the graph that have been used
    -- as operands within the body, including those that are defined within
    -- the body itself. Variables with vertices connected to sinks may be
    -- excluded.
    bodyOperands :: Operands,
    -- | Depth of parent bodies with variables that are required on host. Since
    -- the variables are required on host, the parent statements of these bodies
    -- cannot be moved to device as a whole. They are host-only.
    bodyHostOnlyParents :: IS.IntSet
  }

instance Semigroup BodyStats where
  (BodyStats ho1 r1 o1 hop1) <> (BodyStats ho2 r2 o2 hop2) =
    BodyStats
      { bodyHostOnly = ho1 || ho2,
        bodyReads = r1 || r2,
        bodyOperands = IS.union o1 o2,
        bodyHostOnlyParents = IS.union hop1 hop2
      }

instance Monoid BodyStats where
  mempty =
    BodyStats
      { bodyHostOnly = False,
        bodyReads = False,
        bodyOperands = IS.empty,
        bodyHostOnlyParents = IS.empty
      }

type Graph = MG.Graph Meta

-- | All vertices connected from a source, partitioned into those that have
-- been attempted routed and those which have not.
type Sources = ([Id], [Id])

-- | All terminal vertices of routes.
type Sinks = [Id]

-- | A captured statement for which graphing has been delayed.
type Delayed = (Binding, Exp GPU)

-- | The vertex handle for a variable and its type.
type Binding = (Id, Type)

data State = State
  { -- | The graph being built.
    stateGraph :: Graph,
    -- | All known scalars that have been graphed.
    stateGraphedScalars :: IdSet,
    -- | All variables that directly bind scalars read from device memory.
    stateSources :: Sources,
    -- | Graphed scalars that are used as operands by statements that cannot be
    -- migrated. A read cannot be delayed beyond these, so if the statements
    -- that bind these variables are moved to device, the variables must be read
    -- from device memory.
    stateSinks :: Sinks,
    -- | Observed 'UpdateAcc' host statements to be graphed later.
    stateUpdateAccs :: IM.IntMap [Delayed],
    -- | Information about the current body being graphed.
    stateStats :: BodyStats
  }

execGrapher :: HostOnlyFuns -> Grapher a -> (Graph, Sources, Sinks)
execGrapher hof m =
  let s = R.runReader (execStateT m st) env
   in (stateGraph s, stateSources s, stateSinks s)
  where
    env =
      Env
        { envHostOnlyFuns = hof,
          envMeta =
            Meta
              { metaForkDepth = 0,
                metaBodyDepth = 0,
                metaGraphId = Nothing
              }
        }
    st =
      State
        { stateGraph = MG.empty,
          stateGraphedScalars = IS.empty,
          stateSources = ([], []),
          stateSinks = [],
          stateUpdateAccs = IM.empty,
          stateStats = mempty
        }

-- | Execute a computation in a modified environment.
local :: (Env -> Env) -> Grapher a -> Grapher a
local f = mapStateT (R.local f)

-- | Fetch the value of the environment.
ask :: Grapher Env
ask = lift R.ask

-- | Retrieve a function of the current environment.
asks :: (Env -> a) -> Grapher a
asks = lift . R.asks

-- | Register that the body contains a host-only statement. This means its
-- parent statement and any parent bodies themselves are host-only.
tellHostOnly :: Grapher ()
tellHostOnly =
  modify $ \st -> st {stateStats = (stateStats st) {bodyHostOnly = True}}

-- | Register that the current body contains a statement that reads device
-- memory.
tellRead :: Grapher ()
tellRead =
  modify $ \st -> st {stateStats = (stateStats st) {bodyReads = True}}

-- | Register that these variables are used as operands within the current body.
tellOperands :: IdSet -> Grapher ()
tellOperands is =
  modify $ \st ->
    let stats = stateStats st
        operands = bodyOperands stats
     in st {stateStats = stats {bodyOperands = operands <> is}}

-- | Register that the current statement with a body at the given body depth is
-- host-only.
tellHostOnlyParent :: Int -> Grapher ()
tellHostOnlyParent bodyDepth =
  modify $ \st ->
    let stats = stateStats st
        parents = bodyHostOnlyParents stats
        parents' = IS.insert bodyDepth parents
     in st {stateStats = stats {bodyHostOnlyParents = parents'}}

-- | Get the graph under construction.
getGraph :: Grapher Graph
getGraph = gets stateGraph

-- | All scalar variables with a vertex representation in the graph.
getGraphedScalars :: Grapher IdSet
getGraphedScalars = gets stateGraphedScalars

-- | Reduces the variables to just the 'Id's of those that are scalars and which
-- have a vertex representation in the graph, excluding those that have been
-- connected to sinks.
onlyGraphedScalars :: Foldable t => t VName -> Grapher IdSet
onlyGraphedScalars vs = do
  let is = foldl' (\s n -> IS.insert (nameToId n) s) IS.empty vs
  IS.intersection is <$> getGraphedScalars

-- | Like 'onlyGraphedScalars' but for a single 'VName'.
onlyGraphedScalar :: VName -> Grapher IdSet
onlyGraphedScalar n = do
  let i = nameToId n
  gss <- getGraphedScalars
  if IS.member i gss
    then pure (IS.singleton i)
    else pure IS.empty

-- | Like 'onlyGraphedScalars' but for a single 'SubExp'.
onlyGraphedScalarSubExp :: SubExp -> Grapher IdSet
onlyGraphedScalarSubExp (Constant _) = pure IS.empty
onlyGraphedScalarSubExp (Var n) = onlyGraphedScalar n

-- | Update the graph under construction.
modifyGraph :: (Graph -> Graph) -> Grapher ()
modifyGraph f =
  modify $ \st -> st {stateGraph = f (stateGraph st)}

-- | Update the contents of the graphed scalar set.
modifyGraphedScalars :: (IdSet -> IdSet) -> Grapher ()
modifyGraphedScalars f =
  modify $ \st -> st {stateGraphedScalars = f (stateGraphedScalars st)}

-- | Update the set of source connected vertices.
modifySources :: (Sources -> Sources) -> Grapher ()
modifySources f =
  modify $ \st -> st {stateSources = f (stateSources st)}

-- | Increment the fork depth for variables graphed by this action.
incForkDepthFor :: Grapher a -> Grapher a
incForkDepthFor =
  local $ \env ->
    let meta = envMeta env
        fork_depth = metaForkDepth meta
     in env {envMeta = meta {metaForkDepth = fork_depth + 1}}

-- | Increment the body depth for variables graphed by this action.
incBodyDepthFor :: Grapher a -> Grapher a
incBodyDepthFor =
  local $ \env ->
    let meta = envMeta env
        body_depth = metaBodyDepth meta
     in env {envMeta = meta {metaBodyDepth = body_depth + 1}}

-- | Change the graph id for variables graphed by this action.
graphIdFor :: Id -> Grapher a -> Grapher a
graphIdFor i =
  local $ \env ->
    let meta = envMeta env
     in env {envMeta = meta {metaGraphId = Just i}}

-- | Capture body stats produced by the given action.
captureBodyStats :: Grapher a -> Grapher BodyStats
captureBodyStats m = do
  stats <- gets stateStats
  modify $ \st -> st {stateStats = mempty}

  _ <- m

  stats' <- gets stateStats
  modify $ \st -> st {stateStats = stats <> stats'}

  pure stats'

-- | Can applications of this function be moved to device?
isHostOnlyFun :: Name -> Grapher Bool
isHostOnlyFun fn = asks $ S.member fn . envHostOnlyFuns

-- | Get the 'Meta' corresponding to the current body.
getMeta :: Grapher Meta
getMeta = asks envMeta

-- | Get the body depth of the current body (its nesting level).
getBodyDepth :: Grapher Int
getBodyDepth = asks (metaBodyDepth . envMeta)

-- | Creates a vertex for the given binding, provided that the set of operands
-- is not empty.
createNode :: Binding -> Operands -> Grapher ()
createNode b ops =
  unless (IS.null ops) (addVertex b >> addEdges (oneEdge $ fst b) ops)

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
  modifySources $ \(routed, unrouted) -> (routed, fst b : unrouted)

-- | Adds the given edges to each vertex identified by the 'IdSet'. It is
-- assumed that all vertices reside within the body that currently is being
-- graphed.
addEdges :: Edges -> IdSet -> Grapher ()
addEdges ToSink is = do
  modifyGraph $ \g -> IS.foldl' (flip MG.connectToSink) g is
  modifyGraphedScalars (`IS.difference` is)
addEdges es is = do
  modifyGraph $ \g -> IS.foldl' (flip $ MG.addEdges es) g is
  tellOperands is

-- | Ensure that a variable (which is in scope) will be made available on host
-- before its first use.
requiredOnHost :: Id -> Grapher ()
requiredOnHost i = do
  mv <- MG.get i <$> getGraph
  case mv of
    Nothing -> pure ()
    Just v -> do
      connectToSink i
      tellHostOnlyParent (metaBodyDepth $ vertexMeta v)

-- | Connects the vertex of the given id to a sink.
connectToSink :: Id -> Grapher ()
connectToSink i = do
  modifyGraph (MG.connectToSink i)
  modifyGraphedScalars (IS.delete i)

-- | Like 'connectToSink' but vertex is given by a 'SubExp'. This is a no-op if
-- the 'SubExp' is a constant.
connectSubExpToSink :: SubExp -> Grapher ()
connectSubExpToSink (Var n) = connectToSink (nameToId n)
connectSubExpToSink _ = pure ()

-- | Routes all possible routes within the subgraph identified by this id.
-- Returns the ids of the source connected vertices that were attempted routed.
--
-- Assumption: The subgraph with the given id has just been created and no path
-- exists from it to an external sink.
routeSubgraph :: Id -> Grapher [Id]
routeSubgraph si = do
  st <- get
  let g = stateGraph st
  let (routed, unrouted) = stateSources st
  let (gsrcs, unrouted') = span (inSubGraph si g) unrouted
  let (sinks, g') = MG.routeMany gsrcs g
  put $
    st
      { stateGraph = g',
        stateSources = (gsrcs ++ routed, unrouted'),
        stateSinks = sinks ++ stateSinks st
      }
  pure gsrcs

-- | @inSubGraph si g i@ returns whether @g@ contains a vertex with id @i@ that
-- is declared within the subgraph with id @si@.
inSubGraph :: Id -> Graph -> Id -> Bool
inSubGraph si g i
  | Just v <- MG.get i g,
    Just mgi <- metaGraphId (vertexMeta v) =
    si == mgi
inSubGraph _ _ _ = False

-- | Does the given body return an array whose computation has not been
-- migrated to device?
returnsUnmigratedArray :: [Type] -> BodyT GPU -> Grapher Bool
returnsUnmigratedArray types body =
  -- TODO: Migration status is lost through aliasing, e.g. through body returns.
  checkArrays (zip types $ bodyResult body) <$> getGraph
  where
    checkArrays [] _ = False
    checkArrays ((Array {}, SubExpRes _ (Var n)) : _) g
      | not $ MG.member (nameToId n) g =
        True
    checkArrays (_ : xs) g = checkArrays xs g

graphBody :: BodyT GPU -> Grapher ()
graphBody body = do
  body_depth <- (1 +) <$> getBodyDepth
  body_stats <- captureBodyStats $ incBodyDepthFor $ graphStms (bodyStms body)

  let host_only = IS.member body_depth (bodyHostOnlyParents body_stats)
  modify $ \st ->
    let stats = stateStats st
        hops' = IS.delete body_depth (bodyHostOnlyParents stats)
        -- If body contains a variable that is required on host the parent
        -- statement that contains this body cannot be migrated as a whole.
        stats' = if host_only then stats {bodyHostOnly = True} else stats
     in st {stateStats = stats' {bodyHostOnlyParents = hops'}}

graphStms :: Stms GPU -> Grapher ()
graphStms = mapM_ graphStm

-- | Bindings for all pattern elements bound by a statement.
boundBy :: Stm GPU -> [Binding]
boundBy (Let (Pat pes) _ _) = map f pes
  where
    f (PatElem n t) = (nameToId n, t)

graphStm :: Stm GPU -> Grapher ()
graphStm stm = do
  let bs = boundBy stm
  let e = stmExp stm
  -- IMPORTANT! It is generally assumed that all scalars within types and
  -- shapes are present on host. Any expression of a type wherein one of its
  -- scalar operands appears must therefore ensure that that scalar operand is
  -- marked as a size variable (see 'hostSize' function).
  case e of
    BasicOp SubExp {} -> graphSimple bs e
    BasicOp Opaque {} -> graphSimple bs e
    BasicOp (ArrayLit [] _) -> graphSimple bs e
    BasicOp ArrayLit {} ->
      -- Migrating an array literal of n elements saves n synchronous writes.
      graphAutoMove (one bs)
    BasicOp UnOp {} -> graphSimple bs e
    BasicOp BinOp {} -> graphSimple bs e
    BasicOp CmpOp {} -> graphSimple bs e
    BasicOp ConvOp {} -> graphSimple bs e
    BasicOp Assert {} ->
      -- The next read after the execution of a kernel containing an assertion
      -- will be made asynchronous, followed by an asynchronous read to check
      -- if any assertion failed. The runtime will then block for all enqueued
      -- commands to finish.
      --
      -- Since an assertion only binds a certificate of unit type, an assertion
      -- cannot increase the number of (read) synchronizations that occur. In
      -- this regard it is free to migrate. The synchronization that does occur
      -- is however (presumably) more expensive as the pipeline of GPU work will
      -- be flushed.
      --
      -- Since this cost is difficult to quantify and amortize over assertion
      -- migration candidates (cost depends on ordering of kernels and reads) we
      -- assume it is insignificant. This will likely hold for a system where
      -- multiple threads or processes schedules GPU work, as system-wide
      -- throughput only will decrease if the GPU utilization decreases as a
      -- result.
      graphSimple bs e
    BasicOp (Index _ slice)
      | isFixed slice ->
        graphRead (one bs)
    -- Expressions with a cost sublinear to the size of their result arrays are
    -- risky to migrate as we cannot guarantee that their results are not
    -- returned from a GPUBody, which always copies its return values. Since
    -- this would make the effective asymptotical cost of such statements linear
    -- we block them from being migrated on their own. The parent statement of
    -- an enclosing body may still be migrated, including these.
    BasicOp (Index _ s) -> graphInefficientReturn (sliceDims s) e
    BasicOp Update {} ->
      -- TODO: Any update of a single scalar read from an array can be
      --       transformed into a slice update that eliminates the read.
      --       This means Updates are not sinks for scalar write values,
      --       eliminating additional reads. ReduceDeviceSyncs needs to
      --       rewrite such Updates to remain valid.
      graphInefficientReturn [] e
    BasicOp (FlatIndex _ s) -> graphInefficientReturn (flatSliceDims s) e
    BasicOp FlatUpdate {} -> graphInefficientReturn [] e
    BasicOp (Scratch _ s) -> graphInefficientReturn s e
    BasicOp (Reshape s _) -> graphInefficientReturn (newDims s) e
    BasicOp Rearrange {} -> graphInefficientReturn [] e
    BasicOp Rotate {} -> graphInefficientReturn [] e
    -- Expressions with a cost linear to the size of their result arrays are
    -- inefficient to migrate into GPUBody kernels as such kernels are single-
    -- threaded. For sufficiently large arrays the cost may exceed what is saved
    -- by avoiding reads. We therefore also block these from being migrated.
    BasicOp Concat {} -> graphHostOnly e
    BasicOp Copy {} -> graphHostOnly e
    BasicOp Manifest {} -> graphHostOnly e
    BasicOp Iota {} -> graphHostOnly e
    BasicOp Replicate {} -> graphHostOnly e
    -- END
    BasicOp UpdateAcc {} ->
      graphUpdateAcc (one bs) e
    Apply fn _ _ _ ->
      graphApply fn bs e
    If cond tbody fbody _ ->
      graphIf bs cond tbody fbody
    DoLoop params lform body ->
      graphLoop bs params lform body
    WithAcc inputs f ->
      graphWithAcc bs inputs f
    Op _ ->
      graphHostOnly e
  where
    one [x] = x
    one _ = compilerBugS "Type error: unexpected number of pattern elements."

    isFixed = isJust . sliceIndices

    -- new_dims may introduce new size variables which must be present on host
    -- when this expression is evaluated.
    graphInefficientReturn new_dims e = do
      mapM_ hostSize new_dims
      ops <- graphedScalarOperands e
      addEdges ToSink ops

    hostSize (Var n) = hostSizeVar n
    hostSize _ = pure ()

    hostSizeVar = requiredOnHost . nameToId

-- | Graph a statement which in itself neither reads scalars from device memory
-- nor forces such scalars to be available on host. Such statement can be moved
-- to device to eliminate the host usage of its operands which transitively may
-- depend on a scalar device read.
graphSimple :: [Binding] -> Exp GPU -> Grapher ()
graphSimple bs e = do
  -- Only add vertices to the graph if they have a transitive dependency to
  -- an array read. Transitive dependencies through variables connected to
  -- sinks do not count.
  ops <- graphedScalarOperands e
  let edges = MG.declareEdges (map fst bs)
  unless (IS.null ops) (mapM_ addVertex bs >> addEdges edges ops)

-- | Graph a statement that reads a scalar from device memory.
graphRead :: Binding -> Grapher ()
graphRead b = do
  -- Operands are not important as the source will block routes through b.
  addSource b
  tellRead

-- | Graph a statement that always should be moved to device.
graphAutoMove :: Binding -> Grapher ()
graphAutoMove =
  -- Operands are not important as the source will block routes through b.
  addSource

-- | Graph a statement that is unfit for execution in a GPUBody and thus must
-- be executed on host, requiring all its operands to be made available there.
graphHostOnly :: Exp GPU -> Grapher ()
graphHostOnly e = do
  -- Connect the vertices of all operands to sinks to mark that they are
  -- required on host. Transitive reads that they depend upon can be delayed
  -- no further, and any parent statements cannot be migrated.
  ops <- graphedScalarOperands e
  addEdges ToSink ops
  tellHostOnly

-- | Graph an 'UpdateAcc' statement.
graphUpdateAcc :: Binding -> Exp GPU -> Grapher ()
graphUpdateAcc b e | (_, Acc a _ _ _) <- b =
  -- The actual graphing is delayed to the corrensponding 'WithAcc' parent.
  modify $ \st ->
    let accs = stateUpdateAccs st
        accs' = IM.alter add (nameToId a) accs
     in st {stateUpdateAccs = accs'}
  where
    add Nothing = Just [(b, e)]
    add (Just xs) = Just $ (b, e) : xs
graphUpdateAcc _ _ =
  compilerBugS
    "Type error: UpdateAcc did not produce accumulator typed value."

-- | Graph a function application.
graphApply :: Name -> [Binding] -> Exp GPU -> Grapher ()
graphApply fn bs e = do
  hof <- isHostOnlyFun fn
  if hof
    then graphHostOnly e
    else graphSimple bs e

-- | Graph an if statement.
graphIf :: [Binding] -> SubExp -> BodyT GPU -> BodyT GPU -> Grapher ()
graphIf bs cond tbody fbody = do
  body_host_only <-
    incForkDepthFor
      ( do
          tstats <- captureBodyStats (graphBody tbody)
          fstats <- captureBodyStats (graphBody fbody)
          pure $ bodyHostOnly tstats || bodyHostOnly fstats
      )
  host_only <- (body_host_only ||) <$> returns_unmigrated_array

  cond_id <- case (host_only, cond) of
    (True, Var n) -> connectToSink (nameToId n) >> pure IS.empty
    (False, Var n) -> onlyGraphedScalar n
    (_, _) -> pure IS.empty

  ret <- zipWithM (comb cond_id) (bodyResult tbody) (bodyResult fbody)
  mapM_ (uncurry createNode) (zip bs ret)
  where
    returns_unmigrated_array =
      let check = returnsUnmigratedArray (map snd bs)
       in fmap (||) (check tbody) <*> check fbody

    comb ci a b = (ci <>) <$> onlyGraphedScalars (toSet a <> toSet b)

    toSet (SubExpRes _ (Var n)) = S.singleton n
    toSet _ = S.empty

-- | Graph a loop statement.
graphLoop ::
  [Binding] ->
  [(FParam GPU, SubExp)] ->
  LoopForm GPU ->
  BodyT GPU ->
  Grapher ()
graphLoop [] _ _ _ =
  -- We expect each loop to bind a value or be eliminated.
  compilerBugS "Loop statement bound no variable; should have been eliminated."
graphLoop (b : bs) params lform body = do
  let types = map snd (b : bs)

  -- Graph loop params and body while capturing statistics.
  g0 <- getGraph
  stats <- captureBodyStats (subgraphId `graphIdFor` graphTheLoop)
  let ops = IS.filter (`MG.member` g0) (bodyOperands stats)

  -- Connect loop condition to a sink if the body cannot be migrated.
  host_only <- (bodyHostOnly stats ||) <$> returnsUnmigratedArray types body
  when host_only $ case lform of
    ForLoop _ _ (Var n) _ -> connectToSink (nameToId n)
    WhileLoop n -> connectToSink (nameToId n)
    _ -> pure ()

  -- Merge return values with params. If a parameter is connected to a sink
  -- then its matching return value must also be connected to a sink to ensure
  -- that its value is available on host when the next iteration begins.
  -- Since a parameter aliases its initial value, if a parameter is connected
  -- to a sink then its initial value is guaranteed to be available on host.
  connectLoop

  -- Route the sources within the loop body in isolation.
  -- The loop graph must not be altered after this point.
  srcs <- routeSubgraph subgraphId

  -- Graph the variables bound by the statement. A device read can be delayed
  -- from one iteration to the next, so the corresponding variables bound by
  -- the statement must be treated as a sources.
  mapM_ graphBinding loopValues
  g1 <- getGraph
  let (dbs, vs) = foldl' (deviceBindings g1) (IS.empty, MG.none) srcs
  modifySources $ \(r, u) -> (r, IS.toList dbs ++ u)

  -- Connect operands to sinks if they can reach a sink within the loop.
  -- Otherwise connect them to the loop bound variables that they can
  -- reach and exhaust their normal entry edges into the loop.
  -- This means a read can be delayed through a loop body but not into it.
  --
  -- TODO: Measure whether it is better to block delays through loops.
  foldM_ connectOperand vs (IS.elems ops)

  -- It might be beneficial to move the whole loop to device, to avoid
  -- reading the (initial) loop condition value. This must be balanced
  -- against the need to read the values bound by the loop statement.
  unless host_only $ case lform of
    ForLoop _ _ n _ ->
      onlyGraphedScalarSubExp n >>= addEdges (ToNodes bindings Nothing)
    WhileLoop n
      | (_, _, pval, _) <- loopValueFor n ->
        onlyGraphedScalarSubExp pval >>= addEdges (ToNodes bindings Nothing)
  where
    subgraphId = fst b

    loopValues =
      let tmp = zip3 (b : bs) params (bodyResult body)
          tmp' = flip map tmp $
            \(bnd, (p, pval), res) ->
              let i = nameToId (paramName p)
               in (bnd, i, pval, resSubExp res)
       in filter (\((_, t), _, _, _) -> isScalarType t) tmp'

    bindings = IS.fromList $ map (\((i, _), _, _, _) -> i) loopValues

    loopValueFor n =
      fromJust $ find (\(_, p, _, _) -> p == nameToId n) loopValues

    graphTheLoop = do
      mapM_ graphParam loopValues
      case lform of
        ForLoop _ _ _ elems -> mapM_ graphForInElem elems
        WhileLoop _ -> pure ()
      graphBody body

    graphParam ((_, t), p, pval, _) =
      do
        -- It is unknown whether a read can be delayed via the parameter from
        -- one iteration to the next, so we have to create a vertex even if the
        -- initial value never depends on a read.
        addVertex (p, t)
        ops <- onlyGraphedScalarSubExp pval
        addEdges (MG.oneEdge p) ops

    graphForInElem (p, _) =
      when (isScalar p) $ addSource (nameToId $ paramName p, typeOf p)

    connectLoop = do
      g <- getGraph
      mapM_ (mergeLoopParam g) loopValues

    mergeLoopParam g (_, p, _, res)
      | Var n <- res,
        ret <- nameToId n,
        ret /= p =
        if isSinkConnected p g
          then connectToSink ret
          else addEdges (MG.oneEdge p) (IS.singleton ret)
      | otherwise =
        pure ()

    graphBinding (bnd, p, _, _) =
      createNode bnd (IS.singleton p)

    deviceBindings g (s, vs) i =
      let (r, vs') = MG.reduce g bindingReach vs Normal i
       in case r of
            Produced s' -> (s <> s', vs')
            _ ->
              compilerBugS
                "Migration graph sink could be reached from source after it\
                \ had been attempted routed."

    bindingReach s _ v =
      let i = vertexId v
       in if IS.member i bindings
            then IS.insert i s
            else s

    connectOperand vs i = do
      g <- getGraph
      case MG.get i g of
        Nothing -> pure vs
        Just v ->
          case vertexEdges v of
            ToSink -> pure vs
            ToNodes es Nothing ->
              connectOp g vs i es
            ToNodes _ (Just nx) ->
              connectOp g vs i nx

    connectOp g vs i es = do
      let st = (IS.empty, [], vs)
      let (res, nx, vs') = findBindings g st (IS.elems es)
      case res of
        FoundSink -> connectToSink i
        Produced bnds -> modifyGraph $ MG.adjust (f nx bnds) i
      pure vs'

    f nx bnds v
      | ToNodes es _ <- vertexEdges v =
        let nx' = bnds <> IS.fromList nx
            es' = ToNodes (bnds <> es) (Just nx')
         in v {vertexEdges = es'}
      | otherwise =
        v

    findBindings _ (bnds, nx, vs) [] =
      (Produced bnds, nx, vs)
    findBindings g (bnds, nx, vs) (i : is)
      | Just v <- MG.get i g,
        Just gid <- metaGraphId (vertexMeta v),
        gid == subgraphId -- only search the subgraph
        =
        let (res, vs') = MG.reduce g bindingReach vs Normal i
         in case res of
              FoundSink -> (FoundSink, [], vs')
              Produced bnds' -> findBindings g (bnds <> bnds', nx, vs') is
      | otherwise =
        -- don't exhaust
        findBindings g (bnds, i : nx, vs) is

-- | Graph a 'WithAcc' statement.
graphWithAcc ::
  [Binding] ->
  [WithAccInput GPU] ->
  Lambda GPU ->
  Grapher ()
graphWithAcc bs inputs f = do
  -- Graph the body, capturing 'UpdateAcc' statements for delayed graphing.
  graphBody (lambdaBody f)

  -- Graph each accumulator monoid and its associated 'UpdateAcc' statements.
  mapM_ graph $ zip (lambdaReturnType f) inputs

  -- Connect return variables to bound values. No outgoing edge exists
  -- from an accumulator vertex so skip those. Note that accumulators do
  -- not map to returned arrays one-to-one but one-to-many.
  let res = drop (length inputs) (bodyResult $ lambdaBody f)
  ret <- mapM (onlyGraphedScalarSubExp . resSubExp) res
  mapM_ (uncurry createNode) $ zip (reverse bs) (reverse ret)
  where
    graph (Acc a _ types _, (_, _, comb)) = do
      let i = nameToId a

      delayed <- fromMaybe [] <$> gets (IM.lookup i . stateUpdateAccs)
      modify $ \st -> st {stateUpdateAccs = IM.delete i (stateUpdateAccs st)}

      graphAcc i types (fst <$> comb) delayed

      -- Neutral elements must always be made available on host for 'WithAcc'
      -- to type check.
      mapM_ connectSubExpToSink $ maybe [] snd comb
    graph _ =
      compilerBugS "Type error: WithAcc expression did not return accumulator."

-- Graph the operator and all 'UpdateAcc' statements associated with an
-- accumulator.
--
-- The arguments are the 'Id' for the accumulator token, the element types of
-- the accumulator/operator, its combining function if any, and all associated
-- 'UpdateAcc' statements outside kernels.
graphAcc :: Id -> [Type] -> Maybe (Lambda GPU) -> [Delayed] -> Grapher ()
graphAcc i _ _ [] = addSource (i, Prim Unit) -- only used on device
graphAcc i types op delayed = do
  -- Accumulators are intended for use within SegOps but in principle the AST
  -- allows their 'UpdateAcc's to be used outside a kernel. This case handles
  -- that unlikely situation.

  env <- ask
  st <- get

  -- Collect statistics about the operator statements.
  let lambda = fromMaybe (Lambda [] (Body () SQ.empty []) []) op
  let m = graphBody (lambdaBody lambda)
  let stats = R.runReader (evalStateT (captureBodyStats m) st) env

  -- op operands are read from arrays and written back so if any of the operands
  -- are scalar then a read can be avoided by moving the UpdateAcc usages to
  -- device. If the op itself performs scalar reads its UpdateAcc usages should
  -- also be moved.
  let does_read = bodyReads stats || any isScalarType types
  let host_only = bodyHostOnly stats

  -- Determine which external variables the operator depends upon.
  -- 'bodyOperands' cannot be used as it excludes branch conditions, operands
  -- that were connected to sinks, and others, so instead we create an artifical
  -- expression to capture graphed operands from.
  ops <- graphedScalarOperands (WithAcc [] lambda)

  case (host_only, does_read) of
    (True, _) -> do
      -- If the operator cannot run well in a GPUBody then all non-kernel
      -- UpdateAcc statements are host-only. The current analysis is ignorant
      -- of what happens within kernels so we must assume that the operator
      -- is used within a kernel, meaning that we cannot migrate its statements.
      --
      -- TODO: Improve analysis if UpdateAcc ever is used outside kernels.
      mapM_ (graphHostOnly . snd) delayed
      addEdges ToSink ops
    (_, True) -> do
      -- Migrate all accumulator usage to device.
      mapM_ (graphAutoMove . fst) delayed
      addSource (i, Prim Unit)
    _ -> do
      -- Only migrate operator and UpdateAcc statements to allow their
      -- operands to be migrated.
      createNode (i, Prim Unit) ops
      forM_ delayed $
        \(b, e) -> graphedScalarOperands e >>= createNode b . IS.insert i

-- Returns for an expression all scalar operands used outside a kernel and which
-- have a vertex representation in the graph, excluding those that have been
-- connected to sinks.
-- That is all operands that might require a read from device memory to evaluate
-- the given expression on host.
graphedScalarOperands :: Exp GPU -> Grapher Operands
graphedScalarOperands e =
  let is = fst $ execState (collect e) initial
   in IS.intersection is <$> getGraphedScalars
  where
    initial = (IS.empty, S.empty) -- scalar operands, accumulator tokens
    captureName n = modify $ \(is, accs) -> (IS.insert (nameToId n) is, accs)
    captureAcc a = modify $ \(is, accs) -> (is, S.insert a accs)
    collectFree x = mapM_ captureName (namesToList $ freeIn x)

    collect b@BasicOp {} =
      collectBasic b
    collect (Apply _ params _ _) =
      mapM_ (collectSubExp . fst) params
    collect (If cond tbranch fbranch _) =
      collectSubExp cond >> collectBody tbranch >> collectBody fbranch
    collect (DoLoop params lform body) =
      mapM_ (collectSubExp . snd) params
        >> collectLForm lform
        >> collectBody body
    collect (WithAcc accs f) =
      collectWithAcc accs f
    collect (Op op) =
      collectHostOp op

    -- Note: Plain VName values only refer to arrays.
    collectBasic = walkExpM (identityWalker {walkOnSubExp = collectSubExp})

    collectSubExp (Var n) = captureName n
    collectSubExp _ = pure ()

    collectBody = collectStms . bodyStms
    collectStms = mapM_ collectStm

    collectStm (Let pat _ ua)
      | BasicOp UpdateAcc {} <- ua,
        Pat [pe] <- pat,
        Acc a _ _ _ <- typeOf pe =
        -- Capture the tokens of accumulators used on host.
        captureAcc a >> collectBasic ua
    collectStm stm = collect (stmExp stm)

    collectLForm (ForLoop _ _ b _) = collectSubExp b
    -- WhileLoop condition is declared as a loop parameter.
    collectLForm (WhileLoop _) = pure ()

    -- The collective operands of an operator lambda body are only used on host
    -- if the associated accumulator is used in an UpdateAcc statement outside a
    -- kernel.
    collectWithAcc inputs f = do
      collectBody (lambdaBody f)
      used_accs <- gets snd
      let accs = take (length inputs) (lambdaReturnType f)
      let used = map (\(Acc a _ _ _) -> S.member a used_accs) accs
      mapM_ collectAcc (zip used inputs)

    collectAcc (_, (_, _, Nothing)) = pure ()
    collectAcc (used, (_, _, Just (op, nes))) = do
      mapM_ collectSubExp nes
      when used $ collectBody (lambdaBody op)

    -- Does not collect named operands in
    --
    --   * types and shapes; size variables are assumed available to the host.
    --
    --   * use by a kernel body.
    --
    -- All other operands are conservatively collected even if they generally
    -- appear to be size variables or results computed by a SizeOp.
    collectHostOp (SegOp (SegMap lvl sp _ _)) = do
      collectSegLevel lvl
      collectSegSpace sp
    collectHostOp (SegOp (SegRed lvl sp ops _ _)) = do
      collectSegLevel lvl
      collectSegSpace sp
      mapM_ collectSegBinOp ops
    collectHostOp (SegOp (SegScan lvl sp ops _ _)) = do
      collectSegLevel lvl
      collectSegSpace sp
      mapM_ collectSegBinOp ops
    collectHostOp (SegOp (SegHist lvl sp ops _ _)) = do
      collectSegLevel lvl
      collectSegSpace sp
      mapM_ collectHistOp ops
    collectHostOp (SizeOp op) = collectFree op
    collectHostOp (OtherOp op) = collectFree op
    collectHostOp GPUBody {} = pure ()

    collectSegLevel (SegThread (Count num) (Count size) _) =
      collectSubExp num >> collectSubExp size
    collectSegLevel (SegGroup (Count num) (Count size) _) =
      collectSubExp num >> collectSubExp size

    collectSegSpace space =
      mapM_ collectSubExp (segSpaceDims space)

    collectSegBinOp (SegBinOp _ _ nes _) =
      mapM_ collectSubExp nes

    collectHistOp (HistOp w rf _ nes _ _) = do
      collectSubExp w
      collectSubExp rf
      mapM_ collectSubExp nes
