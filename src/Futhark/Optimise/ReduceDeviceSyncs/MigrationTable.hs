-- |
-- This module implements program analysis to determine which program statements
-- the "Futhark.Optimise.ReduceDeviceSyncs" pass should move into 'GPUBody' kernels
-- to reduce blocking memory transfers between host and device. The results of
-- the analysis is encoded into a 'MigrationTable' which can be queried.
--
-- To reduce blocking scalar reads the module constructs a data flow
-- dependency graph of program variables (see
-- "Futhark.Optimise.ReduceDeviceSyncs.MigrationTable.Graph") in which
-- it finds a minimum vertex cut that separates array reads of scalars
-- from transitive usage that cannot or should not be migrated to
-- device.
--
-- The variables of each partition are assigned a 'MigrationStatus' that states
-- whether the computation of those variables should be moved to device or
-- remain on host. Due to how the graph is built and the vertex cut is found all
-- variables bound by a single statement will belong to the same partition.
--
-- The vertex cut contains all variables that will reside in device memory but
-- are required by host operations. These variables must be read from device
-- memory and cannot be reduced further in number merely by migrating
-- statements (subject to the accuracy of the graph model). The model is built
-- to reduce the worst-case number of scalar reads; an optimal migration of
-- statements depends on runtime data.
--
-- Blocking scalar writes are reduced by either turning such writes into
-- asynchronous kernels, as is done with scalar array literals and accumulator
-- updates, or by transforming host-device writing into device-device copying.
--
-- For details on how the graph is constructed and how the vertex cut is found,
-- see the master thesis "Reducing Synchronous GPU Memory Transfers" by Philip
-- Børgesen (2022).
module Futhark.Optimise.ReduceDeviceSyncs.MigrationTable
  ( -- * Analysis
    analyseFunDef,
    analyseConsts,
    hostOnlyFunDefs,

    -- * Types
    MigrationTable,
    MigrationStatus (..),

    -- * Query

    -- | These functions all assume that no parent statement should be migrated.
    -- That is @shouldMoveStm stm mt@ should return @False@ for every statement
    -- @stm@ with a body that a queried 'VName' or 'Stm' is nested within,
    -- otherwise the query result may be invalid.
    shouldMoveStm,
    shouldMove,
    usedOnHost,
    statusOf,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader qualified as R
import Control.Monad.Trans.State.Strict ()
import Control.Monad.Trans.State.Strict hiding (State)
import Data.Bifunctor (first, second)
import Data.Foldable
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Sequence qualified as SQ
import Data.Set (Set, (\\))
import Data.Set qualified as S
import Futhark.Error
import Futhark.IR.GPU
import Futhark.Optimise.ReduceDeviceSyncs.MigrationTable.Graph
  ( EdgeType (..),
    Edges (..),
    Id,
    IdSet,
    Result (..),
    Routing (..),
    Vertex (..),
  )
import Futhark.Optimise.ReduceDeviceSyncs.MigrationTable.Graph qualified as MG

--------------------------------------------------------------------------------
--                              MIGRATION TABLES                              --
--------------------------------------------------------------------------------

-- | Where the value bound by a name should be computed.
data MigrationStatus
  = -- | The statement that computes the value should be moved to device.
    -- No host usage of the value will be left after the migration.
    MoveToDevice
  | -- | As 'MoveToDevice' but host usage of the value will remain after
    -- migration.
    UsedOnHost
  | -- | The statement that computes the value should remain on host.
    StayOnHost
  deriving (Eq, Ord, Show)

-- | Identifies
--
--     (1) which statements should be moved from host to device to reduce the
--         worst case number of blocking memory transfers.
--
--     (2) which migrated variables that still will be used on the host after
--         all such statements have been moved.
newtype MigrationTable = MigrationTable (IM.IntMap MigrationStatus)

instance Semigroup MigrationTable where
  MigrationTable a <> MigrationTable b = MigrationTable (a `IM.union` b)

-- | Where should the value bound by this name be computed?
statusOf :: VName -> MigrationTable -> MigrationStatus
statusOf n (MigrationTable mt) =
  fromMaybe StayOnHost $ IM.lookup (baseTag n) mt

-- | Should this whole statement be moved from host to device?
shouldMoveStm :: Stm GPU -> MigrationTable -> Bool
shouldMoveStm (Let (Pat ((PatElem n _) : _)) _ (BasicOp (Index _ slice))) mt =
  statusOf n mt == MoveToDevice || any movedOperand slice
  where
    movedOperand (Var op) = statusOf op mt == MoveToDevice
    movedOperand _ = False
shouldMoveStm (Let (Pat ((PatElem n _) : _)) _ (BasicOp _)) mt =
  statusOf n mt /= StayOnHost
shouldMoveStm (Let (Pat ((PatElem n _) : _)) _ Apply {}) mt =
  statusOf n mt /= StayOnHost
shouldMoveStm (Let _ _ (Match cond _ _ _)) mt =
  all ((== MoveToDevice) . (`statusOf` mt)) $ subExpVars cond
shouldMoveStm (Let _ _ (Loop _ (ForLoop _ _ (Var n)) _)) mt =
  statusOf n mt == MoveToDevice
shouldMoveStm (Let _ _ (Loop _ (WhileLoop n) _)) mt =
  statusOf n mt == MoveToDevice
-- BasicOp and Apply statements might not bind any variables (shouldn't happen).
-- If statements might use a constant branch condition.
-- For loop statements might use a constant number of iterations.
-- HostOp statements cannot execute on device.
-- WithAcc statements are never moved in their entirety.
shouldMoveStm _ _ = False

-- | Should the value bound by this name be computed on device?
shouldMove :: VName -> MigrationTable -> Bool
shouldMove n mt = statusOf n mt /= StayOnHost

-- | Will the value bound by this name be used on host?
usedOnHost :: VName -> MigrationTable -> Bool
usedOnHost n mt = statusOf n mt /= MoveToDevice

--------------------------------------------------------------------------------
--                         HOST-ONLY FUNCTION ANALYSIS                        --
--------------------------------------------------------------------------------

-- | Identifies top-level function definitions that cannot be run on the
-- device. The application of any such function is host-only.
type HostOnlyFuns = Set Name

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
  checkFParams $ funDefParams fun
  checkRetTypes $ map fst $ funDefRetType fun
  checkBody $ funDefBody fun
  where
    hostOnly = Nothing
    ok = Just ()
    check isArr as = if any isArr as then hostOnly else ok

    checkFParams = check isArray

    checkLParams = check (isArray . fst)

    checkRetTypes = check isArrayType

    checkPats = check isArray

    checkBody = checkStms . bodyStms

    checkStms stms = S.unions <$> mapM checkStm stms

    checkStm (Let (Pat pats) _ e) = checkPats pats >> checkExp e

    -- Any expression that produces an array is caught by checkPats
    checkExp (BasicOp (Index _ _)) = hostOnly
    checkExp (WithAcc _ _) = hostOnly
    checkExp (Op _) = hostOnly
    checkExp (Apply fn _ _ _) = Just (S.singleton fn)
    checkExp (Match _ cases defbody _) =
      mconcat <$> mapM checkBody (defbody : map caseBody cases)
    checkExp (Loop params _ body) = do
      checkLParams params
      checkBody body
    checkExp BasicOp {} = Just S.empty

--------------------------------------------------------------------------------
--                             MIGRATION ANALYSIS                             --
--------------------------------------------------------------------------------

-- | HostUsage identifies scalar variables that are used on host.
type HostUsage = [Id]

nameToId :: VName -> Id
nameToId = baseTag

-- | Analyses top-level constants.
analyseConsts :: HostOnlyFuns -> [FunDef GPU] -> Stms GPU -> MigrationTable
analyseConsts hof funs consts =
  let usage = M.foldlWithKey (f $ freeIn funs) [] (scopeOf consts)
   in analyseStms hof usage consts
  where
    f free usage n t
      | isScalar t,
        n `nameIn` free =
          nameToId n : usage
      | otherwise =
          usage

-- | Analyses a top-level function definition.
analyseFunDef :: HostOnlyFuns -> FunDef GPU -> MigrationTable
analyseFunDef hof fd =
  let body = funDefBody fd
      usage = foldl' f [] $ zip (bodyResult body) (map fst $ funDefRetType fd)
      stms = bodyStms body
   in analyseStms hof usage stms
  where
    f usage (SubExpRes _ (Var n), t) | isScalarType t = nameToId n : usage
    f usage _ = usage

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

--------------------------------------------------------------------------------
--                                TYPE HELPERS                                --
--------------------------------------------------------------------------------

isScalar :: (Typed t) => t -> Bool
isScalar = isScalarType . typeOf

isScalarType :: TypeBase shape u -> Bool
isScalarType (Prim Unit) = False
isScalarType (Prim _) = True
isScalarType _ = False

isArray :: (Typed t) => t -> Bool
isArray = isArrayType . typeOf

isArrayType :: (ArrayShape shape) => TypeBase shape u -> Bool
isArrayType = (0 <) . arrayRank

--------------------------------------------------------------------------------
--                               GRAPH BUILDING                               --
--------------------------------------------------------------------------------

buildGraph :: HostOnlyFuns -> HostUsage -> Stms GPU -> (Graph, Sources, Sinks)
buildGraph hof usage stms =
  let (g, srcs, sinks) = execGrapher hof (graphStms stms)
      g' = foldl' (flip MG.connectToSink) g usage
   in (g', srcs, sinks)

-- | Graph a body.
graphBody :: Body GPU -> Grapher ()
graphBody body = do
  let res_ops = namesIntSet $ freeIn (bodyResult body)
  body_stats <-
    captureBodyStats $
      incBodyDepthFor (graphStms (bodyStms body) >> tellOperands res_ops)

  body_depth <- (1 +) <$> getBodyDepth
  let host_only = IS.member body_depth (bodyHostOnlyParents body_stats)
  modify $ \st ->
    let stats = stateStats st
        hops' = IS.delete body_depth (bodyHostOnlyParents stats)
        -- If body contains a variable that is required on host the parent
        -- statement that contains this body cannot be migrated as a whole.
        stats' = if host_only then stats {bodyHostOnly = True} else stats
     in st {stateStats = stats' {bodyHostOnlyParents = hops'}}

-- | Graph multiple statements.
graphStms :: Stms GPU -> Grapher ()
graphStms = mapM_ graphStm

-- | Graph a single statement.
graphStm :: Stm GPU -> Grapher ()
graphStm stm = do
  let bs = boundBy stm
  let e = stmExp stm
  -- IMPORTANT! It is generally assumed that all scalars within types and
  -- shapes are present on host. Any expression of a type wherein one of its
  -- scalar operands appears must therefore ensure that that scalar operand is
  -- marked as a size variable (see the 'hostSize' function).
  case e of
    BasicOp (SubExp se) -> do
      graphSimple bs e
      one bs `reusesSubExp` se
    BasicOp (Opaque _ se) -> do
      graphSimple bs e
      one bs `reusesSubExp` se
    BasicOp (ArrayLit arr t)
      | isScalar t,
        any (isJust . subExpVar) arr ->
          -- Migrating an array literal with free variables saves a write for
          -- every scalar it contains. Under some backends the compiler
          -- generates asynchronous writes for scalar constants but otherwise
          -- each write will be synchronous. If all scalars are constants then
          -- the compiler generates more efficient code that copies static
          -- device memory.
          graphAutoMove (one bs)
    BasicOp UnOp {} -> graphSimple bs e
    BasicOp BinOp {} -> graphSimple bs e
    BasicOp CmpOp {} -> graphSimple bs e
    BasicOp ConvOp {} -> graphSimple bs e
    BasicOp Assert {} ->
      -- == OpenCL =============================================================
      --
      -- The next read after the execution of a kernel containing an assertion
      -- will be made asynchronous, followed by an asynchronous read to check
      -- if any assertion failed. The runtime will then block for all enqueued
      -- operations to finish.
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
      --
      -- == CUDA ===============================================================
      --
      -- Under the CUDA backend every read is synchronous and is followed by
      -- a full synchronization that blocks for all enqueued operations to
      -- finish. If any enqueued kernel contained an assertion, another
      -- synchronous read is then made to check if an assertion failed.
      --
      -- Migrating an assertion to save a read may thus introduce new reads, and
      -- the total number of reads can hence either decrease, remain the same,
      -- or even increase, subject to the ordering of reads and kernels that
      -- perform assertions.
      --
      -- Since it is possible to implement the same failure checking scheme as
      -- OpenCL using asynchronous reads (and doing so would be a good idea!)
      -- we consider this to be acceptable.
      --
      -- TODO: Implement the OpenCL failure checking scheme under CUDA. This
      --       should reduce the number of synchronizations per read to one.
      graphSimple bs e
    BasicOp (Index _ slice)
      | isFixed slice ->
          graphRead (one bs)
    BasicOp {}
      | [(_, t)] <- bs,
        dims <- arrayDims t,
        dims /= [], -- i.e. produces an array
        all (== intConst Int64 1) dims ->
          -- An expression that produces an array that only contains a single
          -- primitive value is as efficient to compute and copy as a scalar,
          -- and introduces no size variables.
          --
          -- This is an exception to the inefficiency rules that comes next.
          graphSimple bs e
    -- Expressions with a cost sublinear to the size of their result arrays are
    -- risky to migrate as we cannot guarantee that their results are not
    -- returned from a GPUBody, which always copies its return values. Since
    -- this would make the effective asymptotic cost of such statements linear
    -- we block them from being migrated on their own.
    --
    -- The parent statement of an enclosing body may still be migrated as a
    -- whole given that each of its returned arrays either
    --   1) is backed by memory used by a migratable statement within its body.
    --   2) contains just a single element.
    -- An array matching either criterion is denoted "copyable memory" because
    -- the asymptotic cost of copying it is less than or equal to the statement
    -- that produced it. This makes the parent of statements with sublinear cost
    -- safe to migrate.
    BasicOp (Index arr s) -> do
      graphInefficientReturn (sliceDims s) e
      one bs `reuses` arr
    BasicOp (Update _ arr slice _)
      | isFixed slice -> do
          graphInefficientReturn [] e
          one bs `reuses` arr
    BasicOp (FlatIndex arr s) -> do
      -- Migrating a FlatIndex leads to a memory allocation error.
      --
      -- TODO: Fix FlatIndex memory allocation error.
      --
      -- Can be replaced with 'graphHostOnly e' to disable migration.
      -- A fix can be verified by enabling tests/migration/reuse2_flatindex.fut
      graphInefficientReturn (flatSliceDims s) e
      one bs `reuses` arr
    BasicOp (FlatUpdate arr _ _) -> do
      graphInefficientReturn [] e
      one bs `reuses` arr
    BasicOp (Scratch _ s) ->
      -- Migrating a Scratch leads to a memory allocation error.
      --
      -- TODO: Fix Scratch memory allocation error.
      --
      -- Can be replaced with 'graphHostOnly e' to disable migration.
      -- A fix can be verified by enabling tests/migration/reuse4_scratch.fut
      graphInefficientReturn s e
    BasicOp (Reshape arr s) -> do
      graphInefficientReturn (shapeDims $ newShape s) e
      one bs `reuses` arr
    BasicOp (Rearrange _ arr) -> do
      graphInefficientReturn [] e
      one bs `reuses` arr
    -- Expressions with a cost linear to the size of their result arrays are
    -- inefficient to migrate into GPUBody kernels as such kernels are single-
    -- threaded. For sufficiently large arrays the cost may exceed what is saved
    -- by avoiding reads. We therefore also block these from being migrated,
    -- as well as their parents.
    BasicOp ArrayLit {} ->
      -- An array literal purely of primitive constants can be hoisted out to be
      -- a top-level constant, unless it is to be returned or consumed.
      -- Otherwise its runtime implementation will copy a precomputed static
      -- array and thus behave like a 'Copy'.
      -- Whether the rows are primitive constants or arrays, without any scalar
      -- variable operands such ArrayLit cannot directly prevent a scalar read.
      graphHostOnly e
    BasicOp ArrayVal {} ->
      -- As above.
      graphHostOnly e
    BasicOp Update {} ->
      graphHostOnly e
    BasicOp Concat {} ->
      -- Is unlikely to prevent a scalar read as the only SubExp operand in
      -- practice is a computation of host-only size variables.
      graphHostOnly e
    BasicOp Manifest {} ->
      -- Takes no scalar operands so cannot directly prevent a scalar read.
      -- It is introduced as part of the BlkRegTiling kernel optimization and
      -- is thus unlikely to prevent the migration of a parent which was not
      -- already blocked by some host-only operation.
      graphHostOnly e
    BasicOp Iota {} -> graphHostOnly e
    BasicOp Replicate {} -> graphHostOnly e
    -- END
    BasicOp UpdateAcc {} ->
      graphUpdateAcc (one bs) e
    Apply fn _ _ _ ->
      graphApply fn bs e
    Match ses cases defbody _ ->
      graphMatch bs ses cases defbody
    Loop params lform body ->
      graphLoop bs params lform body
    WithAcc inputs f ->
      graphWithAcc bs inputs f
    Op GPUBody {} ->
      -- A GPUBody can be migrated into a parent GPUBody by replacing it with
      -- its body statements and binding its return values inside 'ArrayLit's.
      tellGPUBody
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
      graphedScalarOperands e >>= addEdges ToSink

    hostSize (Var n) = hostSizeVar n
    hostSize _ = pure ()

    hostSizeVar = requiredOnHost . nameToId

-- | Bindings for all pattern elements bound by a statement.
boundBy :: Stm GPU -> [Binding]
boundBy = map (\(PatElem n t) -> (nameToId n, t)) . patElems . stmPat

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
-- Parent statements of enclosing bodies are also blocked from being migrated.
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

-- | Graph a Match statement.
graphMatch :: [Binding] -> [SubExp] -> [Case (Body GPU)] -> Body GPU -> Grapher ()
graphMatch bs ses cases defbody = do
  body_host_only <-
    incForkDepthFor $
      any bodyHostOnly
        <$> mapM (captureBodyStats . graphBody) (defbody : map caseBody cases)

  let branch_results = results defbody : map (results . caseBody) cases

  -- Record aliases for copyable memory backing returned arrays.
  may_copy_results <- reusesBranches bs branch_results
  let may_migrate = not body_host_only && may_copy_results

  cond_id <-
    if may_migrate
      then onlyGraphedScalars $ subExpVars ses
      else do
        -- The migration status of the condition is what determines
        -- whether the statement may be migrated as a whole or
        -- not. See 'shouldMoveStm'.
        mapM_ (connectToSink . nameToId) (subExpVars ses)
        pure IS.empty

  tellOperands cond_id

  -- Connect branch results to bound variables to allow delaying reads out of
  -- branches. It might also be beneficial to move the whole statement to
  -- device, to avoid reading the branch condition value. This must be balanced
  -- against the need to read the values bound by the if statement.
  --
  -- By connecting the branch condition to each variable bound by the statement
  -- the condition will only stay on device if
  --
  --   (1) the if statement is not required on host, based on the statements
  --       within its body.
  --
  --   (2) no additional reads will be required to use the if statement bound
  --       variables should the whole statement be migrated.
  --
  -- If the condition is migrated to device and stays there, then the if
  -- statement must necessarily execute on device.
  --
  -- While the graph model built by this module generally migrates no more
  -- statements than necessary to obtain a minimum vertex cut, the branches
  -- of if statements are subject to an inaccuracy. Specifically model is not
  -- strong enough to capture their mutual exclusivity and thus encodes that
  -- both branches are taken. While this does not affect the resulting number
  -- of host-device reads it means that some reads may needlessly be delayed
  -- out of branches. The overhead as measured on futhark-benchmarks appears
  -- to be neglible though.
  ret <- mapM (comb cond_id) $ L.transpose branch_results
  mapM_ (uncurry createNode) (zip bs ret)
  where
    results = map resSubExp . bodyResult

    comb ci a = (ci <>) <$> onlyGraphedScalars (S.fromList $ subExpVars a)

-----------------------------------------------------
-- These type aliases are only used by 'graphLoop' --
-----------------------------------------------------
type ReachableBindings = IdSet

type ReachableBindingsCache = MG.Visited (MG.Result ReachableBindings)

type NonExhausted = [Id]

type LoopValue = (Binding, Id, SubExp, SubExp)

-----------------------------------------------------
-----------------------------------------------------

-- | Graph a loop statement.
graphLoop ::
  [Binding] ->
  [(FParam GPU, SubExp)] ->
  LoopForm ->
  Body GPU ->
  Grapher ()
graphLoop [] _ _ _ =
  -- We expect each loop to bind a value or be eliminated.
  compilerBugS "Loop statement bound no variable; should have been eliminated."
graphLoop (b : bs) params lform body = do
  -- Graph loop params and body while capturing statistics.
  g <- getGraph
  stats <- captureBodyStats (subgraphId `graphIdFor` graphTheLoop)

  -- Record aliases for copyable memory backing returned arrays.
  -- Does the loop return any arrays which prevent it from being migrated?
  let args = map snd params
  let results = map resSubExp (bodyResult body)
  may_copy_results <- reusesBranches (b : bs) [args, results]

  -- Connect the loop condition to a sink if the loop cannot be migrated,
  -- ensuring that it will be available to the host. The migration status
  -- of the condition is what determines whether the loop may be migrated
  -- as a whole or not. See 'shouldMoveStm'.
  let may_migrate = not (bodyHostOnly stats) && may_copy_results
  unless may_migrate $ case lform of
    ForLoop _ _ (Var n) -> connectToSink (nameToId n)
    WhileLoop n
      | Just (_, p, _, res) <- loopValueFor n -> do
          connectToSink p
          case res of
            Var v -> connectToSink (nameToId v)
            _ -> pure ()
    _ -> pure ()

  -- Connect graphed return values to their loop parameters.
  mapM_ mergeLoopParam loopValues

  -- Route the sources within the loop body in isolation.
  -- The loop graph must not be altered after this point.
  srcs <- routeSubgraph subgraphId

  -- Graph the variables bound by the statement.
  forM_ loopValues $ \(bnd, p, _, _) -> createNode bnd (IS.singleton p)

  -- If a device read is delayed from one iteration to the next the
  -- corresponding variables bound by the statement must be treated as
  -- sources.
  g' <- getGraph
  let (dbs, rbc) = foldl' (deviceBindings g') (IS.empty, MG.none) srcs
  modifySources $ second (IS.toList dbs <>)

  -- Connect operands to sinks if they can reach a sink within the loop.
  -- Otherwise connect them to the loop bound variables that they can
  -- reach and exhaust their normal entry edges into the loop.
  -- This means a read can be delayed through a loop but not into it if
  -- that would increase the number of reads done by any given iteration.
  let ops = IS.filter (`MG.member` g) (bodyOperands stats)
  foldM_ connectOperand rbc (IS.elems ops)

  -- It might be beneficial to move the whole loop to device, to avoid
  -- reading the (initial) loop condition value. This must be balanced
  -- against the need to read the values bound by the loop statement.
  --
  -- For more details see the similar description for if statements.
  when may_migrate $ case lform of
    ForLoop _ _ n ->
      onlyGraphedScalarSubExp n >>= addEdges (ToNodes bindings Nothing)
    WhileLoop n
      | Just (_, _, arg, _) <- loopValueFor n ->
          onlyGraphedScalarSubExp arg >>= addEdges (ToNodes bindings Nothing)
    _ -> pure ()
  where
    subgraphId :: Id
    subgraphId = fst b

    loopValues :: [LoopValue]
    loopValues =
      let tmp = zip3 (b : bs) params (bodyResult body)
          tmp' = flip map tmp $
            \(bnd, (p, arg), res) ->
              let i = nameToId (paramName p)
               in (bnd, i, arg, resSubExp res)
       in filter (\((_, t), _, _, _) -> isScalar t) tmp'

    bindings :: IdSet
    bindings = IS.fromList $ map (\((i, _), _, _, _) -> i) loopValues

    loopValueFor n =
      find (\(_, p, _, _) -> p == nameToId n) loopValues

    graphTheLoop :: Grapher ()
    graphTheLoop = do
      mapM_ graphParam loopValues

      -- For simplicity we do not currently track memory reuse through merge
      -- parameters. A parameter does not simply reuse the memory of its
      -- argument; it must also consider the iteration return value, which in
      -- turn may depend on other merge parameters.
      --
      -- Situations that would benefit from this tracking is unlikely to occur
      -- at the time of writing, and if it occurs current compiler limitations
      -- will prevent successful compilation.
      -- Specifically it requires the merge parameter argument to reuse memory
      -- from an array literal, and both it and the loop must occur within an
      -- if statement branch. Array literals are generally hoisted out of if
      -- statements however, and when they are not, a memory allocation error
      -- occurs.
      --
      -- TODO: Track memory reuse through merge parameters.

      case lform of
        ForLoop _ _ n ->
          onlyGraphedScalarSubExp n >>= tellOperands
        WhileLoop _ -> pure ()
      graphBody body
      where
        graphParam ((_, t), p, arg, _) =
          do
            -- It is unknown whether a read can be delayed via the parameter
            -- from one iteration to the next, so we have to create a vertex
            -- even if the initial value never depends on a read.
            addVertex (p, t)
            ops <- onlyGraphedScalarSubExp arg
            addEdges (MG.oneEdge p) ops

    mergeLoopParam :: LoopValue -> Grapher ()
    mergeLoopParam (_, p, _, res)
      | Var n <- res,
        ret <- nameToId n,
        ret /= p =
          addEdges (MG.oneEdge p) (IS.singleton ret)
      | otherwise =
          pure ()
    deviceBindings ::
      Graph ->
      (ReachableBindings, ReachableBindingsCache) ->
      Id ->
      (ReachableBindings, ReachableBindingsCache)
    deviceBindings g (rb, rbc) i =
      let (r, rbc') = MG.reduce g bindingReach rbc Normal i
       in case r of
            Produced rb' -> (rb <> rb', rbc')
            _ ->
              compilerBugS
                "Migration graph sink could be reached from source after it\
                \ had been attempted routed."
    bindingReach ::
      ReachableBindings ->
      EdgeType ->
      Vertex Meta ->
      ReachableBindings
    bindingReach rb _ v
      | i <- vertexId v,
        IS.member i bindings =
          IS.insert i rb
      | otherwise =
          rb
    connectOperand ::
      ReachableBindingsCache ->
      Id ->
      Grapher ReachableBindingsCache
    connectOperand cache op = do
      g <- getGraph
      case MG.lookup op g of
        Nothing -> pure cache
        Just v ->
          case vertexEdges v of
            ToSink -> pure cache
            ToNodes es Nothing -> connectOp g cache op es
            ToNodes _ (Just nx) -> connectOp g cache op nx
      where
        connectOp ::
          Graph ->
          ReachableBindingsCache ->
          Id -> -- operand id
          IdSet -> -- its edges
          Grapher ReachableBindingsCache
        connectOp g rbc i es = do
          let (res, nx, rbc') = findBindings g (IS.empty, [], rbc) (IS.elems es)
          case res of
            FoundSink -> connectToSink i
            Produced rb -> modifyGraph $ MG.adjust (updateEdges nx rb) i
          pure rbc'
        updateEdges ::
          NonExhausted ->
          ReachableBindings ->
          Vertex Meta ->
          Vertex Meta
        updateEdges nx rb v
          | ToNodes es _ <- vertexEdges v =
              let nx' = IS.fromList nx
                  es' = ToNodes (rb <> es) $ Just (rb <> nx')
               in v {vertexEdges = es'}
          | otherwise = v
        findBindings ::
          Graph ->
          (ReachableBindings, NonExhausted, ReachableBindingsCache) ->
          [Id] -> -- current non-exhausted edges
          (MG.Result ReachableBindings, NonExhausted, ReachableBindingsCache)
        findBindings _ (rb, nx, rbc) [] =
          (Produced rb, nx, rbc)
        findBindings g (rb, nx, rbc) (i : is)
          | Just v <- MG.lookup i g,
            Just gid <- metaGraphId (vertexMeta v),
            gid == subgraphId -- only search the subgraph
            =
              let (res, rbc') = MG.reduce g bindingReach rbc Normal i
               in case res of
                    FoundSink -> (FoundSink, [], rbc')
                    Produced rb' -> findBindings g (rb <> rb', nx, rbc') is
          | otherwise =
              -- don't exhaust
              findBindings g (rb, i : nx, rbc) is

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

  -- Record aliases for the backing memory of each returned array.
  -- 'WithAcc' statements are never migrated as a whole and always returns
  -- arrays backed by memory allocated elsewhere.
  let arrs = concatMap (\(_, as, _) -> map Var as) inputs
  let res = drop (length inputs) (bodyResult $ lambdaBody f)
  _ <- reusesReturn bs (arrs ++ map resSubExp res)

  -- Connect return variables to bound values. No outgoing edge exists
  -- from an accumulator vertex so skip those. Note that accumulators do
  -- not map to returned arrays one-to-one but one-to-many.
  ret <- mapM (onlyGraphedScalarSubExp . resSubExp) res
  mapM_ (uncurry createNode) $ zip (drop (length arrs) bs) ret
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
graphAcc i _ _ [] = addSource (i, Prim Unit) -- Only used on device.
graphAcc i types op delayed = do
  -- Accumulators are intended for use within SegOps but in principle the AST
  -- allows their 'UpdateAcc's to be used outside a kernel. This case handles
  -- that unlikely situation.

  env <- ask
  st <- get

  -- Collect statistics about the operator statements.
  let lambda = fromMaybe (Lambda [] [] (Body () SQ.empty [])) op
  let m = graphBody (lambdaBody lambda)
  let stats = R.runReader (evalStateT (captureBodyStats m) st) env
  -- We treat GPUBody kernels as host-only to not bother rewriting them inside
  -- operators and to simplify the analysis. They are unlikely to occur anyway.
  --
  -- NOTE: Performance may degrade if a GPUBody is replaced with its contents
  --       but the containing operator is used on host.
  let host_only = bodyHostOnly stats || bodyHasGPUBody stats

  -- op operands are read from arrays and written back so if any of the operands
  -- are scalar then a read can be avoided by moving the UpdateAcc usages to
  -- device. If the op itself performs scalar reads its UpdateAcc usages should
  -- also be moved.
  let does_read = bodyReads stats || any isScalar types

  -- Determine which external variables the operator depends upon.
  -- 'bodyOperands' cannot be used as it might exclude operands that were
  -- connected to sinks within the body, so instead we create an artifical
  -- expression to capture graphed operands from.
  ops <- graphedScalarOperands (WithAcc [] lambda)

  case (host_only, does_read) of
    (True, _) -> do
      -- If the operator cannot run well in a GPUBody then all non-kernel
      -- UpdateAcc statements are host-only. The current analysis is ignorant
      -- of what happens inside kernels so we must assume that the operator
      -- is used within a kernel, meaning that we cannot migrate its statements.
      --
      -- TODO: Improve analysis if UpdateAcc ever is used outside kernels.
      mapM_ (graphHostOnly . snd) delayed
      addEdges ToSink ops
    (_, True) -> do
      -- Migrate all accumulator usage to device to avoid reads and writes.
      mapM_ (graphAutoMove . fst) delayed
      addSource (i, Prim Unit)
    _ -> do
      -- Only migrate operator and UpdateAcc statements if it can allow their
      -- operands to be migrated.
      createNode (i, Prim Unit) ops
      forM_ delayed $
        \(b, e) -> graphedScalarOperands e >>= createNode b . IS.insert i

-- Returns for an expression all scalar operands that must be made available
-- on host to execute the expression there.
graphedScalarOperands :: Exp GPU -> Grapher Operands
graphedScalarOperands e =
  let is = fst $ execState (collect e) initial
   in IS.intersection is <$> getGraphedScalars
  where
    initial = (IS.empty, S.empty) -- scalar operands, accumulator tokens
    captureName n = modify $ first $ IS.insert (nameToId n)
    captureAcc a = modify $ second $ S.insert a
    collectFree x = mapM_ captureName (namesToList $ freeIn x)

    collect b@BasicOp {} =
      collectBasic b
    collect (Apply _ params _ _) =
      mapM_ (collectSubExp . fst) params
    collect (Match ses cases defbody _) = do
      mapM_ collectSubExp ses
      mapM_ (collectBody . caseBody) cases
      collectBody defbody
    collect (Loop params lform body) = do
      mapM_ (collectSubExp . snd) params
      collectLForm lform
      collectBody body
    collect (WithAcc accs f) =
      collectWithAcc accs f
    collect (Op op) =
      collectHostOp op

    collectBasic (BasicOp (Update _ _ slice _)) =
      -- Writing a scalar to an array can be replaced with copying a single-
      -- element slice. If the scalar originates from device memory its read
      -- can thus be prevented without requiring the 'Update' to be migrated.
      collectFree slice
    collectBasic (BasicOp (Replicate shape _)) =
      -- The replicate of a scalar can be rewritten as a replicate of a single
      -- element array followed by a slice index.
      collectFree shape
    collectBasic e' =
      -- Note: Plain VName values only refer to arrays.
      walkExpM (identityWalker {walkOnSubExp = collectSubExp}) e'

    collectSubExp (Var n) = captureName n
    collectSubExp _ = pure ()

    collectBody body = do
      collectStms (bodyStms body)
      collectFree (bodyResult body)
    collectStms = mapM_ collectStm

    collectStm (Let pat _ ua)
      | BasicOp UpdateAcc {} <- ua,
        Pat [pe] <- pat,
        Acc a _ _ _ <- typeOf pe =
          -- Capture the tokens of accumulators used on host.
          captureAcc a >> collectBasic ua
    collectStm stm = collect (stmExp stm)

    collectLForm (ForLoop _ _ b) = collectSubExp b
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
    collectHostOp (SegOp (SegRed lvl sp _ _ ops)) = do
      collectSegLevel lvl
      collectSegSpace sp
      mapM_ collectSegBinOp ops
    collectHostOp (SegOp (SegScan lvl sp _ _ ops)) = do
      collectSegLevel lvl
      collectSegSpace sp
      mapM_ collectSegBinOp ops
    collectHostOp (SegOp (SegHist lvl sp _ _ ops)) = do
      collectSegLevel lvl
      collectSegSpace sp
      mapM_ collectHistOp ops
    collectHostOp (SizeOp op) = collectFree op
    collectHostOp (OtherOp op) = collectFree op
    collectHostOp GPUBody {} = pure ()

    collectSegLevel = mapM_ captureName . namesToList . freeIn

    collectSegSpace space =
      mapM_ collectSubExp (segSpaceDims space)

    collectSegBinOp (SegBinOp _ _ nes _) =
      mapM_ collectSubExp nes

    collectHistOp (HistOp _ rf _ nes _ _) = do
      collectSubExp rf
      mapM_ collectSubExp nes

--------------------------------------------------------------------------------
--                        GRAPH BUILDING - PRIMITIVES                         --
--------------------------------------------------------------------------------

-- | Creates a vertex for the given binding, provided that the set of operands
-- is not empty.
createNode :: Binding -> Operands -> Grapher ()
createNode b ops =
  unless (IS.null ops) (addVertex b >> addEdges (MG.oneEdge $ fst b) ops)

-- | Adds a vertex to the graph for the given binding.
addVertex :: Binding -> Grapher ()
addVertex (i, t) = do
  meta <- getMeta
  let v = MG.vertex i meta
  when (isScalar t) $ modifyGraphedScalars (IS.insert i)
  when (isArray t) $ recordCopyableMemory i (metaBodyDepth meta)
  modifyGraph (MG.insert v)

-- | Adds a source connected vertex to the graph for the given binding.
addSource :: Binding -> Grapher ()
addSource b = do
  addVertex b
  modifySources $ second (fst b :)

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
  mv <- MG.lookup i <$> getGraph
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

-- | Like 'connectToSink' but vertex is given by a t'SubExp'. This is a no-op if
-- the t'SubExp' is a constant.
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
  | Just v <- MG.lookup i g,
    Just mgi <- metaGraphId (vertexMeta v) =
      si == mgi
inSubGraph _ _ _ = False

-- | @b `reuses` n@ records that @b@ binds an array backed by the same memory
-- as @n@. If @b@ is not array typed or the backing memory is not copyable then
-- this does nothing.
reuses :: Binding -> VName -> Grapher ()
reuses (i, t) n
  | isArray t =
      do
        body_depth <- outermostCopyableArray n
        forM_ body_depth (recordCopyableMemory i)
  | otherwise =
      pure ()

reusesSubExp :: Binding -> SubExp -> Grapher ()
reusesSubExp b (Var n) = b `reuses` n
reusesSubExp _ _ = pure ()

-- @reusesReturn bs res@ records each array binding in @bs@ as reusing copyable
-- memory if the corresponding return value in @res@ is backed by copyable
-- memory.
--
-- If every array binding is registered as being backed by copyable memory then
-- the function returns @True@, otherwise it returns @False@.
reusesReturn :: [Binding] -> [SubExp] -> Grapher Bool
reusesReturn bs res = do
  body_depth <- metaBodyDepth <$> getMeta
  foldM (reuse body_depth) True (zip bs res)
  where
    reuse :: Int -> Bool -> (Binding, SubExp) -> Grapher Bool
    reuse body_depth onlyCopyable (b, se)
      | all (== intConst Int64 1) (arrayDims $ snd b) =
          -- Single element arrays are immediately recognizable as copyable so
          -- don't bother recording those. Note that this case also matches
          -- primitive return values.
          pure onlyCopyable
      | (i, t) <- b,
        isArray t,
        Var n <- se =
          do
            res_body_depth <- outermostCopyableArray n
            case res_body_depth of
              Just inner -> do
                recordCopyableMemory i (min body_depth inner)
                let returns_free_var = inner <= body_depth
                pure (onlyCopyable && not returns_free_var)
              _ ->
                pure False
      | otherwise =
          pure onlyCopyable

-- @reusesBranches bs seses@ records each array binding in @bs@ as
-- reusing copyable memory if each corresponding return value in the
-- lists in @ses@ are backed by copyable memory.  Each list is the
-- result of a branch body (i.e. for 'if' the list has two elements).
--
-- If every array binding is registered as being backed by copyable
-- memory then the function returns @True@, otherwise it returns
-- @False@.
reusesBranches :: [Binding] -> [[SubExp]] -> Grapher Bool
reusesBranches bs seses = do
  body_depth <- metaBodyDepth <$> getMeta
  foldM (reuse body_depth) True $ zip bs $ L.transpose seses
  where
    reuse :: Int -> Bool -> (Binding, [SubExp]) -> Grapher Bool
    reuse body_depth onlyCopyable (b, ses)
      | all (== intConst Int64 1) (arrayDims $ snd b) =
          -- Single element arrays are immediately recognizable as copyable so
          -- don't bother recording those. Note that this case also matches
          -- primitive return values.
          pure onlyCopyable
      | (i, t) <- b,
        isArray t,
        Just ns <- mapM subExpVar ses = do
          body_depths <- mapM outermostCopyableArray ns
          case sequence body_depths of
            Just bds -> do
              let inner = minimum bds
              recordCopyableMemory i (min body_depth inner)
              let returns_free_var = inner <= body_depth
              pure (onlyCopyable && not returns_free_var)
            _ ->
              pure False
      | otherwise =
          pure onlyCopyable

--------------------------------------------------------------------------------
--                           GRAPH BUILDING - TYPES                           --
--------------------------------------------------------------------------------

type Grapher = StateT State (R.Reader Env)

data Env = Env
  { -- | See 'HostOnlyFuns'.
    envHostOnlyFuns :: HostOnlyFuns,
    -- | Metadata for the current body being graphed.
    envMeta :: Meta
  }

-- | A measurement of how many bodies something is nested within.
type BodyDepth = Int

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
    metaBodyDepth :: BodyDepth,
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
    -- | Whether the body contained any GPUBody kernels.
    bodyHasGPUBody :: Bool,
    -- | Whether the body performed any reads.
    bodyReads :: Bool,
    -- | All scalar variables represented in the graph that have been used
    -- as return values of the body or as operands within it, including those
    -- that are defined within the body itself. Variables with vertices
    -- connected to sinks may be excluded.
    bodyOperands :: Operands,
    -- | Depth of parent bodies with variables that are required on host. Since
    -- the variables are required on host, the parent statements of these bodies
    -- cannot be moved to device as a whole. They are host-only.
    bodyHostOnlyParents :: IS.IntSet
  }

instance Semigroup BodyStats where
  (BodyStats ho1 gb1 r1 o1 hop1) <> (BodyStats ho2 gb2 r2 o2 hop2) =
    BodyStats
      { bodyHostOnly = ho1 || ho2,
        bodyHasGPUBody = gb1 || gb2,
        bodyReads = r1 || r2,
        bodyOperands = IS.union o1 o2,
        bodyHostOnlyParents = IS.union hop1 hop2
      }

instance Monoid BodyStats where
  mempty =
    BodyStats
      { bodyHostOnly = False,
        bodyHasGPUBody = False,
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

-- | Array variables backed by memory segments that may be copied, mapped to the
-- outermost known body depths that declares arrays backed by a superset of
-- those segments.
type CopyableMemoryMap = IM.IntMap BodyDepth

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
    -- | A map of encountered arrays that are backed by copyable memory.
    -- Trivial instances such as single element arrays are excluded.
    stateCopyableMemory :: CopyableMemoryMap,
    -- | Information about the current body being graphed.
    stateStats :: BodyStats
  }

--------------------------------------------------------------------------------
--                             GRAPHER OPERATIONS                             --
--------------------------------------------------------------------------------

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
          stateCopyableMemory = IM.empty,
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
-- parent statement and any parent bodies themselves are host-only. A host-only
-- statement should not be migrated, either because it cannot run on device or
-- because it would be inefficient to do so.
tellHostOnly :: Grapher ()
tellHostOnly =
  modify $ \st -> st {stateStats = (stateStats st) {bodyHostOnly = True}}

-- | Register that the body contains a GPUBody kernel.
tellGPUBody :: Grapher ()
tellGPUBody =
  modify $ \st -> st {stateStats = (stateStats st) {bodyHasGPUBody = True}}

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
tellHostOnlyParent :: BodyDepth -> Grapher ()
tellHostOnlyParent body_depth =
  modify $ \st ->
    let stats = stateStats st
        parents = bodyHostOnlyParents stats
        parents' = IS.insert body_depth parents
     in st {stateStats = stats {bodyHostOnlyParents = parents'}}

-- | Get the graph under construction.
getGraph :: Grapher Graph
getGraph = gets stateGraph

-- | All scalar variables with a vertex representation in the graph.
getGraphedScalars :: Grapher IdSet
getGraphedScalars = gets stateGraphedScalars

-- | Every known array that is backed by a memory segment that may be copied,
-- mapped to the outermost known body depth where an array is backed by a
-- superset of that segment.
--
-- A body where all returned arrays are backed by such memory and are written by
-- its own statements will retain its asymptotic cost if migrated as a whole.
getCopyableMemory :: Grapher CopyableMemoryMap
getCopyableMemory = gets stateCopyableMemory

-- | The outermost known body depth for an array backed by the same copyable
-- memory as the array with this name.
outermostCopyableArray :: VName -> Grapher (Maybe BodyDepth)
outermostCopyableArray n = IM.lookup (nameToId n) <$> getCopyableMemory

-- | Reduces the variables to just the 'Id's of those that are scalars and which
-- have a vertex representation in the graph, excluding those that have been
-- connected to sinks.
onlyGraphedScalars :: (Foldable t) => t VName -> Grapher IdSet
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

-- | Like 'onlyGraphedScalars' but for a single t'SubExp'.
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

-- | Update the contents of the copyable memory map.
modifyCopyableMemory :: (CopyableMemoryMap -> CopyableMemoryMap) -> Grapher ()
modifyCopyableMemory f =
  modify $ \st -> st {stateCopyableMemory = f (stateCopyableMemory st)}

-- | Update the set of source connected vertices.
modifySources :: (Sources -> Sources) -> Grapher ()
modifySources f =
  modify $ \st -> st {stateSources = f (stateSources st)}

-- | Record that this variable binds an array that is backed by copyable
-- memory shared by an array at this outermost body depth.
recordCopyableMemory :: Id -> BodyDepth -> Grapher ()
recordCopyableMemory i bd =
  modifyCopyableMemory (IM.insert i bd)

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
getBodyDepth :: Grapher BodyDepth
getBodyDepth = asks (metaBodyDepth . envMeta)
