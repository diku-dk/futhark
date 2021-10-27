module Futhark.Analysis.MigrationTable (
  -- * Analysis
  analyseProg,

  -- * Query
  MigrationTable,
  moveToDevice,
  usedOnHost,
) where

import Control.Monad.Writer.Lazy as W
import Control.Monad.Trans.RWS.Lazy
import Control.Parallel.Strategies (parMap, rpar)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Futhark.Analysis.MigrationGraph hiding (empty, addEdges)
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

-- | Identifies top-level function definitions that cannot be run on the
-- device. The application of any such function is host-only.
type HostOnlyFuns = Set Name

-- | HostUsage identifies scalar variables that are used on host.
type HostUsage = [Id]

-- | Analyses statements. The 'HostUsage' list identifies which bound scalar
-- variables that subsequently may be used on host. All free variables such as
-- constants and function parameters are assumed to reside on host.
analyseStms :: HostOnlyFuns -> HostUsage -> Stms GPU -> MigrationTable
analyseStms hof usage stms =
  let g = buildGraph hof usage stms
  -- TODO: Make routes
  -- TODO: Mark D
  -- TODO: Make reads conditional
  in empty -- TODO

buildGraph :: HostOnlyFuns -> HostUsage -> Stms GPU -> Graph
buildGraph hof usage stms =
  let g = execGrapher hof (graphStms stms)
  in foldl' (flip MG.connectToSink) g usage

type Grapher = RWS (HostOnlyFuns, BodyInfo) StmInfo State

data State = State {
    stateGraph :: Graph,
    stateGraphedScalars :: IdSet,
    stateSources :: Sources,
    stateHostOnlyAccOps :: Set VName
  }

type Sources = ([Id], [Id]) -- Unrouted sources, routed sources

execGrapher :: HostOnlyFuns -> Grapher a -> Graph
execGrapher hof g = stateGraph . fst $ execRWS g env st
  where
    env      = (hof, rootInfo)
    st       = State MG.empty IS.empty ([], []) S.empty
    rootInfo = BodyInfo 0 Nothing

-- | Get the graph under construction.
getGraph :: Grapher Graph
getGraph = gets stateGraph

-- Update graph under construction.
modifyGraph :: (Graph -> Graph) -> Grapher ()
modifyGraph f =
  modify $ \s -> s { stateGraph = f (stateGraph s) }

-- All scalar variables that have been added to the graph so far.
getGraphedScalars :: Grapher IdSet
getGraphedScalars = gets stateGraphedScalars

-- Update the contents of the graphed scalar set.
modifyGraphedScalars :: (IdSet -> IdSet) -> Grapher ()
modifyGraphedScalars f =
  modify $ \s -> s { stateGraphedScalars = f (stateGraphedScalars s) }

-- All source connected vertices that have been added to the graph so far.
getSources :: Grapher Sources
getSources = gets stateSources

-- Update the set source connected vertices.
modifySources :: (Sources -> Sources) -> Grapher ()
modifySources f =
  modify $ \s -> s { stateSources = f (stateSources s) }

-- Is an accumulator associated with an operator that only can execute on host?
isAccOpHostOnly :: VName -> Grapher Bool
isAccOpHostOnly a = gets $ (S.member a) . stateHostOnlyAccOps

-- Get the 'BodyInfo' corresponding to the current body.
getBodyInfo :: Grapher BodyInfo
getBodyInfo = asks $ snd

-- Can applications of this function be moved to device?
isHostOnlyFun :: Name -> Grapher Bool
isHostOnlyFun n = asks $ S.member n . fst

-- Whether a statement must remain on the host, not considering the needs of
-- any child statements it may contain.
isHostOnly :: Stm GPU -> Grapher Bool
isHostOnly (Let _ _ (Op _))          = return True
isHostOnly (Let _ _ (Apply n _ _ _)) = isHostOnlyFun n
isHostOnly _                         = return False

graphStms :: Stms GPU -> Grapher ()
graphStms = mapM_ graphStm

graphStm :: Stm GPU -> Grapher ()
graphStm stm = do
  let b = boundBy stm
  let e = stmExp stm 
  ho <- isHostOnly stm
  if ho then graphHostOnly e else case e of
    BasicOp (Assert {})    -> graphAssert    (one b) e
    BasicOp (Index {})     -> graphRead      (one b) e
    BasicOp (UpdateAcc {}) -> graphUpdateAcc (one b) e
    BasicOp _              -> graphSimple         b  e
    Apply {}               -> graphSimple         b  e
    If {}                  -> graphIf             b  e
    DoLoop {}              -> graphLoop           b  e
    WithAcc {}             -> graphWithAcc        b  e
    Op _                   -> graphHostOnly          e
  where
    one [x] = x
    one _   = error "type error: unexpected number of pattern elements"

type Bound = [(Id, Type)]

boundBy :: Stm GPU -> Bound
boundBy (Let (Pat pes) _ _) = map f pes
  where
    f (PatElem v dec) = (nameToId v, typeOf dec)

graphHostOnly :: Exp GPU -> Grapher ()
graphHostOnly e = do
  -- Connect the vertices of all operands to sinks to mark that they are
  -- required on host. Transitive reads that they depend upon can be delayed
  -- no further.
  ops <- graphedScalarOperands e
  addEdges MG.ToSink ops
  modifyGraphedScalars (\s -> s `IS.difference` ops)

graphAssert :: (Id, Type) -> Exp GPU -> Grapher ()
graphAssert b e = do
  -- In the worst case, each assertion moved to device costs an extra read
  -- during next (read) synchronization. To ensure that the number of reads
  -- does not increase by moving assertions, each assertion must in itself
  -- delay one read. By connecting assertion certificates to sinks, a set of
  -- assertions will only be moved to device if they delay a greater number
  -- of reads.
  graphSimple [b] e
  modifyGraph (MG.connectToSink $ fst b)

graphRead :: (Id, Type) -> Exp GPU -> Grapher ()
graphRead (i, t) e = do
  ops <- graphedScalarOperands e
  addSource i t
  addEdges (MG.declareEdges [i]) ops

graphUpdateAcc :: (Id, Type) -> Exp GPU -> Grapher ()
graphUpdateAcc b e | (_, Acc a _ _ _) <- b = do
  ho <- isAccOpHostOnly a
  if ho then graphHostOnly e -- op is host-only => UpdateAcc is host-only
        else graphRead b e   -- ensure UpdateAcc is moved to device
graphUpdateAcc _ _ = error
  "type error: UpdateAcc did not produce accumulator typed value"

graphSimple :: Bound -> Exp GPU -> Grapher ()
graphSimple b e = do
  -- Only add vertices to the graph if they have a transitive dependency to
  -- an array read. Transitive dependencies through variables required on host
  -- (those that are connected to sinks) do not count.
  ops <- graphedScalarOperands e
  if IS.null ops then return ()
  else do
    forM_ b (uncurry addVertex)
    addEdges (MG.declareEdges $ fst $ unzip b) ops

graphIf :: Bound -> Exp GPU -> Grapher ()
graphIf b e = return () -- TODO

graphLoop :: Bound -> Exp GPU -> Grapher ()
graphLoop b e = return () -- TODO

graphWithAcc :: Bound -> Exp GPU -> Grapher ()
graphWithAcc b e = return () -- TODO













data StmInfo = StmInfo
  { -- Whether any of the graphed statements was host-only.
    canMoveToDevice :: Bool,
    -- All operands of the graphed statements.
    bodyOperands :: IdSet,
    -- All bound variables of the graphed statements.
    boundByBody :: IdSet
  }

instance Semigroup StmInfo where
  (StmInfo m1 o1 b1) <> (StmInfo m2 o2 b2) =
    StmInfo (m1 && m2) (IS.union o1 o2) (IS.union b1 b2)

instance Monoid StmInfo where
  mempty = StmInfo True (IS.empty) (IS.empty)







-- | Adds a vertex to the graph for the given 'Id'.
addVertex :: Id -> Type -> Grapher ()
addVertex i t = do
  bi <- getBodyInfo
  let v = MG.vertex i bi
  when (isScalarType t) $ modifyGraphedScalars (IS.insert i)
  modifyGraph (MG.insert v)

-- | Adds a source connected vertex to the graph for the given 'Id'.
addSource :: Id -> Type -> Grapher ()
addSource i t = do
  addVertex i t
  modifySources $ \(r, u) -> (r, i:u)

-- | Adds the given edges to each vertex identified by the 'IdSet'.
addEdges :: Edges -> IdSet -> Grapher ()
addEdges es is =
  modifyGraph $ \g -> IS.foldl' (flip $ MG.addEdges es) g is

-- Reduces the variables to just the 'Id's of those that are scalars and which
-- have been graphed.
onlyGraphedScalars :: Foldable t => t VName -> Grapher IdSet
onlyGraphedScalars vs = do
  let is = foldl' (\s v -> IS.insert (nameToId v) s) IS.empty vs
  gss <- getGraphedScalars
  return (IS.intersection is gss)

-- Returns all non-kernel scalar operands that previously have been graphed,
-- minus those that have been connected to sinks. That is all operands with
-- statements that can be moved to device to potentially avoid a read.
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