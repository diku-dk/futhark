module Futhark.Analysis.MigrationTable (
  -- * Analysis
  analyseProg,

  -- * Query
  MigrationTable,
  moveToDevice,
  usedOnHost,
) where

import Control.Monad.Trans.RWS.Lazy
import qualified Control.Monad.State as ST
import Control.Parallel.Strategies (parMap, rpar)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Futhark.Analysis.MigrationGraph hiding (empty)
import qualified Futhark.Analysis.MigrationGraph as MG
import Futhark.IR.GPU
import Futhark.IR.Traversals

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
-- WithAcc statements execute a GPU kernel (a HostOp) within their lambda.
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
  let mt  = analyseConsts consts
      mts = parMap rpar analyseFunDef funs
  in foldl' merge mt mts

-- | Analyses top-level constants.
analyseConsts :: Stms GPU -> MigrationTable
analyseConsts consts = analyseStms (boundByStms consts) consts

-- | Analyses a top-level function definition.
analyseFunDef :: FunDef GPU -> MigrationTable
analyseFunDef fd =
  let body  = funDefBody fd
      usage = freeIn $ bodyResult body
      stms  = bodyStms body
  in analyseStms usage stms

-- | HostUsage identifies variables that are used on host.
type HostUsage = Names

-- | Analyses statements. The 'HostUsage' set identifies which bound variables
-- that subsequently may be used on the host. All free variables such as
-- constants and function parameters are assumed to reside on host.
analyseStms :: HostUsage -> Stms GPU -> MigrationTable
analyseStms usage stms =
  let g = buildGraph usage stms
  -- TODO: Make routes
  -- TODO: Mark D
  -- TODO: Make reads conditional
  in empty -- TODO

-- Whether a statement must remain on the host, not considering the needs of
-- any child statements it may contain.
hostOnly :: Stm GPU -> Bool
hostOnly (Let _ _ (Op _)) = True
hostOnly _ = False
















buildGraph :: HostUsage -> Stms GPU -> Graph
buildGraph usage stms =
  let (g, _) = execRWS (mapM_ addStm stms) rootInfo MG.empty
  in foldr connectToSink g $ namesToIds usage

type Grapher = RWS BodyInfo StmInfo Graph

data StmInfo = StmInfo
  { -- Whether any of the graphed statements was host-only.
    canMoveToDevice :: Bool,
    -- All operands of the graphed statements.
    bodyOperands :: Set Id,
    -- All bound variables of the graphed statements.
    boundByBody :: Set Id
  }

instance Semigroup StmInfo where
  (StmInfo m1 o1 b1) <> (StmInfo m2 o2 b2) =
    StmInfo (m1 && m2) (S.union o1 o2) (S.union b1 b2)

instance Monoid StmInfo where
  mempty = StmInfo True (S.empty) (S.empty)

rootInfo :: BodyInfo
rootInfo = BodyInfo 0 Nothing

addStm :: Stm GPU -> Grapher ()
addStm _ = return ()

getGraph :: Grapher Graph
getGraph = get










-- Returns all variables that the computation of this statement directly depends
-- upon. Statement certificates are not included, and all kernel body
-- dependencies are excluded.
operands :: Stm GPU -> Set Id
operands (Let _ _ e) =
  -- TODO: May return variables that are guaranteed to be on the host.
  --       Excluding those variable is more efficient but requires a custom tree
  --       walking implementation. It is questionable whether this is worth
  --       optimizing.
  ST.execState (walkExpM operandsCollector e) S.empty

-- Operand collection monad.
type CollectState = ST.State (Set Id)

operandsCollector :: Walker GPU CollectState
operandsCollector = identityWalker
  {
    walkOnSubExp = collectSubExp,
    walkOnBody   = collectBody,
    walkOnVName  = collectVName,
    walkOnOp     = collectOp
  }

collectId :: Id -> CollectState ()
collectId i = do
  s <- ST.get
  ST.put $ S.insert i s

collectVName :: VName -> CollectState ()
collectVName = collectId . nameToId

collectSubExp :: SubExp -> CollectState () 
collectSubExp (Var v) = collectVName v
collectSubExp _       = return ()

collectType :: Type -> CollectState ()
collectType (Acc acc ispace ts _) = do
  collectVName acc
  mapM_ collectSubExp ispace
  mapM_ collectType ts
collectType (Array _ (Shape ds) _) =
  mapM_ collectSubExp ds
collectType _ =
  return ()

collectBody :: Scope GPU -> Body GPU -> CollectState ()
collectBody scope (Body _ stms _) = do
  s <- ST.get
  let s' = foldr f s stms
  let b  = S.fromAscList $ map nameToId $ M.keys scope
  ST.put (s' \\ b)
  where
    f stm s   = operands stm `S.union` (s \\ bound stm) 
    bound stm = S.fromList $ map nameToId $ patNames $ stmPat stm

collectOp :: Op GPU -> CollectState ()
collectOp (SegOp (SegMap lvl space ts _)) = do
  mapM_ collectId (namesToIds $ freeIn lvl)
  mapM_ collectSubExp (segSpaceDims space)
  mapM_ collectType ts
collectOp (SegOp (SegRed lvl space ops ts _)) = do
  mapM_ collectId (namesToIds $ freeIn lvl)
  mapM_ collectSubExp (segSpaceDims space)
  mapM_ collectSegBinOp ops
  mapM_ collectType ts
collectOp (SegOp (SegScan lvl space ops ts _)) = do
  mapM_ collectId (namesToIds $ freeIn lvl)
  mapM_ collectSubExp (segSpaceDims space)
  mapM_ collectSegBinOp ops
  mapM_ collectType ts
collectOp (SegOp (SegHist lvl space ops ts _)) = do
  mapM_ collectId (namesToIds $ freeIn lvl)
  mapM_ collectSubExp (segSpaceDims space)
  mapM_ collectHistOp ops
  mapM_ collectType ts
collectOp (GPUBody ts _) = do
  mapM_ collectType ts
collectOp op =
  mapM_ collectId (namesToIds $ freeIn op)

collectSegBinOp :: SegBinOp GPU -> CollectState ()
collectSegBinOp (SegBinOp _ _ nes (Shape ds)) = do
  mapM_ collectSubExp nes
  mapM_ collectSubExp ds

collectHistOp :: HistOp GPU -> CollectState ()
collectHistOp (HistOp w rf arrs nes (Shape ds) _) = do
  collectSubExp w
  collectSubExp rf
  mapM_ collectVName arrs
  mapM_ collectSubExp nes
  mapM_ collectSubExp ds
