{-# LANGUAGE TypeFamilies #-}

-- | General definitions for the flattening transformation.
--
-- Defines not just the core monads that are involved, but also the various
-- representations, except perhaps the ones that are completely local to another
-- module.
module Futhark.Pass.Flatten.Monad
  ( IrregularKind (..),
    IrregularRep (..),
    ResRep (..),
    DistEnv (..),
    FlattenOps (..),

    -- * Flattening monad
    FlattenM,
    FlattenState (..),
    runFlattenM,

    -- * Demands
    BuiltinFn (..),
    LiftMode (..),
    DemandFn (..),
    demandLifted,
    demandBuiltin,

    -- * Insertions
    insertRep,
    insertReps,
    insertIrregulars,
    insertIrregular,
    insertRegulars,

    -- * Various
    resVar,
    inputReps,
    segsAndElems,
  )
where

import Control.Monad
import Control.Monad.State
import Data.Bifunctor (bimap, second)
import Data.Foldable
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Futhark.IR.GPU
import Futhark.IR.SOACS (SOACS)
import Futhark.Pass.Flatten.Distribute
import Futhark.Tools
import Prelude hiding (div, rem)

-- Note [Representation of Flat Arrays]
--
-- This flattening implementation uses largely the nomenclature and
-- structure described by Cosmin Oancea. In particular, consider an
-- irregular array 'A' where
--
--   - A has 'n' segments (outermost dimension).
--
--   - A has element type 't'.
--
--   - A has a total of 'm' elements (where 'm' is divisible by 'n',
--     and may indeed be 'm').
--
-- Then A is represented by the following arrays:
--
--   - A_D : [m]t; the "data array".
--
--   - A_S : [n]i64; the "shape array" giving the number of scalar elements of each segment.
--
--   - A_F : [m]bool; the "flag array", indicating when an element begins a
--     new segment.
--
--   - A_O : [n]i64; the offset array, indicating for each segment
--     where it starts in the data (and flag) array.
--
--   - A_II1 : [m]t; the "segment indices"; a mapping from element
--     index to index of the segment it belongs to.
--
--   - A_II2 : [m]t; the "inner indices"; a mapping from element index
--     to index within its corresponding segment.
--
-- The arrays that are not the data array are collectively called the
-- "structure arrays". All of the structure arrays can be computed
-- from each other, but conceptually they all coexist.
--
-- Note that we only consider the *outer* dimension to be the
-- "segments". Also, 't' may actually be an array itself (although in
-- this case, the shape of 't' must be invariant to all parallel
-- dimensions). The inner structure is preserved through code, not
-- data. (Or in practice, ad-hoc auxiliary arrays produced by code.)
-- In Cosmin's notation, we maintain only the information for the
-- outermost dimension.
--
-- As an example, consider an irregular array
--
--   A = [ [], [ [1,2,3], [4], [], [5,6] ], [ [7], [], [8,9,10] ] ]
--
-- then
--
--   n = 3
--
--   m = 10
--
--   A_D = [1,2,3,4,5,6,7,8,9,10]
--
--   A_S = [0, 6, 4]
--
--   A_F = [T,F,F,F,F,F,T,F,F,F]
--
--   A_O = [0, 0, 6]
--
--   A_II1 = [1,1,1,1,1,1,2,2,2,2]
--
--   A_II2 = [0,0,0,1,3,3,0,2,2,2]

data IrregularKind
  = Dense
  | Replicated
  deriving (Show, Eq)

data IrregularRep = IrregularRep
  { -- | Array of size of each segment, type @[]i64@.
    irregularS :: VName,
    irregularF :: VName,
    irregularO :: VName,
    irregularD :: VName,
    irregularK :: IrregularKind
  }
  deriving (Show)

data ResRep
  = -- | This variable is represented completely straightforwardly- if it is an
    -- array, it is a regular array.
    Regular VName
  | -- | The representation of an irregular array.
    Irregular IrregularRep
  deriving (Show)

newtype DistEnv = DistEnv {distResMap :: M.Map ResTag ResRep}

insertRep :: ResTag -> ResRep -> DistEnv -> DistEnv
insertRep rt rep env = env {distResMap = M.insert rt rep $ distResMap env}

insertReps :: [(ResTag, ResRep)] -> DistEnv -> DistEnv
insertReps = flip $ foldl (flip $ uncurry insertRep)

insertIrregular :: VName -> VName -> VName -> ResTag -> VName -> IrregularKind -> DistEnv -> DistEnv
insertIrregular ns flags offsets rt elems kind env =
  let rep = Irregular $ IrregularRep ns flags offsets elems kind
   in insertRep rt rep env

insertIrregulars :: VName -> VName -> VName -> [(ResTag, VName)] -> IrregularKind -> DistEnv -> DistEnv
insertIrregulars ns flags offsets bnds kind env =
  let (tags, elems) = unzip bnds
      mkRep elem_arr =
        Irregular $
          IrregularRep
            { irregularS = ns,
              irregularF = flags,
              irregularO = offsets,
              irregularD = elem_arr,
              irregularK = kind
            }
   in insertReps (zip tags $ map mkRep elems) env

insertRegulars :: [ResTag] -> [VName] -> DistEnv -> DistEnv
insertRegulars rts xs =
  insertReps (zip rts $ map Regular xs)

instance Monoid DistEnv where
  mempty = DistEnv mempty

instance Semigroup DistEnv where
  DistEnv x <> DistEnv y = DistEnv (x <> y)

resVar :: ResTag -> DistEnv -> ResRep
resVar rt env = fromMaybe bad $ M.lookup rt $ distResMap env
  where
    bad = error $ "resVar: unknown tag: " ++ show rt

segsAndElems :: DistEnv -> [DistInput] -> (Maybe (VName, VName, VName), [VName])
segsAndElems _ [] = (Nothing, [])
segsAndElems env (DistInputFree v _ : vs) =
  second (v :) $ segsAndElems env vs
segsAndElems env (DistInput rt _ : vs) =
  case resVar rt env of
    Regular v' ->
      second (v' :) $ segsAndElems env vs
    Irregular (IrregularRep segments flags offsets elems k) -> do
      case k of
        Dense -> do
          bimap (mplus $ Just (segments, flags, offsets)) (elems :) $ segsAndElems env vs
        Replicated ->
          second (flags :) $ segsAndElems env vs

-- | Mapping from original variable names to their distributed resreps.
inputReps :: DistInputs -> DistEnv -> M.Map VName (Type, ResRep)
inputReps inputs env = M.fromList $ map (second getRep) inputs
  where
    getRep di = case di of
      DistInput rt t -> (t, resVar rt env)
      DistInputFree v' t -> (t, Regular v')

-- | A representation of the different kinds of builtin functions we can
-- generate. This is used to only generate the ones we actually need for a given
-- program.
data BuiltinFn
  = BuiltinSegIota
  | BuiltinRepIota
  | BuiltinPrefixSum
  | BuiltinPartition
  deriving (Eq, Ord, Show)

data LiftMode
  = UniformLift
  | NonUniformLift
  deriving (Eq, Ord, Show)

-- | Indicate the need for a function to be generated. Instead of immediately
-- generating them ourselves, we collect requirements from multiple flattening
-- operations and satisfy them in their entirety.
data DemandFn
  = -- | We need this function to be lifted.
    DemandLifted Name LiftMode
  | DemandBuiltin BuiltinFn
  deriving (Eq, Ord, Show)

data FlattenState = FlattenState
  { -- In order to generate more stable threshold names, we keep track of the
    -- numbers used for thresholds separately from the ordinary name source.
    stateThresholdCounter :: Int,
    stateNameSource :: VNameSource,
    -- A set of those functions that we have emitted calls to, and which will
    -- need to be generated.
    stateDemandFns :: S.Set DemandFn
  }

newtype FlattenM a = FlattenM (BuilderT GPU (State FlattenState) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      LocalScope GPU,
      HasScope GPU,
      MonadState FlattenState,
      MonadFreshNames
    )

instance MonadBuilder FlattenM where
  type Rep FlattenM = GPU
  mkExpDecM pat e = FlattenM $ mkExpDecM pat e
  mkBodyM stms res = FlattenM $ mkBodyM stms res
  mkLetNamesM pat e = FlattenM $ mkLetNamesM pat e

  addStms = FlattenM . addStms
  collectStms (FlattenM m) = FlattenM $ collectStms m

instance MonadFreshNames (State FlattenState) where
  getNameSource = gets stateNameSource
  putNameSource src = modify $ \s -> s {stateNameSource = src}

-- | Do not nest these - the counter for thresholds will be wrong.
runFlattenM :: (MonadFreshNames m) => Scope GPU -> FlattenM a -> m (a, S.Set DemandFn)
runFlattenM scope (FlattenM m) = modifyNameSource $ \src ->
  let initial_state =
        FlattenState
          { stateThresholdCounter = 0,
            stateNameSource = src,
            stateDemandFns = mempty
          }
      (x, s) = runState (fst <$> runBuilderT m scope) initial_state
   in ((x, stateDemandFns s), stateNameSource s)

-- | Indicate that we rather need a lifted version of this function.
demandLifted :: Name -> LiftMode -> FlattenM ()
demandLifted fname mode = modify $ \s ->
  s {stateDemandFns = S.insert (DemandLifted fname mode) $ stateDemandFns s}

-- | Demand the presence of this builtin function.
demandBuiltin :: BuiltinFn -> FlattenM ()
demandBuiltin b = modify $ \s ->
  s {stateDemandFns = S.insert (DemandBuiltin b) $ stateDemandFns s}

-- | Functions for tying together disparate modules - this is to avoid mutually
-- recursive modules.
data FlattenOps = FlattenOps
  { flattenSegLevel :: SegLevel,
    flattenFunHasParallelism :: FunHasParallelism,
    flattenDistStmAtLevel :: SegLevel -> Segments -> DistEnv -> DistStm -> FlattenM DistEnv,
    flattenScalarStm :: Segments -> DistEnv -> DistInputs -> [DistResult] -> Stm SOACS -> FlattenM DistEnv,
    -- | Transform a statement as if it occurred at the top level, including
    -- multi-versioning of SOACs. Used when a transformation (e.g. loop
    -- interchange) produces a statement that should be treated as if the
    -- program had looked like that all along.
    flattenTopLevelStm :: Stm SOACS -> FlattenM ()
  }
