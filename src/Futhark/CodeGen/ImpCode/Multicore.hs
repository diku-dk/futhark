-- | Multicore imperative code.
module Futhark.CodeGen.ImpCode.Multicore
       ( Program
       , Function
       , FunctionT (Function)
       , Code
       , Multicore(..)
       , Scheduling(..)
       , SchedulerInfo(..)
       , AtomicOp(..)
       , module Futhark.CodeGen.ImpCode
       )
       where

import Futhark.CodeGen.ImpCode hiding (Function, Code)
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.Util.Pretty

-- | An imperative program.
type Program = Imp.Functions Multicore

-- | An imperative function.
type Function = Imp.Function Multicore

-- | A piece of imperative code, with multicore operations inside.
type Code = Imp.Code Multicore


-- | A multicore operation.
data Multicore = Task String [Param] Code (Maybe Code) [Param] SchedulerInfo
               | MCFunc String VName Code Code [Param] VName
               | MulticoreCall (Maybe VName) String
               | Atomic AtomicOp


-- | Atomic operations return the value stored before the update.
-- This old value is stored in the first 'VName'.  The second 'VName'
-- is the memory block to update.  The 'Exp' is the new value.
data AtomicOp = AtomicAdd IntType VName VName (Count Elements Imp.Exp) Exp
              | AtomicSub IntType VName VName (Count Elements Imp.Exp) Exp
              | AtomicAnd IntType VName VName (Count Elements Imp.Exp) Exp
              | AtomicOr IntType VName VName (Count Elements Imp.Exp) Exp
              | AtomicXor IntType VName VName (Count Elements Imp.Exp) Exp
              | AtomicCmpXchg PrimType VName VName (Count Elements Imp.Exp) VName Exp
              deriving (Show)

instance FreeIn AtomicOp where
  freeIn' (AtomicAdd _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicSub _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicAnd _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicOr _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicXor _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicCmpXchg _ _ arr i retval x) = freeIn' arr <> freeIn' i <> freeIn' x <> freeIn' retval


type Granularity = Int

data SchedulerInfo = SchedulerInfo
  { nsubtasks  :: VName -- The variable that describes how many subtasks the scheduler created
  , flatTid    :: VName -- The variable for the tid execution the code
  , iterations :: Imp.Exp -- The number of total iterations for a task
  , scheduling :: Scheduling -- The type scheduling that the task can be performed as
  -- , nested     :: Int        --
  }

-- | Whether the Scheduler can/should schedule the tasks as Dynamic
-- or it is restainted to Static
-- This could carry more information
data Scheduling = Dynamic Granularity
                | Static Granularity

instance Pretty Scheduling where
  ppr (Dynamic granularity) =
    text "Dynamic" <+> ppr granularity
  ppr (Static _) =
    text "Static"

-- TODO fix all of this!
instance Pretty SchedulerInfo where
  ppr (SchedulerInfo nsubtask _ i sched) =
    text "SchedulingInfo" <+>
    text "number of subtasks" <+> ppr nsubtask <+>
    text "scheduling" <+> ppr sched <+>
    text "iter" <+> ppr i

instance Pretty Multicore where
  ppr (Task s free _par_code seq_code retval scheduler) =
    text "parfor" <+>
    ppr scheduler <+>
    ppr free <+>
    text s <+>
    text "seq_code" <+> nestedBlock "{" "}" (ppr seq_code) <+>
    text "retvals" <+> ppr retval

  ppr (MCFunc s i prebody body params info) =
    text "parloop" <+> ppr s <+> ppr i <+>
    ppr prebody <+>
    ppr params <+>
    ppr info <+>
    langle <+>
    nestedBlock "{" "}" (ppr body)

  ppr (MulticoreCall dests f) =
    ppr dests <+> ppr f

  ppr (Atomic _) = text "AtomicOp"

instance FreeIn SchedulerInfo where
  freeIn' (SchedulerInfo nsubtask _ iter _) =
    freeIn' iter <> freeIn' nsubtask

instance FreeIn Multicore where
  freeIn' (Task _ _ par_code seq_code _ info) =
    freeIn' par_code <> freeIn' seq_code  <> freeIn' info
  freeIn' (MCFunc _  _ prebody body _ _) =
    freeIn' prebody <> fvBind (Imp.declaredIn prebody) (freeIn' body)
  freeIn' (MulticoreCall dests _ ) = freeIn' dests
  freeIn' (Atomic aop) = freeIn' aop
