{-# LANGUAGE OverloadedStrings #-}

-- | Multicore imperative code.
module Futhark.CodeGen.ImpCode.Multicore
  ( Program,
    Function,
    FunctionT (Function),
    Code,
    Multicore (..),
    Scheduling (..),
    SchedulerInfo (..),
    AtomicOp (..),
    ParallelTask (..),
    module Futhark.CodeGen.ImpCode,
  )
where

import Futhark.CodeGen.ImpCode hiding (Code, Function)
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.Util.Pretty

-- | An imperative program.
type Program = Imp.Functions Multicore

-- | An imperative function.
type Function = Imp.Function Multicore

-- | A piece of imperative code, with multicore operations inside.
type Code = Imp.Code Multicore

-- | A multicore operation.
data Multicore
  = SegOp String [Param] ParallelTask (Maybe ParallelTask) [Param] SchedulerInfo
  | ParLoop String VName Code Code Code [Param] VName
  | Atomic AtomicOp

-- | Atomic operations return the value stored before the update.
-- This old value is stored in the first 'VName'.  The second 'VName'
-- is the memory block to update.  The 'Exp' is the new value.
data AtomicOp
  = AtomicAdd IntType VName VName (Count Elements (Imp.TExp Int32)) Exp
  | AtomicSub IntType VName VName (Count Elements (Imp.TExp Int32)) Exp
  | AtomicAnd IntType VName VName (Count Elements (Imp.TExp Int32)) Exp
  | AtomicOr IntType VName VName (Count Elements (Imp.TExp Int32)) Exp
  | AtomicXor IntType VName VName (Count Elements (Imp.TExp Int32)) Exp
  | AtomicXchg PrimType VName VName (Count Elements (Imp.TExp Int32)) Exp
  | AtomicCmpXchg PrimType VName VName (Count Elements (Imp.TExp Int32)) VName Exp
  deriving (Show)

instance FreeIn AtomicOp where
  freeIn' (AtomicAdd _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicSub _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicAnd _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicOr _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicXor _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicCmpXchg _ _ arr i retval x) = freeIn' arr <> freeIn' i <> freeIn' x <> freeIn' retval
  freeIn' (AtomicXchg _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x

data SchedulerInfo = SchedulerInfo
  { nsubtasks :: VName, -- The variable that describes how many subtasks the scheduler created
    iterations :: Imp.Exp, -- The number of total iterations for a task
    scheduling :: Scheduling -- The type scheduling for the task
  }

data ParallelTask = ParallelTask
  { task_code :: Code,
    flatTid :: VName -- The variable for the thread id execution the code
  }

-- | Whether the Scheduler should schedule the tasks as Dynamic
-- or it is restainted to Static
data Scheduling
  = Dynamic
  | Static

instance Pretty Scheduling where
  ppr Dynamic = "Dynamic"
  ppr Static = "Static"

instance Pretty SchedulerInfo where
  ppr (SchedulerInfo nsubtask i sched) =
    stack
      [ nestedBlock "number of subtasks {" "}" (ppr nsubtask),
        nestedBlock "scheduling {" "}" (ppr sched),
        nestedBlock "iter {" "}" (ppr i)
      ]

instance Pretty ParallelTask where
  ppr (ParallelTask code tid) =
    "\\" <> ppr tid <+> "->" </> ppr code

instance Pretty Multicore where
  ppr (SegOp s free par_code seq_code retval scheduler) =
    "SegOp" <+> text s <+> nestedBlock "{" "}" ppbody
    where
      ppbody =
        stack
          [ ppr scheduler,
            nestedBlock "free {" "}" (ppr free),
            nestedBlock "par {" "}" (ppr par_code),
            maybe mempty (nestedBlock "seq {" "}" . ppr) seq_code,
            nestedBlock "retvals {" "}" (ppr retval)
          ]
  ppr (ParLoop s i prebody body postbody params info) =
    "parloop" <+> ppr s <+> ppr i <+> nestedBlock "{" "}" ppbody
    where
      ppbody =
        stack
          [ nestedBlock "prebody {" "}" (ppr prebody),
            nestedBlock "params {" "}" (ppr params),
            ppr info,
            nestedBlock "body {" "}" (ppr body),
            nestedBlock "postbody {" "}" (ppr postbody)
          ]
  ppr (Atomic _) = "AtomicOp"

instance FreeIn SchedulerInfo where
  freeIn' (SchedulerInfo nsubtask iter _) =
    freeIn' iter <> freeIn' nsubtask

instance FreeIn ParallelTask where
  freeIn' (ParallelTask code _) =
    freeIn' code

instance FreeIn Multicore where
  freeIn' (SegOp _ _ par_code seq_code _ info) =
    freeIn' par_code <> freeIn' seq_code <> freeIn' info
  freeIn' (ParLoop _ _ prebody body postbody _ _) =
    freeIn' prebody <> fvBind (Imp.declaredIn prebody) (freeIn' $ body <> postbody)
  freeIn' (Atomic aop) = freeIn' aop
