{-# LANGUAGE OverloadedStrings #-}

-- | Multicore imperative code.
module Futhark.CodeGen.ImpCode.Multicore
  ( Program,
    Multicore (..),
    MCCode,
    Scheduling (..),
    SchedulerInfo (..),
    AtomicOp (..),
    ParallelTask (..),
    module Futhark.CodeGen.ImpCode,
  )
where

import Futhark.CodeGen.ImpCode
import Futhark.Util.Pretty

-- | An imperative multicore program.
type Program = Functions Multicore

-- | A multicore operation.
data Multicore
  = SegOp String [Param] ParallelTask (Maybe ParallelTask) [Param] SchedulerInfo
  | ParLoop String (Code Multicore) [Param]
  | -- | Retrieve inclusive start and exclusive end indexes of the
    -- chunk we are supposed to be executing.  Only valid immediately
    -- inside a 'ParLoop' construct!
    GetLoopBounds VName VName
  | -- | Retrieve the task ID that is currently executing.  Only valid
    -- immediately inside a 'ParLoop' construct!
    GetTaskId VName
  | -- | Retrieve the number of subtasks to execute.  Only valid
    -- immediately inside a 'SegOp' or 'ParLoop' construct!
    GetNumTasks VName
  | Atomic AtomicOp

-- | Multicore code.
type MCCode = Code Multicore

-- | Atomic operations return the value stored before the update.
-- This old value is stored in the first 'VName'.  The second 'VName'
-- is the memory block to update.  The 'Exp' is the new value.
data AtomicOp
  = AtomicAdd IntType VName VName (Count Elements (TExp Int32)) Exp
  | AtomicSub IntType VName VName (Count Elements (TExp Int32)) Exp
  | AtomicAnd IntType VName VName (Count Elements (TExp Int32)) Exp
  | AtomicOr IntType VName VName (Count Elements (TExp Int32)) Exp
  | AtomicXor IntType VName VName (Count Elements (TExp Int32)) Exp
  | AtomicXchg PrimType VName VName (Count Elements (TExp Int32)) Exp
  | AtomicCmpXchg PrimType VName VName (Count Elements (TExp Int32)) VName Exp
  deriving (Show)

instance FreeIn AtomicOp where
  freeIn' (AtomicAdd _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicSub _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicAnd _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicOr _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicXor _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicCmpXchg _ _ arr i retval x) = freeIn' arr <> freeIn' i <> freeIn' x <> freeIn' retval
  freeIn' (AtomicXchg _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x

-- | Information about parallel work that is do be done.  This is
-- passed to the scheduler to help it make scheduling decisions.
data SchedulerInfo = SchedulerInfo
  { -- | The number of total iterations for a task.
    iterations :: Exp,
    -- | The type scheduling for the task.
    scheduling :: Scheduling
  }

-- | A task for a v'SegOp'.
newtype ParallelTask = ParallelTask (Code Multicore)

-- | Whether the Scheduler should schedule the tasks as Dynamic
-- or it is restainted to Static
data Scheduling
  = Dynamic
  | Static

instance Pretty Scheduling where
  ppr Dynamic = "Dynamic"
  ppr Static = "Static"

instance Pretty SchedulerInfo where
  ppr (SchedulerInfo i sched) =
    stack
      [ nestedBlock "scheduling {" "}" (ppr sched),
        nestedBlock "iter {" "}" (ppr i)
      ]

instance Pretty ParallelTask where
  ppr (ParallelTask code) = ppr code

instance Pretty Multicore where
  ppr (GetLoopBounds start end) =
    ppr (start, end) <+> "<-" <+> "get_loop_bounds()"
  ppr (GetTaskId v) =
    ppr v <+> "<-" <+> "get_task_id()"
  ppr (GetNumTasks v) =
    ppr v <+> "<-" <+> "get_num_tasks()"
  ppr (SegOp s free seq_code par_code retval scheduler) =
    "SegOp" <+> text s <+> nestedBlock "{" "}" ppbody
    where
      ppbody =
        stack
          [ ppr scheduler,
            nestedBlock "free {" "}" (ppr free),
            nestedBlock "seq {" "}" (ppr seq_code),
            maybe mempty (nestedBlock "par {" "}" . ppr) par_code,
            nestedBlock "retvals {" "}" (ppr retval)
          ]
  ppr (ParLoop s body params) =
    "parloop" <+> ppr s </> nestedBlock "{" "}" ppbody
    where
      ppbody =
        stack
          [ nestedBlock "params {" "}" (ppr params),
            nestedBlock "body {" "}" (ppr body)
          ]
  ppr (Atomic _) = "AtomicOp"

instance FreeIn SchedulerInfo where
  freeIn' (SchedulerInfo iter _) = freeIn' iter

instance FreeIn ParallelTask where
  freeIn' (ParallelTask code) = freeIn' code

instance FreeIn Multicore where
  freeIn' (GetLoopBounds start end) =
    freeIn' (start, end)
  freeIn' (GetTaskId v) =
    freeIn' v
  freeIn' (GetNumTasks v) =
    freeIn' v
  freeIn' (SegOp _ _ par_code seq_code _ info) =
    freeIn' par_code <> freeIn' seq_code <> freeIn' info
  freeIn' (ParLoop _ body _) =
    freeIn' body
  freeIn' (Atomic aop) = freeIn' aop
