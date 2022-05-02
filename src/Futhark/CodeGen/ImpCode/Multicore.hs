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
    lexicalMemoryUsageMC,
    module Futhark.CodeGen.ImpCode,
  )
where

import Futhark.CodeGen.ImpCode
import Futhark.Util.Pretty
import qualified Data.Map as M

-- | An imperative multicore program.
type Program = Functions Multicore

-- | A multicore operation.
data Multicore
  = SegOp String [Param] ParallelTask (Maybe ParallelTask) [Param] SchedulerInfo
  | ParLoop String MCCode [Param]
  | -- | Emit code in ISPC
    ISPCKernel MCCode [Param]
  | -- | ForEach, only valid in ISPC
    ForEach [VName] [Exp] MCCode
  | -- | ForEach_Active, only valid in ISPC
    ForEachActive VName MCCode
  | -- | Extract a lane to a uniform in ISPC
    ExtractLane VName Exp Exp
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
newtype ParallelTask = ParallelTask MCCode

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
  ppr (Atomic _) =
    "AtomicOp"
  ppr (ISPCKernel body _) =
    "ispc" <+> nestedBlock "{" "}" (ppr body)
  ppr (ForEach i limit body) =
    "foreach" <+> ppr i <+> langle <+> ppr limit
      <+> nestedBlock "{" "}" (ppr body)
  ppr (ForEachActive i body) =
    "foreach_active" <+> ppr i
      <+> nestedBlock "{" "}" (ppr body)
  ppr (ExtractLane dest tar lane) =
    ppr dest <+> "<-" <+> "extract" <+> parens (commasep $ map ppr [tar, lane])

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
  freeIn' (Atomic aop) =
    freeIn' aop
  freeIn' (ISPCKernel body _) =
    freeIn' body
  freeIn' (ForEach i bound body) =
    fvBind (namesFromList i) (freeIn' body <> freeIn' bound)
  freeIn' (ForEachActive i body) =
    fvBind (oneName i) (freeIn' body)
  freeIn' (ExtractLane dest tar lane) =
    freeIn' dest <> freeIn' tar <> freeIn' lane

-- TODO(pema): We should probably make something like this but
-- for the (non-ispc) multicore backend, which _does_ look into
-- kernels (since they will be lexical). As it is currently, we
-- treat many memory blocks which _could_ be lexical in the normal
-- multicore backend as non-lexical. Also, the issue with safety tests.
-- | Like @lexicalMemoryUsage@, but traverses inner multicore ops
lexicalMemoryUsageMC :: Function Multicore -> M.Map VName Space
lexicalMemoryUsageMC func =
  M.filterWithKey (const . not . (`nameIn` nonlexical)) $
    declared $ functionBody func
  where
    nonlexical =
      set (functionBody func)
        <> namesFromList (map paramName (functionOutput func))

    go f (x :>>: y) = f x <> f y
    go f (If _ x y) = f x <> f y
    go f (For _ _ x) = f x
    go f (While _ x) = f x
    go f (Comment _ x) = f x
    go f (Op op) = goOp f op
    go _ _ = mempty

    -- We want SetMems and declarations to be visible through custom control flow
    -- so we don't erroneously treat a memblock that could be lexical as needing
    -- refcounting. Importantly, we do not look into kernels, though, since they
    -- go into new functions.
    goOp f (ForEach _ _ body) = go f body
    goOp f (ForEachActive _ body) = go f body
    goOp _ _ = mempty

    declared (DeclareMem mem spc) =
      M.singleton mem spc
    declared x = go declared x

    set (SetMem x y _) = namesFromList [x, y]
    set (Call _ _ args) = foldMap onArg args
      where
        onArg ExpArg {} = mempty
        onArg (MemArg x) = oneName x
    set x = go set x