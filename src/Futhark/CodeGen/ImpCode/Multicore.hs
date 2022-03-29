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
    ForEach VName Exp MCCode
  | -- | ForEach_Active, only valid in ISPC
    ForEachActive VName MCCode
  | -- | Extract a lane to a uniform in ISPC
    ExtractLane VName Exp Exp
  | -- | Specifies the variability of all declarations within this scope
    VariabilityBlock Variability MCCode
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
  ppr (VariabilityBlock qual code) =
    nestedBlock (show qual <> " {") "}" (ppr code)
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
    fvBind (oneName i) (freeIn' body <> freeIn' bound)
  freeIn' (ForEachActive i body) =
    fvBind (oneName i) (freeIn' body)
  freeIn' (VariabilityBlock _ code) =
    freeIn' code
  freeIn' (ExtractLane dest tar lane) =
    freeIn' dest <> freeIn' tar <> freeIn' lane

-- TODO(pema): This is a bit hacky
-- Like @lexicalMemoryUsage@, but traverses inner ops
lexicalMemoryUsageMC :: Function Multicore -> M.Map VName Space
lexicalMemoryUsageMC func =
  M.filterWithKey (const . not . (`nameIn` nonlexical)) $
    declared $ functionBody func
  where
    nonlexical =
      set (functionBody func)
        <> namesFromList (map paramName (functionOutput func))

    go _ f (x :>>: y) = f x <> f y
    go _ f (If _ x y) = f x <> f y
    go _ f (For _ _ x) = f x
    go _ f (While _ x) = f x
    go _ f (Comment _ x) = f x
    go opt f (Op op) = opt f op
    go _ _ _ = mempty

    -- We want nested SetMem's to be visible so we don't erroneously
    -- treat a memblock that needs refcounting as lexical
    -- goOpSet f (ISPCKernel code _) = go goOpSet f code
    goOpSet f (ForEach _ _ body) = go goOpSet f body
    goOpSet f (ForEachActive _ body) = go goOpSet f body
    goOpSet f (VariabilityBlock _ code) = go goOpSet f code
    goOpSet _ _ = mempty

    -- We want nested declarations to NOT be visible so we don't
    -- declare the same memory multiple times in different scopes.
    goOpDeclare f (ForEach _ _ body) = go goOpDeclare f body
    goOpDeclare f (ForEachActive _ body) = go goOpDeclare f body
    goOpDeclare f (VariabilityBlock _ code) = go goOpDeclare f code
    goOpDeclare _ _ = mempty

    declared (DeclareMem mem spc) =
      M.singleton mem spc
    declared x = go goOpDeclare declared x

    set (SetMem x y _) = namesFromList [x, y]
    set (Call _ _ args) = foldMap onArg args
      where
        onArg ExpArg {} = mempty
        onArg (MemArg x) = oneName x
    set (Op (ISPCKernel _ args)) = namesFromList $ map paramName args
    set x = go goOpSet set x
