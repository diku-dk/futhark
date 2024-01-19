-- | Variation of "Futhark.CodeGen.ImpCode" that contains the notion
-- of a kernel invocation.
module Futhark.CodeGen.ImpCode.GPU
  ( Program,
    HostCode,
    KernelCode,
    KernelConst (..),
    KernelConstExp,
    HostOp (..),
    KernelOp (..),
    Fence (..),
    AtomicOp (..),
    BlockDim,
    Kernel (..),
    KernelUse (..),
    module Futhark.CodeGen.ImpCode,
    module Futhark.IR.GPU.Sizes,
  )
where

import Futhark.CodeGen.ImpCode
import Futhark.IR.GPU.Sizes
import Futhark.IR.Pretty ()
import Futhark.Util.Pretty

-- | A program that calls kernels.
type Program = Definitions HostOp

-- | Host-level code that can call kernels.
type HostCode = Code HostOp

-- | Code inside a kernel.
type KernelCode = Code KernelOp

-- | A run-time constant related to kernels.
data KernelConst
  = SizeConst Name SizeClass
  | SizeMaxConst SizeClass
  deriving (Eq, Ord, Show)

-- | An expression whose variables are kernel constants.
type KernelConstExp = PrimExp KernelConst

-- | An operation that runs on the host (CPU).
data HostOp
  = CallKernel Kernel
  | GetSize VName Name SizeClass
  | CmpSizeLe VName Name SizeClass Exp
  | GetSizeMax VName SizeClass
  deriving (Show)

-- | The size of one dimension of a block.
type BlockDim = Either Exp KernelConst

-- | A generic kernel containing arbitrary kernel code.
data Kernel = Kernel
  { kernelBody :: Code KernelOp,
    -- | The host variables referenced by the kernel.
    kernelUses :: [KernelUse],
    kernelNumBlocks :: [Exp],
    kernelBlockSize :: [BlockDim],
    -- | A short descriptive and _unique_ name - should be
    -- alphanumeric and without spaces.
    kernelName :: Name,
    -- | If true, this kernel does not need to check whether we are in
    -- a failing state, as it can cope. Intuitively, it means that the
    -- kernel does not depend on any non-scalar parameters to make
    -- control flow decisions. Replication, transpose, and copy
    -- kernels are examples of this.
    kernelFailureTolerant :: Bool,
    -- | If true, multi-versioning branches will consider this kernel
    -- when considering the shared memory requirements. Set this to
    -- false for kernels that do their own checking.
    kernelCheckSharedMemory :: Bool
  }
  deriving (Show)

-- | Information about a host-level variable that is used inside this
-- kernel.  When generating the actual kernel code, this is used to
-- deduce which parameters are needed.
data KernelUse
  = ScalarUse VName PrimType
  | MemoryUse VName
  | ConstUse VName KernelConstExp
  deriving (Eq, Ord, Show)

instance Pretty KernelConst where
  pretty (SizeConst key size_class) =
    "get_size" <> parens (commasep [pretty key, pretty size_class])
  pretty (SizeMaxConst size_class) =
    "get_max_size" <> parens (pretty size_class)

instance FreeIn KernelConst where
  freeIn' SizeConst {} = mempty
  freeIn' (SizeMaxConst _) = mempty

instance Pretty KernelUse where
  pretty (ScalarUse name t) =
    oneLine $ "scalar_copy" <> parens (commasep [pretty name, pretty t])
  pretty (MemoryUse name) =
    oneLine $ "mem_copy" <> parens (commasep [pretty name])
  pretty (ConstUse name e) =
    oneLine $ "const" <> parens (commasep [pretty name, pretty e])

instance Pretty HostOp where
  pretty (GetSize dest key size_class) =
    pretty dest
      <+> "<-"
      <+> "get_size"
      <> parens (commasep [pretty key, pretty size_class])
  pretty (GetSizeMax dest size_class) =
    pretty dest <+> "<-" <+> "get_size_max" <> parens (pretty size_class)
  pretty (CmpSizeLe dest name size_class x) =
    pretty dest
      <+> "<-"
      <+> "get_size"
      <> parens (commasep [pretty name, pretty size_class])
        <+> "<"
        <+> pretty x
  pretty (CallKernel c) =
    pretty c

instance FreeIn HostOp where
  freeIn' (CallKernel c) =
    freeIn' c
  freeIn' (CmpSizeLe dest _ _ x) =
    freeIn' dest <> freeIn' x
  freeIn' (GetSizeMax dest _) =
    freeIn' dest
  freeIn' (GetSize dest _ _) =
    freeIn' dest

instance FreeIn Kernel where
  freeIn' kernel =
    freeIn'
      ( kernelBody kernel,
        kernelNumBlocks kernel,
        kernelBlockSize kernel
      )

instance Pretty Kernel where
  pretty kernel =
    "kernel"
      <+> brace
        ( "blocks"
            <+> brace (pretty $ kernelNumBlocks kernel)
            </> "tblock_size"
            <+> brace (list $ map (either pretty pretty) $ kernelBlockSize kernel)
            </> "uses"
            <+> brace (commasep $ map pretty $ kernelUses kernel)
            </> "failure_tolerant"
            <+> brace (pretty $ kernelFailureTolerant kernel)
            </> "check_shared_memory"
            <+> brace (pretty $ kernelCheckSharedMemory kernel)
            </> "body"
            <+> brace (pretty $ kernelBody kernel)
        )

-- | When we do a barrier or fence, is it at the local or global
-- level?  By the 'Ord' instance, global is greater than local.
data Fence = FenceLocal | FenceGlobal
  deriving (Show, Eq, Ord)

-- | An operation that occurs within a kernel body.
data KernelOp
  = GetBlockId VName Int
  | GetLocalId VName Int
  | GetLocalSize VName Int
  | GetLockstepWidth VName
  | Atomic Space AtomicOp
  | Barrier Fence
  | MemFence Fence
  | SharedAlloc VName (Count Bytes (TExp Int64))
  | -- | Perform a barrier and also check whether any
    -- threads have failed an assertion.  Make sure all
    -- threads would reach all 'ErrorSync's if any of them
    -- do.  A failing assertion will jump to the next
    -- following 'ErrorSync', so make sure it's not inside
    -- control flow or similar.
    ErrorSync Fence
  deriving (Show)

-- | Atomic operations return the value stored before the update.
-- This old value is stored in the first 'VName'.  The second 'VName'
-- is the memory block to update.  The 'Exp' is the new value.
data AtomicOp
  = AtomicAdd IntType VName VName (Count Elements (TExp Int64)) Exp
  | AtomicFAdd FloatType VName VName (Count Elements (TExp Int64)) Exp
  | AtomicSMax IntType VName VName (Count Elements (TExp Int64)) Exp
  | AtomicSMin IntType VName VName (Count Elements (TExp Int64)) Exp
  | AtomicUMax IntType VName VName (Count Elements (TExp Int64)) Exp
  | AtomicUMin IntType VName VName (Count Elements (TExp Int64)) Exp
  | AtomicAnd IntType VName VName (Count Elements (TExp Int64)) Exp
  | AtomicOr IntType VName VName (Count Elements (TExp Int64)) Exp
  | AtomicXor IntType VName VName (Count Elements (TExp Int64)) Exp
  | AtomicCmpXchg PrimType VName VName (Count Elements (TExp Int64)) Exp Exp
  | AtomicXchg PrimType VName VName (Count Elements (TExp Int64)) Exp
  deriving (Show)

instance FreeIn AtomicOp where
  freeIn' (AtomicAdd _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicFAdd _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicSMax _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicSMin _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicUMax _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicUMin _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicAnd _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicOr _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicXor _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x
  freeIn' (AtomicCmpXchg _ _ arr i x y) = freeIn' arr <> freeIn' i <> freeIn' x <> freeIn' y
  freeIn' (AtomicXchg _ _ arr i x) = freeIn' arr <> freeIn' i <> freeIn' x

instance Pretty KernelOp where
  pretty (GetBlockId dest i) =
    pretty dest
      <+> "<-"
      <+> "get_tblock_id"
      <> parens (pretty i)
  pretty (GetLocalId dest i) =
    pretty dest
      <+> "<-"
      <+> "get_local_id"
      <> parens (pretty i)
  pretty (GetLocalSize dest i) =
    pretty dest
      <+> "<-"
      <+> "get_local_size"
      <> parens (pretty i)
  pretty (GetLockstepWidth dest) =
    pretty dest
      <+> "<-"
      <+> "get_lockstep_width()"
  pretty (Barrier FenceLocal) =
    "local_barrier()"
  pretty (Barrier FenceGlobal) =
    "global_barrier()"
  pretty (MemFence FenceLocal) =
    "mem_fence_local()"
  pretty (MemFence FenceGlobal) =
    "mem_fence_global()"
  pretty (SharedAlloc name size) =
    pretty name <+> equals <+> "shared_alloc" <> parens (pretty size)
  pretty (ErrorSync FenceLocal) =
    "error_sync_local()"
  pretty (ErrorSync FenceGlobal) =
    "error_sync_global()"
  pretty (Atomic _ (AtomicAdd t old arr ind x)) =
    pretty old
      <+> "<-"
      <+> "atomic_add_"
      <> pretty t
      <> parens (commasep [pretty arr <> brackets (pretty ind), pretty x])
  pretty (Atomic _ (AtomicFAdd t old arr ind x)) =
    pretty old
      <+> "<-"
      <+> "atomic_fadd_"
      <> pretty t
      <> parens (commasep [pretty arr <> brackets (pretty ind), pretty x])
  pretty (Atomic _ (AtomicSMax t old arr ind x)) =
    pretty old
      <+> "<-"
      <+> "atomic_smax"
      <> pretty t
      <> parens (commasep [pretty arr <> brackets (pretty ind), pretty x])
  pretty (Atomic _ (AtomicSMin t old arr ind x)) =
    pretty old
      <+> "<-"
      <+> "atomic_smin"
      <> pretty t
      <> parens (commasep [pretty arr <> brackets (pretty ind), pretty x])
  pretty (Atomic _ (AtomicUMax t old arr ind x)) =
    pretty old
      <+> "<-"
      <+> "atomic_umax"
      <> pretty t
      <> parens (commasep [pretty arr <> brackets (pretty ind), pretty x])
  pretty (Atomic _ (AtomicUMin t old arr ind x)) =
    pretty old
      <+> "<-"
      <+> "atomic_umin"
      <> pretty t
      <> parens (commasep [pretty arr <> brackets (pretty ind), pretty x])
  pretty (Atomic _ (AtomicAnd t old arr ind x)) =
    pretty old
      <+> "<-"
      <+> "atomic_and"
      <> pretty t
      <> parens (commasep [pretty arr <> brackets (pretty ind), pretty x])
  pretty (Atomic _ (AtomicOr t old arr ind x)) =
    pretty old
      <+> "<-"
      <+> "atomic_or"
      <> pretty t
      <> parens (commasep [pretty arr <> brackets (pretty ind), pretty x])
  pretty (Atomic _ (AtomicXor t old arr ind x)) =
    pretty old
      <+> "<-"
      <+> "atomic_xor"
      <> pretty t
      <> parens (commasep [pretty arr <> brackets (pretty ind), pretty x])
  pretty (Atomic _ (AtomicCmpXchg t old arr ind x y)) =
    pretty old
      <+> "<-"
      <+> "atomic_cmp_xchg"
      <> pretty t
      <> parens (commasep [pretty arr <> brackets (pretty ind), pretty x, pretty y])
  pretty (Atomic _ (AtomicXchg t old arr ind x)) =
    pretty old
      <+> "<-"
      <+> "atomic_xchg"
      <> pretty t
      <> parens (commasep [pretty arr <> brackets (pretty ind), pretty x])

instance FreeIn KernelOp where
  freeIn' (Atomic _ op) = freeIn' op
  freeIn' _ = mempty

brace :: Doc a -> Doc a
brace body = " {" </> indent 2 body </> "}"
