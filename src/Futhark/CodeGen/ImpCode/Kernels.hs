{-# LANGUAGE FlexibleContexts #-}
-- | Variation of "Futhark.CodeGen.ImpCode" that contains the notion
-- of a kernel invocation.
module Futhark.CodeGen.ImpCode.Kernels
  ( Program
  , Function
  , FunctionT (Function)
  , Code
  , KernelCode
  , KernelConst (..)
  , KernelConstExp
  , HostOp (..)
  , KernelOp (..)
  , AtomicOp (..)
  , CallKernel (..)
  , MapKernel (..)
  , Kernel (..)
  , LocalMemoryUse
  , KernelUse (..)
  , module Futhark.CodeGen.ImpCode
  , module Futhark.Representation.Kernels.Sizes
  -- * Utility functions
  , getKernels
  )
  where

import Control.Monad.Writer
import Data.List
import qualified Data.Set as S

import Futhark.CodeGen.ImpCode hiding (Function, Code)
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.Representation.Kernels.Sizes
import Futhark.Representation.AST.Attributes.Names
import Futhark.Representation.AST.Pretty ()
import Futhark.Util.Pretty

type Program = Functions HostOp
type Function = Imp.Function HostOp
-- | Host-level code that can call kernels.
type Code = Imp.Code CallKernel
-- | Code inside a kernel.
type KernelCode = Imp.Code KernelOp

-- | A run-time constant related to kernels.
newtype KernelConst = SizeConst VName
                    deriving (Eq, Ord, Show)

-- | An expression whose variables are kernel constants.
type KernelConstExp = PrimExp KernelConst

data HostOp = CallKernel CallKernel
            | GetSize VName VName SizeClass
            | CmpSizeLe VName VName SizeClass Imp.Exp
            | GetSizeMax VName SizeClass
            deriving (Show)

data CallKernel = Map MapKernel
                | AnyKernel Kernel
                | MapTranspose PrimType VName Exp VName Exp Exp Exp Exp Exp Exp
            deriving (Show)

-- | A generic kernel containing arbitrary kernel code.
data MapKernel = MapKernel { mapKernelThreadNum :: VName
                             -- ^ Stm position - also serves as a unique
                             -- name for the kernel.
                           , mapKernelDesc :: String
                           -- ^ Used to name the kernel for readability.
                           , mapKernelBody :: Imp.Code KernelOp
                           , mapKernelUses :: [KernelUse]
                           , mapKernelNumGroups :: DimSize
                           , mapKernelGroupSize :: DimSize
                           , mapKernelSize :: Imp.Exp
                           -- ^ Do not actually execute threads past this.
                           }
                     deriving (Show)

data Kernel = Kernel
              { kernelBody :: Imp.Code KernelOp
              , kernelLocalMemory :: [LocalMemoryUse]
              -- ^ The local memory used by this kernel.

              , kernelUses :: [KernelUse]
                -- ^ The host variables referenced by the kernel.

              , kernelNumGroups :: DimSize
              , kernelGroupSize :: DimSize
              , kernelName :: VName
                -- ^ Unique name for the kernel.
              , kernelDesc :: String
               -- ^ A short descriptive name - should be
               -- alphanumeric and without spaces.
              }
            deriving (Show)

-- ^ In-kernel name and per-workgroup size in bytes.
type LocalMemoryUse = (VName, Either MemSize KernelConstExp)

data KernelUse = ScalarUse VName PrimType
               | MemoryUse VName Imp.DimSize
               | ConstUse VName KernelConstExp
                 deriving (Eq, Show)

getKernels :: Program -> [CallKernel]
getKernels = nubBy sameKernel . execWriter . traverse getFunKernels
  where getFunKernels (CallKernel kernel) =
          tell [kernel]
        getFunKernels _ =
          return ()
        sameKernel (MapTranspose bt1 _ _ _ _ _ _ _ _ _) (MapTranspose bt2 _ _ _ _ _ _ _ _ _) =
          bt1 == bt2
        sameKernel _ _ = False

instance Pretty KernelConst where
  ppr (SizeConst key) = text "get_size" <> parens (ppr key)

instance Pretty KernelUse where
  ppr (ScalarUse name t) =
    text "scalar_copy" <> parens (commasep [ppr name, ppr t])
  ppr (MemoryUse name size) =
    text "mem_copy" <> parens (commasep [ppr name, ppr size])
  ppr (ConstUse name e) =
    text "const" <> parens (commasep [ppr name, ppr e])

instance Pretty HostOp where
  ppr (GetSize dest key size_class) =
    ppr dest <+> text "<-" <+>
    text "get_size" <> parens (commasep [ppr key, ppr size_class])
  ppr (GetSizeMax dest size_class) =
    ppr dest <+> text "<-" <+> text "get_size_max" <> parens (ppr size_class)
  ppr (CmpSizeLe dest name size_class x) =
    ppr dest <+> text "<-" <+>
    text "get_size" <> parens (commasep [ppr name, ppr size_class]) <+>
    text "<" <+> ppr x
  ppr (CallKernel c) =
    ppr c

instance FreeIn HostOp where
  freeIn (CallKernel c) = freeIn c
  freeIn (CmpSizeLe dest name _ x) =
    freeIn dest <> freeIn name <> freeIn x
  freeIn (GetSizeMax dest _) =
    freeIn dest
  freeIn (GetSize dest _ _) =
    freeIn dest

instance Pretty CallKernel where
  ppr (Map k) = ppr k
  ppr (AnyKernel k) = ppr k
  ppr (MapTranspose bt dest destoffset src srcoffset num_arrays size_x size_y in_size out_size) =
    text "mapTranspose" <>
    parens (ppr bt <> comma </>
            ppMemLoc dest destoffset <> comma </>
            ppMemLoc src srcoffset <> comma </>
            ppr num_arrays <> comma <+>
            ppr size_x <> comma <+>
            ppr size_y <> comma <+>
            ppr in_size <> comma <+>
            ppr out_size)
    where ppMemLoc base offset =
            ppr base <+> text "+" <+> ppr offset

instance FreeIn CallKernel where
  freeIn (Map k) = freeIn k
  freeIn (AnyKernel k) = freeIn k
  freeIn (MapTranspose _ dest destoffset src srcoffset num_arrays size_x size_y in_size out_size) =
    freeIn [dest, src] <> freeIn [destoffset, srcoffset] <> freeIn num_arrays <>
    freeIn [size_x, size_y] <> freeIn [in_size, out_size]

instance FreeIn Kernel where
  freeIn kernel = freeIn (kernelBody kernel) <>
                  freeIn [kernelNumGroups kernel, kernelGroupSize kernel]

instance Pretty MapKernel where
  ppr kernel =
    text "mapKernel" <+> brace
    (text "uses" <+> brace (commasep $ map ppr $ mapKernelUses kernel) </>
     text "body" <+> brace (ppr (mapKernelThreadNum kernel) <+>
                            text "<- get_thread_number()" </>
                            ppr (mapKernelBody kernel)))

instance Pretty Kernel where
  ppr kernel =
    text "kernel" <+> brace
    (text "groups" <+> brace (ppr $ kernelNumGroups kernel) </>
     text "group_size" <+> brace (ppr $ kernelGroupSize kernel) </>
     text "local_memory" <+> brace (commasep $
                                    map ppLocalMemory $
                                    kernelLocalMemory kernel) </>
     text "uses" <+> brace (commasep $ map ppr $ kernelUses kernel) </>
     text "body" <+> brace (ppr $ kernelBody kernel))
    where ppLocalMemory (name, Left size) =
            ppr name <+> parens (ppr size <+> text "bytes")
          ppLocalMemory (name, Right size) =
            ppr name <+> parens (ppr size <+> text "bytes (const)")

instance FreeIn MapKernel where
  freeIn kernel =
    mapKernelThreadNum kernel `S.delete` freeIn (mapKernelBody kernel)

data KernelOp = GetGroupId VName Int
              | GetLocalId VName Int
              | GetLocalSize VName Int
              | GetGlobalSize VName Int
              | GetGlobalId VName Int
              | GetLockstepWidth VName
              | Atomic AtomicOp
              | Barrier
              | MemFence
              deriving (Show)

-- Atomic operations return the value stored before the update.
-- This value is stored in the first VName.
data AtomicOp = AtomicAdd VName VName (Count Elements) Exp
              | AtomicSMax VName VName (Count Elements) Exp
              | AtomicSMin VName VName (Count Elements) Exp
              | AtomicUMax VName VName (Count Elements) Exp
              | AtomicUMin VName VName (Count Elements) Exp
              | AtomicCmpXchg VName VName (Count Elements) Exp Exp
              | AtomicXchg VName VName (Count Elements) Exp
              deriving (Show)

instance FreeIn AtomicOp where
  freeIn (AtomicAdd _ arr i x) = freeIn arr <> freeIn i <> freeIn x
  freeIn (AtomicSMax _ arr i x) = freeIn arr <> freeIn i <> freeIn x
  freeIn (AtomicSMin _ arr i x) = freeIn arr <> freeIn i <> freeIn x
  freeIn (AtomicUMax _ arr i x) = freeIn arr <> freeIn i <> freeIn x
  freeIn (AtomicUMin _ arr i x) = freeIn arr <> freeIn i <> freeIn x
  freeIn (AtomicCmpXchg _ arr i x y) = freeIn arr <> freeIn i <> freeIn x <> freeIn y
  freeIn (AtomicXchg _ arr i x) = freeIn arr <> freeIn i <> freeIn x

instance Pretty KernelOp where
  ppr (GetGroupId dest i) =
    ppr dest <+> text "<-" <+>
    text "get_group_id" <> parens (ppr i)
  ppr (GetLocalId dest i) =
    ppr dest <+> text "<-" <+>
    text "get_local_id" <> parens (ppr i)
  ppr (GetLocalSize dest i) =
    ppr dest <+> text "<-" <+>
    text "get_local_size" <> parens (ppr i)
  ppr (GetGlobalSize dest i) =
    ppr dest <+> text "<-" <+>
    text "get_global_size" <> parens (ppr i)
  ppr (GetGlobalId dest i) =
    ppr dest <+> text "<-" <+>
    text "get_global_id" <> parens (ppr i)
  ppr (GetLockstepWidth dest) =
    ppr dest <+> text "<-" <+>
    text "get_lockstep_width()"
  ppr Barrier =
    text "barrier()"
  ppr MemFence =
    text "mem_fence()"
  ppr (Atomic (AtomicAdd old arr ind x)) =
    ppr old <+> text "<-" <+> text "atomic_add" <>
    parens (commasep [ppr arr <> brackets (ppr ind), ppr x])
  ppr (Atomic (AtomicSMax old arr ind x)) =
    ppr old <+> text "<-" <+> text "atomic_smax" <>
    parens (commasep [ppr arr <> brackets (ppr ind), ppr x])
  ppr (Atomic (AtomicSMin old arr ind x)) =
    ppr old <+> text "<-" <+> text "atomic_smin" <>
    parens (commasep [ppr arr <> brackets (ppr ind), ppr x])
  ppr (Atomic (AtomicUMax old arr ind x)) =
    ppr old <+> text "<-" <+> text "atomic_umax" <>
    parens (commasep [ppr arr <> brackets (ppr ind), ppr x])
  ppr (Atomic (AtomicUMin old arr ind x)) =
    ppr old <+> text "<-" <+> text "atomic_umin" <>
    parens (commasep [ppr arr <> brackets (ppr ind), ppr x])
  ppr (Atomic (AtomicCmpXchg old arr ind x y)) =
    ppr old <+> text "<-" <+> text "atomic_cmp_xchg" <>
    parens (commasep [ppr arr <> brackets (ppr ind), ppr x, ppr y])
  ppr (Atomic (AtomicXchg old arr ind x)) =
    ppr old <+> text "<-" <+> text "atomic_xchg" <>
    parens (commasep [ppr arr <> brackets (ppr ind), ppr x])

instance FreeIn KernelOp where
  freeIn (Atomic op) = freeIn op
  freeIn _ = mempty

brace :: Doc -> Doc
brace body = text " {" </> indent 2 body </> text "}"
