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
  ppr (CallKernel c) =
    ppr c

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
              | Barrier
              deriving (Show)

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

instance FreeIn KernelOp where
  freeIn = const mempty

brace :: Doc -> Doc
brace body = text " {" </> indent 2 body </> text "}"
