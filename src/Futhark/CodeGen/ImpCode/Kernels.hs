{-# LANGUAGE FlexibleContexts #-}
-- | Variation of "Futhark.CodeGen.ImpCode" that contains the notion
-- of a kernel invocation.
module Futhark.CodeGen.ImpCode.Kernels
  ( Program
  , Function
  , FunctionT (Function)
  , Code
  , KernelCode
  , CallKernel (..)
  , MapKernel (..)
  , Kernel (..)
  , KernelUse (..)
  , InKernel (..)
  , module Futhark.CodeGen.ImpCode
  -- * Utility functions
  , getKernels
  )
  where

import Control.Monad.Writer
import Data.List
import qualified Data.HashSet as HS
import Data.Traversable

import Prelude

import Futhark.CodeGen.ImpCode hiding (Function, Code)
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.Representation.AST.Attributes.Names
import Futhark.Representation.AST.Pretty ()
import Futhark.Util.Pretty

type Program = Functions CallKernel
type Function = Imp.Function CallKernel
-- | Host-level code that can call kernels.
type Code = Imp.Code CallKernel
-- | Code inside a kernel.
type KernelCode = Imp.Code InKernel

data CallKernel = Map MapKernel
                | CallKernel Kernel
                | MapTranspose BasicType VName Exp VName Exp Exp Exp Exp
            deriving (Show)

-- | A generic kernel containing arbitrary kernel code.
data MapKernel = MapKernel { mapKernelThreadNum :: VName
                             -- ^ Binding position - also serves as a unique
                             -- name for the kernel.
                           , mapKernelBody :: Imp.Code InKernel
                           , mapKernelUses :: [KernelUse]
                           , mapKernelSize :: DimSize
                           }
                     deriving (Show)

data Kernel = Kernel
              { kernelBody :: Imp.Code InKernel
              , kernelLocalMemory :: [(VName, MemSize)]
                -- ^ In-kernel name and per-workgroup size in bytes.

              , kernelUses :: [KernelUse]
                -- ^ The host variables referenced by the kernel.

              , kernelNumGroups :: DimSize
              , kernelGroupSize :: DimSize
              , kernelName :: VName
                -- ^ Unique name for the kernel.
              }
            deriving (Show)

data KernelUse = ScalarUse VName BasicType
               | MemoryUse VName Imp.DimSize
                 deriving (Eq, Show)

getKernels :: Program -> [CallKernel]
getKernels = nubBy sameKernel . execWriter . traverse getFunKernels
  where getFunKernels kernel =
          tell [kernel] >> return kernel
        sameKernel (MapTranspose bt1 _ _ _ _ _ _ _) (MapTranspose bt2 _ _ _ _ _ _ _) =
          bt1 == bt2
        sameKernel _ _ = False

instance Pretty KernelUse where
  ppr (ScalarUse name t) =
    text "scalar_copy" <> parens (commasep [ppr name, ppr t])
  ppr (MemoryUse name size) =
    text "mem_copy" <> parens (commasep [ppr name, ppr size])

instance Pretty CallKernel where
  ppr (Map k) = ppr k
  ppr (CallKernel k) = ppr k
  ppr (MapTranspose bt dest destoffset src srcoffset num_arrays size_x size_y) =
    text "mapTranspose" <>
    parens (ppr bt <> comma </>
            ppMemLoc dest destoffset <> comma </>
            ppMemLoc src srcoffset <> comma </>
            ppr num_arrays <> comma <+> ppr size_x <> comma <+> ppr size_y)
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
     text "group_size" <+> brace (ppr $ kernelNumGroups kernel) </>
     text "local_memory" <+> brace (commasep $
                                    map ppLocalMemory $
                                    kernelLocalMemory kernel) </>
     text "uses" <+> brace (commasep $ map ppr $ kernelUses kernel) </>
     text "body" <+> brace (ppr $ kernelBody kernel))
    where ppLocalMemory (name, size) =
            ppr name <+> parens (ppr size <+> text "bytes")

instance FreeIn MapKernel where
  freeIn kernel =
    mapKernelThreadNum kernel `HS.delete` freeIn (mapKernelBody kernel)

data InKernel = GetGroupId VName Int
              | GetLocalId VName Int
              | GetLocalSize VName Int
              | GetGlobalSize VName Int
              | GetGlobalId VName Int
              | GetWaveSize VName
              | Barrier
              deriving (Show)

instance Pretty InKernel where
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
  ppr (GetWaveSize dest) =
    ppr dest <+> text "<-" <+>
    text "get_wave_size()"
  ppr Barrier =
    text "barrier()"

instance FreeIn InKernel where
  freeIn = const mempty

brace :: Doc -> Doc
brace body = text " {" </> indent 2 body </> text "}"
