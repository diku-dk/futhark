-- | Variation of "Futhark.CodeGen.ImpCode" that contains the notion
-- of a kernel invocation.
module Futhark.CodeGen.KernelImp
  ( Program
  , ProgramT (Program)
  , Function
  , FunctionT (Function)
  , Code
  , KernelCode
  , CallKernel (..)
  , MapKernel (..)
  , ReduceKernel (..)
  , KernelUse (..)
  , InKernel (..)
  , module Futhark.CodeGen.ImpCode
  )
  where

import Data.Monoid

import qualified Data.HashSet as HS

import Futhark.CodeGen.ImpCode hiding (Program, Function, Code)
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.Representation.AST.Attributes.Names
import Futhark.Representation.AST.Pretty ()
import Futhark.Util.Pretty

type Program = Imp.Program CallKernel
type Function = Imp.Function CallKernel
-- | Host-level code that can call kernels.
type Code = Imp.Code CallKernel
-- | Code inside a kernel.
type KernelCode = Imp.Code InKernel

data CallKernel = Kernel MapKernel
                | Reduce ReduceKernel
                | MapTranspose BasicType VName Exp VName Exp Exp Exp Exp
            deriving (Show)

-- | A generic kernel containing arbitrary kernel code.
data MapKernel = MapKernel { kernelThreadNum :: VName
                             -- ^ Binding position - also serves as a unique
                             -- name for the kernel.
                           , kernelBody :: Imp.Code InKernel
                           , kernelUses :: [KernelUse]
                           , kernelSize :: DimSize
                           }
                     deriving (Show)

data ReduceKernel = ReduceKernel
                    { reductionReduceOperation :: Imp.Code InKernel
                    , reductionFoldOperation :: Imp.Code InKernel
                    , reductionThreadLocalMemory :: [(VName, MemSize)]
                      -- ^ In-kernel name and per-workgroup size in bytes.
                    , reductionWriteFoldResult :: Imp.Code InKernel
                    , reductionPrologue :: Imp.Code InKernel
                    , reductionWriteFinalResult :: Imp.Code InKernel

                    , reductionUses :: [KernelUse]
                    , reductionNumGroups :: DimSize
                    , reductionGroupSize :: DimSize
                    , reductionOffsetName :: VName
                    , reductionKernelName :: VName
                      -- ^ Unique name for the kernel.
                    }
                    deriving (Show)

data KernelUse = ScalarUse VName BasicType
               | MemoryUse VName Imp.DimSize
                 deriving (Eq, Show)

instance Pretty KernelUse where
  ppr (ScalarUse name t) =
    text "scalar_copy" <> parens (commasep [ppr name, ppr t])
  ppr (MemoryUse name size) =
    text "mem_copy" <> parens (commasep [ppr name, ppr size])

instance Pretty CallKernel where
  ppr (Kernel k) = ppr k
  ppr (Reduce k) = ppr k
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
    text "kernel" <+> brace
    (text "uses" <+> brace (commasep $ map ppr $ kernelUses kernel) </>
     text "body" <+> brace (ppr (kernelThreadNum kernel) <+>
                            text "<- get_thread_number()" </>
                            ppr (kernelBody kernel)))

instance Pretty ReduceKernel where
  ppr = text . show

instance FreeIn MapKernel where
  freeIn kernel =
    kernelThreadNum kernel `HS.delete` freeIn (kernelBody kernel)

data InKernel = GetGroupId VName Int
              | GetLocalId VName Int
              | GetLocalSize VName Int
              | GetGlobalSize VName Int
              | GetGlobalId VName Int
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

instance FreeIn InKernel where
  freeIn = const mempty

brace :: Doc -> Doc
brace body = text " {" </> indent 2 body </> text "}"
