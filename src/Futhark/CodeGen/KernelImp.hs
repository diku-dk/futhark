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
  , KernelUse (..)
  , InKernel (..)
  , module Futhark.CodeGen.ImpCode
  )
  where

import Data.Monoid

import qualified Data.HashSet as HS
import Text.PrettyPrint.Mainland

import Futhark.CodeGen.ImpCode hiding (Program, Function, Code)
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.Representation.AST.Attributes.Names
import Futhark.Representation.AST.Pretty ()

type Program = Imp.Program CallKernel
type Function = Imp.Function CallKernel
-- | Host-level code that can call kernels.
type Code = Imp.Code CallKernel
-- | Code inside a kernel.
type KernelCode = Imp.Code InKernel

data CallKernel = CallKernel { kernelThreadNum :: VName
                               -- ^ Binding position - also serves as a unique
                               -- name for the kernel.
                             , kernelBody :: Imp.Code InKernel
                             , kernelUses :: [KernelUse]
                             , kernelSize :: DimSize
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
  ppr kernel =
    text "kernel" <+> brace
    (text "uses" <+> brace (commasep $ map ppr $ kernelUses kernel) </>
     text "body" <+> brace (ppr (kernelThreadNum kernel) <+>
                            text "<- get_thread_number()" </>
                            ppr (kernelBody kernel)))

instance FreeIn CallKernel where
  freeIn kernel =
    kernelThreadNum kernel `HS.delete` freeIn (kernelBody kernel)

data InKernel = InKernel
              deriving (Show)

instance Pretty InKernel where
  ppr InKernel = text "kernel_no_op()"

instance FreeIn InKernel where
  freeIn = const mempty

brace :: Doc -> Doc
brace body = text " {" </> indent 2 body </> text "}"
