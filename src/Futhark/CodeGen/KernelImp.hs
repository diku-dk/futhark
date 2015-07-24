-- | Variation of "Futhark.CodeGen.ImpCode" that contains the notion
-- of a kernel invocation.
module Futhark.CodeGen.KernelImp
  ( Program
  , ProgramT (Program)
  , Function
  , FunctionT (Function)
  , Code
  , Kernel (..)
  , KernelUse (..)
  , module Futhark.CodeGen.ImpCode
  )
  where

import qualified Data.HashSet as HS
import Text.PrettyPrint.Mainland

import Futhark.CodeGen.ImpCode hiding (Program, Function, Code)
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.Representation.AST.Attributes.Names
import Futhark.Representation.AST.Pretty ()

type Program = Imp.Program Kernel
type Function = Imp.Function Kernel
type Code = Imp.Code Kernel

data Kernel = Kernel { kernelThreadNum :: VName
                       -- ^ Binding position - also serves as a unique
                       -- name for the kernel.
                     , kernelBody :: Code
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

instance Pretty Kernel where
  ppr kernel =
    text "kernel" <+> brace
    (text "uses" <+> brace (commasep $ map ppr $ kernelUses kernel) </>
     text "body" <+> brace (ppr (kernelThreadNum kernel) <+>
                            text "<- get_thread_number()" </>
                            ppr (kernelBody kernel)))

instance FreeIn Kernel where
  freeIn kernel =
    kernelThreadNum kernel `HS.delete` freeIn (kernelBody kernel)

brace :: Doc -> Doc
brace body = text " {" </> indent 2 body </> text "}"
