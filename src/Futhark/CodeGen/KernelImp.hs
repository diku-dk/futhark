-- | Variation of "Futhark.CodeGen.ImpCode" that contains the notion
-- of a kernel invocation.
module Futhark.CodeGen.KernelImp
  ( Program
  , ProgramT (Program)
  , Function
  , FunctionT (Function)
  , Code
  , Kernel (..)
  , KernelCopy (..)
  , module Futhark.CodeGen.ImpCode
  )
  where

import Text.PrettyPrint.Mainland

import Futhark.CodeGen.ImpCode hiding (Program, Function, Code)
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.Representation.AST.Pretty ()

type Program = Imp.Program Kernel
type Function = Imp.Function Kernel
type Code = Imp.Code Kernel

data Kernel = Kernel { kernelThreadNum :: VName
                       -- ^ Binding position - also serves as a unique
                       -- name for the kernel.
                     , kernelBody :: Code
                     , kernelCopyIn :: [KernelCopy]
                     , kernelCopyOut :: [KernelCopy]
                     }

data KernelCopy = CopyScalar VName BasicType
                | CopyMemory VName Imp.DimSize
                deriving (Eq, Show)

instance Pretty KernelCopy where
  ppr (CopyScalar name t) =
    text "scalar_copy" <> parens (commasep [ppr name, ppr t])
  ppr (CopyMemory name size) =
    text "mem_copy" <> parens (commasep [ppr name, ppr size])

instance Pretty Kernel where
  ppr kernel =
    text "kernel" <+> brace
    (text "copy-in" <+> brace (commasep $ map ppr $ kernelCopyIn kernel) </>
     text "body" <+> brace (ppr (kernelThreadNum kernel) <+>
                            text "<- get_thread_number()" </>
                            ppr (kernelBody kernel)) </>
     text "copy-out" <+> brace (commasep $ map ppr $ kernelCopyOut kernel))

brace :: Doc -> Doc
brace body = text " {" </> indent 2 body </> text "}"
