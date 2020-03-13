-- | Multicore imperative code.
module Futhark.CodeGen.ImpCode.Multicore
       ( Program
       , Function
       , FunctionT (Function)
       , Code
       , Multicore(..)
       , module Futhark.CodeGen.ImpCode
       )
       where

import Futhark.CodeGen.ImpCode hiding (Function, Code)
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.Representation.AST.Attributes.Names

import Futhark.Util.Pretty

-- | An imperative program.
type Program = Imp.Functions Multicore

-- | An imperative function.
type Function = Imp.Function Multicore

-- | A piece of imperative code, with multicore operations inside.
type Code = Imp.Code Multicore

-- | A parallel operation.
data Multicore = ParLoop [VName] [Type] VName Imp.Exp Code
               | ParRed VName Imp.Exp Code


instance Pretty Multicore where
  ppr (ParLoop fargs _ i e body) =
    text "parfor" <+> ppr fargs <+> ppr i <+> langle <+> ppr e <+>
    nestedBlock "{" "}" (ppr body)
  ppr (ParRed i e body) =
    text "parred" <+> ppr i <+> langle <+> ppr e <+>
    nestedBlock "{" "}" (ppr body)

instance FreeIn Multicore where
  freeIn' (ParLoop _ _ _ e body) = freeIn' e <> freeIn' body
  freeIn' (ParRed _ e body) = freeIn' e <> freeIn' body
