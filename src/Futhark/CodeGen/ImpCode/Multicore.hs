-- | Multicore imperative code.
module Futhark.CodeGen.ImpCode.Multicore
       ( Program
       , Function
       , FunctionT (Function)
       , Code
       , Multicore(..)
       , MulticoreFunc(..)
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


-- | A function
data MulticoreFunc = MulticoreFunc [VName] [Type] Code

-- | A parallel operation.
data Multicore = ParLoop VName Imp.Exp MulticoreFunc
               | ParRed VName Imp.Exp Code


instance Pretty MulticoreFunc where
  ppr (MulticoreFunc fargs _ func) =
    text "parfor" <+> ppr fargs <+> langle <+>
    nestedBlock "{" "}" (ppr func)

instance Pretty Multicore where
  ppr (ParLoop i e func) =
    text "parfor" <+> ppr i <+> langle <+> ppr e <+>
    nestedBlock "{" "}" (ppr func)
  ppr (ParRed i e body) =
    text "parred" <+> ppr i <+> langle <+> ppr e <+>
    nestedBlock "{" "}" (ppr body)


instance FreeIn MulticoreFunc where
  freeIn' (MulticoreFunc _ _ func) = freeIn' func

instance FreeIn Multicore where
  freeIn' (ParLoop _ e func) = freeIn' e <> freeIn' func
  freeIn' (ParRed _ e body) = freeIn' e <> freeIn' body
