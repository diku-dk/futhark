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
               | ParLoopAcc VName Imp.Exp MulticoreFunc -- A loop with an accumulator


instance Pretty MulticoreFunc where
  ppr (MulticoreFunc fargs _ body) =
    text "parfor" <+> ppr fargs <+> langle <+>
    nestedBlock "{" "}" (ppr body)

instance Pretty Multicore where
  ppr (ParLoop i e func) =
    text "parfor" <+> ppr i <+> langle <+> ppr e <+>
    nestedBlock "{" "}" (ppr func)
  ppr (ParLoopAcc i e func) =
    text "parfor" <+> ppr i <+> langle <+> ppr e <+>
    nestedBlock "{" "}" (ppr func)

instance FreeIn MulticoreFunc where
  freeIn' (MulticoreFunc _ _ body) = freeIn' body

instance FreeIn Multicore where
  freeIn' (ParLoop _ e func) = freeIn' e <> freeIn' func
  freeIn' (ParLoopAcc _ e func) = freeIn' e <> freeIn' func
