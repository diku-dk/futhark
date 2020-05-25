-- | Multicore imperative code.
module Futhark.CodeGen.ImpCode.Multicore
       ( Program
       , Function
       , FunctionT (Function)
       , Code
       , Multicore(..)
       , MulticoreFunc(..)
       , SequentialFunc(..)
       , Scheduling(..)
       , ValueType(..)
       , module Futhark.CodeGen.ImpCode
       )
       where

import Futhark.CodeGen.ImpCode hiding (Function, Code)
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.Util.Pretty

-- | An imperative program.
type Program = Imp.Functions Multicore

-- | An imperative function.
type Function = Imp.Function Multicore

-- | A piece of imperative code, with multicore operations inside.
type Code = Imp.Code Multicore

-- | A function
data MulticoreFunc = MulticoreFunc Code Code VName

data SequentialFunc = SequentialFunc Code Code

-- | A parallel operation.
data Multicore = ParLoop [Param] Imp.Exp Code Code
               | MulticoreCall (Maybe VName) String
               | MCFunc [Param] VName VName Scheduling Code Code VName
               | SeqCode VName Code Code

type Granularity = Int32

data ValueType = Prim | MemBlock | Other

-- | Whether the Scheduler can/should schedule the tasks as Dynamic
-- or it is restainted to Static
-- This could carry more information
data Scheduling = Dynamic Granularity
                | Static

instance Pretty MulticoreFunc where
  ppr (MulticoreFunc prebody body _ ) =
    ppr prebody <+>
    langle <+>
    nestedBlock "{" "}" (ppr body)

instance Pretty SequentialFunc where
  ppr (SequentialFunc prebody body) =
    ppr prebody <+>
    langle <+>
    nestedBlock "{" "}" (ppr body)


instance Pretty Multicore where
  ppr (ParLoop _ _ par_code seq_code) =
    ppr par_code <+> ppr seq_code
  ppr (MCFunc _ _ _ _ prebody body _ ) =
    ppr prebody <+> ppr body
  ppr (SeqCode _ prebody body) =
    ppr prebody <+> ppr body
  ppr (MulticoreCall dests f) =
    ppr dests <+> ppr f


instance FreeIn SequentialFunc where
  freeIn' (SequentialFunc prebody body) =
    freeIn' prebody <> fvBind (Imp.declaredIn prebody) (freeIn' body)

instance FreeIn MulticoreFunc where
  freeIn' (MulticoreFunc prebody body _) =
    freeIn' prebody <> fvBind (Imp.declaredIn prebody) (freeIn' body)

instance FreeIn Multicore where
  freeIn' (ParLoop _ _ par_code seq_code) =
    freeIn' par_code <> freeIn' seq_code
  freeIn' (MCFunc _ _ _ _ prebody body _) =
    freeIn' prebody <> fvBind (Imp.declaredIn prebody) (freeIn' body)
  freeIn' (SeqCode _ prebody body) =
    freeIn' prebody <> fvBind (Imp.declaredIn prebody) (freeIn' body)
  freeIn' (MulticoreCall dests _ ) = freeIn' dests
