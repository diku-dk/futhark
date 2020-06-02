-- | Multicore imperative code.
module Futhark.CodeGen.ImpCode.Multicore
       ( Program
       , Function
       , FunctionT (Function)
       , Code
       , Multicore(..)
       , Scheduling(..)
       , MulticoreInfo(..)
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

type Cost = Int32

data MulticoreInfo = MulticoreInfo VName Scheduling VName
-- MulticoreInfo ntasks Sched tid

-- | A multicore operation.
data Multicore = Task [Param] Imp.Exp Code Code VName [Param]
               | MCFunc VName Code Code [Param] MulticoreInfo
               | MulticoreCall (Maybe VName) String

type Granularity = Int32

-- | Whether the Scheduler can/should schedule the tasks as Dynamic
-- or it is restainted to Static
-- This could carry more information
data Scheduling = Dynamic Granularity
                | Static

instance Pretty Scheduling where
  ppr (Dynamic granularity) =
    text "Dynamic" <+> ppr granularity
  ppr Static =
    text "Static"


instance Pretty MulticoreInfo where
  ppr (MulticoreInfo _ sched _) =
    text "MulticoreInfo" <+> ppr sched

instance Pretty Multicore where
  ppr (Task free e par_code seq_code i retval) =
    text "parfor" <+> ppr i <+> langle <+> ppr e <+>
    ppr free <+>
    text "par_code" <+> nestedBlock "{" "}" (ppr par_code) <+>
    text "seq_code" <+> nestedBlock "{" "}" (ppr seq_code) <+>
    text "retvals" <+> ppr retval

  ppr (MCFunc i prebody body params info) =
    text "parfunc" <+> ppr i <+>
    ppr prebody <+>
    ppr params <+>
    ppr info <+>
    langle <+>
    nestedBlock "{" "}" (ppr body)

  ppr (MulticoreCall dests f) =
    ppr dests <+> ppr f


instance FreeIn Multicore where
  freeIn' (Task _ e par_code seq_code _ _) =
    freeIn' e <> freeIn' par_code <> freeIn' seq_code
  freeIn' (MCFunc _ prebody body _ _) =
    freeIn' prebody <> fvBind (Imp.declaredIn prebody) (freeIn' body)
  freeIn' (MulticoreCall dests _ ) = freeIn' dests
