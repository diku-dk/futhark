-- | MPI imperative code.
module Futhark.CodeGen.ImpCode.MPI
  ( Program,
    Function,
    FunctionT (Function),
    Code,
    MPIOp (..),
    module Futhark.CodeGen.ImpCode,
  )
where

import Futhark.CodeGen.ImpCode hiding (Code, Function)
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.Util.Pretty

-- | An imperative program.
type Program = Imp.Definitions MPIOp

-- | An imperative function.
type Function = Imp.Function MPIOp

-- | A piece of imperative code.
type Code = Imp.Code MPIOp

-- | Some kind of MPI-level imperative operation, that will presumably
-- be turned into some particular C code.
data MPIOp = Segop String [Param] Code [Param] Imp.Exp
   | DistributedLoop String VName Code Code Code [Param] VName
   | CrashWithThisMessage String

instance Pretty MPIOp where
  ppr (CrashWithThisMessage s) = text "crash" <+> parens (ppr s)
  ppr (Segop name free seq_code retval iterations) =
    text name
      <+> ppr free
      <+> text "seq_code"
      <+> nestedBlock "{" "}" (ppr seq_code)
      <+> text "retvals"
      <+> ppr retval
      <+> text "iterations"
      <+> ppr iterations
  ppr (DistributedLoop s i prebody body postbody params info) =
    text "parloop" <+> ppr s <+> ppr i
      <+> ppr prebody
      <+> ppr params
      <+> ppr info
      <+> langle
      <+> nestedBlock "{" "}" (ppr body)
      <+> ppr postbody


-- The free variables of an MPIOp.
instance FreeIn MPIOp where
  freeIn' (CrashWithThisMessage _) = mempty
  freeIn' (DistributedLoop _ _ prebody body postbody _ _) = freeIn' prebody <> fvBind (Imp.declaredIn prebody) (freeIn' $ body <> postbody)
  freeIn' (Segop _ _ code _ _) = freeIn' code