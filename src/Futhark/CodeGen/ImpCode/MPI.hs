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
data MPIOp = Segop String [Param] Code [Param]
   | DistributedLoop String VName Code Code Code [Param] VName
   | CrashWithThisMessage String

instance Pretty MPIOp where
  ppr (CrashWithThisMessage s) = text "crash" <+> parens (ppr s)
  ppr _ = text ""

-- The free variables of an MPIOp.
instance FreeIn MPIOp where
  freeIn' (CrashWithThisMessage _) = mempty
  freeIn' (DistributedLoop _ _ prebody body postbody _ _) = freeIn' prebody <> fvBind (Imp.declaredIn prebody) (freeIn' $ body <> postbody)
  freeIn' (Segop _ _ code _) = freeIn' code