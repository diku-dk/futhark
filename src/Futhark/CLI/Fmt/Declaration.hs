module Futhark.CLI.Fmt.Declaration (
  fmtDec
  ) where

import Futhark.CLI.Fmt.Format
import Futhark.CLI.Fmt.Type
import Language.Futhark

-- | Formatting of Futhark declarations.
fmtDec :: UncheckedDec -> FormatState Fmt
fmtDec (ValDec t) = undefined -- A value declaration.
fmtDec (TypeDec tb) = fmtTypeBind tb -- A type declaration.
fmtDec (ModTypeDec tb) = undefined -- A module type declation.
fmtDec (ModDec tb) = undefined -- A module declation.
fmtDec (OpenDec tb loc) = undefined -- I have no clue.
fmtDec (LocalDec tb loc) = undefined -- I have no clue, maybe this just adds the local keyword?
fmtDec (ImportDec path tb loc) = undefined -- Import declarations.
