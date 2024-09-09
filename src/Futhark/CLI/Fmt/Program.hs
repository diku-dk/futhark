module Futhark.CLI.Fmt.Program (
  format
  ) where

import Futhark.CLI.Fmt.Format
import Futhark.CLI.Fmt.Declaration
import Language.Futhark
import Language.Futhark.Parser ( Comment (..) )

-- | Does not return residual comments, because these are simply
-- inserted at the end.
fmtProg :: UncheckedProg -> FormatState Fmt
fmtProg (Prog dc decs) = do
  dc' <- fmtDocComment dc
  decs' <- fmtMany fmtDec decs
  cs <- emptyComments
  pure $ dc' <> decs' <> map commentText cs 

format :: UncheckedProg -> [Comment] -> Fmt
format prog = runFormatState (fmtProg prog)
