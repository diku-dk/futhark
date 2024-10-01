-- | @futhark fmt@
module Futhark.CLI.Fmt (main) where

import Futhark.CLI.Fmt.Printer
import Data.Text.IO qualified as T
import Futhark.Util.Options
import Language.Futhark
import Language.Futhark.Parser ( SyntaxError (..) )
import System.Exit
import System.IO

-- | Run @futhark fmt@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ do
      res <- fmtText file <$> T.readFile file
      case res of
        Left (SyntaxError loc err) -> do
          T.hPutStr stderr $ locText loc <> ":\n" <> prettyText err
          exitFailure
        Right fmt -> T.hPutStr stdout fmt
    _any -> Nothing
