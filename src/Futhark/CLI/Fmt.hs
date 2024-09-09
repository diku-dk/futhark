module Futhark.CLI.Fmt (
  main
  ) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Futhark.Util.Options
import Language.Futhark
import Language.Futhark.Parser
  ( SyntaxError (..),
    parseFutharkWithComments,
  )
import System.Exit
import System.IO
import Text.Printf (printf)
import Futhark.CLI.Fmt.Program

-- | Run @futhark fmt@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ do
      pres <- parseFutharkWithComments file <$> T.readFile file
      case pres of
        Left (SyntaxError loc err) -> do
          T.hPutStr stderr $ locText loc <> ":\n" <> prettyText err
          exitFailure
        Right (prog, cs) -> do
          let number i l = T.pack $ printf "%4d %s" (i :: Int) l
          let fmt = format prog cs
          T.hPutStr stdout $ T.unlines $ zipWith number [0 ..] fmt
    _ -> Nothing
