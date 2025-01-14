-- | @futhark fmt@
module Futhark.CLI.Fmt (main) where

import Control.Monad (forM_, unless)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Futhark.Fmt.Printer
import Futhark.Util.Options
import Futhark.Util.Pretty (docText, hPutDoc, putDoc)
import Language.Futhark
import Language.Futhark.Parser (SyntaxError (..))
import System.Exit
import System.IO

newtype FmtCfg = FmtCfg
  { cfgCheck :: Bool
  }

initialFmtCfg :: FmtCfg
initialFmtCfg = FmtCfg {cfgCheck = False}

fmtOptions :: [FunOptDescr FmtCfg]
fmtOptions =
  [ Option
      ""
      ["check"]
      (NoArg $ Right $ \cfg -> cfg {cfgCheck = True})
      "Check whether file is correctly formatted."
  ]

-- | Run @futhark fmt@.
main :: String -> [String] -> IO ()
main = mainWithOptions initialFmtCfg fmtOptions "[FILES]" $ \args cfg ->
  case args of
    [] -> Just $ putDoc =<< onInput =<< T.getContents
    files ->
      Just $ forM_ files $ \file -> do
        file_s <- T.readFile file
        doc <- onInput file_s
        if cfgCheck cfg
          then unless (docText doc == file_s) $ do
            T.hPutStrLn stderr $ T.pack file <> ": not formatted correctly."
            T.hPutStr stderr $ docText doc
            exitFailure
          else withFile file WriteMode $ \h -> hPutDoc h doc
  where
    onInput s = do
      case fmtToDoc "<stdin>" s of
        Left (SyntaxError loc err) -> do
          T.hPutStr stderr $ locText loc <> ":\n" <> prettyText err
          exitFailure
        Right fmt -> pure fmt
