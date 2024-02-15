module Futhark.CLI.View (main) where

import Control.Monad
import Control.Monad.IO.Class
import Futhark.Analysis.View
import Futhark.Compiler
import Futhark.Util.Options
import Futhark.Util.Pretty (hPutDoc)
import Language.Futhark.Warnings
import System.IO

newtype RefineConfig = RefineConfig
  { checkWarn :: Bool }

newRefineConfig :: RefineConfig
newRefineConfig = RefineConfig True

options :: [FunOptDescr RefineConfig]
options =
  [ ]

-- | Run @futhark refinement@.
main :: String -> [String] -> IO ()
main = mainWithOptions newRefineConfig options "program" $ \args cfg ->
  case args of
    [file] -> Just $ do
      (warnings, imps, vns) <- readProgramOrDie file
      when (checkWarn cfg && anyWarnings warnings) $
        liftIO $
          hPutDoc stderr $
            prettyWarnings warnings
      -- putStrLn $ "Proved: " <> take 100 (show (mkViewProg vns imps)) <> "..."
      let res = mkViewProg vns imps
      putStrLn $ "\nIndex function:\n---------------\n" <> show res
      -- pure ()
    _ -> Nothing

