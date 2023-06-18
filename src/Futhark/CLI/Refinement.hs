module Futhark.CLI.Refinement (main) where

import Control.Monad
import Control.Monad.IO.Class
import Futhark.Analysis.Refinement
import Futhark.Compiler
import Futhark.Util.Options
import Futhark.Util.Pretty (hPutDoc, pretty, prettyString, vsep)
import Language.Futhark.Warnings
import System.IO

data RefineConfig = RefineConfig
  { refineAll :: Bool,
    checkWarn :: Bool,
    printProg :: Bool,
    printAST :: Bool
  }

newRefineConfig :: RefineConfig
newRefineConfig = RefineConfig False True True False

options :: [FunOptDescr RefineConfig]
options =
  [ Option
      "v"
      []
      (NoArg $ Right $ \cfg -> cfg {refineAll = True})
      "Print all checks.",
    Option
      "w"
      []
      (NoArg $ Right $ \cfg -> cfg {checkWarn = False})
      "Disable all typechecker warnings.",
    Option
      "p"
      []
      (NoArg $ Right $ \cfg -> cfg {printProg = True})
      "Print transformed program.",
    Option
      "a"
      []
      (NoArg $ Right $ \opts -> opts {printAST = True})
      "Output ASTs instead of prettyprinted programs."
  ]

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
      let (imps', _) = refineProg_ vns imps
      when (printProg cfg) $
        liftIO $
          forM_ (map snd imps') $ \fm ->
            putStrLn $
              if printAST cfg
                then show $ fileProg fm
                else prettyString $ fileProg fm
    _ -> Nothing
