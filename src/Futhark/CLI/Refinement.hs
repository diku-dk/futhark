module Futhark.CLI.Refinement (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.List qualified as L
import Futhark.Analysis.Refinement
import Futhark.Analysis.Refinement.Latex
import Futhark.Compiler
import Futhark.Util.Options
import Futhark.Util.Pretty (hPutDoc)
import Language.Futhark.Warnings
import System.IO

data RefineConfig = RefineConfig
  { printSuccesses :: Bool,
    checkWarn :: Bool,
    printAlg :: Bool,
    printInfos :: Bool,
    laTeX :: Maybe FilePath
  }

newRefineConfig :: RefineConfig
newRefineConfig = RefineConfig False True False False Nothing

options :: [FunOptDescr RefineConfig]
options =
  [ Option
      "l"
      ["filepath"]
      ( ReqArg
          (\fp -> Right $ \config -> config {laTeX = Just fp})
          "FILEPATH"
      )
      "Print LaTeX trace."
  ]

--  [ Option
--      "v"
--      []
--      (NoArg $ Right $ \cfg -> cfg {printSuccesses = True})
--      "Print all checks.",
--    Option
--      "w"
--      []
--      (NoArg $ Right $ \cfg -> cfg {checkWarn = False})
--      "Disable all typechecker warnings.",
--    Option
--      "a"
--      []
--      (NoArg $ Right $ \cfg -> cfg {printAlg = True})
--      "Print the algebraic environment.",
--    Option
--      "i"
--      []
--      (NoArg $ Right $ \cfg -> cfg {printSuccesses = True})
--      "Print info."
--  ]

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
      -- putStrLn $ "Proved: " <> show (refineProg vns imps)
      -- putStrLn $ unlines (refineProg vns imps)
      let Just basename = reverse <$> L.stripPrefix (reverse ".fut") (reverse file)
          res = refineProg vns imps

      -- mapM_ (putStrLn . prettyString) (refineProg vns imps)

      case laTeX cfg of
        Just fp -> mkLaTeX fp res
        _ -> pure ()
    -- putStrLn $ unlines (refineProg vns imps)
    -- let (_, algenv, log) = refineProg vns imps
    -- when (printInfos cfg) $ do
    --  putStrLn "Info:"
    --  liftIO $ mapM_ putStrLn $ infos log
    -- putStrLn "Failed checks:"
    -- liftIO $ mapM_ putStrLn $ fails log
    -- when (printSuccesses cfg) $ do
    --  putStrLn "Successful checks:"
    --  liftIO $ mapM_ putStrLn $ successes log
    -- when (printAlg cfg) $ do
    --  liftIO $ putStrLn $ prettyString algenv
    _ -> Nothing
