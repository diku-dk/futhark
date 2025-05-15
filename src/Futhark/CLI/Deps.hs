-- | @futhark deps@
module Futhark.CLI.Deps (main) where

import Futhark.Compiler
import Futhark.Util.Options
import Language.Futhark.Deps

-- | Run @futhark deps@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> do
      Just $ do
        (_, imports, _) <- readProgramOrDie file
        let fm = snd $ last imports
          in putStrLn $ deps (fileProg fm)
    _ -> Nothing
