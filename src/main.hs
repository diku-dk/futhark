-- | The *actual* @futhark@ command line program, as seen by cabal.
module Main (main) where

import qualified Futhark.CLI.Main

-- | This is the main function.
main :: IO ()
main = Futhark.CLI.Main.main
