-- | The *actual* @futhark@ command line program, as seen by cabal.
module Main (main) where

import Futhark.CLI.Main qualified

-- | This is the main function.
main :: IO ()
main = Futhark.CLI.Main.main
