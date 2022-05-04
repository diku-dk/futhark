-- | The *actual* @futhark@ command line program, as seen by cabal.
module Main (main) where

import qualified Futhark.CLI.Main

main :: IO ()
main = Futhark.CLI.Main.main
