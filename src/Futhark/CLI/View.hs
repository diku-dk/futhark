{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Futhark.CLI.View (main) where

import Control.Monad
import Control.Monad.IO.Class
import Futhark.Analysis.View
import Futhark.Compiler
import Futhark.Util.Options
import Futhark.Util.Pretty (hPutDoc, putDoc, Pretty (pretty), prettyString)
import Language.Futhark.Warnings
import System.IO
import qualified Data.Map as M
import Data.Char (isSpace)
import Language.Futhark.Core (baseString)

-- import Futhark.Internalise.Defunctionalise as Defunctionalise
-- import Futhark.Internalise.Defunctorise as Defunctorise
-- import Futhark.Internalise.FullNormalise as FullNormalise
-- import Control.Monad.State
-- import Debug.Trace (traceM)

newtype RefineConfig = RefineConfig
  { checkWarn :: Bool }

newRefineConfig :: RefineConfig
newRefineConfig = RefineConfig True

options :: [FunOptDescr RefineConfig]
options =
  [ ]

tests :: [(FilePath, [String])]
tests =
  [ ("tests/refinement/part2indices.fut", [
      "∀i₆₁₇₆ ∈ iota n₆₀₆₈ .",
      "    | (conds₆₀₇₀)[i₆₁₇₆] => -1 + Σj₆₁₇₂∈[0, ..., i₆₁₇₆] (⟦(conds₆₀₇₀)[j₆₁₇₂]⟧)",
      "    | ¬((conds₆₀₇₀)[i₆₁₇₆]) => -1 + Σj₆₁₇₂∈[0, ..., -1 + n₆₀₆₈] (⟦(conds₆₀₇₀)[j₆₁₇₂]⟧) + Σj₆₁₇₄∈[0, ..., i₆₁₇₆] (⟦¬((conds₆₀₇₀)[j₆₁₇₄])⟧)"
    ])
  ]

-- Remove leading and trailing whitespace.
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- Trim whitespace and remove VName ids.
strip :: String -> String
strip = trim . filter stripId
  where
    nums :: [Char]
    nums = "₀₁₂₃₄₅₆₇₈₉"
    stripId c = c `notElem` nums

-- | Run tests on all files in the "tests/refinement" directory.
runAllTests :: IO ()
runAllTests = do
  mapM_ runTest tests

-- | Run test on a specific file.
runTest :: (FilePath, [String]) -> IO ()
runTest (file, expected) = do
  putStrLn $ "Running test on file: " ++ file
  (_warnings, imports, src) <- readProgramOrDie file
  let res = M.mapKeys baseString $ mkViewProg src imports
  let actual = res M.! "inds"
  if map strip expected == map strip (lines (prettyString actual))
  then putStrLn $ "Test passed: " ++ file
  else error $ "Test failed: " ++ file

-- | Run @futhark refinement@.
main :: String -> [String] -> IO ()
main = mainWithOptions newRefineConfig options "program" $ \args cfg ->
  case args of
    ["test"] -> Just runAllTests
    [file] -> Just $ do
      (warnings, imports, src) <- readProgramOrDie file
      when (checkWarn cfg && anyWarnings warnings) $
        liftIO $
          hPutDoc stderr $
            prettyWarnings warnings
      -- putStrLn $ "Proved: " <> take 100 (show (mkViewProg vns imps)) <> "..."
      -- let valbinds = flip evalState src $
      --                  Defunctorise.transformProg imports
      --                  >>= FullNormalise.transformProg
      -- let res = mkViewProg src valbinds
      let res = mkViewProg src imports
      putStrLn "\nIndex function:\n---------------\n"
      putDoc (pretty res)
    _ -> Nothing

