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

-- Expected (name, index function in prettyString format).
-- prettyString format used for janky equality testing.
type Test = (String, [String])
-- File along with Test.
tests :: [(FilePath, [Test])]
tests =
  [ ("tests/refinement/part2indices.fut",
     [ ("inds", [
        "∀i₆₁₇₆ ∈ iota n₆₀₆₈ .",
        "    | (conds₆₀₇₀)[i₆₁₇₆] => -1 + Σj₆₁₇₂∈[0, ..., i₆₁₇₆] (⟦(conds₆₀₇₀)[j₆₁₇₂]⟧)",
        "    | ¬((conds₆₀₇₀)[i₆₁₇₆]) => -1 + Σj₆₁₇₂∈[0, ..., -1 + n₆₀₆₈] (⟦(conds₆₀₇₀)[j₆₁₇₂]⟧) + Σj₆₁₇₄∈[0, ..., i₆₁₇₆] (⟦¬((conds₆₀₇₀)[j₆₁₇₄])⟧)"
       ])
     ]),
    ("tests/refinement/part2indices_neg_conds.fut",
     [ ("inds", [
        "∀i₆₁₇₆ ∈ iota n₆₀₆₈ .",
        "    | (conds₆₀₇₀)[i₆₁₇₆] => -1 + Σj₆₁₇₂∈[0, ..., i₆₁₇₆] (⟦(conds₆₀₇₀)[j₆₁₇₂]⟧)",
        "    | ¬((conds₆₀₇₀)[i₆₁₇₆]) => -1 + Σj₆₁₇₂∈[0, ..., -1 + n₆₀₆₈] (⟦(conds₆₀₇₀)[j₆₁₇₂]⟧) + Σj₆₁₇₄∈[0, ..., i₆₁₇₆] (⟦¬((conds₆₀₇₀)[j₆₁₇₄])⟧)"
       ])
     ]),
    ("tests/refinement/part2indices_scan_exc.fut",
     [ ("inds", [
        "∀i₆₂₀₄ ∈ iota n₆₀₆₈ .",
        "    | (conds₆₀₇₀)[i₆₂₀₄] => Σj₆₂₀₀∈[1, ..., i₆₂₀₄] (⟦(conds₆₀₇₀)[-1 + j₆₂₀₀]⟧)",
        "    | ¬((conds₆₀₇₀)[i₆₂₀₄]) => -1 + Σj₆₂₀₀∈[1, ..., n₆₀₆₈] (⟦(conds₆₀₇₀)[-1 + j₆₂₀₀]⟧) + Σj₆₂₀₂∈[0, ..., i₆₂₀₄] (⟦¬((conds₆₀₇₀)[j₆₂₀₂])⟧)"
       ])
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
runTest :: (FilePath, [Test]) -> IO ()
runTest (file, expected) = do
  putStrLn $ "Running test on file: " ++ file
  (_warnings, imports, src) <- readProgramOrDie file
  let res = M.mapKeys baseString $ mkViewProg src imports
  let passed = all (checkTest res) expected
  if passed
  then putStrLn $ "Test passed: " ++ file
  else error $ "Test failed: " ++ file
  where
    checkTest res (name, expected_indxfn) =
      let actual = res M.! name
      in  map strip expected_indxfn == map strip (lines (prettyString actual))

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

