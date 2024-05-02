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
  [ ("tests/refinement/scalar.fut",
     [ ("y", [
        "∀i₆₁₇₆ ∈ iota x₆₀₆₈ .",
        "    | True ⇒  x₆₀₆₈"
       ])
     ]),
    ("tests/refinement/array_to_scalar.fut",
     [ ("y", [
        ".",
        "    | True ⇒  xs₆₀₆₈[i₆₁₇₆]"
       ])
     ]),
    ("tests/refinement/map.fut",
     [ ("y", [
        "∀i₆₁₇₆ ∈ iota n₆₀₆₈ .",
        "    | True ⇒  xs₆₀₇₀[i₆₁₇₆]"
       ])
     ]),
    ("tests/refinement/if.fut",
     [ ("y", [
        "∀i₆₁₂₀ ∈ iota n₆₀₆₈ .",
        "    | xs₆₀₇₀[i₆₁₂₀] > 0 && xs₆₀₇₀[0] > 1337 ⇒  xs₆₀₇₀[i₆₁₂₀]",
        "    | ¬(xs₆₀₇₀[i₆₁₂₀] > 0) && xs₆₀₇₀[0] > 1337 ⇒  -1 + xs₆₀₇₀[i₆₁₂₀]",
        "    | xs₆₀₇₀[i₆₁₂₀] > 1 && ¬(xs₆₀₇₀[0] > 1337) ⇒  xs₆₀₇₀[i₆₁₂₀]",
        "    | ¬(xs₆₀₇₀[i₆₁₂₀] > 1) && ¬(xs₆₀₇₀[0] > 1337) ⇒  -1 + xs₆₀₇₀[i₆₁₂₀]"
       ])
     ]),
    ("tests/refinement/part2indices.fut",
     [ ("inds", [
        "∀i₆₁₇₆ ∈ iota n₆₀₆₈ .",
        "    | conds₆₀₇₀[i₆₁₇₆] ⇒  -1 + ∑j₆₁₇₂∈[0, ..., i₆₁₇₆] (⟦conds₆₀₇₀[j₆₁₇₂]⟧)",
        "    | ¬(conds₆₀₇₀[i₆₁₇₆]) ⇒  i₆₁₇₆ + ∑j₆₁₇₂∈[0, ..., -1 + n₆₀₆₈] (⟦conds₆₀₇₀[j₆₁₇₂]⟧) + -1*∑j₆₁₇₄∈[0, ..., i₆₁₇₆] (⟦conds₆₀₇₀[j₆₁₇₄]⟧)"
       ])
     ]),
    ("tests/refinement/part2indices_let_inside_map.fut",
     [ ("inds", [
        "∀i₆₁₇₆ ∈ iota n₆₀₆₈ .",
        "    | conds₆₀₇₀[i₆₁₇₆] ⇒  -1 + ∑j₆₁₇₂∈[0, ..., i₆₁₇₆] (⟦conds₆₀₇₀[j₆₁₇₂]⟧)",
        "    | ¬(conds₆₀₇₀[i₆₁₇₆]) ⇒  i₆₁₇₆ + ∑j₆₁₇₂∈[0, ..., -1 + n₆₀₆₈] (⟦conds₆₀₇₀[j₆₁₇₂]⟧) + -1*∑j₆₁₇₄∈[0, ..., i₆₁₇₆] (⟦conds₆₀₇₀[j₆₁₇₄]⟧)"
       ])
     ]),
    ("tests/refinement/part2indices_neg_conds.fut",
     [ ("inds", [
        "∀i₆₁₇₆ ∈ iota n₆₀₆₈ .",
        "    | conds₆₀₇₀[i₆₁₇₆] ⇒  -1 + ∑j₆₁₇₂∈[0, ..., i₆₁₇₆] (⟦conds₆₀₇₀[j₆₁₇₂]⟧)",
        "    | ¬(conds₆₀₇₀[i₆₁₇₆]) ⇒  i₆₁₇₆ + ∑j₆₁₇₂∈[0, ..., -1 + n₆₀₆₈] (⟦conds₆₀₇₀[j₆₁₇₂]⟧) + -1*∑j₆₁₇₄∈[0, ..., i₆₁₇₆] (⟦conds₆₀₇₀[j₆₁₇₄]⟧)"
       ])
     ]),
    ("tests/refinement/part2indices_scan_exc.fut",
     [ ("inds", [
        "∀i₆₂₀₄ ∈ iota n₆₀₆₈ .",
        "    | conds₆₀₇₀[i₆₂₀₄] ⇒  ∑j₆₂₀₀∈[1, ..., i₆₂₀₄] (⟦conds₆₀₇₀[-1 + j₆₂₀₀]⟧)",
        "    | ¬(conds₆₀₇₀[i₆₂₀₄]) ⇒  i₆₂₀₄ + ∑j₆₂₀₀∈[1, ..., n₆₀₆₈] (⟦conds₆₀₇₀[-1 + j₆₂₀₀]⟧) + -1*∑j₆₂₀₂∈[0, ..., i₆₂₀₄] (⟦conds₆₀₇₀[j₆₂₀₂]⟧)"
       ])
     ]),
    ("tests/refinement/refine_iterator1.fut",
     [ ("zs", [
        "∀i₆₂₁₉ ∈ iota n₆₀₆₈ .",
        "    | i₆₂₁₉ == 0 ⇒  xs₆₀₆₉[0]",
        "    | ¬(i₆₂₁₉ == 0) ⇒  1337"
       ])
     ]),
    ("tests/refinement/refine_iterator2.fut",
     [ ("zs", [
        "∀i₆₂₂₃ ∈ iota n₆₁₀₆ .",
        "    | True ⇒  ∑xs₆₀₆₉[0 : i₆₂₂₃]"
       ])
     ]),
    ("tests/refinement/refine_iterator3.fut",
     [ ("zs", [
        "∀i₆₂₂₇ ∈ iota n₆₁₄₂ .",
        "    | i₆₂₂₇ < 0 ⇒  ∑xs₆₁₄₃[0 : i₆₂₂₇]",
        "    | ¬(i₆₂₂₇ < 0) ⇒  i₆₂₂₇"
       ])
     ]),
    ("tests/refinement/refine_iterator4.fut",
     [ ("zs", [
        "∀i₆₂₃₁ ∈ iota n₆₁₇₉ .",
        "    | i₆₂₃₁ == 3 ⇒  ∑j₆₂₂₉∈[0, ..., 3] (xs₆₁₈₀[j₆₂₂₉])",
        "    | ¬(i₆₂₃₁ == 3) ⇒  i₆₂₃₁"
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
  let res = M.mapKeys baseString $ mkIndexFnProg src imports
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
      -- putStrLn $ "Proved: " <> take 100 (show (mkIndexFnProg vns imps)) <> "..."
      -- let valbinds = flip evalState src $
      --                  Defunctorise.transformProg imports
      --                  >>= FullNormalise.transformProg
      -- let res = mkIndexFnProg src valbinds
      let res = mkIndexFnProg src imports
      putStrLn "\nIndex function:\n---------------\n"
      putDoc (pretty res)
    _ -> Nothing

