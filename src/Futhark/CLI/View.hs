{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Futhark.CLI.View (main) where

import Control.Monad
import Control.Monad.IO.Class
import Futhark.Analysis.View
import Futhark.Analysis.View.Latex
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

data RefineConfig = RefineConfig
  { checkWarn :: Bool,
    laTeX :: Maybe FilePath
  }

newRefineConfig :: RefineConfig
newRefineConfig = RefineConfig True Nothing

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
        "    | xs₆₀₇₀[i₆₁₂₀] <= 0 && xs₆₀₇₀[0] > 1337 ⇒  -1 + xs₆₀₇₀[i₆₁₂₀]",
        "    | xs₆₀₇₀[i₆₁₂₀] > 1 && xs₆₀₇₀[0] <= 1337 ⇒  xs₆₀₇₀[i₆₁₂₀]",
        "    | xs₆₀₇₀[i₆₁₂₀] <= 1 && xs₆₀₇₀[0] <= 1337 ⇒  -1 + xs₆₀₇₀[i₆₁₂₀]"
       ])
     ]),
    ("tests/refinement/part2indices.fut",
     [ ("inds", [
        "∀i₆₁₇₆ ∈ iota n₆₀₆₈ .",
        "    | conds₆₀₇₀[i₆₁₇₆] ⇒  -1 + ∑j₆₁₇₆∈[0, ..., i₆₁₇₆] (⟦conds₆₀₇₀[j₆₁₇₆]⟧)",
        "    | ¬(conds₆₀₇₀[i₆₁₇₆]) ⇒  i₆₁₇₆ + ∑j₆₁₇₆∈[1 + i₆₁₇₆, ..., -1 + n₆₀₆₈] (⟦conds₆₀₇₀[j₆₁₇₆]⟧)"
       ])
     ]),
    ("tests/refinement/part2indices_let_inside_map.fut",
     [ ("inds", [
        "∀i₆₁₇₆ ∈ iota n₆₀₆₈ .",
        "    | conds₆₀₇₀[i₆₁₇₆] ⇒  -1 + ∑j₆₁₇₆∈[0, ..., i₆₁₇₆] (⟦conds₆₀₇₀[j₆₁₇₆]⟧)",
        "    | ¬(conds₆₀₇₀[i₆₁₇₆]) ⇒  i₆₁₇₆ + ∑j₆₁₇₆∈[1 + i₆₁₈₈, ..., -1 + n₆₀₆₈] (⟦conds₆₀₇₀[j₆₁₇₆]⟧)"
       ])
     ]),
    ("tests/refinement/part2indices_neg_conds.fut",
     [ ("inds", [
        "∀i₆₁₇₆ ∈ iota n₆₀₆₈ .",
        "    | conds₆₀₇₀[i₆₁₇₆] ⇒  -1 + ∑j₆₁₇₆∈[0, ..., i₆₁₇₆] (⟦conds₆₀₇₀[j₆₁₇₆]⟧)",
        "    | ¬(conds₆₀₇₀[i₆₁₇₆]) ⇒  i₆₁₇₆ + ∑j₆₁₇₆∈[1 + i₆₁₈₈, ..., -1 + n₆₀₆₈] (⟦conds₆₀₇₀[j₆₁₇₆]⟧)"
       ])
     ]),
    ("tests/refinement/part2indices_scan_exc.fut",
     [ ("inds", [
        "∀i₆₁₇₆ ∈ iota n₆₀₆₈ .",
        "    | conds₆₀₇₀[i₆₁₇₆] ⇒  ∑j₆₁₇₆∈[0, ..., -1 + i₆₁₇₆] (⟦conds₆₀₇₀[j₆₁₇₆]⟧)",
        "    | ¬(conds₆₀₇₀[i₆₁₇₆]) ⇒  i₆₁₇₆ + ∑j₆₁₇₆∈[1 + i₆₁₇₆, ..., -1 + n₆₀₆₈] (⟦conds₆₀₇₀[j₆₁₇₆]⟧)"
       ])
     ]),
    ("tests/refinement/refine_iterator1.fut",
     [ ("zs", [
        "∀i₆₂₁₉ ∈ iota n₆₀₆₈ .",
        "    | i₆₂₁₉ == 0 ⇒  xs₆₀₆₉[0]",
        "    | i₆₂₁₉ /= 0 ⇒  1337"
       ])
     ]),
    ("tests/refinement/refine_iterator2.fut",
     [ ("zs", [
        "∀i₆₁₀₇ ∈ iota n₆₀₆₈ .",
        "    | True ⇒  ∑j₆₁₀₆∈[0, ..., i₆₁₀₇] (xs₆₀₆₉[j₆₁₀₆])"
       ])
     ]),
    ("tests/refinement/refine_iterator3.fut",
     [ ("zs", [
        "∀i₆₂₂₇ ∈ iota n₆₁₄₂ .",
        "    | True ⇒  i₆₂₂₇"
       ])
     ]),
    ("tests/refinement/refine_iterator4.fut",
     [ ("zs", [
        "∀i₆₂₃₁ ∈ iota n₆₁₇₉ .",
        "    | i₆₂₃₁ == 3 ⇒  ∑j₆₁₀₇∈[0, ..., 3] (xs₆₀₆₉[j₆₁₀₇])",
        "    | i₆₂₃₁ /= 3 ⇒  i₆₂₃₁"
       ])
     ]),
    ("tests/refinement/mkFlagArray.fut",
     [ ("res", [
        "∀i₆₁₈₉ ∈ ⊎k₆₁₉₂=iota m₆₀₆₉ [∑j₆₁₇₃∈[0, ..., -1 + k₆₁₉₂] (shape₆₀₈₁[j₆₁₇₃]), ..., ∑j₆₁₇₃∈[0, ..., k₆₁₉₂] (shape₆₀₈₁[j₆₁₇₃])) .",
        "    | i₆₁₈₉ == ∑j₆₁₇₃∈[0, ..., -1 + k₆₁₉₂] (shape₆₀₈₁[j₆₁₇₃]) ⇒  xs₆₀₈₂[k₆₁₉₂]",
        "    | i₆₁₈₉ /= ∑j₆₁₇₃∈[0, ..., -1 + k₆₁₉₂] (shape₆₀₈₁[j₆₁₇₃]) ⇒  zero₆₀₇₀"
       ])
     ]),
    ("tests/refinement/sgmsum.fut",
     [ ("ys", [
        "∀i₆₁₉₇ ∈ ⊎k₆₂₀₀=iota m₆₀₆₉ [∑j₆₁₈₈∈[0, ..., -1 + k₆₂₀₀] (shape₆₀₈₁[j₆₁₈₈]), ..., ∑j₆₁₈₈∈[0, ..., k₆₂₀₀] (shape₆₀₈₁[j₆₁₈₈])) .",
        "    | True ⇒  ∑j₆₂₁₄∈[∑j₆₁₈₈∈[0, ..., -1 + k₆₂₀₀] (shape₆₀₈₁[j₆₁₈₈]), ..., i₆₁₉₇] (xs₆₀₈₂[j₆₂₁₄])"
       ])
     ]),
    ("tests/refinement/use_previous.fut",
     [ ("fun2", [
        "∀i₆₁₇₆ ∈ iota (-1 + n₆₀₆₈) .",
        "    | True ⇒  2674 + 2*inputs₆₀₇₀[1 + i₆₁₇₆]"
       ]),
       ("fun3", [
        "∀i₆₁₇₆ ∈ iota (-1 + n₆₀₆₈) .",
        "    | True ⇒  2674"
       ]),
       ("fun4", [
        "∀i₆₁₇₆ ∈ iota n₆₀₆₈ .",
        "    | True ⇒  2*∑j₆₂₃₇∈[0, ..., i₆₁₇₆] (inputs₆₁₇₄[j₆₂₃₇])"
       ]),
       ("fun5", [
        "∀i₆₁₇₆ ∈ iota n₆₀₆₈ .",
        "    | True ⇒  2 + 2*i₆₁₇₆"
       ])
     ]),
    ("tests/refinement/mkseg.fut",
     [ ("mkseg", [
        "∀i₆₁₈₉ ∈ ⊎k₆₁₉₂=iota m₆₀₆₉ [∑j₆₁₇₃∈[0, ..., -1 + k₆₁₉₂] (shape₆₀₈₁[j₆₁₇₃]), ..., ∑j₆₁₇₃∈[0, ..., k₆₁₉₂] (shape₆₀₈₁[j₆₁₇₃])) .",
        "    | True ⇒  k₆₁₉₂"
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
  let (res, _) = mkIndexFnProg src imports
  let passed = all (checkTest . M.mapKeys baseString $ res) expected
  if passed
  then putStrLn $ "\ESC[1;32mTest passed:\ESC[0m " ++ file
  else error $ "\ESC[1;31mTest failed:\ESC[0m " ++ file
  where
    checkTest actuals (name, expected_indxfn) =
      let actual = actuals M.! name
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
      let (res, log) = mkIndexFnProg src imports
      putStrLn "\nIndex function:\n---------------\n"
      putDoc (pretty res)

      case laTeX cfg of
        Just fp -> mkLaTeX fp log
        _ -> pure ()
    _ -> Nothing

