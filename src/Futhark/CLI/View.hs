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
        "    | conds₆₀₇₀[i₆₁₇₆] ⇒  -1 + ∑⟦conds₆₀₇₀⟧[0 : i₆₁₇₆]",
        "    | ¬(conds₆₀₇₀[i₆₁₇₆]) ⇒  i₆₁₇₆ + ∑⟦conds₆₀₇₀⟧[1 + i₆₁₇₆ : -1 + n₆₀₆₈]"
       ])
     ]),
    ("tests/refinement/part2indices_let_inside_map.fut",
     [ ("inds", [
        "∀i₆₁₇₆ ∈ iota n₆₀₆₈ .",
        "    | conds₆₀₇₀[i₆₁₇₆] ⇒  -1 + ∑⟦conds₆₀₇₀⟧[0 : i₆₁₇₆]",
        "    | ¬(conds₆₀₇₀[i₆₁₇₆]) ⇒  i₆₁₇₆ + ∑⟦conds₆₀₇₀⟧[1 + i₆₁₇₆ : -1 + n₆₀₆₈]"
       ])
     ]),
    ("tests/refinement/part2indices_neg_conds.fut",
     [ ("inds", [
        "∀i₆₁₇₆ ∈ iota n₆₀₆₈ .",
        "    | conds₆₀₇₀[i₆₁₇₆] ⇒  -1 + ∑⟦conds₆₀₇₀⟧[0 : i₆₁₇₆]",
        "    | ¬(conds₆₀₇₀[i₆₁₇₆]) ⇒  i₆₁₇₆ + ∑⟦conds₆₀₇₀⟧[1 + i₆₁₇₆ : -1 + n₆₀₆₈]"
       ])
     ]),
    ("tests/refinement/part2indices_scan_exc.fut",
     [ ("inds", [
        "∀i₆₂₀₄ ∈ iota n₆₀₆₈ .",
        "    | conds₆₀₇₀[i₆₂₀₄] ⇒  ∑⟦conds₆₀₇₀⟧[0 : -1 + i₆₂₀₄]",
        "    | ¬(conds₆₀₇₀[i₆₂₀₄]) ⇒  i₆₂₀₄ + ∑⟦conds₆₀₇₀⟧[1 + i₆₁₇₆ : -1 + n₆₀₆₈]"
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
        "∀i₆₂₂₃ ∈ iota n₆₁₀₆ .",
        "    | True ⇒  ∑xs₆₀₆₉[0 : i₆₂₂₃]"
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
        "    | i₆₂₃₁ == 3 ⇒  ∑xs₆₁₈₀[0 : 3]",
        "    | i₆₂₃₁ /= 3 ⇒  i₆₂₃₁"
       ])
     ]),
    ("tests/refinement/mkFlagArray.fut",
     [ ("res", [
        "∀i₆₁₈₈ ∈ ⊎k₆₁₉₁=iota m₆₀₆₉ [∑shape₆₀₈₁[0 : -1 + k₆₁₉₁], ..., ∑shape₆₀₈₁[0 : k₆₁₉₁]) .",
        "    | i₆₁₈₈ == ∑shape₆₀₈₁[0 : -1 + k₆₁₉₁] ⇒  xs₆₀₈₂[∑shape₆₀₈₁[0 : -1 + k₆₁₉₁]]",
        "    | i₆₁₈₈ /= ∑shape₆₀₈₁[0 : -1 + k₆₁₉₁] ⇒  zero₆₀₇₀"
       ])
     ]),
    ("tests/refinement/sgmsum.fut",
     [ ("ys", [
        "∀i₆₁₉₆ ∈ ⊎k₆₁₉₉=iota m₆₀₆₉ [∑shape₆₀₈₁[0 : -1 + k₆₁₉₉], ..., ∑shape₆₀₈₁[0 : k₆₁₉₉]) .",
        "    | True ⇒  ∑xs₆₀₈₂[∑shape₆₀₈₁[0 : -1 + k₆₁₉₉] : i₆₁₉₆ + ∑shape₆₀₈₁[0 : -1 + k₆₁₉₉]]"
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
  then putStrLn $ "Test passed: " ++ file
  else error $ "Test failed: " ++ file
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

