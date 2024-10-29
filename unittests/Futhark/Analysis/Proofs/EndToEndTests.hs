module Futhark.Analysis.Proofs.EndToEndTests (tests) where

import Control.Monad (unless)
import Data.Maybe (mapMaybe)
import Futhark.Analysis.Proofs.Convert
import Futhark.Analysis.Proofs.IndexFn
import Futhark.Analysis.Proofs.Monad
import Futhark.Analysis.Proofs.Query
import Futhark.Compiler.CLI (Imports, fileProg, readProgramOrDie)
import Futhark.Util.Pretty (docString, line, pretty, prettyString, (<+>))
import Language.Futhark qualified as E
import Test.Tasty
import Test.Tasty.HUnit

-- Doubly last: get the last ValBind in the last import.
getLastValBind :: Imports -> E.ValBind
getLastValBind imports = case reverse imports of
  [] -> error "No imports"
  finalImport : _ ->
    last . mapMaybe getValBind . E.progDecs . fileProg . snd $ finalImport
  where
    getValBind (E.ValDec vb) = Just vb
    getValBind _ = Nothing

tests :: TestTree
tests =
  testGroup
    "Proofs.EndToEnd"
    [ mkTest
        "tests/indexfn/part2indices.fut"
        ( \fn@(IndexFn (Forall _ (Iota n)) _) -> do
            prove (PermutationOfZeroTo n) fn
        )
        Yes
    ]
  where
    mkTest programFile action expected = testCase programFile $ do
      (_, imports, vns) <- readProgramOrDie programFile
      let vb = getLastValBind imports
      let actual = fst . flip runIndexFnM vns $ do
            indexfn <- mkIndexFnValBind vb
            case indexfn of
              Nothing -> pure Nothing
              Just indexfn' -> Just <$> action indexfn'
      case actual of
        Nothing ->
          assertFailure $ "Failed to make index fn for " <> prettyString vb
        Just actual' ->
          actual' @??= expected

    actual @??= expected = unless (actual == expected) (assertFailure msg)
      where
        msg =
          docString $
            "expected:" <+> pretty expected <> line <> "but got: " <+> pretty actual
