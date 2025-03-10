module Futhark.Analysis.Properties.EndToEndTests (tests) where

import Control.Monad (forM_, unless)
import Data.Maybe (mapMaybe)
import Futhark.Analysis.Properties.Convert
import Futhark.Analysis.Properties.IndexFn
import Futhark.Analysis.Properties.IndexFnPlus (intervalEnd, domainEnd)
import Futhark.Analysis.Properties.Monad
import Futhark.Analysis.Properties.Prove
import Futhark.Analysis.Properties.Query
import Futhark.Analysis.Properties.Rewrite (rewrite)
import Futhark.Compiler.CLI (fileProg, readProgramOrDie)
import Futhark.SoP.SoP (int2SoP, (.-.))
import Futhark.Util.Pretty (docStringW, line, pretty, (<+>))
import Language.Futhark qualified as E
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Properties.EndToEnd"
    [ mkTest
        "tests/indexfn/part2indices.fut"
        ( \[_pivot, fn@(IndexFn (Forall _ (Iota n)) _)] -> do
            proveFn (PermutationOfZeroTo (n .-. int2SoP 1)) fn
        )
        Yes,
      mkTest
        "tests/indexfn/part2indices.fut"
        ( \[_pivot, fn@(IndexFn (Forall _ (Iota n)) _)] -> do
            proveFn (PInjectiveRCD (int2SoP 0, n)) fn
        )
        Yes,
      mkTest
        "tests/indexfn/part2indices.fut"
        ( \[_pivot, fn@(IndexFn (Forall _ (Iota _)) _)] -> do
            proveFn (PInjectiveRCD (int2SoP 0, int2SoP 3)) fn
        )
        Yes,
      mkTest
        "tests/indexfn/dummyindices.fut"
        ( \[fn@(IndexFn (Forall _ (Iota n)) _)] -> do
            proveFn (PermutationOfZeroTo (n .-. int2SoP 1)) fn
        )
        Unknown,
      mkTest
        "tests/indexfn/dummyindices2.fut"
        ( \[fn@(IndexFn (Forall _ (Iota n)) _)] -> do
            proveFn (PermutationOfZeroTo (n .-. int2SoP 1)) fn
        )
        Unknown,
      mkTest
        "tests/indexfn/part2indices_numeric_conds.fut"
        ( \[fn@(IndexFn (Forall _ (Iota n)) _)] -> do
            proveFn (PermutationOfZeroTo (n .-. int2SoP 1)) fn
        )
        Yes,
      mkTest
        "tests/indexfn/part2indices_predicatefn.fut"
        ( \[fn@(IndexFn (Forall _ (Iota n)) _)] -> do
            proveFn (PermutationOfZeroTo (n .-. int2SoP 1)) fn
        )
        Yes,
      mkTest
        "tests/indexfn/part2indices_predicatefn2.fut"
        ( \[fn@(IndexFn (Forall _ (Iota n)) _)] -> do
            proveFn (PermutationOfZeroTo (n .-. int2SoP 1)) fn
        )
        Yes,
      mkTest
        "tests/indexfn/part3indices.fut"
        ( \[fn@(IndexFn (Forall _ (Iota n)) _)] -> do
            proveFn (PermutationOfZeroTo (n .-. int2SoP 1)) fn
        )
        Yes,
      mkTest
        "tests/indexfn/part2indicesL.fut"
        ( \(fn@(IndexFn (Forall _ dom@(Cat _ _ start)) _) : _) -> do
            end <- rewrite $ intervalEnd dom
            proveFn (ForallSegments $ \_ -> PermutationOfRange start end) fn
        )
        Yes
    ]
  where
    mkTest programFile action expected = testCase programFile $ do
      (_, imports, vns) <- readProgramOrDie programFile
      let last_import = case reverse imports of
            [] -> error "No imports"
            x : _ -> x
      let vbs = getValBinds last_import
      let actual = fst . flip runIndexFnM vns $ do
            forM_ (init vbs) mkIndexFnValBind
            indexfn <- mkIndexFnValBind (last vbs)
            action indexfn
      actual @??= expected

    getValBinds = mapMaybe getValBind . E.progDecs . fileProg . snd

    getValBind (E.ValDec vb) = Just vb
    getValBind _ = Nothing

    actual @??= expected = unless (actual == expected) (assertFailure msg)
      where
        msg =
          docStringW 120 $
            "expected:" <+> pretty expected <> line <> "but got: " <+> pretty actual
