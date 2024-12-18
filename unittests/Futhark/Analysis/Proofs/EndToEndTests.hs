module Futhark.Analysis.Proofs.EndToEndTests (tests) where

import Control.Monad (forM_, unless)
import Data.Maybe (mapMaybe)
import Futhark.Analysis.Proofs.Convert
import Futhark.Analysis.Proofs.IndexFn
import Futhark.Analysis.Proofs.Monad
import Futhark.Analysis.Proofs.Query
import Futhark.Compiler.CLI (fileProg, readProgramOrDie)
import Futhark.SoP.SoP (int2SoP, (.-.))
import Futhark.Util.Pretty (docStringW, line, pretty, (<+>))
import Language.Futhark qualified as E
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Proofs.EndToEnd"
    [ mkTest
{--
        "tests/indexfn/part2indices.fut"
        ( \fn@(IndexFn (Forall _ (Iota n)) _) -> do
            prove (PermutationOfZeroTo (n .-. int2SoP 1)) fn
        )
        Yes,
      mkTest
        "tests/indexfn/dummyindices.fut"
        ( \fn@(IndexFn (Forall _ (Iota n)) _) -> do
            prove (PermutationOfZeroTo (n .-. int2SoP 1)) fn
        )
        Unknown,
      mkTest
        "tests/indexfn/dummyindices2.fut"
        ( \fn@(IndexFn (Forall _ (Iota n)) _) -> do
            prove (PermutationOfZeroTo (n .-. int2SoP 1)) fn
        )
        Unknown,
      mkTest
        "tests/indexfn/part2indices_numeric_conds.fut"
        ( \fn@(IndexFn (Forall _ (Iota n)) _) -> do
            prove (PermutationOfZeroTo (n .-. int2SoP 1)) fn
        )
        Yes,
      mkTest
        "tests/indexfn/part2indices_predicatefn.fut"
        ( \fn@(IndexFn (Forall _ (Iota n)) _) -> do
            prove (PermutationOfZeroTo (n .-. int2SoP 1)) fn
        )
        Yes,
      mkTest
        "tests/indexfn/part2indices_predicatefn2.fut"
        ( \fn@(IndexFn (Forall _ (Iota n)) _) -> do
            prove (PermutationOfZeroTo (n .-. int2SoP 1)) fn
        )
        Yes,
      mkTest
--}
        "tests/indexfn/part3indices.fut"
        ( \fn@(IndexFn (Forall _ (Iota n)) _) -> do
            debugOn
            prove (PermutationOfZeroTo (n .-. int2SoP 1)) fn
        )
{--
        Yes,
      mkTest
        "tests/indexfn/part2indicesL.fut"
        ( \fn@(IndexFn (Forall _ dom@(Cat _ _ start)) _) -> do
            end <- rewrite $ intervalEnd dom
            prove (ForallSegments $ \_ -> PermutationOfRange start end) fn
        )
        Yes,
      mkTest
        "tests/indexfn/part2indicesL.fut"
        ( \fn -> do
            debugM "TEMPORARY CASE TEMPORARY CASE TEMPORARY CASE TEMPORARY CASE TEMPORARY CASE TEMPORARY CASE TEMPORARY CASE TEMPORARY CASE TEMPORARY CASE"
            -- XXX Temporary madness until we have part2indicesL on the desired form.
            csL <- newNameFromString "csL"
            shape <- newNameFromString "shape"
            j <- newNameFromString "j"
            i <- newNameFromString "i"
            m <- newNameFromString "m"
            k <- newNameFromString "k"
            b <- newNameFromString "b"
            let sHole = sym2SoP . Hole
            let int = int2SoP
                csL_i = Idx (Hole csL) (sHole i)
                seg_k_start = sym2SoP $ Sum j (int 0) (sHole k .-. int 1) (Idx (Hole shape) (sHole j))
                seg_k_end = int (-1) .+. sym2SoP (Sum j (int 0) (sHole k) (Idx (Hole shape) (sHole j)))
                actual_pat =
                  IndexFn
                    { iterator = Forall i (Cat k (sHole m) (sHole b)),
                      body =
                        cases
                          [ ( csL_i,
                              -- offset at segment k
                              seg_k_start
                                -- number of trues in this segment up to and including current index
                                .+. sym2SoP (Sum j (int 1 .+. seg_k_start) (sHole i) (Idx (Hole csL) (sHole j)))
                                .+. sym2SoP (Idx (Hole csL) seg_k_start)
                                -- minus 1 (remember that current index csL[i] is true)
                                .-. int 1
                            ),
                            ( neg csL_i,
                              -- global index
                              sHole i
                                -- plus number of trues that come after this index in the current segment
                                .+. sym2SoP (Sum j (seg_k_start .+. int 1) seg_k_end (Idx (Hole csL) (sHole j)))
                                .-. sym2SoP (Sum j (seg_k_start .+. int 1) (sHole i) (Idx (Hole csL) (sHole j)))
                            )
                          ]
                    }
                desired_pat =
                  IndexFn
                    { iterator = Forall i (Cat k (sHole m) (sHole b)),
                      body =
                        cases
                          [ ( csL_i,
                              -- offset at segment k
                              seg_k_start
                                -- number of trues in this segment up to and including current index
                                .+. sym2SoP (Sum j seg_k_start (sHole i) (Idx (Hole csL) (sHole j)))
                                -- minus 1 (remember that current index csL[i] is true)
                                .-. int 1
                            ),
                            ( neg csL_i,
                              -- global index
                              sHole i
                                -- plus number of trues that come after this index in the current segment
                                .+. sym2SoP (Sum j (sHole i .+. int 1) seg_k_end (Idx (Hole csL) (sHole j)))
                            )
                          ]
                    }
            s <- fromJust <$> unify actual_pat fn
            debugPrettyM "SUB SUB SUB SUB SUB SUB SUB SUB SUB SUB SUB SUB SUB SUB\n" s
            fn_desired_form <- subIndexFn s desired_pat
            let IndexFn (Forall _ dom@(Cat _ _ start)) _ = fn_desired_form
            end <- rewrite $ intervalEnd dom
            prove (ForallSegments $ \_ -> PermutationOfRange start end) fn_desired_form
        )
--}
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
