module Futhark.Analysis.Properties.Artifact (tests) where

import Control.Monad (forM, forM_, unless, when)
import Data.Maybe (mapMaybe)
import Futhark.Analysis.Properties.Convert
import Futhark.Analysis.Properties.IndexFn
import Futhark.Analysis.Properties.IndexFnPlus (subIndexFn)
import Futhark.Analysis.Properties.Monad
import Futhark.Analysis.Properties.Symbol (Symbol (..))
import Futhark.Analysis.Properties.Unify (renameSame, unify)
import Futhark.Compiler.CLI (fileProg, readProgramOrDie)
import Futhark.MonadFreshNames (newNameFromString)
import Futhark.SoP.SoP (int2SoP, sym2SoP, (.-.))
import Futhark.Util.Pretty (docStringW, line, pretty, (<+>))
import Language.Futhark qualified as E
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Properties.Artifact"
    [ mkTest
        "tests/indexfn/fft.fut"
        ( pure $ \(i, n, xs, _) ->
            -- Match anything; we test whether the intermediate analysis is OK.
            [ IndexFn
                { shape = [[Forall i (Iota (sHole n))]],
                  body = cases [(Bool True, sym2SoP $ Hole xs)]
                }
            ]
        ),
      mkTest
        "tests/indexfn/partition.fut"
        ( newNameFromString "j" >>= \j ->
            newNameFromString "p" >>= \p -> pure $ \(i, n, xs, is_inv) ->
              [ IndexFn
                  { shape = [],
                    body =
                      cases
                        [ ( Bool True,
                            sym2SoP (Sum j (int2SoP 0) (sHole n .-. int2SoP 1) (Apply (Hole p) [sym2SoP $ Apply (Hole xs) [sHole j]]))
                          )
                        ]
                  },
                IndexFn
                  { shape = [[Forall i (Iota (sHole n))]],
                    body =
                      cases
                        [(Bool True, sym2SoP $ Apply (Hole xs) [sym2SoP $ Apply (Hole is_inv) [sHole i]])]
                  }
              ]
        ),
      mkTest
        "tests/indexfn/seg_partition.fut"
        ( pure $ \(i, n, xs, is_inv) ->
            [ IndexFn
                { shape = [[Forall i (Iota (sHole n))]],
                  body =
                    cases
                      [(Bool True, sym2SoP $ Apply (Hole xs) [sym2SoP $ Apply (Hole is_inv) [sHole i]])]
                }
            ]
        ),
      mkTest
        "tests/indexfn/partition3.fut"
        ( pure $ \(i, n, xs, is_inv) ->
            [ IndexFn
                { shape = [[Forall i (Iota (sHole n))]],
                  body =
                    cases
                      [(Bool True, sym2SoP $ Apply (Hole xs) [sym2SoP $ Apply (Hole is_inv) [sHole i]])]
                }
            ]
        ),
      mkTest
        "tests/indexfn/filter.fut"
        ( pure $ \(i, n, xs, is_inv) ->
            [ IndexFn
                { shape = [[Forall i (Iota (sHole n))]],
                  body =
                    cases
                      [(Bool True, sym2SoP $ Apply (Hole xs) [sym2SoP $ Apply (Hole is_inv) [sHole i]])]
                }
            ]
        ),
      mkTest
        "tests/indexfn/filter_segmented_array.fut"
        ( pure $ \(i, n, safe_scatter, _) ->
            [ IndexFn
                { shape = [[Forall i (Iota (sHole n))]],
                  body =
                    cases
                      [(Bool True, sym2SoP $ Apply (Hole safe_scatter) [sHole i])]
                }
            ]
        ),
      mkTest
        "tests/indexfn/maxMatch_2d.fut"
        ( pure $ \(i, n, is_inv, _) ->
            [ IndexFn
                { shape = [[Forall i (Iota (sHole n))]],
                  body =
                    cases
                      [(Bool True, sym2SoP $ Apply (Hole is_inv) [sHole i])]
                }
            ]
        ),
      mkTest
        "tests/indexfn/kmeans_kernel.fut"
        ( pure $ \(anything, _, _, _) ->
            -- Match anything here; this test merely checks bounds in the program.
            [ IndexFn
                { shape = [],
                  body =
                    cases
                      [(Bool True, sHole anything)]
                }
            ]
        ),
      mkTest
        "tests/indexfn/primes.fut"
        ( pure $ \(i, n, xs, _) ->
            [ IndexFn
                { shape = [[Forall i (Iota (sHole n))]],
                  -- matches anything; we're just checking the program.
                  body = cases [(Bool True, sHole xs)]
                }
            ]
        )
    ]
  where
    mkTest programFile expectedPat = testCase (basename programFile) $ do
      (_, imports, vns) <- readProgramOrDie programFile
      let last_import = case reverse imports of
            [] -> error "No imports"
            x : _ -> x
      let vbs = getValBinds last_import
      let (actuals, expecteds) = unzip $ runTest vns vbs expectedPat
      when (null actuals) $
        assertFailure "The last value binding does not create an index function."
      actuals @??= expecteds

    basename = drop (length prefix)
      where
        prefix :: String = "tests/indexfn/"

    getValBinds = mapMaybe getValBind . E.progDecs . fileProg . snd

    getValBind (E.ValDec vb) = Just vb
    getValBind _ = Nothing

    -- We need to make the index function and run unification using
    -- the same VNameSource, otherwise the variables in the index function
    -- are likely to be considered bound quantifier variables.
    runTest vns vbs expectedPat = fst . flip runIndexFnM vns $ do
      i <- newNameFromString "i"
      x <- newNameFromString "h"
      y <- newNameFromString "h"
      z <- newNameFromString "h"
      let preceding_vbs = init vbs
      let last_vb = last vbs
      -- Evaluate expectedPat first for any side effects like debug toggling.
      pat <- expectedPat
      let expecteds = pat (i, x, y, z)
      forM_ preceding_vbs mkIndexFnValBind
      actuals <- mkIndexFnValBind last_vb
      forM (zip expecteds actuals) $ \(expected, actual) -> do
        s <- unify expected actual
        case s of
          Nothing ->
            renameSame actual expected
          Just s' -> do
            e <- subIndexFn s' expected
            renameSame actual e

    sHole = sym2SoP . Hole

    actual @??= expected = unless (actual == expected) (assertFailure msg)
      where
        msg = do
          docStringW 120 $
            "expected:" <+> pretty expected <> line <> "but got: " <+> pretty actual
