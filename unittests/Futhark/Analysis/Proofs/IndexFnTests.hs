module Futhark.Analysis.Proofs.IndexFnTests (tests) where

import Control.Monad (unless)
import Data.Maybe (mapMaybe)
import Futhark.Analysis.Proofs.Convert
import Futhark.Analysis.Proofs.IndexFn
import Futhark.Analysis.Proofs.IndexFnPlus (subIndexFn)
import Futhark.Analysis.Proofs.Symbol (Symbol (..))
import Futhark.Analysis.Proofs.Unify (renameSame, unify)
import Futhark.Compiler.CLI (Imports, fileProg, readProgramOrDie)
import Futhark.MonadFreshNames (newNameFromString)
import Futhark.SoP.SoP (int2SoP, sym2SoP, (.*.), (.+.), (.-.))
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
    "Proofs.IndexFn"
    [ mkTest
        "tests/indexfn/map.fut"
        ( pure $ \(i, n, xs, _) ->
            IndexFn
              { iterator = Forall i (Iota (sHole n)),
                body = cases [(Bool True, int2SoP 2 .*. sym2SoP (Idx (Hole xs) (sHole i)))]
              }
        ),
      mkTest
        "tests/indexfn/map-if.fut"
        ( pure $ \(i, n, xs, _) ->
            let xs_i = sym2SoP (Idx (Hole xs) (sHole i))
             in IndexFn
                  { iterator = Forall i (Iota (sHole n)),
                    body =
                      cases
                        [ (xs_i :> int2SoP 100, int2SoP 2 .*. xs_i),
                          (xs_i :<= int2SoP 100, xs_i)
                        ]
                  }
        ),
      mkTest
        "tests/indexfn/scalar.fut"
        ( pure $ \(i, _, x, _) ->
            IndexFn
              { iterator = Forall i (Iota (sHole x)),
                body = cases [(Bool True, int2SoP 2 .*. sHole x)]
              }
        ),
      mkTest
        "tests/indexfn/scan.fut"
        ( pure $ \(i, n, xs, j) ->
            IndexFn
              { iterator = Forall i (Iota (sHole n)),
                body =
                  cases
                    [ ( Bool True,
                        sym2SoP $
                          LinComb j (int2SoP 0) (sHole i) (Indicator (Idx (Hole xs) (sHole j)))
                      )
                    ]
              }
        ),
      mkTest
        "tests/indexfn/scan2.fut"
        ( pure $ \(i, n, xs, j) ->
            IndexFn
              { iterator = Forall i (Iota (sHole n)),
                body =
                  cases
                    [ ( Bool True,
                        int2SoP 1 .+. sHole i .-. sym2SoP (LinComb j (int2SoP 0) (sHole i) (Indicator (Idx (Hole xs) (sHole j))))
                      )
                    ]
              }
        ),
      mkTest
        "tests/indexfn/scalar2.fut"
        ( pure $ \(_, n, xs, j) ->
            IndexFn
              { iterator = Empty,
                body =
                  cases
                    [ ( Bool True,
                        sym2SoP $
                          LinComb j (int2SoP 0) (sHole n .-. int2SoP 1) (Indicator (Idx (Hole xs) (sHole j)))
                      )
                    ]
              }
        ),
      mkTest
        "tests/indexfn/part2indices.fut"
        ( pure $ \(i, n, xs, j) ->
            let xs_i = Idx (Hole xs) (sHole i)
             in IndexFn
                  { iterator = Forall i (Iota (sHole n)),
                    body =
                      cases
                        [ ( xs_i,
                            int2SoP (-1) .+. sym2SoP (LinComb j (int2SoP 0) (sHole i) (Indicator (Idx (Hole xs) (sHole j))))
                          ),
                          ( Not xs_i,
                            sHole i .+. sym2SoP (LinComb j (sHole i .+. int2SoP 1) (sHole n .-. int2SoP 1) (Indicator (Idx (Hole xs) (sHole j))))
                          )
                        ]
                  }
        ),
      mkTest
        "tests/indexfn/map2.fut"
        ( pure $ \(i, n, h1, h2) ->
            IndexFn
              { iterator = Forall i (Iota (sHole n)),
                body =
                  cases
                    [(Bool True, sym2SoP $ Idx (Hole h1) (sHole h2 .-. int2SoP 1))]
              }
        ),
      mkTest
        "tests/indexfn/part2indices_numeric_conds.fut"
        ( pure $ \(i, n, xs, j) ->
            let xs_i = Idx (Hole xs) (sHole i)
             in IndexFn
                  { iterator = Forall i (Iota (sHole n)),
                    body =
                      cases
                        [ ( xs_i,
                            int2SoP (-1) .+. sym2SoP (LinComb j (int2SoP 0) (sHole i) (Indicator (Idx (Hole xs) (sHole j))))
                          ),
                          ( Not xs_i,
                            sHole i .+. sym2SoP (LinComb j (sHole i .+. int2SoP 1) (sHole n .-. int2SoP 1) (Indicator (Idx (Hole xs) (sHole j))))
                          )
                        ]
                  }
        )
    ]
  where
    -- mkTest :: String -> IndexFn -> TestTree
    mkTest programFile expectedPat = testCase programFile $ do
      (_, imports, vns) <- readProgramOrDie programFile
      let vb = getLastValBind imports
      case runTest vns vb expectedPat of
        Just (actual, expected) -> actual @??= expected
        _ -> assertFailure $ "Failed to make index fn for " <> prettyString vb

    -- We need to make the index function and run unification using
    -- the same VNameSource, otherwise the variables in the index function
    -- are likely to be considered bound quantifier variables.
    runTest vns vb expectedPat = fst . flip runIndexFnM vns $ do
      i <- newNameFromString "i"
      x <- newNameFromString "h"
      y <- newNameFromString "h"
      z <- newNameFromString "h"
      -- Evaluate expectedPat first for any side effects like debug toggling.
      pat <- expectedPat
      let expected = pat (i, x, y, z)
      debugM (prettyString expected)
      indexfn <- mkIndexFnValBind vb
      case indexfn of
        Nothing -> pure Nothing
        Just actual -> do
          s <- unify expected actual
          case s of
            Nothing -> pure $ Just (actual, expected)
            Just s' -> do
              e <- subIndexFn s' expected
              Just <$> renameSame actual e

    sHole = sym2SoP . Hole

    actual @??= expected = do
      unless (actual == expected) (assertFailure msg)
      where
        msg =
          docString $
            "expected:" <+> pretty expected <> line <> "but got: " <+> pretty actual