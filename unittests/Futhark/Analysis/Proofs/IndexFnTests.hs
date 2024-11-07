module Futhark.Analysis.Proofs.IndexFnTests (tests) where

import Control.Monad (forM_, unless)
import Data.Maybe (mapMaybe)
import Futhark.Analysis.Proofs.Convert
import Futhark.Analysis.Proofs.IndexFn
import Futhark.Analysis.Proofs.IndexFnPlus (subIndexFn)
import Futhark.Analysis.Proofs.Monad
import Futhark.Analysis.Proofs.Symbol (Symbol (..), neg)
import Futhark.Analysis.Proofs.Unify (renameSame, unify)
import Futhark.Compiler.CLI (fileProg, readProgramOrDie)
import Futhark.MonadFreshNames (newNameFromString)
import Futhark.SoP.SoP (int2SoP, sym2SoP, (.*.), (.+.), (.-.))
import Futhark.Util.Pretty (docString, line, pretty, (<+>))
import Language.Futhark qualified as E
import Test.Tasty
import Test.Tasty.HUnit

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
        "tests/indexfn/map-if-elim.fut"
        ( pure $ \(i, n, xs, _) ->
            let xs_i = sym2SoP (Idx (Hole xs) (sHole i))
             in IndexFn
                  { iterator = Forall i (Iota (sHole n)),
                    body = cases [(Bool True, int2SoP 2 .*. xs_i)]
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
                          Sum j (int2SoP 0) (sHole i) (Idx (Hole xs) (sHole j))
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
                        int2SoP 1 .+. sHole i .-. sym2SoP (Sum j (int2SoP 0) (sHole i) (Idx (Hole xs) (sHole j)))
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
                          Sum j (int2SoP 0) (sHole n .-. int2SoP 1) (Idx (Hole xs) (sHole j))
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
                            sym2SoP (Sum j (int2SoP 0) (sHole i .-. int2SoP 1) (Idx (Hole xs) (sHole j)))
                          ),
                          ( neg xs_i,
                            sHole i .+. sym2SoP (Sum j (sHole i .+. int2SoP 1) (sHole n .-. int2SoP 1) (Idx (Hole xs) (sHole j)))
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
            let xs_i = sym2SoP $ Idx (Hole xs) (sHole i)
                xs_j = sym2SoP $ Idx (Hole xs) (sHole j)
             in IndexFn
                  { iterator = Forall i (Iota (sHole n)),
                    body =
                      cases
                        [ ( xs_i :== int2SoP 1,
                            sym2SoP (Sum j (int2SoP 0) (sHole i .-. int2SoP 1) (xs_j :== int2SoP 1))
                          ),
                          ( xs_i :/= int2SoP 1,
                            sHole i .+. sym2SoP (Sum j (sHole i .+. int2SoP 1) (sHole n .-. int2SoP 1) (xs_j :== int2SoP 1))
                          )
                        ]
                  }
        ),
      mkTest
        "tests/indexfn/part2indices_predicatefn.fut"
        ( newNameFromString "p" >>= \p -> pure $ \(i, n, xs, j) ->
            let xs_i = Apply (Hole p) [sym2SoP $ Idx (Hole xs) (sHole i)]
                xs_j = Apply (Hole p) [sym2SoP $ Idx (Hole xs) (sHole j)]
             in IndexFn
                  { iterator = Forall i (Iota (sHole n)),
                    body =
                      cases
                        [ ( xs_i,
                            sym2SoP (Sum j (int2SoP 0) (sHole i .-. int2SoP 1) xs_j)
                          ),
                          ( neg xs_i,
                            sHole i .+. sym2SoP (Sum j (sHole i .+. int2SoP 1) (sHole n .-. int2SoP 1) xs_j)
                          )
                        ]
                  }
        ),
      mkTest
        "tests/indexfn/part2indices_predicatefn2.fut"
        ( newNameFromString "p" >>= \p -> pure $ \(i, n, xs, j) ->
            let xs_i = Apply (Hole p) [sym2SoP $ Idx (Hole xs) (sHole i)]
                xs_j = Apply (Hole p) [sym2SoP $ Idx (Hole xs) (sHole j)]
             in IndexFn
                  { iterator = Forall i (Iota (sHole n)),
                    body =
                      cases
                        [ ( xs_i,
                            sym2SoP (Sum j (int2SoP 0) (sHole i .-. int2SoP 1) xs_j)
                          ),
                          ( neg xs_i,
                            sHole i .+. sym2SoP (Sum j (sHole i .+. int2SoP 1) (sHole n .-. int2SoP 1) xs_j)
                          )
                        ]
                  }
        ),
      mkTest
        "tests/indexfn/part3indices.fut"
        ( pure $ \(i, n, cs, j) ->
            let cs_i = sym2SoP $ Idx (Hole cs) (sHole i)
                cs_j = sym2SoP $ Idx (Hole cs) (sHole j)
             in IndexFn
                  { iterator = Forall i (Iota (sHole n)),
                    body =
                      cases
                        [ ( cs_i :== int2SoP 2,
                            sym2SoP (Sum j (int2SoP 0) (sHole n .-. int2SoP 1) (cs_j :== int2SoP 1))
                              .+. sym2SoP (Sum j (int2SoP 0) (sHole i .-. int2SoP 1) (cs_j :== int2SoP 2))
                          ),
                          ( cs_i :== int2SoP 1,
                            sym2SoP (Sum j (int2SoP 0) (sHole i .-. int2SoP 1) (cs_j :== int2SoP 1))
                          ),
                          ( (cs_i :/= int2SoP 1) :&& (cs_i :/= int2SoP 2),
                            sHole i
                              .+. sym2SoP (Sum j (sHole i .+. int2SoP 1) (sHole n .-. int2SoP 1) (cs_j :== int2SoP 1))
                              .+. sym2SoP (Sum j (sHole i .+. int2SoP 1) (sHole n .-. int2SoP 1) (cs_j :== int2SoP 2))
                          )
                        ]
                  }
        ),
      mkTest
        "tests/indexfn/mk_flag_array.fut"
        ( newNameFromString "k" >>= \k ->
            newNameFromString "j" >>= \j ->
              newNameFromString "zero" >>= \zero -> pure $ \(i, m, xs, shape) ->
                let sum_km1 = sym2SoP $ Sum j (int2SoP 0) (sVar k .-. int2SoP 1) (Idx (Hole shape) (sVar j))
                    shape_k = sym2SoP (Idx (Hole shape) (sVar k))
                 in IndexFn
                      { iterator = Forall i (Cat k (sHole m) sum_km1),
                        body =
                          cases
                            [ (shape_k :> int2SoP 0, sym2SoP $ Idx (Hole xs) (sVar k)),
                              (shape_k :<= int2SoP 0, sHole zero)
                            ]
                      }
        ),
      mkTest
        "tests/indexfn/map_tuple.fut"
        ( pure $ \(i, n, xs, ys) ->
            let xs_i = sym2SoP $ Idx (Hole xs) (sHole i)
                ys_i = sym2SoP $ Idx (Hole ys) (sHole i)
             in IndexFn
                  { iterator = Forall i (Iota (sHole n)),
                    body = cases [(Bool True, sym2SoP $ Tuple [xs_i .*. ys_i, xs_i .+. ys_i])]
                  }
        ),
      mkTest
        "tests/indexfn/segment_sum.fut"
        ( pure $ \(i, n, xs, flags) ->
            let xs_i = sym2SoP $ Idx (Hole xs) (sHole i)
                flags_i = Idx (Hole flags) (sHole i)
             in IndexFn
                  { iterator = Forall i (Iota (sHole n)),
                    body = cases [(flags_i, xs_i), (Not flags_i, xs_i .+. sym2SoP Recurrence)]
                  }
        ),
      mkTest
        "tests/indexfn/segment_ids.fut"
        ( withDebug . pure $ \(i, n, xs, flags) ->
            let xs_i = sym2SoP $ Idx (Hole xs) (sHole i)
                flags_i = Idx (Hole flags) (sHole i)
             in IndexFn
                  { iterator = Forall i (Iota (sHole n)),
                    body = cases [(flags_i, xs_i), (Not flags_i, xs_i)]
                  }
        )
    ]
  where
    -- mkTest programFile expectedPat = testCase programFile $ do
    --   let config = newFutharkConfig
    --   prog <- flip runFutharkM NotVerbose $ do
    --         (_, prog_imports, namesrc) <- readProgramOrDie programFile
    --         putNameSource namesrc
    --         int_prog <- almostInternaliseProg config prog_imports
    --         namesrc' <- getNameSource
    --         pure (int_prog, namesrc')
    --   case prog of
    --     Left _ -> error "meh"
    --     Right (vbs, vns) -> do
    --       let (actual, expected) = runTest vns vbs expectedPat
    --       actual @??= expected
    mkTest programFile expectedPat = testCase programFile $ do
      (_, imports, vns) <- readProgramOrDie programFile
      let last_import = case reverse imports of
            [] -> error "No imports"
            x : _ -> x
      let vbs = getValBinds last_import
      let (actual, expected) = runTest vns vbs expectedPat
      actual @??= expected

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
      let expected = pat (i, x, y, z)
      forM_ preceding_vbs mkIndexFnValBind
      actual <- mkIndexFnValBind last_vb
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
        msg =
          docString $
            "expected:" <+> pretty expected <> line <> "but got: " <+> pretty actual
