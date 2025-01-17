module Futhark.Analysis.Proofs.IndexFnTests (tests) where

import Control.Monad (forM_, unless, forM)
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
import Futhark.Util.Pretty (docStringW, line, pretty, (<+>))
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
            [IndexFn
              { iterator = Forall i (Iota (sHole n)),
                body = cases [(Bool True, int2SoP 2 .*. sym2SoP (Idx (Hole xs) (sHole i)))]
              }]
        ),
      mkTest
        "tests/indexfn/abs.fut"
        ( withDebug $ pure $ \(_, _, x, _) ->
            [IndexFn
              { iterator = Empty,
                body = cases [(sHole x :< int2SoP 0, int2SoP (-1) .*. sHole x),
                              (sHole x :>= int2SoP 0, sHole x)]
              }]
        ),
      mkTest
        "tests/indexfn/rotate.fut"
        ( withDebug $ pure $ \(i, r, a, n) ->
            let shift = sHole r .+. sHole i
            in [IndexFn
              { iterator = Forall i (Iota (sHole n)),
                body = cases [(shift :< sHole n, sym2SoP $ Idx (Hole a) shift),
                              (shift :>= sHole n, sym2SoP $ Idx (Hole a) (shift .-. sHole n))]
              }]
        ),
      mkTest
        "tests/indexfn/map-tuple.fut"
        ( pure $ \(i, n, xs, ys) ->
            [IndexFn
              { iterator = Forall i (Iota (sHole n)),
                body = cases [(Bool True, int2SoP 2 .+. sym2SoP (Idx (Hole xs) (sHole i)))]
              }
            , IndexFn
              { iterator = Forall i (Iota (sHole n)),
                body = cases [(Bool True, int2SoP 3 .+. sym2SoP (Idx (Hole ys) (sHole i)))]
              }]
        ),
      mkTest
        "tests/indexfn/map-tuple2.fut"
        ( pure $ \(i, n, xs, ys) ->
            let xs_i = sym2SoP $ Idx (Hole xs) (sHole i)
                ys_i = sym2SoP $ Idx (Hole ys) (sHole i)
             in [ IndexFn
                   { iterator = Forall i (Iota (sHole n)),
                     body = cases [(Bool True, xs_i .*. ys_i)]
                   }
                , IndexFn
                  { iterator = Forall i (Iota (sHole n)),
                    body = cases [(Bool True, xs_i .+. ys_i)]
                  }]
        ),
      mkTest
        "tests/indexfn/map-if.fut"
        ( pure $ \(i, n, xs, _) ->
            let xs_i = sym2SoP (Idx (Hole xs) (sHole i))
             in [IndexFn
                  { iterator = Forall i (Iota (sHole n)),
                    body =
                      cases
                        [ (xs_i :> int2SoP 100, int2SoP 2 .*. xs_i),
                          (xs_i :<= int2SoP 100, xs_i)
                        ]
                  }]
        ),
      mkTest
        "tests/indexfn/map-if-elim.fut"
        ( pure $ \(i, n, xs, _) ->
            let xs_i = sym2SoP (Idx (Hole xs) (sHole i))
             in [IndexFn
                  { iterator = Forall i (Iota (sHole n)),
                    body = cases [(Bool True, int2SoP 2 .*. xs_i)]
                  }]
        ),
      mkTest
        "tests/indexfn/scalar.fut"
        ( pure $ \(i, _, x, _) ->
            [IndexFn
              { iterator = Forall i (Iota (sHole x)),
                body = cases [(Bool True, int2SoP 2 .*. sHole x)]
              }]
        ),
      mkTest
        "tests/indexfn/scan.fut"
        ( pure $ \(i, n, xs, j) ->
            [IndexFn
              { iterator = Forall i (Iota (sHole n)),
                body =
                  cases
                    [ ( Bool True,
                        sym2SoP $
                          Sum j (int2SoP 0) (sHole i) (Idx (Hole xs) (sHole j))
                      )
                    ]
              }]
        ),
      mkTest
        "tests/indexfn/scan2.fut"
        ( pure $ \(i, n, xs, j) ->
            [IndexFn
              { iterator = Forall i (Iota (sHole n)),
                body =
                  cases
                    [ ( Bool True,
                        int2SoP 1 .+. sHole i .-. sym2SoP (Sum j (int2SoP 0) (sHole i) (Idx (Hole xs) (sHole j)))
                      )
                    ]
              }]
        ),
      mkTest
        "tests/indexfn/scalar2.fut"
        ( pure $ \(_, n, xs, j) ->
            [IndexFn
              { iterator = Empty,
                body =
                  cases
                    [ ( Bool True,
                        sym2SoP $
                          Sum j (int2SoP 0) (sHole n .-. int2SoP 1) (Idx (Hole xs) (sHole j))
                      )
                    ]
              }]
        ),
      mkTest
        "tests/indexfn/part2indices.fut"
        ( pure $ \(i, n, xs, j) ->
            let xs_i = Idx (Hole xs) (sHole i)
             in [IndexFn
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
                  }]
        ),
      mkTest
        "tests/indexfn/map2.fut"
        ( pure $ \(i, n, h1, h2) ->
            let inds_i = sym2SoP $ Idx (Hole h2) (sHole i)
                p = int2SoP 0 :< inds_i :&& inds_i :<= sHole n
            in [IndexFn
              { iterator = Forall i (Iota (sHole n)),
                body =
                  cases
                    [(p, sym2SoP $ Idx (Hole h1) (inds_i .-. int2SoP 1)),
                     (neg p, int2SoP 0)
                    ]
              }]
        ),
      mkTest
        "tests/indexfn/part2indices_numeric_conds.fut"
        ( pure $ \(i, n, xs, j) ->
            let xs_i = sym2SoP $ Idx (Hole xs) (sHole i)
                xs_j = sym2SoP $ Idx (Hole xs) (sHole j)
             in [IndexFn
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
                  }]
        ),
      mkTest
        "tests/indexfn/part2indices_predicatefn.fut"
        ( newNameFromString "p" >>= \p -> pure $ \(i, n, xs, j) ->
            let xs_i = Apply (Hole p) [sym2SoP $ Idx (Hole xs) (sHole i)]
                xs_j = Apply (Hole p) [sym2SoP $ Idx (Hole xs) (sHole j)]
             in [IndexFn
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
                  }]
        ),
      mkTest
        "tests/indexfn/part2indices_predicatefn2.fut"
        ( newNameFromString "p" >>= \p -> pure $ \(i, n, xs, j) ->
            let xs_i = Apply (Hole p) [sym2SoP $ Idx (Hole xs) (sHole i)]
                xs_j = Apply (Hole p) [sym2SoP $ Idx (Hole xs) (sHole j)]
             in [IndexFn
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
                  }]
        ),
      mkTest
        "tests/indexfn/part3indices.fut"
        ( pure $ \(i, n, cs, j) ->
            let cs_i = sym2SoP $ Idx (Hole cs) (sHole i)
                cs_j = sym2SoP $ Idx (Hole cs) (sHole j)
             in [IndexFn
                  { iterator = Forall i (Iota (sHole n)),
                    body =
                      cases
                        [ ( cs_i :== int2SoP 2,
                            -- Mind the gap in the sums due to the above predicate simplifying a -1 away.
                            sym2SoP (Sum j (sHole i .+. int2SoP 1) (sHole n .-. int2SoP 1) (cs_j :== int2SoP 1))
                              .+. sym2SoP (Sum j (int2SoP 0) (sHole i .-. int2SoP 1) (cs_j :== int2SoP 1))
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
                  }]
        ),
      mkTest
        "tests/indexfn/mk_flag_array.fut"
        ( newNameFromString "k" >>= \k ->
            newNameFromString "j" >>= \j ->
              newNameFromString "zero" >>= \zero -> pure $ \(i, m, xs, shape) ->
                let sum_km1 = sym2SoP $ Sum j (int2SoP 0) (sVar k .-. int2SoP 1) (Idx (Hole shape) (sVar j))
                    sum_mm1 = sym2SoP $ Sum j (int2SoP 0) (sHole m .-. int2SoP 1) (Idx (Hole shape) (sVar j))
                 in [IndexFn
                      { iterator = Empty,
                        body = cases [ (Bool True, sum_mm1) ]
                      }
                    , IndexFn
                      { iterator = Forall i (Cat k (sHole m) sum_km1),
                        body =
                          cases
                            [ (sVar i :== sum_km1, sym2SoP $ Idx (Hole xs) (sVar k)),
                              (sVar i :/= sum_km1, sHole zero)
                            ]
                      }]
        ),
      mkTest
        "tests/indexfn/segment_sum.fut"
        ( pure $ \(i, n, xs, flags) ->
            let xs_i = sym2SoP $ Idx (Hole xs) (sHole i)
                flags_i = Idx (Hole flags) (sHole i)
             in [IndexFn
                  { iterator = Forall i (Iota (sHole n)),
                    body = cases [(flags_i, xs_i), (Not flags_i, xs_i .+. sym2SoP Recurrence)]
                  }]
        ),
      mkTest
        "tests/indexfn/segment_ids.fut"
        ( pure $ \(i, m, k, b) ->
            [IndexFn
              { iterator = Forall i (Cat k (sHole m) (sHole b)),
                body = cases [(Bool True, sHole k)]
              }]
        ),
      mkTest
        "tests/indexfn/part2indicesL.fut"
        ( newNameFromString "csL" >>= \csL ->
            newNameFromString "shape" >>= \shape ->
              newNameFromString "j" >>= \j -> pure $ \(i, m, k, b) ->
                let int = int2SoP
                    csL_i = Idx (Hole csL) (sHole i)
                    seg_k_start = sym2SoP $ Sum j (int 0) (sHole k .-. int 1) (Idx (Hole shape) (sHole j))
                    seg_k_end = int (-1) .+. sym2SoP (Sum j (int 0) (sHole k) (Idx (Hole shape) (sHole j)))
                 in [IndexFn
                      { iterator = Forall i (Cat k (sHole m) (sHole b)),
                        body =
                          cases
                            [ ( csL_i,
                                -- offset at segment k
                                seg_k_start
                                  -- number of trues in this segment up to and including current index
                                  .+. sym2SoP (Sum j seg_k_start (sHole i .-. int 1) (Idx (Hole csL) (sHole j)))
                              ),
                              ( neg csL_i,
                                -- global index
                                sHole i
                                  -- plus number of trues that come after this index in the current segment
                                  .+. sym2SoP (Sum j (sHole i .+. int 1) seg_k_end (Idx (Hole csL) (sHole j)))
                              )
                            ]
                      }]
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
      let (actuals, expecteds) = unzip $ runTest vns vbs expectedPat
      actuals @??= expecteds

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

    sVar = sym2SoP . Var

    actual @??= expected = unless (actual == expected) (assertFailure msg)
      where
        msg = do
          docStringW 120 $
            "expected:" <+> pretty expected <> line <> "but got: " <+> pretty actual
