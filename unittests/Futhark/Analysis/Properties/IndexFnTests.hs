module Futhark.Analysis.Properties.IndexFnTests (tests) where

import Control.Monad (forM, forM_, unless, when)
import Data.Maybe (mapMaybe)
import Futhark.Analysis.Properties.Convert
import Futhark.Analysis.Properties.IndexFn
import Futhark.Analysis.Properties.IndexFnPlus (subIndexFn)
import Futhark.Analysis.Properties.Monad
import Futhark.Analysis.Properties.Symbol (Symbol (..), neg)
import Futhark.Analysis.Properties.Unify (renameSame, unify)
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
    "Properties.IndexFn"
    [ mkTest
        "tests/indexfn/cooley-tukey-fft-par.fut"
        ( pure $ \(i, n, xs, _) ->
            [ IndexFn
                { shape = [Forall i (Iota (sHole n))],
                  body = cases [(Bool True, int2SoP 2 .*. sym2SoP (Apply (Hole xs) [sHole i]))]
                }
            ]
        ),
      mkTest
        "tests/indexfn/map.fut"
        ( pure $ \(i, n, xs, _) ->
            [ IndexFn
                { shape = [Forall i (Iota (sHole n))],
                  body = cases [(Bool True, int2SoP 2 .*. sym2SoP (Apply (Hole xs) [sHole i]))]
                }
            ]
        ),
      mkTest
        "tests/indexfn/scatter_perm.fut"
        ( pure $ \(i, n, xs, _) ->
            [ IndexFn
                { shape = [Forall i (Iota (sHole n))],
                  body =
                    cases
                      [(Bool True, sym2SoP $ Apply (Hole xs) [sHole i])]
                }
            ]
        ),
      mkTest
        "tests/indexfn/reverse.fut"
        ( pure $ \(i, n, xs, sigma) ->
            [ IndexFn
                { shape = [Forall i (Iota (sHole n))],
                  body =
                    cases
                      [(Bool True, sym2SoP $ Apply (Hole xs) [sym2SoP $ Apply (Hole sigma) [sHole i]])]
                }
            ]
        ),
      mkTest
        "tests/indexfn/abs.fut"
        ( pure $ \(_, _, x, _) ->
            [ IndexFn
                { shape = [],
                  body =
                    cases
                      [ (sHole x :< int2SoP 0, int2SoP (-1) .*. sHole x),
                        (sHole x :>= int2SoP 0, sHole x)
                      ]
                }
            ]
        ),
      mkTest
        "tests/indexfn/map-tuple.fut"
        ( pure $ \(i, n, xs, ys) ->
            [ IndexFn
                { shape = [Forall i (Iota (sHole n))],
                  body = cases [(Bool True, int2SoP 2 .+. sym2SoP (Apply (Hole xs) [sHole i]))]
                },
              IndexFn
                { shape = [Forall i (Iota (sHole n))],
                  body = cases [(Bool True, int2SoP 3 .+. sym2SoP (Apply (Hole ys) [sHole i]))]
                }
            ]
        ),
      mkTest
        "tests/indexfn/map-tuple2.fut"
        ( pure $ \(i, n, xs, ys) ->
            let xs_i = sym2SoP $ Apply (Hole xs) [sHole i]
                ys_i = sym2SoP $ Apply (Hole ys) [sHole i]
             in [ IndexFn
                    { shape = [Forall i (Iota (sHole n))],
                      body = cases [(Bool True, xs_i .*. ys_i)]
                    },
                  IndexFn
                    { shape = [Forall i (Iota (sHole n))],
                      body = cases [(Bool True, xs_i .+. ys_i)]
                    }
                ]
        ),
      mkTest
        "tests/indexfn/map-if.fut"
        ( pure $ \(i, n, xs, _) ->
            let xs_i = sym2SoP (Apply (Hole xs) [sHole i])
             in [ IndexFn
                    { shape = [Forall i (Iota (sHole n))],
                      body =
                        cases
                          [ (xs_i :> int2SoP 100, int2SoP 2 .*. xs_i),
                            (xs_i :<= int2SoP 100, xs_i)
                          ]
                    }
                ]
        ),
      mkTest
        "tests/indexfn/map-if-nested.fut"
        ( pure $ \(i, n, xs, _) ->
            let xs_i = sym2SoP (Apply (Hole xs) [sHole i])
             in [ IndexFn
                    { shape = [Forall i (Iota (sHole n))],
                      body =
                        cases
                          [ (xs_i :> int2SoP 100, int2SoP 2 .*. xs_i),
                            (xs_i :<= int2SoP 100, xs_i)
                          ]
                    }
                ]
        ),
      mkTest
        "tests/indexfn/map-if-elim.fut"
        ( pure $ \(i, n, xs, _) ->
            let xs_i = sym2SoP (Apply (Hole xs) [sHole i])
             in [ IndexFn
                    { shape = [Forall i (Iota (sHole n))],
                      body = cases [(Bool True, int2SoP 2 .*. xs_i)]
                    }
                ]
        ),
      mkTest
        "tests/indexfn/scalar.fut"
        ( pure $ \(i, _, x, _) ->
            [ IndexFn
                { shape = [Forall i (Iota (sHole x))],
                  body = cases [(Bool True, int2SoP 2 .*. sHole x)]
                }
            ]
        ),
      mkTest
        "tests/indexfn/scan.fut"
        ( pure $ \(i, n, xs, j) ->
            [ IndexFn
                { shape = [Forall i (Iota (sHole n))],
                  body =
                    cases
                      [ ( Bool True,
                          sym2SoP $
                            Sum j (int2SoP 0) (sHole i) (Apply (Hole xs) [sHole j])
                        )
                      ]
                }
            ]
        ),
      mkTest
        "tests/indexfn/scan_lambda.fut"
        ( pure $ \(i, n, xs, j) ->
            [ IndexFn
                { shape = [Forall i (Iota (sHole n))],
                  body =
                    cases
                      [ ( Bool True,
                          sym2SoP $
                            Sum j (int2SoP 0) (sHole i) (Apply (Hole xs) [sHole j])
                        )
                      ]
                }
            ]
        ),
      mkTest
        "tests/indexfn/scan2.fut"
        ( pure $ \(i, n, xs, j) ->
            [ IndexFn
                { shape = [Forall i (Iota (sHole n))],
                  body =
                    cases
                      [ ( Bool True,
                          int2SoP 1 .+. sHole i .-. sym2SoP (Sum j (int2SoP 0) (sHole i) (Apply (Hole xs) [sHole j]))
                        )
                      ]
                }
            ]
        ),
      mkTest
        "tests/indexfn/scalar2.fut"
        ( pure $ \(_, n, xs, j) ->
            [ IndexFn
                { shape = [],
                  body =
                    cases
                      [ ( Bool True,
                          sym2SoP $
                            Sum j (int2SoP 0) (sHole n .-. int2SoP 1) (Apply (Hole xs) [sHole j])
                        )
                      ]
                }
            ]
        ),
      mkTest
        "tests/indexfn/part2indices.fut"
        ( pure $ \(i, n, xs, j) ->
            let xs_i = Apply (Hole xs) [sHole i]
             in [ IndexFn
                    { shape = [],
                      body =
                        cases
                          [ ( Bool True,
                              sym2SoP (Sum j (int2SoP 0) (sHole n .-. int2SoP 1) (Apply (Hole xs) [sHole j]))
                            )
                          ]
                    },
                  IndexFn
                    { shape = [Forall i (Iota (sHole n))],
                      body =
                        cases
                          [ ( xs_i,
                              sym2SoP (Sum j (int2SoP 0) (sHole i .-. int2SoP 1) (Apply (Hole xs) [sHole j]))
                            ),
                            ( neg xs_i,
                              sHole i .+. sym2SoP (Sum j (sHole i .+. int2SoP 1) (sHole n .-. int2SoP 1) (Apply (Hole xs) [sHole j]))
                            )
                          ]
                    }
                ]
        ),
      mkTest
        "tests/indexfn/map2.fut"
        ( pure $ \(i, n, h1, h2) ->
            let inds_i = sym2SoP $ Apply (Hole h2) [sHole i]
                p = int2SoP 0 :< inds_i :&& inds_i :<= sHole n
             in [ IndexFn
                    { shape = [Forall i (Iota (sHole n))],
                      body =
                        cases
                          [ (p, sym2SoP $ Apply (Hole h1) [inds_i .-. int2SoP 1]),
                            (neg p, int2SoP 0)
                          ]
                    }
                ]
        ),
      mkTest
        "tests/indexfn/part2indices_numeric_conds.fut"
        ( pure $ \(i, n, xs, j) ->
            let xs_i = sym2SoP $ Apply (Hole xs) [sHole i]
                xs_j = sym2SoP $ Apply (Hole xs) [sHole j]
             in [ IndexFn
                    { shape = [Forall i (Iota (sHole n))],
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
                ]
        ),
      mkTest
        "tests/indexfn/part2indices_predicatefn.fut"
        ( newNameFromString "p" >>= \p -> pure $ \(i, n, xs, j) ->
            let xs_i = Apply (Hole p) [sym2SoP $ Apply (Hole xs) [sHole i]]
                xs_j = Apply (Hole p) [sym2SoP $ Apply (Hole xs) [sHole j]]
             in [ IndexFn
                    { shape = [Forall i (Iota (sHole n))],
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
                ]
        ),
      mkTest
        "tests/indexfn/part2indices_predicatefn2.fut"
        ( newNameFromString "p" >>= \p -> pure $ \(i, n, xs, j) ->
            let xs_i = Apply (Hole p) [sym2SoP $ Apply (Hole xs) [sHole i]]
                xs_j = Apply (Hole p) [sym2SoP $ Apply (Hole xs) [sHole j]]
             in [ IndexFn
                    { shape = [Forall i (Iota (sHole n))],
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
                ]
        ),
      mkTest
        "tests/indexfn/part3indices.fut"
        ( pure $ \(i, n, cs, j) ->
            let cs_i = sym2SoP $ Apply (Hole cs) [sHole i]
                cs_j = sym2SoP $ Apply (Hole cs) [sHole j]
             in [ IndexFn
                    { shape = [],
                      body =
                        cases
                          [ ( Bool True,
                              sym2SoP (Sum j (int2SoP 0) (sHole n .-. int2SoP 1) (cs_j :== int2SoP 1))
                            )
                          ]
                    },
                  IndexFn
                    { shape = [],
                      body =
                        cases
                          [ ( Bool True,
                              sym2SoP (Sum j (int2SoP 0) (sHole n .-. int2SoP 1) (cs_j :== int2SoP 1))
                                .+. sym2SoP (Sum j (int2SoP 0) (sHole n .-. int2SoP 1) (cs_j :== int2SoP 2))
                            )
                          ]
                    },
                  IndexFn
                    { shape = [Forall i (Iota (sHole n))],
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
                    }
                ]
        ),
      mkTest
        "tests/indexfn/mk_flag_array.fut"
        ( newNameFromString "k" >>= \k ->
            newNameFromString "j" >>= \j ->
              newNameFromString "zero" >>= \zero -> pure $ \(i, m, xs, shape) ->
                let sum_km1 = sym2SoP $ Sum j (int2SoP 0) (sVar k .-. int2SoP 1) (Apply (Hole shape) [sVar j])
                    sum_mm1 = sym2SoP $ Sum j (int2SoP 0) (sHole m .-. int2SoP 1) (Apply (Hole shape) [sVar j])
                 in [ IndexFn
                        { shape = [],
                          body = cases [(Bool True, sum_mm1)]
                        },
                      IndexFn
                        { shape = [Forall i (Cat k (sHole m) sum_km1)],
                          body =
                            cases
                              [ (sVar i :== sum_km1, sym2SoP $ Apply (Hole xs) [sVar k]),
                                (sVar i :/= sum_km1, sHole zero)
                              ]
                        }
                    ]
        ),
      mkTest
        "tests/indexfn/mk_flag_array_exclusive.fut"
        ( newNameFromString "k" >>= \k ->
            newNameFromString "j" >>= \j ->
              newNameFromString "zero" >>= \zero -> pure $ \(i, m, xs, shape) ->
                let sum_km1 = sym2SoP $ Sum j (int2SoP 0) (sVar k .-. int2SoP 1) (Apply (Hole shape) [sVar j])
                    sum_mm1 = sym2SoP $ Sum j (int2SoP 0) (sHole m .-. int2SoP 1) (Apply (Hole shape) [sVar j])
                 in [ IndexFn
                        { shape = [],
                          body = cases [(Bool True, sum_mm1)]
                        },
                      IndexFn
                        { shape = [Forall i (Cat k (sHole m) sum_km1)],
                          body =
                            cases
                              [ (sVar i :== sum_km1, sym2SoP $ Apply (Hole xs) [sVar k]),
                                (sVar i :/= sum_km1, sHole zero)
                              ]
                        }
                    ]
        ),
      mkTest
        "tests/indexfn/segment_sum.fut"
        ( pure $ \(i, n, xs, flags) ->
            let xs_i = sym2SoP $ Apply (Hole xs) [sHole i]
                flags_i = Apply (Hole flags) [sHole i]
             in [ IndexFn
                    { shape = [Forall i (Iota (sHole n))],
                      body =
                        cases
                          [ ((sVar i :== int2SoP 0) :|| flags_i, xs_i),
                            ((sVar i :/= int2SoP 0) :&& Not flags_i, xs_i .+. sym2SoP Recurrence)
                          ]
                    }
                ]
        ),
      mkTest
        "tests/indexfn/segment_ids.fut"
        ( pure $ \(i, m, k, b) ->
            [ IndexFn
                { shape = [Forall i (Cat k (sHole m) (sHole b))],
                  body = cases [(Bool True, sHole k)]
                }
            ]
        ),
      mkTest
        "tests/indexfn/segment_ids2.fut"
        ( pure $ \(i, m, k, b) ->
            [ IndexFn
                { shape = [Forall i (Cat k (sHole m) (sHole b))],
                  body = cases [(Bool False, sHole k)] --- XXX dummy
                }
            ]
        ),
      mkTest
        "tests/indexfn/part2indicesL.fut"
        ( newNameFromString "csL" >>= \csL ->
            newNameFromString "shape" >>= \shape ->
              newNameFromString "j" >>= \j -> pure $ \(i, m, k, b) ->
                let int = int2SoP
                    csL_i = Apply (Hole csL) [sHole i]
                    seg_k_start = sym2SoP $ Sum j (int 0) (sHole k .-. int 1) (Apply (Hole shape) [sHole j])
                    seg_k_end = int (-1) .+. sym2SoP (Sum j (int 0) (sHole k) (Apply (Hole shape) [sHole j]))
                 in [ IndexFn
                        { shape = [Forall i (Cat k (sHole m) (sHole b))],
                          body =
                            cases
                              [ ( csL_i,
                                  -- offset at segment k
                                  seg_k_start
                                    -- number of trues in this segment up to and including current index
                                    .+. sym2SoP (Sum j seg_k_start (sHole i .-. int 1) (Apply (Hole csL) [sHole j]))
                                ),
                                ( neg csL_i,
                                  -- global index
                                  sHole i
                                    -- plus number of trues that come after this index in the current segment
                                    .+. sym2SoP (Sum j (sHole i .+. int 1) seg_k_end (Apply (Hole csL) [sHole j]))
                                )
                              ]
                        }
                    ]
        ),
      mkTest
        "tests/indexfn/filter_indices.fut"
        ( newNameFromString "xs" >>= \xs ->
            newNameFromString "n" >>= \n ->
              newNameFromString "p" >>= \p -> pure $ \(i, _, _, j) ->
                let p_xs arg = Apply (Hole p) [sym2SoP $ Apply (Hole xs) [sHole arg]]
                 in [ IndexFn
                        { shape = [],
                          body =
                            cases
                              [ ( Bool True,
                                  sym2SoP (Sum j (int2SoP 0) (sHole n .-. int2SoP 1) (p_xs j))
                                )
                              ]
                        },
                      IndexFn
                        { shape = [Forall i (Iota (sHole n))],
                          body =
                            cases
                              [ ( p_xs i,
                                  sym2SoP (Sum j (int2SoP 0) (sHole i .-. int2SoP 1) (p_xs j))
                                ),
                                ( neg (p_xs i),
                                  int2SoP (-1)
                                )
                              ]
                        }
                    ]
        ),
      mkTest
        "tests/indexfn/partition2.fut"
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
                  { shape = [Forall i (Iota (sHole n))],
                    body =
                      cases
                        [(Bool True, sym2SoP $ Apply (Hole xs) [sym2SoP $ Apply (Hole is_inv) [sHole i]])]
                  }
              ]
        ),
      mkTest
        "tests/indexfn/partition2_alt.fut"
        ( pure $ \(i, n, xs, is_inv) ->
            [ IndexFn
                { shape = [Forall i (Iota (sHole n))],
                  body =
                    cases
                      [(Bool True, sym2SoP $ Apply (Hole xs) [sym2SoP $ Apply (Hole is_inv) [sHole i]])]
                }
            ]
        ),
      mkTest
        "tests/indexfn/partition2L.fut"
        ( pure $ \(i, n, xs, is_inv) ->
            [ IndexFn
                { shape = [Forall i (Iota (sHole n))],
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
                { shape = [Forall i (Iota (sHole n))],
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
                { shape = [Forall i (Iota (sHole n))],
                  body =
                    cases
                      [(Bool True, sym2SoP $ Apply (Hole xs) [sym2SoP $ Apply (Hole is_inv) [sHole i]])]
                }
            ]
        ),
      mkTest
        "tests/indexfn/filter_segmented_array.fut"
        ( pure $ \(i, n, xs, is_inv) ->
            [ IndexFn
                { shape = [Forall i (Iota (sHole n))],
                  body =
                    cases
                      [(Bool True, sym2SoP $ Apply (Hole xs) [sym2SoP $ Apply (Hole is_inv) [sHole i]])]
                }
            ]
        ),
      mkTest
        "tests/indexfn/maxMatch.fut"
        ( pure $ \(i, n, is_inv, _) ->
            [ IndexFn
                { shape = [Forall i (Iota (sHole n))],
                  body =
                    cases
                      [(Bool True, sym2SoP $ Apply (Hole is_inv) [sHole i])]
                }
            ]
        ),
      mkTest
        "tests/indexfn/maxMatch_2d.fut"
        ( pure $ \(i, n, is_inv, _) ->
            [ IndexFn
                { shape = [Forall i (Iota (sHole n))],
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
        "tests/indexfn/nd_map-map.fut"
        ( newNameFromString "j" >>= \j -> pure $ \(i, n, xs, _) ->
            [ IndexFn
                { shape = [Forall i (Iota $ sHole n), Forall j (Iota $ int2SoP 2)],
                  body =
                    cases
                      [(Bool True, sym2SoP (Apply (Hole xs) [sHole i]) .+. sHole j)]
                }
            ]
        ),
      mkTest
        "tests/indexfn/nd_map-scan.fut"
        ( newNameFromString "j" >>= \j -> pure $ \(i, n, _, k) ->
            [ IndexFn
                { shape = [Forall i (Iota $ sHole n), Forall j (Iota $ int2SoP 2)],
                  body =
                    cases
                      [(Bool True, sym2SoP (Sum k (int2SoP 1) (sHole j) (Hole k)))]
                }
            ]
        ),
      mkTest
        "tests/indexfn/nd_expansion.fut"
        ( newNameFromString "j" >>= \j -> pure $ \(i, n, xs, _) ->
            [ IndexFn
                { shape = [Forall i (Iota $ sHole n), Forall j (Iota $ int2SoP 2)],
                  body =
                    cases
                      [(Bool True, int2SoP 2 .*. sym2SoP (Apply (Hole xs) [sVar i]) .+. sHole j)]
                }
            ]
        ),
      mkTest
        "tests/indexfn/if-array-type.fut"
        ( pure $ \(i, n, xs, _) ->
            [ IndexFn
                { shape = [Forall i (Iota $ sHole n)],
                  body =
                    cases
                      [(Bool True, sym2SoP (Apply (Hole xs) [sVar i]))]
                },
              IndexFn
                { shape = [Forall i (Iota $ sHole n)],
                  body =
                    cases
                      [(Bool True, sym2SoP (Apply (Hole xs) [sVar i]))]
                }
            ]
        ),
      mkTest
        "tests/indexfn/zipArgs2d.fut"
        ( newNameFromString "j" >>= \j -> pure $ \(i, n, m, _) ->
            [ IndexFn
                { shape = [Forall i (Iota $ sHole n), Forall j (Iota $ sHole m)],
                  body =
                    cases
                      [(Bool True, sHole i .+. sHole j .+. int2SoP 1)]
                }
            ]
        ),
      mkTest
        "tests/indexfn/primes.fut"
        ( pure $ \(i, n, xs, _) ->
            [ IndexFn
                { shape = [Forall i (Iota (sHole n))],
                  -- matches anything; we're just checking the program.
                  body = cases [(Bool True, sHole xs)]
                }
            ]
        ),
      mkTest
        "tests/indexfn/quickhull.fut"
        ( pure $ \(i, n, xs, _) ->
            [ IndexFn
                { shape = [Forall i (Iota (sHole n))],
                  -- matches anything; we're just checking the program.
                  body = cases [(Bool True, sHole xs)]
                }
            ]
        ),
      mkTest
        "tests/indexfn/lolhull.fut"
        ( pure $ \(i, n, xs, _) ->
            [ IndexFn
                { shape = [Forall i (Iota (sHole n))],
                  -- matches anything; we're just checking the program.
                  body = cases [(Bool True, sHole xs)]
                }
            ]
        )
        -- mkTest
        --   "tests/indexfn/part3indices_alternative.fut"
        --   ( newNameFromString "q" >>= \q -> pure $ \(i, n, p, j) ->
        --       let p_i = Apply (Hole p) [sHole i]
        --           p_j = Apply (Hole p) [sHole j]
        --           q_i = Apply (Hole q) [sHole i]
        --           q_j = Apply (Hole q) [sHole j]
        --        in [ IndexFn
        --               { shape = [],
        --                 body =
        --                   cases
        --                     [ ( Bool True,
        --                         sym2SoP (Sum j (int2SoP 0) (sHole n .-. int2SoP 1) p_j)
        --                       )
        --                     ]
        --               },
        --             IndexFn
        --               { shape = [],
        --                 body =
        --                   cases
        --                     [ ( Bool True,
        --                         sym2SoP (Sum j (int2SoP 0) (sHole n .-. int2SoP 1) p_j)
        --                           .+. sym2SoP (Sum j (int2SoP 0) (sHole n .-. int2SoP 1) q_j)
        --                       )
        --                     ]
        --               },
        --             IndexFn
        --               { shape = [Forall i (Iota (sHole n))],
        --                 body =
        --                   cases
        --                     [ ( p_i,
        --                         sym2SoP (Sum j (int2SoP 0) (sHole i .-. int2SoP 1) p_j)
        --                       ),
        --                       ( neg p_i :&& q_i,
        --                         -- Mind the gap in the sums due to the above predicate simplifying a -1 away.
        --                         sym2SoP (Sum j (sHole i .+. int2SoP 1) (sHole n .-. int2SoP 1) p_j)
        --                           .+. sym2SoP (Sum j (int2SoP 0) (sHole i .-. int2SoP 1) p_j)
        --                           .+. sym2SoP (Sum j (int2SoP 0) (sHole i .-. int2SoP 1) q_j)
        --                       ),
        --                       ( neg p_i :&& neg q_i,
        --                         sHole i
        --                           .+. sym2SoP (Sum j (sHole i .+. int2SoP 1) (sHole n .-. int2SoP 1) p_j)
        --                           .+. sym2SoP (Sum j (sHole i .+. int2SoP 1) (sHole n .-. int2SoP 1) q_j)
        --                       )
        --                     ]
        --               }
        --           ]
        --   )
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

    sVar = sym2SoP . Var

    actual @??= expected = unless (actual == expected) (assertFailure msg)
      where
        msg = do
          docStringW 120 $
            "expected:" <+> pretty expected <> line <> "but got: " <+> pretty actual
