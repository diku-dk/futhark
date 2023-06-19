module Futhark.SoP.RefineTests (tests) where

import Data.Set qualified as S
import Futhark.Analysis.PrimExp
import Futhark.SoP.Convert
import Futhark.SoP.FourierMotzkin
import Futhark.SoP.Monad
import Futhark.SoP.Parse
import Futhark.SoP.Refine
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Environment refinement tests"
    [test_nw, test_lud]

test_nw :: TestTree
test_nw =
  testGroup
    "Tests based on NW logs"
    $ let lessThans = S.fromList . map parsePrimExp
          nonNegatives =
            S.fromList
              . map (parsePrimExp . (\s -> "(sle64 " <> "(sub64 (0i64) (" <> s <> ")) " <> "(0i64))"))
       in [ testCase
              "Example 1"
              $ let less_thans =
                      [ "(slt64 (mul64 (64i64) (i_13617)) (sub64 (sub64 (fptosi_f64_i64 (sqrt64 (sitofp_i64_f64 (n_13434)))) (1i64)) (128i64)))",
                        "(slt64 (mul64 (64i64) (gtid_14374)) (sub64 (sub64 (sub64 (fptosi_f64_i64 (sqrt64 (sitofp_i64_f64 (n_13434)))) (1i64)) (mul64 (64i64) (i_13617))) (128i64)))"
                      ]
                    non_negatives = ["n_13434", "i_13617", "gtid_14374"]
                    pes = lessThans less_thans <> nonNegatives non_negatives
                    goal = parsePrimExp "(slt64 (64i64) (fptosi_f64_i64 (sqrt64 (sitofp_i64_f64 (n_13434)))))"
                    resM :: SoPM String (PrimExp String) Bool
                    resM = do
                      refineAlgEnv pes
                      fmSolveGEq0 . snd =<< toSoPCmp goal
                 in fst (runSoPM_ resM) @?= True,
            testCase
              "Example 2"
              $ let less_thans =
                      [ "(slt64 (mul64 (64i64) (i_13969)) (sub64 (sub64 (fptosi_f64_i64 (sqrt64 (sitofp_i64_f64 (mul64 (n_13783) (n_13783))))) (1i64)) (64i64)))",
                        "(slt64 (mul64 (64i64) (gtid_14718)) (sub64 (sub64 (sub64 (fptosi_f64_i64 (sqrt64 (sitofp_i64_f64 (mul64 (n_13783) (n_13783))))) (1i64)) (mul64 (64i64) (i_13969))) (64i64)))"
                      ]
                    non_negatives = ["n_13783", "mul64 (n_13783) (n_13783)", "i_13969", "gtid_14718"]
                    pes = lessThans less_thans <> nonNegatives non_negatives
                    goal = parsePrimExp "(slt64 (add_nw64 (add_nw64 (64i64) (mul_nw64 (64i64) (gtid_14718))) (mul_nw64 (62i64) (fptosi_f64_i64 (sqrt64 (sitofp_i64_f64 (mul64 (n_13783) (n_13783))))))) (add_nw64 (-64i64) (mul_nw64 (64i64) (fptosi_f64_i64 (sqrt64 (sitofp_i64_f64 (mul64 (n_13783) (n_13783))))))))"

                    resM :: SoPM String (PrimExp String) Bool
                    resM = do
                      refineAlgEnv pes
                      fmSolveGEq0 . snd =<< toSoPCmp goal
                 in fst (runSoPM_ resM) @?= True,
            testCase
              "Example 3 (test the limits of the range of Example 1)"
              $ let less_thans =
                      [ "(slt64 (mul64 (64i64) (i_13617)) (sub64 (sub64 (fptosi_f64_i64 (sqrt64 (sitofp_i64_f64 (n_13434)))) (1i64)) (128i64)))",
                        "(slt64 (mul64 (64i64) (gtid_14374)) (sub64 (sub64 (sub64 (fptosi_f64_i64 (sqrt64 (sitofp_i64_f64 (n_13434)))) (1i64)) (mul64 (64i64) (i_13617))) (128i64)))"
                      ]
                    non_negatives = ["n_13434", "i_13617", "gtid_14374"]
                    pes = lessThans less_thans <> nonNegatives non_negatives
                    maximalTrue =
                      fst $ (runSoPM_ :: SoPM String (PrimExp String) a -> (a, AlgEnv String (PrimExp String))) $ do
                        refineAlgEnv pes
                        fmSolveGEq0 . snd
                          =<< toSoPCmp
                            ( parsePrimExp "(slt64 (129i64) (fptosi_f64_i64 (sqrt64 (sitofp_i64_f64 (n_13434)))))"
                            )

                    minimalFalse =
                      fst $ (runSoPM_ :: SoPM String (PrimExp String) a -> (a, AlgEnv String (PrimExp String))) $ do
                        refineAlgEnv pes
                        fmSolveGEq0 . snd
                          =<< toSoPCmp
                            ( parsePrimExp "(slt64 (130i64) (fptosi_f64_i64 (sqrt64 (sitofp_i64_f64 (n_13434)))))"
                            )
                 in maximalTrue && not minimalFalse @?= True
          ]

test_lud :: TestTree
test_lud =
  testGroup
    "Tests based on LUD logs"
    $ let lessThans =
            S.fromList
              . map (parsePrimExp . (\(i, b) -> "(slt64 (" <> i <> ") (" <> b <> "))"))
          nonNegatives =
            S.fromList
              . map (parsePrimExp . (\s -> "(sle64 " <> "(sub64 (0i64) (" <> s <> ")) " <> "(0i64))"))
       in [ testCase
              "Example 1"
              $ let less_thans =
                      [ ("step_14910", "sub64 (num_blocks_14902) (1i64)"),
                        ("gtid_16211", "sub64 (num_blocks_14902) (add64 (1i64) (step_14910))"),
                        ("gtid_16212", "32i64"),
                        ("gtid_16304", "sub64 (num_blocks_14902) (add64 (1i64) (step_14910))"),
                        ("gtid_16305", "32i64"),
                        ("gtid_16448", "sub64 (num_blocks_14902) (add64 (1i64) (step_14910))"),
                        ("gtid_16449", "sub64 (num_blocks_14902) (add64 (1i64) (step_14910))"),
                        ("gid_y_17184", "1i64"),
                        ("gid_x_17183", "1i64")
                      ]
                    non_negatives =
                      [ "num_blocks_14902",
                        "opaque_res_14906",
                        "step_14910",
                        "sub64 (num_blocks_14902) (i_14963)",
                        "opaque_res_15034",
                        "max_group_size_15534",
                        "segmap_group_size_16060",
                        "segmap_group_size_16089",
                        "segmap_group_size_16103",
                        "segmap_group_size_16130",
                        "segmap_group_size_16207",
                        "gtid_16211",
                        "gtid_16212",
                        "segmap_group_size_16300",
                        "gtid_16304",
                        "gtid_16305",
                        "gtid_16448",
                        "gtid_16449",
                        "gid_x_17183",
                        "gid_y_17184",
                        "smax64 (0i64) (binop_y_17516)",
                        "mul_nw64 (4096i64) (j_m_i_14964)",
                        "smax64 (0i64) (binop_y_17765)",
                        "smax64 (0i64) (binop_y_17784)",
                        "smax64 (0i64) (binop_y_17913)"
                      ]
                    pes = lessThans less_thans <> nonNegatives non_negatives
                    goal = parsePrimExp "(slt64 (1023i64) (mul_nw64 (1024i64) (num_blocks_14902)) )"
                    resM :: SoPM String (PrimExp String) Bool
                    resM = do
                      refineAlgEnv pes
                      fmSolveGEq0 . snd =<< toSoPCmp goal
                 in fst (runSoPM_ resM) @?= True,
            testCase
              "Example 2"
              $ let less_thans =
                      [ ("step_14910", "sub64 (num_blocks_14902) (1i64)"),
                        ("gtid_16211", "sub64 (num_blocks_14902) (add64 (1i64) (step_14910))"),
                        ("gtid_16212", "32i64"),
                        ("gtid_16304", "sub64 (num_blocks_14902) (add64 (1i64) (step_14910))"),
                        ("gtid_16305", "32i64"),
                        ("gtid_16448", "sub64 (num_blocks_14902) (add64 (1i64) (step_14910))"),
                        ("gtid_16449", "sub64 (num_blocks_14902) (add64 (1i64) (step_14910))"),
                        ("gid_y_17184", "1i64"),
                        ("gid_x_17183", "1i64")
                      ]
                    non_negatives =
                      [ "num_blocks_14902",
                        "opaque_res_14906",
                        "step_14910",
                        "sub64 (num_blocks_14902) (i_14963)",
                        "opaque_res_15034",
                        "max_group_size_15534",
                        "segmap_group_size_16060",
                        "segmap_group_size_16089",
                        "segmap_group_size_16103",
                        "segmap_group_size_16130",
                        "segmap_group_size_16207",
                        "gtid_16211",
                        "gtid_16212",
                        "segmap_group_size_16300",
                        "gtid_16304",
                        "gtid_16305",
                        "gtid_16448",
                        "gtid_16449",
                        "gid_x_17183",
                        "gid_y_17184",
                        "smax64 (0i64) (binop_y_17516)",
                        "mul_nw64 (4096i64) (j_m_i_14964)",
                        "smax64 (0i64) (binop_y_17765)",
                        "smax64 (0i64) (binop_y_17784)",
                        "smax64 (0i64) (binop_y_17913)"
                      ]
                    pes = lessThans less_thans <> nonNegatives non_negatives
                    goal = parsePrimExp "(slt64 (add_nw64 (add_nw64 (add_nw64 (1023i64) (mul_nw64 (1024i64) (gtid_16449))) (mul_nw64 (1024i64) (gid_y_17184))) (mul_nw64 (32i64) (gid_x_17183))) (mul_nw64 (1024i64) (num_blocks_14902)))"

                    resM :: SoPM String (PrimExp String) Bool
                    resM = do
                      refineAlgEnv pes
                      fmSolveGEq0 . snd =<< toSoPCmp goal
                 in fst (runSoPM_ resM) @?= True,
            testCase
              "Example 3"
              $ let less_thans =
                      [ ("step_14910", "sub64 (num_blocks_14902) (1i64)"),
                        ("gtid_16211", "sub64 (num_blocks_14902) (add64 (1i64) (step_14910))"),
                        ("gtid_16212", "32i64"),
                        ("gtid_16304", "sub64 (num_blocks_14902) (add64 (1i64) (step_14910))"),
                        ("gtid_16305", "32i64"),
                        ("gtid_16448", "sub64 (num_blocks_14902) (add64 (1i64) (step_14910))"),
                        ("gtid_16449", "sub64 (num_blocks_14902) (add64 (1i64) (step_14910))"),
                        ("gid_y_17184", "1i64"),
                        ("gid_x_17183", "1i64")
                      ]

                    non_negatives =
                      [ "num_blocks_14902",
                        "opaque_res_14906",
                        "step_14910",
                        "sub64 (num_blocks_14902) (i_14963)",
                        "opaque_res_15034",
                        "max_group_size_15534",
                        "segmap_group_size_16060",
                        "segmap_group_size_16089",
                        "segmap_group_size_16103",
                        "segmap_group_size_16130",
                        "segmap_group_size_16207",
                        "gtid_16211",
                        "gtid_16212",
                        "segmap_group_size_16300",
                        "gtid_16304",
                        "gtid_16305",
                        "gtid_16448",
                        "gtid_16449",
                        "gid_x_17183",
                        "gid_y_17184",
                        "smax64 (0i64) (binop_y_17516)",
                        "mul_nw64 (4096i64) (j_m_i_14964)",
                        "smax64 (0i64) (binop_y_17765)",
                        "smax64 (0i64) (binop_y_17784)",
                        "smax64 (0i64) (binop_y_17913)"
                      ]

                    pes = lessThans less_thans <> nonNegatives non_negatives
                    goal = parsePrimExp "(slt64 (add_nw64 (add_nw64 (mul_nw64 (1024i64) (gid_y_17184)) (mul_nw64 (32i64) (gid_x_17183))) (1024i64) ) (1025i64 ))"

                    resM :: SoPM String (PrimExp String) Bool
                    resM = do
                      refineAlgEnv pes
                      fmSolveGEq0 . snd =<< toSoPCmp goal
                 in fst (runSoPM_ resM) @?= True,
            testCase
              "Example 4"
              $ let less_thans =
                      [ ("step_14910", "sub64 (num_blocks_14902) (1i64)"),
                        ( "gtid_16211",
                          "sub64 (num_blocks_14902) (add64 (1i64) (step_14910))"
                        ),
                        ("gtid_16212", "32i64"),
                        ( "gtid_16304",
                          "sub64 (num_blocks_14902) (add64 (1i64) (step_14910))"
                        ),
                        ("gtid_16305", "32i64"),
                        ( "gtid_16448",
                          "sub64 (num_blocks_14902) (add64 (1i64) (step_14910))"
                        ),
                        ("gtid_16449", "sub64 (num_blocks_14902) (add64 (1i64) (step_14910))"),
                        ("gid_y_17184", "1i64"),
                        ("gid_x_17183", "1i64")
                      ]

                    non_negatives =
                      [ "num_blocks_14902",
                        "opaque_res_14906",
                        "step_14910",
                        "sub64 (num_blocks_14902) (i_14963)",
                        "opaque_res_15034",
                        "max_group_size_15534",
                        "segmap_group_size_16060",
                        "segmap_group_size_16089",
                        "segmap_group_size_16103",
                        "segmap_group_size_16130",
                        "segmap_group_size_16207",
                        "gtid_16211",
                        "gtid_16212",
                        "segmap_group_size_16300",
                        "gtid_16304",
                        "gtid_16305",
                        "gtid_16448",
                        "gtid_16449",
                        "gid_x_17183",
                        "gid_y_17184",
                        "smax64 (0i64) (binop_y_17516)",
                        "mul_nw64 (4096i64) (j_m_i_14964)",
                        "smax64 (0i64) (binop_y_17765)",
                        "smax64 (0i64) (binop_y_17784)",
                        "smax64 (0i64) (binop_y_17913)"
                      ]

                    pes = lessThans less_thans <> nonNegatives non_negatives
                    goal = parsePrimExp "(slt64 (mul_nw64 (1024i64) (gtid_16449) ) (add_nw64 (add_nw64 (add_nw64 (mul_nw64 (1024i64) (gtid_16449)) (mul_nw64 (1024i64) (gid_y_17184))) (mul_nw64 (32i64) (gid_x_17183))) (1i64) ))"

                    resM :: SoPM String (PrimExp String) Bool
                    resM = do
                      refineAlgEnv pes
                      fmSolveGEq0 . snd =<< toSoPCmp goal
                 in fst (runSoPM_ resM) @?= True
          ]
