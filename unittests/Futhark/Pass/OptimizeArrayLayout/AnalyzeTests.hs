module Futhark.Pass.OptimizeArrayLayout.AnalyzeTests (tests) where

import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as M
import Futhark.Analysis.AccessPattern
import Futhark.IR.Prop.Names
import Futhark.IR.Syntax
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "AnalyzeTests" [analyzeStmTests]

analyzeStmTests :: TestTree
analyzeStmTests =
  testGroup
    "analyzeStm"
    [analyzeIndexTests]

analyzeIndexTests :: TestTree
analyzeIndexTests =
  testGroup
    "analyzeIndex"
    $ do
      let arr_name = VName "xss" 5144
      -- ============================= TestCase0 =============================
      -- Most simple case where we want to manifest an array, hence, we record
      -- the Index in the IndexTable.
      let testCase0 = testCase "2D manifest" $ do
            let ctx =
                  mempty
                    { parents =
                        [ SegOpName (SegmentedMap (VName "defunc_0_map_res" 5204)),
                          LoopBodyName (VName "defunc_0_f_res" 5208)
                        ],
                      assignments =
                        M.fromList
                          [ (VName "gtid" 5205, CtxVal mempty Parallel 0 mempty Simple),
                            (VName "i" 5209, CtxVal mempty Sequential 1 mempty Simple)
                          ]
                    }
            let patternNames = [VName "b" 5211]
            let dimFixes =
                  [ DimFix (Var (VName "gtid" 5205)),
                    DimFix (Var (VName "i" 5209))
                  ]
            let indexTable =
                  M.fromList
                    [ ( SegmentedMap (VName "defunc_0_map_res" 5204),
                        M.fromList
                          [ ( (arr_name, []),
                              M.fromList
                                [ ( VName "b" 5211,
                                    [ DimAccess (IM.fromList [(5205, (VName "gtid" 5205, 0, Parallel))]) 0 Simple,
                                      DimAccess (IM.fromList [(5209, (VName "i" 5209, 1, Sequential))]) 1 Simple
                                    ]
                                  )
                                ]
                            )
                          ]
                      )
                    ]
            let (_, indexTable') = analyzeIndex ctx patternNames arr_name dimFixes
            indexTable' @?= indexTable

      -- ============================= TestCase2 =============================
      -- We don't want to manifest an array with only one dimension, so we don't
      -- record anything in the IndexTable.
      let testCase1 = testCase "1D manifest" $ do
            let ctx =
                  mempty
                    { parents =
                        [ SegOpName (SegmentedMap (VName "defunc_0_map_res" 5204)),
                          LoopBodyName (VName "defunc_0_f_res" 5208)
                        ]
                    }
            let patternNames = [VName "b" 5211]
            let dimFixes = [DimFix (Var (VName "i" 5209))]

            let (_, indexTable') = analyzeIndex ctx patternNames arr_name dimFixes
            indexTable' @?= mempty

      -- ============================= TestCase1 =============================
      -- We don't want to record anything to the IndexTable when the array is
      -- not accessed inside a SegMap
      -- TODO: Create a similar one for MC with loops
      let testCase2 = testCase "Not inside SegMap" $ do
            let ctx = mempty
            let patternNames = [VName "b" 5211]
            let dimFixes =
                  [ DimFix (Var (VName "gtid" 5205)),
                    DimFix (Var (VName "i" 5209))
                  ]
            let (_, indexTable') = analyzeIndex ctx patternNames arr_name dimFixes
            indexTable' @?= mempty

      -- ============================= TestCase3 =============================
      -- If an array is allocated inside a loop or SegMap, we want to record that
      -- information in the ArrayName of the IndexTable.
      let testCase3 = testCase "Allocated inside SegMap" $ do
            let parents' =
                  [ SegOpName (SegmentedMap (VName "defunc_0_map_res" 5204)),
                    LoopBodyName (VName "defunc_0_f_res" 5208)
                  ]
            let ctx =
                  mempty
                    { parents = parents',
                      assignments =
                        M.fromList
                          [ (VName "gtid" 5205, CtxVal mempty Parallel 0 mempty Simple),
                            (VName "i" 5209, CtxVal mempty Sequential 1 mempty Simple),
                            (arr_name, CtxVal mempty Parallel 0 parents' Inscrutable)
                          ]
                    }
            let patternNames = [VName "b" 5211]
            let dimFixes =
                  [ DimFix (Var (VName "gtid" 5205)),
                    DimFix (Var (VName "i" 5209))
                  ]
            let indexTable =
                  M.fromList
                    [ ( SegmentedMap (VName "defunc_0_map_res" 5204),
                        M.fromList
                          [ ( (arr_name, parents'),
                              M.fromList
                                [ ( VName "b" 5211,
                                    [ DimAccess (IM.fromList [(5205, (VName "gtid" 5205, 0, Parallel))]) 0 Simple,
                                      DimAccess (IM.fromList [(5209, (VName "i" 5209, 1, Sequential))]) 1 Simple
                                    ]
                                  )
                                ]
                            )
                          ]
                      )
                    ]
            let (_, indexTable') = analyzeIndex ctx patternNames arr_name dimFixes
            indexTable' @?= indexTable

      -- ============================= TestCase4 =============================
      -- If the vars in the index are temporaries, we want to reduce them to
      -- to the thread IDs and or loop counters they are functions of.
      let testCase4 = testCase "Reduce dependencies" $ do
            let ctx =
                  mempty
                    { parents =
                        [ SegOpName (SegmentedMap (VName "defunc_0_map_res" 5204)),
                          LoopBodyName (VName "defunc_0_f_res" 5208)
                        ],
                      assignments =
                        M.fromList
                          [ (VName "gtid" 5205, CtxVal mempty Parallel 0 mempty Simple),
                            (VName "i" 5209, CtxVal mempty Sequential 1 mempty Simple),
                            (VName "tmp0" 5210, CtxVal (namesFromList [VName "gtid" 5205]) Sequential 2 mempty Simple),
                            (VName "tmp1" 5211, CtxVal (namesFromList [VName "i" 5209]) Sequential 3 mempty Simple),
                            (VName "k" 5212, CtxVal mempty Sequential 1 mempty Simple)
                          ],
                      constants = namesFromList [VName "k" 5212]
                    }
            let patternNames = [VName "b" 5211]
            let dimFixes =
                  [ DimFix (Var (VName "tmp0" 5210)),
                    DimFix (Var (VName "tmp1" 5211)),
                    DimFix (Var (VName "k" 5212))
                  ]
            let indexTable =
                  M.fromList
                    [ ( SegmentedMap (VName "defunc_0_map_res" 5204),
                        M.fromList
                          [ ( (arr_name, []),
                              M.fromList
                                [ ( VName "b" 5211,
                                    [ DimAccess (IM.fromList [(5205, (VName "gtid" 5205, 0, Parallel))]) 0 Simple,
                                      DimAccess (IM.fromList [(5209, (VName "i" 5209, 1, Sequential))]) 1 Simple,
                                      DimAccess mempty 2 Simple
                                    ]
                                  )
                                ]
                            )
                          ]
                      )
                    ]
            let (_, indexTable') = analyzeIndex ctx patternNames arr_name dimFixes
            indexTable' @?= indexTable

      [testCase0, testCase1, testCase2, testCase3, testCase4]

