module Futhark.Optimise.ArrayLayout.AnalyseTests (tests) where

import Data.Map.Strict qualified as M
import Futhark.Analysis.AccessPattern
import Futhark.IR.GPU
import Futhark.IR.GPUTests ()
import Futhark.IR.SyntaxTests ()
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Analyse" [analyseStmTests]

analyseStmTests :: TestTree
analyseStmTests =
  testGroup
    "analyseStm"
    [analyseIndexTests, analyseDimAccessesTests]

analyseIndexTests :: TestTree
analyseIndexTests =
  testGroup
    "analyseIndex"
    $ do
      let arr_name = "xss_5144"
      -- ============================= TestCase0 =============================
      -- Most simple case where we want to manifest an array, hence, we record
      -- the Index in the IndexTable.
      let testCase0 = testCase "2D manifest" $ do
            let ctx =
                  mempty
                    { parents =
                        [ SegOpName (SegmentedMap "defunc_0_map_res_5204"),
                          LoopBodyName "defunc_0_f_res_5208"
                        ],
                      assignments =
                        M.fromList
                          [ ("gtid_5205", VariableInfo mempty 0 mempty ThreadID),
                            ("i_5209", VariableInfo mempty 1 mempty LoopVar)
                          ]
                    }
            let patternNames = ["b_5211"]
            let dimFixes =
                  [ DimFix (Var "gtid_5205"),
                    DimFix (Var "i_5209")
                  ]
            let indexTable =
                  M.fromList
                    [ ( SegmentedMap "defunc_0_map_res_5204",
                        M.fromList
                          [ ( (arr_name, [], [0 .. 1]),
                              M.fromList
                                [ ( "b_5211",
                                    [ DimAccess (M.fromList [("gtid_5205", Dependency 0 ThreadID)]) (Just "gtid_5205"),
                                      DimAccess (M.fromList [("i_5209", Dependency 1 LoopVar)]) (Just "i_5209")
                                    ]
                                  )
                                ]
                            )
                          ]
                      )
                    ]
            let (_, indexTable') = analyseIndex ctx patternNames arr_name dimFixes
            indexTable' @?= indexTable

      -- ============================= TestCase2 =============================
      -- We don't want to manifest an array with only one dimension, so we don't
      -- record anything in the IndexTable.
      let testCase1 = testCase "1D manifest" $ do
            let ctx =
                  mempty
                    { parents =
                        [ SegOpName (SegmentedMap "defunc_0_map_res_5204"),
                          LoopBodyName "defunc_0_f_res_5208"
                        ]
                    }
            let patternNames = ["b_5211"]
            let dimFixes = [DimFix "i_5209"]

            let (_, indexTable') = analyseIndex ctx patternNames arr_name dimFixes
            indexTable' @?= mempty

      -- ============================= TestCase1 =============================
      -- We don't want to record anything to the IndexTable when the array is
      -- not accessed inside a SegMap
      -- TODO: Create a similar one for MC with loops
      let testCase2 = testCase "Not inside SegMap" $ do
            let ctx = mempty
            let patternNames = ["b_5211"]
            let dimFixes =
                  [ DimFix "gtid_5205",
                    DimFix "i_5209"
                  ]
            let (_, indexTable') = analyseIndex ctx patternNames arr_name dimFixes
            indexTable' @?= mempty

      -- ============================= TestCase3 =============================
      -- If an array is allocated inside a loop or SegMap, we want to record that
      -- information in the ArrayName of the IndexTable.
      let testCase3 = testCase "Allocated inside SegMap" $ do
            let parents' =
                  [ SegOpName (SegmentedMap "defunc_0_map_res_5204"),
                    LoopBodyName "defunc_0_f_res_5208"
                  ]
            let ctx =
                  mempty
                    { parents = parents',
                      assignments =
                        M.fromList
                          [ ("gtid_5205", VariableInfo mempty 0 mempty ThreadID),
                            ("i_5209", VariableInfo mempty 1 mempty LoopVar),
                            (arr_name, VariableInfo mempty 0 parents' Variable)
                          ]
                    }
            let patternNames = ["b_5211"]
            let dimFixes =
                  [ DimFix "gtid_5205",
                    DimFix "i_5209"
                  ]
            let indexTable =
                  M.fromList
                    [ ( SegmentedMap "defunc_0_map_res_5204",
                        M.fromList
                          [ ( (arr_name, parents', [0 .. 1]),
                              M.fromList
                                [ ( "b_5211",
                                    [ DimAccess (M.fromList [("gtid_5205", Dependency 0 ThreadID)]) (Just "gtid_5205"),
                                      DimAccess (M.fromList [("i_5209", Dependency 1 LoopVar)]) (Just "i_5209")
                                    ]
                                  )
                                ]
                            )
                          ]
                      )
                    ]
            let (_, indexTable') = analyseIndex ctx patternNames arr_name dimFixes
            indexTable' @?= indexTable

      -- ============================= TestCase4 =============================
      -- If the vars in the index are temporaries, we want to reduce them to
      -- to the thread IDs and or loop counters they are functions of.
      let testCase4 = testCase "Reduce dependencies" $ do
            let ctx =
                  mempty
                    { parents =
                        [ SegOpName (SegmentedMap "defunc_0_map_res_5204"),
                          LoopBodyName "defunc_0_f_res_5208"
                        ],
                      assignments =
                        M.fromList
                          [ ("gtid_5205", VariableInfo mempty 0 mempty ThreadID),
                            ("i_5209", VariableInfo mempty 1 mempty LoopVar),
                            ("tmp0_5210", VariableInfo (namesFromList ["gtid_5205"]) 2 mempty Variable),
                            ("tmp1_5211", VariableInfo (namesFromList ["i_5209"]) 3 mempty Variable),
                            ("k_5212", VariableInfo mempty 1 mempty ConstType)
                          ]
                    }
            let patternNames = ["b_5211"]
            let dimFixes =
                  [ DimFix "tmp0_5210",
                    DimFix "tmp1_5211",
                    DimFix "k_5212"
                  ]
            let indexTable =
                  M.fromList
                    [ ( SegmentedMap "defunc_0_map_res_5204",
                        M.fromList
                          [ ( (arr_name, [], [0 .. 2]),
                              M.fromList
                                [ ( "b_5211",
                                    [ DimAccess (M.fromList [("gtid_5205", Dependency 0 ThreadID)]) (Just "tmp0_5210"),
                                      DimAccess (M.fromList [("i_5209", Dependency 1 LoopVar)]) (Just "tmp1_5211"),
                                      DimAccess mempty (Just "k_5212")
                                    ]
                                  )
                                ]
                            )
                          ]
                      )
                    ]
            let (_, indexTable') = analyseIndex ctx patternNames arr_name dimFixes
            indexTable' @?= indexTable

      [testCase0, testCase1, testCase2, testCase3, testCase4]

analyseDimAccessesTests :: TestTree
analyseDimAccessesTests = testGroup
  "analyseDimAccesses"
  $ do
    let testCase0 = testCase "Fold" $ do
          let indexTable =
                M.fromList
                  [ ( SegmentedMap "defunc_0_map_res_5204",
                      M.fromList
                        [ ( ("xss_5144", [], [0, 1]),
                            M.fromList
                              [ ( "b_5211",
                                  [ DimAccess (M.fromList [("gtid_5205", Dependency 0 ThreadID)]) (Just "gtid_5205"),
                                    DimAccess (M.fromList [("i_5209", Dependency 1 LoopVar)]) (Just "i_5209")
                                  ]
                                )
                              ]
                          )
                        ]
                    )
                  ]
          let indexTable' = (analyseDimAccesses @GPU) prog0
          indexTable' @?= indexTable

    [testCase0]
  where
    prog0 :: Prog GPU
    prog0 =
      "\
      \entry(\"main\",\
      \      {xss: [][]i64},\
      \      {[]i64})\
      \  entry_main (n_5142 : i64,\
      \              m_5143 : i64,\
      \              xss_5144 : [n_5142][m_5143]i64)\
      \  : {[n_5142]i64#([2], [0])} = {\
      \  let {segmap_group_size_5202 : i64} =\
      \    get_size(segmap_group_size_5190, thread_block_size)\
      \  let {segmap_usable_groups_5203 : i64} =\
      \    sdiv_up64(n_5142, segmap_group_size_5202)\
      \  let {defunc_0_map_res_5204 : [n_5142]i64} =\
      \    segmap(thread; ; grid=segmap_usable_groups_5203; blocksize=segmap_group_size_5202)\
      \    (gtid_5205 < n_5142) (~phys_tid_5206) : {i64} {\
      \      let {defunc_0_f_res_5208 : i64} =\
      \        loop {acc_5210 : i64} = {0i64}\
      \        for i_5209:i64 < m_5143 do {\
      \          let {b_5211 : i64} =\
      \            xss_5144[gtid_5205, i_5209]\
      \          let {defunc_0_f_res_5212 : i64} =\
      \            add64(acc_5210, b_5211)\
      \          in {defunc_0_f_res_5212}\
      \        }\
      \      return {returns defunc_0_f_res_5208}\
      \    }\
      \  in {defunc_0_map_res_5204}\
      \}"
