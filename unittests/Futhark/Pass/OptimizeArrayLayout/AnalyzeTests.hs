module Futhark.Pass.OptimizeArrayLayout.AnalyzeTests (tests) where

import Data.List.NonEmpty
import Data.Map.Strict qualified as M
import Data.Sequence.Internal qualified as S
import Futhark.Analysis.AccessPattern
import Futhark.IR.GPU
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Analyze" [analyzeStmTests]

analyzeStmTests :: TestTree
analyzeStmTests =
  testGroup
    "analyzeStm"
    [analyzeIndexTests, analyzeDimAccesssTests]

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
                          [ (VName "gtid" 5205, VariableInfo mempty 0 mempty ThreadID),
                            (VName "i" 5209, VariableInfo mempty 1 mempty LoopVar)
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
                          [ ( (arr_name, [], [0 .. 1]),
                              M.fromList
                                [ ( VName "b" 5211,
                                    [ DimAccess (M.fromList [(VName "gtid" 5205, Dependency 0 ThreadID)]) (Just $ VName "gtid" 5205),
                                      DimAccess (M.fromList [(VName "i" 5209, Dependency 1 LoopVar)]) (Just $ VName "i" 5209)
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
                          [ (VName "gtid" 5205, VariableInfo mempty 0 mempty ThreadID),
                            (VName "i" 5209, VariableInfo mempty 1 mempty LoopVar),
                            (arr_name, VariableInfo mempty 0 parents' Variable)
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
                          [ ( (arr_name, parents', [0 .. 1]),
                              M.fromList
                                [ ( VName "b" 5211,
                                    [ DimAccess (M.fromList [(VName "gtid" 5205, Dependency 0 ThreadID)]) (Just $ VName "gtid" 5205),
                                      DimAccess (M.fromList [(VName "i" 5209, Dependency 1 LoopVar)]) (Just $ VName "i" 5209)
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
                          [ (VName "gtid" 5205, VariableInfo mempty 0 mempty ThreadID),
                            (VName "i" 5209, VariableInfo mempty 1 mempty LoopVar),
                            (VName "tmp0" 5210, VariableInfo (namesFromList [VName "gtid" 5205]) 2 mempty Variable),
                            (VName "tmp1" 5211, VariableInfo (namesFromList [VName "i" 5209]) 3 mempty Variable),
                            (VName "k" 5212, VariableInfo mempty 1 mempty ConstType)
                          ]
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
                          [ ( (arr_name, [], [0 .. 2]),
                              M.fromList
                                [ ( VName "b" 5211,
                                    [ DimAccess (M.fromList [(VName "gtid" 5205, Dependency 0 ThreadID)]) (Just $ VName "tmp0" 5210),
                                      DimAccess (M.fromList [(VName "i" 5209, Dependency 1 LoopVar)]) (Just $ VName "tmp1" 5211),
                                      DimAccess mempty (Just $ VName "k" 5212)
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

analyzeDimAccesssTests :: TestTree
analyzeDimAccesssTests = testGroup
  "analyzeDimAccesss"
  $ do
    let testCase0 = testCase "Fold" $ do
          let indexTable =
                M.fromList
                  [ ( SegmentedMap (VName "defunc_0_map_res" 5204),
                      M.fromList
                        [ ( (VName "xss" 5144, [], [0, 1]),
                            M.fromList
                              [ ( VName "b" 5211,
                                  [ DimAccess (M.fromList [(VName "gtid" 5205, Dependency 0 ThreadID)]) (Just $ VName "gtid" 5205),
                                    DimAccess (M.fromList [(VName "i" 5209, Dependency 1 LoopVar)]) (Just $ VName "i" 5209)
                                  ]
                                )
                              ]
                          )
                        ]
                    )
                  ]
          let indexTable' = (analyzeDimAccesss @GPU) prog0
          indexTable' @?= indexTable

    let testCase1 = testCase "Long program" $ do
          let indexTable =
                M.fromList
                  [ ( SegmentedMap {vnameFromSegOp = VName "defunc_0_map_res" 6291},
                      M.fromList
                        [ ( (VName "xsss" 5720, [], [0 .. 3]),
                            M.fromList
                              [ ( VName "+_lhs" 6308,
                                  [ DimAccess (M.fromList [(VName "gtid" 6292, Dependency 0 ThreadID)]) (Just $ VName "gtid" 6292),
                                    DimAccess (M.fromList [(VName "i" 6299, Dependency 1 LoopVar)]) (Just $ VName "map2_arg2" 6301),
                                    DimAccess (M.fromList [(VName "gtid" 6304, Dependency 2 ThreadID)]) (Just $ VName "gtid" 6304),
                                    DimAccess mempty Nothing
                                  ]
                                ),
                                ( VName "+_lhs" 6326,
                                  [ DimAccess (M.fromList [(VName "gtid" 6292, Dependency 0 ThreadID)]) (Just $ VName "gtid" 6292),
                                    DimAccess (M.fromList [(VName "i" 6299, Dependency 1 LoopVar)]) (Just $ VName "map2_arg2" 6301),
                                    DimAccess (M.fromList [(VName "gtid" 6315, Dependency 2 ThreadID)]) (Just $ VName "gtid" 6315),
                                    DimAccess
                                      { dependencies =
                                          M.fromList
                                            [ (VName "i" 6299, Dependency 1 LoopVar),
                                              (VName "j" 6322, Dependency 3 LoopVar)
                                            ],
                                        originalVar = Just $ VName "+_lhs" 6325
                                      }
                                  ]
                                )
                              ]
                          ),
                          ( (VName "res" 6300, [CondBodyName (VName "defunc_0_map_res" 6243), SegOpName (SegmentedMap {vnameFromSegOp = VName "defunc_0_map_res" 6291})], [0 .. 1]),
                            M.fromList
                              [ ( VName "+_rhs" 6309,
                                  [ DimAccess (M.fromList [(VName "gtid" 6304, Dependency 2 ThreadID)]) (Just $ VName "gtid" 6304),
                                    DimAccess mempty Nothing
                                  ]
                                ),
                                ( VName "+_rhs" 6328,
                                  [ DimAccess (M.fromList [(VName "gtid" 6315, Dependency 2 ThreadID)]) (Just $ VName "gtid" 6315),
                                    DimAccess
                                      { dependencies =
                                          M.fromList
                                            [ (VName "i" 6299, Dependency 1 LoopVar),
                                              (VName "j" 6322, Dependency 3 LoopVar)
                                            ],
                                        originalVar = Just $ VName "+_rhs" 6327
                                      }
                                  ]
                                )
                              ]
                          )
                        ]
                    ),
                    ( SegmentedMap {vnameFromSegOp = VName "wew_r" 6303},
                      M.fromList
                        [ ( (VName "xsss" 5720, [], [0 .. 3]),
                            M.fromList
                              [ ( VName "+_lhs" 6308,
                                  [ DimAccess (M.fromList [(VName "gtid" 6292, Dependency 0 ThreadID)]) (Just $ VName "gtid" 6292),
                                    DimAccess (M.fromList [(VName "i" 6299, Dependency 1 LoopVar)]) (Just $ VName "map2_arg2" 6301),
                                    DimAccess (M.fromList [(VName "gtid" 6304, Dependency 2 ThreadID)]) (Just $ VName "gtid" 6304),
                                    DimAccess mempty Nothing
                                  ]
                                )
                              ]
                          ),
                          ( (VName "res" 6300, [CondBodyName (VName "defunc_0_map_res" 6243), SegOpName (SegmentedMap {vnameFromSegOp = VName "defunc_0_map_res" 6291})], [0 .. 1]),
                            M.fromList
                              [ ( VName "+_rhs" 6309,
                                  [ DimAccess (M.fromList [(VName "gtid" 6304, Dependency 2 ThreadID)]) (Just $ VName "gtid" 6304),
                                    DimAccess mempty Nothing
                                  ]
                                )
                              ]
                          )
                        ]
                    ),
                    ( SegmentedMap {vnameFromSegOp = VName "defunc_0_map_res" 6314},
                      M.fromList
                        [ ( (VName "xsss" 5720, [], [0 .. 3]),
                            M.fromList
                              [ ( VName "+_lhs" 6326,
                                  [ DimAccess (M.fromList [(VName "gtid" 6292, Dependency 0 ThreadID)]) (Just $ VName "gtid" 6292),
                                    DimAccess (M.fromList [(VName "i" 6299, Dependency 1 LoopVar)]) (Just $ VName "map2_arg2" 6301),
                                    DimAccess (M.fromList [(VName "gtid" 6315, Dependency 2 ThreadID)]) (Just $ VName "gtid" 6315),
                                    DimAccess
                                      { dependencies =
                                          M.fromList
                                            [ (VName "i" 6299, Dependency 1 LoopVar),
                                              (VName "j" 6322, Dependency 3 LoopVar)
                                            ],
                                        originalVar = Just $ VName "+_lhs" 6325
                                      }
                                  ]
                                )
                              ]
                          ),
                          ( (VName "res" 6300, [CondBodyName (VName "defunc_0_map_res" 6243), SegOpName (SegmentedMap {vnameFromSegOp = VName "defunc_0_map_res" 6291})], [0 .. 1]),
                            M.fromList
                              [ ( VName "+_rhs" 6328,
                                  [ DimAccess (M.fromList [(VName "gtid" 6315, Dependency 2 ThreadID)]) (Just $ VName "gtid" 6315),
                                    DimAccess (M.fromList [(VName "i" 6299, Dependency 1 LoopVar), (VName "j" 6322, Dependency 3 LoopVar)]) (Just $ VName "+_rhs" 6327)
                                  ]
                                )
                              ]
                          )
                        ]
                    ),
                    ( SegmentedMap {vnameFromSegOp = VName "wew_r_r" 6506},
                      M.fromList
                        [ ( (VName "xsss" 5720, [], [0 .. 3]),
                            M.fromList
                              [ ( VName "+_lhs" 6512,
                                  [ DimAccess (M.fromList [(VName "gtid" 6507, Dependency 1 ThreadID)]) (Just $ VName "gtid" 6507),
                                    DimAccess mempty (Just $ VName "map2_arg2" 6550),
                                    DimAccess (M.fromList [(VName "gtid" 6508, Dependency 2 ThreadID)]) (Just $ VName "gtid" 6508),
                                    DimAccess mempty Nothing
                                  ]
                                )
                              ]
                          ),
                          ( (VName "res_expanded" 6494, [CondBodyName (VName "defunc_0_map_res" 6243)], [0 .. 2]),
                            M.fromList
                              [ ( VName "+_rhs" 6513,
                                  [ DimAccess (M.fromList [(VName "gtid" 6507, Dependency 1 ThreadID)]) (Just $ VName "gtid" 6507),
                                    DimAccess (M.fromList [(VName "gtid" 6508, Dependency 2 ThreadID)]) (Just $ VName "gtid" 6508),
                                    DimAccess mempty Nothing
                                  ]
                                )
                              ]
                          )
                        ]
                    ),
                    ( SegmentedMap {vnameFromSegOp = VName "lifted_lambda_res" 6523},
                      M.fromList
                        [ ( (VName "xsss" 5720, [], [0 .. 3]),
                            M.fromList
                              [ ( VName "+_lhs" 6536,
                                  [ DimAccess (M.fromList [(VName "gtid" 6524, Dependency 2 ThreadID)]) (Just $ VName "gtid" 6524),
                                    DimAccess mempty (Just $ VName "map2_arg2" 6551),
                                    DimAccess (M.fromList [(VName "gtid" 6525, Dependency 3 ThreadID)]) (Just $ VName "gtid" 6525),
                                    DimAccess (M.fromList [(VName "i" 6493, Dependency 0 LoopVar), (VName "j" 6532, Dependency 4 LoopVar)]) (Just $ VName "+_lhs" 6535)
                                  ]
                                )
                              ]
                          ),
                          ( (VName "res_expanded" 6494, [CondBodyName (VName "defunc_0_map_res" 6243)], [0 .. 2]),
                            M.fromList
                              [ ( VName "+_rhs" 6538,
                                  [ DimAccess (M.fromList [(VName "gtid" 6524, Dependency 2 ThreadID)]) (Just $ VName "gtid" 6524),
                                    DimAccess (M.fromList [(VName "gtid" 6525, Dependency 3 ThreadID)]) (Just $ VName "gtid" 6525),
                                    DimAccess (M.fromList [(VName "i" 6493, Dependency 0 LoopVar), (VName "j" 6532, Dependency 4 LoopVar)]) (Just $ VName "+_rhs" 6537)
                                  ]
                                )
                              ]
                          )
                        ]
                    )
                  ]
          let indexTable' = (analyzeDimAccesss @GPU) prog1
          indexTable' @?= indexTable

    [testCase0, testCase1]
  where
    prog0 =
      Prog
        { progTypes = OpaqueTypes [],
          progConsts = mempty,
          progFuns =
            [ FunDef
                { funDefEntryPoint =
                    Just
                      ( nameFromString "main",
                        [ EntryParam
                            { entryParamName = nameFromString "xss",
                              entryParamUniqueness = Nonunique,
                              entryParamType =
                                TypeTransparent
                                  ( ValueType
                                      Signed
                                      (Rank 2)
                                      (IntType Int64)
                                  )
                            }
                        ],
                        [ EntryResult
                            { entryResultUniqueness = Nonunique,
                              entryResultType =
                                TypeTransparent
                                  ( ValueType
                                      Signed
                                      (Rank 1)
                                      (IntType Int64)
                                  )
                            }
                        ]
                      ),
                  funDefAttrs = Attrs {unAttrs = mempty},
                  funDefName = nameFromString "entry_main",
                  funDefRetType =
                    [ ( Array
                          (IntType Int64)
                          (Shape {shapeDims = [Free (Var (VName "n" 5142))]})
                          Nonunique,
                        RetAls {paramAls = [2], otherAls = [0]}
                      )
                    ],
                  funDefParams =
                    [ Param
                        { paramAttrs = Attrs {unAttrs = mempty},
                          paramName = VName "n" 5142,
                          paramDec = Prim (IntType Int64)
                        },
                      Param
                        { paramAttrs = Attrs {unAttrs = mempty},
                          paramName = VName "m" 5143,
                          paramDec = Prim (IntType Int64)
                        },
                      Param
                        { paramAttrs = Attrs {unAttrs = mempty},
                          paramName = VName "xss" 5144,
                          paramDec =
                            Array
                              (IntType Int64)
                              ( Shape
                                  { shapeDims =
                                      [ Var (VName "n" 5142),
                                        Var (VName "m" 5143)
                                      ]
                                  }
                              )
                              Nonunique
                        }
                    ],
                  funDefBody =
                    Body
                      { bodyDec = (),
                        bodyStms =
                          S.fromList
                            [ Let
                                { stmPat =
                                    Pat
                                      { patElems =
                                          [ PatElem
                                              { patElemName = VName "segmap_group_size" 5202,
                                                patElemDec = Prim (IntType Int64)
                                              }
                                          ]
                                      },
                                  stmAux =
                                    StmAux
                                      { stmAuxCerts = Certs {unCerts = []},
                                        stmAuxAttrs = Attrs {unAttrs = mempty},
                                        stmAuxDec = ()
                                      },
                                  stmExp =
                                    Op
                                      ( SizeOp
                                          ( GetSize
                                              "segmap_group_size_5190"
                                              SizeGroup
                                          )
                                      )
                                },
                              Let
                                { stmPat =
                                    Pat
                                      { patElems =
                                          [ PatElem
                                              { patElemName = VName "segmap_usable_groups" 5203,
                                                patElemDec = Prim (IntType Int64)
                                              }
                                          ]
                                      },
                                  stmAux =
                                    StmAux
                                      { stmAuxCerts = Certs {unCerts = []},
                                        stmAuxAttrs = Attrs {unAttrs = mempty},
                                        stmAuxDec = ()
                                      },
                                  stmExp =
                                    BasicOp
                                      ( BinOp
                                          (SDivUp Int64 Unsafe)
                                          (Var (VName "n" 5142))
                                          (Var (VName "segmap_group_size" 5202))
                                      )
                                },
                              Let
                                { stmPat =
                                    Pat
                                      { patElems =
                                          [ PatElem
                                              { patElemName =
                                                  VName "defunc_0_map_res" 5204,
                                                patElemDec =
                                                  Array
                                                    (IntType Int64)
                                                    ( Shape
                                                        { shapeDims = [Var (VName "n" 5142)]
                                                        }
                                                    )
                                                    NoUniqueness
                                              }
                                          ]
                                      },
                                  stmAux =
                                    StmAux
                                      { stmAuxCerts = Certs {unCerts = []},
                                        stmAuxAttrs = Attrs {unAttrs = mempty},
                                        stmAuxDec = ()
                                      },
                                  stmExp =
                                    Op
                                      ( SegOp
                                          ( SegMap
                                              ( SegThread
                                                  SegNoVirt
                                                  ( Just
                                                      ( KernelGrid
                                                          { gridNumGroups =
                                                              Count {unCount = Var (VName "segmap_usable_groups" 5203)},
                                                            gridGroupSize =
                                                              Count {unCount = Var (VName "segmap_group_size" 5202)}
                                                          }
                                                      )
                                                  )
                                              )
                                              ( SegSpace
                                                  { segFlat = VName "phys_tid" 5206,
                                                    unSegSpace =
                                                      [ ( VName "gtid" 5205,
                                                          Var (VName "n" 5142)
                                                        )
                                                      ]
                                                  }
                                              )
                                              [Prim (IntType Int64)]
                                              ( KernelBody
                                                  { kernelBodyDec = (),
                                                    kernelBodyStms =
                                                      S.fromList
                                                        [ Let
                                                            { stmPat =
                                                                Pat
                                                                  { patElems =
                                                                      [ PatElem
                                                                          { patElemName = VName "defunc_0_f_res" 5208,
                                                                            patElemDec = Prim (IntType Int64)
                                                                          }
                                                                      ]
                                                                  },
                                                              stmAux =
                                                                StmAux
                                                                  { stmAuxCerts = Certs {unCerts = []},
                                                                    stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                    stmAuxDec = ()
                                                                  },
                                                              stmExp =
                                                                Loop
                                                                  [ ( Param
                                                                        { paramAttrs = Attrs {unAttrs = mempty},
                                                                          paramName = VName "acc" 5210,
                                                                          paramDec = Prim (IntType Int64)
                                                                        },
                                                                      Constant
                                                                        ( IntValue
                                                                            (Int64Value 0)
                                                                        )
                                                                    )
                                                                  ]
                                                                  ( ForLoop
                                                                      (VName "i" 5209)
                                                                      Int64
                                                                      (Var (VName "m" 5143))
                                                                  )
                                                                  ( Body
                                                                      { bodyDec = (),
                                                                        bodyStms =
                                                                          S.fromList
                                                                            [ Let
                                                                                { stmPat =
                                                                                    Pat
                                                                                      { patElems =
                                                                                          [ PatElem
                                                                                              { patElemName = VName "b" 5211,
                                                                                                patElemDec = Prim (IntType Int64)
                                                                                              }
                                                                                          ]
                                                                                      },
                                                                                  stmAux =
                                                                                    StmAux
                                                                                      { stmAuxCerts = Certs {unCerts = []},
                                                                                        stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                        stmAuxDec = ()
                                                                                      },
                                                                                  stmExp =
                                                                                    BasicOp
                                                                                      ( Index
                                                                                          (VName "xss" 5144)
                                                                                          ( Slice
                                                                                              { unSlice =
                                                                                                  [ DimFix (Var (VName "gtid" 5205)),
                                                                                                    DimFix (Var (VName "i" 5209))
                                                                                                  ]
                                                                                              }
                                                                                          )
                                                                                      )
                                                                                },
                                                                              Let
                                                                                { stmPat =
                                                                                    Pat
                                                                                      { patElems =
                                                                                          [ PatElem
                                                                                              { patElemName = VName "defunc_0_f_res" 5212,
                                                                                                patElemDec = Prim (IntType Int64)
                                                                                              }
                                                                                          ]
                                                                                      },
                                                                                  stmAux =
                                                                                    StmAux
                                                                                      { stmAuxCerts = Certs {unCerts = []},
                                                                                        stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                        stmAuxDec = ()
                                                                                      },
                                                                                  stmExp =
                                                                                    BasicOp
                                                                                      ( BinOp
                                                                                          (Add Int64 OverflowWrap)
                                                                                          (Var (VName "acc" 5210))
                                                                                          (Var (VName "b" 5211))
                                                                                      )
                                                                                }
                                                                            ],
                                                                        bodyResult =
                                                                          [ SubExpRes
                                                                              { resCerts = Certs {unCerts = []},
                                                                                resSubExp = Var (VName "defunc_0_f_res" 5212)
                                                                              }
                                                                          ]
                                                                      }
                                                                  )
                                                            }
                                                        ],
                                                    kernelBodyResult =
                                                      [ Returns
                                                          ResultMaySimplify
                                                          (Certs {unCerts = []})
                                                          (Var (VName "defunc_0_f_res" 5208))
                                                      ]
                                                  }
                                              )
                                          )
                                      )
                                }
                            ],
                        bodyResult =
                          [ SubExpRes
                              { resCerts = Certs {unCerts = []},
                                resSubExp = Var (VName "defunc_0_map_res" 5204)
                              }
                          ]
                      }
                }
            ]
        }

    prog1 =
      Prog
        { progTypes = OpaqueTypes [],
          progConsts = mempty,
          progFuns =
            [ FunDef
                { funDefEntryPoint =
                    Just
                      ( nameFromString "main",
                        [ EntryParam
                            { entryParamName = nameFromString "xsss",
                              entryParamUniqueness = Nonunique,
                              entryParamType =
                                TypeTransparent
                                  ( ValueType
                                      Signed
                                      (Rank 4)
                                      (IntType Int64)
                                  )
                            },
                          EntryParam
                            { entryParamName = nameFromString "is",
                              entryParamUniqueness = Nonunique,
                              entryParamType =
                                TypeTransparent
                                  ( ValueType
                                      Signed
                                      (Rank 1)
                                      (IntType Int64)
                                  )
                            },
                          EntryParam
                            { entryParamName = nameFromString "is2",
                              entryParamUniqueness = Nonunique,
                              entryParamType =
                                TypeTransparent
                                  ( ValueType
                                      Signed
                                      (Rank 1)
                                      (IntType Int64)
                                  )
                            }
                        ],
                        [ EntryResult
                            { entryResultUniqueness = Unique,
                              entryResultType =
                                TypeTransparent
                                  ( ValueType
                                      Signed
                                      (Rank 3)
                                      (IntType Int64)
                                  )
                            }
                        ]
                      ),
                  funDefAttrs = Attrs {unAttrs = mempty},
                  funDefName = nameFromString "entry_main",
                  funDefRetType =
                    [ ( Array
                          (IntType Int64)
                          ( Shape
                              { shapeDims =
                                  [ Free (Var (VName "l" 5716)),
                                    Free (Var (VName "n" 5718)),
                                    Free (Var (VName "o" 5719))
                                  ]
                              }
                          )
                          Unique,
                        RetAls
                          { paramAls = [],
                            otherAls = []
                          }
                      )
                    ],
                  funDefParams =
                    [ Param
                        { paramAttrs = Attrs {unAttrs = mempty},
                          paramName = VName "l" 5716,
                          paramDec = Prim (IntType Int64)
                        },
                      Param
                        { paramAttrs = Attrs {unAttrs = mempty},
                          paramName = VName "m" 5717,
                          paramDec = Prim (IntType Int64)
                        },
                      Param
                        { paramAttrs = Attrs {unAttrs = mempty},
                          paramName = VName "n" 5718,
                          paramDec = Prim (IntType Int64)
                        },
                      Param
                        { paramAttrs = Attrs {unAttrs = mempty},
                          paramName = VName "o" 5719,
                          paramDec = Prim (IntType Int64)
                        },
                      Param
                        { paramAttrs = Attrs {unAttrs = mempty},
                          paramName = VName "xsss" 5720,
                          paramDec =
                            Array
                              (IntType Int64)
                              ( Shape
                                  { shapeDims =
                                      [ Var (VName "l" 5716),
                                        Var (VName "m" 5717),
                                        Var (VName "n" 5718),
                                        Var (VName "o" 5719)
                                      ]
                                  }
                              )
                              Nonunique
                        },
                      Param
                        { paramAttrs = Attrs {unAttrs = mempty},
                          paramName = VName "is" 5721,
                          paramDec =
                            Array
                              (IntType Int64)
                              ( Shape {shapeDims = [Var (VName "n" 5718)]}
                              )
                              Nonunique
                        },
                      Param
                        { paramAttrs = Attrs {unAttrs = mempty},
                          paramName = VName "is2" 5722,
                          paramDec =
                            Array
                              (IntType Int64)
                              ( Shape {shapeDims = [Var (VName "m" 5717)]}
                              )
                              Nonunique
                        }
                    ],
                  funDefBody =
                    Body
                      { bodyDec = (),
                        bodyStms =
                          S.fromList
                            [ Let
                                { stmPat =
                                    Pat
                                      { patElems =
                                          [ PatElem
                                              { patElemName = VName "index" 6244,
                                                patElemDec =
                                                  Array
                                                    (IntType Int64)
                                                    ( Shape
                                                        { shapeDims =
                                                            [ Var (VName "l" 5716),
                                                              Var (VName "n" 5718),
                                                              Var (VName "o" 5719)
                                                            ]
                                                        }
                                                    )
                                                    NoUniqueness
                                              }
                                          ]
                                      },
                                  stmAux =
                                    StmAux
                                      { stmAuxCerts =
                                          Certs
                                            { unCerts = []
                                            },
                                        stmAuxAttrs = Attrs {unAttrs = mempty},
                                        stmAuxDec = ()
                                      },
                                  stmExp =
                                    BasicOp
                                      ( Index
                                          (VName "xsss" 5720)
                                          ( Slice
                                              { unSlice =
                                                  [ DimSlice
                                                      (Constant (IntValue (Int64Value 0)))
                                                      (Var (VName "l" 5716))
                                                      (Constant (IntValue (Int64Value 1))),
                                                    DimFix
                                                      (Constant (IntValue (Int64Value 0))),
                                                    DimSlice
                                                      (Constant (IntValue (Int64Value 0)))
                                                      (Var (VName "n" 5718))
                                                      (Constant (IntValue (Int64Value 1))),
                                                    DimSlice
                                                      (Constant (IntValue (Int64Value 0)))
                                                      (Var (VName "o" 5719))
                                                      (Constant (IntValue (Int64Value 1)))
                                                  ]
                                              }
                                          )
                                      )
                                },
                              Let
                                { stmPat =
                                    Pat
                                      { patElems =
                                          [ PatElem
                                              { patElemName = VName "max_group_size" 6288,
                                                patElemDec = Prim (IntType Int64)
                                              }
                                          ]
                                      },
                                  stmAux =
                                    StmAux
                                      { stmAuxCerts =
                                          Certs
                                            { unCerts = []
                                            },
                                        stmAuxAttrs = Attrs {unAttrs = mempty},
                                        stmAuxDec = ()
                                      },
                                  stmExp =
                                    Op
                                      (SizeOp (GetSizeMax SizeGroup))
                                },
                              Let
                                { stmPat =
                                    Pat
                                      { patElems =
                                          [ PatElem
                                              { patElemName = VName "fits" 6289,
                                                patElemDec = Prim Bool
                                              }
                                          ]
                                      },
                                  stmAux =
                                    StmAux
                                      { stmAuxCerts =
                                          Certs
                                            { unCerts = []
                                            },
                                        stmAuxAttrs = Attrs {unAttrs = mempty},
                                        stmAuxDec = ()
                                      },
                                  stmExp =
                                    BasicOp
                                      ( CmpOp
                                          (CmpSle Int64)
                                          (Var (VName "n" 5718))
                                          (Var (VName "max_group_size" 6288))
                                      )
                                },
                              Let
                                { stmPat =
                                    Pat
                                      { patElems =
                                          [ PatElem
                                              { patElemName = VName "suff_intra_par" 6287,
                                                patElemDec = Prim Bool
                                              }
                                          ]
                                      },
                                  stmAux =
                                    StmAux
                                      { stmAuxCerts =
                                          Certs
                                            { unCerts = []
                                            },
                                        stmAuxAttrs = Attrs {unAttrs = mempty},
                                        stmAuxDec = ()
                                      },
                                  stmExp =
                                    Op
                                      ( SizeOp
                                          ( CmpSizeLe
                                              (nameFromString "suff_intra_par_0")
                                              ( SizeThreshold
                                                  []
                                                  (Just 32)
                                              )
                                              (Var (VName "n" 5718))
                                          )
                                      )
                                },
                              Let
                                { stmPat =
                                    Pat
                                      { patElems =
                                          [ PatElem
                                              { patElemName = VName "intra_suff_and_fits" 6290,
                                                patElemDec = Prim Bool
                                              }
                                          ]
                                      },
                                  stmAux =
                                    StmAux
                                      { stmAuxCerts =
                                          Certs
                                            { unCerts = []
                                            },
                                        stmAuxAttrs = Attrs {unAttrs = mempty},
                                        stmAuxDec = ()
                                      },
                                  stmExp =
                                    BasicOp
                                      ( BinOp
                                          LogAnd
                                          (Var (VName "suff_intra_par" 6287))
                                          (Var (VName "fits" 6289))
                                      )
                                },
                              Let
                                { stmPat =
                                    Pat
                                      { patElems =
                                          [ PatElem
                                              { patElemName = VName "nest_size" 6503,
                                                patElemDec = Prim (IntType Int64)
                                              }
                                          ]
                                      },
                                  stmAux =
                                    StmAux
                                      { stmAuxCerts =
                                          Certs
                                            { unCerts = []
                                            },
                                        stmAuxAttrs = Attrs {unAttrs = mempty},
                                        stmAuxDec = ()
                                      },
                                  stmExp =
                                    BasicOp
                                      ( BinOp
                                          (Mul Int64 OverflowUndef)
                                          (Var (VName "l" 5716))
                                          (Var (VName "n" 5718))
                                      )
                                },
                              Let
                                { stmPat =
                                    Pat
                                      { patElems =
                                          [ PatElem
                                              { patElemName = VName "segmap_group_size" 6504,
                                                patElemDec = Prim (IntType Int64)
                                              }
                                          ]
                                      },
                                  stmAux =
                                    StmAux
                                      { stmAuxCerts =
                                          Certs
                                            { unCerts = []
                                            },
                                        stmAuxAttrs = Attrs {unAttrs = mempty},
                                        stmAuxDec = ()
                                      },
                                  stmExp =
                                    Op
                                      ( SizeOp
                                          ( GetSize
                                              (nameFromString "segmap_group_size_6390")
                                              SizeGroup
                                          )
                                      )
                                },
                              Let
                                { stmPat =
                                    Pat
                                      { patElems =
                                          [ PatElem
                                              { patElemName = VName "segmap_usable_groups" 6505,
                                                patElemDec = Prim (IntType Int64)
                                              }
                                          ]
                                      },
                                  stmAux =
                                    StmAux
                                      { stmAuxCerts =
                                          Certs
                                            { unCerts = []
                                            },
                                        stmAuxAttrs = Attrs {unAttrs = mempty},
                                        stmAuxDec = ()
                                      },
                                  stmExp =
                                    BasicOp
                                      ( BinOp
                                          (SDivUp Int64 Safe)
                                          (Var (VName "nest_size" 6503))
                                          (Var (VName "segmap_group_size" 6504))
                                      )
                                },
                              Let
                                { stmPat =
                                    Pat
                                      { patElems =
                                          [ PatElem
                                              { patElemName = VName "segmap_group_size" 6520,
                                                patElemDec = Prim (IntType Int64)
                                              }
                                          ]
                                      },
                                  stmAux =
                                    StmAux
                                      { stmAuxCerts =
                                          Certs
                                            { unCerts = []
                                            },
                                        stmAuxAttrs = Attrs {unAttrs = mempty},
                                        stmAuxDec = ()
                                      },
                                  stmExp =
                                    Op
                                      ( SizeOp
                                          ( GetSize
                                              (nameFromString "segmap_group_size_6356")
                                              SizeGroup
                                          )
                                      )
                                },
                              Let
                                { stmPat =
                                    Pat
                                      { patElems =
                                          [ PatElem
                                              { patElemName = VName "num_groups" 6521,
                                                patElemDec = Prim (IntType Int64)
                                              }
                                          ]
                                      },
                                  stmAux =
                                    StmAux
                                      { stmAuxCerts =
                                          Certs
                                            { unCerts = []
                                            },
                                        stmAuxAttrs = Attrs {unAttrs = mempty},
                                        stmAuxDec = ()
                                      },
                                  stmExp =
                                    Op
                                      ( SizeOp
                                          ( CalcNumGroups
                                              (Var (VName "nest_size" 6503))
                                              (nameFromString "segmap_num_groups_6358")
                                              (Var (VName "segmap_group_size" 6520))
                                          )
                                      )
                                },
                              Let
                                { stmPat =
                                    Pat
                                      { patElems =
                                          [ PatElem
                                              { patElemName = VName "defunc_0_map_res" 6243,
                                                patElemDec =
                                                  Array
                                                    (IntType Int64)
                                                    ( Shape
                                                        { shapeDims =
                                                            [ Var (VName "l" 5716),
                                                              Var (VName "n" 5718),
                                                              Var (VName "o" 5719)
                                                            ]
                                                        }
                                                    )
                                                    NoUniqueness
                                              }
                                          ]
                                      },
                                  stmAux =
                                    StmAux
                                      { stmAuxCerts =
                                          Certs
                                            { unCerts = []
                                            },
                                        stmAuxAttrs = Attrs {unAttrs = mempty},
                                        stmAuxDec = ()
                                      },
                                  stmExp =
                                    Match
                                      [ Var
                                          (VName "intra_suff_and_fits" 6290)
                                      ]
                                      [ Case
                                          { casePat =
                                              [Just (BoolValue True)],
                                            caseBody =
                                              Body
                                                { bodyDec = (),
                                                  bodyStms =
                                                    S.fromList
                                                      [ Let
                                                          { stmPat =
                                                              Pat
                                                                { patElems =
                                                                    [ PatElem
                                                                        { patElemName = VName "defunc_0_map_res" 6291,
                                                                          patElemDec =
                                                                            Array
                                                                              (IntType Int64)
                                                                              ( Shape
                                                                                  { shapeDims =
                                                                                      [ Var (VName "l" 5716),
                                                                                        Var (VName "n" 5718),
                                                                                        Var (VName "o" 5719)
                                                                                      ]
                                                                                  }
                                                                              )
                                                                              NoUniqueness
                                                                        }
                                                                    ]
                                                                },
                                                            stmAux =
                                                              StmAux
                                                                { stmAuxCerts =
                                                                    Certs
                                                                      { unCerts = []
                                                                      },
                                                                  stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                  stmAuxDec = ()
                                                                },
                                                            stmExp =
                                                              Op
                                                                ( SegOp
                                                                    ( SegMap
                                                                        ( SegGroup
                                                                            SegNoVirt
                                                                            ( Just
                                                                                ( KernelGrid
                                                                                    { gridNumGroups =
                                                                                        Count
                                                                                          { unCount = Var (VName "l" 5716)
                                                                                          },
                                                                                      gridGroupSize =
                                                                                        Count
                                                                                          { unCount = Var (VName "n" 5718)
                                                                                          }
                                                                                    }
                                                                                )
                                                                            )
                                                                        )
                                                                        ( SegSpace
                                                                            { segFlat =
                                                                                VName
                                                                                  (nameFromString "phys_group_id")
                                                                                  6293,
                                                                              unSegSpace =
                                                                                [ ( VName "gtid" 6292,
                                                                                    Var (VName "l" 5716)
                                                                                  )
                                                                                ]
                                                                            }
                                                                        )
                                                                        [ Array
                                                                            (IntType Int64)
                                                                            ( Shape
                                                                                { shapeDims =
                                                                                    [ Var (VName "n" 5718),
                                                                                      Var (VName "o" 5719)
                                                                                    ]
                                                                                }
                                                                            )
                                                                            NoUniqueness
                                                                        ]
                                                                        ( KernelBody
                                                                            { kernelBodyDec = (),
                                                                              kernelBodyStms =
                                                                                S.fromList
                                                                                  [ Let
                                                                                      { stmPat =
                                                                                          Pat
                                                                                            { patElems =
                                                                                                [ PatElem
                                                                                                    { patElemName = VName "as_transformed_transformed_row" 6296,
                                                                                                      patElemDec =
                                                                                                        Array
                                                                                                          (IntType Int64)
                                                                                                          ( Shape
                                                                                                              { shapeDims =
                                                                                                                  [ Var (VName "n" 5718),
                                                                                                                    Var (VName "o" 5719)
                                                                                                                  ]
                                                                                                              }
                                                                                                          )
                                                                                                          NoUniqueness
                                                                                                    }
                                                                                                ]
                                                                                            },
                                                                                        stmAux =
                                                                                          StmAux
                                                                                            { stmAuxCerts =
                                                                                                Certs
                                                                                                  { unCerts = []
                                                                                                  },
                                                                                              stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                              stmAuxDec = ()
                                                                                            },
                                                                                        stmExp =
                                                                                          BasicOp
                                                                                            ( Index
                                                                                                (VName "xsss" 5720)
                                                                                                ( Slice
                                                                                                    { unSlice =
                                                                                                        [ DimFix
                                                                                                            (Var (VName "gtid" 6292)),
                                                                                                          DimFix
                                                                                                            ( Constant
                                                                                                                ( IntValue
                                                                                                                    (Int64Value 0)
                                                                                                                )
                                                                                                            ),
                                                                                                          DimSlice
                                                                                                            ( Constant
                                                                                                                ( IntValue
                                                                                                                    (Int64Value 0)
                                                                                                                )
                                                                                                            )
                                                                                                            (Var (VName "n" 5718))
                                                                                                            ( Constant
                                                                                                                ( IntValue
                                                                                                                    (Int64Value 1)
                                                                                                                )
                                                                                                            ),
                                                                                                          DimSlice
                                                                                                            ( Constant
                                                                                                                ( IntValue
                                                                                                                    (Int64Value 0)
                                                                                                                )
                                                                                                            )
                                                                                                            (Var (VName "o" 5719))
                                                                                                            ( Constant
                                                                                                                ( IntValue
                                                                                                                    (Int64Value 1)
                                                                                                                )
                                                                                                            )
                                                                                                        ]
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                      },
                                                                                    Let
                                                                                      { stmPat =
                                                                                          Pat
                                                                                            { patElems =
                                                                                                [ PatElem
                                                                                                    { patElemName = VName "lifted_lambda_res" 6298,
                                                                                                      patElemDec =
                                                                                                        Array
                                                                                                          (IntType Int64)
                                                                                                          ( Shape
                                                                                                              { shapeDims =
                                                                                                                  [ Var (VName "n" 5718),
                                                                                                                    Var (VName "o" 5719)
                                                                                                                  ]
                                                                                                              }
                                                                                                          )
                                                                                                          NoUniqueness
                                                                                                    }
                                                                                                ]
                                                                                            },
                                                                                        stmAux =
                                                                                          StmAux
                                                                                            { stmAuxCerts =
                                                                                                Certs
                                                                                                  { unCerts = []
                                                                                                  },
                                                                                              stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                              stmAuxDec = ()
                                                                                            },
                                                                                        stmExp =
                                                                                          Loop
                                                                                            [ ( Param
                                                                                                  { paramAttrs = Attrs {unAttrs = mempty},
                                                                                                    paramName = VName "res" 6300,
                                                                                                    paramDec =
                                                                                                      Array
                                                                                                        (IntType Int64)
                                                                                                        ( Shape
                                                                                                            { shapeDims =
                                                                                                                [ Var (VName "n" 5718),
                                                                                                                  Var (VName "o" 5719)
                                                                                                                ]
                                                                                                            }
                                                                                                        )
                                                                                                        Nonunique
                                                                                                  },
                                                                                                Var (VName "as_transformed_transformed_row" 6296)
                                                                                              )
                                                                                            ]
                                                                                            ( ForLoop
                                                                                                (VName "i" 6299)
                                                                                                Int64
                                                                                                (Var (VName "m" 5717))
                                                                                            )
                                                                                            ( Body
                                                                                                { bodyDec = (),
                                                                                                  bodyStms =
                                                                                                    S.fromList
                                                                                                      [ Let
                                                                                                          { stmPat =
                                                                                                              Pat
                                                                                                                { patElems =
                                                                                                                    [ PatElem
                                                                                                                        { patElemName = VName "map2_arg2" 6301,
                                                                                                                          patElemDec = Prim (IntType Int64)
                                                                                                                        }
                                                                                                                    ]
                                                                                                                },
                                                                                                            stmAux =
                                                                                                              StmAux
                                                                                                                { stmAuxCerts =
                                                                                                                    Certs
                                                                                                                      { unCerts = []
                                                                                                                      },
                                                                                                                  stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                  stmAuxDec = ()
                                                                                                                },
                                                                                                            stmExp =
                                                                                                              BasicOp
                                                                                                                ( Index
                                                                                                                    ( VName "is2" 5722
                                                                                                                    )
                                                                                                                    ( Slice
                                                                                                                        { unSlice =
                                                                                                                            [DimFix (Var (VName "i" 6299))]
                                                                                                                        }
                                                                                                                    )
                                                                                                                )
                                                                                                          },
                                                                                                        Let
                                                                                                          { stmPat =
                                                                                                              Pat
                                                                                                                { patElems =
                                                                                                                    [ PatElem
                                                                                                                        { patElemName = VName "wew_r" 6303,
                                                                                                                          patElemDec =
                                                                                                                            Array
                                                                                                                              (IntType Int64)
                                                                                                                              ( Shape
                                                                                                                                  { shapeDims = [Var (VName "n" 5718)]
                                                                                                                                  }
                                                                                                                              )
                                                                                                                              NoUniqueness
                                                                                                                        }
                                                                                                                    ]
                                                                                                                },
                                                                                                            stmAux =
                                                                                                              StmAux
                                                                                                                { stmAuxCerts =
                                                                                                                    Certs
                                                                                                                      { unCerts = []
                                                                                                                      },
                                                                                                                  stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                  stmAuxDec = ()
                                                                                                                },
                                                                                                            stmExp =
                                                                                                              Op
                                                                                                                ( SegOp
                                                                                                                    ( SegMap
                                                                                                                        (SegThread SegNoVirt Nothing)
                                                                                                                        ( SegSpace
                                                                                                                            { segFlat = VName "phys_tid" 6305,
                                                                                                                              unSegSpace =
                                                                                                                                [ ( VName "gtid" 6304,
                                                                                                                                    Var (VName "n" 5718)
                                                                                                                                  )
                                                                                                                                ]
                                                                                                                            }
                                                                                                                        )
                                                                                                                        [Prim (IntType Int64)]
                                                                                                                        ( KernelBody
                                                                                                                            { kernelBodyDec = (),
                                                                                                                              kernelBodyStms =
                                                                                                                                S.fromList
                                                                                                                                  [ Let
                                                                                                                                      { stmPat =
                                                                                                                                          Pat
                                                                                                                                            { patElems =
                                                                                                                                                [ PatElem
                                                                                                                                                    { patElemName = VName "+_lhs" 6308,
                                                                                                                                                      patElemDec = Prim (IntType Int64)
                                                                                                                                                    }
                                                                                                                                                ]
                                                                                                                                            },
                                                                                                                                        stmAux =
                                                                                                                                          StmAux
                                                                                                                                            { stmAuxCerts =
                                                                                                                                                Certs
                                                                                                                                                  { unCerts = []
                                                                                                                                                  },
                                                                                                                                              stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                              stmAuxDec = ()
                                                                                                                                            },
                                                                                                                                        stmExp =
                                                                                                                                          BasicOp
                                                                                                                                            ( Index
                                                                                                                                                (VName "xsss" 5720)
                                                                                                                                                ( Slice
                                                                                                                                                    { unSlice =
                                                                                                                                                        [ DimFix (Var (VName "gtid" 6292)),
                                                                                                                                                          DimFix (Var (VName "map2_arg2" 6301)),
                                                                                                                                                          DimFix (Var (VName "gtid" 6304)),
                                                                                                                                                          DimFix
                                                                                                                                                            ( Constant
                                                                                                                                                                ( IntValue
                                                                                                                                                                    (Int64Value 0)
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                        ]
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                                            )
                                                                                                                                      },
                                                                                                                                    Let
                                                                                                                                      { stmPat =
                                                                                                                                          Pat
                                                                                                                                            { patElems =
                                                                                                                                                [ PatElem
                                                                                                                                                    { patElemName = VName "+_rhs" 6309,
                                                                                                                                                      patElemDec = Prim (IntType Int64)
                                                                                                                                                    }
                                                                                                                                                ]
                                                                                                                                            },
                                                                                                                                        stmAux =
                                                                                                                                          StmAux
                                                                                                                                            { stmAuxCerts =
                                                                                                                                                Certs
                                                                                                                                                  { unCerts = []
                                                                                                                                                  },
                                                                                                                                              stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                              stmAuxDec = ()
                                                                                                                                            },
                                                                                                                                        stmExp =
                                                                                                                                          BasicOp
                                                                                                                                            ( Index
                                                                                                                                                (VName "res" 6300)
                                                                                                                                                ( Slice
                                                                                                                                                    { unSlice =
                                                                                                                                                        [ DimFix
                                                                                                                                                            (Var (VName "gtid" 6304)),
                                                                                                                                                          DimFix
                                                                                                                                                            ( Constant
                                                                                                                                                                ( IntValue
                                                                                                                                                                    (Int64Value 0)
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                        ]
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                                            )
                                                                                                                                      },
                                                                                                                                    Let
                                                                                                                                      { stmPat =
                                                                                                                                          Pat
                                                                                                                                            { patElems =
                                                                                                                                                [ PatElem
                                                                                                                                                    { patElemName = VName "wew" 6310,
                                                                                                                                                      patElemDec = Prim (IntType Int64)
                                                                                                                                                    }
                                                                                                                                                ]
                                                                                                                                            },
                                                                                                                                        stmAux =
                                                                                                                                          StmAux
                                                                                                                                            { stmAuxCerts =
                                                                                                                                                Certs
                                                                                                                                                  { unCerts = []
                                                                                                                                                  },
                                                                                                                                              stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                              stmAuxDec = ()
                                                                                                                                            },
                                                                                                                                        stmExp =
                                                                                                                                          BasicOp
                                                                                                                                            ( BinOp
                                                                                                                                                (Add Int64 OverflowWrap)
                                                                                                                                                (Var (VName "+_lhs" 6308))
                                                                                                                                                (Var (VName "+_rhs" 6309))
                                                                                                                                            )
                                                                                                                                      }
                                                                                                                                  ],
                                                                                                                              kernelBodyResult =
                                                                                                                                [ Returns
                                                                                                                                    ResultMaySimplify
                                                                                                                                    ( Certs
                                                                                                                                        { unCerts = []
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                    (Var (VName "wew" 6310))
                                                                                                                                ]
                                                                                                                            }
                                                                                                                        )
                                                                                                                    )
                                                                                                                )
                                                                                                          },
                                                                                                        Let
                                                                                                          { stmPat =
                                                                                                              Pat
                                                                                                                { patElems =
                                                                                                                    [ PatElem
                                                                                                                        { patElemName = VName "wew_r_tr_rep" 6312,
                                                                                                                          patElemDec =
                                                                                                                            Array
                                                                                                                              (IntType Int64)
                                                                                                                              ( Shape
                                                                                                                                  { shapeDims =
                                                                                                                                      [ Constant
                                                                                                                                          ( IntValue
                                                                                                                                              (Int64Value 1)
                                                                                                                                          ),
                                                                                                                                        Var (VName "n" 5718)
                                                                                                                                      ]
                                                                                                                                  }
                                                                                                                              )
                                                                                                                              NoUniqueness
                                                                                                                        }
                                                                                                                    ]
                                                                                                                },
                                                                                                            stmAux =
                                                                                                              StmAux
                                                                                                                { stmAuxCerts =
                                                                                                                    Certs
                                                                                                                      { unCerts = []
                                                                                                                      },
                                                                                                                  stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                  stmAuxDec = ()
                                                                                                                },
                                                                                                            stmExp =
                                                                                                              BasicOp
                                                                                                                ( Replicate
                                                                                                                    ( Shape
                                                                                                                        { shapeDims =
                                                                                                                            [ Constant
                                                                                                                                ( IntValue
                                                                                                                                    (Int64Value 1)
                                                                                                                                )
                                                                                                                            ]
                                                                                                                        }
                                                                                                                    )
                                                                                                                    ( Var (VName "wew_r" 6303)
                                                                                                                    )
                                                                                                                )
                                                                                                          },
                                                                                                        Let
                                                                                                          { stmPat =
                                                                                                              Pat
                                                                                                                { patElems =
                                                                                                                    [ PatElem
                                                                                                                        { patElemName = VName "loop_init_r" 6313,
                                                                                                                          patElemDec =
                                                                                                                            Array
                                                                                                                              (IntType Int64)
                                                                                                                              ( Shape
                                                                                                                                  { shapeDims =
                                                                                                                                      [ Var (VName "n" 5718),
                                                                                                                                        Constant
                                                                                                                                          ( IntValue
                                                                                                                                              (Int64Value 1)
                                                                                                                                          )
                                                                                                                                      ]
                                                                                                                                  }
                                                                                                                              )
                                                                                                                              NoUniqueness
                                                                                                                        }
                                                                                                                    ]
                                                                                                                },
                                                                                                            stmAux =
                                                                                                              StmAux
                                                                                                                { stmAuxCerts =
                                                                                                                    Certs
                                                                                                                      { unCerts = []
                                                                                                                      },
                                                                                                                  stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                  stmAuxDec = ()
                                                                                                                },
                                                                                                            stmExp =
                                                                                                              BasicOp
                                                                                                                ( Rearrange
                                                                                                                    [1, 0]
                                                                                                                    (VName "wew_r_tr_rep" 6312)
                                                                                                                )
                                                                                                          },
                                                                                                        Let
                                                                                                          { stmPat =
                                                                                                              Pat
                                                                                                                { patElems =
                                                                                                                    [ PatElem
                                                                                                                        { patElemName = VName "defunc_0_map_res" 6314,
                                                                                                                          patElemDec =
                                                                                                                            Array
                                                                                                                              (IntType Int64)
                                                                                                                              ( Shape
                                                                                                                                  { shapeDims =
                                                                                                                                      [ Var (VName "n" 5718),
                                                                                                                                        Var (VName "o" 5719)
                                                                                                                                      ]
                                                                                                                                  }
                                                                                                                              )
                                                                                                                              NoUniqueness
                                                                                                                        }
                                                                                                                    ]
                                                                                                                },
                                                                                                            stmAux =
                                                                                                              StmAux
                                                                                                                { stmAuxCerts =
                                                                                                                    Certs
                                                                                                                      { unCerts = []
                                                                                                                      },
                                                                                                                  stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                  stmAuxDec = ()
                                                                                                                },
                                                                                                            stmExp =
                                                                                                              Op
                                                                                                                ( SegOp
                                                                                                                    ( SegMap
                                                                                                                        (SegThread SegNoVirt Nothing)
                                                                                                                        ( SegSpace
                                                                                                                            { segFlat = VName "phys_tid" 6316,
                                                                                                                              unSegSpace =
                                                                                                                                [ ( VName "gtid" 6315,
                                                                                                                                    Var (VName "n" 5718)
                                                                                                                                  )
                                                                                                                                ]
                                                                                                                            }
                                                                                                                        )
                                                                                                                        [ Array
                                                                                                                            (IntType Int64)
                                                                                                                            ( Shape
                                                                                                                                { shapeDims =
                                                                                                                                    [Var (VName "o" 5719)]
                                                                                                                                }
                                                                                                                            )
                                                                                                                            NoUniqueness
                                                                                                                        ]
                                                                                                                        ( KernelBody
                                                                                                                            { kernelBodyDec = (),
                                                                                                                              kernelBodyStms =
                                                                                                                                S.fromList
                                                                                                                                  [ Let
                                                                                                                                      { stmPat =
                                                                                                                                          Pat
                                                                                                                                            { patElems =
                                                                                                                                                [ PatElem
                                                                                                                                                    { patElemName = VName "loop_init" 6319,
                                                                                                                                                      patElemDec =
                                                                                                                                                        Array
                                                                                                                                                          (IntType Int64)
                                                                                                                                                          ( Shape
                                                                                                                                                              { shapeDims =
                                                                                                                                                                  [ Constant
                                                                                                                                                                      ( IntValue
                                                                                                                                                                          (Int64Value 1)
                                                                                                                                                                      )
                                                                                                                                                                  ]
                                                                                                                                                              }
                                                                                                                                                          )
                                                                                                                                                          NoUniqueness
                                                                                                                                                    }
                                                                                                                                                ]
                                                                                                                                            },
                                                                                                                                        stmAux =
                                                                                                                                          StmAux
                                                                                                                                            { stmAuxCerts =
                                                                                                                                                Certs
                                                                                                                                                  { unCerts = []
                                                                                                                                                  },
                                                                                                                                              stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                              stmAuxDec = ()
                                                                                                                                            },
                                                                                                                                        stmExp =
                                                                                                                                          BasicOp
                                                                                                                                            ( Index
                                                                                                                                                (VName "loop_init_r" 6313)
                                                                                                                                                ( Slice
                                                                                                                                                    { unSlice =
                                                                                                                                                        [ DimFix (Var (VName "gtid" 6315)),
                                                                                                                                                          DimSlice
                                                                                                                                                            ( Constant
                                                                                                                                                                ( IntValue
                                                                                                                                                                    (Int64Value 0)
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                            ( Constant
                                                                                                                                                                ( IntValue
                                                                                                                                                                    (Int64Value 1)
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                            ( Constant
                                                                                                                                                                ( IntValue
                                                                                                                                                                    (Int64Value 1)
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                        ]
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                                            )
                                                                                                                                      },
                                                                                                                                    Let
                                                                                                                                      { stmPat =
                                                                                                                                          Pat
                                                                                                                                            { patElems =
                                                                                                                                                [ PatElem
                                                                                                                                                    { patElemName = VName "wew" 6320,
                                                                                                                                                      patElemDec = Prim (IntType Int64)
                                                                                                                                                    },
                                                                                                                                                  PatElem
                                                                                                                                                    { patElemName = VName "wew" 6321,
                                                                                                                                                      patElemDec =
                                                                                                                                                        Array
                                                                                                                                                          (IntType Int64)
                                                                                                                                                          ( Shape
                                                                                                                                                              { shapeDims =
                                                                                                                                                                  [ Var (VName "wew" 6320)
                                                                                                                                                                  ]
                                                                                                                                                              }
                                                                                                                                                          )
                                                                                                                                                          NoUniqueness
                                                                                                                                                    }
                                                                                                                                                ]
                                                                                                                                            },
                                                                                                                                        stmAux =
                                                                                                                                          StmAux
                                                                                                                                            { stmAuxCerts =
                                                                                                                                                Certs
                                                                                                                                                  { unCerts = []
                                                                                                                                                  },
                                                                                                                                              stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                              stmAuxDec = ()
                                                                                                                                            },
                                                                                                                                        stmExp =
                                                                                                                                          Loop
                                                                                                                                            [ ( Param
                                                                                                                                                  { paramAttrs = Attrs {unAttrs = mempty},
                                                                                                                                                    paramName = VName "loop_d₂₀" 6323,
                                                                                                                                                    paramDec = Prim (IntType Int64)
                                                                                                                                                  },
                                                                                                                                                Constant
                                                                                                                                                  ( IntValue
                                                                                                                                                      (Int64Value 1)
                                                                                                                                                  )
                                                                                                                                              ),
                                                                                                                                              ( Param
                                                                                                                                                  { paramAttrs = Attrs {unAttrs = mempty},
                                                                                                                                                    paramName = VName "r" 6324,
                                                                                                                                                    paramDec =
                                                                                                                                                      Array
                                                                                                                                                        (IntType Int64)
                                                                                                                                                        ( Shape
                                                                                                                                                            { shapeDims =
                                                                                                                                                                [ Var (VName "loop_d₂₀" 6323)
                                                                                                                                                                ]
                                                                                                                                                            }
                                                                                                                                                        )
                                                                                                                                                        Nonunique
                                                                                                                                                  },
                                                                                                                                                Var (VName "loop_init" 6319)
                                                                                                                                              )
                                                                                                                                            ]
                                                                                                                                            ( ForLoop
                                                                                                                                                ( VName "j" 6322
                                                                                                                                                )
                                                                                                                                                Int64
                                                                                                                                                (Var (VName "n" 5718))
                                                                                                                                            )
                                                                                                                                            ( Body
                                                                                                                                                { bodyDec = (),
                                                                                                                                                  bodyStms =
                                                                                                                                                    S.fromList
                                                                                                                                                      [ Let
                                                                                                                                                          { stmPat =
                                                                                                                                                              Pat
                                                                                                                                                                { patElems =
                                                                                                                                                                    [ PatElem
                                                                                                                                                                        { patElemName = VName "+_lhs" 6325,
                                                                                                                                                                          patElemDec = Prim (IntType Int64)
                                                                                                                                                                        }
                                                                                                                                                                    ]
                                                                                                                                                                },
                                                                                                                                                            stmAux =
                                                                                                                                                              StmAux
                                                                                                                                                                { stmAuxCerts =
                                                                                                                                                                    Certs
                                                                                                                                                                      { unCerts = []
                                                                                                                                                                      },
                                                                                                                                                                  stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                                                  stmAuxDec = ()
                                                                                                                                                                },
                                                                                                                                                            stmExp =
                                                                                                                                                              BasicOp
                                                                                                                                                                ( BinOp
                                                                                                                                                                    (Mul Int64 OverflowWrap)
                                                                                                                                                                    ( Var (VName "i" 6299)
                                                                                                                                                                    )
                                                                                                                                                                    ( Var (VName "j" 6322)
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                          },
                                                                                                                                                        Let
                                                                                                                                                          { stmPat =
                                                                                                                                                              Pat
                                                                                                                                                                { patElems =
                                                                                                                                                                    [ PatElem
                                                                                                                                                                        { patElemName = VName "+_lhs" 6326,
                                                                                                                                                                          patElemDec = Prim (IntType Int64)
                                                                                                                                                                        }
                                                                                                                                                                    ]
                                                                                                                                                                },
                                                                                                                                                            stmAux =
                                                                                                                                                              StmAux
                                                                                                                                                                { stmAuxCerts =
                                                                                                                                                                    Certs
                                                                                                                                                                      { unCerts = []
                                                                                                                                                                      },
                                                                                                                                                                  stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                                                  stmAuxDec = ()
                                                                                                                                                                },
                                                                                                                                                            stmExp =
                                                                                                                                                              BasicOp
                                                                                                                                                                ( Index
                                                                                                                                                                    (VName "xsss" 5720)
                                                                                                                                                                    ( Slice
                                                                                                                                                                        { unSlice =
                                                                                                                                                                            [ DimFix (Var (VName "gtid" 6292)),
                                                                                                                                                                              DimFix (Var (VName "map2_arg2" 6301)),
                                                                                                                                                                              DimFix (Var (VName "gtid" 6315)),
                                                                                                                                                                              DimFix (Var (VName "+_lhs" 6325))
                                                                                                                                                                            ]
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                          },
                                                                                                                                                        Let
                                                                                                                                                          { stmPat =
                                                                                                                                                              Pat
                                                                                                                                                                { patElems =
                                                                                                                                                                    [ PatElem
                                                                                                                                                                        { patElemName = VName "+_rhs" 6327,
                                                                                                                                                                          patElemDec = Prim (IntType Int64)
                                                                                                                                                                        }
                                                                                                                                                                    ]
                                                                                                                                                                },
                                                                                                                                                            stmAux =
                                                                                                                                                              StmAux
                                                                                                                                                                { stmAuxCerts =
                                                                                                                                                                    Certs
                                                                                                                                                                      { unCerts = []
                                                                                                                                                                      },
                                                                                                                                                                  stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                                                  stmAuxDec = ()
                                                                                                                                                                },
                                                                                                                                                            stmExp =
                                                                                                                                                              BasicOp
                                                                                                                                                                ( BinOp
                                                                                                                                                                    (Add Int64 OverflowWrap)
                                                                                                                                                                    ( Var (VName "i" 6299)
                                                                                                                                                                    )
                                                                                                                                                                    ( Var (VName "j" 6322)
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                          },
                                                                                                                                                        Let
                                                                                                                                                          { stmPat =
                                                                                                                                                              Pat
                                                                                                                                                                { patElems =
                                                                                                                                                                    [ PatElem
                                                                                                                                                                        { patElemName = VName "+_rhs" 6328,
                                                                                                                                                                          patElemDec = Prim (IntType Int64)
                                                                                                                                                                        }
                                                                                                                                                                    ]
                                                                                                                                                                },
                                                                                                                                                            stmAux =
                                                                                                                                                              StmAux
                                                                                                                                                                { stmAuxCerts =
                                                                                                                                                                    Certs
                                                                                                                                                                      { unCerts = []
                                                                                                                                                                      },
                                                                                                                                                                  stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                                                  stmAuxDec = ()
                                                                                                                                                                },
                                                                                                                                                            stmExp =
                                                                                                                                                              BasicOp
                                                                                                                                                                ( Index
                                                                                                                                                                    (VName "res" 6300)
                                                                                                                                                                    ( Slice
                                                                                                                                                                        { unSlice =
                                                                                                                                                                            [ DimFix (Var (VName "gtid" 6315)),
                                                                                                                                                                              DimFix (Var (VName "+_rhs" 6327))
                                                                                                                                                                            ]
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                          },
                                                                                                                                                        Let
                                                                                                                                                          { stmPat =
                                                                                                                                                              Pat
                                                                                                                                                                { patElems =
                                                                                                                                                                    [ PatElem
                                                                                                                                                                        { patElemName = VName "++_rhs" 6329,
                                                                                                                                                                          patElemDec = Prim (IntType Int64)
                                                                                                                                                                        }
                                                                                                                                                                    ]
                                                                                                                                                                },
                                                                                                                                                            stmAux =
                                                                                                                                                              StmAux
                                                                                                                                                                { stmAuxCerts =
                                                                                                                                                                    Certs
                                                                                                                                                                      { unCerts = []
                                                                                                                                                                      },
                                                                                                                                                                  stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                                                  stmAuxDec = ()
                                                                                                                                                                },
                                                                                                                                                            stmExp =
                                                                                                                                                              BasicOp
                                                                                                                                                                ( BinOp
                                                                                                                                                                    (Add Int64 OverflowWrap)
                                                                                                                                                                    ( Var (VName "+_lhs" 6326)
                                                                                                                                                                    )
                                                                                                                                                                    ( Var (VName "+_rhs" 6328)
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                          },
                                                                                                                                                        Let
                                                                                                                                                          { stmPat =
                                                                                                                                                              Pat
                                                                                                                                                                { patElems =
                                                                                                                                                                    [ PatElem
                                                                                                                                                                        { patElemName = VName "++_arg" 6330,
                                                                                                                                                                          patElemDec =
                                                                                                                                                                            Array
                                                                                                                                                                              (IntType Int64)
                                                                                                                                                                              ( Shape
                                                                                                                                                                                  { shapeDims =
                                                                                                                                                                                      [ Constant
                                                                                                                                                                                          ( IntValue
                                                                                                                                                                                              (Int64Value 1)
                                                                                                                                                                                          )
                                                                                                                                                                                      ]
                                                                                                                                                                                  }
                                                                                                                                                                              )
                                                                                                                                                                              NoUniqueness
                                                                                                                                                                        }
                                                                                                                                                                    ]
                                                                                                                                                                },
                                                                                                                                                            stmAux =
                                                                                                                                                              StmAux
                                                                                                                                                                { stmAuxCerts =
                                                                                                                                                                    Certs
                                                                                                                                                                      { unCerts = []
                                                                                                                                                                      },
                                                                                                                                                                  stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                                                  stmAuxDec = ()
                                                                                                                                                                },
                                                                                                                                                            stmExp =
                                                                                                                                                              BasicOp
                                                                                                                                                                ( Replicate
                                                                                                                                                                    ( Shape
                                                                                                                                                                        { shapeDims =
                                                                                                                                                                            [ Constant
                                                                                                                                                                                ( IntValue
                                                                                                                                                                                    (Int64Value 1)
                                                                                                                                                                                )
                                                                                                                                                                            ]
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                    ( Var (VName "++_rhs" 6329)
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                          },
                                                                                                                                                        Let
                                                                                                                                                          { stmPat =
                                                                                                                                                              Pat
                                                                                                                                                                { patElems =
                                                                                                                                                                    [ PatElem
                                                                                                                                                                        { patElemName = VName "conc_tmp" 6331,
                                                                                                                                                                          patElemDec = Prim (IntType Int64)
                                                                                                                                                                        }
                                                                                                                                                                    ]
                                                                                                                                                                },
                                                                                                                                                            stmAux =
                                                                                                                                                              StmAux
                                                                                                                                                                { stmAuxCerts =
                                                                                                                                                                    Certs
                                                                                                                                                                      { unCerts = []
                                                                                                                                                                      },
                                                                                                                                                                  stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                                                  stmAuxDec = ()
                                                                                                                                                                },
                                                                                                                                                            stmExp =
                                                                                                                                                              BasicOp
                                                                                                                                                                ( BinOp
                                                                                                                                                                    (Add Int64 OverflowUndef)
                                                                                                                                                                    ( Constant
                                                                                                                                                                        ( IntValue
                                                                                                                                                                            (Int64Value 1)
                                                                                                                                                                        )
                                                                                                                                                                    )
                                                                                                                                                                    ( Var (VName "loop_d₂₀" 6323)
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                          },
                                                                                                                                                        Let
                                                                                                                                                          { stmPat =
                                                                                                                                                              Pat
                                                                                                                                                                { patElems =
                                                                                                                                                                    [ PatElem
                                                                                                                                                                        { patElemName = VName "++_res" 6332,
                                                                                                                                                                          patElemDec =
                                                                                                                                                                            Array
                                                                                                                                                                              (IntType Int64)
                                                                                                                                                                              ( Shape
                                                                                                                                                                                  { shapeDims =
                                                                                                                                                                                      [Var (VName "conc_tmp" 6331)]
                                                                                                                                                                                  }
                                                                                                                                                                              )
                                                                                                                                                                              NoUniqueness
                                                                                                                                                                        }
                                                                                                                                                                    ]
                                                                                                                                                                },
                                                                                                                                                            stmAux =
                                                                                                                                                              StmAux
                                                                                                                                                                { stmAuxCerts =
                                                                                                                                                                    Certs
                                                                                                                                                                      { unCerts = []
                                                                                                                                                                      },
                                                                                                                                                                  stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                                                  stmAuxDec = ()
                                                                                                                                                                },
                                                                                                                                                            stmExp =
                                                                                                                                                              BasicOp
                                                                                                                                                                ( Concat
                                                                                                                                                                    0
                                                                                                                                                                    ( VName "r" 6324
                                                                                                                                                                        :| [VName "++_arg" 6330]
                                                                                                                                                                    )
                                                                                                                                                                    ( Var (VName "conc_tmp" 6331)
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                          }
                                                                                                                                                      ],
                                                                                                                                                  bodyResult =
                                                                                                                                                    [ SubExpRes
                                                                                                                                                        { resCerts = Certs {unCerts = []},
                                                                                                                                                          resSubExp =
                                                                                                                                                            Var
                                                                                                                                                              (VName "conc_tmp" 6331)
                                                                                                                                                        },
                                                                                                                                                      SubExpRes
                                                                                                                                                        { resCerts = Certs {unCerts = []},
                                                                                                                                                          resSubExp =
                                                                                                                                                            Var
                                                                                                                                                              (VName "++_res" 6332)
                                                                                                                                                        }
                                                                                                                                                    ]
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                      },
                                                                                                                                    Let
                                                                                                                                      { stmPat =
                                                                                                                                          Pat
                                                                                                                                            { patElems =
                                                                                                                                                [ PatElem
                                                                                                                                                    { patElemName = VName "result_proper_shape" 6333,
                                                                                                                                                      patElemDec =
                                                                                                                                                        Array
                                                                                                                                                          (IntType Int64)
                                                                                                                                                          ( Shape
                                                                                                                                                              { shapeDims =
                                                                                                                                                                  [ Var (VName "o" 5719)
                                                                                                                                                                  ]
                                                                                                                                                              }
                                                                                                                                                          )
                                                                                                                                                          NoUniqueness
                                                                                                                                                    }
                                                                                                                                                ]
                                                                                                                                            },
                                                                                                                                        stmAux =
                                                                                                                                          StmAux
                                                                                                                                            { stmAuxCerts =
                                                                                                                                                Certs
                                                                                                                                                  { unCerts = []
                                                                                                                                                  },
                                                                                                                                              stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                              stmAuxDec = ()
                                                                                                                                            },
                                                                                                                                        stmExp =
                                                                                                                                          BasicOp
                                                                                                                                            ( Reshape
                                                                                                                                                ReshapeCoerce
                                                                                                                                                ( Shape
                                                                                                                                                    { shapeDims =
                                                                                                                                                        [ Var
                                                                                                                                                            ( VName "o" 5719
                                                                                                                                                            )
                                                                                                                                                        ]
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                                                (VName "wew" 6321)
                                                                                                                                            )
                                                                                                                                      }
                                                                                                                                  ],
                                                                                                                              kernelBodyResult =
                                                                                                                                [ Returns
                                                                                                                                    ResultMaySimplify
                                                                                                                                    ( Certs
                                                                                                                                        { unCerts = []
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                    ( Var (VName "result_proper_shape" 6333)
                                                                                                                                    )
                                                                                                                                ]
                                                                                                                            }
                                                                                                                        )
                                                                                                                    )
                                                                                                                )
                                                                                                          }
                                                                                                      ],
                                                                                                  bodyResult =
                                                                                                    [ SubExpRes
                                                                                                        { resCerts = Certs {unCerts = []},
                                                                                                          resSubExp =
                                                                                                            Var
                                                                                                              (VName "defunc_0_map_res" 6314)
                                                                                                        }
                                                                                                    ]
                                                                                                }
                                                                                            )
                                                                                      }
                                                                                  ],
                                                                              kernelBodyResult =
                                                                                [ Returns
                                                                                    ResultMaySimplify
                                                                                    ( Certs
                                                                                        { unCerts = []
                                                                                        }
                                                                                    )
                                                                                    ( Var (VName "lifted_lambda_res" 6298)
                                                                                    )
                                                                                ]
                                                                            }
                                                                        )
                                                                    )
                                                                )
                                                          }
                                                      ],
                                                  bodyResult =
                                                    [ SubExpRes
                                                        { resCerts =
                                                            Certs
                                                              { unCerts = []
                                                              },
                                                          resSubExp = Var (VName "defunc_0_map_res" 6291)
                                                        }
                                                    ]
                                                }
                                          }
                                      ]
                                      ( Body
                                          { bodyDec = (),
                                            bodyStms =
                                              S.fromList
                                                [ Let
                                                    { stmPat =
                                                        Pat
                                                          { patElems =
                                                              [ PatElem
                                                                  { patElemName = VName "res_expanded_inter_copy" 6491,
                                                                    patElemDec =
                                                                      Array
                                                                        (IntType Int64)
                                                                        ( Shape
                                                                            { shapeDims =
                                                                                [ Var (VName "l" 5716),
                                                                                  Var (VName "n" 5718),
                                                                                  Var (VName "o" 5719)
                                                                                ]
                                                                            }
                                                                        )
                                                                        NoUniqueness
                                                                  }
                                                              ]
                                                          },
                                                      stmAux =
                                                        StmAux
                                                          { stmAuxCerts =
                                                              Certs
                                                                { unCerts = []
                                                                },
                                                            stmAuxAttrs = Attrs {unAttrs = mempty},
                                                            stmAuxDec = ()
                                                          },
                                                      stmExp =
                                                        BasicOp
                                                          ( Replicate
                                                              ( Shape
                                                                  { shapeDims = []
                                                                  }
                                                              )
                                                              ( Var (VName "index" 6244)
                                                              )
                                                          )
                                                    },
                                                  Let
                                                    { stmPat =
                                                        Pat
                                                          { patElems =
                                                              [ PatElem
                                                                  { patElemName = VName "defunc_0_map_res" 6492,
                                                                    patElemDec =
                                                                      Array
                                                                        (IntType Int64)
                                                                        ( Shape
                                                                            { shapeDims =
                                                                                [ Var (VName "l" 5716),
                                                                                  Var (VName "n" 5718),
                                                                                  Var (VName "o" 5719)
                                                                                ]
                                                                            }
                                                                        )
                                                                        NoUniqueness
                                                                  }
                                                              ]
                                                          },
                                                      stmAux =
                                                        StmAux
                                                          { stmAuxCerts =
                                                              Certs
                                                                { unCerts = []
                                                                },
                                                            stmAuxAttrs = Attrs {unAttrs = mempty},
                                                            stmAuxDec = ()
                                                          },
                                                      stmExp =
                                                        Loop
                                                          [ ( Param
                                                                { paramAttrs = Attrs {unAttrs = mempty},
                                                                  paramName = VName "res_expanded" 6494,
                                                                  paramDec =
                                                                    Array
                                                                      (IntType Int64)
                                                                      ( Shape
                                                                          { shapeDims =
                                                                              [ Var (VName "l" 5716),
                                                                                Var (VName "n" 5718),
                                                                                Var (VName "o" 5719)
                                                                              ]
                                                                          }
                                                                      )
                                                                      Unique
                                                                },
                                                              Var (VName "res_expanded_inter_copy" 6491)
                                                            )
                                                          ]
                                                          ( ForLoop
                                                              ( VName "i" 6493
                                                              )
                                                              Int64
                                                              ( Var (VName "m" 5717)
                                                              )
                                                          )
                                                          ( Body
                                                              { bodyDec = (),
                                                                bodyStms =
                                                                  S.fromList
                                                                    [ Let
                                                                        { stmPat =
                                                                            Pat
                                                                              { patElems =
                                                                                  [ PatElem
                                                                                      { patElemName = VName "map2_arg2_dev" 6549,
                                                                                        patElemDec =
                                                                                          Array
                                                                                            (IntType Int64)
                                                                                            ( Shape
                                                                                                { shapeDims =
                                                                                                    [ Constant
                                                                                                        ( IntValue
                                                                                                            (Int64Value 1)
                                                                                                        )
                                                                                                    ]
                                                                                                }
                                                                                            )
                                                                                            NoUniqueness
                                                                                      }
                                                                                  ]
                                                                              },
                                                                          stmAux =
                                                                            StmAux
                                                                              { stmAuxCerts =
                                                                                  Certs
                                                                                    { unCerts = []
                                                                                    },
                                                                                stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                stmAuxDec = ()
                                                                              },
                                                                          stmExp =
                                                                            Op
                                                                              ( GPUBody
                                                                                  [Prim (IntType Int64)]
                                                                                  ( Body
                                                                                      { bodyDec = (),
                                                                                        bodyStms =
                                                                                          S.fromList
                                                                                            [ Let
                                                                                                { stmPat =
                                                                                                    Pat
                                                                                                      { patElems =
                                                                                                          [ PatElem
                                                                                                              { patElemName = VName "map2_arg2" 6548,
                                                                                                                patElemDec = Prim (IntType Int64)
                                                                                                              }
                                                                                                          ]
                                                                                                      },
                                                                                                  stmAux =
                                                                                                    StmAux
                                                                                                      { stmAuxCerts =
                                                                                                          Certs
                                                                                                            { unCerts = []
                                                                                                            },
                                                                                                        stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                        stmAuxDec = ()
                                                                                                      },
                                                                                                  stmExp =
                                                                                                    BasicOp
                                                                                                      ( Index
                                                                                                          (VName "is2" 5722)
                                                                                                          ( Slice
                                                                                                              { unSlice =
                                                                                                                  [ DimFix
                                                                                                                      (Var (VName "i" 6493))
                                                                                                                  ]
                                                                                                              }
                                                                                                          )
                                                                                                      )
                                                                                                }
                                                                                            ],
                                                                                        bodyResult =
                                                                                          [ SubExpRes
                                                                                              { resCerts =
                                                                                                  Certs
                                                                                                    { unCerts = []
                                                                                                    },
                                                                                                resSubExp = Var (VName "map2_arg2" 6548)
                                                                                              }
                                                                                          ]
                                                                                      }
                                                                                  )
                                                                              )
                                                                        },
                                                                      Let
                                                                        { stmPat =
                                                                            Pat
                                                                              { patElems =
                                                                                  [ PatElem
                                                                                      { patElemName = VName "wew_r_r" 6506,
                                                                                        patElemDec =
                                                                                          Array
                                                                                            (IntType Int64)
                                                                                            ( Shape
                                                                                                { shapeDims =
                                                                                                    [ Var (VName "l" 5716),
                                                                                                      Var (VName "n" 5718)
                                                                                                    ]
                                                                                                }
                                                                                            )
                                                                                            NoUniqueness
                                                                                      }
                                                                                  ]
                                                                              },
                                                                          stmAux =
                                                                            StmAux
                                                                              { stmAuxCerts =
                                                                                  Certs
                                                                                    { unCerts = []
                                                                                    },
                                                                                stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                stmAuxDec = ()
                                                                              },
                                                                          stmExp =
                                                                            Op
                                                                              ( SegOp
                                                                                  ( SegMap
                                                                                      ( SegThread
                                                                                          SegNoVirt
                                                                                          ( Just
                                                                                              ( KernelGrid
                                                                                                  { gridNumGroups =
                                                                                                      Count
                                                                                                        { unCount = Var (VName "segmap_usable_groups" 6505)
                                                                                                        },
                                                                                                    gridGroupSize =
                                                                                                      Count
                                                                                                        { unCount = Var (VName "segmap_group_size" 6504)
                                                                                                        }
                                                                                                  }
                                                                                              )
                                                                                          )
                                                                                      )
                                                                                      ( SegSpace
                                                                                          { segFlat = VName "phys_tid" 6509,
                                                                                            unSegSpace =
                                                                                              [ ( VName "gtid" 6507,
                                                                                                  Var (VName "l" 5716)
                                                                                                ),
                                                                                                ( VName "gtid" 6508,
                                                                                                  Var (VName "n" 5718)
                                                                                                )
                                                                                              ]
                                                                                          }
                                                                                      )
                                                                                      [Prim (IntType Int64)]
                                                                                      ( KernelBody
                                                                                          { kernelBodyDec = (),
                                                                                            kernelBodyStms =
                                                                                              S.fromList
                                                                                                [ Let
                                                                                                    { stmPat =
                                                                                                        Pat
                                                                                                          { patElems =
                                                                                                              [ PatElem
                                                                                                                  { patElemName = VName "map2_arg2" 6550,
                                                                                                                    patElemDec = Prim (IntType Int64)
                                                                                                                  }
                                                                                                              ]
                                                                                                          },
                                                                                                      stmAux =
                                                                                                        StmAux
                                                                                                          { stmAuxCerts =
                                                                                                              Certs
                                                                                                                { unCerts = []
                                                                                                                },
                                                                                                            stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                            stmAuxDec = ()
                                                                                                          },
                                                                                                      stmExp =
                                                                                                        BasicOp
                                                                                                          ( Index
                                                                                                              ( VName "map2_arg2_dev" 6549
                                                                                                              )
                                                                                                              ( Slice
                                                                                                                  { unSlice =
                                                                                                                      [ DimFix
                                                                                                                          ( Constant
                                                                                                                              ( IntValue
                                                                                                                                  (Int64Value 0)
                                                                                                                              )
                                                                                                                          )
                                                                                                                      ]
                                                                                                                  }
                                                                                                              )
                                                                                                          )
                                                                                                    },
                                                                                                  Let
                                                                                                    { stmPat =
                                                                                                        Pat
                                                                                                          { patElems =
                                                                                                              [ PatElem
                                                                                                                  { patElemName = VName "+_lhs" 6512,
                                                                                                                    patElemDec = Prim (IntType Int64)
                                                                                                                  }
                                                                                                              ]
                                                                                                          },
                                                                                                      stmAux =
                                                                                                        StmAux
                                                                                                          { stmAuxCerts =
                                                                                                              Certs
                                                                                                                { unCerts = []
                                                                                                                },
                                                                                                            stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                            stmAuxDec = ()
                                                                                                          },
                                                                                                      stmExp =
                                                                                                        BasicOp
                                                                                                          ( Index
                                                                                                              ( VName "xsss" 5720
                                                                                                              )
                                                                                                              ( Slice
                                                                                                                  { unSlice =
                                                                                                                      [ DimFix (Var (VName "gtid" 6507)),
                                                                                                                        DimFix (Var (VName "map2_arg2" 6550)),
                                                                                                                        DimFix (Var (VName "gtid" 6508)),
                                                                                                                        DimFix
                                                                                                                          ( Constant
                                                                                                                              ( IntValue
                                                                                                                                  (Int64Value 0)
                                                                                                                              )
                                                                                                                          )
                                                                                                                      ]
                                                                                                                  }
                                                                                                              )
                                                                                                          )
                                                                                                    },
                                                                                                  Let
                                                                                                    { stmPat =
                                                                                                        Pat
                                                                                                          { patElems =
                                                                                                              [ PatElem
                                                                                                                  { patElemName = VName "+_rhs" 6513,
                                                                                                                    patElemDec = Prim (IntType Int64)
                                                                                                                  }
                                                                                                              ]
                                                                                                          },
                                                                                                      stmAux =
                                                                                                        StmAux
                                                                                                          { stmAuxCerts =
                                                                                                              Certs
                                                                                                                { unCerts = []
                                                                                                                },
                                                                                                            stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                            stmAuxDec = ()
                                                                                                          },
                                                                                                      stmExp =
                                                                                                        BasicOp
                                                                                                          ( Index
                                                                                                              ( VName "res_expanded" 6494
                                                                                                              )
                                                                                                              ( Slice
                                                                                                                  { unSlice =
                                                                                                                      [ DimFix (Var (VName "gtid" 6507)),
                                                                                                                        DimFix (Var (VName "gtid" 6508)),
                                                                                                                        DimFix
                                                                                                                          ( Constant
                                                                                                                              ( IntValue
                                                                                                                                  (Int64Value 0)
                                                                                                                              )
                                                                                                                          )
                                                                                                                      ]
                                                                                                                  }
                                                                                                              )
                                                                                                          )
                                                                                                    },
                                                                                                  Let
                                                                                                    { stmPat =
                                                                                                        Pat
                                                                                                          { patElems =
                                                                                                              [ PatElem
                                                                                                                  { patElemName = VName "wew" 6514,
                                                                                                                    patElemDec = Prim (IntType Int64)
                                                                                                                  }
                                                                                                              ]
                                                                                                          },
                                                                                                      stmAux =
                                                                                                        StmAux
                                                                                                          { stmAuxCerts =
                                                                                                              Certs
                                                                                                                { unCerts = []
                                                                                                                },
                                                                                                            stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                            stmAuxDec = ()
                                                                                                          },
                                                                                                      stmExp =
                                                                                                        BasicOp
                                                                                                          ( BinOp
                                                                                                              (Add Int64 OverflowWrap)
                                                                                                              (Var (VName "+_lhs" 6512))
                                                                                                              (Var (VName "+_rhs" 6513))
                                                                                                          )
                                                                                                    }
                                                                                                ],
                                                                                            kernelBodyResult =
                                                                                              [ Returns
                                                                                                  ResultMaySimplify
                                                                                                  ( Certs
                                                                                                      { unCerts = []
                                                                                                      }
                                                                                                  )
                                                                                                  ( Var (VName "wew" 6514)
                                                                                                  )
                                                                                              ]
                                                                                          }
                                                                                      )
                                                                                  )
                                                                              )
                                                                        },
                                                                      Let
                                                                        { stmPat =
                                                                            Pat
                                                                              { patElems =
                                                                                  [ PatElem
                                                                                      { patElemName = VName "wew_r_r_tr_rep" 6516,
                                                                                        patElemDec =
                                                                                          Array
                                                                                            (IntType Int64)
                                                                                            ( Shape
                                                                                                { shapeDims =
                                                                                                    [ Constant
                                                                                                        ( IntValue
                                                                                                            (Int64Value 1)
                                                                                                        ),
                                                                                                      Var (VName "l" 5716),
                                                                                                      Var (VName "n" 5718)
                                                                                                    ]
                                                                                                }
                                                                                            )
                                                                                            NoUniqueness
                                                                                      }
                                                                                  ]
                                                                              },
                                                                          stmAux =
                                                                            StmAux
                                                                              { stmAuxCerts =
                                                                                  Certs
                                                                                    { unCerts = []
                                                                                    },
                                                                                stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                stmAuxDec = ()
                                                                              },
                                                                          stmExp =
                                                                            BasicOp
                                                                              ( Replicate
                                                                                  ( Shape
                                                                                      { shapeDims =
                                                                                          [ Constant
                                                                                              ( IntValue
                                                                                                  (Int64Value 1)
                                                                                              )
                                                                                          ]
                                                                                      }
                                                                                  )
                                                                                  ( Var (VName "wew_r_r" 6506)
                                                                                  )
                                                                              )
                                                                        },
                                                                      Let
                                                                        { stmPat =
                                                                            Pat
                                                                              { patElems =
                                                                                  [ PatElem
                                                                                      { patElemName = VName "loop_init_r_r" 6517,
                                                                                        patElemDec =
                                                                                          Array
                                                                                            (IntType Int64)
                                                                                            ( Shape
                                                                                                { shapeDims =
                                                                                                    [ Var (VName "l" 5716),
                                                                                                      Var (VName "n" 5718),
                                                                                                      Constant
                                                                                                        ( IntValue
                                                                                                            (Int64Value 1)
                                                                                                        )
                                                                                                    ]
                                                                                                }
                                                                                            )
                                                                                            NoUniqueness
                                                                                      }
                                                                                  ]
                                                                              },
                                                                          stmAux =
                                                                            StmAux
                                                                              { stmAuxCerts =
                                                                                  Certs
                                                                                    { unCerts = []
                                                                                    },
                                                                                stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                stmAuxDec = ()
                                                                              },
                                                                          stmExp =
                                                                            BasicOp
                                                                              ( Rearrange
                                                                                  [ 1,
                                                                                    2,
                                                                                    0
                                                                                  ]
                                                                                  (VName "wew_r_r_tr_rep" 6516)
                                                                              )
                                                                        },
                                                                      Let
                                                                        { stmPat =
                                                                            Pat
                                                                              { patElems =
                                                                                  [ PatElem
                                                                                      { patElemName = VName "lifted_lambda_res" 6523,
                                                                                        patElemDec =
                                                                                          Array
                                                                                            (IntType Int64)
                                                                                            ( Shape
                                                                                                { shapeDims =
                                                                                                    [ Var (VName "l" 5716),
                                                                                                      Var (VName "n" 5718),
                                                                                                      Var (VName "o" 5719)
                                                                                                    ]
                                                                                                }
                                                                                            )
                                                                                            NoUniqueness
                                                                                      }
                                                                                  ]
                                                                              },
                                                                          stmAux =
                                                                            StmAux
                                                                              { stmAuxCerts =
                                                                                  Certs
                                                                                    { unCerts = []
                                                                                    },
                                                                                stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                stmAuxDec = ()
                                                                              },
                                                                          stmExp =
                                                                            Op
                                                                              ( SegOp
                                                                                  ( SegMap
                                                                                      ( SegThread
                                                                                          SegVirt
                                                                                          ( Just
                                                                                              ( KernelGrid
                                                                                                  { gridNumGroups =
                                                                                                      Count
                                                                                                        { unCount =
                                                                                                            Var
                                                                                                              ( VName "num_groups" 6521
                                                                                                              )
                                                                                                        },
                                                                                                    gridGroupSize =
                                                                                                      Count
                                                                                                        { unCount =
                                                                                                            Var
                                                                                                              ( VName "segmap_group_size" 6520
                                                                                                              )
                                                                                                        }
                                                                                                  }
                                                                                              )
                                                                                          )
                                                                                      )
                                                                                      ( SegSpace
                                                                                          { segFlat = VName "phys_tid" 6526,
                                                                                            unSegSpace =
                                                                                              [ ( VName "gtid" 6524,
                                                                                                  Var (VName "l" 5716)
                                                                                                ),
                                                                                                ( VName "gtid" 6525,
                                                                                                  Var (VName "n" 5718)
                                                                                                )
                                                                                              ]
                                                                                          }
                                                                                      )
                                                                                      [ Array
                                                                                          (IntType Int64)
                                                                                          ( Shape
                                                                                              { shapeDims =
                                                                                                  [ Var (VName "o" 5719)
                                                                                                  ]
                                                                                              }
                                                                                          )
                                                                                          NoUniqueness
                                                                                      ]
                                                                                      ( KernelBody
                                                                                          { kernelBodyDec = (),
                                                                                            kernelBodyStms =
                                                                                              S.fromList
                                                                                                [ Let
                                                                                                    { stmPat =
                                                                                                        Pat
                                                                                                          { patElems =
                                                                                                              [ PatElem
                                                                                                                  { patElemName = VName "map2_arg2" 6551,
                                                                                                                    patElemDec = Prim (IntType Int64)
                                                                                                                  }
                                                                                                              ]
                                                                                                          },
                                                                                                      stmAux =
                                                                                                        StmAux
                                                                                                          { stmAuxCerts =
                                                                                                              Certs
                                                                                                                { unCerts = []
                                                                                                                },
                                                                                                            stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                            stmAuxDec = ()
                                                                                                          },
                                                                                                      stmExp =
                                                                                                        BasicOp
                                                                                                          ( Index
                                                                                                              ( VName "map2_arg2_dev" 6549
                                                                                                              )
                                                                                                              ( Slice
                                                                                                                  { unSlice =
                                                                                                                      [ DimFix
                                                                                                                          ( Constant
                                                                                                                              ( IntValue
                                                                                                                                  (Int64Value 0)
                                                                                                                              )
                                                                                                                          )
                                                                                                                      ]
                                                                                                                  }
                                                                                                              )
                                                                                                          )
                                                                                                    },
                                                                                                  Let
                                                                                                    { stmPat =
                                                                                                        Pat
                                                                                                          { patElems =
                                                                                                              [ PatElem
                                                                                                                  { patElemName = VName "loop_init" 6529,
                                                                                                                    patElemDec =
                                                                                                                      Array
                                                                                                                        (IntType Int64)
                                                                                                                        ( Shape
                                                                                                                            { shapeDims =
                                                                                                                                [ Constant
                                                                                                                                    ( IntValue
                                                                                                                                        (Int64Value 1)
                                                                                                                                    )
                                                                                                                                ]
                                                                                                                            }
                                                                                                                        )
                                                                                                                        NoUniqueness
                                                                                                                  }
                                                                                                              ]
                                                                                                          },
                                                                                                      stmAux =
                                                                                                        StmAux
                                                                                                          { stmAuxCerts =
                                                                                                              Certs
                                                                                                                { unCerts = []
                                                                                                                },
                                                                                                            stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                            stmAuxDec = ()
                                                                                                          },
                                                                                                      stmExp =
                                                                                                        BasicOp
                                                                                                          ( Index
                                                                                                              ( VName "loop_init_r_r" 6517
                                                                                                              )
                                                                                                              ( Slice
                                                                                                                  { unSlice =
                                                                                                                      [ DimFix (Var (VName "gtid" 6524)),
                                                                                                                        DimFix (Var (VName "gtid" 6525)),
                                                                                                                        DimSlice
                                                                                                                          ( Constant
                                                                                                                              ( IntValue
                                                                                                                                  (Int64Value 0)
                                                                                                                              )
                                                                                                                          )
                                                                                                                          ( Constant
                                                                                                                              ( IntValue
                                                                                                                                  (Int64Value 1)
                                                                                                                              )
                                                                                                                          )
                                                                                                                          ( Constant
                                                                                                                              ( IntValue
                                                                                                                                  (Int64Value 1)
                                                                                                                              )
                                                                                                                          )
                                                                                                                      ]
                                                                                                                  }
                                                                                                              )
                                                                                                          )
                                                                                                    },
                                                                                                  Let
                                                                                                    { stmPat =
                                                                                                        Pat
                                                                                                          { patElems =
                                                                                                              [ PatElem
                                                                                                                  { patElemName = VName "wew" 6530,
                                                                                                                    patElemDec = Prim (IntType Int64)
                                                                                                                  },
                                                                                                                PatElem
                                                                                                                  { patElemName = VName "wew" 6531,
                                                                                                                    patElemDec =
                                                                                                                      Array
                                                                                                                        (IntType Int64)
                                                                                                                        ( Shape
                                                                                                                            { shapeDims =
                                                                                                                                [Var (VName "wew" 6530)]
                                                                                                                            }
                                                                                                                        )
                                                                                                                        NoUniqueness
                                                                                                                  }
                                                                                                              ]
                                                                                                          },
                                                                                                      stmAux =
                                                                                                        StmAux
                                                                                                          { stmAuxCerts =
                                                                                                              Certs
                                                                                                                { unCerts = []
                                                                                                                },
                                                                                                            stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                            stmAuxDec = ()
                                                                                                          },
                                                                                                      stmExp =
                                                                                                        Loop
                                                                                                          [ ( Param
                                                                                                                { paramAttrs = Attrs {unAttrs = mempty},
                                                                                                                  paramName = VName "loop_d₂₀" 6533,
                                                                                                                  paramDec = Prim (IntType Int64)
                                                                                                                },
                                                                                                              Constant
                                                                                                                ( IntValue
                                                                                                                    (Int64Value 1)
                                                                                                                )
                                                                                                            ),
                                                                                                            ( Param
                                                                                                                { paramAttrs = Attrs {unAttrs = mempty},
                                                                                                                  paramName = VName "r" 6534,
                                                                                                                  paramDec =
                                                                                                                    Array
                                                                                                                      (IntType Int64)
                                                                                                                      ( Shape
                                                                                                                          { shapeDims =
                                                                                                                              [Var (VName "loop_d₂₀" 6533)]
                                                                                                                          }
                                                                                                                      )
                                                                                                                      Nonunique
                                                                                                                },
                                                                                                              Var (VName "loop_init" 6529)
                                                                                                            )
                                                                                                          ]
                                                                                                          ( ForLoop
                                                                                                              ( VName "j" 6532
                                                                                                              )
                                                                                                              Int64
                                                                                                              (Var (VName "n" 5718))
                                                                                                          )
                                                                                                          ( Body
                                                                                                              { bodyDec = (),
                                                                                                                bodyStms =
                                                                                                                  S.fromList
                                                                                                                    [ Let
                                                                                                                        { stmPat =
                                                                                                                            Pat
                                                                                                                              { patElems =
                                                                                                                                  [ PatElem
                                                                                                                                      { patElemName = VName "+_lhs" 6535,
                                                                                                                                        patElemDec = Prim (IntType Int64)
                                                                                                                                      }
                                                                                                                                  ]
                                                                                                                              },
                                                                                                                          stmAux =
                                                                                                                            StmAux
                                                                                                                              { stmAuxCerts =
                                                                                                                                  Certs
                                                                                                                                    { unCerts = []
                                                                                                                                    },
                                                                                                                                stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                stmAuxDec = ()
                                                                                                                              },
                                                                                                                          stmExp =
                                                                                                                            BasicOp
                                                                                                                              ( BinOp
                                                                                                                                  (Mul Int64 OverflowWrap)
                                                                                                                                  (Var (VName "i" 6493))
                                                                                                                                  (Var (VName "j" 6532))
                                                                                                                              )
                                                                                                                        },
                                                                                                                      Let
                                                                                                                        { stmPat =
                                                                                                                            Pat
                                                                                                                              { patElems =
                                                                                                                                  [ PatElem
                                                                                                                                      { patElemName = VName "+_lhs" 6536,
                                                                                                                                        patElemDec = Prim (IntType Int64)
                                                                                                                                      }
                                                                                                                                  ]
                                                                                                                              },
                                                                                                                          stmAux =
                                                                                                                            StmAux
                                                                                                                              { stmAuxCerts =
                                                                                                                                  Certs
                                                                                                                                    { unCerts = []
                                                                                                                                    },
                                                                                                                                stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                stmAuxDec = ()
                                                                                                                              },
                                                                                                                          stmExp =
                                                                                                                            BasicOp
                                                                                                                              ( Index
                                                                                                                                  (VName "xsss" 5720)
                                                                                                                                  ( Slice
                                                                                                                                      { unSlice =
                                                                                                                                          [ DimFix (Var (VName "gtid" 6524)),
                                                                                                                                            DimFix (Var (VName "map2_arg2" 6551)),
                                                                                                                                            DimFix (Var (VName "gtid" 6525)),
                                                                                                                                            DimFix (Var (VName "+_lhs" 6535))
                                                                                                                                          ]
                                                                                                                                      }
                                                                                                                                  )
                                                                                                                              )
                                                                                                                        },
                                                                                                                      Let
                                                                                                                        { stmPat =
                                                                                                                            Pat
                                                                                                                              { patElems =
                                                                                                                                  [ PatElem
                                                                                                                                      { patElemName = VName "+_rhs" 6537,
                                                                                                                                        patElemDec = Prim (IntType Int64)
                                                                                                                                      }
                                                                                                                                  ]
                                                                                                                              },
                                                                                                                          stmAux =
                                                                                                                            StmAux
                                                                                                                              { stmAuxCerts =
                                                                                                                                  Certs
                                                                                                                                    { unCerts = []
                                                                                                                                    },
                                                                                                                                stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                stmAuxDec = ()
                                                                                                                              },
                                                                                                                          stmExp =
                                                                                                                            BasicOp
                                                                                                                              ( BinOp
                                                                                                                                  (Add Int64 OverflowWrap)
                                                                                                                                  (Var (VName "i" 6493))
                                                                                                                                  (Var (VName "j" 6532))
                                                                                                                              )
                                                                                                                        },
                                                                                                                      Let
                                                                                                                        { stmPat =
                                                                                                                            Pat
                                                                                                                              { patElems =
                                                                                                                                  [ PatElem
                                                                                                                                      { patElemName = VName "+_rhs" 6538,
                                                                                                                                        patElemDec = Prim (IntType Int64)
                                                                                                                                      }
                                                                                                                                  ]
                                                                                                                              },
                                                                                                                          stmAux =
                                                                                                                            StmAux
                                                                                                                              { stmAuxCerts =
                                                                                                                                  Certs
                                                                                                                                    { unCerts = []
                                                                                                                                    },
                                                                                                                                stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                stmAuxDec = ()
                                                                                                                              },
                                                                                                                          stmExp =
                                                                                                                            BasicOp
                                                                                                                              ( Index
                                                                                                                                  ( VName "res_expanded" 6494
                                                                                                                                  )
                                                                                                                                  ( Slice
                                                                                                                                      { unSlice =
                                                                                                                                          [ DimFix (Var (VName "gtid" 6524)),
                                                                                                                                            DimFix (Var (VName "gtid" 6525)),
                                                                                                                                            DimFix (Var (VName "+_rhs" 6537))
                                                                                                                                          ]
                                                                                                                                      }
                                                                                                                                  )
                                                                                                                              )
                                                                                                                        },
                                                                                                                      Let
                                                                                                                        { stmPat =
                                                                                                                            Pat
                                                                                                                              { patElems =
                                                                                                                                  [ PatElem
                                                                                                                                      { patElemName = VName "++_rhs" 6539,
                                                                                                                                        patElemDec = Prim (IntType Int64)
                                                                                                                                      }
                                                                                                                                  ]
                                                                                                                              },
                                                                                                                          stmAux =
                                                                                                                            StmAux
                                                                                                                              { stmAuxCerts =
                                                                                                                                  Certs
                                                                                                                                    { unCerts = []
                                                                                                                                    },
                                                                                                                                stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                stmAuxDec = ()
                                                                                                                              },
                                                                                                                          stmExp =
                                                                                                                            BasicOp
                                                                                                                              ( BinOp
                                                                                                                                  (Add Int64 OverflowWrap)
                                                                                                                                  (Var (VName "+_lh" 6536))
                                                                                                                                  (Var (VName "+_rh" 6538))
                                                                                                                              )
                                                                                                                        },
                                                                                                                      Let
                                                                                                                        { stmPat =
                                                                                                                            Pat
                                                                                                                              { patElems =
                                                                                                                                  [ PatElem
                                                                                                                                      { patElemName = VName "++_arg" 6540,
                                                                                                                                        patElemDec =
                                                                                                                                          Array
                                                                                                                                            (IntType Int64)
                                                                                                                                            ( Shape
                                                                                                                                                { shapeDims =
                                                                                                                                                    [ Constant
                                                                                                                                                        ( IntValue
                                                                                                                                                            (Int64Value 1)
                                                                                                                                                        )
                                                                                                                                                    ]
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                            NoUniqueness
                                                                                                                                      }
                                                                                                                                  ]
                                                                                                                              },
                                                                                                                          stmAux =
                                                                                                                            StmAux
                                                                                                                              { stmAuxCerts =
                                                                                                                                  Certs
                                                                                                                                    { unCerts = []
                                                                                                                                    },
                                                                                                                                stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                stmAuxDec = ()
                                                                                                                              },
                                                                                                                          stmExp =
                                                                                                                            BasicOp
                                                                                                                              ( Replicate
                                                                                                                                  ( Shape
                                                                                                                                      { shapeDims =
                                                                                                                                          [ Constant
                                                                                                                                              ( IntValue
                                                                                                                                                  (Int64Value 1)
                                                                                                                                              )
                                                                                                                                          ]
                                                                                                                                      }
                                                                                                                                  )
                                                                                                                                  (Var (VName "++_rh" 6539))
                                                                                                                              )
                                                                                                                        },
                                                                                                                      Let
                                                                                                                        { stmPat =
                                                                                                                            Pat
                                                                                                                              { patElems =
                                                                                                                                  [ PatElem
                                                                                                                                      { patElemName = VName "conc_tmp" 6541,
                                                                                                                                        patElemDec = Prim (IntType Int64)
                                                                                                                                      }
                                                                                                                                  ]
                                                                                                                              },
                                                                                                                          stmAux =
                                                                                                                            StmAux
                                                                                                                              { stmAuxCerts =
                                                                                                                                  Certs
                                                                                                                                    { unCerts = []
                                                                                                                                    },
                                                                                                                                stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                stmAuxDec = ()
                                                                                                                              },
                                                                                                                          stmExp =
                                                                                                                            BasicOp
                                                                                                                              ( BinOp
                                                                                                                                  (Add Int64 OverflowUndef)
                                                                                                                                  ( Constant
                                                                                                                                      ( IntValue
                                                                                                                                          (Int64Value 1)
                                                                                                                                      )
                                                                                                                                  )
                                                                                                                                  (Var (VName "loop_d₂₀" 6533))
                                                                                                                              )
                                                                                                                        },
                                                                                                                      Let
                                                                                                                        { stmPat =
                                                                                                                            Pat
                                                                                                                              { patElems =
                                                                                                                                  [ PatElem
                                                                                                                                      { patElemName = VName "++_res" 6542,
                                                                                                                                        patElemDec =
                                                                                                                                          Array
                                                                                                                                            (IntType Int64)
                                                                                                                                            ( Shape
                                                                                                                                                { shapeDims =
                                                                                                                                                    [ Var (VName "conc_tmp" 6541)
                                                                                                                                                    ]
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                            NoUniqueness
                                                                                                                                      }
                                                                                                                                  ]
                                                                                                                              },
                                                                                                                          stmAux =
                                                                                                                            StmAux
                                                                                                                              { stmAuxCerts =
                                                                                                                                  Certs
                                                                                                                                    { unCerts = []
                                                                                                                                    },
                                                                                                                                stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                                                stmAuxDec = ()
                                                                                                                              },
                                                                                                                          stmExp =
                                                                                                                            BasicOp
                                                                                                                              ( Concat
                                                                                                                                  0
                                                                                                                                  ( VName "r" 6534
                                                                                                                                      :| [ VName "++_arg" 6540
                                                                                                                                         ]
                                                                                                                                  )
                                                                                                                                  (Var (VName "conc_tmp" 6541))
                                                                                                                              )
                                                                                                                        }
                                                                                                                    ],
                                                                                                                bodyResult =
                                                                                                                  [ SubExpRes
                                                                                                                      { resCerts =
                                                                                                                          Certs
                                                                                                                            { unCerts = []
                                                                                                                            },
                                                                                                                        resSubExp = Var (VName "conc_tmp" 6541)
                                                                                                                      },
                                                                                                                    SubExpRes
                                                                                                                      { resCerts =
                                                                                                                          Certs
                                                                                                                            { unCerts = []
                                                                                                                            },
                                                                                                                        resSubExp = Var (VName "++_res" 6542)
                                                                                                                      }
                                                                                                                  ]
                                                                                                              }
                                                                                                          )
                                                                                                    },
                                                                                                  Let
                                                                                                    { stmPat =
                                                                                                        Pat
                                                                                                          { patElems =
                                                                                                              [ PatElem
                                                                                                                  { patElemName = VName "result_proper_shape" 6543,
                                                                                                                    patElemDec =
                                                                                                                      Array
                                                                                                                        (IntType Int64)
                                                                                                                        ( Shape
                                                                                                                            { shapeDims =
                                                                                                                                [Var (VName "o" 5719)]
                                                                                                                            }
                                                                                                                        )
                                                                                                                        NoUniqueness
                                                                                                                  }
                                                                                                              ]
                                                                                                          },
                                                                                                      stmAux =
                                                                                                        StmAux
                                                                                                          { stmAuxCerts =
                                                                                                              Certs
                                                                                                                { unCerts = []
                                                                                                                },
                                                                                                            stmAuxAttrs = Attrs {unAttrs = mempty},
                                                                                                            stmAuxDec = ()
                                                                                                          },
                                                                                                      stmExp =
                                                                                                        BasicOp
                                                                                                          ( Reshape
                                                                                                              ReshapeCoerce
                                                                                                              ( Shape
                                                                                                                  { shapeDims =
                                                                                                                      [Var (VName "o" 5719)]
                                                                                                                  }
                                                                                                              )
                                                                                                              ( VName "wew" 6531
                                                                                                              )
                                                                                                          )
                                                                                                    }
                                                                                                ],
                                                                                            kernelBodyResult =
                                                                                              [ Returns
                                                                                                  ResultMaySimplify
                                                                                                  ( Certs
                                                                                                      { unCerts = []
                                                                                                      }
                                                                                                  )
                                                                                                  ( Var (VName "result_proper_shape" 6543)
                                                                                                  )
                                                                                              ]
                                                                                          }
                                                                                      )
                                                                                  )
                                                                              )
                                                                        }
                                                                    ],
                                                                bodyResult =
                                                                  [ SubExpRes
                                                                      { resCerts =
                                                                          Certs
                                                                            { unCerts = []
                                                                            },
                                                                        resSubExp =
                                                                          Var
                                                                            (VName "lifted_lambda_res" 6523)
                                                                      }
                                                                  ]
                                                              }
                                                          )
                                                    }
                                                ],
                                            bodyResult =
                                              [ SubExpRes
                                                  { resCerts =
                                                      Certs
                                                        { unCerts = []
                                                        },
                                                    resSubExp =
                                                      Var
                                                        (VName "defunc_0_map_res" 6492)
                                                  }
                                              ]
                                          }
                                      )
                                      ( MatchDec
                                          { matchReturns =
                                              [ Array
                                                  (IntType Int64)
                                                  ( Shape
                                                      { shapeDims =
                                                          [ Free (Var (VName "l" 5716)),
                                                            Free (Var (VName "n" 5718)),
                                                            Free (Var (VName "o" 5719))
                                                          ]
                                                      }
                                                  )
                                                  NoUniqueness
                                              ],
                                            matchSort = MatchEquiv
                                          }
                                      )
                                }
                            ],
                        bodyResult =
                          [ SubExpRes
                              { resCerts = Certs {unCerts = []},
                                resSubExp =
                                  Var
                                    (VName "defunc_0_map_res" 6243)
                              }
                          ]
                      }
                }
            ]
        }
