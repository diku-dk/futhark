module Futhark.Pass.OptimiseArrayLayout.AnalyseTests (tests) where

import Data.Map.Strict qualified as M
import Data.Sequence.Internal qualified as S
import Futhark.Analysis.AccessPattern
import Futhark.IR.GPU
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Analyse" [analyseStmTests]

analyseStmTests :: TestTree
analyseStmTests =
  testGroup
    "analyseStm"
    [analyseIndexTests, analyseDimAccesssTests]

analyseIndexTests :: TestTree
analyseIndexTests =
  testGroup
    "analyseIndex"
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
            let (_, indexTable') = analyseIndex ctx patternNames arr_name dimFixes
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

            let (_, indexTable') = analyseIndex ctx patternNames arr_name dimFixes
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
            let (_, indexTable') = analyseIndex ctx patternNames arr_name dimFixes
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
            let (_, indexTable') = analyseIndex ctx patternNames arr_name dimFixes
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
            let (_, indexTable') = analyseIndex ctx patternNames arr_name dimFixes
            indexTable' @?= indexTable

      [testCase0, testCase1, testCase2, testCase3, testCase4]

analyseDimAccesssTests :: TestTree
analyseDimAccesssTests = testGroup
  "analyseDimAccesss"
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
          let indexTable' = (analyseDimAccesss @GPU) prog0
          indexTable' @?= indexTable

    [testCase0]
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
                                              SizeThreadBlock
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
                                                          { gridNumBlocks =
                                                              Count {unCount = Var (VName "segmap_usable_groups" 5203)},
                                                            gridBlockSize =
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
