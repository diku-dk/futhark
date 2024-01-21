module Futhark.Pass.OptimiseArrayLayout.AnalysePrimExpTests (tests) where

import Control.Monad.State.Strict
import Data.Map.Strict qualified as M
import Data.Sequence.Internal qualified as S
import Futhark.Analysis.AnalysePrimExp
import Futhark.Analysis.PrimExp
import Futhark.IR.GPU
import Futhark.IR.GPUTests ()
import Futhark.IR.MC
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "AnalyzePrim" [stmToPrimExpsTests]

stmToPrimExpsTests :: TestTree
stmToPrimExpsTests =
  testGroup
    "stmToPrimExps"
    [stmToPrimExpsTestsGPU, stmToPrimExpsTestsMC]

stmToPrimExpsTestsGPU :: TestTree
stmToPrimExpsTestsGPU =
  testGroup
    "GPU"
    $ do
      let scope =
            M.fromList
              [ ("n_5142", FParamName "i64"),
                ("m_5143", FParamName "i64"),
                ("xss_5144", FParamName "[n_5142][m_5143]i64"),
                ("segmap_group_size_5201", LetName "i64"),
                ("segmap_usable_groups_5202", LetName "i64"),
                ("defunc_0_map_res_5203", LetName "[n_5142]i64"),
                ("defunc_0_f_res_5207", LetName "i64"),
                ("i_5208", IndexName Int64),
                ("acc_5209", FParamName "i64"),
                ("b_5210", LetName "i64"),
                ("defunc_0_f_res_5211", LetName "i64")
              ]
      let emptyStmAux = defAux ()
      [ testCase "BinOp" $ do
          let stm =
                Let
                  { stmPat =
                      Pat
                        { patElems =
                            [ PatElem
                                { patElemName = "defunc_0_f_res_5211",
                                  patElemDec = Prim (IntType Int64)
                                }
                            ]
                        },
                    stmAux = emptyStmAux,
                    stmExp =
                      BasicOp
                        ( BinOp
                            (Add Int64 OverflowWrap)
                            (Var "acc_5209")
                            (Var "b_5210")
                        )
                  }
          let res = execState ((stmToPrimExps @GPU) scope stm) mempty
          let expected =
                M.fromList
                  [ ( "defunc_0_f_res_5211",
                      Just
                        ( BinOpExp
                            (Add Int64 OverflowWrap)
                            (LeafExp "acc_5209" (IntType Int64))
                            (LeafExp "b_5210" (IntType Int64))
                        )
                    )
                  ]
          res @?= expected,
        testCase "Index" $ do
          let stm =
                Let
                  { stmPat =
                      Pat
                        { patElems =
                            [ PatElem
                                { patElemName = "b_5210",
                                  patElemDec = Prim (IntType Int64)
                                }
                            ]
                        },
                    stmAux = emptyStmAux,
                    stmExp =
                      BasicOp
                        ( Index
                            "xss_5144"
                            ( Slice
                                { unSlice =
                                    [ DimFix (Var "gtid_5204"),
                                      DimFix (Var "i_5208")
                                    ]
                                }
                            )
                        )
                  }
          let res = execState ((stmToPrimExps @GPU) scope stm) mempty
          let expected = M.fromList [("b_5210", Nothing)]
          res @?= expected,
        testCase "Loop" $ do
          let stm =
                Let
                  { stmPat =
                      Pat
                        { patElems =
                            [ PatElem
                                { patElemName = "defunc_0_f_res_5207",
                                  patElemDec = Prim (IntType Int64)
                                }
                            ]
                        },
                    stmAux = emptyStmAux,
                    stmExp =
                      Loop
                        [ ( Param
                              { paramAttrs = Attrs {unAttrs = mempty},
                                paramName = "acc_5209",
                                paramDec = Prim (IntType Int64)
                              },
                            Constant (IntValue (Int64Value 0))
                          )
                        ]
                        ( ForLoop
                            "i_5208"
                            Int64
                            (Var "m_5143")
                        )
                        ( Body
                            { bodyDec = (),
                              bodyStms = mempty,
                              bodyResult =
                                [ SubExpRes
                                    { resCerts = Certs {unCerts = []},
                                      resSubExp = Var "defunc_0_f_res_5211"
                                    }
                                ]
                            }
                        )
                  }
          let res = execState ((stmToPrimExps @GPU) scope stm) mempty
          let expected =
                M.fromList
                  [ ("defunc_0_f_res_5207", Nothing),
                    ("i_5208", Just (LeafExp "m_5143" (IntType Int64))),
                    ("acc_5209", Just (LeafExp "acc_5209" (IntType Int64)))
                  ]
          res @?= expected,
        testCase "Loop body" $ do
          let stm =
                Let
                  { stmPat =
                      Pat
                        { patElems =
                            [ PatElem
                                { patElemName = "defunc_0_f_res_5207",
                                  patElemDec = Prim (IntType Int64)
                                }
                            ]
                        },
                    stmAux = emptyStmAux,
                    stmExp =
                      Loop
                        [ ( Param
                              { paramAttrs = Attrs {unAttrs = mempty},
                                paramName = "acc_5209",
                                paramDec = Prim (IntType Int64)
                              },
                            Constant (IntValue (Int64Value 0))
                          )
                        ]
                        ( ForLoop
                            "i_5208"
                            Int64
                            (Var "m_5143")
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
                                                    { patElemName = "b_5210",
                                                      patElemDec = Prim (IntType Int64)
                                                    }
                                                ]
                                            },
                                        stmAux = emptyStmAux,
                                        stmExp =
                                          BasicOp
                                            ( Index
                                                "xss_5144"
                                                ( Slice
                                                    { unSlice =
                                                        [ DimFix (Var "gtid_5204"),
                                                          DimFix (Var "i_5208")
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
                                                    { patElemName = "defunc_0_f_res_5211",
                                                      patElemDec = Prim (IntType Int64)
                                                    }
                                                ]
                                            },
                                        stmAux = emptyStmAux,
                                        stmExp =
                                          BasicOp
                                            ( BinOp
                                                (Add Int64 OverflowWrap)
                                                (Var "acc_5209")
                                                (Var "b_5210")
                                            )
                                      }
                                  ],
                              bodyResult =
                                [ SubExpRes
                                    { resCerts = Certs {unCerts = []},
                                      resSubExp = Var "defunc_0_f_res_5211"
                                    }
                                ]
                            }
                        )
                  }
          let res = execState ((stmToPrimExps @GPU) scope stm) mempty
          let expected =
                M.fromList
                  [ ("defunc_0_f_res_5207", Nothing),
                    ("i_5208", Just (LeafExp "m_5143" (IntType Int64))),
                    ("acc_5209", Just (LeafExp "acc_5209" (IntType Int64))),
                    ("b_5210", Nothing),
                    ( "defunc_0_f_res_5211",
                      Just
                        ( BinOpExp
                            (Add Int64 OverflowWrap)
                            (LeafExp "acc_5209" (IntType Int64))
                            (LeafExp "b_5210" (IntType Int64))
                        )
                    )
                  ]
          res @?= expected,
        testCase "SegMap" $
          do
            let stm =
                  Let
                    { stmPat =
                        Pat
                          { patElems =
                              [ PatElem
                                  { patElemName = "defunc_0_map_res_5125",
                                    patElemDec =
                                      Array
                                        (IntType Int64)
                                        (Shape {shapeDims = [Var "n_5142"]})
                                        NoUniqueness
                                  }
                              ]
                          },
                      stmAux = emptyStmAux,
                      stmExp =
                        Op
                          ( SegOp
                              ( SegMap
                                  ( SegThread
                                      SegNoVirt
                                      ( Just
                                          ( KernelGrid
                                              { gridNumBlocks = Count {unCount = Var "segmap_usable_groups_5124"},
                                                gridBlockSize = Count {unCount = Var "segmap_group_size_5123"}
                                              }
                                          )
                                      )
                                  )
                                  ( SegSpace
                                      { segFlat = "phys_tid_5127",
                                        unSegSpace =
                                          [ ( "gtid_5126",
                                              Var "n_5142"
                                            )
                                          ]
                                      }
                                  )
                                  [Prim (IntType Int64)]
                                  ( KernelBody
                                      { kernelBodyDec = (),
                                        kernelBodyStms = mempty,
                                        kernelBodyResult =
                                          [ Returns
                                              ResultMaySimplify
                                              (Certs {unCerts = []})
                                              (Var "lifted_lambda_res_5129")
                                          ]
                                      }
                                  )
                              )
                          )
                    }
            let res = execState ((stmToPrimExps @GPU) scope stm) mempty
            let expected =
                  M.fromList
                    [ ("defunc_0_map_res_5125", Nothing),
                      ("gtid_5126", Just (LeafExp "n_5142" (IntType Int64)))
                    ]
            res @?= expected,
        testCase "SegMap body" $
          do
            let stm =
                  Let
                    { stmPat =
                        Pat
                          { patElems =
                              [ PatElem
                                  { patElemName = "defunc_0_map_res_5125",
                                    patElemDec =
                                      Array
                                        (IntType Int64)
                                        (Shape {shapeDims = [Var "n_5142"]})
                                        NoUniqueness
                                  }
                              ]
                          },
                      stmAux = emptyStmAux,
                      stmExp =
                        Op
                          ( SegOp
                              ( SegMap
                                  ( SegThread
                                      SegNoVirt
                                      ( Just
                                          ( KernelGrid
                                              { gridNumBlocks = Count {unCount = Var "segmap_usable_groups_5124"},
                                                gridBlockSize = Count {unCount = Var "segmap_group_size_5123"}
                                              }
                                          )
                                      )
                                  )
                                  ( SegSpace
                                      { segFlat = "phys_tid_5127",
                                        unSegSpace =
                                          [ ( "gtid_5126",
                                              Var "n_5142"
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
                                                              { patElemName = "eta_p_5128",
                                                                patElemDec = Prim (IntType Int64)
                                                              }
                                                          ]
                                                      },
                                                  stmAux = emptyStmAux,
                                                  stmExp =
                                                    BasicOp
                                                      ( Index
                                                          "xs_5093"
                                                          ( Slice
                                                              { unSlice = [DimFix (Var "gtid_5126")]
                                                              }
                                                          )
                                                      )
                                                },
                                              Let
                                                { stmPat =
                                                    Pat
                                                      { patElems =
                                                          [ PatElem
                                                              { patElemName = "lifted_lambda_res_5129",
                                                                patElemDec = Prim (IntType Int64)
                                                              }
                                                          ]
                                                      },
                                                  stmAux = emptyStmAux,
                                                  stmExp =
                                                    BasicOp
                                                      ( BinOp
                                                          (Add Int64 OverflowWrap)
                                                          (Constant (IntValue (Int64Value 2)))
                                                          (Var "eta_p_5128")
                                                      )
                                                }
                                            ],
                                        kernelBodyResult =
                                          [ Returns
                                              ResultMaySimplify
                                              (Certs {unCerts = []})
                                              (Var "lifted_lambda_res_5129")
                                          ]
                                      }
                                  )
                              )
                          )
                    }
            let res = execState ((stmToPrimExps @GPU) scope stm) mempty
            let expected =
                  M.fromList
                    [ ("defunc_0_map_res_5125", Nothing),
                      ("gtid_5126", Just (LeafExp "n_5142" (IntType Int64))),
                      ("eta_p_5128", Nothing),
                      ( "lifted_lambda_res_5129",
                        Just
                          ( BinOpExp
                              (Add Int64 OverflowWrap)
                              (ValueExp (IntValue (Int64Value 2)))
                              (LeafExp "eta_p_5128" (IntType Int64))
                          )
                      )
                    ]
            res @?= expected
        ]

stmToPrimExpsTestsMC :: TestTree
stmToPrimExpsTestsMC =
  testGroup
    "MC"
    $ do
      let scope =
            M.fromList
              [ ("n_5142", FParamName (Prim (IntType Int64))),
                ("m_5143", FParamName (Prim (IntType Int64))),
                ( "xss_5144",
                  FParamName
                    ( Array
                        (IntType Int64)
                        ( Shape
                            { shapeDims =
                                [ Var "n_5142",
                                  Var "m_5143"
                                ]
                            }
                        )
                        Nonunique
                    )
                ),
                ("segmap_group_size_5201", LetName (Prim (IntType Int64))),
                ("segmap_usable_groups_5202", LetName (Prim (IntType Int64))),
                ( "defunc_0_map_res_5203",
                  LetName
                    ( Array
                        (IntType Int64)
                        (Shape {shapeDims = [Var "n_5142"]})
                        NoUniqueness
                    )
                ),
                ("defunc_0_f_res_5207", LetName (Prim (IntType Int64))),
                ("i_5208", IndexName Int64),
                ("acc_5209", FParamName (Prim (IntType Int64))),
                ("b_5210", LetName (Prim (IntType Int64))),
                ("defunc_0_f_res_5211", LetName (Prim (IntType Int64)))
              ]
      let emptyStmAux =
            StmAux
              { stmAuxCerts = Certs {unCerts = mempty},
                stmAuxAttrs = Attrs {unAttrs = mempty},
                stmAuxDec = ()
              }
      [ testCase "BinOp" $ do
          let stm =
                Let
                  { stmPat =
                      Pat
                        { patElems =
                            [ PatElem
                                { patElemName = "defunc_0_f_res_5211",
                                  patElemDec = Prim (IntType Int64)
                                }
                            ]
                        },
                    stmAux = emptyStmAux,
                    stmExp =
                      BasicOp
                        ( BinOp
                            (Add Int64 OverflowWrap)
                            (Var "acc_5209")
                            (Var "b_5210")
                        )
                  }
          let res = execState ((stmToPrimExps @MC) scope stm) mempty
          let expected =
                M.fromList
                  [ ( "defunc_0_f_res_5211",
                      Just
                        ( BinOpExp
                            (Add Int64 OverflowWrap)
                            (LeafExp "acc_5209" (IntType Int64))
                            (LeafExp "b_5210" (IntType Int64))
                        )
                    )
                  ]
          res @?= expected,
        testCase "Index" $ do
          let stm =
                Let
                  { stmPat =
                      Pat
                        { patElems =
                            [ PatElem
                                { patElemName = "b_5210",
                                  patElemDec = Prim (IntType Int64)
                                }
                            ]
                        },
                    stmAux = emptyStmAux,
                    stmExp =
                      BasicOp
                        ( Index
                            "xss_5144"
                            ( Slice
                                { unSlice =
                                    [ DimFix (Var "gtid_5204"),
                                      DimFix (Var "i_5208")
                                    ]
                                }
                            )
                        )
                  }
          let res = execState ((stmToPrimExps @MC) scope stm) mempty
          let expected = M.fromList [("b_5210", Nothing)]
          res @?= expected,
        testCase "Loop" $ do
          let stm =
                Let
                  { stmPat =
                      Pat
                        { patElems =
                            [ PatElem
                                { patElemName = "defunc_0_f_res_5207",
                                  patElemDec = Prim (IntType Int64)
                                }
                            ]
                        },
                    stmAux = emptyStmAux,
                    stmExp =
                      Loop
                        [ ( Param
                              { paramAttrs = Attrs {unAttrs = mempty},
                                paramName = "acc_5209",
                                paramDec = Prim (IntType Int64)
                              },
                            Constant (IntValue (Int64Value 0))
                          )
                        ]
                        ( ForLoop
                            "i_5208"
                            Int64
                            (Var "m_5143")
                        )
                        ( Body
                            { bodyDec = (),
                              bodyStms = mempty,
                              bodyResult =
                                [ SubExpRes
                                    { resCerts = Certs {unCerts = []},
                                      resSubExp = Var "defunc_0_f_res_5211"
                                    }
                                ]
                            }
                        )
                  }
          let res = execState ((stmToPrimExps @MC) scope stm) mempty
          let expected =
                M.fromList
                  [ ("defunc_0_f_res_5207", Nothing),
                    ("i_5208", Just (LeafExp "m_5143" (IntType Int64))),
                    ("acc_5209", Just (LeafExp "acc_5209" (IntType Int64)))
                  ]
          res @?= expected,
        testCase "Loop body" $ do
          let stm =
                Let
                  { stmPat =
                      Pat
                        { patElems =
                            [ PatElem
                                { patElemName = "defunc_0_f_res_5207",
                                  patElemDec = Prim (IntType Int64)
                                }
                            ]
                        },
                    stmAux = emptyStmAux,
                    stmExp =
                      Loop
                        [ ( Param
                              { paramAttrs = Attrs {unAttrs = mempty},
                                paramName = "acc_5209",
                                paramDec = Prim (IntType Int64)
                              },
                            Constant (IntValue (Int64Value 0))
                          )
                        ]
                        ( ForLoop
                            "i_5208"
                            Int64
                            (Var "m_5143")
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
                                                    { patElemName = "b_5210",
                                                      patElemDec = Prim (IntType Int64)
                                                    }
                                                ]
                                            },
                                        stmAux = emptyStmAux,
                                        stmExp =
                                          BasicOp
                                            ( Index
                                                "xss_5144"
                                                ( Slice
                                                    { unSlice =
                                                        [ DimFix (Var "gtid_5204"),
                                                          DimFix (Var "i_5208")
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
                                                    { patElemName = "defunc_0_f_res_5211",
                                                      patElemDec = Prim (IntType Int64)
                                                    }
                                                ]
                                            },
                                        stmAux = emptyStmAux,
                                        stmExp =
                                          BasicOp
                                            ( BinOp
                                                (Add Int64 OverflowWrap)
                                                (Var "acc_5209")
                                                (Var "b_5210")
                                            )
                                      }
                                  ],
                              bodyResult =
                                [ SubExpRes
                                    { resCerts = Certs {unCerts = []},
                                      resSubExp = Var "defunc_0_f_res_5211"
                                    }
                                ]
                            }
                        )
                  }
          let res = execState ((stmToPrimExps @MC) scope stm) mempty
          let expected =
                M.fromList
                  [ ("defunc_0_f_res_5207", Nothing),
                    ("i_5208", Just (LeafExp "m_5143" (IntType Int64))),
                    ("acc_5209", Just (LeafExp "acc_5209" (IntType Int64))),
                    ("b_5210", Nothing),
                    ( "defunc_0_f_res_5211",
                      Just
                        ( BinOpExp
                            (Add Int64 OverflowWrap)
                            (LeafExp "acc_5209" (IntType Int64))
                            (LeafExp "b_5210" (IntType Int64))
                        )
                    )
                  ]
          res @?= expected,
        testCase "SegMap" $ do
          let stm =
                Let
                  { stmPat =
                      Pat
                        { patElems =
                            [ PatElem
                                { patElemName = "defunc_0_map_res_5125",
                                  patElemDec =
                                    Array
                                      (IntType Int64)
                                      (Shape {shapeDims = [Var "n_5142"]})
                                      NoUniqueness
                                }
                            ]
                        },
                    stmAux = emptyStmAux,
                    stmExp =
                      Op
                        ( ParOp
                            Nothing
                            ( SegMap
                                ()
                                ( SegSpace
                                    { segFlat = "flat_tid_5112",
                                      unSegSpace =
                                        [ ( "gtid_5126",
                                            Var "n_5142"
                                          )
                                        ]
                                    }
                                )
                                [Prim (IntType Int64)]
                                ( KernelBody
                                    { kernelBodyDec = (),
                                      kernelBodyStms = mempty,
                                      kernelBodyResult =
                                        [ Returns
                                            ResultMaySimplify
                                            (Certs {unCerts = []})
                                            (Var "lifted_lambda_res_5129")
                                        ]
                                    }
                                )
                            )
                        )
                  }
          let res = execState ((stmToPrimExps @MC) scope stm) mempty
          let expected =
                M.fromList
                  [ ("defunc_0_map_res_5125", Nothing),
                    ("gtid_5126", Just (LeafExp "n_5142" (IntType Int64)))
                  ]
          res @?= expected,
        testCase "SegMap body" $ do
          let stm =
                Let
                  { stmPat =
                      Pat
                        { patElems =
                            [ PatElem
                                { patElemName = "defunc_0_map_res_5125",
                                  patElemDec =
                                    Array
                                      (IntType Int64)
                                      (Shape {shapeDims = [Var "n_5142"]})
                                      NoUniqueness
                                }
                            ]
                        },
                    stmAux = emptyStmAux,
                    stmExp =
                      Op
                        ( ParOp
                            Nothing
                            ( SegMap
                                ()
                                ( SegSpace
                                    { segFlat = "flat_tid_5112",
                                      unSegSpace =
                                        [ ( "gtid_5126",
                                            Var "n_5142"
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
                                                            { patElemName = "eta_p_5128",
                                                              patElemDec = Prim (IntType Int64)
                                                            }
                                                        ]
                                                    },
                                                stmAux = emptyStmAux,
                                                stmExp =
                                                  BasicOp
                                                    ( Index
                                                        "xs_5093"
                                                        (Slice {unSlice = [DimFix (Var "gtid_5126")]})
                                                    )
                                              },
                                            Let
                                              { stmPat =
                                                  Pat
                                                    { patElems =
                                                        [ PatElem
                                                            { patElemName = "lifted_lambda_res_5129",
                                                              patElemDec = Prim (IntType Int64)
                                                            }
                                                        ]
                                                    },
                                                stmAux = emptyStmAux,
                                                stmExp =
                                                  BasicOp
                                                    ( BinOp
                                                        (Add Int64 OverflowWrap)
                                                        (Constant (IntValue (Int64Value 2)))
                                                        (Var "eta_p_5128")
                                                    )
                                              }
                                          ],
                                      kernelBodyResult =
                                        [ Returns
                                            ResultMaySimplify
                                            (Certs {unCerts = []})
                                            (Var "lifted_lambda_res_5129")
                                        ]
                                    }
                                )
                            )
                        )
                  }
          let res = execState ((stmToPrimExps @MC) scope stm) mempty
          let expected =
                M.fromList
                  [ ("defunc_0_map_res_5125", Nothing),
                    ("gtid_5126", Just (LeafExp "n_5142" (IntType Int64))),
                    ("eta_p_5128", Nothing),
                    ( "lifted_lambda_res_5129",
                      Just
                        ( BinOpExp
                            (Add Int64 OverflowWrap)
                            (ValueExp (IntValue (Int64Value 2)))
                            (LeafExp "eta_p_5128" (IntType Int64))
                        )
                    )
                  ]
          res @?= expected
        ]
