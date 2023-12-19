module Futhark.Pass.GALOP.AnalysePrimExpTests (tests) where

import Control.Monad.State.Strict
import Data.Map.Strict qualified as M
import Data.Sequence.Internal qualified as S
import Futhark.Analysis.AnalysePrimExp
import Futhark.Analysis.PrimExp
import Futhark.IR.GPU
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
              [ (VName "n" 5142, FParamName (Prim (IntType Int64))),
                (VName "m" 5143, FParamName (Prim (IntType Int64))),
                ( VName "xss" 5144,
                  FParamName
                    ( Array
                        (IntType Int64)
                        ( Shape
                            { shapeDims =
                                [ Var (VName "n" 5142),
                                  Var (VName "m" 5143)
                                ]
                            }
                        )
                        Nonunique
                    )
                ),
                (VName "segmap_group_size" 5201, LetName (Prim (IntType Int64))),
                (VName "segmap_usable_groups" 5202, LetName (Prim (IntType Int64))),
                ( VName "defunc_0_map_res" 5203,
                  LetName
                    ( Array
                        (IntType Int64)
                        (Shape {shapeDims = [Var (VName "n" 5142)]})
                        NoUniqueness
                    )
                ),
                (VName "defunc_0_f_res" 5207, LetName (Prim (IntType Int64))),
                (VName "i" 5208, IndexName Int64),
                (VName "acc" 5209, FParamName (Prim (IntType Int64))),
                (VName "b" 5210, LetName (Prim (IntType Int64))),
                (VName "defunc_0_f_res" 5211, LetName (Prim (IntType Int64)))
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
                                { patElemName = VName "defunc_0_f_res" 5211,
                                  patElemDec = Prim (IntType Int64)
                                }
                            ]
                        },
                    stmAux = emptyStmAux,
                    stmExp =
                      BasicOp
                        ( BinOp
                            (Add Int64 OverflowWrap)
                            (Var (VName "acc" 5209))
                            (Var (VName "b" 5210))
                        )
                  }
          let res = execState ((stmToPrimExps @GPU) scope stm) mempty
          let expected =
                M.fromList
                  [ ( VName "defunc_0_f_res" 5211,
                      Just
                        ( BinOpExp
                            (Add Int64 OverflowWrap)
                            (LeafExp (VName "acc" 5209) (IntType Int64))
                            (LeafExp (VName "b" 5210) (IntType Int64))
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
                                { patElemName = VName "b" 5210,
                                  patElemDec = Prim (IntType Int64)
                                }
                            ]
                        },
                    stmAux = emptyStmAux,
                    stmExp =
                      BasicOp
                        ( Index
                            (VName "xss" 5144)
                            ( Slice
                                { unSlice =
                                    [ DimFix (Var (VName "gtid" 5204)),
                                      DimFix (Var (VName "i" 5208))
                                    ]
                                }
                            )
                        )
                  }
          let res = execState ((stmToPrimExps @GPU) scope stm) mempty
          let expected = M.fromList [(VName "b" 5210, Nothing)]
          res @?= expected,
        testCase "Loop" $ do
          let stm =
                Let
                  { stmPat =
                      Pat
                        { patElems =
                            [ PatElem
                                { patElemName = VName "defunc_0_f_res" 5207,
                                  patElemDec = Prim (IntType Int64)
                                }
                            ]
                        },
                    stmAux = emptyStmAux,
                    stmExp =
                      Loop
                        [ ( Param
                              { paramAttrs = Attrs {unAttrs = mempty},
                                paramName = VName "acc" 5209,
                                paramDec = Prim (IntType Int64)
                              },
                            Constant (IntValue (Int64Value 0))
                          )
                        ]
                        ( ForLoop
                            (VName "i" 5208)
                            Int64
                            (Var (VName "m" 5143))
                        )
                        ( Body
                            { bodyDec = (),
                              bodyStms = mempty,
                              bodyResult =
                                [ SubExpRes
                                    { resCerts = Certs {unCerts = []},
                                      resSubExp = Var (VName "defunc_0_f_res" 5211)
                                    }
                                ]
                            }
                        )
                  }
          let res = execState ((stmToPrimExps @GPU) scope stm) mempty
          let expected =
                M.fromList
                  [ (VName "defunc_0_f_res" 5207, Nothing),
                    (VName "i" 5208, Just (LeafExp (VName "m" 5143) (IntType Int64))),
                    (VName "acc" 5209, Just (LeafExp (VName "acc" 5209) (IntType Int64)))
                  ]
          res @?= expected,
        testCase "Loop body" $ do
          let stm =
                Let
                  { stmPat =
                      Pat
                        { patElems =
                            [ PatElem
                                { patElemName = VName "defunc_0_f_res" 5207,
                                  patElemDec = Prim (IntType Int64)
                                }
                            ]
                        },
                    stmAux = emptyStmAux,
                    stmExp =
                      Loop
                        [ ( Param
                              { paramAttrs = Attrs {unAttrs = mempty},
                                paramName = VName "acc" 5209,
                                paramDec = Prim (IntType Int64)
                              },
                            Constant (IntValue (Int64Value 0))
                          )
                        ]
                        ( ForLoop
                            (VName "i" 5208)
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
                                                    { patElemName = VName "b" 5210,
                                                      patElemDec = Prim (IntType Int64)
                                                    }
                                                ]
                                            },
                                        stmAux = emptyStmAux,
                                        stmExp =
                                          BasicOp
                                            ( Index
                                                (VName "xss" 5144)
                                                ( Slice
                                                    { unSlice =
                                                        [ DimFix (Var (VName "gtid" 5204)),
                                                          DimFix (Var (VName "i" 5208))
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
                                                    { patElemName = VName "defunc_0_f_res" 5211,
                                                      patElemDec = Prim (IntType Int64)
                                                    }
                                                ]
                                            },
                                        stmAux = emptyStmAux,
                                        stmExp =
                                          BasicOp
                                            ( BinOp
                                                (Add Int64 OverflowWrap)
                                                (Var (VName "acc" 5209))
                                                (Var (VName "b" 5210))
                                            )
                                      }
                                  ],
                              bodyResult =
                                [ SubExpRes
                                    { resCerts = Certs {unCerts = []},
                                      resSubExp = Var (VName "defunc_0_f_res" 5211)
                                    }
                                ]
                            }
                        )
                  }
          let res = execState ((stmToPrimExps @GPU) scope stm) mempty
          let expected =
                M.fromList
                  [ (VName "defunc_0_f_res" 5207, Nothing),
                    (VName "i" 5208, Just (LeafExp (VName "m" 5143) (IntType Int64))),
                    (VName "acc" 5209, Just (LeafExp (VName "acc" 5209) (IntType Int64))),
                    (VName "b" 5210, Nothing),
                    ( VName "defunc_0_f_res" 5211,
                      Just
                        ( BinOpExp
                            (Add Int64 OverflowWrap)
                            (LeafExp (VName "acc" 5209) (IntType Int64))
                            (LeafExp (VName "b" 5210) (IntType Int64))
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
                                  { patElemName = VName "defunc_0_map_res" 5125,
                                    patElemDec =
                                      Array
                                        (IntType Int64)
                                        (Shape {shapeDims = [Var (VName "n" 5142)]})
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
                                              { gridNumGroups = Count {unCount = Var (VName "segmap_usable_groups" 5124)},
                                                gridGroupSize = Count {unCount = Var (VName "segmap_group_size" 5123)}
                                              }
                                          )
                                      )
                                  )
                                  ( SegSpace
                                      { segFlat = VName "phys_tid" 5127,
                                        unSegSpace =
                                          [ ( VName "gtid" 5126,
                                              Var (VName "n" 5142)
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
                                              (Var (VName "lifted_lambda_res" 5129))
                                          ]
                                      }
                                  )
                              )
                          )
                    }
            let res = execState ((stmToPrimExps @GPU) scope stm) mempty
            let expected =
                  M.fromList
                    [ (VName "defunc_0_map_res" 5125, Nothing),
                      (VName "gtid" 5126, Just (LeafExp (VName "n" 5142) (IntType Int64)))
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
                                  { patElemName = VName "defunc_0_map_res" 5125,
                                    patElemDec =
                                      Array
                                        (IntType Int64)
                                        (Shape {shapeDims = [Var (VName "n" 5142)]})
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
                                              { gridNumGroups = Count {unCount = Var (VName "segmap_usable_groups" 5124)},
                                                gridGroupSize = Count {unCount = Var (VName "segmap_group_size" 5123)}
                                              }
                                          )
                                      )
                                  )
                                  ( SegSpace
                                      { segFlat = VName "phys_tid" 5127,
                                        unSegSpace =
                                          [ ( VName "gtid" 5126,
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
                                                              { patElemName = VName "eta_p" 5128,
                                                                patElemDec = Prim (IntType Int64)
                                                              }
                                                          ]
                                                      },
                                                  stmAux = emptyStmAux,
                                                  stmExp =
                                                    BasicOp
                                                      ( Index
                                                          (VName "xs" 5093)
                                                          ( Slice
                                                              { unSlice = [DimFix (Var (VName "gtid" 5126))]
                                                              }
                                                          )
                                                      )
                                                },
                                              Let
                                                { stmPat =
                                                    Pat
                                                      { patElems =
                                                          [ PatElem
                                                              { patElemName = VName "lifted_lambda_res" 5129,
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
                                                          (Var (VName "eta_p" 5128))
                                                      )
                                                }
                                            ],
                                        kernelBodyResult =
                                          [ Returns
                                              ResultMaySimplify
                                              (Certs {unCerts = []})
                                              (Var (VName "lifted_lambda_res" 5129))
                                          ]
                                      }
                                  )
                              )
                          )
                    }
            let res = execState ((stmToPrimExps @GPU) scope stm) mempty
            let expected =
                  M.fromList
                    [ (VName "defunc_0_map_res" 5125, Nothing),
                      (VName "gtid" 5126, Just (LeafExp (VName "n" 5142) (IntType Int64))),
                      (VName "eta_p" 5128, Nothing),
                      ( VName "lifted_lambda_res" 5129,
                        Just
                          ( BinOpExp
                              (Add Int64 OverflowWrap)
                              (ValueExp (IntValue (Int64Value 2)))
                              (LeafExp (VName "eta_p" 5128) (IntType Int64))
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
              [ (VName "n" 5142, FParamName (Prim (IntType Int64))),
                (VName "m" 5143, FParamName (Prim (IntType Int64))),
                ( VName "xss" 5144,
                  FParamName
                    ( Array
                        (IntType Int64)
                        ( Shape
                            { shapeDims =
                                [ Var (VName "n" 5142),
                                  Var (VName "m" 5143)
                                ]
                            }
                        )
                        Nonunique
                    )
                ),
                (VName "segmap_group_size" 5201, LetName (Prim (IntType Int64))),
                (VName "segmap_usable_groups" 5202, LetName (Prim (IntType Int64))),
                ( VName "defunc_0_map_res" 5203,
                  LetName
                    ( Array
                        (IntType Int64)
                        (Shape {shapeDims = [Var (VName "n" 5142)]})
                        NoUniqueness
                    )
                ),
                (VName "defunc_0_f_res" 5207, LetName (Prim (IntType Int64))),
                (VName "i" 5208, IndexName Int64),
                (VName "acc" 5209, FParamName (Prim (IntType Int64))),
                (VName "b" 5210, LetName (Prim (IntType Int64))),
                (VName "defunc_0_f_res" 5211, LetName (Prim (IntType Int64)))
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
                                { patElemName = VName "defunc_0_f_res" 5211,
                                  patElemDec = Prim (IntType Int64)
                                }
                            ]
                        },
                    stmAux = emptyStmAux,
                    stmExp =
                      BasicOp
                        ( BinOp
                            (Add Int64 OverflowWrap)
                            (Var (VName "acc" 5209))
                            (Var (VName "b" 5210))
                        )
                  }
          let res = execState ((stmToPrimExps @MC) scope stm) mempty
          let expected =
                M.fromList
                  [ ( VName "defunc_0_f_res" 5211,
                      Just
                        ( BinOpExp
                            (Add Int64 OverflowWrap)
                            (LeafExp (VName "acc" 5209) (IntType Int64))
                            (LeafExp (VName "b" 5210) (IntType Int64))
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
                                { patElemName = VName "b" 5210,
                                  patElemDec = Prim (IntType Int64)
                                }
                            ]
                        },
                    stmAux = emptyStmAux,
                    stmExp =
                      BasicOp
                        ( Index
                            (VName "xss" 5144)
                            ( Slice
                                { unSlice =
                                    [ DimFix (Var (VName "gtid" 5204)),
                                      DimFix (Var (VName "i" 5208))
                                    ]
                                }
                            )
                        )
                  }
          let res = execState ((stmToPrimExps @MC) scope stm) mempty
          let expected = M.fromList [(VName "b" 5210, Nothing)]
          res @?= expected,
        testCase "Loop" $ do
          let stm =
                Let
                  { stmPat =
                      Pat
                        { patElems =
                            [ PatElem
                                { patElemName = VName "defunc_0_f_res" 5207,
                                  patElemDec = Prim (IntType Int64)
                                }
                            ]
                        },
                    stmAux = emptyStmAux,
                    stmExp =
                      Loop
                        [ ( Param
                              { paramAttrs = Attrs {unAttrs = mempty},
                                paramName = VName "acc" 5209,
                                paramDec = Prim (IntType Int64)
                              },
                            Constant (IntValue (Int64Value 0))
                          )
                        ]
                        ( ForLoop
                            (VName "i" 5208)
                            Int64
                            (Var (VName "m" 5143))
                        )
                        ( Body
                            { bodyDec = (),
                              bodyStms = mempty,
                              bodyResult =
                                [ SubExpRes
                                    { resCerts = Certs {unCerts = []},
                                      resSubExp = Var (VName "defunc_0_f_res" 5211)
                                    }
                                ]
                            }
                        )
                  }
          let res = execState ((stmToPrimExps @MC) scope stm) mempty
          let expected =
                M.fromList
                  [ (VName "defunc_0_f_res" 5207, Nothing),
                    (VName "i" 5208, Just (LeafExp (VName "m" 5143) (IntType Int64))),
                    (VName "acc" 5209, Just (LeafExp (VName "acc" 5209) (IntType Int64)))
                  ]
          res @?= expected,
        testCase "Loop body" $ do
          let stm =
                Let
                  { stmPat =
                      Pat
                        { patElems =
                            [ PatElem
                                { patElemName = VName "defunc_0_f_res" 5207,
                                  patElemDec = Prim (IntType Int64)
                                }
                            ]
                        },
                    stmAux = emptyStmAux,
                    stmExp =
                      Loop
                        [ ( Param
                              { paramAttrs = Attrs {unAttrs = mempty},
                                paramName = VName "acc" 5209,
                                paramDec = Prim (IntType Int64)
                              },
                            Constant (IntValue (Int64Value 0))
                          )
                        ]
                        ( ForLoop
                            (VName "i" 5208)
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
                                                    { patElemName = VName "b" 5210,
                                                      patElemDec = Prim (IntType Int64)
                                                    }
                                                ]
                                            },
                                        stmAux = emptyStmAux,
                                        stmExp =
                                          BasicOp
                                            ( Index
                                                (VName "xss" 5144)
                                                ( Slice
                                                    { unSlice =
                                                        [ DimFix (Var (VName "gtid" 5204)),
                                                          DimFix (Var (VName "i" 5208))
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
                                                    { patElemName = VName "defunc_0_f_res" 5211,
                                                      patElemDec = Prim (IntType Int64)
                                                    }
                                                ]
                                            },
                                        stmAux = emptyStmAux,
                                        stmExp =
                                          BasicOp
                                            ( BinOp
                                                (Add Int64 OverflowWrap)
                                                (Var (VName "acc" 5209))
                                                (Var (VName "b" 5210))
                                            )
                                      }
                                  ],
                              bodyResult =
                                [ SubExpRes
                                    { resCerts = Certs {unCerts = []},
                                      resSubExp = Var (VName "defunc_0_f_res" 5211)
                                    }
                                ]
                            }
                        )
                  }
          let res = execState ((stmToPrimExps @MC) scope stm) mempty
          let expected =
                M.fromList
                  [ (VName "defunc_0_f_res" 5207, Nothing),
                    (VName "i" 5208, Just (LeafExp (VName "m" 5143) (IntType Int64))),
                    (VName "acc" 5209, Just (LeafExp (VName "acc" 5209) (IntType Int64))),
                    (VName "b" 5210, Nothing),
                    ( VName "defunc_0_f_res" 5211,
                      Just
                        ( BinOpExp
                            (Add Int64 OverflowWrap)
                            (LeafExp (VName "acc" 5209) (IntType Int64))
                            (LeafExp (VName "b" 5210) (IntType Int64))
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
                                { patElemName = VName "defunc_0_map_res" 5125,
                                  patElemDec =
                                    Array
                                      (IntType Int64)
                                      (Shape {shapeDims = [Var (VName "n" 5142)]})
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
                                    { segFlat = VName "flat_tid" 5112,
                                      unSegSpace =
                                        [ ( VName "gtid" 5126,
                                            Var (VName "n" 5142)
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
                                            (Var (VName "lifted_lambda_res" 5129))
                                        ]
                                    }
                                )
                            )
                        )
                  }
          let res = execState ((stmToPrimExps @MC) scope stm) mempty
          let expected =
                M.fromList
                  [ (VName "defunc_0_map_res" 5125, Nothing),
                    (VName "gtid" 5126, Just (LeafExp (VName "n" 5142) (IntType Int64)))
                  ]
          res @?= expected,
        testCase "SegMap body" $ do
          let stm =
                Let
                  { stmPat =
                      Pat
                        { patElems =
                            [ PatElem
                                { patElemName = VName "defunc_0_map_res" 5125,
                                  patElemDec =
                                    Array
                                      (IntType Int64)
                                      (Shape {shapeDims = [Var (VName "n" 5142)]})
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
                                    { segFlat = VName "flat_tid" 5112,
                                      unSegSpace =
                                        [ ( VName "gtid" 5126,
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
                                                            { patElemName = VName "eta_p" 5128,
                                                              patElemDec = Prim (IntType Int64)
                                                            }
                                                        ]
                                                    },
                                                stmAux = emptyStmAux,
                                                stmExp =
                                                  BasicOp
                                                    ( Index
                                                        (VName "xs" 5093)
                                                        (Slice {unSlice = [DimFix (Var (VName "gtid" 5126))]})
                                                    )
                                              },
                                            Let
                                              { stmPat =
                                                  Pat
                                                    { patElems =
                                                        [ PatElem
                                                            { patElemName = VName "lifted_lambda_res" 5129,
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
                                                        (Var (VName "eta_p" 5128))
                                                    )
                                              }
                                          ],
                                      kernelBodyResult =
                                        [ Returns
                                            ResultMaySimplify
                                            (Certs {unCerts = []})
                                            (Var (VName "lifted_lambda_res" 5129))
                                        ]
                                    }
                                )
                            )
                        )
                  }
          let res = execState ((stmToPrimExps @MC) scope stm) mempty
          let expected =
                M.fromList
                  [ (VName "defunc_0_map_res" 5125, Nothing),
                    (VName "gtid" 5126, Just (LeafExp (VName "n" 5142) (IntType Int64))),
                    (VName "eta_p" 5128, Nothing),
                    ( VName "lifted_lambda_res" 5129,
                      Just
                        ( BinOpExp
                            (Add Int64 OverflowWrap)
                            (ValueExp (IntValue (Int64Value 2)))
                            (LeafExp (VName "eta_p" 5128) (IntType Int64))
                        )
                    )
                  ]
          res @?= expected
        ]
