module Futhark.Optimise.Fusion.ScremaTests (tests) where

import Control.Monad.Reader
import Control.Monad.State
import Data.String (fromString)
import Futhark.Analysis.HORep.SOAC as SOAC
import Futhark.FreshNames
import Futhark.IR.SOACS
import Futhark.IR.SOACSTests ()
import Futhark.Optimise.Fusion.Screma
  ( SuperScrema (..),
    fuseScrema,
    fuseSuperScrema,
    moveRedScanSuperScrema,
    splitLambdaByPar,
  )
import Test.Tasty
import Test.Tasty.HUnit

withFreshNamesScopeError :: ReaderT (Scope SOACS) (StateT VNameSource Maybe) a -> Maybe a
withFreshNamesScopeError m =
  evalStateT (runReaderT m mempty) (newNameSource 10000)

withFreshNamesAndScope :: ReaderT (Scope SOACS) (State VNameSource) a -> a
withFreshNamesAndScope m =
  evalState (runReaderT m mempty) (newNameSource 10000)

withFreshNames :: State VNameSource a -> a
withFreshNames m =
  evalState m (newNameSource 10000)

fromLines :: [String] -> Lambda SOACS
fromLines = fromString . unlines

splitLambdaByParTester :: [VName] -> Lambda SOACS -> Tuple2 (Lambda SOACS) (Lambda SOACS)
splitLambdaByParTester names lam = Tuple2 (lam_x', lam_y')
  where
    ((_, lam_x', _), (_, lam_y', _)) =
      splitLambdaByPar names (lambdaParams lam) lam (lambdaReturnType lam)

-- | A wrapper that makes 'show' behave like 'prettyString'.
newtype Tuple2 a b = Tuple2 (a, b)
  deriving (Eq, Ord)

instance (Pretty a, Pretty b) => Show (Tuple2 a b) where
  show (Tuple2 (x, y)) = "(" <> prettyString x <> ",\n" <> prettyString y <> ")"

newtype Tuple3 a b c = Tuple3 (Maybe (a, b, c))
  deriving (Eq, Ord)

instance (Pretty a, Pretty b, Pretty c) => Show (Tuple3 a b c) where
  show (Tuple3 (Just (x, y, z))) =
    "(" <> prettyString x <> ",\n" <> prettyString y <> ",\n" <> prettyString z <> ")"
  show (Tuple3 Nothing) = "Nothing"

-- | A wrapper that makes 'pretty' behave like 'Show'.
newtype Singleton a = Singleton a
  deriving (Eq, Ord)

instance (Pretty a) => Show (Singleton a) where
  show (Singleton x) = prettyString x

tests :: TestTree
tests =
  testGroup
    "ScremaTests"
    [ testGroup
        "splitLambdaByPar"
        [ testCase "keeps params and result" $
            let lam = "\\{x_0 : i32, x_1 : i32} : {i32, i32} -> {x_0, x_1}"
                lam_x = "\\{x_0 : i32} : {i32} -> {x_0}"
                lam_y = "\\{x_1 : i32} : {i32} -> {x_1}"
                names = ["x_0"]
             in splitLambdaByParTester names lam @?= Tuple2 (lam_x, lam_y),
          testCase "keeps computation in first lambda" $
            let lam =
                  fromLines
                    [ "\\{x_0 : i32, x_1 : i32} : {i32, i32} ->",
                      "  let {x_2 : i32} = add32(x_0, x_1)",
                      "  in {x_0, x_2}"
                    ]
                lam_x =
                  fromLines
                    [ "\\{x_0 : i32, x_1 : i32} : {i32} ->",
                      "  let {x_2 : i32} = add32(x_0, x_1)",
                      "  in {x_2}"
                    ]
                lam_y = "\\{x_0 : i32} : {i32} -> {x_0}"
                names = ["x_1"]
             in splitLambdaByParTester names lam @?= Tuple2 (lam_x, lam_y),
          testCase "keeps computations in both lambdas" $
            let lam =
                  fromLines
                    [ "\\{x_0 : i32, x_1 : i32} : {i32, i32} -> ",
                      "  let {x_2 : i32} = add32(x_0, x_1) ",
                      "  let {x_3 : i32} = add32(1i32, x_0) ",
                      "  in {x_3, x_2}"
                    ]
                lam_x =
                  fromLines
                    [ "\\{x_0 : i32, x_1 : i32} : {i32} -> ",
                      "  let {x_2 : i32} = add32(x_0, x_1) ",
                      "  in {x_2}"
                    ]
                lam_y =
                  fromLines
                    [ "\\{x_0 : i32} : {i32} -> ",
                      "  let {x_3 : i32} = add32(1i32, x_0) ",
                      "  in {x_3}"
                    ]
                names = ["x_1"]
             in splitLambdaByParTester names lam @?= Tuple2 (lam_x, lam_y),
          testCase "keeps line order" $
            let lam =
                  fromLines
                    [ "\\{x_0 : i32, x_1 : i32} : {i32, i32} -> ",
                      "  let {x_3 : i32} = add32(1i32, x_0) ",
                      "  let {x_2 : i32} = add32(x_0, x_1) ",
                      "  let {x_4 : i32} = add32(1i32, x_3) ",
                      "  in {x_4, x_2}"
                    ]
                lam_x =
                  fromLines
                    [ "\\{x_0 : i32, x_1 : i32} : {i32} -> ",
                      "  let {x_2 : i32} = add32(x_0, x_1) ",
                      "  in {x_2}"
                    ]
                lam_y =
                  fromLines
                    [ "\\{x_0 : i32} : {i32} -> ",
                      "  let {x_3 : i32} = add32(1i32, x_0) ",
                      "  let {x_4 : i32} = add32(1i32, x_3) ",
                      "  in {x_4}"
                    ]
                names = ["x_1"]
             in splitLambdaByParTester names lam @?= Tuple2 (lam_x, lam_y),
          testCase "does redundant work" $
            let lam =
                  fromLines
                    [ "\\{x_0 : i32, x_1 : i32} : {i32, i32} -> ",
                      "  let {x_2 : i32} = add32(1i32, x_0) ",
                      "  let {x_3 : i32} = add32(x_1, x_2) ",
                      "  in {x_3, x_2}"
                    ]
                lam_x =
                  fromLines
                    [ "\\{x_0 : i32, x_1 : i32} : {i32} -> ",
                      "  let {x_2 : i32} = add32(1i32, x_0) ",
                      "  let {x_3 : i32} = add32(x_1, x_2) ",
                      "  in {x_3}"
                    ]
                lam_y =
                  fromLines
                    [ "\\{x_0 : i32} : {i32} -> ",
                      "  let {x_2 : i32} = add32(1i32, x_0) ",
                      "  in {x_2}"
                    ]
                names = ["x_1"]
             in splitLambdaByParTester names lam @?= Tuple2 (lam_x, lam_y)
        ],
      testGroup
        "fuseSuperScrema"
        [ testCase "map-scan (vertical)" $
            let scan_op =
                  Scan
                    ( fromLines
                        [ "\\ {eta_p_5571 : i32, eta_p_5572 : i32} : {i32} ->",
                          "let {defunc_0_op_res_5573 : i32} = add32(eta_p_5571, eta_p_5572)",
                          "in {defunc_0_op_res_5573}"
                        ]
                    )
                    ["0i32"]
                ident_a = "input_a_5565 : [d_5537]i32"
                ident_b = "input_b_5538 : [d_5537]i32"
                input_a = SOAC.identInput ident_a
                input_b = SOAC.identInput ident_b
             in Tuple2
                  ( withFreshNames
                      ( fuseSuperScrema
                          "d_5537"
                          [input_a]
                          ( ScremaForm
                              ( fromLines
                                  [ "\\ {eta_p_5566 : i32} : {i32} ->",
                                    "let {lifted_lambda_res_5567 : i32} = add32(2i32, eta_p_5566)",
                                    "in {lifted_lambda_res_5567}"
                                  ]
                              )
                              []
                              []
                              "\\ {x_5568 : i32} : {i32} -> {x_5568}"
                          )
                          [identName ident_b]
                          [input_b]
                          ( ScremaForm
                              "\\ {x_5570 : i32} : {i32} -> {x_5570}"
                              [scan_op]
                              []
                              "\\ {x_5574 : i32} : {i32} -> {x_5574}"
                          )
                          ["defunc_0_scan_res_5569"]
                      )
                  )
                  @?= Tuple2
                    ( SuperScrema
                        "d_5537"
                        [input_a]
                        ( fromLines
                            [ "\\ {eta_p_5566 : i32}: {i32} ->",
                              "let {lifted_lambda_res_5567 : i32} = add32(2i32, eta_p_5566)",
                              "in {lifted_lambda_res_5567}"
                            ]
                        )
                        []
                        []
                        ( fromLines
                            [ "\\ {x_5568 : i32}: {i32, i32} -> ",
                              "let {x_5570 : i32} = x_5568",
                              "in {x_5570, x_5568}"
                            ]
                        )
                        [scan_op]
                        []
                        ( fromLines
                            [ "\\ {x_5574 : i32, x_10000 : i32}: {i32, i32} ->",
                              "{x_5574, x_10000}"
                            ]
                        ),
                      ["defunc_0_scan_res_5569", identName ident_b]
                    ),
          testCase "map-scan (horizontal)" $
            let scan_op =
                  Scan
                    ( fromLines
                        [ "\\ {eta_p_5571 : i32, eta_p_5572 : i32} : {i32} ->",
                          "let {defunc_0_op_res_5573 : i32} = add32(eta_p_5571, eta_p_5572)",
                          "in {defunc_0_op_res_5573}"
                        ]
                    )
                    ["0i32"]
                ident_a = "input_a_5565 : [d_5537]i32"
                ident_b = "input_b_5538 : [d_5537]i32"
                input_a = SOAC.identInput ident_a
                input_b = SOAC.identInput ident_b
                out_a = "out_a_5564145"
                out_b = "out_b_5534156"
             in Tuple2
                  ( withFreshNames
                      ( fuseSuperScrema
                          "d_5537"
                          [input_a]
                          ( ScremaForm
                              ( fromLines
                                  [ "\\ {eta_p_5566 : i32} : {i32} ->",
                                    "let {lifted_lambda_res_5567 : i32} = add32(2i32, eta_p_5566)",
                                    "in {lifted_lambda_res_5567}"
                                  ]
                              )
                              []
                              []
                              "\\ {x_5568 : i32} : {i32} -> {x_5568}"
                          )
                          [out_a]
                          [input_b]
                          ( ScremaForm
                              "\\ {x_5570 : i32} : {i32} -> {x_5570}"
                              [scan_op]
                              []
                              "\\ {x_5574 : i32} : {i32} -> {x_5574}"
                          )
                          [out_b]
                      )
                  )
                  @?= Tuple2
                    ( SuperScrema
                        "d_5537"
                        [input_a, input_b]
                        ( fromLines
                            [ "\\ {eta_p_5566 : i32, x_10000 : i32}: {i32, i32} ->",
                              "let {lifted_lambda_res_5567 : i32} = add32(2i32, eta_p_5566)",
                              "in {lifted_lambda_res_5567, x_10000}"
                            ]
                        )
                        []
                        []
                        ( fromLines
                            [ "\\ {x_5568 : i32, x_5570 : i32}: {i32, i32} -> ",
                              "{x_5570, x_5568}"
                            ]
                        )
                        [scan_op]
                        []
                        ( fromLines
                            [ "\\ {x_5574 : i32, x_10001 : i32}: {i32, i32} ->",
                              "{x_5574, x_10001}"
                            ]
                        ),
                      [out_b, out_a]
                    ),
          testCase "map-scan (vertical) with reduce (horizontal)" $
            let scan_op =
                  Scan
                    ( fromLines
                        [ "\\ {eta_p_5571 : i32, eta_p_5572 : i32} : {i32} ->",
                          "let {defunc_0_op_res_5573 : i32} = add32(eta_p_5571, eta_p_5572)",
                          "in {defunc_0_op_res_5573}"
                        ]
                    )
                    ["0i32"]
                reduce_op =
                  Reduce
                    Commutative
                    ( fromLines
                        [ "\\ {eta_p_55720 : i32, eta_p_557201 : i32} : {i32} ->",
                          "let {defunc_0_op_res_5573 : i32} = add32(eta_p_55720, eta_p_557201)",
                          "in {defunc_0_op_res_5573}"
                        ]
                    )
                    ["0i32"]

                ident_a = "input_a_5565 : [d_5537]i32"
                ident_b = "input_b_5538 : [d_5537]i32"
                input_a = SOAC.identInput ident_a
                input_b = SOAC.identInput ident_b
             in Tuple2
                  ( withFreshNames
                      ( fuseSuperScrema
                          "d_5537"
                          [input_a]
                          ( ScremaForm
                              ( fromLines
                                  [ "\\ {eta_p_5566 : i32} : {i32} ->",
                                    "let {lifted_lambda_res_5567 : i32} = add32(2i32, eta_p_5566)",
                                    "in {lifted_lambda_res_5567, lifted_lambda_res_5567}"
                                  ]
                              )
                              []
                              [reduce_op]
                              "\\ {x_5568 : i32} : {i32} -> {x_5568}"
                          )
                          ["red_out_543532", identName ident_b]
                          [input_b]
                          ( ScremaForm
                              "\\ {x_5570 : i32} : {i32} -> {x_5570}"
                              [scan_op]
                              []
                              ( fromLines
                                  [ "\\ {x_5574 : i32} : {i32} ->",
                                    "let {y_5567 : i32} = add32(2i32, x_5574)",
                                    "in {y_5567}"
                                  ]
                              )
                          )
                          ["defunc_0_scan_res_5569"]
                      )
                  )
                  @?= Tuple2
                    ( SuperScrema
                        "d_5537"
                        [input_a]
                        ( fromLines
                            [ "\\ {eta_p_5566 : i32}: {i32} ->",
                              "let {lifted_lambda_res_5567 : i32} = add32(2i32, eta_p_5566)",
                              "in {lifted_lambda_res_5567, lifted_lambda_res_5567}"
                            ]
                        )
                        []
                        [reduce_op]
                        ( fromLines
                            [ "\\ {x_5568 : i32}: {i32, i32} -> ",
                              "let {x_5570 : i32} = x_5568",
                              "in {x_5570, x_5568}"
                            ]
                        )
                        [scan_op]
                        []
                        ( fromLines
                            [ "\\ {x_5574 : i32, x_10000 : i32}: {i32, i32} ->",
                              "let {y_5567 : i32} = add32(2i32, x_5574)",
                              "in {y_5567, x_10000}"
                            ]
                        ),
                      ["red_out_543532", "defunc_0_scan_res_5569", identName ident_b]
                    ),
          testCase "map-map (vertical)" $
            let ident_a = "input_a_5565 : [d_5537]i32"
                ident_b = "input_b_5538 : [d_5537]i32"
                input_a = SOAC.identInput ident_a
                input_b = SOAC.identInput ident_b
             in Tuple2
                  ( withFreshNames
                      ( fuseSuperScrema
                          "d_5537"
                          [input_a]
                          ( ScremaForm
                              ( fromLines
                                  [ "\\ {eta_p_5566 : i32} : {i32} ->",
                                    "let {lifted_lambda_res_5567 : i32} = add32(2i32, eta_p_5566)",
                                    "in {lifted_lambda_res_5567, lifted_lambda_res_5567}"
                                  ]
                              )
                              []
                              []
                              "\\ {x_5568 : i32} : {i32} -> {x_5568}"
                          )
                          [identName ident_b]
                          [input_b]
                          ( ScremaForm
                              ( fromLines
                                  [ "\\ {x_5574 : i32} : {i32} ->",
                                    "let {y_5567 : i32} = add32(3i32, x_5574)",
                                    "in {y_5567}"
                                  ]
                              )
                              []
                              []
                              ( fromLines
                                  [ "\\ {x_5570 : i32} : {i32} -> {x_5570}"
                                  ]
                              )
                          )
                          ["defunc_0_scan_res_5569"]
                      )
                  )
                  @?= Tuple2
                    ( SuperScrema
                        "d_5537"
                        [input_a]
                        ( fromLines
                            [ "\\ {eta_p_5566 : i32}: {i32} ->",
                              "let {lifted_lambda_res_5567 : i32} = add32(2i32, eta_p_5566)",
                              "in {lifted_lambda_res_5567, lifted_lambda_res_5567}"
                            ]
                        )
                        []
                        []
                        ( fromLines
                            [ "\\ {x_5568 : i32}: {i32, i32} -> ",
                              "let {x_5574 : i32} = x_5568",
                              "let {y_5567 : i32} = add32(3i32, x_5574)",
                              "in {y_5567, x_5568}"
                            ]
                        )
                        []
                        []
                        ( fromLines
                            [ "\\ {x_5570 : i32, x_10000 : i32}: {i32, i32} ->",
                              "in {x_5570, x_10000}"
                            ]
                        ),
                      ["defunc_0_scan_res_5569", identName ident_b]
                    ),
          testCase "map-scan-map (vertical)" $
            let scan_op =
                  Scan
                    ( fromLines
                        [ "\\ {eta_p_5571 : i32, eta_p_5572 : i32} : {i32} ->",
                          "let {defunc_0_op_res_5573 : i32} = add32(eta_p_5571, eta_p_5572)",
                          "in {defunc_0_op_res_5573}"
                        ]
                    )
                    ["0i32"]
                ident_a = "input_a_5565 : [d_5537]i32"
                ident_b = "input_b_5538 : [d_5537]i32"
                input_a = SOAC.identInput ident_a
                input_b = SOAC.identInput ident_b
             in Tuple2
                  ( withFreshNames
                      ( fuseSuperScrema
                          "d_5537"
                          [input_a]
                          ( ScremaForm
                              ( fromLines
                                  [ "\\ {eta_p_5566 : i32} : {i32} ->",
                                    "let {lifted_lambda_res_5567 : i32} = add32(2i32, eta_p_5566)",
                                    "in {lifted_lambda_res_5567}"
                                  ]
                              )
                              []
                              []
                              "\\ {x_5568 : i32} : {i32} -> {x_5568}"
                          )
                          [identName ident_b]
                          [input_b]
                          ( ScremaForm
                              "\\ {x_5570 : i32} : {i32} -> {x_5570}"
                              [scan_op]
                              []
                              ( fromLines
                                  [ "\\ {x_5574 : i32} : {i32} ->",
                                    "let {y_6363: i32} = add32(3i32, x_5574)",
                                    "in {y_6363}"
                                  ]
                              )
                          )
                          ["defunc_0_scan_res_5569"]
                      )
                  )
                  @?= Tuple2
                    ( SuperScrema
                        "d_5537"
                        [input_a]
                        ( fromLines
                            [ "\\ {eta_p_5566 : i32}: {i32} ->",
                              "let {lifted_lambda_res_5567 : i32} = add32(2i32, eta_p_5566)",
                              "in {lifted_lambda_res_5567}"
                            ]
                        )
                        []
                        []
                        ( fromLines
                            [ "\\ {x_5568 : i32}: {i32, i32} -> ",
                              "let {x_5570 : i32} = x_5568",
                              "in {x_5570, x_5568}"
                            ]
                        )
                        [scan_op]
                        []
                        ( fromLines
                            [ "\\ {x_5574 : i32, x_10000 : i32}: {i32, i32} ->",
                              "let {y_6363 : i32} = add32(3i32, x_5574)",
                              "in {y_6363, x_10000}"
                            ]
                        ),
                      ["defunc_0_scan_res_5569", identName ident_b]
                    ),
          testCase "red-red fusion" $
            let reduce_op' =
                  Reduce
                    Commutative
                    ( fromLines
                        [ "\\ {a_0 : i32, b_1 : i32} : {i32} ->",
                          "let {c_2 : i32} = add32(a_0, b_1)",
                          "in {c_2}"
                        ]
                    )
                    ["0i32"]
                reduce_op =
                  Reduce
                    Commutative
                    ( fromLines
                        [ "\\ {a_4 : f32, b_5 : f32} : {f32} ->",
                          "let {c_6 : f32} = fadd32(a_4, b_5)",
                          "in {c_6}"
                        ]
                    )
                    ["0.0f32"]
                ident_a = "input_a_7 : [d_9]i32"
                input_a = SOAC.identInput ident_a
                ident_b = "input_b_8 : [d_9]f32"
                input_b = SOAC.identInput ident_b
             in Tuple2
                  ( withFreshNames
                      ( fuseSuperScrema
                          "d_9"
                          [input_a]
                          ( ScremaForm
                              ( fromLines
                                  [ "\\ {x_11 : i32} : {i32} ->",
                                    "in {x_11}"
                                  ]
                              )
                              []
                              [reduce_op']
                              (fromLines ["nilFn"])
                          )
                          ["out_a_12"]
                          [input_b]
                          ( ScremaForm
                              ( fromLines
                                  [ "\\ {x_13 : f32} : {f32} ->",
                                    "in {x_13}"
                                  ]
                              )
                              []
                              [reduce_op]
                              (fromLines ["nilFn"])
                          )
                          ["out_b_14"]
                      )
                  )
                  @?= Tuple2
                    ( SuperScrema
                        "d_9"
                        [input_a, input_b]
                        ( fromLines
                            [ "\\ {x_11 : i32, x_10000 : f32}: {i32, f32} ->",
                              "{x_11, x_10000}"
                            ]
                        )
                        []
                        [reduce_op']
                        ( fromLines
                            [ "\\ {x_13 : f32} : {f32} ->",
                              "{x_13}"
                            ]
                        )
                        []
                        [reduce_op]
                        (fromLines ["nilFn"]),
                      ["out_a_12", "out_b_14"]
                    ),
          testCase "scan-scan fusion" $
            let scan_op' =
                  Scan
                    ( fromLines
                        [ "\\ {a_0 : i32, b_1 : i32} : {i32} ->",
                          "let {c_2 : i32} = add32(a_0, b_1)",
                          "in {c_2}"
                        ]
                    )
                    ["0i32"]
                scan_op =
                  Scan
                    ( fromLines
                        [ "\\ {a_4 : f32, b_5 : f32} : {f32} ->",
                          "let {c_6 : f32} = fadd32(a_4, b_5)",
                          "in {c_6}"
                        ]
                    )
                    ["0.0f32"]
                ident_a = "input_a_7 : [d_9]i32"
                input_a = SOAC.identInput ident_a
                ident_b = "input_b_8 : [d_9]f32"
                input_b = SOAC.identInput ident_b
             in Tuple2
                  ( withFreshNames
                      ( fuseSuperScrema
                          "d_9"
                          [input_a]
                          ( ScremaForm
                              ( fromLines
                                  [ "\\ {x_11 : i32} : {i32} ->",
                                    "in {x_11}"
                                  ]
                              )
                              [scan_op']
                              []
                              ( fromLines
                                  [ "\\ {x_12 : i32} : {i32} ->",
                                    "in {x_12}"
                                  ]
                              )
                          )
                          ["out_a_16"]
                          [input_b]
                          ( ScremaForm
                              ( fromLines
                                  [ "\\ {x_13 : f32} : {f32} ->",
                                    "in {x_13}"
                                  ]
                              )
                              [scan_op]
                              []
                              ( fromLines
                                  [ "\\ {x_14 : f32} : {f32} ->",
                                    "in {x_14}"
                                  ]
                              )
                          )
                          ["out_b_15"]
                      )
                  )
                  @?= Tuple2
                    ( SuperScrema
                        "d_9"
                        [input_a, input_b]
                        ( fromLines
                            [ "\\ {x_11 : i32, x_10000 : f32}: {i32, f32} ->",
                              "{x_11, x_10000}"
                            ]
                        )
                        [scan_op']
                        []
                        ( fromLines
                            [ "\\ {x_12 : i32, x_13 : f32} : {i32, f32} ->",
                              "{x_13, x_12}"
                            ]
                        )
                        [scan_op]
                        []
                        ( fromLines
                            [ "\\ {x_14 : f32, x_10001 : i32} : {f32, i32} ->",
                              "{x_14, x_10001}"
                            ]
                        ),
                      ["out_b_15", "out_a_16"]
                    )
        ],
      testGroup
        "moveRedScanSuperScrema"
        [ testCase "Only map" $
            let ident_a = "input_a_5565 : [d_5537]i32"
                input_a = SOAC.identInput ident_a
             in Singleton
                  ( withFreshNamesAndScope
                      ( moveRedScanSuperScrema
                          ( SuperScrema
                              "d_5537"
                              [input_a]
                              ( fromLines
                                  [ "\\ {x_0 : i32}: {i32} ->",
                                    "let {y_1 : i32} = add32(1i32, x_0)",
                                    "in {y_1}"
                                  ]
                              )
                              []
                              []
                              ( fromLines
                                  [ "\\ {x_2 : i32}: {i32} -> ",
                                    "let {y_3 : i32} = add32(2i32, x_2)",
                                    "in {y_3}"
                                  ]
                              )
                              []
                              []
                              ( fromLines
                                  [ "\\ {x_4 : i32}: {i32} -> ",
                                    "let {y_5 : i32} = add32(3i32, x_4)",
                                    "in {y_5}"
                                  ]
                              )
                          )
                      )
                  )
                  @?= Singleton
                    ( SuperScrema
                        "d_5537"
                        [input_a]
                        ( fromLines
                            [ "\\ {x_0 : i32}: {i32} ->",
                              "let {y_1 : i32} = add32(1i32, x_0)",
                              "in {y_1}"
                            ]
                        )
                        []
                        []
                        ( fromLines
                            [ "\\ {x_2 : i32}: {i32} -> ",
                              "let {y_3 : i32} = add32(2i32, x_2)",
                              "in {y_3}"
                            ]
                        )
                        []
                        []
                        ( fromLines
                            [ "\\ {x_4 : i32}: {i32} -> ",
                              "let {y_5 : i32} = add32(3i32, x_4)",
                              "in {y_5}"
                            ]
                        )
                    ),
          testCase "map-map-scan-map" $
            let scan_op =
                  Scan
                    ( fromLines
                        [ "\\ {eta_p_5571 : i32, eta_p_5572 : i32} : {i32} ->",
                          "let {defunc_0_op_res_5573 : i32} = add32(eta_p_5571, eta_p_5572)",
                          "in {defunc_0_op_res_5573}"
                        ]
                    )
                    ["0i32"]
                ident_a = "input_a_5565 : [d_5537]i32"
                input_a = SOAC.identInput ident_a
             in Singleton
                  ( withFreshNamesAndScope
                      ( moveRedScanSuperScrema
                          ( SuperScrema
                              "d_5537"
                              [input_a]
                              ( fromLines
                                  [ "\\ {x_0 : i32}: {i32} ->",
                                    "let {y_1 : i32} = add32(2i32, x_0)",
                                    "in {y_1}"
                                  ]
                              )
                              []
                              []
                              ( fromLines
                                  [ "\\ {x_2 : i32}: {i32} -> ",
                                    "let {y_3 : i32} = add32(2i32, x_2)",
                                    "in {y_3}"
                                  ]
                              )
                              [scan_op]
                              []
                              ( fromLines
                                  [ "\\ {x_4 : i32}: {i32} -> ",
                                    "let {y_5 : i32} = add32(2i32, x_4)",
                                    "in {y_5}"
                                  ]
                              )
                          )
                      )
                  )
                  @?= Singleton
                    ( SuperScrema
                        "d_5537"
                        [input_a]
                        ( fromLines
                            [ "\\ {x_0 : i32}: {i32, i32} ->",
                              "let {y_1 : i32} = add32(2i32, x_0)",
                              "let {x_10000 : i32} = y_1",
                              "let {y_10001 : i32} = add32(2i32, x_10000)",
                              "in {y_10001, y_1}"
                            ]
                        )
                        [scan_op]
                        []
                        ( fromLines
                            [ "\\ {x_10002 : i32, x_2 : i32}: {i32} -> ",
                              "in {x_10002}"
                            ]
                        )
                        []
                        []
                        ( fromLines
                            [ "\\ {x_4 : i32}: {i32} -> ",
                              "let {y_5 : i32} = add32(2i32, x_4)",
                              "in {y_5}"
                            ]
                        )
                    ),
          testCase "map-scan-map-map" $
            let scan_op =
                  Scan
                    ( fromLines
                        [ "\\ {eta_p_5571 : i32, eta_p_5572 : i32} : {i32} ->",
                          "let {defunc_0_op_res_5573 : i32} = add32(eta_p_5571, eta_p_5572)",
                          "in {defunc_0_op_res_5573}"
                        ]
                    )
                    ["0i32"]
                ident_a = "input_a_5565 : [d_5537]i32"
                input_a = SOAC.identInput ident_a
             in Singleton
                  ( withFreshNamesAndScope
                      ( moveRedScanSuperScrema
                          ( SuperScrema
                              "d_5537"
                              [input_a]
                              ( fromLines
                                  [ "\\ {x_5566 : i32}: {i32} ->",
                                    "let {y_5567 : i32} = add32(1i32, x_5566)",
                                    "in {y_5567}"
                                  ]
                              )
                              [scan_op]
                              []
                              ( fromLines
                                  [ "\\ {x_5568 : i32}: {i32} -> ",
                                    "let {y_5570 : i32} = add32(2i32, x_5568)",
                                    "in {y_5570}"
                                  ]
                              )
                              []
                              []
                              ( fromLines
                                  [ "\\ {x_5571 : i32}: {i32} -> ",
                                    "let {y_5572 : i32} = add32(3i32, x_5571)",
                                    "in {y_5572}"
                                  ]
                              )
                          )
                      )
                  )
                  @?= Singleton
                    ( SuperScrema
                        "d_5537"
                        [input_a]
                        ( fromLines
                            [ "\\ {x_5566 : i32}: {i32} ->",
                              "let {y_5567 : i32} = add32(1i32, x_5566)",
                              "in {y_5567}"
                            ]
                        )
                        [scan_op]
                        []
                        ( fromLines
                            [ "\\ {x_5568 : i32}: {i32} -> ",
                              "let {y_5570 : i32} = add32(2i32, x_5568)",
                              "in {y_5570}"
                            ]
                        )
                        []
                        []
                        ( fromLines
                            [ "\\ {x_5571 : i32}: {i32} -> ",
                              "let {y_5572 : i32} = add32(3i32, x_5571)",
                              "in {y_5572}"
                            ]
                        )
                    ),
          testCase "map-map-scan,reduce-map" $
            let scan_op =
                  Scan
                    ( fromLines
                        [ "\\ {eta_p_5571 : i32, eta_p_5572 : i32} : {i32} ->",
                          "let {defunc_0_op_res_5573 : i32} = add32(eta_p_5571, eta_p_5572)",
                          "in {defunc_0_op_res_5573}"
                        ]
                    )
                    ["0i32"]
                reduce_op =
                  Reduce
                    Commutative
                    ( fromLines
                        [ "\\ {eta_p_55720 : i32, eta_p_557201 : i32} : {i32} ->",
                          "let {defunc_0_op_res_5573 : i32} = add32(eta_p_55720, eta_p_557201)",
                          "in {defunc_0_op_res_5573}"
                        ]
                    )
                    ["0i32"]
                ident_a = "input_a_5565 : [d_5537]i32"
                input_a = SOAC.identInput ident_a
             in Singleton
                  ( withFreshNamesAndScope
                      ( moveRedScanSuperScrema
                          ( SuperScrema
                              "d_5537"
                              [input_a]
                              ( fromLines
                                  [ "\\ {x_5566 : i32}: {i32} ->",
                                    "let {y_5567 : i32} = add32(2i32, x_5566)",
                                    "in {y_5567}"
                                  ]
                              )
                              []
                              []
                              ( fromLines
                                  [ "\\ {x_5568 : i32}: {i32, i32} -> ",
                                    "let {y_5570 : i32} = add32(2i32, x_5568)",
                                    "in {y_5570, y_5570}"
                                  ]
                              )
                              [scan_op]
                              [reduce_op]
                              ( fromLines
                                  [ "\\ {x_5571 : i32}: {i32} -> ",
                                    "let {y_5572 : i32} = add32(2i32, x_5571)",
                                    "in {y_5572}"
                                  ]
                              )
                          )
                      )
                  )
                  @?= Singleton
                    ( SuperScrema
                        "d_5537"
                        [input_a]
                        ( fromLines
                            [ "\\ {x_5566 : i32}: {i32, i32, i32} ->",
                              "let {y_5567 : i32} = add32(2i32, x_5566)",
                              "let {x_10000 : i32} = y_5567",
                              "let {y_10001 : i32} = add32(2i32, x_10000)",
                              "in {y_10001, y_10001, y_5567}"
                            ]
                        )
                        [scan_op]
                        [reduce_op]
                        ( fromLines
                            [ "\\ {x_10002 : i32, x_5568 : i32}: {i32} -> ",
                              "in {x_10002}"
                            ]
                        )
                        []
                        []
                        ( fromLines
                            [ "\\ {x_5571 : i32}: {i32} -> ",
                              "let {y_5572 : i32} = add32(2i32, x_5571)",
                              "in {y_5572}"
                            ]
                        )
                    ),
          testCase "map-reduce-map-scan,reduce-map" $
            let scan_op =
                  Scan
                    ( fromLines
                        [ "\\ {eta_p_5571 : i32, eta_p_5572 : i32} : {i32} ->",
                          "let {defunc_0_op_res_5573 : i32} = add32(eta_p_5571, eta_p_5572)",
                          "in {defunc_0_op_res_5573}"
                        ]
                    )
                    ["0i32"]
                reduce_op =
                  Reduce
                    Commutative
                    ( fromLines
                        [ "\\ {eta_p_55720 : i32, eta_p_557201 : i32} : {i32} ->",
                          "let {defunc_0_op_res_5573 : i32} = add32(eta_p_55720, eta_p_557201)",
                          "in {defunc_0_op_res_5573}"
                        ]
                    )
                    ["0i32"]
                reduce_op' =
                  Reduce
                    Commutative
                    ( fromLines
                        [ "\\ {eta_p_0 : i32, eta_p_1 : i32} : {i32} ->",
                          "let {defunc_0_op_res_3 : i32} = add32(eta_p_0, eta_p_1)",
                          "in {defunc_0_op_res_3}"
                        ]
                    )
                    ["0i32"]
                ident_a = "input_a_5565 : [d_5537]i32"
                input_a = SOAC.identInput ident_a
             in Singleton
                  ( withFreshNamesAndScope
                      ( moveRedScanSuperScrema
                          ( SuperScrema
                              "d_5537"
                              [input_a]
                              ( fromLines
                                  [ "\\ {x_5566 : i32}: {i32, i32} ->",
                                    "let {y_5567 : i32} = add32(1i32, x_5566)",
                                    "in {x_5566, y_5567}"
                                  ]
                              )
                              []
                              [reduce_op']
                              ( fromLines
                                  [ "\\ {x_6777 : i32}: {i32, i32} -> ",
                                    "{x_6777, x_6777}"
                                  ]
                              )
                              [scan_op]
                              [reduce_op]
                              ( fromLines
                                  [ "\\ {x_5571 : i32}: {i32} -> ",
                                    "let {y_5572 : i32} = add32(3i32, x_5571)",
                                    "in {y_5572}"
                                  ]
                              )
                          )
                      )
                  )
                  @?= Singleton
                    ( SuperScrema
                        "d_5537"
                        [input_a]
                        ( fromLines
                            [ "\\ {x_5566 : i32}: {i32, i32, i32, i32} ->",
                              "let {y_5567 : i32} = add32(1i32, x_5566)",
                              "let {x_10000 : i32} = y_5567",
                              "in {x_10000, x_5566, x_10000, y_5567}"
                            ]
                        )
                        [scan_op]
                        [reduce_op', reduce_op]
                        ( fromLines
                            [ "\\ {x_10001 : i32, x_6777 : i32}: {i32} -> ",
                              "in {x_10001}"
                            ]
                        )
                        []
                        []
                        ( fromLines
                            [ "\\ {x_5571 : i32}: {i32} -> ",
                              "let {y_5572 : i32} = add32(3i32, x_5571)",
                              "in {y_5572}"
                            ]
                        )
                    )
        ],
      testGroup
        "fuseScrema"
        [ testCase "map-map (vertical)" $
            let w = Var "n"
                ident_a = "input_a_6 : [d_5537]i32"
                ident_b = "input_b_7 : [d_5537]i32"
                input_a = SOAC.identInput ident_a
                input_b = SOAC.identInput ident_b
             in Tuple3
                  ( withFreshNamesScopeError
                      ( fuseScrema
                          w
                          [input_a]
                          ( ScremaForm
                              ( fromLines
                                  [ "\\ {x_0 : i32} : {i32} ->",
                                    "let {y_1 : i32} = add32(1i32, x_0)",
                                    "in {y_1}"
                                  ]
                              )
                              []
                              []
                              "\\ {x_2 : i32} : {i32} -> {x_2}"
                          )
                          [identName ident_b]
                          [input_b]
                          ( ScremaForm
                              ( fromLines
                                  [ "\\ {x_3: i32} : {i32} ->",
                                    "let {y_4 : i32} = add32(2i32, x_3)",
                                    "in {y_4}"
                                  ]
                              )
                              []
                              []
                              ( fromLines
                                  [ "\\ {x_5 : i32} : {i32} -> {x_5}"
                                  ]
                              )
                          )
                          ["b_out_8"]
                      )
                  )
                  @?= Tuple3
                    ( Just
                        ( [input_a],
                          ScremaForm
                            ( fromLines
                                [ "\\ {x_0 : i32} : {i32, i32} ->",
                                  "let {y_1 : i32} = add32(1i32, x_0)",
                                  "let {y_10010 : i32} = add32(3i32, x_0)",
                                  "in {y_10010, y_1}"
                                ]
                            )
                            []
                            []
                            ( fromLines
                                [ "\\ {x_10013 : i32, x_10014 : i32} : {i32, i32} ->",
                                  "{x_10013, x_10014}"
                                ]
                            ),
                          ["b_out_8", identName ident_b]
                        )
                    ),
          testCase "red-red fusion" $
            let reduce_op' =
                  Reduce
                    Commutative
                    ( fromLines
                        [ "\\ {a_0 : i32, b_1 : i32} : {i32} ->",
                          "let {c_2 : i32} = add32(a_0, b_1)",
                          "in {c_2}"
                        ]
                    )
                    ["0i32"]
                reduce_op =
                  Reduce
                    Commutative
                    ( fromLines
                        [ "\\ {a_4 : f32, b_5 : f32} : {f32} ->",
                          "let {c_6 : f32} = fadd32(a_4, b_5)",
                          "in {c_6}"
                        ]
                    )
                    ["0.0f32"]
                ident_a = "input_a_7 : [d_9]i32"
                input_a = SOAC.identInput ident_a
                ident_b = "input_b_8 : [d_9]f32"
                input_b = SOAC.identInput ident_b
             in Tuple3
                  ( withFreshNamesScopeError
                      ( fuseScrema
                          "d_9"
                          [input_a]
                          ( ScremaForm
                              ( fromLines
                                  [ "\\ {x_11 : i32} : {i32} ->",
                                    "in {x_11}"
                                  ]
                              )
                              []
                              [reduce_op']
                              (fromLines ["nilFn"])
                          )
                          ["out_a_12"]
                          [input_b]
                          ( ScremaForm
                              ( fromLines
                                  [ "\\ {x_13 : f32} : {f32} ->",
                                    "in {x_13}"
                                  ]
                              )
                              []
                              [reduce_op]
                              (fromLines ["nilFn"])
                          )
                          ["out_b_14"]
                      )
                  )
                  @?= Tuple3
                    ( Just
                        ( [input_a, input_b],
                          ScremaForm
                        ( fromLines
                            [ "\\ {x_11 : i32, x_10000 : f32}: {i32, f32} ->",
                              "{x_11, x_10000}"
                            ]
                        )
                        []
                        [reduce_op', reduce_op]
                        ( fromLines ["nilFn"]),
                          ["out_a_12", "b_out_14"]
                        )
                    ),
          testCase "red-red fusion" $
            let reduce_op' =
                  Reduce
                    Commutative
                    ( fromLines
                        [ "\\ {a_0 : i32, b_1 : i32} : {i32} ->",
                          "let {c_2 : i32} = add32(a_0, b_1)",
                          "in {c_2}"
                        ]
                    )
                    ["0i32"]
                reduce_op =
                  Reduce
                    Commutative
                    ( fromLines
                        [ "\\ {a_4 : f32, b_5 : f32} : {f32} ->",
                          "let {c_6 : f32} = fadd32(a_4, b_5)",
                          "in {c_6}"
                        ]
                    )
                    ["0.0f32"]
                scan_op' =
                  Scan
                    ( fromLines
                        [ "\\ {a_7 : i64, b_8 : i64} : {i64} ->",
                          "let {c_9 : i64} = add64(a_7, b_8)",
                          "in {c_9}"
                        ]
                    )
                    ["0i64"]
                scan_op =
                  Scan
                    ( fromLines
                        [ "\\ {a_10 : f64, b_11 : f64} : {f64} ->",
                          "let {c_12 : f64} = fadd64(a_10, b_11)",
                          "in {c_12}"
                        ]
                    )
                    ["0.0f64"]
                ident_0_a = "input_a_13 : [d_9]i32"
                input_0_a = SOAC.identInput ident_0_a
                ident_0_b = "input_b_14 : [d_9]f32"
                input_0_b = SOAC.identInput ident_0_b
                ident_1_a = "input_a_15 : [d_9]i64"
                input_1_a = SOAC.identInput ident_1_a
                ident_1_b = "input_b_16 : [d_9]f64"
                input_1_b = SOAC.identInput ident_1_b
             in Tuple3
                  ( withFreshNamesScopeError
                      ( fuseScrema
                          "d_9"
                          [input_0_a, input_1_a]
                          ( ScremaForm
                              ( fromLines
                                  [ "\\ {x_17 : i32, x_18 : i64} : {i32, i64} ->",
                                    "in {x_17, x_18}"
                                  ]
                              )
                              [scan_op']
                              [reduce_op']
                              (fromLines
                                  [ "\\ {x_19 : i64} : {i64} ->",
                                   "in {x_19}"
                                  ])
                          )
                          ["out_a_0_20", "out_a_1_21"]
                          [input_0_b, input_1_b]
                          ( ScremaForm
                              ( fromLines
                                  [ "\\ {x_22 : f32, x_23 : f64} : {f32, f64} ->",
                                    "in {x_22, x_23}"
                                  ]
                              )
                              [scan_op]
                              [reduce_op]
                              (fromLines
                                  [ "\\ {x_24 : f64} : {f64} ->",
                                   "in {x_24}"
                                  ])
                          )
                          ["out_b_0_25", "out_b_1_26"]
                      )
                  )
                  @?= Tuple3
                    ( Just
                        ( [input_0_a, input_1_b],
                          ScremaForm
                        ( fromLines
                            [ "\\ {x_11 : i32, x_10000 : f32}: {i32, f32} ->",
                              "{x_11, x_10000}"
                            ]
                        )
                        []
                        [reduce_op', reduce_op]
                        ( fromLines ["nilFn"]),
                          ["out_a_12", "b_out_14"]
                        )
                    )
        ]
    ]
