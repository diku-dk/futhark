module Futhark.Optimise.Fusion.ScremaTests (tests) where

import Control.Monad.State
import Data.String (fromString)
import Futhark.Analysis.HORep.SOAC as SOAC
import Futhark.FreshNames
import Futhark.IR.SOACS
import Futhark.IR.SOACSTests ()
import Futhark.Optimise.Fusion.Screma
  ( SuperScrema (..),
    fuseSuperScrema,
    moveRedScanSuperScrema,
    splitLambdaByPar,
    splitLambdaByRes,
  )
import Test.Tasty
import Test.Tasty.HUnit

fromLines :: [String] -> Lambda SOACS
fromLines = fromString . unlines

splitLambdaByParTester :: [VName] -> Lambda SOACS -> (Lambda SOACS, Lambda SOACS)
splitLambdaByParTester names lam = (lam_x', lam_y')
  where
    ((_, lam_x', _), (_, lam_y', _)) =
      splitLambdaByPar names (lambdaParams lam) lam (lambdaReturnType lam)

freshNames :: State VNameSource a -> a
freshNames m = evalState m $ newNameSource 10000

-- | A wrapper that makes 'show' behave like 'prettyString'.
newtype SP a b = SP (a, b)
  deriving (Eq, Ord)

instance (Pretty a, Pretty b) => Show (SP a b) where
  show (SP (x, y)) = "(" <> prettyString x <> ",\n" <> prettyString y <> ")"

-- | A wrapper that makes 'pretty' behave like 'Show'.
newtype PS a = PS a
  deriving (Eq, Ord)

instance (Pretty a) => Show (PS a) where
  show (PS x) = prettyString x

tests :: TestTree
tests =
  testGroup
    "ScremaTests"
    [ testGroup
        "splitLambdaByPar"
        [ testCase "keeps params and result." $
            let lam = "\\{x_0 : i32, x_1 : i32} : {i32, i32} -> {x_0, x_1}"
                lam_x = "\\{x_0 : i32} : {i32} -> {x_0}"
                lam_y = "\\{x_1 : i32} : {i32} -> {x_1}"
                names = ["x_0"]
             in splitLambdaByParTester names lam @?= (lam_x, lam_y),
          testCase "keeps computation in first lambda." $
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
             in splitLambdaByParTester names lam @?= (lam_x, lam_y),
          testCase "keeps computations in both lambdas." $
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
             in splitLambdaByParTester names lam @?= (lam_x, lam_y),
          testCase "keeps line order." $
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
             in splitLambdaByParTester names lam @?= (lam_x, lam_y),
          testCase "does redundant work." $
            let lam =
                  fromLines
                    [ "\\{x_0 : i32, x_1 : i32} : {i32, i32} -> ",
                      "  let {x_2 : i32} = add32(1i32, x_0) ",
                      "  let {x_3 : i32} = add32(x_2, x_1) ",
                      "  in {x_3, x_2}"
                    ]
                lam_x =
                  fromLines
                    [ "\\{x_0 : i32, x_1 : i32} : {i32} -> ",
                      "  let {x_2 : i32} = add32(1i32, x_0) ",
                      "  let {x_3 : i32} = add32(x_2, x_1) ",
                      "  in {x_3}"
                    ]
                lam_y =
                  fromLines
                    [ "\\{x_0 : i32} : {i32} -> ",
                      "  let {x_2 : i32} = add32(1i32, x_0) ",
                      "  in {x_2}"
                    ]
                names = ["x_1"]
             in splitLambdaByParTester names lam @?= (lam_x, lam_y)
        ],
      testGroup
        "splitLambdaByRes"
        [ testCase "keeps params and result." $
            let lam = "\\{x_0 : i32, x_1 : i32} : {i32, i32} -> {x_0, x_1}"
                lam_x = "\\{x_0 : i32} : {i32} -> {x_0}"
                lam_y = "\\{x_1 : i32} : {i32} -> {x_1}"
                names = ["x_0"]
             in splitLambdaByRes names lam @?= (lam_x, lam_y),
          testCase "keeps computation in first lambda." $
            let lam =
                  fromLines
                    [ "\\{x_0 : i32, x_1 : i32} : {i32, i32} -> ",
                      "  let {x_2 : i32} = add32(x_0, x_1) ",
                      "  in {x_0, x_2}"
                    ]
                lam_x =
                  fromLines
                    [ "\\{x_0 : i32, x_1 : i32} : {i32} -> ",
                      "  let {x_2 : i32} = add32(x_0, x_1) ",
                      "  in {x_2}"
                    ]
                lam_y = "\\{x_0 : i32} : {i32} -> {x_0}"
                names = ["x_2"]
             in splitLambdaByRes names lam @?= (lam_x, lam_y),
          testCase "keeps computations in both lambdas." $
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
                names = ["x_2"]
             in splitLambdaByRes names lam @?= (lam_x, lam_y),
          testCase "keeps line order." $
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
                names = ["x_2"]
             in splitLambdaByRes names lam @?= (lam_x, lam_y),
          testCase "does redundant work." $
            let lam =
                  fromLines
                    [ "\\{x_0 : i32, x_1 : i32} : {i32, i32} -> ",
                      "  let {x_2 : i32} = add32(1i32, x_0) ",
                      "  let {x_3 : i32} = add32(x_2, x_1) ",
                      "  in {x_3, x_2}"
                    ]
                lam_x =
                  fromLines
                    [ "\\{x_0 : i32, x_1 : i32} : {i32} -> ",
                      "  let {x_2 : i32} = add32(1i32, x_0) ",
                      "  let {x_3 : i32} = add32(x_2, x_1) ",
                      "  in {x_3}"
                    ]
                lam_y =
                  fromLines
                    [ "\\{x_0 : i32} : {i32} -> ",
                      "  let {x_2 : i32} = add32(1i32, x_0) ",
                      "  in {x_2}"
                    ]
                names = ["x_3"]
             in splitLambdaByRes names lam @?= (lam_x, lam_y)
        ],
      testGroup
        "fuseScrema"
        [],
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
             in SP
                  ( freshNames
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
                  @?= SP
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
             in SP
                  ( freshNames
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
                  @?= SP
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
             in SP
                  ( freshNames
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
                  @?= SP
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
             in SP
                  ( freshNames
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
                  @?= SP
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
             in SP
                  ( freshNames
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
                  @?= SP
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
                    )
        ],
      testGroup
        "moveRedScanSuperScrema"
        [ testCase "Only map." $
            let ident_a = "input_a_5565 : [d_5537]i32"
                input_a = SOAC.identInput ident_a
             in PS
                  ( freshNames
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
                              []
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
                  @?= PS
                    ( SuperScrema
                        "d_5537"
                        [input_a]
                        ( fromLines
                            [ "\\ {x_5566 : i32}: {i32} ->",
                              "let {y_5567 : i32} = add32(1i32, x_5566)",
                              "in {y_5567}"
                            ]
                        )
                        []
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
             in PS
                  ( freshNames
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
                              []
                              []
                              ( fromLines
                                  [ "\\ {x_5568 : i32}: {i32} -> ",
                                    "let {y_5570 : i32} = add32(2i32, x_5568)",
                                    "in {y_5570}"
                                  ]
                              )
                              [scan_op]
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
                  @?= PS
                    ( SuperScrema
                        "d_5537"
                        [input_a]
                        ( fromLines
                            [ "\\ {x_5566 : i32}: {i32, i32} ->",
                              "let {y_5567 : i32} = add32(1i32, x_5566)",
                              "let {x_10000 : i32} = y_5567",
                              "let {y_10001 : i32} = add32(2i32, x_10000)",
                              "in {y_10001, y_5567}"
                            ]
                        )
                        [scan_op]
                        []
                        ( fromLines
                            [ "\\ {x_10002 : i32, x_5568 : i32}: {i32} -> ",
                              "in {x_10002}"
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
             in PS
                  ( freshNames
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
                  @?= PS
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
             in PS
                  ( freshNames
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
                              []
                              []
                              ( fromLines
                                  [ "\\ {x_5568 : i32}: {i32} -> ",
                                    "let {y_5570 : i32} = add32(2i32, x_5568)",
                                    "in {y_5570, y_5570}"
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
                  @?= PS
                    ( SuperScrema
                        "d_5537"
                        [input_a]
                        ( fromLines
                            [ "\\ {x_5566 : i32}: {i32, i32} ->",
                              "let {y_5567 : i32} = add32(1i32, x_5566)",
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
                              "let {y_5572 : i32} = add32(3i32, x_5571)",
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
             in PS
                  ( freshNames
                      ( moveRedScanSuperScrema
                          ( SuperScrema
                              "d_5537"
                              [input_a]
                              ( fromLines
                                  [ "\\ {x_5566 : i32}: {i32} ->",
                                    "let {y_5567 : i32} = add32(1i32, x_5566)",
                                    "in {x_5566, y_5567}"
                                  ]
                              )
                              []
                              [reduce_op']
                              ( fromLines
                                  [ "\\ {x_5568 : i32}: {i32} -> ",
                                    "let {y_5570 : i32} = add32(2i32, x_5568)",
                                    "in {y_5570, y_5570}"
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
                  @?= PS
                    ( SuperScrema
                        "d_5537"
                        [input_a]
                        ( fromLines
                            [ "\\ {x_5566 : i32}: {i32, i32} ->",
                              "let {y_5567 : i32} = add32(1i32, x_5566)",
                              "let {y_10001 : i32} = add32(2i32, x_10000)",
                              "in {y_10001, x_5566, y_10001, y_5567}"
                            ]
                        )
                        [scan_op]
                        [reduce_op', reduce_op]
                        ( fromLines
                            [ "\\ {x_10002 : i32, x_5568 : i32}: {i32} -> ",
                              "in {x_10002}"
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
        ]
    ]
