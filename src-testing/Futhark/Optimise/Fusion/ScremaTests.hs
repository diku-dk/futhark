module Futhark.Optimise.Fusion.ScremaTests (tests) where

import Data.String (fromString)
import Futhark.Analysis.HORep.SOAC as SOAC
import Futhark.IR.SOACS
import Futhark.IR.SOACSTests ()
import Futhark.Optimise.Fusion.Screma (fuseLambda, splitLambdaByPar, splitLambdaByRes)
import Test.Tasty
import Test.Tasty.HUnit

fromLines :: [String] -> Lambda SOACS
fromLines = fromString . unlines

tests :: TestTree
tests =
  testGroup
    "ScremaTests"
    [ testGroup
        "FuseScrema"
        [],
      -- testCase "map-map" $
      --   let lam_c = "\\{x_0 : i32} : {i32} -> {x_0}"
      --       inp_c = [SOAC.Input mempty "xs_1" "[10i64]i32"]
      --       out_c = ["ys_3"]
      --       lam_p = "\\{x_2 : i32} : {i32} -> {x_2}"
      --       out_p = ["zs_4"]
      --    in fuseLambda lam_c inp_c out_c lam_p out_p
      --         @?= Just
      --           ( [SOAC.Input mempty "xs_1" "[10i64]i32"],
      --             "\\{x_2 : i32, x_0 : i32} : {i32,i32} -> {x_2, x_0}",
      --             ["zs_4", "ys_3"]
      --           )

      testGroup
        "splitLambdaByPar"
        [ testCase "keeps params and result." $
            let lam = "\\{x_0 : i32, x_1 : i32} : {i32, i32} -> {x_0, x_1}"
                lam_x = "\\{x_0 : i32} : {i32} -> {x_0}"
                lam_y = "\\{x_1 : i32} : {i32} -> {x_1}"
                names = ["x_0"]
             in splitLambdaByPar names lam @?= (lam_x, lam_y),
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
             in splitLambdaByPar names lam @?= (lam_x, lam_y),
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
             in splitLambdaByPar names lam @?= (lam_x, lam_y),
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
             in splitLambdaByPar names lam @?= (lam_x, lam_y),
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
             in splitLambdaByPar names lam @?= (lam_x, lam_y)
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
        ]
    ]
