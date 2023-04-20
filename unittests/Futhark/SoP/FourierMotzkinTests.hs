module Futhark.SoP.FourierMotzkinTests (tests) where

import Futhark.SoP.FourierMotzkin
import Futhark.SoP.Parse
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Solving inequalities with basic ranges"
    [ testCase "Ranges 1" $
        let sop = parseSoP "i*N + j - N*N"
            rs =
              parseRangeEnv
                [ "0 <= 2*i <= 2*N - 2",
                  "0 <= 2*j <= 2*N - 2",
                  "0 <= N"
                ]
         in fmSolveLTh0_ rs sop @?= True,
      testCase "Ranges 2" $
        let sop = parseSoP "i*N + j"
            rs =
              parseRangeEnv
                [ "0 <= i <= N - 1",
                  "0 <= j <= N - 1",
                  "0 <= N"
                ]
         in fmSolveGEq0_ rs sop @?= True
    ]
