module Futhark.SoP.FourierMotzkinTests (tests) where

import Futhark.Analysis.PrimExp
import Futhark.SoP.FourierMotzkin
import Futhark.SoP.Monad
import Futhark.SoP.Parse
import Futhark.SoP.SoP
import Test.Tasty
import Test.Tasty.HUnit

fmSolveLTh0_ :: RangeEnv String -> SoP String -> Bool
fmSolveLTh0_ rs = evalSoPM mempty {ranges = rs} . (fmSolveLTh0 :: SoP String -> SoPM String (PrimExp String) p Bool)

fmSolveGTh0_ :: RangeEnv String -> SoP String -> Bool
fmSolveGTh0_ rs = evalSoPM mempty {ranges = rs} . (fmSolveGTh0 :: SoP String -> SoPM String (PrimExp String) p Bool)

fmSolveGEq0_ :: RangeEnv String -> SoP String -> Bool
fmSolveGEq0_ rs = evalSoPM mempty {ranges = rs} . (fmSolveGEq0 :: SoP String -> SoPM String (PrimExp String) p Bool)

fmSolveLEq0_ :: RangeEnv String -> SoP String -> Bool
fmSolveLEq0_ rs = evalSoPM mempty {ranges = rs} . (fmSolveLEq0 :: SoP String -> SoPM String (PrimExp String) p Bool)

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
