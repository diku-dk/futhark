module Futhark.SoP.SoPTests (tests) where

import Futhark.SoP.Parse
import Futhark.SoP.SoP
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Arithmetic tests"
    $ let sop1 = parseSoP "x + y"
          sop2 = parseSoP "5 + x*x + 2*x + 3*y + y*y"
          sop3 = parseSoP "-3 + x + y"
       in [ testCase "Addition" $
              addSoPs sop1 sop2 @?= parseSoP "5 + x*x + 3*x + 4*y + y*y",
            testCase "Multiplication 1" $
              mulSoPs sop1 sop2
                @?= parseSoP
                  "5*x + x*x*x + 2*x*x + 3*y*x + y*y*x + 5*y + x*x*y + 2*x*y + 3*y*y + y*y*y",
            testCase "Multiplication 2" $
              mulSoPs sop1 sop2
                @?= parseSoP
                  "(x+y) * (5 + x*x + 2*x + 3*y + y*y)",
            testCase "Negation 1" $
              negSoP sop3
                @?= parseSoP
                  "3 - x - y",
            testCase "Negation 2" $
              negSoP (negSoP sop3) @?= sop3
          ]
