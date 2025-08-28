module Futhark.Analysis.DataDependenciesTests (tests) where

import Data.Map qualified as M
import Futhark.Analysis.DataDependencies
import Futhark.IR.SOACS
import Futhark.IR.SOACSTests ()
import Test.Tasty
import Test.Tasty.HUnit

names :: [VName] -> Names
names = namesFromList

tests :: TestTree
tests =
  testGroup
    "DataDependenciesTests"
    [ testGroup
        "lambdaDependencies"
        [ testCase "if" $
            lambdaDependencies
              mempty
              ("\\{x_0: i32} : {i32} -> {x_0}" :: Lambda SOACS)
              [names ["y_1"]]
              @?= [names ["y_1"]],
          testCase "flip" $
            lambdaDependencies
              mempty
              ("\\{x_0: i32, x_1: bool} : {bool, i32} -> {x_1, x_0}" :: Lambda SOACS)
              [names ["y_2"], names ["y_3"]]
              @?= [names ["y_3"], names ["y_2"]],
          testCase "add" $
            lambdaDependencies
              mempty
              ("\\{x_0: i32, x_1: i32} : {i32} -> let {x_2: i32} = add32(x_1, x_0) in {x_2}" :: Lambda SOACS)
              [names ["y_2"], names ["y_3"]]
              @?= [names ["y_3", "y_2"]],
          testCase "outer" $
            lambdaDependencies
              (M.fromList [("x_1", names ["x_4"])])
              ("\\{x_0: i32} : {i32} -> let {x_2: i32} = add32(x_1, x_0) in {x_2}" :: Lambda SOACS)
              [names ["y_2"]]
              @?= [names ["y_2", "x_1"]]
        ]
    ]
