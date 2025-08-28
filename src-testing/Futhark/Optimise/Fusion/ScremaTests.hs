module Futhark.Optimise.Fusion.ScremaTests (tests) where

import Futhark.Analysis.HORep.SOAC as SOAC
import Futhark.IR.SOACSTests ()
import Futhark.Optimise.Fusion.Screma (fuseLambda)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "FuseScrema"
    [ testCase "map-map" $
        let lam_c = "\\{x_0 : i32} : {i32} -> {x_0}"
            inp_c = [SOAC.Input mempty "xs_1" "[10i64]i32"]
            out_c = ["ys_3"]
            lam_p = "\\{x_2 : i32} : {i32} -> {x_2}"
            out_p = ["zs_4"]
         in fuseLambda lam_c inp_c out_c lam_p out_p
              @?= Just
                ( [SOAC.Input mempty "xs_1" "[10i64]i32"],
                  "\\{x_2 : i32, x_0 : i32} : {i32,i32} -> {x_2, x_0}",
                  ["zs_4", "ys_3"]
                )
    ]
