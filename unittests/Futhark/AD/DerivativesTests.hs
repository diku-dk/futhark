module Futhark.AD.DerivativesTests (tests) where

import qualified Data.Map as M
import Futhark.AD.Derivatives
import Futhark.Analysis.PrimExp
import Futhark.IR.Syntax.Core (nameFromString)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Futhark.AD.DerivativesTests"
    [ testGroup "Primitive functions" $
        map primFunTest $
          filter (not . (`elem` missing_primfuns) . fst) $ M.toList primFuns
    ]
  where
    primFunTest (f, (ts, ret, _)) =
      testCase f $
        case pdBuiltin (nameFromString f) (map (ValueExp . blankPrimValue) ts) of
          Nothing -> assertFailure "pdBuiltin gives Nothing"
          Just v -> map primExpType v @?= replicate (length ts) ret

    -- We know we have no derivatives for these... and they are not
    -- coming any time soon.
    missing_primfuns =
      [ "gamma16",
        "gamma32",
        "gamma64",
        "lgamma16",
        "lgamma32",
        "lgamma64"
      ]
