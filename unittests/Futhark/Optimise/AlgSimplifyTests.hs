module Futhark.Optimise.AlgSimplifyTests ( tests )
where

import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import qualified Data.Map.Strict as M

import Futhark.Representation.AST
import Futhark.Analysis.ScalExp
import Futhark.Analysis.ScalExpTests (parseScalExp')
import Futhark.Analysis.AlgSimplify

tests :: TestTree
tests = testGroup "AlgSimplifyTests" $ constantFoldTests ++ suffCondTests

constantFoldTests :: [TestTree]
constantFoldTests =
  [ cfoldTest "2+2" "4"
  , cfoldTest "2-2" "0"
  , cfoldTest "2*3" "6"
  , cfoldTest "6/3" "2"

    -- Simple cases over; let's try some variables.
  , cfoldTest "0+x" "x"
  , cfoldTest "x+x" "2*x" -- Sensitive to operand order
  , cfoldTest "x-0" "x"
  , cfoldTest "x-x" "0"
  , cfoldTest "x/x" "1"
  , cfoldTest "x/1" "x"
  , cfoldTest "x/x" "1"
  ]
  where vars = declareVars [("x", int32)]
        simplify'' e = simplify' vars e []
        scalExp = parseScalExp' vars

        cfoldTest input expected =
          testCase ("constant-fold " ++ input) $
          simplify'' input @?= scalExp expected

suffCondTests :: [TestTree]
suffCondTests =
  [
    suffCondTest "5<n" [["False"]]
  , suffCondTest "0 <= i && i <= n-1" [["True"]]
  , suffCondTest "i-(m-1) <= 0" [["9<m"]]
  ]
  where suffsort = sort . map sort
        simplify'' e = simplify' vars e ranges

        suffCondTest input expected =
          testCase ("sufficient conditions for " ++ input) $
          suffsort (mkSuffConds' vars input ranges) @?=
          suffsort (map (map simplify'') expected)

        vars = declareVars [ ("n", int32)
                           , ("m", int32)
                           , ("i", int32)
                           ]
        ranges = [ ("n", "10", "10")
                 , ("i", "0", "9")
                 ]

type RangesRep' = [(String, String, String)]

type VarDecls = [(String, PrimType)]

type VarInfo = M.Map String (Int, Type)

lookupVarName :: String -> VarInfo -> VName
lookupVarName s varinfo = case M.lookup s varinfo of
  Nothing    -> error $ "Unknown variable " ++ s
  Just (x,_) -> VName (nameFromString s) x

declareVars :: VarDecls -> VarInfo
declareVars = M.fromList . snd . mapAccumL declare 0
  where declare i (name, t) = (i+1, (name, (i, Prim t)))

instantiateRanges :: VarInfo -> RangesRep' -> RangesRep
instantiateRanges varinfo r =
  M.fromList $ snd $ mapAccumL fix 0 r
  where fix i (name, lower,upper) =
          (i+1,
           (lookupVarName name varinfo,
            (i, fixBound lower, fixBound upper)))
        fixBound "" = Nothing
        fixBound s  = Just $ parseScalExp' varinfo s

simplify' :: VarInfo -> String -> RangesRep' -> ScalExp
simplify' varinfo s r = simplify e r'
  where e = parseScalExp' varinfo s
        r' = instantiateRanges varinfo r

mkSuffConds' :: VarInfo -> String -> RangesRep' -> [[ScalExp]]
mkSuffConds' varinfo s r =
  case mkSuffConds e r' of
    Left _ -> [[e]]
    Right sc -> sc
  where e = simplify (parseScalExp' varinfo s) r'
        r' = instantiateRanges varinfo r
