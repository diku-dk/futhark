module Futhark.Optimise.AlgSimplifyTests ( tests )
where

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

import Data.List
import Data.Loc
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M

import Futhark.InternalRep
import Futhark.Analysis.ScalExp
import Futhark.Analysis.ScalExpTests (parseScalExp')
import Futhark.Optimise.AlgSimplify

tests :: [Test]
tests = constantFoldTests ++ suffCondTests

constantFoldTests :: [Test]
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
  where vars = declareVars [("x", Int)]
        simplify'' e = simplify' vars e True []
        scalExp = parseScalExp' vars

        cfoldTest input expected =
          testCase ("constant-fold " ++ input) $
          simplify'' input @?= scalExp expected

suffCondTests :: [Test]
suffCondTests =
  [
    suffCondTest "5<n" [["False"]]
  , suffCondTest "0 <= i && i <= n-1" [["True"]]
  , suffCondTest "i-(m-1) <= 0" [["9<m"]]
  ]
  where suffsort = sort . map sort
        simplify'' e = simplify' vars e True ranges

        suffCondTest input expected =
          testCase ("sufficient conditions for " ++ input) $
          suffsort (mkSuffConds' vars input noLoc ranges) @?=
          suffsort (map (map simplify'') expected)

        vars = declareVars [ ("n", Int)
                           , ("m", Int)
                           , ("i", Int)
                           ]
        ranges = [ ("n", "10", "10")
                 , ("i", "0", "9")
                 ]

type RangesRep' = [(String, String, String)]

type VarDecls = [(String, BasicType)]

type VarInfo = M.Map String (Int, Type)

lookupVarName :: String -> VarInfo -> VName
lookupVarName s varinfo = case M.lookup s varinfo of
  Nothing    -> error $ "Unknown variable " ++ s
  Just (x,_) -> ID (nameFromString s, x)

declareVars :: VarDecls -> VarInfo
declareVars = M.fromList . snd . mapAccumL declare 0
  where declare i (name, t) = (i+1, (name, (i, Basic t)))

instantiateRanges :: VarInfo -> RangesRep' -> RangesRep
instantiateRanges varinfo r =
  HM.fromList $ snd $ mapAccumL fix 0 r
  where fix i (name, lower,upper) =
          (i+1,
           (lookupVarName name varinfo,
            (i, fixBound lower, fixBound upper)))
        fixBound "" = Nothing
        fixBound s  = Just $ parseScalExp' varinfo s

simplify' :: VarInfo -> String -> Bool -> RangesRep' -> ScalExp
simplify' varinfo s b r = case simplify e noLoc b r' of
  Left err -> error $ show err
  Right e' -> e'
  where e = parseScalExp' varinfo s
        r' = instantiateRanges varinfo r

mkSuffConds' :: VarInfo -> String -> SrcLoc -> RangesRep' -> [[ScalExp]]
mkSuffConds' varinfo s loc r =
  case simplify e loc True r' of
    Left err -> error $ show err
    Right e' ->
      case mkSuffConds e' loc r' of
        Left err -> error $ show err
        Right sc -> sc
  where e = parseScalExp' varinfo s
        r' = instantiateRanges varinfo r
