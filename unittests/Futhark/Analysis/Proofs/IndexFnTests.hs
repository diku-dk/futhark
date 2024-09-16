module Futhark.Analysis.Proofs.IndexFnTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Futhark.Analysis.Proofs.IndexFn
import Futhark.Compiler.CLI (readProgramOrDie, fileProg, Imports)
import Futhark.Analysis.Proofs.Convert
import qualified Language.Futhark as E
import Data.Maybe (mapMaybe)
import Futhark.Analysis.Proofs.Symbol (Symbol (..))
import Futhark.SoP.SoP (int2SoP)
import Debug.Trace (traceM, trace)
import Futhark.Util.Pretty (prettyString)

-- Doubly last: get the last ValBind in the last import.
getLastValBind :: Imports -> E.ValBind
getLastValBind imports = case reverse imports of
  [] -> error "No imports"
  finalImport:_  ->
    last . mapMaybe getValBind . E.progDecs . fileProg . snd $ finalImport
  where
    getValBind (E.ValDec vb) = Just vb
    getValBind _ = Nothing

tests :: TestTree
tests = testGroup "Proofs.IndexFn"
  [ mkTest
      "tests/indexfn/map.fut"
      (IndexFn Empty (cases [(Bool True, int2SoP 0)]))
  ]
  where
    mkTest :: String -> IndexFn -> TestTree
    mkTest programFile expected = testCase programFile $ do
      (_, imports, vns) <- readProgramOrDie programFile
      let vb = getLastValBind imports
      -- traceM $ "valbind " <> prettyString vb
      let indexfn = fst . flip runIndexFnM vns . mkIndexFnValBind $ vb
      indexfn @?= Just expected
