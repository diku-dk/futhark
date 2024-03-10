module Futhark.Test.WebGPUTest 
  ( generateTests
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text qualified as T
import Futhark.CodeGen.ImpCode.WebGPU
import Futhark.CodeGen.ImpGen.WebGPU (compileProg)
import Futhark.IR.GPUMem qualified as F
import Futhark.MonadFreshNames
import Futhark.Test.Spec
import Futhark.Test.Values qualified as V
import Futhark.Util.Pretty
import Data.Maybe (mapMaybe)
import Data.List (foldl')

generateTests :: (MonadFreshNames m, MonadIO m) =>
  FilePath -> F.Prog F.GPUMem -> m T.Text
generateTests path prog = do
  compiled <- snd <$> compileProg prog
  spec <- liftIO $ testSpecFromProgramOrDie (path <> ".fut")
  let tests = testCasesLiteral spec
  let shader = shaderLiteral compiled
  pure (tests <> "\n\n" <> shader)

shaderLiteral :: Program -> T.Text
shaderLiteral prog = "const shader = `\n" <> webgpuProgram prog <> "\n`;"

testCasesLiteral :: ProgramTest -> T.Text
testCasesLiteral (ProgramTest _ _ (RunCases ios _ _)) =
  let specs = map ((<> ",\n") . prettyText . mkTestSpec) ios
   in "const tests = [\n" <> foldl' (<>) "" specs <> "];"
testCasesLiteral t = "// Unsupported test: " <> testDescription t


-- const tests = [
--   { entry: 'someName',
--     runs: [
--       {
--         inputTypes: ['i32'],
--         input: [[0, 1, 2, 3]],
--         expected: [[0, 2, 4, 6]],
--       },
--     ],
--   },
-- ];
data JsTestSpec = JsTestSpec 
  { jsEntryPoint :: T.Text,
    jsRuns :: [JsTestRun]
  }
data JsTestRun = JsTestRun
  { jsInputTypes :: [V.PrimType],
    jsInput :: [V.Value],
    jsExpected :: [V.Value]
  }

mkTestSpec :: InputOutputs -> JsTestSpec
mkTestSpec (InputOutputs entry runs) = JsTestSpec entry (mapMaybe mkRun runs)

mkRun :: TestRun -> Maybe JsTestRun
mkRun (TestRun _ (Values vals)
                 (Succeeds (Just (SuccessValues (Values expect)))) _ _) =
  let typs = map V.valueElemType vals
   in Just $ JsTestRun typs vals expect
mkRun _ = Nothing

instance Pretty JsTestRun where
  pretty (JsTestRun typs input expected) =
    "{" </> indent 2 (
      "type: ["
        <> commasep (map (\t -> "'" <> pretty (V.primTypeText t) <> "'") typs)
        <> "],"
      </> "input: " <> pretty input <> ","
      </> "expected: " <> pretty expected <> ","
    ) </> "}"

instance Pretty JsTestSpec where
  pretty (JsTestSpec entry runs) =
    "{" </> indent 2 (
      "entry: '" <> pretty entry <> "',"
      </> "runs: ["
      </> indent 2 (commastack $ map pretty runs)
      </> "],"
    ) </> "}"
