module Futhark.Test.WebGPUTest 
  ( generateTests
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text qualified as T
import Data.Map qualified as M
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
  let info = kernelInfoLiteral compiled
  let shader = shaderLiteral compiled
  pure (tests <> "\n\n" <> info <> "\n\n" <> shader)

shaderLiteral :: Program -> T.Text
shaderLiteral prog = "window.shader = `\n"
  <> webgpuPrelude prog
  <> "\n"
  <> webgpuProgram prog
  <> "\n`;"

-- window.kernels = [
--   { name: 'some_vname_5568',
--     overrides: ['override', 'declarations'],
--     scalarsBindSlot: 0,
--     bindSlots: [1, 2, 3],
--   },
-- ];
kernelInfoLiteral :: Program -> T.Text
kernelInfoLiteral prog = "window.kernels = " <> docText fmtInfos <> ";"
  where
    infos = M.toList $ webgpuKernels prog
    fmtInfos = "[" </> indent 2 (commastack $ map fmtInfo infos) </> "]"
    fmtInfo (name, ki) =
      "{" </> indent 2 (
        "name: '" <> pretty name <> "',"
        </> "overrides: [" <> commasep
          (map (\o -> "'" <> pretty o <> "'") (overrideNames ki)) <> "],"
        </> "scalarsBindSlot: " <> pretty (scalarsBindSlot ki) <> ","
        </> "bindSlots: " <> pretty (memBindSlots ki) <> ","
      ) </> "}"

-- window.tests = [
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
testCasesLiteral :: ProgramTest -> T.Text
testCasesLiteral (ProgramTest _ _ (RunCases ios _ _)) =
  let specs = map ((<> ",\n") . prettyText . mkTestSpec) ios
   in "window.tests = [\n" <> foldl' (<>) "" specs <> "];"
testCasesLiteral t = "// Unsupported test: " <> testDescription t

data JsTestSpec = JsTestSpec 
  { jsEntryPoint :: T.Text,
    jsRuns :: [JsTestRun]
  }
data JsTestRun = JsTestRun
  { jsInputTypes :: [V.PrimType],
    jsInput :: [V.Value],
    jsExpectedTypes :: [V.PrimType],
    jsExpected :: [V.Value]
  }

mkTestSpec :: InputOutputs -> JsTestSpec
mkTestSpec (InputOutputs entry runs) = JsTestSpec entry (mapMaybe mkRun runs)

mkRun :: TestRun -> Maybe JsTestRun
mkRun (TestRun _ (Values vals)
                 (Succeeds (Just (SuccessValues (Values expected)))) _ _) =
  let inputTyps = map V.valueElemType vals
      expectedTyps = map V.valueElemType expected
   in Just $ JsTestRun inputTyps vals expectedTyps expected
mkRun _ = Nothing

instance Pretty JsTestRun where
  pretty (JsTestRun inputTyps input expectedTyps expected) =
    "{" </> indent 2 (
      "inputTypes: ["
        <> commasep (map (\t -> "'" <> pretty (V.primTypeText t) <> "'")
            inputTyps)
        <> "],"
      </> "input: " <> fmt inputTyps input <> ","
      </> "expectedTypes: [" 
        <> commasep (map (\t -> "'" <> pretty (V.primTypeText t) <> "'")
            expectedTyps)
        <> "],"
      </> "expected: " <> fmt expectedTyps expected <> ","
    ) </> "}"
    where
      fmtVal V.I64 v = pretty v <> "n"
      fmtVal V.U64 v = pretty v <> "n"
      fmtVal _ v = pretty v
      fmtArrRaw typ vs = "[" <> commasep (map (fmtVal typ) vs) <> "]"
      -- Hacky way to avoid the 'i32', 'i64' etc. suffixes as they are not valid
      -- JS.
      fixAnnots typ d = pretty $ T.replace (V.primTypeText typ) "" (docText d)
      fixSpecials d = pretty $
        T.replace ".nan" "NaN" $ T.replace ".inf" "Infinity" $ docText d
      fmtArray typ vs = fixSpecials $ fixAnnots typ $
        fmtArrRaw typ (V.valueElems vs)
      fmt typs vss = "[" <> commasep (zipWith fmtArray typs vss) <> "]"

instance Pretty JsTestSpec where
  pretty (JsTestSpec entry runs) =
    "{" </> indent 2 (
      "entry: '" <> pretty entry <> "',"
      </> "runs: ["
      </> indent 2 (commastack $ map pretty runs)
      </> "],"
    ) </> "}"
