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
import Futhark.Test.Spec qualified as Spec

generateTests :: (MonadFreshNames m, MonadIO m) =>
  FilePath -> F.Prog F.GPUMem -> m T.Text
generateTests path prog = do
  compiled <- snd <$> compileProg prog
  spec <- liftIO $ Spec.testSpecFromProgramOrDie (path <> ".fut")
  let specText = prettyText $ show spec
  let wgslText = webgpuProgram compiled
  pure (specText <> "\n\n" <> wgslText)
