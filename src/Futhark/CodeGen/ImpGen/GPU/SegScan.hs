-- | Code generation for 'SegScan'.  Dispatches to either a
-- single-pass or two-pass implementation, depending on the nature of
-- the scan and the chosen abckend.
module Futhark.CodeGen.ImpGen.GPU.SegScan (compileSegScan) where

import Control.Monad
import Data.Maybe
import Futhark.CodeGen.ImpCode.GPU qualified as Imp
import Futhark.CodeGen.ImpGen hiding (compileProg)
import Futhark.CodeGen.ImpGen.GPU.Base
import Futhark.CodeGen.ImpGen.GPU.SegScan.SinglePass qualified as SinglePass
import Futhark.CodeGen.ImpGen.GPU.SegScan.TwoPass qualified as TwoPass
import Futhark.IR.GPUMem

-- The single-pass scan does not support multiple operators, so jam
-- them together here.
combineScans :: [SegBinOp GPUMem] -> SegBinOp GPUMem
combineScans ops =
  SegBinOp
    { segBinOpComm = mconcat (map segBinOpComm ops),
      segBinOpLambda = lam',
      segBinOpNeutral = concatMap segBinOpNeutral ops,
      segBinOpShape = mempty -- Assumed
    }
  where
    lams = map segBinOpLambda ops
    xParams lam = take (length (lambdaReturnType lam)) (lambdaParams lam)
    yParams lam = drop (length (lambdaReturnType lam)) (lambdaParams lam)
    lam' =
      Lambda
        { lambdaParams = concatMap xParams lams ++ concatMap yParams lams,
          lambdaReturnType = concatMap lambdaReturnType lams,
          lambdaBody =
            Body
              ()
              (mconcat (map (bodyStms . lambdaBody) lams))
              (concatMap (bodyResult . lambdaBody) lams)
        }

bodyAssert :: Body GPUMem -> Bool
bodyAssert = any (expAssert . stmExp) . bodyStms
  where
    expAssert (BasicOp Assert {}) = True
    expAssert e = isNothing $ walkExpM walker e
    walker =
      identityWalker
        { walkOnBody = const $ guard . not . bodyAssert
        }

canBeSinglePass :: [SegBinOp GPUMem] -> Maybe (SegBinOp GPUMem)
canBeSinglePass ops
  | all ok ops =
      Just $ combineScans ops
  | otherwise =
      Nothing
  where
    ok op =
      segBinOpShape op == mempty
        && all primType (lambdaReturnType (segBinOpLambda op))
        && not (bodyAssert (lambdaBody (segBinOpLambda op)))

-- | Compile 'SegScan' instance to host-level code with calls to
-- various kernels.
compileSegScan ::
  Pat LetDecMem ->
  SegLevel ->
  SegSpace ->
  [SegBinOp GPUMem] ->
  KernelBody GPUMem ->
  CallKernelGen ()
compileSegScan pat lvl space scans kbody = sWhen (0 .<. n) $ do
  emit $ Imp.DebugPrint "\n# SegScan" Nothing
  target <- hostTarget <$> askEnv
  case target of
    CUDA
      | Just scan' <- canBeSinglePass scans ->
          SinglePass.compileSegScan pat lvl space scan' kbody
    _ -> TwoPass.compileSegScan pat lvl space scans kbody
  emit $ Imp.DebugPrint "" Nothing
  where
    n = product $ map pe64 $ segSpaceDims space
