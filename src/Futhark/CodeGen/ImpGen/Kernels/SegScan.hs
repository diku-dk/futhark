module Futhark.CodeGen.ImpGen.Kernels.SegScan (compileSegScan) where

import Futhark.CodeGen.ImpGen hiding (compileProg)
import Futhark.CodeGen.ImpGen.Kernels.Base
import qualified Futhark.CodeGen.ImpGen.Kernels.SegScan.SinglePass as SinglePass
import qualified Futhark.CodeGen.ImpGen.Kernels.SegScan.TwoPass as TwoPass
import Futhark.IR.KernelsMem

-- The single-pass scan does not support multiple operators, so jam
-- them together here.
combineScans :: [SegBinOp KernelsMem] -> SegBinOp KernelsMem
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

canBeSinglePass :: [SegBinOp KernelsMem] -> Maybe (SegBinOp KernelsMem)
canBeSinglePass ops
  | all ok ops =
    Just $ combineScans ops
  | otherwise =
    Nothing
  where
    ok op =
      segBinOpShape op == mempty
        && all primType (lambdaReturnType (segBinOpLambda op))

-- | Compile 'SegScan' instance to host-level code with calls to
-- various kernels.
compileSegScan ::
  Pattern KernelsMem ->
  SegLevel ->
  SegSpace ->
  [SegBinOp KernelsMem] ->
  KernelBody KernelsMem ->
  CallKernelGen ()
compileSegScan pat lvl space scans kbody = do
  target <- hostTarget <$> askEnv
  case target of
    CUDA
      | Just scan' <- canBeSinglePass scans ->
        SinglePass.compileSegScan pat lvl space [scan'] kbody
    _ -> TwoPass.compileSegScan pat lvl space scans kbody
