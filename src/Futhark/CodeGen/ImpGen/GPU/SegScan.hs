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
combineScanOps :: [SegBinOp GPUMem] -> SegBinOp GPUMem
combineScanOps ops =
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

bodyHas :: (Exp GPUMem -> Bool) -> Body GPUMem -> Bool
bodyHas f = any (f' . stmExp) . bodyStms
  where
    f' e
      | f e = True
      | otherwise = isNothing $ walkExpM walker e
    walker =
      identityWalker
        { walkOnBody = const $ guard . not . bodyHas f
        }

canBeSinglePass :: [SegBinOp GPUMem] -> Maybe (SegBinOp GPUMem)
canBeSinglePass scan_ops =
  if all ok scan_ops
    then Just $ combineScanOps scan_ops
    else Nothing
  where
    ok op =
      segBinOpShape op == mempty
        && all primType (lambdaReturnType (segBinOpLambda op))
        && not (bodyHas isAssert (lambdaBody (segBinOpLambda op)))
    isAssert (BasicOp Assert {}) = True
    isAssert _ = False

-- | Compile 'SegScan' instance to host-level code with calls to
-- various kernels.
compileSegScan ::
  Pat LetDecMem ->
  SegLevel ->
  SegSpace ->
  KernelBody GPUMem ->
  [SegBinOp GPUMem] ->
  SegPostOp GPUMem ->
  CallKernelGen ()
compileSegScan pat lvl space map_kbody scan_ops post_op =
  sWhen (0 .<. n) $ do
    emit $ Imp.DebugPrint "\n# SegScan" Nothing
    target <- hostTarget <$> askEnv

    case (targetSupportsSinglePass target, canBeSinglePass scan_ops) of
      (True, Just scan_ops') ->
        SinglePass.compileSegScan pat lvl space scan_ops' map_kbody
      _ ->
        TwoPass.compileSegScan pat lvl space scan_ops map_kbody
    emit $ Imp.DebugPrint "" Nothing
  where
    n = product $ map pe64 $ segSpaceDims space
    targetSupportsSinglePass CUDA = True
    targetSupportsSinglePass HIP = True
    targetSupportsSinglePass _ = False
