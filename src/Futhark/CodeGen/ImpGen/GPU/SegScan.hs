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
combineScanOps :: [SegScanOp GPUMem] -> SegScanOp GPUMem
combineScanOps ops =
  SegScanOp
    { segScanOpLambda = lam',
      segScanOpShape = mempty -- Assumed
    }
  where
    splitParams op = splitAt (segScanOpArity op) $ lambdaParams $ segScanOpLambda op
    (allXParams, allYParams) = unzip $ map splitParams ops
    lam' =
      Lambda
        { lambdaParams = concat allXParams ++ concat allYParams,
          lambdaReturnType = concatMap (lambdaReturnType . segScanOpLambda) ops,
          lambdaBody =
            Body
              ()
              (mconcat (map (bodyStms . lambdaBody . segScanOpLambda) ops))
              (concatMap (bodyResult . lambdaBody . segScanOpLambda) ops)
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

canBeSinglePass :: [SegScanOp GPUMem] -> Maybe (SegScanOp GPUMem)
canBeSinglePass scan_ops =
  if all ok scan_ops
    then Just $ combineScanOps scan_ops
    else Nothing
  where
    ok op =
      segScanOpShape op == mempty
        && all primType (lambdaReturnType lam)
        && not (bodyHas isAssert (lambdaBody lam))
      where
        lam = segScanOpLambda op
    isAssert (BasicOp Assert {}) = True
    isAssert _ = False

-- | Compile 'SegScan' instance to host-level code with calls to
-- various kernels.
compileSegScan ::
  Pat LetDecMem ->
  SegLevel ->
  SegSpace ->
  [Type] ->
  KernelBody GPUMem ->
  [SegScanOp GPUMem] ->
  SegPostOp GPUMem ->
  CallKernelGen ()
compileSegScan pat lvl space ts map_kbody scan_ops post_op =
  sWhen (0 .<. n) $ do
    emit $ Imp.DebugPrint "\n# SegScan" Nothing
    target <- hostTarget <$> askEnv

    case (targetSupportsSinglePass target, canBeSinglePass scan_ops) of
      (True, Just scan_ops') ->
        SinglePass.compileSegScan pat lvl space ts scan_ops' map_kbody post_op
      _ ->
        TwoPass.compileSegScan pat lvl space ts scan_ops map_kbody post_op
    emit $ Imp.DebugPrint "" Nothing
  where
    n = product $ map pe64 $ segSpaceDims space
    targetSupportsSinglePass CUDA = True
    targetSupportsSinglePass HIP = True
    targetSupportsSinglePass _ = False
