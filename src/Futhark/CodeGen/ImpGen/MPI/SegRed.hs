module Futhark.CodeGen.ImpGen.MPI.SegRed
  ( compileSegRed,
    compileSegRed',
  )
where

import Control.Monad
import qualified Futhark.CodeGen.ImpCode.MPI as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.MPI.Base
import Futhark.IR.MCMem
import Prelude hiding (quot, rem)
import Debug.Trace

type DoSegBody = (([(SubExp, [Imp.TExp Int64])] -> MPIGen ()) -> MPIGen ())

-- | Generate code for a SegRed construct
compileSegRed ::
  Pattern MCMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  KernelBody MCMem ->
  MPIGen Imp.Code
compileSegRed pat space reds kbody
  | [(gtid, w)] <- unSegSpace space,
    [SegBinOp _ lam nes (Shape [])] <- reds = collect $ do
    traceShowM "abc"
    
    acc_vs <- forM (lambdaReturnType lam) $ \(Prim pt) ->
      fmap tvVar $ dPrim "acc" pt

    forM_ (zip acc_vs nes) $ \(acc_v, ne) -> do
      ne' <- toExp ne
      acc_v <~~ ne'

    sFor "i" (toInt64Exp w) $ \i -> do
      dPrimV_ gtid i
      compileStms mempty (kernelBodyStms kbody) $ do
        dLParams $ lambdaParams lam
        let (x_params, y_params) =
              splitAt (length nes) $ lambdaParams lam

        forM_ (zip x_params acc_vs) $ \(x_param, acc_v) ->
          copyDWIMFix (paramName x_param) [] (Var acc_v) []

        forM_ (zip y_params (kernelBodyResult kbody)) $ \(y_param, Returns _ se) ->
          copyDWIMFix (paramName y_param) [] se []

        compileStms mempty (bodyStms (lambdaBody lam)) $
          forM_ (zip acc_vs (bodyResult (lambdaBody lam))) $ \(acc_v, se) ->
            copyDWIMFix acc_v [] se []

    forM_ (zip (patternNames pat) acc_vs) $ \(v, acc_v) ->
      copyDWIMFix v [] (Var acc_v) []
compileSegRed _pat _space _reds _kbody = collect $ do emit Imp.Skip

-- | Like 'compileSegRed', but where the body is a monadic action.
compileSegRed' ::
  Pattern MCMem ->
  SegSpace ->
  [SegBinOp MCMem] ->
  TV Int32 ->
  DoSegBody ->
  MPIGen Imp.Code
compileSegRed' _pat _space _reds _nsubtasks _kbody =
  undefined