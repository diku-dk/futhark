{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Code generation for 'SegStencil' is quite straightforward.
module Futhark.CodeGen.ImpGen.Kernels.SegStencil (compileSegStencil) where

import Control.Monad.Except
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Kernels.Base
import Futhark.IR.KernelsMem
import Futhark.Util.IntegralExp (divUp)
import Prelude hiding (quot, rem)
import Data.List (transpose)

-- | Compile 'SegMap' instance code.
compileSegStencil ::
  Pattern KernelsMem ->
  SegLevel ->
  SegSpace ->
  StencilOp KernelsMem ->
  KernelBody KernelsMem ->
  CallKernelGen ()
compileSegStencil pat lvl space op kbody = do
  let (is, dims) = unzip $ unSegSpace space
      dims' = map toInt64Exp dims
      num_groups' = toInt64Exp <$> segNumGroups lvl
      group_size' = toInt64Exp <$> segGroupSize lvl
      stencil_ixss = map (map fromInteger) $ stencilIndexes op
      invarElems = map kernelResultSubExp $ kernelBodyResult kbody
      lam = stencilOp op
      lamPars = lambdaParams lam
      lamBody = lambdaBody lam
      lamSplitPoint = length invarElems
      (invariantParams,variantParams) =
        splitAt lamSplitPoint lamPars

  case lvl of
    SegThread {} -> do
      emit $ Imp.DebugPrint "\n# SegStencil" Nothing
      let virt_num_groups =
            sExt32 $ product dims' `divUp` unCount group_size'
      sKernelThread "segstencil" num_groups' group_size' (segFlat space) $
        virtualiseGroups (segVirt lvl) virt_num_groups $ \group_id -> do
          local_tid <- kernelLocalThreadId . kernelConstants <$> askEnv
          let global_tid =
                sExt64 group_id * sExt64 (unCount group_size')
                  + sExt64 local_tid

          -- create global ids for each axis (to check for out of bounds)
          let gid_flat = map sExt64 $ unflattenIndex (map sExt64 dims') global_tid
          zipWithM_ dPrimV_ is gid_flat

          -- check for out of bound on global id for each axis
          sWhen (isActive $ unSegSpace space) $ do
            -- compile invariant elements
            compileStms mempty (kernelBodyStms kbody) $ pure ()

            -- declare and attach invariant elements to invariantParams
            zipWithM_ dPrimV_ (map paramName invariantParams)
                =<< (map TPrimExp <$> mapM toExp invarElems)

            -- calculate the unrolled variants parameter indexers
            let bounded_ixs = flip map (zip3 gid_flat stencil_ixss dims') $
                  \(axis_gid, axis_ixs, axis_len) -> flip map axis_ixs $
                      \oi -> sMin64 (axis_len - 1) (sMax64 0 (axis_gid + oi))
            let vname_ixs_for_tup =
                  concatMap (\vn -> map ((,) vn) (transpose bounded_ixs)) $ stencilArrays op

            -- declare lambda variant parameters
            dLParams variantParams

            -- load variants into lambda variant parameters
            forM_ (zip variantParams vname_ixs_for_tup) $
              \(vparam, (input_arr, ixs_tup)) -> do
                  let pname = paramName vparam
                  copyDWIMFix pname [] (Var input_arr) ixs_tup

            -- compile lambda function and designate output style
            compileStms mempty (bodyStms lamBody) $
              zipWithM_ (compileThreadResult space) (patternElements pat) $
                map (Returns ResultMaySimplify) $ bodyResult lamBody

    SegGroup {} ->
      error "not implemented"
