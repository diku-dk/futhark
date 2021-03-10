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
compileSegStencil pat lvl space stencilOp kbody = do
  let (is, dims) = unzip $ unSegSpace space
      dims' = map toInt64Exp dims
      num_groups' = toInt64Exp <$> segNumGroups lvl
      group_size' = toInt64Exp <$> segGroupSize lvl
      StencilOp stencil_is arrs lam' = stencilOp
      parameters = (lambdaParams lam')
      (invariantParams,variantParams) = splitAt ((length parameters) - (length arrs)) parameters
  

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

          zipWithM_ dPrimV_ is $
            map sExt64 $ unflattenIndex (map sExt64 dims') global_tid

          -- zipWithM dPrimV_ variantParams array[gtid + stencil_is_elem] 

          -- Imp.Call?


          --(\x y z -> x + y + z)

          --x <- 1
          -- ...
          --x + y + z

          sWhen (isActive $ unSegSpace space) $
            compileStms mempty (kernelBodyStms kbody) $
              zipWithM_ (compileThreadResult space) (patternElements pat) $
                kernelBodyResult kbody
    SegGroup {} ->
      error "not implemented"
