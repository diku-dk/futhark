{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Facilities for converting a 'Kernels' program to 'KernelsMem'.
module Futhark.Pass.ExplicitAllocations.Kernels
       ( explicitAllocations
       , explicitAllocationsInStms
       )
where

import qualified Data.Map as M
import qualified Data.Set as S

import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.IR.KernelsMem
import Futhark.IR.Kernels
import Futhark.Pass.ExplicitAllocations
import Futhark.Pass.ExplicitAllocations.SegOp

instance SizeSubst (HostOp lore op) where
  opSizeSubst (Pattern _ [size]) (SizeOp (SplitSpace _ _ _ elems_per_thread)) =
    M.singleton (patElemName size) elems_per_thread
  opSizeSubst _ _ = mempty

  opIsConst (SizeOp GetSize{}) = True
  opIsConst (SizeOp GetSizeMax{}) = True
  opIsConst _ = False

instance SizeSubst (SegOp lvl lore) where
  opSizeSubst _ _ = mempty

allocAtLevel :: SegLevel -> AllocM fromlore tlore a -> AllocM fromlore tlore a
allocAtLevel lvl = local $ \env -> env { allocSpace = space
                                       , aggressiveReuse = True
                                       }
  where space = case lvl of SegThread{} -> DefaultSpace
                            SegGroup{} -> Space "local"

handleSegOp :: SegOp SegLevel Kernels
            -> AllocM Kernels KernelsMem (SegOp SegLevel KernelsMem)
handleSegOp op = do
  num_threads <- letSubExp "num_threads" $ BasicOp $ BinOp (Mul Int32 OverflowUndef)
                 (unCount (segNumGroups lvl)) (unCount (segGroupSize lvl))
  allocAtLevel lvl $ mapSegOpM (mapper num_threads) op
  where scope = scopeOfSegSpace $ segSpace op
        lvl = segLevel op
        mapper num_threads =
          identitySegOpMapper
          { mapOnSegOpBody =
              localScope scope . local f . allocInKernelBody
          , mapOnSegOpLambda =
              local inThread .
              allocInBinOpLambda num_threads (segSpace op)
          }
        f = case segLevel op of SegThread{} -> inThread
                                SegGroup{} -> inGroup
        inThread env = env { envExpHints = inThreadExpHints }
        inGroup env = env { envExpHints = inGroupExpHints }

handleHostOp :: HostOp Kernels (SOAC Kernels)
             -> AllocM Kernels KernelsMem (MemOp (HostOp KernelsMem ()))
handleHostOp (SizeOp op) =
  return $ Inner $ SizeOp op
handleHostOp (OtherOp op) =
  error $ "Cannot allocate memory in SOAC: " ++ pretty op
handleHostOp (SegOp op) =
  Inner . SegOp <$> handleSegOp op

kernelExpHints :: Allocator KernelsMem m => Exp KernelsMem -> m [ExpHint]
kernelExpHints (BasicOp (Manifest perm v)) = do
  dims <- arrayDims <$> lookupType v
  let perm_inv = rearrangeInverse perm
      dims' = rearrangeShape perm dims
      ixfun = IxFun.permute (IxFun.iota $ map (primExpFromSubExp int32) dims')
              perm_inv
  return [Hint ixfun DefaultSpace]

kernelExpHints (Op (Inner (SegOp (SegMap lvl@SegThread{} space ts body)))) =
  zipWithM (mapResultHint lvl space) ts $ kernelBodyResult body

kernelExpHints (Op (Inner (SegOp (SegRed lvl@SegThread{} space reds ts body)))) =
  (map (const NoHint) red_res <>) <$> zipWithM (mapResultHint lvl space) (drop num_reds ts) map_res
  where num_reds = segBinOpResults reds
        (red_res, map_res) = splitAt num_reds $ kernelBodyResult body

kernelExpHints e =
  return $ replicate (expExtTypeSize e) NoHint

mapResultHint :: Allocator lore m =>
                 SegLevel -> SegSpace -> Type -> KernelResult -> m ExpHint
mapResultHint lvl space = hint
  where num_threads = primExpFromSubExp int32 (unCount $ segNumGroups lvl) *
                      primExpFromSubExp int32 (unCount $ segGroupSize lvl)

        -- Heuristic: do not rearrange for returned arrays that are
        -- sufficiently small.
        coalesceReturnOfShape _ [] = False
        coalesceReturnOfShape bs [Constant (IntValue (Int32Value d))] = bs * d > 4
        coalesceReturnOfShape _ _ = True

        hint t Returns{}
          | ElemPrim pt <- elemType t,
            coalesceReturnOfShape (primByteSize pt) $ arrayDims t = do
              let space_dims = segSpaceDims space
              t_dims <- mapM dimAllocationSize $ arrayDims t
              return $ Hint (innermost space_dims t_dims) DefaultSpace

        hint t (ConcatReturns SplitStrided{} w _ _) = do
          t_dims <- mapM dimAllocationSize $ arrayDims t
          return $ Hint (innermost [w] t_dims) DefaultSpace

        hint Prim{} (ConcatReturns SplitContiguous w elems_per_thread _) = do
          let ixfun_base = IxFun.iota [num_threads, primExpFromSubExp int32 elems_per_thread]
              ixfun_tr = IxFun.permute ixfun_base [1,0]
              ixfun = IxFun.reshape ixfun_tr $ map (DimNew . primExpFromSubExp int32) [w]
          return $ Hint ixfun DefaultSpace

        hint _ _ = return NoHint

innermost :: [SubExp] -> [SubExp] -> IxFun
innermost space_dims t_dims =
  let r = length t_dims
      dims = space_dims ++ t_dims
      perm = [length space_dims..length space_dims+r-1] ++
             [0..length space_dims-1]
      perm_inv = rearrangeInverse perm
      dims_perm = rearrangeShape perm dims
      ixfun_base = IxFun.iota $ map (primExpFromSubExp int32) dims_perm
      ixfun_rearranged = IxFun.permute ixfun_base perm_inv
  in ixfun_rearranged

semiStatic :: S.Set VName -> SubExp -> Bool
semiStatic _ Constant{} = True
semiStatic consts (Var v) = v `S.member` consts

inGroupExpHints :: Allocator KernelsMem m => Exp KernelsMem -> m [ExpHint]
inGroupExpHints (Op (Inner (SegOp (SegMap _ space ts body))))
  | any private $ kernelBodyResult body = do
      consts <- askConsts
      return $ do
        (t, r) <- zip ts $ kernelBodyResult body
        return $
          if private r && all (semiStatic consts) (arrayDims t)
          then let seg_dims = map (primExpFromSubExp int32) $ segSpaceDims space
                   dims = seg_dims ++ map (primExpFromSubExp int32) (arrayDims t)
                   nilSlice d = DimSlice 0 d 0
                   ElemPrim pt = elemType t
             in Hint (IxFun.slice (IxFun.iota dims) $
                      fullSliceNum dims $ map nilSlice seg_dims) $
                ScalarSpace (arrayDims t) pt
          else NoHint
  where private (Returns ResultPrivate _) = True
        private _                         = False
inGroupExpHints e = return $ replicate (expExtTypeSize e) NoHint

inThreadExpHints :: Allocator KernelsMem m => Exp KernelsMem -> m [ExpHint]
inThreadExpHints e = do
  consts <- askConsts
  mapM (maybePrivate consts) =<< expExtType e
  where maybePrivate consts t
          | Just (Array (ElemPrim pt) shape _) <- hasStaticShape t,
            all (semiStatic consts) $ shapeDims shape = do
              let ixfun = IxFun.iota $ map (primExpFromSubExp int32) $
                          shapeDims shape
              return $ Hint ixfun $ ScalarSpace (shapeDims shape) pt
          | otherwise =
              return NoHint

-- | The pass from 'Kernels' to 'KernelsMem'.
explicitAllocations :: Pass Kernels KernelsMem
explicitAllocations = explicitAllocationsGeneric handleHostOp kernelExpHints

-- | Convert some 'Kernels' stms to 'KernelsMem'.
explicitAllocationsInStms :: (MonadFreshNames m, HasScope KernelsMem m) =>
                             Stms Kernels -> m (Stms KernelsMem)
explicitAllocationsInStms = explicitAllocationsInStmsGeneric handleHostOp kernelExpHints
