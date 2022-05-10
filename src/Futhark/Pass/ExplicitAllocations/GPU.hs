{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Facilities for converting a 'GPU' program to 'GPUMem'.
module Futhark.Pass.ExplicitAllocations.GPU
  ( explicitAllocations,
    explicitAllocationsInStms,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S
import Futhark.IR.GPU
import Futhark.IR.GPUMem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.Pass.ExplicitAllocations
import Futhark.Pass.ExplicitAllocations.SegOp

instance SizeSubst (HostOp rep op) where
  opSizeSubst (Pat [size]) (SizeOp (SplitSpace _ _ _ elems_per_thread)) =
    M.singleton (patElemName size) elems_per_thread
  opSizeSubst _ _ = mempty

  opIsConst (SizeOp GetSize {}) = True
  opIsConst (SizeOp GetSizeMax {}) = True
  opIsConst _ = False

allocAtLevel :: SegLevel -> AllocM fromrep trep a -> AllocM fromrep trep a
allocAtLevel lvl = local $ \env ->
  env
    { allocSpace = space,
      aggressiveReuse = True
    }
  where
    space = case lvl of
      SegThread {} -> DefaultSpace
      SegGroup {} -> Space "local"

handleSegOp ::
  SegOp SegLevel GPU ->
  AllocM GPU GPUMem (SegOp SegLevel GPUMem)
handleSegOp op = do
  num_threads <-
    letSubExp "num_threads" $
      BasicOp $
        BinOp
          (Mul Int64 OverflowUndef)
          (unCount (segNumGroups lvl))
          (unCount (segGroupSize lvl))
  allocAtLevel lvl $ mapSegOpM (mapper num_threads) op
  where
    scope = scopeOfSegSpace $ segSpace op
    lvl = segLevel op
    mapper num_threads =
      identitySegOpMapper
        { mapOnSegOpBody =
            localScope scope . local f . allocInKernelBody,
          mapOnSegOpLambda =
            local inThread
              . allocInBinOpLambda num_threads (segSpace op)
        }
    f = case segLevel op of
      SegThread {} -> inThread
      SegGroup {} -> inGroup
    inThread env = env {envExpHints = inThreadExpHints}
    inGroup env = env {envExpHints = inGroupExpHints}

handleHostOp ::
  HostOp GPU (SOAC GPU) ->
  AllocM GPU GPUMem (MemOp (HostOp GPUMem ()))
handleHostOp (SizeOp op) =
  pure $ Inner $ SizeOp op
handleHostOp (OtherOp op) =
  error $ "Cannot allocate memory in SOAC: " ++ pretty op
handleHostOp (SegOp op) =
  Inner . SegOp <$> handleSegOp op
handleHostOp (GPUBody ts (Body _ stms res)) =
  fmap (Inner . GPUBody ts) . buildBody_ . allocInStms stms $ pure res

kernelExpHints :: Exp GPUMem -> AllocM GPU GPUMem [ExpHint]
kernelExpHints (BasicOp (Manifest perm v)) = do
  dims <- arrayDims <$> lookupType v
  let perm_inv = rearrangeInverse perm
      dims' = rearrangeShape perm dims
      ixfun = IxFun.permute (IxFun.iota $ map pe64 dims') perm_inv
  pure [Hint ixfun DefaultSpace]
kernelExpHints (Op (Inner (SegOp (SegMap lvl@SegThread {} space ts body)))) =
  zipWithM (mapResultHint lvl space) ts $ kernelBodyResult body
kernelExpHints (Op (Inner (SegOp (SegRed lvl@SegThread {} space reds ts body)))) =
  (map (const NoHint) red_res <>) <$> zipWithM (mapResultHint lvl space) (drop num_reds ts) map_res
  where
    num_reds = segBinOpResults reds
    (red_res, map_res) = splitAt num_reds $ kernelBodyResult body
kernelExpHints e =
  pure $ replicate (expExtTypeSize e) NoHint

mapResultHint ::
  SegLevel ->
  SegSpace ->
  Type ->
  KernelResult ->
  AllocM GPU GPUMem ExpHint
mapResultHint lvl space = hint
  where
    num_threads =
      pe64 (unCount $ segNumGroups lvl) * pe64 (unCount $ segGroupSize lvl)

    -- Heuristic: do not rearrange for returned arrays that are
    -- sufficiently small.
    coalesceReturnOfShape _ [] = False
    coalesceReturnOfShape bs [Constant (IntValue (Int64Value d))] = bs * d > 4
    coalesceReturnOfShape _ _ = True

    hint t Returns {}
      | coalesceReturnOfShape (primByteSize (elemType t)) $ arrayDims t = do
          chunkmap <- asks chunkMap
          let space_dims = segSpaceDims space
              t_dims = map (dimAllocationSize chunkmap) $ arrayDims t
          pure $ Hint (innermost space_dims t_dims) DefaultSpace
    hint t (ConcatReturns _ SplitStrided {} w _ _) = do
      chunkmap <- asks chunkMap
      let t_dims = map (dimAllocationSize chunkmap) $ arrayDims t
      pure $ Hint (innermost [w] t_dims) DefaultSpace
    hint Prim {} (ConcatReturns _ SplitContiguous w elems_per_thread _) = do
      let ixfun_base = IxFun.iota [sExt64 num_threads, pe64 elems_per_thread]
          ixfun_tr = IxFun.permute ixfun_base [1, 0]
          ixfun = IxFun.reshape ixfun_tr $ map (DimNew . pe64) [w]
      pure $ Hint ixfun DefaultSpace
    hint _ _ = pure NoHint

innermost :: [SubExp] -> [SubExp] -> IxFun
innermost space_dims t_dims =
  let r = length t_dims
      dims = space_dims ++ t_dims
      perm =
        [length space_dims .. length space_dims + r - 1]
          ++ [0 .. length space_dims - 1]
      perm_inv = rearrangeInverse perm
      dims_perm = rearrangeShape perm dims
      ixfun_base = IxFun.iota $ map pe64 dims_perm
      ixfun_rearranged = IxFun.permute ixfun_base perm_inv
   in ixfun_rearranged

semiStatic :: S.Set VName -> SubExp -> Bool
semiStatic _ Constant {} = True
semiStatic consts (Var v) = v `S.member` consts

inGroupExpHints :: Exp GPUMem -> AllocM GPU GPUMem [ExpHint]
inGroupExpHints (Op (Inner (SegOp (SegMap _ space ts body))))
  | any private $ kernelBodyResult body = do
      consts <- asks envConsts
      pure $ do
        (t, r) <- zip ts $ kernelBodyResult body
        pure $
          if private r && all (semiStatic consts) (arrayDims t)
            then
              let seg_dims = map pe64 $ segSpaceDims space
                  dims = seg_dims ++ map pe64 (arrayDims t)
                  nilSlice d = DimSlice 0 d 0
               in Hint
                    ( IxFun.slice (IxFun.iota dims) $
                        fullSliceNum dims $ map nilSlice seg_dims
                    )
                    $ ScalarSpace (arrayDims t) $ elemType t
            else NoHint
  where
    private (Returns ResultPrivate _ _) = True
    private _ = False
inGroupExpHints e = pure $ replicate (expExtTypeSize e) NoHint

inThreadExpHints :: Exp GPUMem -> AllocM GPU GPUMem [ExpHint]
inThreadExpHints e = do
  consts <- asks envConsts
  mapM (maybePrivate consts) =<< expExtType e
  where
    maybePrivate consts t
      | Just (Array pt shape _) <- hasStaticShape t,
        all (semiStatic consts) $ shapeDims shape = do
          let ixfun = IxFun.iota $ map pe64 $ shapeDims shape
          pure $ Hint ixfun $ ScalarSpace (shapeDims shape) pt
      | otherwise =
          pure NoHint

-- | The pass from 'GPU' to 'GPUMem'.
explicitAllocations :: Pass GPU GPUMem
explicitAllocations = explicitAllocationsGeneric handleHostOp kernelExpHints

-- | Convert some 'GPU' stms to 'GPUMem'.
explicitAllocationsInStms ::
  (MonadFreshNames m, HasScope GPUMem m) =>
  Stms GPU ->
  m (Stms GPUMem)
explicitAllocationsInStms = explicitAllocationsInStmsGeneric handleHostOp kernelExpHints
