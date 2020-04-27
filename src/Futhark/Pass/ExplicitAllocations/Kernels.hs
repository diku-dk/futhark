{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Pass.ExplicitAllocations.Kernels
       ( explicitAllocations
       , explicitAllocationsInStms
       )
where

import qualified Futhark.Representation.Mem.IxFun as IxFun
import Futhark.Representation.KernelsMem
import Futhark.Representation.Kernels
import Futhark.Pass.ExplicitAllocations

allocAtLevel :: SegLevel -> AllocM fromlore tlore a -> AllocM fromlore tlore a
allocAtLevel lvl = local $ \env -> env { allocSpace = space
                                       , aggressiveReuse = True
                                       }
  where space = case lvl of SegThread{} -> DefaultSpace
                            SegGroup{} -> Space "local"

allocInBinOpLambda :: SegLevel -> SegSpace -> Lambda Kernels
                   -> AllocM Kernels KernelsMem (Lambda KernelsMem)
allocInBinOpLambda lvl (SegSpace flat _) lam = do
  num_threads <- letSubExp "num_threads" $ BasicOp $ BinOp (Mul Int32)
                 (unCount (segNumGroups lvl)) (unCount (segGroupSize lvl))
  let (acc_params, arr_params) =
        splitAt (length (lambdaParams lam) `div` 2) $ lambdaParams lam
      index_x = LeafExp flat int32
      index_y = index_x + primExpFromSubExp int32 num_threads
  (acc_params', arr_params') <-
    allocInBinOpParams num_threads index_x index_y acc_params arr_params

  local (\env -> env { envExpHints = inThreadExpHints }) $
    allocInLambda (acc_params' ++ arr_params')
    (lambdaBody lam) (lambdaReturnType lam)

allocInBinOpParams :: SubExp
                   -> PrimExp VName -> PrimExp VName
                   -> [LParam Kernels]
                   -> [LParam Kernels]
                   -> AllocM Kernels KernelsMem ([LParam KernelsMem], [LParam KernelsMem])
allocInBinOpParams num_threads my_id other_id xs ys = unzip <$> zipWithM alloc xs ys
  where alloc x y =
          case paramType x of
            Array bt shape u -> do
              twice_num_threads <-
                letSubExp "twice_num_threads" $
                BasicOp $ BinOp (Mul Int32) num_threads $ intConst Int32 2
              let t = paramType x `arrayOfRow` twice_num_threads
              mem <- allocForArray t DefaultSpace
              -- XXX: this iota ixfun is a bit inefficient; leading to uncoalesced access.
              let base_dims = map (primExpFromSubExp int32) (arrayDims t)
                  ixfun_base = IxFun.iota base_dims
                  ixfun_x = IxFun.slice ixfun_base $
                            fullSliceNum base_dims [DimFix my_id]
                  ixfun_y = IxFun.slice ixfun_base $
                            fullSliceNum base_dims [DimFix other_id]
              return (x { paramAttr = MemArray bt shape u $ ArrayIn mem ixfun_x },
                      y { paramAttr = MemArray bt shape u $ ArrayIn mem ixfun_y })
            Prim bt ->
              return (x { paramAttr = MemPrim bt },
                      y { paramAttr = MemPrim bt })
            Mem space ->
              return (x { paramAttr = MemMem space },
                      y { paramAttr = MemMem space })

allocInLambda :: [LParam KernelsMem] -> Body Kernels -> [Type]
              -> AllocM Kernels KernelsMem (Lambda KernelsMem)
allocInLambda params body rettype = do
  body' <- localScope (scopeOfLParams params) $
           allocInStms (bodyStms body) $ \bnds' ->
           return $ Body () bnds' $ bodyResult body
  return $ Lambda params body' rettype

allocInKernelBody :: SegLevel -> KernelBody Kernels
                  -> AllocM Kernels KernelsMem (KernelBody KernelsMem)
allocInKernelBody lvl (KernelBody () stms res) =
  local f $ allocInStms stms $ \stms' -> return $ KernelBody () stms' res
  where f = case lvl of SegThread{} -> inThread
                        SegGroup{} -> inGroup
        inThread env = env { envExpHints = inThreadExpHints }
        inGroup env = env { envExpHints = inGroupExpHints }

handleSegOp :: SegOp SegLevel Kernels
            -> AllocM Kernels KernelsMem (SegOp SegLevel KernelsMem)
handleSegOp op = allocAtLevel (segLevel op) $ mapSegOpM mapper op
  where scope = scopeOfSegSpace $ segSpace op
        mapper = identitySegOpMapper
             { mapOnSegOpBody = localScope scope . allocInKernelBody (segLevel op)
             , mapOnSegOpLambda = allocInBinOpLambda (segLevel op) $ segSpace op
             }

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
  where num_reds = segRedResults reds
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
          | coalesceReturnOfShape (primByteSize (elemType t)) $ arrayDims t = do
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

inGroupExpHints :: Exp KernelsMem -> AllocM Kernels KernelsMem [ExpHint]
inGroupExpHints (Op (Inner (SegOp (SegMap _ space ts body))))
  | any private $ kernelBodyResult body = return $ do
      (t, r) <- zip ts $ kernelBodyResult body
      return $
        if private r
        then let seg_dims = map (primExpFromSubExp int32) $ segSpaceDims space
                 dims = seg_dims ++ map (primExpFromSubExp int32) (arrayDims t)
                 nilSlice d = DimSlice 0 d 0
           in Hint (IxFun.slice (IxFun.iota dims) $
                    fullSliceNum dims $ map nilSlice seg_dims) $
              ScalarSpace (arrayDims t) $ elemType t
        else NoHint
  where private (Returns ResultPrivate _) = True
        private _                         = False
inGroupExpHints e = return $ replicate (expExtTypeSize e) NoHint

inThreadExpHints :: Allocator KernelsMem m => Exp KernelsMem -> m [ExpHint]
inThreadExpHints e =
  mapM maybePrivate =<< expExtType e
  where maybePrivate t
          | Just (Array pt shape _) <- hasStaticShape t,
            all semiStatic $ shapeDims shape = do
              let ixfun = IxFun.iota $ map (primExpFromSubExp int32) $
                          shapeDims shape
              return $ Hint ixfun $ ScalarSpace (shapeDims shape) pt
          | otherwise =
              return NoHint

        semiStatic Constant{} = True
        semiStatic _ = False

explicitAllocations :: Pass Kernels KernelsMem
explicitAllocations = explicitAllocationsGeneric handleHostOp kernelExpHints

explicitAllocationsInStms :: (MonadFreshNames m, HasScope KernelsMem m) =>
                             Stms Kernels -> m (Stms KernelsMem)
explicitAllocationsInStms = explicitAllocationsInStmsGeneric handleHostOp kernelExpHints
