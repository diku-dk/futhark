{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Facilities for converting a 'GPU' program to 'GPUMem'.
module Futhark.Pass.ExplicitAllocations.GPU
  ( explicitAllocations,
    explicitAllocationsInStms,
  )
where

import Control.Monad
import Data.Set qualified as S
import Futhark.IR.GPU
import Futhark.IR.GPUMem
import Futhark.IR.Mem.IxFun qualified as IxFun
import Futhark.Pass.ExplicitAllocations
import Futhark.Pass.ExplicitAllocations.SegOp

instance SizeSubst (HostOp rep op) where
  opIsConst (SizeOp GetSize {}) = True
  opIsConst (SizeOp GetSizeMax {}) = True
  opIsConst _ = False

allocAtLevel :: SegLevel -> AllocM GPU GPUMem a -> AllocM GPU GPUMem a
allocAtLevel lvl = local $ \env ->
  env
    { allocSpace = space,
      aggressiveReuse = True,
      allocInOp = handleHostOp (Just lvl)
    }
  where
    space = case lvl of
      SegGroup {} -> Space "local"
      SegThread {} -> Space "device"
      SegThreadInGroup {} -> Space "device"

handleSegOp ::
  Maybe SegLevel ->
  SegOp SegLevel GPU ->
  AllocM GPU GPUMem (SegOp SegLevel GPUMem)
handleSegOp outer_lvl op = do
  num_threads <-
    letSubExp "num_threads"
      =<< case maybe_grid of
        Just grid ->
          pure . BasicOp $
            BinOp
              (Mul Int64 OverflowUndef)
              (unCount (gridNumGroups grid))
              (unCount (gridGroupSize grid))
        Nothing ->
          foldBinOp
            (Mul Int64 OverflowUndef)
            (intConst Int64 1)
            (segSpaceDims $ segSpace op)
  allocAtLevel (segLevel op) $ mapSegOpM (mapper num_threads) op
  where
    maybe_grid =
      case (outer_lvl, segLevel op) of
        (Just (SegThread _ (Just grid)), _) -> Just grid
        (Just (SegGroup _ (Just grid)), _) -> Just grid
        (_, SegThread _ (Just grid)) -> Just grid
        (_, SegGroup _ (Just grid)) -> Just grid
        _ -> Nothing
    scope = scopeOfSegSpace $ segSpace op
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
      SegThreadInGroup {} -> inThread
      SegGroup {} -> inGroup
    inThread env = env {envExpHints = inThreadExpHints}
    inGroup env = env {envExpHints = inGroupExpHints}

handleHostOp ::
  Maybe SegLevel ->
  HostOp SOAC GPU ->
  AllocM GPU GPUMem (MemOp (HostOp NoOp) GPUMem)
handleHostOp _ (SizeOp op) =
  pure $ Inner $ SizeOp op
handleHostOp _ (OtherOp op) =
  error $ "Cannot allocate memory in SOAC: " ++ prettyString op
handleHostOp outer_lvl (SegOp op) =
  Inner . SegOp <$> handleSegOp outer_lvl op
handleHostOp _ (GPUBody ts (Body _ stms res)) =
  fmap (Inner . GPUBody ts) . buildBody_ . allocInStms stms $ pure res

kernelExpHints :: Exp GPUMem -> AllocM GPU GPUMem [ExpHint]
kernelExpHints (BasicOp (Manifest perm v)) = do
  dims <- arrayDims <$> lookupType v
  let perm_inv = rearrangeInverse perm
      dims' = rearrangeShape perm dims
      ixfun = IxFun.permute (IxFun.iota $ map pe64 dims') perm_inv
  pure [Hint ixfun $ Space "device"]
kernelExpHints (Op (Inner (SegOp (SegMap lvl@(SegThread _ _) space ts body)))) =
  zipWithM (mapResultHint lvl space) ts $ kernelBodyResult body
kernelExpHints (Op (Inner (SegOp (SegRed lvl@(SegThread _ _) space reds ts body)))) =
  (map (const NoHint) red_res <>) <$> zipWithM (mapResultHint lvl space) (drop num_reds ts) map_res
  where
    num_reds = segBinOpResults reds
    (red_res, map_res) = splitAt num_reds $ kernelBodyResult body
kernelExpHints e = defaultExpHints e

mapResultHint ::
  SegLevel ->
  SegSpace ->
  Type ->
  KernelResult ->
  AllocM GPU GPUMem ExpHint
mapResultHint _lvl space = hint
  where
    -- Heuristic: do not rearrange for returned arrays that are
    -- sufficiently small.
    coalesceReturnOfShape _ [] = False
    coalesceReturnOfShape bs [Constant (IntValue (Int64Value d))] = bs * d > 4
    coalesceReturnOfShape _ _ = True

    hint t Returns {}
      | coalesceReturnOfShape (primByteSize (elemType t)) $ arrayDims t = do
          let space_dims = segSpaceDims space
          pure $ Hint (innermost space_dims (arrayDims t)) $ Space "device"
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
                        fullSliceNum dims $
                          map nilSlice seg_dims
                    )
                    $ ScalarSpace (arrayDims t)
                    $ elemType t
            else NoHint
  where
    private (Returns ResultPrivate _ _) = True
    private _ = False
inGroupExpHints e = defaultExpHints e

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
explicitAllocations = explicitAllocationsGeneric (Space "device") (handleHostOp Nothing) kernelExpHints

-- | Convert some 'GPU' stms to 'GPUMem'.
explicitAllocationsInStms ::
  (MonadFreshNames m, HasScope GPUMem m) =>
  Stms GPU ->
  m (Stms GPUMem)
explicitAllocationsInStms = explicitAllocationsInStmsGeneric (Space "device") (handleHostOp Nothing) kernelExpHints
