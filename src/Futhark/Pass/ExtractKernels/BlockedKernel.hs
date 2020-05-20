{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Pass.ExtractKernels.BlockedKernel
       ( DistLore
       , MkSegLevel
       , ThreadRecommendation(..)
       , segRed
       , nonSegRed
       , segScan
       , segHist
       , segMap

       , mapKernel
       , KernelInput(..)
       , readKernelInput

       , mkSegSpace
       , dummyDim
       )
       where

import Control.Monad
import Control.Monad.Writer
import Data.List ()

import Prelude hiding (quot)

import Futhark.Analysis.PrimExp
import Futhark.IR
import Futhark.IR.SegOp
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Transform.Rename

-- | Constraints pertinent to performing distribution/flattening.
type DistLore lore = (Bindable lore,
                      HasSegOp lore,
                      BinderOps lore,
                      LetDec lore ~ Type,
                      ExpDec lore ~ (),
                      BodyDec lore ~ ())

data ThreadRecommendation = ManyThreads | NoRecommendation SegVirt

type MkSegLevel lore m =
  [SubExp] -> String -> ThreadRecommendation -> BinderT lore m (SegOpLevel lore)

mkSegSpace :: MonadFreshNames m => [(VName, SubExp)] -> m SegSpace
mkSegSpace dims = SegSpace <$> newVName "phys_tid" <*> pure dims

prepareRedOrScan :: (MonadBinder m, DistLore (Lore m)) =>
                    SubExp
                 -> Lambda (Lore m)
                 -> [VName] -> [(VName, SubExp)] -> [KernelInput]
                 -> m (SegSpace, KernelBody (Lore m))
prepareRedOrScan w map_lam arrs ispace inps = do
  gtid <- newVName "gtid"
  space <- mkSegSpace $ ispace ++ [(gtid, w)]
  kbody <- fmap (uncurry (flip (KernelBody ()))) $ runBinder $
           localScope (scopeOfSegSpace space) $ do
    mapM_ readKernelInput inps
    forM_ (zip (lambdaParams map_lam) arrs) $ \(p, arr) -> do
      arr_t <- lookupType arr
      letBindNames_ [paramName p] $
        BasicOp $ Index arr $ fullSlice arr_t [DimFix $ Var gtid]
    map (Returns ResultMaySimplify) <$> bodyBind (lambdaBody map_lam)

  return (space, kbody)

segRed :: (MonadFreshNames m, DistLore lore, HasScope lore m) =>
          SegOpLevel lore
       -> Pattern lore
       -> SubExp -- segment size
       -> [SegBinOp lore]
       -> Lambda lore
       -> [VName]
       -> [(VName, SubExp)] -- ispace = pair of (gtid, size) for the maps on "top" of this reduction
       -> [KernelInput]     -- inps = inputs that can be looked up by using the gtids from ispace
       -> m (Stms lore)
segRed lvl pat w ops map_lam arrs ispace inps = runBinder_ $ do
  (kspace, kbody) <- prepareRedOrScan w map_lam arrs ispace inps
  letBind_ pat $ Op $ segOp $
    SegRed lvl kspace ops (lambdaReturnType map_lam) kbody

segScan :: (MonadFreshNames m, DistLore lore, HasScope lore m) =>
           SegOpLevel lore
        -> Pattern lore
        -> SubExp -- segment size
        -> [SegBinOp lore] -> Lambda lore
        -> [VName]
        -> [(VName, SubExp)] -- ispace = pair of (gtid, size) for the maps on "top" of this scan
        -> [KernelInput]     -- inps = inputs that can be looked up by using the gtids from ispace
        -> m (Stms lore)
segScan lvl pat w ops map_lam arrs ispace inps = runBinder_ $ do
  (kspace, kbody) <- prepareRedOrScan w map_lam arrs ispace inps
  letBind_ pat $ Op $ segOp $
    SegScan lvl kspace ops (lambdaReturnType map_lam) kbody

segMap :: (MonadFreshNames m, DistLore lore, HasScope lore m) =>
          SegOpLevel lore
       -> Pattern lore
       -> SubExp -- segment size
       -> Lambda lore
       -> [VName]
       -> [(VName, SubExp)] -- ispace = pair of (gtid, size) for the maps on "top" of this map
       -> [KernelInput]     -- inps = inputs that can be looked up by using the gtids from ispace
       -> m (Stms lore)
segMap lvl pat w map_lam arrs ispace inps = runBinder_ $ do
  (kspace, kbody) <- prepareRedOrScan w map_lam arrs ispace inps
  letBind_ pat $ Op $ segOp $
    SegMap lvl kspace (lambdaReturnType map_lam) kbody

dummyDim :: (MonadFreshNames m, MonadBinder m, DistLore (Lore m)) =>
            Pattern (Lore m)
         -> m (Pattern (Lore m), [(VName, SubExp)], m ())
dummyDim pat = do
  -- We add a unit-size segment on top to ensure that the result
  -- of the SegRed is an array, which we then immediately index.
  -- This is useful in the case that the value is used on the
  -- device afterwards, as this may save an expensive
  -- host-device copy (scalars are kept on the host, but arrays
  -- may be on the device).
  let addDummyDim t = t `arrayOfRow` intConst Int32 1
  pat' <- fmap addDummyDim <$> renamePattern pat
  dummy <- newVName "dummy"
  let ispace = [(dummy, intConst Int32 1)]

  return (pat', ispace,
          forM_ (zip (patternNames pat') (patternNames pat)) $ \(from, to) -> do
             from_t <- lookupType from
             letBindNames_ [to] $ BasicOp $ Index from $
               fullSlice from_t [DimFix $ intConst Int32 0])

nonSegRed :: (MonadFreshNames m, DistLore lore, HasScope lore m) =>
             SegOpLevel lore
          -> Pattern lore
          -> SubExp
          -> [SegBinOp lore]
          -> Lambda lore
          -> [VName]
          -> m (Stms lore)
nonSegRed lvl pat w ops map_lam arrs = runBinder_ $ do
  (pat', ispace, read_dummy) <- dummyDim pat
  addStms =<< segRed lvl pat' w ops map_lam arrs ispace []
  read_dummy

segHist :: (DistLore lore, MonadFreshNames m, HasScope lore m) =>
           SegOpLevel lore
        -> Pattern lore
        -> SubExp
        -> [(VName,SubExp)] -- ^ Segment indexes and sizes.
        -> [KernelInput]
        -> [HistOp lore]
        -> Lambda lore -> [VName]
        -> m (Stms lore)
segHist lvl pat arr_w ispace inps ops lam arrs = runBinder_ $ do
  gtid <- newVName "gtid"
  space <- mkSegSpace $ ispace ++ [(gtid, arr_w)]

  kbody <- fmap (uncurry (flip $ KernelBody ())) $ runBinder $
           localScope (scopeOfSegSpace space) $ do
    mapM_ readKernelInput inps
    forM_ (zip (lambdaParams lam) arrs) $ \(p, arr) -> do
      arr_t <- lookupType arr
      letBindNames_ [paramName p] $
        BasicOp $ Index arr $ fullSlice arr_t [DimFix $ Var gtid]
    map (Returns ResultMaySimplify) <$> bodyBind (lambdaBody lam)

  letBind_ pat $ Op $ segOp $ SegHist lvl space ops (lambdaReturnType lam) kbody

mapKernelSkeleton :: (DistLore lore, HasScope lore m, MonadFreshNames m) =>
                     [(VName, SubExp)] -> [KernelInput]
                  -> m (SegSpace, Stms lore)
mapKernelSkeleton ispace inputs = do
  read_input_bnds <- runBinder_ $ mapM readKernelInput inputs

  space <- mkSegSpace ispace
  return (space, read_input_bnds)

mapKernel :: (DistLore lore, HasScope lore m, MonadFreshNames m) =>
             MkSegLevel lore m
          -> [(VName, SubExp)] -> [KernelInput]
          -> [Type] -> KernelBody lore
          -> m (SegOp (SegOpLevel lore) lore, Stms lore)
mapKernel mk_lvl ispace inputs rts (KernelBody () kstms krets) = runBinderT' $ do
  (space, read_input_stms) <- mapKernelSkeleton ispace inputs

  let kbody' = KernelBody () (read_input_stms <> kstms) krets

  -- If the kernel creates arrays (meaning it will require memory
  -- expansion), we want to truncate the amount of threads.
  -- Otherwise, have at it!  This is a bit of a hack - in principle,
  -- we should make this decision later, when we have a clearer idea
  -- of what is happening inside the kernel.
  let r = if all primType rts then ManyThreads else NoRecommendation SegVirt

  lvl <- mk_lvl (map snd ispace) "segmap" r

  return $ SegMap lvl space rts kbody'

data KernelInput = KernelInput { kernelInputName :: VName
                               , kernelInputType :: Type
                               , kernelInputArray :: VName
                               , kernelInputIndices :: [SubExp]
                               }
                 deriving (Show)

readKernelInput :: (DistLore (Lore m), MonadBinder m) =>
                   KernelInput -> m ()
readKernelInput inp = do
  let pe = PatElem (kernelInputName inp) $ kernelInputType inp
  arr_t <- lookupType $ kernelInputArray inp
  letBind_ (Pattern [] [pe]) $
    BasicOp $ Index (kernelInputArray inp) $
    fullSlice arr_t $ map DimFix $ kernelInputIndices inp
