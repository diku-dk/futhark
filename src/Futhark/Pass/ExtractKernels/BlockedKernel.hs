{-# LANGUAGE TypeFamilies #-}

module Futhark.Pass.ExtractKernels.BlockedKernel
  ( DistRep,
    MkSegLevel,
    ThreadRecommendation (..),
    segRed,
    nonSegRed,
    segScan,
    segHist,
    segMap,
    mapKernel,
    KernelInput (..),
    readKernelInput,
    mkSegSpace,
    dummyDim,
  )
where

import Control.Monad
import Futhark.Analysis.PrimExp
import Futhark.IR
import Futhark.IR.Aliases (AliasableRep)
import Futhark.IR.GPU.Op (SegVirt (..))
import Futhark.IR.SegOp
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Transform.Rename
import Prelude hiding (quot)

-- | Constraints pertinent to performing distribution/flattening.
type DistRep rep =
  ( Buildable rep,
    HasSegOp rep,
    BuilderOps rep,
    LetDec rep ~ Type,
    ExpDec rep ~ (),
    BodyDec rep ~ (),
    AliasableRep rep
  )

data ThreadRecommendation = ManyThreads | NoRecommendation SegVirt

type MkSegLevel rep m =
  [SubExp] -> String -> ThreadRecommendation -> BuilderT rep m (SegOpLevel rep)

mkSegSpace :: (MonadFreshNames m) => [(VName, SubExp)] -> m SegSpace
mkSegSpace dims = SegSpace <$> newVName "phys_tid" <*> pure dims

prepareRedOrScan ::
  (MonadBuilder m, DistRep (Rep m)) =>
  Certs ->
  SubExp ->
  Lambda (Rep m) ->
  [VName] ->
  [(VName, SubExp)] ->
  [KernelInput] ->
  m (SegSpace, KernelBody (Rep m))
prepareRedOrScan cs w map_lam arrs ispace inps = do
  gtid <- newVName "gtid"
  space <- mkSegSpace $ ispace ++ [(gtid, w)]
  kbody <- fmap (uncurry (flip (KernelBody ()))) $
    runBuilder $
      localScope (scopeOfSegSpace space) $ do
        mapM_ readKernelInput inps
        certifying cs . mapM_ readKernelInput $ do
          (p, arr) <- zip (lambdaParams map_lam) arrs
          pure $ KernelInput (paramName p) (paramType p) arr [Var gtid]
        res <- bodyBind (lambdaBody map_lam)
        forM res $ \(SubExpRes res_cs se) -> pure $ Returns ResultMaySimplify res_cs se

  pure (space, kbody)

segRed ::
  (MonadFreshNames m, DistRep rep, HasScope rep m) =>
  SegOpLevel rep ->
  Pat (LetDec rep) ->
  Certs ->
  SubExp -> -- segment size
  [SegBinOp rep] ->
  Lambda rep ->
  [VName] ->
  [(VName, SubExp)] -> -- ispace = pair of (gtid, size) for the maps on "top" of this reduction
  [KernelInput] -> -- inps = inputs that can be looked up by using the gtids from ispace
  m (Stms rep)
segRed lvl pat cs w ops map_lam arrs ispace inps = runBuilder_ $ do
  (kspace, kbody) <- prepareRedOrScan cs w map_lam arrs ispace inps
  letBind pat $
    Op $
      segOp $
        SegRed lvl kspace ops (lambdaReturnType map_lam) kbody

segScan ::
  (MonadFreshNames m, DistRep rep, HasScope rep m) =>
  SegOpLevel rep ->
  Pat (LetDec rep) ->
  Certs ->
  SubExp -> -- segment size
  [SegBinOp rep] ->
  Lambda rep ->
  [VName] ->
  [(VName, SubExp)] -> -- ispace = pair of (gtid, size) for the maps on "top" of this scan
  [KernelInput] -> -- inps = inputs that can be looked up by using the gtids from ispace
  m (Stms rep)
segScan lvl pat cs w ops map_lam arrs ispace inps = runBuilder_ $ do
  (kspace, kbody) <- prepareRedOrScan cs w map_lam arrs ispace inps
  letBind pat $
    Op $
      segOp $
        SegScan lvl kspace ops (lambdaReturnType map_lam) kbody

segMap ::
  (MonadFreshNames m, DistRep rep, HasScope rep m) =>
  SegOpLevel rep ->
  Pat (LetDec rep) ->
  SubExp -> -- segment size
  Lambda rep ->
  [VName] ->
  [(VName, SubExp)] -> -- ispace = pair of (gtid, size) for the maps on "top" of this map
  [KernelInput] -> -- inps = inputs that can be looked up by using the gtids from ispace
  m (Stms rep)
segMap lvl pat w map_lam arrs ispace inps = runBuilder_ $ do
  (kspace, kbody) <- prepareRedOrScan mempty w map_lam arrs ispace inps
  letBind pat $
    Op $
      segOp $
        SegMap lvl kspace (lambdaReturnType map_lam) kbody

dummyDim ::
  (MonadBuilder m) =>
  Pat Type ->
  m (Pat Type, [(VName, SubExp)], m ())
dummyDim pat = do
  -- We add a unit-size segment on top to ensure that the result
  -- of the SegRed is an array, which we then immediately index.
  -- This is useful in the case that the value is used on the
  -- device afterwards, as this may save an expensive
  -- host-device copy (scalars are kept on the host, but arrays
  -- may be on the device).
  let addDummyDim t = t `arrayOfRow` intConst Int64 1
  pat' <- fmap addDummyDim <$> renamePat pat
  dummy <- newVName "dummy"
  let ispace = [(dummy, intConst Int64 1)]

  pure
    ( pat',
      ispace,
      forM_ (zip (patNames pat') (patNames pat)) $ \(from, to) -> do
        from_t <- lookupType from
        letBindNames [to] . BasicOp $
          case from_t of
            Acc {} -> SubExp $ Var from
            _ -> Index from $ fullSlice from_t [DimFix $ intConst Int64 0]
    )

nonSegRed ::
  (MonadFreshNames m, DistRep rep, HasScope rep m) =>
  SegOpLevel rep ->
  Pat Type ->
  SubExp ->
  [SegBinOp rep] ->
  Lambda rep ->
  [VName] ->
  m (Stms rep)
nonSegRed lvl pat w ops map_lam arrs = runBuilder_ $ do
  (pat', ispace, read_dummy) <- dummyDim pat
  addStms =<< segRed lvl pat' mempty w ops map_lam arrs ispace []
  read_dummy

segHist ::
  (DistRep rep, MonadFreshNames m, HasScope rep m) =>
  SegOpLevel rep ->
  Pat Type ->
  SubExp ->
  -- | Segment indexes and sizes.
  [(VName, SubExp)] ->
  [KernelInput] ->
  [HistOp rep] ->
  Lambda rep ->
  [VName] ->
  m (Stms rep)
segHist lvl pat arr_w ispace inps ops lam arrs = runBuilder_ $ do
  gtid <- newVName "gtid"
  space <- mkSegSpace $ ispace ++ [(gtid, arr_w)]

  kbody <- fmap (uncurry (flip $ KernelBody ())) $
    runBuilder $
      localScope (scopeOfSegSpace space) $ do
        mapM_ readKernelInput inps
        forM_ (zip (lambdaParams lam) arrs) $ \(p, arr) -> do
          arr_t <- lookupType arr
          letBindNames [paramName p] $
            BasicOp $
              Index arr $
                fullSlice arr_t [DimFix $ Var gtid]
        res <- bodyBind (lambdaBody lam)
        forM res $ \(SubExpRes cs se) ->
          pure $ Returns ResultMaySimplify cs se

  letBind pat $ Op $ segOp $ SegHist lvl space ops (lambdaReturnType lam) kbody

mapKernelSkeleton ::
  (DistRep rep, HasScope rep m, MonadFreshNames m) =>
  [(VName, SubExp)] ->
  [KernelInput] ->
  m (SegSpace, Stms rep)
mapKernelSkeleton ispace inputs = do
  read_input_stms <- runBuilder_ $ mapM readKernelInput inputs

  space <- mkSegSpace ispace
  pure (space, read_input_stms)

mapKernel ::
  (DistRep rep, HasScope rep m, MonadFreshNames m) =>
  MkSegLevel rep m ->
  [(VName, SubExp)] ->
  [KernelInput] ->
  [Type] ->
  KernelBody rep ->
  m (SegOp (SegOpLevel rep) rep, Stms rep)
mapKernel mk_lvl ispace inputs rts (KernelBody () kstms krets) = runBuilderT' $ do
  (space, read_input_stms) <- mapKernelSkeleton ispace inputs

  let kbody' = KernelBody () (read_input_stms <> kstms) krets

  -- If the kernel creates arrays (meaning it will require memory
  -- expansion), we want to truncate the amount of threads.
  -- Otherwise, have at it!  This is a bit of a hack - in principle,
  -- we should make this decision later, when we have a clearer idea
  -- of what is happening inside the kernel.
  let r = if all primType rts then ManyThreads else NoRecommendation SegVirt

  lvl <- mk_lvl (map snd ispace) "segmap" r

  pure $ SegMap lvl space rts kbody'

data KernelInput = KernelInput
  { kernelInputName :: VName,
    kernelInputType :: Type,
    kernelInputArray :: VName,
    kernelInputIndices :: [SubExp]
  }
  deriving (Show)

readKernelInput ::
  (DistRep (Rep m), MonadBuilder m) =>
  KernelInput ->
  m ()
readKernelInput inp = do
  let pe = PatElem (kernelInputName inp) $ kernelInputType inp
  letBind (Pat [pe]) . BasicOp $
    case kernelInputType inp of
      Acc {} ->
        SubExp $ Var $ kernelInputArray inp
      _ ->
        Index (kernelInputArray inp) . Slice $
          map DimFix (kernelInputIndices inp)
            ++ map sliceDim (arrayDims (kernelInputType inp))
