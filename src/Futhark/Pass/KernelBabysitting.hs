{-# LANGUAGE TypeFamilies #-}

-- | Do various kernel optimisations - mostly related to coalescing.
module Futhark.Pass.KernelBabysitting (babysitKernels) where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor (first)
import Data.Foldable
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.IR.GPU
import Futhark.Pass
import Futhark.Tools
import Futhark.Util

-- | The pass definition.
babysitKernels :: Pass GPU GPU
babysitKernels =
  Pass
    "babysit kernels"
    "Transpose kernel input arrays for better performance."
    $ intraproceduralTransformation onStms
  where
    onStms scope stms = do
      let m = localScope scope $ transformStms mempty stms
      fmap fst $ modifyNameSource $ runState (runBuilderT m M.empty)

type BabysitM = Builder GPU

transformStms :: ExpMap -> Stms GPU -> BabysitM (Stms GPU)
transformStms expmap stms = collectStms_ $ foldM_ transformStm expmap stms

transformBody :: ExpMap -> Body GPU -> BabysitM (Body GPU)
transformBody expmap (Body () stms res) = do
  stms' <- transformStms expmap stms
  pure $ Body () stms' res

-- | Map from variable names to defining expression.  We use this to
-- hackily determine whether something is transposed or otherwise
-- funky in memory (and we'd prefer it not to be).  If we cannot find
-- it in the map, we just assume it's all good.  HACK and FIXME, I
-- suppose.  We really should do this at the memory level.
type ExpMap = M.Map VName (Stm GPU)

nonlinearInMemory :: VName -> ExpMap -> Maybe (Maybe [Int])
nonlinearInMemory name m =
  case M.lookup name m of
    Just (Let _ _ (BasicOp (Opaque _ (Var arr)))) -> nonlinearInMemory arr m
    Just (Let _ _ (BasicOp (Rearrange perm _))) -> Just $ Just $ rearrangeInverse perm
    Just (Let _ _ (BasicOp (Reshape _ _ arr))) -> nonlinearInMemory arr m
    Just (Let _ _ (BasicOp (Manifest perm _))) -> Just $ Just perm
    Just (Let pat _ (Op (SegOp (SegMap _ _ ts _)))) ->
      nonlinear
        =<< find
          ((== name) . patElemName . fst)
          (zip (patElems pat) ts)
    _ -> Nothing
  where
    nonlinear (pe, t)
      | inner_r <- arrayRank t,
        inner_r > 0 = do
          let outer_r = arrayRank (patElemType pe) - inner_r
          pure $ Just $ rearrangeInverse $ [inner_r .. inner_r + outer_r - 1] ++ [0 .. inner_r - 1]
      | otherwise = Nothing

transformStm :: ExpMap -> Stm GPU -> BabysitM ExpMap
transformStm expmap (Let pat aux (Op (SegOp op)))
  -- FIXME: We only make coalescing optimisations for SegThread
  -- SegOps, because that's what the analysis assumes.  For SegBlock
  -- we should probably look at the component SegThreads, but it
  -- apparently hasn't come up in practice yet.
  | SegThread {} <- segLevel op = do
      let mapper =
            identitySegOpMapper
              { mapOnSegOpBody =
                  transformKernelBody expmap (segSpace op)
              }
      op' <- mapSegOpM mapper op
      let stm' = Let pat aux $ Op $ SegOp op'
      addStm stm'
      pure $ M.fromList [(name, stm') | name <- patNames pat] <> expmap
transformStm expmap (Let pat aux e) = do
  e' <- mapExpM (transform expmap) e
  let stm' = Let pat aux e'
  addStm stm'
  pure $ M.fromList [(name, stm') | name <- patNames pat] <> expmap

transform :: ExpMap -> Mapper GPU GPU BabysitM
transform expmap =
  identityMapper {mapOnBody = \scope -> localScope scope . transformBody expmap}

transformKernelBody ::
  ExpMap ->
  SegSpace ->
  KernelBody GPU ->
  BabysitM (KernelBody GPU)
transformKernelBody expmap space kbody = do
  -- Go spelunking for accesses to arrays that are defined outside the
  -- kernel body and where the indices are kernel thread indices.
  scope <- askScope
  let thread_gids = map fst $ unSegSpace space
      thread_local = namesFromList $ segFlat space : thread_gids
      free_ker_vars = freeIn kbody `namesSubtract` getKerVariantIds space
  evalStateT
    ( traverseKernelBodyArrayIndexes
        free_ker_vars
        thread_local
        (scope <> scopeOfSegSpace space)
        (ensureCoalescedAccess expmap (unSegSpace space))
        kbody
    )
    mempty
  where
    getKerVariantIds = namesFromList . M.keys . scopeOfSegSpace

type ArrayIndexTransform m =
  Names ->
  (VName -> Bool) -> -- thread local?
  (VName -> SubExp -> Bool) -> -- variant to a certain gid (given as first param)?
  Scope GPU -> -- type environment
  VName ->
  Slice SubExp ->
  m (Maybe (VName, Slice SubExp))

traverseKernelBodyArrayIndexes ::
  forall f.
  (Monad f) =>
  Names ->
  Names ->
  Scope GPU ->
  ArrayIndexTransform f ->
  KernelBody GPU ->
  f (KernelBody GPU)
traverseKernelBodyArrayIndexes free_ker_vars thread_variant outer_scope f (KernelBody () kstms kres) =
  KernelBody () . stmsFromList
    <$> mapM
      ( onStm
          ( varianceInStms mempty kstms,
            outer_scope
          )
      )
      (stmsToList kstms)
    <*> pure kres
  where
    onLambda :: (VarianceTable, Scope GPU) -> Lambda GPU -> f (Lambda GPU)
    onLambda (variance, scope) lam =
      (\body' -> lam {lambdaBody = body'})
        <$> onBody (variance, scope') (lambdaBody lam)
      where
        scope' = scope <> scopeOfLParams (lambdaParams lam)

    onBody (variance, scope) (Body bdec stms bres) = do
      stms' <- stmsFromList <$> mapM (onStm (variance', scope')) (stmsToList stms)
      pure $ Body bdec stms' bres
      where
        variance' = varianceInStms variance stms
        scope' = scope <> scopeOf stms

    onStm (variance, _) (Let pat dec (BasicOp (Index arr is))) =
      Let pat dec . oldOrNew <$> f free_ker_vars isThreadLocal isGidVariant outer_scope arr is
      where
        oldOrNew Nothing =
          BasicOp $ Index arr is
        oldOrNew (Just (arr', is')) =
          BasicOp $ Index arr' is'

        isGidVariant gid (Var v) =
          gid == v || nameIn gid (M.findWithDefault (oneName v) v variance)
        isGidVariant _ _ = False

        isThreadLocal v =
          thread_variant
            `namesIntersect` M.findWithDefault (oneName v) v variance
    onStm (variance, scope) (Let pat dec e) =
      Let pat dec <$> mapExpM (mapper (variance, scope)) e

    onOp :: (VarianceTable, Scope GPU) -> Op GPU -> f (Op GPU)
    onOp ctx (OtherOp soac) =
      OtherOp <$> mapSOACM (soacMapper ctx) soac
    onOp _ op = pure op

    soacMapper ctx =
      (identitySOACMapper @GPU)
        { mapOnSOACLambda = onLambda ctx
        }

    mapper ctx =
      (identityMapper @GPU)
        { mapOnBody = const (onBody ctx),
          mapOnOp = onOp ctx
        }

type Replacements = M.Map (VName, Slice SubExp) VName

ensureCoalescedAccess ::
  (MonadBuilder m) =>
  ExpMap ->
  [(VName, SubExp)] ->
  ArrayIndexTransform (StateT Replacements m)
ensureCoalescedAccess
  expmap
  thread_space
  free_ker_vars
  isThreadLocal
  isGidVariant
  outer_scope
  arr
  slice = do
    seen <- gets $ M.lookup (arr, slice)

    case (seen, isThreadLocal arr, typeOf <$> M.lookup arr outer_scope) of
      -- Already took care of this case elsewhere.
      (Just arr', _, _) ->
        pure $ Just (arr', slice)
      (Nothing, False, Just t)
        -- We are fully indexing the array with thread IDs, but the
        -- indices are in a permuted order.
        | Just is <- sliceIndices slice,
          length is == arrayRank t,
          Just is' <- coalescedIndexes free_ker_vars isGidVariant (map Var thread_gids) is,
          Just perm <- is' `isPermutationOf` is ->
            replace =<< lift (rearrangeInput (nonlinearInMemory arr expmap) perm arr)
        -- Check whether the access is already coalesced because of a
        -- previous rearrange being applied to the current array:
        -- 1. get the permutation of the source-array rearrange
        -- 2. apply it to the slice
        -- 3. check that the innermost index is actually the gid
        --    of the innermost kernel dimension.
        -- If so, the access is already coalesced, nothing to do!
        -- (Cosmin's Heuristic.)
        | Just (Let _ _ (BasicOp (Rearrange perm _))) <- M.lookup arr expmap,
          not $ null perm,
          not $ null thread_gids,
          inner_gid <- last thread_gids,
          length slice >= length perm,
          slice' <- map (unSlice slice !!) perm,
          DimFix inner_ind <- last slice',
          not $ null thread_gids,
          isGidVariant inner_gid inner_ind ->
            pure Nothing
        -- We are not fully indexing an array, but the remaining slice
        -- is invariant to the innermost-kernel dimension. We assume
        -- the remaining slice will be sequentially streamed, hence
        -- tiling will be applied later and will solve coalescing.
        -- Hence nothing to do at this point. (Cosmin's Heuristic.)
        | (is, rem_slice) <- splitSlice slice,
          not $ null rem_slice,
          allDimAreSlice rem_slice,
          Nothing <- M.lookup arr expmap,
          pt <- elemType t,
          not $ tooSmallSlice (primByteSize pt) rem_slice,
          is /= map Var (take (length is) thread_gids) || length is == length thread_gids,
          not (null thread_gids || null is),
          last thread_gids `notNameIn` (freeIn is <> freeIn rem_slice) ->
            pure Nothing
        -- We are not fully indexing the array, and the indices are not
        -- a proper prefix of the thread indices, and some indices are
        -- thread local, so we assume (HEURISTIC!)  that the remaining
        -- dimensions will be traversed sequentially.
        | (is, rem_slice) <- splitSlice slice,
          not $ null rem_slice,
          pt <- elemType t,
          not $ tooSmallSlice (primByteSize pt) rem_slice,
          is /= map Var (take (length is) thread_gids) || length is == length thread_gids,
          any isThreadLocal (namesToList $ freeIn is) -> do
            let perm = coalescingPermutation (length is) $ arrayRank t
            replace =<< lift (rearrangeInput (nonlinearInMemory arr expmap) perm arr)

        -- Everything is fine... assuming that the array is in row-major
        -- order!  Make sure that is the case.
        | Just {} <- nonlinearInMemory arr expmap ->
            case sliceIndices slice of
              Just is
                | Just _ <- coalescedIndexes free_ker_vars isGidVariant (map Var thread_gids) is ->
                    replace =<< lift (rowMajorArray arr)
                | otherwise ->
                    pure Nothing
              _ -> replace =<< lift (rowMajorArray arr)
      _ -> pure Nothing
    where
      (thread_gids, _thread_gdims) = unzip thread_space

      replace arr' = do
        modify $ M.insert (arr, slice) arr'
        pure $ Just (arr', slice)

-- Heuristic for avoiding rearranging too small arrays.
tooSmallSlice :: Int32 -> Slice SubExp -> Bool
tooSmallSlice bs = fst . foldl comb (True, bs) . sliceDims
  where
    comb (True, x) (Constant (IntValue (Int32Value d))) = (d * x < 4, d * x)
    comb (_, x) _ = (False, x)

splitSlice :: Slice SubExp -> ([SubExp], Slice SubExp)
splitSlice (Slice []) = ([], Slice [])
splitSlice (Slice (DimFix i : is)) = first (i :) $ splitSlice (Slice is)
splitSlice is = ([], is)

allDimAreSlice :: Slice SubExp -> Bool
allDimAreSlice (Slice []) = True
allDimAreSlice (Slice (DimFix _ : _)) = False
allDimAreSlice (Slice (_ : is)) = allDimAreSlice (Slice is)

-- Try to move thread indexes into their proper position.
coalescedIndexes :: Names -> (VName -> SubExp -> Bool) -> [SubExp] -> [SubExp] -> Maybe [SubExp]
coalescedIndexes free_ker_vars isGidVariant tgids is
  -- Do Nothing if:
  -- 1. any of the indices is a constant or a kernel free variable
  --    (because it would transpose a bigger array then needed -- big overhead).
  -- 2. the innermost index is variant to the innermost-thread gid
  --    (because access is likely to be already coalesced)
  -- 3. the indexes are a prefix of the thread indexes, because that
  -- means multiple threads will be accessing the same element.
  | any isCt is =
      Nothing
  | any (`nameIn` free_ker_vars) (subExpVars is) =
      Nothing
  | is `L.isPrefixOf` tgids =
      Nothing
  | not (null tgids),
    not (null is),
    Var innergid <- last tgids,
    num_is > 0 && isGidVariant innergid (last is) =
      Just is
  -- 3. Otherwise try fix coalescing
  | otherwise =
      Just $ reverse $ foldl move (reverse is) $ zip [0 ..] (reverse tgids)
  where
    num_is = length is

    move is_rev (i, tgid)
      -- If tgid is in is_rev anywhere but at position i, and
      -- position i exists, we move it to position i instead.
      | Just j <- L.elemIndex tgid is_rev,
        i /= j,
        i < num_is =
          swap i j is_rev
      | otherwise =
          is_rev

    swap i j l
      | Just ix <- maybeNth i l,
        Just jx <- maybeNth j l =
          update i jx $ update j ix l
      | otherwise =
          error $ "coalescedIndexes swap: invalid indices" ++ show (i, j, l)

    update 0 x (_ : ys) = x : ys
    update i x (y : ys) = y : update (i - 1) x ys
    update _ _ [] = error "coalescedIndexes: update"

    isCt :: SubExp -> Bool
    isCt (Constant _) = True
    isCt (Var _) = False

coalescingPermutation :: Int -> Int -> [Int]
coalescingPermutation num_is rank =
  [num_is .. rank - 1] ++ [0 .. num_is - 1]

rearrangeInput ::
  (MonadBuilder m) =>
  Maybe (Maybe [Int]) ->
  [Int] ->
  VName ->
  m VName
rearrangeInput (Just (Just current_perm)) perm arr
  | current_perm == perm = pure arr -- Already has desired representation.
rearrangeInput Nothing perm arr
  | L.sort perm == perm = pure arr -- We don't know the current
  -- representation, but the indexing
  -- is linear, so let's hope the
  -- array is too.
rearrangeInput (Just Just {}) perm arr
  | L.sort perm == perm = rowMajorArray arr -- We just want a row-major array, no tricks.
rearrangeInput manifest perm arr = do
  -- We may first manifest the array to ensure that it is flat in
  -- memory.  This is sometimes unnecessary, in which case the copy
  -- will hopefully be removed by the simplifier.
  manifested <- if isJust manifest then rowMajorArray arr else pure arr
  letExp (baseString arr ++ "_coalesced") $
    BasicOp $
      Manifest perm manifested

rowMajorArray ::
  (MonadBuilder m) =>
  VName ->
  m VName
rowMajorArray arr = do
  rank <- arrayRank <$> lookupType arr
  letExp (baseString arr ++ "_rowmajor") $ BasicOp $ Manifest [0 .. rank - 1] arr

--- Computing variance.

type VarianceTable = M.Map VName Names

varianceInStms :: VarianceTable -> Stms GPU -> VarianceTable
varianceInStms t = foldl varianceInStm t . stmsToList

varianceInStm :: VarianceTable -> Stm GPU -> VarianceTable
varianceInStm variance stm =
  foldl' add variance $ patNames $ stmPat stm
  where
    add variance' v = M.insert v binding_variance variance'
    look variance' v = oneName v <> M.findWithDefault mempty v variance'
    binding_variance = mconcat $ map (look variance) $ namesToList (freeIn stm)
