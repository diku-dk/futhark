{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Do various kernel optimisations - mostly related to coalescing.
module Futhark.Pass.KernelBabysitting ( babysitKernels )
       where

import Control.Arrow (first)
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Data.Foldable
import Data.List (elemIndex, isPrefixOf, sort)
import Data.Maybe

import Futhark.MonadFreshNames
import Futhark.IR
import Futhark.IR.Kernels
       hiding (Prog, Body, Stm, Pattern, PatElem,
               BasicOp, Exp, Lambda, FunDef, FParam, LParam, RetType)
import Futhark.Tools
import Futhark.Pass
import Futhark.Util

-- | The pass definition.
babysitKernels :: Pass Kernels Kernels
babysitKernels = Pass "babysit kernels"
                 "Transpose kernel input arrays for better performance." $
                 intraproceduralTransformation onStms
  where onStms scope stms = do
          let m = localScope scope $ transformStms mempty stms
          fmap fst $ modifyNameSource $ runState (runBinderT m M.empty)

type BabysitM = Binder Kernels

transformStms :: ExpMap -> Stms Kernels -> BabysitM (Stms Kernels)
transformStms expmap stms = collectStms_ $ foldM_ transformStm expmap stms

transformBody :: ExpMap -> Body Kernels -> BabysitM (Body Kernels)
transformBody expmap (Body () stms res) = do
  stms' <- transformStms expmap stms
  return $ Body () stms' res

-- | Map from variable names to defining expression.  We use this to
-- hackily determine whether something is transposed or otherwise
-- funky in memory (and we'd prefer it not to be).  If we cannot find
-- it in the map, we just assume it's all good.  HACK and FIXME, I
-- suppose.  We really should do this at the memory level.
type ExpMap = M.Map VName (Stm Kernels)

nonlinearInMemory :: VName -> ExpMap -> Maybe (Maybe [Int])
nonlinearInMemory name m =
  case M.lookup name m of
    Just (Let _ _ (BasicOp (Opaque (Var arr)))) -> nonlinearInMemory arr m
    Just (Let _ _ (BasicOp (Rearrange perm _))) -> Just $ Just $ rearrangeInverse perm
    Just (Let _ _ (BasicOp (Reshape _ arr))) -> nonlinearInMemory arr m
    Just (Let _ _ (BasicOp (Manifest perm _))) -> Just $ Just perm
    Just (Let pat _ (Op (SegOp (SegMap _ _ ts _)))) ->
      nonlinear =<< find ((==name) . patElemName . fst)
      (zip (patternElements pat) ts)
    _ -> Nothing
  where nonlinear (pe, t)
          | inner_r <- arrayRank t, inner_r > 0 = do
              let outer_r = arrayRank (patElemType pe) - inner_r
              return $ Just $ rearrangeInverse $ [inner_r..inner_r+outer_r-1] ++ [0..inner_r-1]
          | otherwise = Nothing

transformStm :: ExpMap -> Stm Kernels -> BabysitM ExpMap
transformStm expmap (Let pat aux (Op (SegOp op))) = do
  let mapper = identitySegOpMapper
               { mapOnSegOpBody =
                   transformKernelBody expmap (segLevel op) (segSpace op)
               }
  op' <- mapSegOpM mapper op
  let stm' = Let pat aux $ Op $ SegOp op'
  addStm stm'
  return $ M.fromList [ (name, stm') | name <- patternNames pat ] <> expmap
transformStm expmap (Let pat aux e) = do
  e' <- mapExpM (transform expmap) e
  let bnd' = Let pat aux e'
  addStm bnd'
  return $ M.fromList [ (name, bnd') | name <- patternNames pat ] <> expmap

transform :: ExpMap -> Mapper Kernels Kernels BabysitM
transform expmap =
  identityMapper { mapOnBody = \scope -> localScope scope . transformBody expmap }

transformKernelBody :: ExpMap -> SegLevel -> SegSpace -> KernelBody Kernels
                    -> BabysitM (KernelBody Kernels)
transformKernelBody expmap lvl space kbody = do
  -- Go spelunking for accesses to arrays that are defined outside the
  -- kernel body and where the indices are kernel thread indices.
  scope <- askScope
  let thread_gids = map fst $ unSegSpace space
      thread_local = namesFromList $ segFlat space : thread_gids
      free_ker_vars = freeIn kbody `namesSubtract` getKerVariantIds space
  num_threads <- letSubExp "num_threads" $ BasicOp $ BinOp (Mul Int32 OverflowUndef)
                 (unCount $ segNumGroups lvl) (unCount $ segGroupSize lvl)
  evalStateT (traverseKernelBodyArrayIndexes
              free_ker_vars
              thread_local
              (scope <> scopeOfSegSpace space)
              (ensureCoalescedAccess expmap (unSegSpace space) num_threads)
              kbody)
    mempty
  where getKerVariantIds = namesFromList . M.keys . scopeOfSegSpace

type ArrayIndexTransform m =
  Names ->
  (VName -> Bool) ->           -- thread local?
  (VName -> SubExp -> Bool)->  -- variant to a certain gid (given as first param)?
  (SubExp -> Maybe SubExp) ->  -- split substitution?
  Scope Kernels ->            -- type environment
  VName -> Slice SubExp -> m (Maybe (VName, Slice SubExp))

traverseKernelBodyArrayIndexes :: (Applicative f, Monad f) =>
                                  Names
                               -> Names
                               -> Scope Kernels
                               -> ArrayIndexTransform f
                               -> KernelBody Kernels
                               -> f (KernelBody Kernels)
traverseKernelBodyArrayIndexes free_ker_vars thread_variant outer_scope f (KernelBody () kstms kres) =
  KernelBody () . stmsFromList <$>
  mapM (onStm (varianceInStms mempty kstms,
               mkSizeSubsts kstms,
               outer_scope)) (stmsToList kstms) <*>
  pure kres
  where onLambda (variance, szsubst, scope) lam =
          (\body' -> lam { lambdaBody = body' }) <$>
          onBody (variance, szsubst, scope') (lambdaBody lam)
          where scope' = scope <> scopeOfLParams (lambdaParams lam)

        onBody (variance, szsubst, scope) (Body bdec stms bres) = do
          stms' <- stmsFromList <$> mapM (onStm (variance', szsubst', scope')) (stmsToList stms)
          pure $ Body bdec stms' bres
          where variance' = varianceInStms variance stms
                szsubst' = mkSizeSubsts stms <> szsubst
                scope' = scope <> scopeOf stms

        onStm (variance, szsubst, _) (Let pat dec (BasicOp (Index arr is))) =
          Let pat dec . oldOrNew <$> f free_ker_vars isThreadLocal isGidVariant sizeSubst outer_scope arr is
          where oldOrNew Nothing =
                  BasicOp $ Index arr is
                oldOrNew (Just (arr', is')) =
                  BasicOp $ Index arr' is'

                isGidVariant gid (Var v) =
                  gid == v || nameIn gid (M.findWithDefault (oneName v) v variance)
                isGidVariant _ _ = False

                isThreadLocal v =
                  thread_variant `namesIntersect`
                  M.findWithDefault (oneName v) v variance

                sizeSubst (Constant v) = Just $ Constant v
                sizeSubst (Var v)
                  | v `M.member` outer_scope      = Just $ Var v
                  | Just v' <- M.lookup v szsubst = sizeSubst v'
                  | otherwise                      = Nothing

        onStm (variance, szsubst, scope) (Let pat dec e) =
          Let pat dec <$> mapExpM (mapper (variance, szsubst, scope)) e

        onOp ctx (OtherOp soac) =
          OtherOp <$> mapSOACM identitySOACMapper{ mapOnSOACLambda = onLambda ctx } soac
        onOp _ op = return op

        mapper ctx = identityMapper { mapOnBody = const (onBody ctx)
                                    , mapOnOp = onOp ctx }

        mkSizeSubsts = foldMap mkStmSizeSubst
          where mkStmSizeSubst (Let (Pattern [] [pe]) _ (Op (SizeOp (SplitSpace _ _ _ elems_per_i)))) =
                  M.singleton (patElemName pe) elems_per_i
                mkStmSizeSubst _ = mempty

type Replacements = M.Map (VName, Slice SubExp) VName

ensureCoalescedAccess :: MonadBinder m =>
                         ExpMap
                      -> [(VName,SubExp)]
                      -> SubExp
                      -> ArrayIndexTransform (StateT Replacements m)
ensureCoalescedAccess expmap thread_space num_threads free_ker_vars isThreadLocal
                      isGidVariant sizeSubst outer_scope arr slice = do
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
        slice' <- map (slice !!) perm,
        DimFix inner_ind <- last slice',
        not $ null thread_gids,
        isGidVariant inner_gid inner_ind ->
          return Nothing

      -- We are not fully indexing an array, but the remaining slice
      -- is invariant to the innermost-kernel dimension. We assume
      -- the remaining slice will be sequentially streamed, hence
      -- tiling will be applied later and will solve coalescing.
      -- Hence nothing to do at this point. (Cosmin's Heuristic.)
      | (is, rem_slice) <- splitSlice slice,
        not $ null rem_slice,
        allDimAreSlice rem_slice,
        Nothing <- M.lookup arr expmap,
        ElemPrim pt <- elemType t,
        not $ tooSmallSlice (primByteSize pt) rem_slice,
        is /= map Var (take (length is) thread_gids) || length is == length thread_gids,
        not (null thread_gids || null is),
        not (last thread_gids `nameIn` (freeIn is <> freeIn rem_slice)) ->
          return Nothing

      -- We are not fully indexing the array, and the indices are not
      -- a proper prefix of the thread indices, and some indices are
      -- thread local, so we assume (HEURISTIC!)  that the remaining
      -- dimensions will be traversed sequentially.
      | (is, rem_slice) <- splitSlice slice,
        not $ null rem_slice,
        ElemPrim pt <- elemType t,
        not $ tooSmallSlice (primByteSize pt) rem_slice,
        is /= map Var (take (length is) thread_gids) || length is == length thread_gids,
        any isThreadLocal (namesToList $ freeIn is) -> do
          let perm = coalescingPermutation (length is) $ arrayRank t
          replace =<< lift (rearrangeInput (nonlinearInMemory arr expmap) perm arr)

      -- We are taking a slice of the array with a unit stride.  We
      -- assume that the slice will be traversed sequentially.
      --
      -- We will really want to treat the sliced dimension like two
      -- dimensions so we can transpose them.  This may require
      -- padding.
      | (is, rem_slice) <- splitSlice slice,
        and $ zipWith (==) is $ map Var thread_gids,
        DimSlice offset len (Constant stride):_ <- rem_slice,
        isThreadLocalSubExp offset,
        Just {} <- sizeSubst len,
        oneIsh stride -> do
          let num_chunks = if null is
                           then primExpFromSubExp int32 num_threads
                           else coerceIntPrimExp Int32 $
                                product $ map (primExpFromSubExp int32) $
                                drop (length is) thread_gdims
          replace =<< lift (rearrangeSlice (length is) (arraySize (length is) t) num_chunks arr)

      -- Everything is fine... assuming that the array is in row-major
      -- order!  Make sure that is the case.
      | Just{} <- nonlinearInMemory arr expmap ->
          case sliceIndices slice of
            Just is | Just _ <- coalescedIndexes free_ker_vars isGidVariant (map Var thread_gids) is ->
                        replace =<< lift (rowMajorArray arr)
                    | otherwise ->
                        return Nothing
            _ -> replace =<< lift (rowMajorArray arr)

    _ -> return Nothing

  where (thread_gids, thread_gdims) = unzip thread_space

        replace arr' = do
          modify $ M.insert (arr, slice) arr'
          return $ Just (arr', slice)

        isThreadLocalSubExp (Var v) = isThreadLocal v
        isThreadLocalSubExp Constant{} = False

-- Heuristic for avoiding rearranging too small arrays.
tooSmallSlice :: Int32 -> Slice SubExp -> Bool
tooSmallSlice bs = fst . foldl comb (True,bs) . sliceDims
  where comb (True, x) (Constant (IntValue (Int32Value d))) = (d*x < 4, d*x)
        comb (_, x)     _                                   = (False, x)

splitSlice :: Slice SubExp -> ([SubExp], Slice SubExp)
splitSlice [] = ([], [])
splitSlice (DimFix i:is) = first (i:) $ splitSlice is
splitSlice is = ([], is)

allDimAreSlice :: Slice SubExp -> Bool
allDimAreSlice [] = True
allDimAreSlice (DimFix _:_) = False
allDimAreSlice (_:is) = allDimAreSlice is

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
  | is `isPrefixOf` tgids =
      Nothing
  | not (null tgids),
    not (null is),
    Var innergid <- last tgids,
    num_is > 0 && isGidVariant innergid (last is) =
      Just is
  -- 3. Otherwise try fix coalescing
  | otherwise =
      Just $ reverse $ foldl move (reverse is) $ zip [0..] (reverse tgids)
  where num_is = length is

        move is_rev (i, tgid)
          -- If tgid is in is_rev anywhere but at position i, and
          -- position i exists, we move it to position i instead.
          | Just j <- elemIndex tgid is_rev, i /= j, i < num_is =
              swap i j is_rev
          | otherwise =
              is_rev

        swap i j l
          | Just ix <- maybeNth i l,
            Just jx <- maybeNth j l =
              update i jx $ update j ix l
          | otherwise =
              error $ "coalescedIndexes swap: invalid indices" ++ show (i, j, l)

        update 0 x (_:ys) = x : ys
        update i x (y:ys) = y : update (i-1) x ys
        update _ _ []     = error "coalescedIndexes: update"

        isCt :: SubExp -> Bool
        isCt (Constant _) = True
        isCt (Var      _) = False

coalescingPermutation :: Int -> Int -> [Int]
coalescingPermutation num_is rank =
  [num_is..rank-1] ++ [0..num_is-1]

rearrangeInput :: MonadBinder m =>
                  Maybe (Maybe [Int]) -> [Int] -> VName -> m VName
rearrangeInput (Just (Just current_perm)) perm arr
  | current_perm == perm = return arr -- Already has desired representation.

rearrangeInput Nothing perm arr
  | sort perm == perm = return arr -- We don't know the current
                                   -- representation, but the indexing
                                   -- is linear, so let's hope the
                                   -- array is too.
rearrangeInput (Just Just{}) perm arr
  | sort perm == perm = rowMajorArray arr -- We just want a row-major array, no tricks.
rearrangeInput manifest perm arr = do
  -- We may first manifest the array to ensure that it is flat in
  -- memory.  This is sometimes unnecessary, in which case the copy
  -- will hopefully be removed by the simplifier.
  manifested <- if isJust manifest then rowMajorArray arr else return arr
  letExp (baseString arr ++ "_coalesced") $
    BasicOp $ Manifest perm manifested

rowMajorArray :: MonadBinder m =>
                 VName -> m VName
rowMajorArray arr = do
  rank <- arrayRank <$> lookupType arr
  letExp (baseString arr ++ "_rowmajor") $ BasicOp $ Manifest [0..rank-1] arr

rearrangeSlice :: MonadBinder m =>
                  Int -> SubExp -> PrimExp VName -> VName
               -> m VName
rearrangeSlice d w num_chunks arr = do
  num_chunks' <- toSubExp "num_chunks" num_chunks

  (w_padded, padding) <- paddedScanReduceInput w num_chunks'

  per_chunk <- letSubExp "per_chunk" $
               BasicOp $ BinOp (SQuot Int32 Unsafe) w_padded num_chunks'
  arr_t <- lookupType arr
  arr_padded <- padArray w_padded padding arr_t
  rearrange num_chunks' w_padded per_chunk (baseString arr) arr_padded arr_t

  where padArray w_padded padding arr_t = do
          let arr_shape = arrayShape arr_t
              padding_shape = setDim d arr_shape padding
          arr_padding <-
            letExp (baseString arr <> "_padding") $
            BasicOp $ Scratch (elemType arr_t) (shapeDims padding_shape)
          letExp (baseString arr <> "_padded") $
            BasicOp $ Concat d arr [arr_padding] w_padded

        rearrange num_chunks' w_padded per_chunk arr_name arr_padded arr_t = do
          let arr_dims = arrayDims arr_t
              pre_dims = take d arr_dims
              post_dims = drop (d+1) arr_dims
              extradim_shape = Shape $ pre_dims ++ [num_chunks', per_chunk] ++ post_dims
              tr_perm = [0..d-1] ++ map (+d) ([1] ++ [2..shapeRank extradim_shape-1-d] ++ [0])
          arr_extradim <-
            letExp (arr_name <> "_extradim") $
            BasicOp $ Reshape (map DimNew $ shapeDims extradim_shape) arr_padded
          arr_extradim_tr <-
            letExp (arr_name <> "_extradim_tr") $
            BasicOp $ Manifest tr_perm arr_extradim
          arr_inv_tr <- letExp (arr_name <> "_inv_tr") $
            BasicOp $ Reshape (map DimCoercion pre_dims ++ map DimNew (w_padded : post_dims))
            arr_extradim_tr
          letExp (arr_name <> "_inv_tr_init") =<<
            eSliceArray d  arr_inv_tr (eSubExp $ constant (0::Int32)) (eSubExp w)

paddedScanReduceInput :: MonadBinder m =>
                         SubExp -> SubExp
                      -> m (SubExp, SubExp)
paddedScanReduceInput w stride = do
  w_padded <- letSubExp "padded_size" =<<
              eRoundToMultipleOf Int32 (eSubExp w) (eSubExp stride)
  padding <- letSubExp "padding" $ BasicOp $ BinOp (Sub Int32 OverflowUndef) w_padded w
  return (w_padded, padding)

--- Computing variance.

type VarianceTable = M.Map VName Names

varianceInStms :: VarianceTable -> Stms Kernels -> VarianceTable
varianceInStms t = foldl varianceInStm t . stmsToList

varianceInStm :: VarianceTable -> Stm Kernels -> VarianceTable
varianceInStm variance bnd =
  foldl' add variance $ patternNames $ stmPattern bnd
  where add variance' v = M.insert v binding_variance variance'
        look variance' v = oneName v <> M.findWithDefault mempty v variance'
        binding_variance = mconcat $ map (look variance) $ namesToList (freeIn bnd)
