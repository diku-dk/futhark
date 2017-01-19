{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Do various kernel optimisations - mostly related to coalescing.
module Futhark.Pass.KernelBabysitting
       ( babysitKernels
       , nonlinearInMemory
       )
       where

import Control.Arrow (first)
import Control.Applicative
import Control.Monad.State
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Monoid

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.AST
import Futhark.Representation.Kernels
       hiding (Prog, Body, Stm, Pattern, PatElem,
               BasicOp, Exp, Lambda, ExtLambda, FunDef, FParam, LParam, RetType)
import Futhark.Tools
import Futhark.Pass
import Futhark.Util

babysitKernels :: Pass Kernels Kernels
babysitKernels =
  Pass { passName = "babysit kernels"
       , passDescription = "Transpose kernel input arrays for better performance."
       , passFunction = intraproceduralTransformation transformFunDef
       }

transformFunDef :: MonadFreshNames m => FunDef Kernels -> m (FunDef Kernels)
transformFunDef fundec = do
  (body', _) <- modifyNameSource $ runState (runBinderT m HM.empty)
  return fundec { funDefBody = body' }
  where m = inScopeOf fundec $
            transformBody $ funDefBody fundec

type BabysitM = Binder Kernels

transformBody :: Body Kernels -> BabysitM (Body Kernels)
transformBody (Body () bnds res) = insertStmsM $ do
  foldM_ transformStm HM.empty bnds
  return $ resultBody res

-- | Map from variable names to defining expression.  We use this to
-- hackily determine whether something is transposed or otherwise
-- funky in memory (and we'd prefer it not to be).  If we cannot find
-- it in the map, we just assume it's all good.  HACK and FIXME, I
-- suppose.  We really should do this at the memory level.
type ExpMap = HM.HashMap VName (Stm Kernels)

nonlinearInMemory :: VName -> ExpMap -> Maybe (Maybe [Int])
nonlinearInMemory name m =
  case HM.lookup name m of
    Just (Let _ _ (BasicOp (Rearrange _ perm _))) -> Just $ Just perm
    Just (Let _ _ (BasicOp (Reshape _ _ arr))) -> nonlinearInMemory arr m
    Just (Let _ _ (BasicOp (Manifest perm _))) -> Just $ Just perm
    Just (Let pat _ (Op (Kernel _ _ _ ts _))) ->
      nonlinear =<< find ((==name) . patElemName . fst)
      (zip (patternElements pat) ts)
    _ -> Nothing
  where nonlinear (pe, t)
          | inner_r <- arrayRank t, inner_r > 0 = do
              let outer_r = arrayRank (patElemType pe) - inner_r
              return $ Just $ [inner_r..inner_r+outer_r-1] ++ [0..inner_r-1]
          | otherwise = Nothing

transformStm :: ExpMap -> Stm Kernels -> BabysitM ExpMap

transformStm expmap (Let pat () (Op (Kernel desc cs space ts kbody))) = do
  -- Go spelunking for accesses to arrays that are defined outside the
  -- kernel body and where the indices are kernel thread indices.
  scope <- askScope
  let thread_gids = map fst $ spaceDimensions space
      thread_local = HS.fromList $ spaceGlobalId space : thread_gids

  kbody'' <- evalStateT (traverseKernelBodyArrayIndexes
                         thread_local
                         (castScope scope <> scopeOfKernelSpace space)
                         (ensureCoalescedAccess expmap thread_gids)
                         kbody)
             mempty

  let bnd' = Let pat () $ Op $ Kernel desc cs space ts kbody''
  addStm bnd'
  return $ HM.fromList [ (name, bnd') | name <- patternNames pat ] <> expmap

transformStm expmap (Let pat () e) = do
  e' <- mapExpM transform e
  let bnd' = Let pat () e'
  addStm bnd'
  return $ HM.fromList [ (name, bnd') | name <- patternNames pat ] <> expmap

transform :: Mapper Kernels Kernels BabysitM
transform = identityMapper { mapOnBody = \scope -> localScope scope . transformBody }

type ArrayIndexTransform m =
  (VName -> Bool) ->           -- thread local?
  (SubExp -> Maybe SubExp) ->  -- split substitution?
  Scope InKernel ->            -- type environment
  VName -> Slice SubExp -> m (Maybe (VName, Slice SubExp))

traverseKernelBodyArrayIndexes :: (Applicative f, Monad f) =>
                                  Names
                               -> Scope InKernel
                               -> ArrayIndexTransform f
                               -> KernelBody InKernel
                               -> f (KernelBody InKernel)
traverseKernelBodyArrayIndexes thread_variant outer_scope f (KernelBody () kstms kres) =
  KernelBody () <$>
  mapM (onStm (varianceInStms mempty kstms,
               mkSizeSubsts kstms,
               outer_scope)) kstms <*>
  pure kres
  where onLambda (variance, szsubst, scope) lam =
          (\body' -> lam { lambdaBody = body' }) <$>
          onBody (variance, szsubst, scope') (lambdaBody lam)
          where scope' = scope <> scopeOfLParams (lambdaParams lam)

        onStreamLambda (variance, szsubst, scope) lam =
          (\body' -> lam { groupStreamLambdaBody = body' }) <$>
          onBody (variance, szsubst, scope') (groupStreamLambdaBody lam)
          where scope' = scope <> scopeOf lam

        onBody (variance, szsubst, scope) (Body battr stms bres) =
          Body battr <$> mapM (onStm (variance', szsubst', scope')) stms <*> pure bres
          where variance' = varianceInStms variance stms
                szsubst' = mkSizeSubsts stms <> szsubst
                scope' = scope <> scopeOf stms

        onStm (variance, szsubst, scope) (Let pat attr (BasicOp (Index cs arr is))) =
          Let pat attr . oldOrNew <$> f isThreadLocal sizeSubst scope arr is
          where oldOrNew Nothing =
                  BasicOp $ Index cs arr is
                oldOrNew (Just (arr', is')) =
                  BasicOp $ Index cs arr' is'

                isThreadLocal v =
                  not $ HS.null $
                  thread_variant `HS.intersection`
                  HM.lookupDefault mempty v variance

                sizeSubst (Constant v) = Just $ Constant v
                sizeSubst (Var v)
                  | v `HM.member` outer_scope      = Just $ Var v
                  | Just v' <- HM.lookup v szsubst = sizeSubst v'
                  | otherwise                      = Nothing

        onStm (variance, szsubst, scope) (Let pat attr e) =
          Let pat attr <$> mapExpM (mapper (variance, szsubst, scope)) e

        mapper ctx = identityMapper { mapOnBody = const (onBody ctx)
                                    , mapOnOp = onOp ctx
                                    }

        onOp ctx (GroupReduce w lam input) =
          GroupReduce w <$> onLambda ctx lam <*> pure input
        onOp ctx (GroupStream w maxchunk lam accs arrs) =
           GroupStream w maxchunk <$> onStreamLambda ctx lam <*> pure accs <*> pure arrs
        onOp _ stm = pure stm

        mkSizeSubsts = mconcat . map mkStmSizeSubst
          where mkStmSizeSubst (Let (Pattern [] [pe]) _ (Op (SplitSpace _ _ _ elems_per_i))) =
                  HM.singleton (patElemName pe) elems_per_i
                mkStmSizeSubst _ = mempty

-- Not a hashmap, as SubExp is not hashable.
type Replacements = M.Map (VName, Slice SubExp) VName

ensureCoalescedAccess :: MonadBinder m =>
                         ExpMap
                      -> [VName]
                      -> ArrayIndexTransform (StateT Replacements m)
ensureCoalescedAccess expmap thread_gids isThreadLocal sizeSubst scope arr slice = do
  seen <- gets $ M.lookup (arr, slice)

  case (seen, isThreadLocal arr, typeOf <$> HM.lookup arr scope) of
    -- Already took care of this case elsewhere.
    (Just arr', _, _) ->
      pure $ Just (arr', slice)

    (Nothing, False, Just t)
      -- We are fully indexing the array with thread IDs, but the
      -- indices are in a permuted order.
      | Just is <- sliceIndices slice,
        length is == arrayRank t,
        Just is' <- coalescedIndexes (map Var thread_gids) is,
        Just perm <- is' `isPermutationOf` is ->
          replace =<< lift (rearrangeInput (nonlinearInMemory arr expmap) perm arr)

      -- We are taking a slice of the array with a unit stride.  We
      -- assume that the array will be traversed sequentially.
      --
      -- We will really want to treat the outer dimension like two
      -- dimensions so we can transpose them.  This may require
      -- padding, but it will also require us to know an upper bound
      -- on 'len'.
      | DimSlice offset len (Constant stride) : _rem_slice <- slice,
        all isThreadLocal $ HS.toList $ freeIn offset,
        Just len' <- sizeSubst len,
        oneIsh stride ->
          replace =<< lift (rearrangeSlice (arraySize 0 t) len' arr)

      -- We are not fully indexing the array, and the indices are not
      -- a proper prefix of the thread indices, and some indices are
      -- thread local, so we assume (HEURISTIC!)  that the remaining
      -- dimensions will be traversed sequentially.
      | (is, rem_slice) <- splitSlice slice,
        not $ null rem_slice,
        not $ tooSmallSlice (primByteSize (elemType t)) rem_slice,
        is /= map Var (take (length is) thread_gids)
        || length is == length thread_gids,
        any isThreadLocal (HS.toList $ freeIn rem_slice) -> do
          let perm = coalescingPermutation (length is) $ arrayRank t
          replace =<< lift (rearrangeInput (nonlinearInMemory arr expmap) perm arr)

      -- Everything is fine... assuming that the array is in row-major
      -- order!  Make sure that is the case.
      | Just{} <- nonlinearInMemory arr expmap ->
          case sliceIndices slice of
            Just is | Just _ <- coalescedIndexes (map Var thread_gids) is ->
                        replace =<< lift (rowMajorArray arr)
                    | otherwise ->
                        return Nothing
            _ -> replace =<< lift (rowMajorArray arr)

    _ -> return Nothing

  where replace arr' = do
          modify $ M.insert (arr, slice) arr'
          return $ Just (arr', slice)

-- Heuristic for avoiding rearranging too small arrays.
tooSmallSlice :: Int32 -> Slice SubExp -> Bool
tooSmallSlice bs = fst . foldl comb (True,bs) . sliceDims
  where comb (True, x) (Constant (IntValue (Int32Value d))) = (d*x < 4, d*x)
        comb (_, x)     _                                   = (False, x)

splitSlice :: Slice SubExp -> ([SubExp], Slice SubExp)
splitSlice [] = ([], [])
splitSlice (DimFix i:is) = first (i:) $ splitSlice is
splitSlice is = ([], is)

-- Try to move thread indexes into their proper position.
coalescedIndexes :: [SubExp] -> [SubExp] -> Maybe [SubExp]
coalescedIndexes tgids is
  -- Do Nothing if:
  -- 1. the innermost index is the innermost thread id
  --    (because access is already coalesced)
  -- 2. any of the indices is a constant, i.e., kernel free variable
  --    (because it would transpose a bigger array then needed -- big overhead).
  | any isCt is =
      Nothing
  | num_is > 0 && not (null tgids) && last is == last tgids =
      Just is
  -- Otherwise try fix coalescing
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
                  SubExp -> SubExp -> VName
               -> m VName
rearrangeSlice w elements_per_thread arr = do
  num_threads <- letSubExp "num_threadS" =<<
                 eRoundToMultipleOf Int32 (eSubExp w) (eSubExp elements_per_thread)
  (w_padded, padding) <- paddedScanReduceInput w num_threads

  arr_t <- lookupType arr
  arr_padded <- padArray w_padded padding arr_t
  rearrange num_threads w_padded (baseString arr) arr_padded (rowType arr_t)

  where padArray w_padded padding arr_t = do
          let arr_shape = arrayShape arr_t
              padding_shape = arr_shape `setOuterDim` padding
          arr_padding <-
            letExp (baseString arr <> "_padding") $
            BasicOp $ Scratch (elemType arr_t) (shapeDims padding_shape)
          letExp (baseString arr <> "_padded") $
            BasicOp $ Concat [] 0 arr [arr_padding] w_padded

        rearrange num_threads w_padded arr_name arr_padded row_type = do
          let row_dims = arrayDims row_type
              extradim_shape = Shape $ [num_threads, elements_per_thread] ++ row_dims
              tr_perm = [1] ++ [2..shapeRank extradim_shape-1] ++ [0]
              tr_perm_inv = rearrangeInverse tr_perm
          arr_extradim <-
            letExp (arr_name <> "_extradim") $
            BasicOp $ Reshape [] (map DimNew $ shapeDims extradim_shape) arr_padded
          arr_extradim_tr <-
            letExp (arr_name <> "_extradim_tr") $
            BasicOp $ Rearrange [] tr_perm arr_extradim
          arr_extradim_manifested <-
            letExp (arr_name <> "_extradim_manifested") $
            BasicOp $ Copy arr_extradim_tr
          arr_extradim_inv_tr <-
            letExp (arr_name <> "_extradim_inv_tr") $
            BasicOp $ Rearrange [] tr_perm_inv arr_extradim_manifested
          arr_inv_tr <- letExp (arr_name <> "_inv_tr") $
            BasicOp $ Reshape [] (reshapeOuter [DimNew w_padded] 2 extradim_shape)
            arr_extradim_inv_tr
          letExp (arr_name <> "_inv_tr_init") $
            BasicOp $ Split [] 0 [w] arr_inv_tr

paddedScanReduceInput :: MonadBinder m =>
                         SubExp -> SubExp
                      -> m (SubExp, SubExp)
paddedScanReduceInput w num_threads = do
  w_padded <- letSubExp "padded_size" =<<
              eRoundToMultipleOf Int32 (eSubExp w) (eSubExp num_threads)
  padding <- letSubExp "padding" $ BasicOp $ BinOp (Sub Int32) w_padded w
  return (w_padded, padding)

--- Computing variance.

type VarianceTable = HM.HashMap VName Names

varianceInStms :: VarianceTable -> [Stm InKernel] -> VarianceTable
varianceInStms = foldl varianceInStm

varianceInStm :: VarianceTable -> Stm InKernel -> VarianceTable
varianceInStm variance bnd =
  foldl' add variance $ patternNames $ bindingPattern bnd
  where add variance' v = HM.insert v binding_variance variance'
        look variance' v = HS.insert v $ HM.lookupDefault mempty v variance'
        binding_variance = mconcat $ map (look variance) $ HS.toList (freeInStm bnd)
