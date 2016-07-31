{-# LANGUAGE FlexibleContexts #-}
-- | Do various kernel optimisations - mostly related to coalescing.
module Futhark.Pass.KernelBabysitting
       ( babysitKernels
       , nonlinearInMemory
       )
       where

import Control.Applicative
import Control.Monad.State
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Monoid

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.Kernels
import Futhark.Tools
import Futhark.Pass
import Futhark.Util

babysitKernels :: Pass Kernels Kernels
babysitKernels =
  Pass { passName = "babysit kernels"
       , passDescription = "Transpose kernel input arrays for better performance."
       , passFunction = intraproceduralTransformation transformFunDef
       }

transformFunDef :: MonadFreshNames m => FunDef -> m FunDef
transformFunDef fundec = do
  (body', _) <- modifyNameSource $ runState (runBinderT m HM.empty)
  return fundec { funDefBody = body' }
  where m = inScopeOf fundec $
            transformBody $ funDefBody fundec

type BabysitM = Binder Kernels

transformBody :: Body -> BabysitM Body
transformBody (Body () bnds res) = insertBindingsM $ do
  foldM_ transformBinding HM.empty bnds
  return $ resultBody res

-- | Map from variable names to defining expression.  We use this to
-- hackily determine whether something is transposed or otherwise
-- funky in memory (and we'd prefer it not to be).  If we cannot find
-- it in the map, we just assume it's all good.  HACK and FIXME, I
-- suppose.  We really should do this at the memory level.
type ExpMap = HM.HashMap VName Binding

nonlinearInMemory :: VName -> ExpMap -> Maybe (Maybe [Int])
nonlinearInMemory name m =
  case HM.lookup name m of
    Just (Let _ _ (PrimOp (Rearrange _ perm _))) -> Just $ Just perm
    Just (Let _ _ (PrimOp (Reshape _ _ arr))) -> nonlinearInMemory arr m
    Just (Let pat _ (Op (Kernel _ _ ts _))) ->
      nonlinear =<< find ((==name) . patElemName . fst)
      (zip (patternElements pat) ts)
    _ -> Nothing
  where nonlinear (pe, t)
          | inner_r <- arrayRank t, inner_r > 0 = do
              let outer_r = arrayRank (patElemType pe) - inner_r
              return $ Just $ [inner_r..inner_r+outer_r-1] ++ [0..inner_r-1]
          | otherwise = Nothing

transformBinding :: ExpMap -> Binding -> BabysitM ExpMap

transformBinding expmap (Let pat () (DoLoop ctx val form body)) = do
  body' <- localScope (scopeOfFParams $ map fst $ ctx ++ val) $
           localScope (scopeOfLoopForm form) $
           transformBody body
  addBinding $ Let pat () $ DoLoop ctx val form body'
  return expmap

transformBinding expmap (Let pat ()
                         (Op (ScanKernel cs w kernel_size lam foldlam nes arrs)))
  | kernelWorkgroups kernel_size /= constant (1::Int32) = do
  -- We want to pad and transpose the input arrays.

  lam' <- transformLambda lam
  foldlam' <- transformLambda foldlam

  addBinding $ Let pat () $ Op $
    ScanKernel cs w kernel_size lam' foldlam' nes arrs
  return expmap

transformBinding expmap (Let pat () (Op (Kernel cs space ts kbody))) = do
  -- First we do the easy stuff, which deals with SplitArray statements.
  kbody' <- transformKernelBody num_threads cs kbody

  -- Then we go spelunking for accesses to arrays that are defined
  -- outside the kernel body and where the indices are kernel thread
  -- indices.
  scope <- askScope
  let boundOutside = fmap typeOf . (`HM.lookup` scope)
      thread_gids = map fst $ spaceDimensions space

  kbody'' <- evalStateT (traverseKernelBodyArrayIndexes
                         (ensureCoalescedAccess expmap thread_gids boundOutside)
                         kbody')
             mempty

  let bnd' = Let pat () $ Op $ Kernel cs space ts kbody''
  addBinding bnd'
  return $ HM.fromList [ (name, bnd') | name <- patternNames pat ] <> expmap
  where num_threads = spaceNumThreads space

transformBinding expmap (Let pat () e) = do
  e' <- mapExpM transform e
  let bnd' = Let pat () e'
  addBinding bnd'
  return $ HM.fromList [ (name, bnd') | name <- patternNames pat ] <> expmap

transform :: Mapper Kernels Kernels BabysitM
transform = identityMapper { mapOnBody = transformBody }

transformLambda :: Lambda -> BabysitM Lambda
transformLambda lam = do
  body' <- inScopeOf lam $
           transformBody $ lambdaBody lam
  return lam { lambdaBody = body' }

paddedScanReduceInput :: SubExp -> SubExp
                      -> BabysitM (SubExp, SubExp)
paddedScanReduceInput w num_threads = do
  w_padded <- letSubExp "padded_size" =<<
              eRoundToMultipleOf Int32 (eSubExp w) (eSubExp num_threads)
  padding <- letSubExp "padding" $ PrimOp $ BinOp (Sub Int32) w_padded w
  return (w_padded, padding)

rearrangeScanReduceInput :: MonadBinder m =>
                            Certificates
                         -> SubExp -> SubExp -> SubExp -> SubExp -> SubExp -> VName
                         -> m VName
rearrangeScanReduceInput cs num_threads padding w w_padded elements_per_thread arr = do
  arr_t <- lookupType arr
  arr_padded <- padArray arr_t
  rearrange (baseString arr) arr_padded (rowType arr_t)

  where padArray arr_t = do
          let arr_shape = arrayShape arr_t
              padding_shape = arr_shape `setOuterDim` padding
          arr_padding <-
            letExp (baseString arr <> "_padding") $
            PrimOp $ Scratch (elemType arr_t) (shapeDims padding_shape)
          letExp (baseString arr <> "_padded") $
            PrimOp $ Concat [] 0 arr [arr_padding] w_padded

        rearrange arr_name arr_padded row_type = do
          let row_dims = arrayDims row_type
              extradim_shape = Shape $ [num_threads, elements_per_thread] ++ row_dims
              tr_perm = [1] ++ [2..shapeRank extradim_shape-1] ++ [0]
              tr_perm_inv = rearrangeInverse tr_perm
          arr_extradim <-
            letExp (arr_name <> "_extradim") $
            PrimOp $ Reshape cs (map DimNew $ shapeDims extradim_shape) arr_padded
          arr_extradim_tr <-
            letExp (arr_name <> "_extradim_tr") $
            PrimOp $ Rearrange [] tr_perm arr_extradim
          arr_extradim_manifested <-
            letExp (arr_name <> "_extradim_manifested") $
            PrimOp $ Copy arr_extradim_tr
          arr_extradim_inv_tr <-
            letExp (arr_name <> "_extradim_inv_tr") $
            PrimOp $ Rearrange [] tr_perm_inv arr_extradim_manifested
          arr_inv_tr <- letExp (arr_name <> "_inv_tr") $
            PrimOp $ Reshape [] (reshapeOuter [DimNew w_padded] 2 extradim_shape)
            arr_extradim_inv_tr
          letExp (arr_name <> "_inv_tr_init") $
            PrimOp $ Split [] 0 [w] arr_inv_tr

transformKernelBody :: SubExp -> Certificates
                    -> KernelBody Kernels
                    -> BabysitM (KernelBody Kernels)
transformKernelBody num_threads cs (KernelBody stms res) =
  KernelBody <$> mapM transformKernelStm stms <*> pure res
  where boundInKernel = (`elem` HM.keys (scopeOf stms))

        transformKernelStm (SplitArray dest InOrder w elems_per_thread arrs) = do
          (w_padded, padding) <- paddedScanReduceInput w num_threads
          SplitArray dest InOrder w elems_per_thread <$>
            mapM (maybeRearrange w w_padded padding elems_per_thread) arrs
        transformKernelStm stm =
          return stm

        maybeRearrange w w_padded padding elems_per_thread arr
          | boundInKernel arr =
              return arr
          | otherwise =
              rearrangeScanReduceInput cs num_threads padding w w_padded elems_per_thread arr

type ArrayIndexTransform m = VName -> [SubExp] -> m (Maybe (VName, [SubExp]))

traverseKernelBodyArrayIndexes :: (Applicative f, Monad f) =>
                                  ArrayIndexTransform f
                               -> GenKernelBody res Kernels
                               -> f (GenKernelBody res Kernels)
traverseKernelBodyArrayIndexes f (KernelBody kstms kres) =
  KernelBody <$> mapM onStatement kstms <*> pure kres
  where onStatement (Thread threads bnd) =
          Thread threads <$> onBinding bnd
        onStatement (GroupReduce pes w lam input) =
          GroupReduce pes w <$> onLambda lam <*> pure input
        onStatement (GroupStream pes w maxchunk lam accs arrs) =
          GroupStream pes w maxchunk <$> onStreamLambda lam <*> pure accs <*> pure arrs
        onStatement (GroupIf pes cond tb fb) =
          GroupIf pes cond <$> onKernelBody tb <*> onKernelBody fb
        onStatement stm = pure stm

        onLambda lam =
          (\body' -> lam { lambdaBody = body' }) <$>
          onBody (lambdaBody lam)

        onStreamLambda lam =
          (\body' -> lam { groupStreamLambdaBody = body' }) <$>
          onKernelBody (groupStreamLambdaBody lam)

        onBody (Body battr bnds bres) =
          Body battr <$> mapM onBinding bnds <*> pure bres

        onKernelBody = traverseKernelBodyArrayIndexes f

        onBinding (Let pat attr (PrimOp (Index cs arr is))) =
          Let pat attr . oldOrNew <$> f arr is
          where oldOrNew Nothing =
                  PrimOp $ Index cs arr is
                oldOrNew (Just (arr', is')) =
                  PrimOp $ Index cs arr' is'
        onBinding (Let pat attr e) =
          Let pat attr <$> mapExpM mapper e

        mapper = identityMapper { mapOnBody = onBody }

-- Not a hashmap, as SubExp is not hashable.
type Replacements = M.Map (VName, [SubExp]) VName

ensureCoalescedAccess :: MonadBinder m =>
                         ExpMap
                      -> [VName]
                      -> (VName -> Maybe Type)
                      -> ArrayIndexTransform (StateT Replacements m)
ensureCoalescedAccess expmap thread_gids boundOutside arr is = do
  seen <- gets $ M.lookup (arr, is)

  case (seen, boundOutside arr) of
    -- Already took care of this case elsewhere.
    (Just arr', _) ->
      pure $ Just (arr', is)

    (Nothing, Just t)
      -- We are fully indexing the array with thread IDs, but the
      -- indices are in a permuted order.
      | length is == arrayRank t,
        is' <- coalescedIndexes (map Var thread_gids) is,
        Just perm <- is' `isPermutationOf` is,
        perm /= [0..length perm-1] ->
          replace =<< lift (rearrangeInput (nonlinearInMemory arr expmap) perm arr)

      -- We are not fully indexing the array, so we assume
      -- (HEURISTIC!)  that the remaining dimensions will be traversed
      -- sequentially.
      | length is < arrayRank t -> do
          let perm = coalescingPermutation (length is) $ arrayRank t
          replace =<< lift (rearrangeInput (nonlinearInMemory arr expmap) perm arr)

      -- Everything is fine... assuming that the array is in row-major
      -- order!  Make sure that is the case.
      | Just{} <- nonlinearInMemory arr expmap ->
          replace =<< lift (flatInput arr)


    _ -> return Nothing

  where replace arr' = do
          modify $ M.insert (arr, is) arr'
          return $ Just (arr', is)

-- Try to move thread indexes into their proper position.
coalescedIndexes :: (Eq a, Show a) => [a] -> [a] -> [a]
coalescedIndexes tgids is =
  reverse $ foldl move (reverse is) $ zip [0..] (reverse tgids)
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

coalescingPermutation :: Int -> Int -> [Int]
coalescingPermutation num_is rank =
  [num_is..rank-1] ++ [0..num_is-1]

rearrangeInput :: MonadBinder m =>
                  Maybe (Maybe [Int]) -> [Int] -> VName -> m VName
rearrangeInput (Just (Just current_perm)) perm arr
  | current_perm == perm = return arr
rearrangeInput manifest perm arr = do
  let inv_perm = rearrangeInverse perm
  -- We may first manifest the array to ensure that it is flat in
  -- memory.  This is sometimes unnecessary, in which case the copy
  -- will hopefully be removed by the simplifier.
  manifested <- if isJust manifest then flatInput arr else return arr
  transposed <- letExp (baseString arr ++ "_tr") $
                PrimOp $ Rearrange [] perm manifested
  tr_manifested <- letExp (baseString arr ++ "_tr_manifested") $
                   PrimOp $ Copy transposed
  letExp (baseString arr ++ "_inv_tr") $
    PrimOp $ Rearrange [] inv_perm tr_manifested

flatInput :: MonadBinder m =>
             VName -> m VName
flatInput arr =
  letExp (baseString arr ++ "_manifested") $ PrimOp $ Copy arr
