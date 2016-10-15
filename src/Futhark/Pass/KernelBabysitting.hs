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
    Just (Let pat _ (Op (Kernel _ _ ts _))) ->
      nonlinear =<< find ((==name) . patElemName . fst)
      (zip (patternElements pat) ts)
    _ -> Nothing
  where nonlinear (pe, t)
          | inner_r <- arrayRank t, inner_r > 0 = do
              let outer_r = arrayRank (patElemType pe) - inner_r
              return $ Just $ [inner_r..inner_r+outer_r-1] ++ [0..inner_r-1]
          | otherwise = Nothing

transformStm :: ExpMap -> Stm Kernels -> BabysitM ExpMap

transformStm expmap (Let pat () (Op (Kernel cs space ts kbody))) = do
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
  addStm bnd'
  return $ HM.fromList [ (name, bnd') | name <- patternNames pat ] <> expmap
  where num_threads = spaceNumThreads space

transformStm expmap (Let pat () e) = do
  e' <- mapExpM transform e
  let bnd' = Let pat () e'
  addStm bnd'
  return $ HM.fromList [ (name, bnd') | name <- patternNames pat ] <> expmap

transform :: Mapper Kernels Kernels BabysitM
transform = identityMapper { mapOnBody = \scope -> localScope scope . transformBody }

paddedScanReduceInput :: SubExp -> SubExp
                      -> BabysitM (SubExp, SubExp)
paddedScanReduceInput w num_threads = do
  w_padded <- letSubExp "padded_size" =<<
              eRoundToMultipleOf Int32 (eSubExp w) (eSubExp num_threads)
  padding <- letSubExp "padding" $ BasicOp $ BinOp (Sub Int32) w_padded w
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
            BasicOp $ Scratch (elemType arr_t) (shapeDims padding_shape)
          letExp (baseString arr <> "_padded") $
            BasicOp $ Concat [] 0 arr [arr_padding] w_padded

        rearrange arr_name arr_padded row_type = do
          let row_dims = arrayDims row_type
              extradim_shape = Shape $ [num_threads, elements_per_thread] ++ row_dims
              tr_perm = [1] ++ [2..shapeRank extradim_shape-1] ++ [0]
              tr_perm_inv = rearrangeInverse tr_perm
          arr_extradim <-
            letExp (arr_name <> "_extradim") $
            BasicOp $ Reshape cs (map DimNew $ shapeDims extradim_shape) arr_padded
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

transformKernelBody :: SubExp -> Certificates
                    -> KernelBody InKernel
                    -> BabysitM (KernelBody InKernel)
transformKernelBody num_threads cs (KernelBody () stms res) =
  KernelBody () <$> mapM transformKernelStm stms <*> pure res
  where boundInKernel = (`elem` HM.keys (scopeOf stms))

        transformKernelStm (Let pat () (Op (SplitArray InOrder w i num_is elems_per_thread arrs))) = do
          (w_padded, padding) <- paddedScanReduceInput w num_threads
          Let pat () . Op . SplitArray InOrder w i num_is elems_per_thread <$>
            mapM (maybeRearrange w w_padded padding elems_per_thread) arrs

        transformKernelStm stm =
          return stm

        maybeRearrange w w_padded padding elems_per_thread arr
          | boundInKernel arr =
              return arr
          | otherwise =
              rearrangeScanReduceInput cs num_threads padding w w_padded elems_per_thread arr

type ArrayIndexTransform m = VName -> Slice SubExp -> m (Maybe (VName, Slice SubExp))

traverseKernelBodyArrayIndexes :: (Applicative f, Monad f) =>
                                  ArrayIndexTransform f
                               -> KernelBody InKernel
                               -> f (KernelBody InKernel)
traverseKernelBodyArrayIndexes f (KernelBody () kstms kres) =
  KernelBody () <$> mapM onStm kstms <*> pure kres
  where onLambda lam =
          (\body' -> lam { lambdaBody = body' }) <$>
          onBody (lambdaBody lam)

        onStreamLambda lam =
          (\body' -> lam { groupStreamLambdaBody = body' }) <$>
          onBody (groupStreamLambdaBody lam)

        onBody (Body battr bnds bres) =
          Body battr <$> mapM onStm bnds <*> pure bres

        onStm (Let pat attr (BasicOp (Index cs arr is))) =
          Let pat attr . oldOrNew <$> f arr is
          where oldOrNew Nothing =
                  BasicOp $ Index cs arr is
                oldOrNew (Just (arr', is')) =
                  BasicOp $ Index cs arr' is'
        onStm (Let pat attr e) =
          Let pat attr <$> mapExpM mapper e

        mapper = identityMapper { mapOnBody = const onBody
                                , mapOnOp = onOp
                                }

        onOp (GroupReduce w lam input) =
          GroupReduce w <$> onLambda lam <*> pure input
        onOp (GroupStream w maxchunk lam accs arrs) =
           GroupStream w maxchunk <$> onStreamLambda lam <*> pure accs <*> pure arrs
        onOp stm = pure stm


-- Not a hashmap, as SubExp is not hashable.
type Replacements = M.Map (VName, Slice SubExp) VName

ensureCoalescedAccess :: MonadBinder m =>
                         ExpMap
                      -> [VName]
                      -> (VName -> Maybe Type)
                      -> ArrayIndexTransform (StateT Replacements m)
ensureCoalescedAccess expmap thread_gids boundOutside arr slice = do
  seen <- gets $ M.lookup (arr, slice)

  case (seen, boundOutside arr) of
    -- Already took care of this case elsewhere.
    (Just arr', _) ->
      pure $ Just (arr', slice)

    (Nothing, Just t)
      -- We are fully indexing the array with thread IDs, but the
      -- indices are in a permuted order.
      | Just is <- sliceIndices slice,
        length is == arrayRank t,
        is' <- coalescedIndexes (map Var thread_gids) is,
        Just perm <- is' `isPermutationOf` is,
        perm /= [0..length perm-1] ->
          replace =<< lift (rearrangeInput (nonlinearInMemory arr expmap) perm arr)

      -- We are not fully indexing the array, and the indices are not
      -- a proper prefix of the thread indices, so we assume (HEURISTIC!)
      -- that the remaining dimensions will be traversed sequentially.
      | (is, rem_slice) <- splitSlice slice,
        not $ null rem_slice,
        not $ tooSmallSlice (primByteSize (elemType t)) rem_slice,
        is /= map Var (take (length is) thread_gids) ||
         length is == length thread_gids -> do
          let perm = coalescingPermutation (length is) $ arrayRank t
          replace =<< lift (rearrangeInput (nonlinearInMemory arr expmap) perm arr)

      -- Everything is fine... assuming that the array is in row-major
      -- order!  Make sure that is the case.
      | Just{} <- nonlinearInMemory arr expmap ->
          replace =<< lift (flatInput arr)


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
coalescedIndexes :: [SubExp] -> [SubExp] -> [SubExp]
coalescedIndexes tgids is =
  if num_is > 0 && not (null tgids) && (last is == last tgids || any isCt is)
  -- Do Nothing if:
  -- 1. the innermost index is the innermost thread id
  --    (because access is already coalesced)
  -- 2. any of the indices is a constant, i.e., kernel free variable
  --    (because it would transpose a bigger array then needed -- big overhead).
  then is
  -- Otherwise try fix coalescing
  else reverse $ foldl move (reverse is) $ zip [0..] (reverse tgids)
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
  | current_perm == perm = return arr
rearrangeInput manifest perm arr = do
  let inv_perm = rearrangeInverse perm
  -- We may first manifest the array to ensure that it is flat in
  -- memory.  This is sometimes unnecessary, in which case the copy
  -- will hopefully be removed by the simplifier.
  manifested <- if isJust manifest then flatInput arr else return arr
  transposed <- letExp (baseString arr ++ "_tr") $
                BasicOp $ Rearrange [] perm manifested
  tr_manifested <- letExp (baseString arr ++ "_tr_manifested") $
                   BasicOp $ Copy transposed
  letExp (baseString arr ++ "_inv_tr") $
    BasicOp $ Rearrange [] inv_perm tr_manifested

flatInput :: MonadBinder m =>
             VName -> m VName
flatInput arr =
  letExp (baseString arr ++ "_manifested") $ BasicOp $ Copy arr
