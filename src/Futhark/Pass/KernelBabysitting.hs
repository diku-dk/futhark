{-# LANGUAGE FlexibleContexts #-}
-- | Do various kernel optimisations - mostly related to coalescing.
module Futhark.Pass.KernelBabysitting
       ( babysitKernels )
       where

import Control.Applicative
import Control.Monad.State
import qualified Data.HashMap.Lazy as HM
import Data.Monoid

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.Kernels
import Futhark.Tools
import Futhark.Pass

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
type ExpMap = HM.HashMap VName Exp

nonlinearInMemory :: VName -> ExpMap -> Bool
nonlinearInMemory name m =
  case HM.lookup name m of
    Just (PrimOp Rearrange{}) -> True
    Just (PrimOp (Reshape _ _ arr)) -> nonlinearInMemory arr m
    _ -> False

transformBinding :: ExpMap -> Binding -> BabysitM ExpMap

transformBinding expmap (Let pat () (DoLoop ctx val form body)) = do
  body' <- localScope (scopeOfFParams $ map fst $ ctx ++ val) $
           localScope (scopeOfLoopForm form) $
           transformBody body
  addBinding $ Let pat () $ DoLoop ctx val form body'
  return expmap

transformBinding expmap (Let pat ()
                         (Op (ReduceKernel cs w kernel_size comm parlam seqlam arrs)))
  | num_groups /= constant (1::Int32) = do
  -- We want to pad and transpose the input arrays.

  (w', kernel_size', arrs') <-
    rearrangeScanReduceInputs comm cs w kernel_size arrs

  parlam' <- transformLambda parlam
  seqlam' <- transformLambda seqlam

  addBinding $ Let pat () $ Op $
    ReduceKernel cs w' kernel_size' comm parlam' seqlam' arrs'
  return expmap
  where num_groups = kernelWorkgroups kernel_size

transformBinding expmap (Let pat ()
                         (Op (ChunkedMapKernel cs w kernel_size o lam arrs))) = do
  -- We want to pad and transpose the input arrays.

  (w', kernel_size', arrs') <-
    rearrangeScanReduceInputs comm cs w kernel_size arrs

  lam' <- transformLambda lam

  addBinding $ Let pat () $ Op $
    ChunkedMapKernel cs w' kernel_size' o lam' arrs'
  return expmap
  where comm = case o of Disorder -> Commutative
                         InOrder -> Noncommutative

transformBinding expmap (Let pat ()
                         (Op (ScanKernel cs w kernel_size lam foldlam nes arrs)))
  | kernelWorkgroups kernel_size /= constant (1::Int32) = do
  -- We want to pad and transpose the input arrays.

  lam' <- transformLambda lam
  foldlam' <- transformLambda foldlam

  addBinding $ Let pat () $ Op $
    ScanKernel cs w kernel_size lam' foldlam' nes arrs
  return expmap

transformBinding expmap (Let pat () (Op (MapKernel cs w i ispace inps returns body))) = do
  body' <- inScopeOf ((i, IndexInfo) :
                      [ (j, IndexInfo) | (j, _) <- ispace ]) $
           inScopeOf inps $
           transformBody body
  -- For every input that is an array, we transpose the next-outermost
  -- and outermost dimension.
  inps' <- rearrangeInputs expmap (map fst ispace) inps
  -- For every return that is an array, we transpose the
  -- next-outermost and outermost dimension.
  let value_elems = patternValueElements pat
  (value_elems', returns') <- rearrangeReturns num_is value_elems returns
  let pat' = Pattern [] value_elems'
  addBinding $ Let pat' () $ Op $ MapKernel cs w i ispace inps' returns' body'
  mapM_ maybeRearrangeResult $ zip3 value_elems value_elems' returns'
  return expmap
  where num_is = length ispace

        maybeRearrangeResult (orig_pat_elem, new_pat_elem, (_, perm))
          | orig_pat_elem == new_pat_elem =
            return ()
          | otherwise =
            addBinding $
            mkLet' [] [patElemIdent orig_pat_elem] $
            PrimOp $ Rearrange [] (rearrangeInverse perm) $
            patElemName new_pat_elem

transformBinding expmap (Let pat () e) = do
  e' <- mapExpM transform e
  addBinding $ Let pat () e'
  return $ HM.fromList [ (name, e') | name <- patternNames pat ] <> expmap

transform :: Mapper Kernels Kernels BabysitM
transform = identityMapper { mapOnBody = transformBody
                           }

transformLambda :: Lambda -> BabysitM Lambda
transformLambda lam = do
  body' <- inScopeOf lam $
           transformBody $ lambdaBody lam
  return lam { lambdaBody = body' }

rearrangeInputs :: ExpMap -> [VName] -> [KernelInput Kernels]
                -> BabysitM [KernelInput Kernels]
rearrangeInputs expmap is = mapM maybeRearrangeInput
  where
    iteratesLastDimension = (== map Var (drop 1 $ reverse is)) .
                            reverse .
                            kernelInputIndices

    maybeRearrangeInput inp =
      case paramType $ kernelInputParam inp of
        Array {} | not $ iteratesLastDimension inp -> do
          arr_t <- lookupType arr
          let perm = coalescingPermutation num_inp_is $ arrayRank arr_t
          rearrangeInput perm inp
        Prim {}
          | Just perm <- map Var is `isPermutationOf` inp_is,
            perm /= [0..length perm-1] ->
              rearrangeInput perm inp
        _ | nonlinearInMemory arr expmap -> do
              flat <- letExp (baseString arr ++ "_flat") $ PrimOp $ Copy arr
              return inp { kernelInputArray = flat }
          | otherwise ->
              return inp
      where arr = kernelInputArray inp
            inp_is = kernelInputIndices inp
            num_inp_is = length inp_is

    rearrangeInput perm inp = do
      let inv_perm = rearrangeInverse perm
      transposed <- letExp (baseString arr ++ "_tr") $
                    PrimOp $ Rearrange [] perm arr
      manifested <- letExp (baseString arr ++ "_tr_manifested") $
                    PrimOp $ Copy transposed
      inv_transposed <- letExp (baseString arr ++ "_inv_tr") $
                        PrimOp $ Rearrange [] inv_perm manifested
      return inp { kernelInputArray = inv_transposed }
      where arr = kernelInputArray inp

coalescingPermutation :: Int -> Int -> [Int]
coalescingPermutation num_is rank =
  [num_is..rank-1] ++ [0..num_is-1]


returnsPermutation :: Int -> Int -> [Int]
returnsPermutation num_is rank =
  [0..num_is-2] ++ [num_is, num_is-1] ++ [num_is+1..rank-1]

rearrangeReturns :: Int -> [PatElem] -> [(Type, [Int])] ->
                    BabysitM ([PatElem], [(Type, [Int])])
rearrangeReturns num_is pat_elems returns =
  unzip <$> zipWithM rearrangeReturn pat_elems returns
  where rearrangeReturn (PatElem name BindVar namet) (t@Array{}, perm) = do
          name_tr <- newVName $ baseString name <> "_tr_res"
          let perm' = rearrangeShape (returnsPermutation num_is $ num_is + arrayRank t) perm
              new_pat_elem = PatElem name_tr BindVar $ rearrangeType perm' namet
          return (new_pat_elem, (t, perm'))
        rearrangeReturn pat_elem (t, perm) =
          return (pat_elem, (t, perm))

rearrangeScanReduceInputs :: Commutativity
                          -> Certificates
                          -> SubExp
                          -> KernelSize
                          -> [VName]
                          -> BabysitM (SubExp, KernelSize, [VName])
rearrangeScanReduceInputs Commutative _ w kernel_size arrs =
  return (w, kernel_size, arrs)

rearrangeScanReduceInputs Noncommutative cs w kernel_size arrs = do
  (kernel_size', w', padding) <- paddedScanReduceInput w kernel_size
  arrs' <- mapM (rearrangeScanReduceInput cs num_threads padding w' $
                 kernelElementsPerThread kernel_size) arrs
  return (w', kernel_size', arrs')
  where num_threads = kernelNumThreads kernel_size

paddedScanReduceInput :: SubExp -> KernelSize
                      -> BabysitM (KernelSize, SubExp, SubExp)
paddedScanReduceInput w kernel_size = do
  w' <- letSubExp "padded_size" =<<
        eRoundToMultipleOf Int32 (eSubExp w) (eSubExp num_threads)
  padding <- letSubExp "padding" $ PrimOp $ BinOp (Sub Int32) w' w

  offset_multiple <-
    letSubExp "offset_multiple" =<<
    eDivRoundingUp Int32 (eSubExp w') (eSubExp num_threads)

  let kernel_size' =
        kernel_size { kernelThreadOffsetMultiple = offset_multiple }
  return (kernel_size', w', padding)
  where num_threads = kernelNumThreads kernel_size

rearrangeScanReduceInput :: MonadBinder m =>
                            Certificates
                         -> SubExp -> SubExp -> SubExp -> SubExp -> VName
                         -> m VName
rearrangeScanReduceInput cs num_threads padding w' elements_per_thread arr = do
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
            PrimOp $ Concat [] arr [arr_padding] w'

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
          letExp (arr_name <> "_inv_tr") $
            PrimOp $ Reshape [] (reshapeOuter [DimNew w'] 2 extradim_shape)
            arr_extradim_inv_tr
