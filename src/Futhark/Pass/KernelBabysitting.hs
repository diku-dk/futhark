{-# LANGUAGE FlexibleContexts #-}
-- | The OpenCL code generator is a fragile and sensitive thing and it
-- needs a carefully massaged program to work at all.
--
-- This pass will turn SOACs into sequential loops.  The only
-- difference from first order transform is another approach to
-- stream.
module Futhark.Pass.KernelBabysitting
       ( babysitKernels )
       where

import Control.Applicative
import Control.Monad.State
import qualified Data.HashMap.Lazy as HM
import Data.Monoid

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.Basic
import Futhark.Tools
import Futhark.Pass
import qualified Futhark.Transform.FirstOrderTransform as FOT

babysitKernels :: Pass Basic Basic
babysitKernels =
  Pass { passName = "babysit kernels"
       , passDescription = "Remove stream and transpose kernel input arrays for better performance."
       , passFunction = intraproceduralTransformation transformFunDec
       }

transformFunDec :: MonadFreshNames m => FunDec -> m FunDec
transformFunDec fundec = do
  (body', _) <- modifyNameSource $ runState (runBinderT m HM.empty)
  return fundec { funDecBody = body' }
  where m = bindingIdentTypes (map paramIdent $ funDecParams fundec) $
            transformBody $ funDecBody fundec

type SequentialiseM = Binder Basic

transformBody :: Body -> SequentialiseM Body
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
    Just (PrimOp (Rearrange {})) -> True
    Just (PrimOp (Reshape _ _ arr)) -> nonlinearInMemory arr m
    _ -> False

transformBinding :: ExpMap -> Binding -> SequentialiseM ExpMap

transformBinding expmap (Let pat () (LoopOp (DoLoop res merge form body))) = do
  body' <- bindingParamTypes (map fst merge) $ bindingIdentTypes form_idents $
           transformBody body
  addBinding $ Let pat () $ LoopOp $ DoLoop res merge form body'
  return expmap
  where form_idents = case form of ForLoop i _ -> [Ident i $ Basic Int]
                                   WhileLoop _ -> []

transformBinding expmap (Let pat ()
                         (LoopOp (ReduceKernel cs w kernel_size parlam seqlam nes arrs)))
  | num_chunks /= Constant (IntVal 1) = do
  -- We want to pad and transpose the input arrays.

  num_threads <- letSubExp "num_threads" $
                 PrimOp $ BinOp Times num_chunks group_size Int

  w' <- letSubExp "padded_size" =<<
        eRoundToMultipleOf (eSubExp w) (eSubExp num_threads)
  elements_per_thread <- letSubExp "elements_per_thread" $
                         PrimOp $ BinOp Quot w' num_threads Int

  padding <- letSubExp "padding" $ PrimOp $ BinOp Minus w' w Int

  offset_multiple <-
    letSubExp "offset_multiple" =<<
    eDivRoundingUp (eSubExp w') (eSubExp num_threads)

  let kernel_size' =
        kernel_size { kernelThreadOffsetMultiple = offset_multiple }

  arrs' <- mapM (padAndRearrangeIf2D num_threads elements_per_thread padding w') arrs

  parlam' <- transformLambda parlam
  seqlam' <- transformLambda seqlam

  addBinding $ Let pat () $ LoopOp $
    ReduceKernel cs w kernel_size' parlam' seqlam' nes arrs'
  return expmap
  where num_chunks = kernelWorkgroups kernel_size
        group_size = kernelWorkgroupSize kernel_size

        padAndRearrangeIf2D num_threads elements_per_thread padding w' arr = do
          arr_t <- lookupType arr
          if arrayRank arr_t == 1
            then padAndRearrange num_threads elements_per_thread padding w' arr arr_t
            else return arr

        padAndRearrange num_threads elements_per_thread padding w' arr arr_t = do
          let arr_shape = arrayShape arr_t
              row_dims = arrayDims (rowType arr_t)
              padding_shape = arr_shape `setOuterDim` padding
              extradim_shape = Shape $
                               [num_threads, elements_per_thread] ++ row_dims
              tr_perm = [1,0] ++ [2..shapeRank extradim_shape-1]
          arr_padding <-
            letExp (baseString arr <> "_padding") $
            PrimOp $ Scratch (elemType arr_t) (shapeDims padding_shape)
          arr_padded <-
            letExp (baseString arr <> "_padded") $
            PrimOp $ Concat [] arr [arr_padding] w'
          arr_extradim <-
            letExp (baseString arr <> "_extradim") $
            PrimOp $ Reshape cs (map DimNew $ shapeDims extradim_shape) arr_padded
          arr_extradim_tr <-
            letExp (baseString arr <> "_extradim_tr") $
            PrimOp $ Rearrange [] tr_perm arr_extradim
          arr_extradim_manifested <-
            letExp (baseString arr <> "_extradim_manifested") $
            PrimOp $ Copy arr_extradim_tr
          arr_extradim_inv_tr <-
            letExp (baseString arr <> "_extradim_inv_tr") $
            PrimOp $ Rearrange [] tr_perm arr_extradim_manifested
          letExp (baseString arr <> "_inv_tr") $
            PrimOp $ Reshape [] (reshapeOuter [DimNew w'] 2 extradim_shape) arr_extradim_inv_tr


transformBinding expmap (Let pat () (LoopOp (Kernel cs w i ispace inps returns body))) = do
  body' <- bindingIdentTypes (Ident i (Basic Int) :
                              map ((`Ident` Basic Int) . fst) ispace ++
                              map kernelInputIdent inps) $
           transformBody body
  -- For every input that is an array, we transpose the next-outermost
  -- and outermost dimension.
  inps' <- rearrangeInputs expmap (map fst ispace) inps
  -- For every return that is an array, we transpose the
  -- next-outermost and outermost dimension.
  let value_elems = patternValueElements pat
  (value_elems', returns') <- rearrangeReturns num_is value_elems returns
  let pat' = Pattern [] value_elems'
  addBinding $ Let pat' () $ LoopOp $ Kernel cs w i ispace inps' returns' body'
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

transformBinding expmap (Let pat () (LoopOp (Stream cs w form lam arrs _)))
  | Just accs <- redForm form = do
  let (body_bnds,res) = sequentialStreamWholeArray w accs lam arrs
      reshapeRes t (Var v)
        | null (arrayDims t) = PrimOp $ SubExp $ Var v
        | otherwise          = shapeCoerce cs (arrayDims t) v
      reshapeRes _ se        = PrimOp $ SubExp se
      res_bnds =
        [ mkLet' [] [ident] $ reshapeRes (identType ident) se
        | (ident,se) <- zip (patternValueIdents pat) res]
  expmap' <- foldM transformBinding expmap body_bnds
  shapemap <- shapeMapping (patternValueTypes pat) <$> mapM subExpType res
  forM_ (HM.toList shapemap) $ \(name,se) ->
    when (name `elem` patternContextNames pat) $
      void $ transformBinding expmap =<< mkLetNames' [name] (PrimOp $ SubExp se)
  foldM transformBinding expmap' res_bnds
  where redForm (RedLike _ _ accs) = Just accs
        redForm (Sequential accs)  = Just accs
        redForm _                  = Nothing

transformBinding expmap (Let pat () e) = do
  e' <- mapExpM transform e
  ((), bnds) <- runBinder $ FOT.transformBinding $ Let pat () e'
  foldM addBinding' expmap bnds
  where addBinding' expmap' bnd = do
          addBinding bnd
          return $
            HM.fromList [ (name, bindingExp bnd)
                        | name <- patternNames $ bindingPattern bnd ]
            <> expmap'

transform :: Mapper Basic Basic SequentialiseM
transform = identityMapper { mapOnBody = transformBody
                           , mapOnLambda = transformLambda
                           , mapOnExtLambda = transformExtLambda
                           }

transformLambda :: Lambda -> SequentialiseM Lambda
transformLambda lam = do
  body' <- bindingParamTypes (lambdaParams lam) $
           transformBody $ lambdaBody lam
  return lam { lambdaBody = body' }

transformExtLambda :: ExtLambda -> SequentialiseM ExtLambda
transformExtLambda lam = do
  body' <- bindingParamTypes (extLambdaParams lam) $
           transformBody $ extLambdaBody lam
  return lam { extLambdaBody = body' }

rearrangeInputs :: ExpMap -> [VName] -> [KernelInput Basic]
                -> SequentialiseM [KernelInput Basic]
rearrangeInputs expmap is = mapM maybeRearrangeInput
  where
    iteratesLastDimension = (== map Var (drop 1 $ reverse is)) .
                            reverse .
                            kernelInputIndices

    maybeRearrangeInput inp =
      case paramType $ kernelInputParam inp of
        Array {}
          | not $ iteratesLastDimension inp -> do
              arr_t <- lookupType arr
              let perm = coalescingPermutation num_inp_is $ arrayRank arr_t
              rearrangeInput perm inp
        Basic {}
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
  [0..num_is-2] ++ [num_is, num_is-1] ++ [num_is+1..rank-1]

rearrangeReturns :: Int -> [PatElem] -> [(Type, [Int])] ->
                    SequentialiseM ([PatElem], [(Type, [Int])])
rearrangeReturns num_is pat_elems returns =
  unzip <$> zipWithM rearrangeReturn pat_elems returns
  where rearrangeReturn (PatElem ident BindVar ()) (t@(Array {}), perm) = do
          name_tr <- newVName $ baseString (identName ident) <> "_tr_res"
          let perm' = rearrangeShape (coalescingPermutation num_is $ num_is + arrayRank t) perm
              ident' = Ident name_tr $ rearrangeType perm' $ identType ident
              new_pat_elem = PatElem ident' BindVar ()
          return (new_pat_elem, (t, perm'))
        rearrangeReturn pat_elem (t, perm) =
          return (pat_elem, (t, perm))
