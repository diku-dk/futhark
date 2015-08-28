{-# LANGUAGE FlexibleContexts #-}
-- | The OpenCL code generator is a fragile and sensitive thing and it
-- needs a carefully massaged program to work at all.
--
-- This pass will turn SOACs into sequential loops.  The only
-- difference from first order transform is another approach to
-- stream.
module Futhark.KernelBabysitting
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
import qualified Futhark.FirstOrderTransform as FOT

babysitKernels :: Prog -> Prog
babysitKernels = intraproceduralTransformation transformFunDec

transformFunDec :: MonadFreshNames m => FunDec -> m FunDec
transformFunDec fundec = do
  (body', _) <- modifyNameSource $ runState (runBinderT m HM.empty)
  return fundec { funDecBody = body' }
  where m = bindingIdentTypes (map paramIdent $ funDecParams fundec) $
            transformBody $ funDecBody fundec

type SequentialiseM = Binder Basic

transformBody :: Body -> SequentialiseM Body
transformBody (Body () bnds res) = insertBindingsM $ do
  mapM_ transformBinding bnds
  return $ resultBody res

transformBinding :: Binding -> SequentialiseM ()

transformBinding (Let pat () (LoopOp (DoLoop res merge form body))) = do
  body' <- bindingParamTypes (map fst merge) $ bindingIdentTypes form_idents $
           transformBody body
  addBinding $ Let pat () $ LoopOp $ DoLoop res merge form body'
  where form_idents = case form of ForLoop i _ -> [Ident i $ Basic Int]
                                   WhileLoop _ -> []

transformBinding (Let pat () (LoopOp (Kernel cs w i ispace inps returns body))) = do
  body' <- bindingIdentTypes (Ident i (Basic Int) :
                              map ((`Ident` Basic Int) . fst) ispace ++
                              map kernelInputIdent inps) $
           transformBody body
  -- For every input that is an array, we transpose the next-outermost
  -- and outermost dimension.
  inps' <- rearrangeInputs (map fst ispace) inps
  -- For every return that is an array, we transpose the
  -- next-outermost and outermost dimension.
  let value_elems = patternValueElements pat
  (value_elems', returns') <- rearrangeReturns num_is value_elems returns
  let pat' = Pattern [] value_elems'
  addBinding $ Let pat' () $ LoopOp $ Kernel cs w i ispace inps' returns' body'
  mapM_ maybeRearrangeResult $ zip3 value_elems value_elems' returns'
  where num_is = length ispace

        maybeRearrangeResult (orig_pat_elem, new_pat_elem, (_, perm))
          | orig_pat_elem == new_pat_elem =
            return ()
          | otherwise =
            addBinding $
            mkLet' [] [patElemIdent orig_pat_elem] $
            PrimOp $ Rearrange [] (permuteInverse perm) $
            patElemName new_pat_elem

transformBinding (Let pat () (LoopOp (Stream cs w form lam arrs _)))
  | Just accs <- redForm form = do
  let (body_bnds,res) = sequentialStreamWholeArray w accs lam arrs
      reshapeRes t (Var v)
        | null (arrayDims t) = PrimOp $ SubExp $ Var v
        | otherwise          = shapeCoerce cs (arrayDims t) v
      reshapeRes _ se        = PrimOp $ SubExp se
      res_bnds =
        [ mkLet' [] [ident] $ reshapeRes (identType ident) se
        | (ident,se) <- zip (patternValueIdents pat) res]
  mapM_ transformBinding body_bnds
  shapemap <- shapeMapping (patternValueTypes pat) <$> mapM subExpType res
  forM_ (HM.toList shapemap) $ \(name,se) ->
    when (name `elem` patternContextNames pat) $
      transformBinding =<< mkLetNames' [name] (PrimOp $ SubExp se)
  mapM_ transformBinding res_bnds
  where redForm (RedLike _ _ accs) = Just accs
        redForm (Sequential accs)  = Just accs
        redForm _                  = Nothing

transformBinding (Let pat () e) = do
  e' <- mapExpM transform e
  ((), bnds) <- runBinder $ FOT.transformBinding $ Let pat () e'
  mapM_ addBinding bnds

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

rearrangeInputs :: [VName] -> [KernelInput Basic]
                -> SequentialiseM [KernelInput Basic]
rearrangeInputs is = mapM rearrangeInput
  where
    iteratesLastDimension = (== map Var (drop 1 $ reverse is)) .
                            reverse .
                            kernelInputIndices

    rearrangeInput inp =
      case paramType $ kernelInputParam inp of
        Array {} | not $ iteratesLastDimension inp -> do
          let arr = kernelInputArray inp
              num_is = length $ kernelInputIndices inp
          arr_t <- lookupType arr
          let perm = coalescingPermutation num_is $ arrayRank arr_t
              inv_perm = permuteInverse perm
          transposed <- letExp (baseString arr ++ "_tr") $
                        PrimOp $ Rearrange [] perm arr
          manifested <- letExp (baseString arr ++ "_tr_manifested") $
                        PrimOp $ Copy transposed
          inv_transposed <- letExp (baseString arr ++ "_inv_tr") $
                        PrimOp $ Rearrange [] inv_perm manifested
          return inp { kernelInputArray = inv_transposed }
        _ -> return inp

coalescingPermutation :: Int -> Int -> [Int]
coalescingPermutation num_is rank =
  [0..num_is-2] ++ [num_is, num_is-1] ++ [num_is+1..rank-1]

rearrangeReturns :: Int -> [PatElem] -> [(Type, [Int])] ->
                    SequentialiseM ([PatElem], [(Type, [Int])])
rearrangeReturns num_is pat_elems returns =
  unzip <$> zipWithM rearrangeReturn pat_elems returns
  where rearrangeReturn (PatElem ident BindVar ()) (t@(Array {}), perm) = do
          name_tr <- newVName $ baseString (identName ident) <> "_tr_res"
          let perm' = permuteShape (coalescingPermutation num_is $ num_is + arrayRank t) perm
              ident' = Ident name_tr $ rearrangeType perm' $ identType ident
              new_pat_elem = PatElem ident' BindVar ()
          return (new_pat_elem, (t, perm'))
        rearrangeReturn pat_elem (t, perm) =
          return (pat_elem, (t, perm))
