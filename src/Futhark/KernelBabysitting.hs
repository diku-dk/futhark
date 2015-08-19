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
  inps' <- if any isLoop $ bodyBindings body'
           then rearrangeInputs inps
           else return inps
  addBinding $ Let pat () $ LoopOp $ Kernel cs w i ispace inps' returns body'
  where isLoop (Let _ _ (LoopOp (DoLoop {}))) = True
        isLoop _                              = False

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

rearrangeInputs :: [KernelInput Basic] -> SequentialiseM [KernelInput Basic]
rearrangeInputs = mapM rearrangeInput
  where
    rearrangeInput inp =
      case paramType $ kernelInputParam inp of
        Array {} -> do
          let arr = kernelInputArray inp
              num_is = length $ kernelInputIndices inp
          arr_t <- lookupType arr
          let perm = [0..num_is-2] ++ [num_is, num_is-1] ++ [num_is+1..arrayRank arr_t-1]
              inv_perm = permuteInverse perm
          transposed <- letExp (baseString arr ++ "_tr") $
                        PrimOp $ Rearrange [] perm arr
          manifested <- letExp (baseString arr ++ "_tr_manifested") $
                        PrimOp $ Copy CopyLinear transposed
          inv_transposed <- letExp (baseString arr ++ "_inv_tr") $
                        PrimOp $ Rearrange [] inv_perm manifested
          return inp { kernelInputArray = inv_transposed }
        _ -> return inp
