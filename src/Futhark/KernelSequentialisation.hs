module Futhark.KernelSequentialisation
       ( sequentialiseKernels )
       where

import Control.Monad.State
import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.Basic
import Futhark.Tools
import qualified Futhark.FirstOrderTransform as FOT

sequentialiseKernels :: Prog -> Prog
sequentialiseKernels = intraproceduralTransformation transformFunDec

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
transformBinding (Let pat () (LoopOp (Map cs w fun arrs)))
  | Body () [bnd] res <- lambdaBody fun, -- Body has a single binding
    map Var (patternNames $ bindingPattern bnd) == res, -- Returned verbatim
    LoopOp (Map {}) <- bindingExp bnd = do
      fun' <- transformLambda fun
      addBinding $ Let pat () $ LoopOp $ Map cs w fun' arrs

  | otherwise = do
      fun' <- joinBinder $ FOT.transformLambda fun
      addBinding $ Let pat () $ LoopOp $ Map cs w fun' arrs

transformBinding (Let pat () (LoopOp (DoLoop res merge form body))) = do
  body' <- bindingParamTypes (map fst merge) $ bindingIdentTypes form_idents $
           transformBody body
  addBinding $ Let pat () $ LoopOp $ DoLoop res merge form body'
  where form_idents = case form of ForLoop i _ -> [Ident i $ Basic Int]
                                   WhileLoop _ -> []

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
