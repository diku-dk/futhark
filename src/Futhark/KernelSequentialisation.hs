module Futhark.KernelSequentialisation
       ( sequentialiseKernels )
       where

import Control.Applicative
import Control.Monad.State
import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.Basic
import Futhark.Tools
import qualified Futhark.FirstOrderTransform as FOT

sequentialiseKernels :: Prog -> Prog
sequentialiseKernels prog =
  evalState (Prog <$> mapM transformFunDec (progFunctions prog)) src
  where src = newNameSourceForProg prog

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
transformBinding (Let pat () (LoopOp (Map cs fun arrs))) = do
  (fun', bnds) <- runBinder $ FOT.transformLambda fun
  mapM_ addBinding bnds
  addBinding $ Let pat () $ LoopOp $ Map cs fun' arrs

transformBinding (Let pat () e) = do
  e' <- mapExpM transform e
  ((), bnds) <- runBinder $ FOT.transformBinding $ Let pat () e'
  mapM_ addBinding bnds
  where transform = identityMapper { mapOnBody = transformBody
                                   , mapOnLambda = transformLambda
                                   }

transformLambda :: Lambda -> SequentialiseM Lambda
transformLambda lam = do
  body' <- bindingParamTypes (lambdaParams lam) $
           transformBody $ lambdaBody lam
  return lam { lambdaBody = body' }
