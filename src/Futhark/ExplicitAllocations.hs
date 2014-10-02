{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Futhark.ExplicitAllocations
       ( explicitAllocations )
where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.DList as DL
import Data.Loc

import qualified Futhark.Representation.Basic as In
import Futhark.MonadFreshNames
import Futhark.Representation.ExplicitMemory
import Futhark.Tools

newtype AllocM a = AllocM (Binder ExplicitMemory a)
                 deriving (Applicative, Functor, Monad,
                           MonadFreshNames,
                           MonadWriter (DL.DList Binding))

instance MonadBinder AllocM where
  addBinding = addBindingWriter
  collectBindings = collectBindingsWriter

instance BindableM AllocM where
  type Lore AllocM = ExplicitMemory
  mkLetM pat e = do
    let (ctx,_) = splitAt (contextSize $ typeOf e) pat
        boundInCtx = flip elem (map identName ctx) . identName
        extDim (Var v)       = boundInCtx v
        extDim (Constant {}) = False
    attr' <- forM pat $ \ident ->
      case identType ident of
        Mem _    -> return Scalar
        Basic _  -> return Scalar
        Array {}
          | any extDim $ arrayDims $ identType ident ->
            error "Cannot deal with existential memory just yet"
          | otherwise -> do
            size <-
              computeSize loc
              (intconst (basicSize $ elemType $ identType ident) loc) $
              arrayDims $ identType ident
            m <- letExp "mem" $ PrimOp $ Alloc size loc
            return $ MemSummary m Identity
    let pat' = Pattern $ zipWith Bindee pat attr'
    return $ Let pat' () e
    where loc = srclocOf e

  mkBodyM bnds res = return $ Body () bnds res

runAllocM :: MonadFreshNames m => AllocM Body -> m Body
runAllocM (AllocM m) = do
  (Body () bnds res, morebnds) <- runBinder'' m
  return $ Body () (morebnds<>bnds) res

explicitAllocations :: In.Prog -> Prog
explicitAllocations prog =
  Prog $ evalState (mapM allocInFun $ In.progFunctions prog) free
  where free = newNameSourceForProg prog

allocInFun :: MonadFreshNames m => In.FunDec -> m FunDec
allocInFun (fname,rettype,params,body,loc) = do
  body' <- runAllocM $ allocInBody body
  return (fname,rettype,params,body',loc)

allocInBody :: In.Body -> AllocM Body
allocInBody (Body _ bnds res) = do
  bnds' <- concat <$> mapM allocInBinding' bnds
  return $ Body () bnds' res
  where allocInBinding' bnd = do
          (bnd',bnds') <- collectBindings $ allocInBinding bnd
          return $ bnds'<>[bnd']

allocInBinding :: In.Binding -> AllocM Binding
allocInBinding (Let pat _ e) =
  mkLetM (patternIdents pat) =<< allocInExp e

allocInExp :: In.Exp -> AllocM Exp
allocInExp = mapExpM alloc
  where alloc = identityMapper { mapOnBinding = allocInBinding
                               , mapOnBody = allocInBody
                               , mapOnLambda = allocInLambda
                               }

allocInLambda :: In.Lambda -> AllocM Lambda
allocInLambda lam = do
  body <- allocInBody $ lambdaBody lam
  return $ lam { lambdaBody = body }

basicSize :: BasicType -> Int
basicSize Int = 4
basicSize Bool = 1
basicSize Char = 1
basicSize Real = 8
basicSize Cert = 1

computeSize :: MonadBinder m => SrcLoc -> SubExp -> [SubExp] -> m SubExp
computeSize _ current [] = return current
computeSize loc current (x:xs) = do
  let pexp = pure . PrimOp . SubExp
  e <- eBinOp Times (pexp current) (pexp x) (Basic Int) loc
  v <- letSubExp "x" e
  computeSize loc v xs
