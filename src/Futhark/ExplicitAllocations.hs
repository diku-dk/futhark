{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Futhark.ExplicitAllocations
       ( explicitAllocations )
where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.DList as DL
import Data.Loc
import qualified Data.HashMap.Lazy as HM

import qualified Futhark.Representation.Basic as In
import Futhark.MonadFreshNames
import Futhark.Representation.ExplicitMemory
import qualified Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe as IxFun
import Futhark.Tools
import qualified Futhark.Analysis.ScalExp as SE

newtype AllocM a = AllocM (ReaderT (HM.HashMap VName MemSummary)
                           (Binder ExplicitMemory)
                           a)
                 deriving (Applicative, Functor, Monad,
                           MonadReader (HM.HashMap VName MemSummary),
                           MonadWriter (DL.DList Binding))

instance MonadBinder AllocM where
  addBinding = addBindingWriter
  collectBindings = collectBindingsWriter

instance MonadFreshNames AllocM where
  getNameSource = AllocM $ lift getNameSource
  putNameSource = AllocM . lift . putNameSource

instance BindableM AllocM where
  type Lore AllocM = ExplicitMemory

  mkLetM [v] e@(PrimOp (Rearrange _ perm (Var orig) _)) = do
    res <- lookupSummary orig
    case res of
      Just (MemSummary m origfun) -> do
        let ixfun = IxFun.permute origfun perm
            pat' = Pattern [Bindee v $ MemSummary m ixfun]
        return $ Let pat' () e
      _ -> basicMkLetM [v] e

  mkLetM pat e = basicMkLetM pat e

  mkBodyM bnds res = return $ Body () bnds res

basicMkLetM :: [Ident]
            -> Exp
            -> AllocM Binding
basicMkLetM pat e = do
  let (ctx,_) = splitAt (contextSize $ typeOf e) pat
      boundInCtx = flip elem (map identName ctx) . identName
      extDim (Var v)       = boundInCtx v
      extDim (Constant {}) = False
  attr' <- forM pat $ \ident ->
    case identType ident of
      Mem _    -> return Scalar
      Basic _  -> return Scalar
      t@(Array {})
        | any extDim $ arrayDims t ->
          error "Cannot deal with existential memory just yet"
        | otherwise -> do
          size <-
            computeSize loc
            (intconst (basicSize $ elemType t) loc) $
            arrayDims t
          m <- letExp "mem" $ PrimOp $ Alloc size loc
          return $ MemSummary m $
            IxFun.iota $ IxFun.shapeFromSubExps $ arrayDims t
  let pat' = Pattern $ zipWith Bindee pat attr'
  return $ Let pat' () e
  where loc = srclocOf e

lookupSummary :: Ident -> AllocM (Maybe MemSummary)
lookupSummary = asks . HM.lookup . identName

runAllocM :: MonadFreshNames m => AllocM Body -> m Body
runAllocM (AllocM m) = do
  (Body () bnds res, morebnds) <- runBinder'' $ runReaderT m HM.empty
  return $ Body () (morebnds<>bnds) res

explicitAllocations :: In.Prog -> Prog
explicitAllocations prog =
  Prog $ evalState (mapM allocInFun $ In.progFunctions prog) free
  where free = newNameSourceForProg prog

allocInFun :: MonadFreshNames m => In.FunDec -> m FunDec
allocInFun (In.FunDec fname rettype params body loc) = do
  body' <- runAllocM $ allocInBody body
  return $ FunDec fname rettype params body' loc

allocInBody :: In.Body -> AllocM Body
allocInBody (Body _ bnds res) =
  allocInBindings bnds [] $ \bnds' ->
    return $ Body () bnds' res

  where allocInBindings [] bnds' m =
          m bnds'
        allocInBindings (x:xs) bnds' m = do
          allocbnds <- allocInBinding' x
          let summaries =
                HM.fromList
                [(bindeeName bindee, bindeeLore bindee) |
                 bindee <- concatMap (patternBindees . bindingPattern) allocbnds ]
          local (`HM.union` summaries) $
            allocInBindings xs (bnds'++allocbnds) m

        allocInBinding' bnd = do
          (bnd',bnds') <- collectBindings $ allocInBinding bnd
          return $ bnds'<>[bnd']

allocInBinding :: In.Binding -> AllocM Binding
allocInBinding (Let pat _ e) =
  mkLetM (patternIdents pat) =<< allocInExp e

allocInExp :: In.Exp -> AllocM Exp
allocInExp (LoopOp (Map cs f arrs loc)) = do
  let size = arraysSize 0 $ map subExpType arrs
  is <- newIdent "is" (arrayOf (Basic Int) (Shape [size]) Nonunique) loc
  letBind [is] $ PrimOp $ Iota size loc
  i  <- newIdent "i" (Basic Int) loc
  summaries <- liftM (HM.fromList . concat) $
               forM (zip (lambdaParams f) arrs) $ \(p,arr) ->
    if basicType $ identType p then return []
    else
      case arr of
        Constant {} -> return []
        Var v -> do
          res <- lookupSummary v
          case res of
            Just (MemSummary m origfun) ->
              return [(identName p,
                       MemSummary m $ IxFun.applyInd origfun [SE.Id i])]
            _ -> return []
  f' <- local (HM.union summaries) $
        allocInLambda
        f { lambdaParams = i : lambdaParams f
          }
  return $ LoopOp $ Map cs f' (Var is:arrs) loc
allocInExp e = mapExpM alloc e
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
