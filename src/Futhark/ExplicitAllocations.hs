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

  mkLetM [v] e@(PrimOp (Update _ src _ _ _)) = do
    res <- lookupSummary src
    case res of
      Just (MemSummary m origfun) -> do
        let pat' = Pattern [Bindee v $ MemSummary m origfun]
        return $ Let pat' () e
      _ -> basicMkLetM [v] e

  mkLetM [v] e@(PrimOp (SubExp (Var src))) = do
    res <- lookupSummary src
    case res of
      Just (MemSummary m origfun) -> do
        let pat' = Pattern [Bindee v $ MemSummary m origfun]
        return $ Let pat' () e
      _ -> basicMkLetM [v] e

  mkLetM pat e = basicMkLetM pat e

  mkBodyM bnds res = return $ Body () bnds res

basicMkLetM :: [Ident]
            -> Exp
            -> AllocM Binding
basicMkLetM pat e = do
  pat' <- Pattern <$> allocsForPattern pat (typeOf e)
  return $ Let pat' () e

allocForArray :: Type -> SrcLoc -> AllocM (SubExp, Ident)
allocForArray t loc = do
  size <-
    computeSize loc
    (intconst (basicSize $ elemType t) loc) $
    arrayDims t
  m <- letExp "mem" $ PrimOp $ Alloc size loc
  return (size, m)

allocsForPattern :: [Ident] -> ResType -> AllocM [Bindee MemSummary]
allocsForPattern pat (ResType ts) = do
  let (ctxpat,valpat) = splitAt (length pat - length ts) pat
      ctxpat' = [ Bindee ident Scalar | ident <- ctxpat ]
  (vals,(memsizes,mems)) <- runWriterT $ forM (zip valpat ts) $ \(ident, (t, memret)) ->
    let loc = srclocOf ident in
    case memret of
      ReturnsScalar -> return $ Bindee ident Scalar
      ReturnsInBlock mem ->
        return $ Bindee ident $ directIndexFunction mem $ identType ident
      ReturnsInAnyBlock
        | Just shape <- knownShape $ arrayShape t-> do
        (_, m) <- lift $ allocForArray (identType ident `setArrayShape` Shape shape) loc
        return $ Bindee ident $ directIndexFunction m $ identType ident
      _ -> do
        (memsize,mem,ident') <- lift $ memForBindee ident
        tell ([memsize], [mem])
        return ident'
  return $ memsizes <> mems <> ctxpat' <> vals
  where knownShape = mapM known . extShapeDims
        known (Free v) = Just v
        known (Ext {}) = Nothing

memForBindee :: (MonadFreshNames m) =>
                Ident
             -> m (BindeeT MemSummary, BindeeT MemSummary, BindeeT MemSummary)
memForBindee ident = do
  size <- newIdent (memname <> "_size") (Basic Int) loc
  mem <- newIdent memname (Mem $ Var size) loc
  return (Bindee size Scalar,
          Bindee mem Scalar,
          Bindee ident $ directIndexFunction mem t)
  where  memname = baseString (identName ident) <> "_mem"
         t       = identType ident
         loc     = srclocOf ident

directIndexFunction :: Ident -> Type -> MemSummary
directIndexFunction mem t =
  MemSummary mem $ IxFun.iota $ IxFun.shapeFromSubExps $ arrayDims t

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

lookupSummary :: Ident -> AllocM (Maybe MemSummary)
lookupSummary = asks . HM.lookup . identName

bindeeSummary :: BindeeT MemSummary -> (VName, MemSummary)
bindeeSummary bindee = (bindeeName bindee, bindeeLore bindee)

bindeesSummary :: [BindeeT MemSummary] -> HM.HashMap VName MemSummary
bindeesSummary = HM.fromList . map bindeeSummary

runAllocM :: MonadFreshNames m => AllocM FunDec -> m FunDec
runAllocM (AllocM m) = do
  (fundec, morebnds) <- runBinder'' $ runReaderT m HM.empty
  return fundec { funDecBody =
                     let Body () bnds res = funDecBody fundec
                     in Body () (morebnds<>bnds) res
                }

allocInFParams :: [In.FParam] -> ([FParam] -> AllocM a)
               -> AllocM a
allocInFParams params m = do
  (valparams, (memsizeparams, memparams)) <- runWriterT $ forM params $ \param ->
    case bindeeType param of
      Array {} -> do
        (memsize,mem,param') <- lift $ memForBindee $ bindeeIdent param
        tell ([memsize], [mem])
        return param'
      _ -> return param { bindeeLore = Scalar }
  let summary = bindeesSummary valparams
      params' = memsizeparams <> memparams <> valparams
  local (summary `HM.union`) $ m params'

ensureLinearArray :: SubExp -> AllocM (SubExp, Ident, SubExp)
ensureLinearArray (Var v) = do
  res <- lookupSummary v
  case res of
    Just (MemSummary mem ixfun)
      | Mem size <- identType mem,
        IxFun.isLinear ixfun ->
      return (size, mem, Var v)
    _ ->
      -- We need to do a new allocation, copy 'v', and make a new
      -- binding for the size of the memory block.
      allocLinearArray (baseString $ identName v) $ Var v
ensureLinearArray (Constant val loc) =
  allocLinearArray "const_array" $ Constant val loc

allocLinearArray :: String
                 -> SubExp -> AllocM (SubExp, Ident, SubExp)
allocLinearArray s se = do
  (size, mem) <- allocForArray t loc
  v' <- newIdent s t loc
  let pat = Pattern [Bindee v' $ directIndexFunction mem t]
  addBinding $ Let pat () $ PrimOp $ SubExp se
  return (size, mem, Var v')
  where loc = srclocOf se
        t   = subExpType se

funcallArgs :: [(SubExp,Diet)] -> AllocM [(SubExp,Diet)]
funcallArgs args = do
  (valargs, (memsizeargs, memargs)) <- runWriterT $ forM args $ \(arg,d) ->
    case subExpType arg of
      Array {} -> do
        (size, mem, arg') <- lift $ ensureLinearArray arg
        tell ([(size, Observe)], [(Var mem, Observe)])
        return (arg', d)
      _ ->
        return (arg, d)
  return $ memsizeargs <> memargs <> valargs

explicitAllocations :: In.Prog -> Prog
explicitAllocations prog =
  Prog $ evalState (mapM allocInFun $ In.progFunctions prog) free
  where free = newNameSourceForProg prog

memoryInResType :: In.ResType -> ResType
memoryInResType = extResType . resTypeValues

allocInFun :: MonadFreshNames m => In.FunDec -> m FunDec
allocInFun (In.FunDec fname rettype params body loc) =
  runAllocM $ allocInFParams params $ \params' -> do
    body' <- insertBindingsM $ allocInBody body
    return $ FunDec fname (memoryInResType rettype) params' body' loc

allocInBody :: In.Body -> AllocM Body
allocInBody (Body _ bnds res) =
  allocInBindings bnds $ \bnds' ->
    return $ Body () bnds' res

allocInBindings :: [In.Binding] -> ([Binding] -> AllocM a)
                -> AllocM a
allocInBindings origbnds m = allocInBindings' origbnds []
  where allocInBindings' [] bnds' =
          m bnds'
        allocInBindings' (x:xs) bnds' = do
          allocbnds <- allocInBinding' x
          let summaries =
                bindeesSummary $
                concatMap (patternBindees . bindingPattern) allocbnds
          local (`HM.union` summaries) $
            allocInBindings' xs (bnds'++allocbnds)
        allocInBinding' bnd = do
          (bnd',bnds') <- collectBindings $ allocInBinding bnd
          return $ bnds' <> [bnd']

allocInBinding :: In.Binding -> AllocM Binding
allocInBinding (Let pat _ e) =
  mkLetM (patternIdents pat) =<< allocInExp e

funcallSubExps :: [SubExp] -> AllocM [SubExp]
funcallSubExps ses = map fst <$>
                     funcallArgs [ (se, Observe) | se <- ses ]

allocInExp :: In.Exp -> AllocM Exp
allocInExp (LoopOp (DoLoop res merge i bound
                    (Body () bodybnds bodyres) loc)) =
  allocInFParams mergeparams $ \mergeparams' -> do
    mergeinit' <- funcallSubExps mergeinit
    body' <- insertBindingsM $ allocInBindings bodybnds $ \bodybnds' -> do
      ses <- funcallSubExps $ resultSubExps bodyres
      let res' = bodyres { resultSubExps = ses }
      return $ Body () bodybnds' res'
    return $ LoopOp $
      DoLoop res (zip mergeparams' mergeinit') i bound body' loc
  where (mergeparams, mergeinit) = unzip merge
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
allocInExp (LoopOp (Reduce {})) =
  fail "Cannot put explicit allocations in reduce yet."
allocInExp (LoopOp (Scan {})) =
  fail "Cannot put explicit allocations in scan yet."
allocInExp (LoopOp (Redomap {})) =
  fail "Cannot put explicit allocations in redomap yet."
allocInExp (LoopOp (Filter {})) =
  fail "Cannot put explicit allocations in filter yet."
allocInExp (Apply fname args rettype loc) = do
  args' <- funcallArgs args
  return $ Apply fname args' (memoryInResType rettype) loc
allocInExp e = mapExpM alloc e
  where alloc = identityMapper { mapOnBinding = allocInBinding
                               , mapOnBody = allocInBody
                               , mapOnLambda = allocInLambda
                               , mapOnResType = return . memoryInResType
                               , mapOnFParam = fail "Unhandled fparam in ExplicitAllocations"
                               }

allocInLambda :: In.Lambda -> AllocM Lambda
allocInLambda lam = do
  body <- allocInBody $ lambdaBody lam
  return $ lam { lambdaBody = body }
