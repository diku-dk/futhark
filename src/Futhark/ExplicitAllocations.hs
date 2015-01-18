{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Futhark.ExplicitAllocations
       ( explicitAllocations
       , simplifiable
       )
where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.DList as DL
import Data.List
import Data.Maybe
import Data.Loc
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import qualified Futhark.Representation.Basic as In
import Futhark.Representation.Aliases
  (Aliases,
   mkAliasedBody,
   mkAliasedLetBinding,
   removeExpAliases,
   removePatternAliases)
import Futhark.MonadFreshNames
import Futhark.Representation.ExplicitMemory
import qualified Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe as IxFun
import Futhark.Tools
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.ScalExp as SE
import Futhark.Optimise.Simplifier.Simplifiable (Simplifiable (..))
import qualified Futhark.Optimise.Simplifier.Engine as Engine

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

  mkLetM pat e = return $ Let pat () e

  mkLetNamesM names e = do
    (ts',sizes) <- instantiateShapes' loc $ expExtType e
    let vals = [ Ident name t loc | (name, t) <- zip names ts' ]
    res <- specialisedMkLetM sizes vals e
    case res of Just bnd -> return bnd
                Nothing  -> basicMkLetM sizes vals e
    where loc = srclocOf e

  mkBodyM bnds res = return $ Body () bnds res

basicMkLetM :: [Ident]
            -> [Ident]
            -> Exp
            -> AllocM Binding
basicMkLetM sizes vals e = do
  t <- expReturns lookupSummary' e
  pat' <- Pattern <$> allocsForPattern sizes vals t
  return $ Let pat' () e

specialisedMkLetM :: [Ident] -> [Ident] -> Exp -> AllocM (Maybe Binding)
specialisedMkLetM [] [Ident name _ _] e@(PrimOp (Update _ src _ _ _)) = do
  res <- lookupSummary src
  case res of
    Just (MemSummary m origfun) -> do
      let ident = Ident name (identType src) $ srclocOf src
          pat' = Pattern [Bindee ident $ MemSummary m origfun]
      return $ Just $ Let pat' () e
    _ -> return Nothing

specialisedMkLetM [] [Ident name _ _] e@(PrimOp (SubExp (Var src))) = do
  res <- lookupSummary src
  case res of
    Just (MemSummary m origfun) -> do
      let ident = Ident name (identType src) $ srclocOf src
          pat' = Pattern [Bindee ident $ MemSummary m origfun]
      return $ Just $ Let pat' () e
    _ -> return Nothing

specialisedMkLetM [] [ident] e@(PrimOp (Index _ src is _)) = do
  summary <- lookupSummary' $ identName src
  case summary of
    MemSummary m ixfun -> do
      let annot
            | arrayRank (identType src) == length is =
              Scalar
            | otherwise =
              MemSummary m $ IxFun.applyInd ixfun $
              map SE.subExpToScalExp is
          pat = Pattern [Bindee ident annot]
      return $ Just $ Let pat () e
    _ -> fail $ "Invalid memory summary for " ++ pretty src ++ ": " ++
         pretty summary

specialisedMkLetM [] [ident1, ident2] e@(PrimOp (Split _ n a _ _)) = do
  summary <- lookupSummary' $ identName a
  case summary of
    MemSummary m ixfun -> do
      let t = identType ident1
          offset = sliceOffset (arrayShape t) [SE.subExpToScalExp n]
          bindee1 = Bindee ident1 $ MemSummary m ixfun
          bindee2 = Bindee ident2 $ MemSummary m $ IxFun.offset ixfun offset
          pat = [bindee1, bindee2]
      return $ Just $ Let (Pattern pat) () e
    _ -> fail $ "Invalid memory summary for " ++ pretty a ++ ": " ++
         pretty summary

specialisedMkLetM [] [ident] e@(PrimOp (Reshape _ _ a _)) = do
  summary <- lookupSummary' $ identName a
  case summary of
    MemSummary m ixfun -> -- FIXME: is this really the right index function?
      return $ Just $ Let (Pattern [Bindee ident $ MemSummary m ixfun]) () e
    _ -> fail $ "Invalid memory summary for " ++ pretty a ++ ": " ++
         pretty summary

specialisedMkLetM _ _ _ = return Nothing

allocForArray :: Type -> SrcLoc -> AllocM (SubExp, Ident)
allocForArray t loc = do
  size <-
    computeSize loc
    (intconst (basicSize $ elemType t) loc) $
    arrayDims t
  m <- letExp "mem" $ PrimOp $ Alloc size loc
  return (size, m)

allocsForPattern :: [Ident] -> [Ident] -> [ExpReturns]
                 -> AllocM [Bindee MemSummary]
allocsForPattern sizeidents validents rts = do
  let sizes' = [ Bindee size Scalar | size <- sizeidents ]
  (vals,(memsizes,mems)) <- runWriterT $ forM (zip validents rts) $ \(ident, rt) ->
    let loc = srclocOf ident in
    case rt of
      ReturnsScalar _ -> return $ Bindee ident Scalar
      ReturnsMemory _ -> return $ Bindee ident Scalar
      ReturnsArray _ _ _ (Just (ReturnsInBlock mem ixfun)) ->
        return $ Bindee ident $ MemSummary mem ixfun
      ReturnsArray _ extshape _ Nothing
        | Just shape <- knownShape extshape -> do
        (_, m) <- lift $ allocForArray (identType ident `setArrayShape` Shape shape) loc
        return $ Bindee ident $ directIndexFunction m $ identType ident
      _ -> do
        (memsize,mem,ident') <- lift $ memForBindee ident
        tell ([memsize], [mem])
        return ident'
  return $ memsizes <> mems <> sizes' <> vals
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

sliceOffset :: Shape -> [SE.ScalExp] -> SE.ScalExp
sliceOffset shape is =
  SE.ssum $ zipWith SE.STimes is sliceSizes
  where sliceSizes =
          map SE.sproduct $
          drop 1 $ tails $ map SE.subExpToScalExp $ shapeDims shape

computeSize :: MonadBinder m => SrcLoc -> SubExp -> [SubExp] -> m SubExp
computeSize _ current [] = return current
computeSize loc current (x:xs) = do
  let pexp = pure . PrimOp . SubExp
  e <- eBinOp Times (pexp current) (pexp x) (Basic Int) loc
  v <- letSubExp "x" e
  computeSize loc v xs

lookupSummary :: Ident -> AllocM (Maybe MemSummary)
lookupSummary = asks . HM.lookup . identName

lookupSummary' :: VName -> AllocM MemSummary
lookupSummary' name = do
  res <- asks $ HM.lookup name
  case res of
    Just summary -> return summary
    Nothing ->
      fail $ "No memory summary for variable " ++ pretty name

bindeeSummary :: BindeeT MemSummary -> (VName, MemSummary)
bindeeSummary bindee = (bindeeName bindee, bindeeLore bindee)

bindeesSummary :: [BindeeT MemSummary] -> HM.HashMap VName MemSummary
bindeesSummary = HM.fromList . map bindeeSummary

runAllocM :: MonadFreshNames m => AllocM a -> m a
runAllocM = runAllocMWithEnv HM.empty

runAllocMWithEnv :: MonadFreshNames m =>
                    HM.HashMap VName MemSummary
                 -> AllocM a
                 -> m a
runAllocMWithEnv env (AllocM m) = liftM fst $ runBinder'' $ runReaderT m env

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

ensureDirectArray :: Ident -> AllocM (SubExp, Ident, SubExp)
ensureDirectArray v = do
  res <- lookupSummary v
  case res of
    Just (MemSummary mem ixfun)
      | Mem size <- identType mem,
        IxFun.isDirect ixfun ->
      return (size, mem, Var v)
    _ ->
      -- We need to do a new allocation, copy 'v', and make a new
      -- binding for the size of the memory block.
      allocLinearArray (baseString $ identName v) $ Var v

allocLinearArray :: String
                 -> SubExp -> AllocM (SubExp, Ident, SubExp)
allocLinearArray s se = do
  (size, mem) <- allocForArray t loc
  v' <- newIdent s t loc
  let pat = Pattern [Bindee v' $ directIndexFunction mem t]
  addBinding $ Let pat () $ PrimOp $ Copy se loc
  return (size, mem, Var v')
  where loc = srclocOf se
        t   = subExpType se

funcallArgs :: [(SubExp,Diet)] -> AllocM [(SubExp,Diet)]
funcallArgs args = do
  (valargs, (memsizeargs, memargs)) <- runWriterT $ forM args $ \(arg,d) ->
    case (arg, subExpType arg) of
      (Var v, Array {}) -> do
        (size, mem, arg') <- lift $ ensureDirectArray v
        tell ([(size, Observe)], [(Var mem, Observe)])
        return (arg', d)
      _ ->
        return (arg, d)
  return $ memsizeargs <> memargs <> valargs

explicitAllocations :: In.Prog -> Prog
explicitAllocations prog =
  Prog $ evalState (mapM allocInFun $ In.progFunctions prog) free
  where free = newNameSourceForProg prog

memoryInRetType :: In.RetType -> RetType
memoryInRetType (ExtRetType ts) =
  evalState (mapM addAttr ts) $ startOfFreeIDRange ts
  where addAttr (Basic t) = return $ ReturnsScalar t
        addAttr (Mem _)  = fail "memoryInRetType: too much memory"
        addAttr (Array bt shape u) = do
          i <- get
          put $ i + 1
          return $ ReturnsArray bt shape u $ ReturnsNewBlock i

startOfFreeIDRange :: [ExtType] -> Int
startOfFreeIDRange = (1+) . HS.foldl' max 0 . shapeContext

allocInFun :: MonadFreshNames m => In.FunDec -> m FunDec
allocInFun (In.FunDec fname rettype params body loc) =
  runAllocM $ allocInFParams params $ \params' -> do
    body' <- insertBindingsM $ allocInBody body
    return $ FunDec fname (memoryInRetType rettype) params' body' loc

allocInBody :: In.Body -> AllocM Body
allocInBody (Body _ bnds res) =
  allocInBindings bnds $ \bnds' -> do
    (ses, allocs) <- collectBindings $ mapM ensureDirect $ resultSubExps res
    return $ Body () (bnds'<>allocs) res { resultSubExps = ses }
  where ensureDirect se@(Constant {}) = return se
        ensureDirect (Var v)
          | basicType $ identType v = return $ Var v
          | otherwise = do (_, _, v') <- ensureDirectArray v
                           return v'

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
allocInBinding (Let pat _ e) = do
  e' <- allocInExp e
  t <- expReturns lookupSummary' e'
  let (sizeidents, validents) =
        splitAt (patternSize pat - length (expExtType e)) $
        patternIdents pat
  res <- specialisedMkLetM sizeidents validents e'
  case res of
    Just bnd -> return bnd
    Nothing -> do
      pat' <- Pattern <$> allocsForPattern sizeidents validents t
      mkLetM pat' e'

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
  let size = arraysSize 0 $ map identType arrs
  is <- letExp "is" $ PrimOp $ Iota size loc
  i  <- newIdent "i" (Basic Int) loc
  summaries <- liftM (HM.fromList . concat) $
               forM (zip (lambdaParams f) arrs) $ \(p,arr) ->
    if basicType $ identType p then return []
    else do
      res <- lookupSummary arr
      case res of
        Just (MemSummary m origfun) ->
          return [(identName p,
                   MemSummary m $ IxFun.applyInd origfun [SE.Id i])]
        _ -> return []
  f' <- local (HM.union summaries) $
        allocInLambda
        f { lambdaParams = i : lambdaParams f
          }
  return $ LoopOp $ Map cs f' (is:arrs) loc
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
  return $ Apply fname args' (memoryInRetType rettype) loc
allocInExp e = mapExpM alloc e
  where alloc = identityMapper { mapOnBinding = allocInBinding
                               , mapOnBody = allocInBody
                               , mapOnLambda = allocInLambda
                               , mapOnRetType = return . memoryInRetType
                               , mapOnFParam = fail "Unhandled fparam in ExplicitAllocations"
                               }

allocInLambda :: In.Lambda -> AllocM Lambda
allocInLambda lam = do
  body <- allocInBody $ lambdaBody lam
  return $ lam { lambdaBody = body }

vtableToAllocEnv :: ST.SymbolTable (Aliases ExplicitMemory)
                 -> HM.HashMap VName MemSummary
vtableToAllocEnv = HM.fromList . mapMaybe entryToMemSummary .
                   HM.toList . ST.bindings
  where entryToMemSummary (k,entry) = do
          summary <- (snd <$> ST.entryLetBoundLore entry) <|>
                     ST.entryFParamLore entry
          return (k, summary)

simplifiable :: (Engine.MonadEngine m,
                 Engine.InnerLore m ~ ExplicitMemory) =>
                Simplifiable m
simplifiable =
  Simplifiable mkLetS' mkBodyS' mkLetNamesS'
  simplifyMemSummary simplifyMemSummary
  simplifyRetType'
  where mkLetS' vtable pat e = do
          Let pat' lore _ <- runAllocMWithEnv env $
                             mkLetM (removePatternAliases pat) $
                             removeExpAliases e
          return $ mkAliasedLetBinding pat' lore e
          where env = vtableToAllocEnv vtable

        mkBodyS' _ bnds res = return $ mkAliasedBody () bnds res

        mkLetNamesS' vtable names e = do
          Let pat' lore _ <-
            runAllocMWithEnv env $ mkLetNamesM names $ removeExpAliases e
          return $ mkAliasedLetBinding pat' lore e
          where env = vtableToAllocEnv vtable

        simplifyMemSummary Scalar = return Scalar
        simplifyMemSummary (MemSummary ident ixfun) =
          MemSummary <$> Engine.simplifyIdent ident <*> pure ixfun

        simplifyRetType' = mapM simplifyReturns
          where simplifyReturns (ReturnsScalar bt) =
                  return $ ReturnsScalar bt
                simplifyReturns (ReturnsArray bt shape u ret) =
                  ReturnsArray bt <$>
                  Engine.simplifyExtShape shape <*>
                  pure u <*>
                  simplifyMemReturn ret
                simplifyReturns (ReturnsMemory size) =
                  ReturnsMemory <$> Engine.simplifySubExp size
                simplifyMemReturn (ReturnsNewBlock i) =
                  return $ ReturnsNewBlock i
                simplifyMemReturn (ReturnsInBlock v ixfun) =
                  ReturnsInBlock <$> Engine.simplifyIdent v <*>
                  pure ixfun
