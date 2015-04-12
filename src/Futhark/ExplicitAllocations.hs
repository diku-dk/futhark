{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, FlexibleContexts #-}
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
import Data.Maybe
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import qualified Futhark.Representation.Basic as In
import Futhark.Optimise.Simplifier.Lore
  (Wise,
   mkWiseBody,
   mkWiseLetBinding,
   removeExpWisdom,
   removePatternWisdom)
import Futhark.MonadFreshNames
import Futhark.Representation.ExplicitMemory
import qualified Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe as IxFun
import Futhark.Tools
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.Optimise.Simplifier.Simplify (SimpleOps (..))
import qualified Futhark.Optimise.Simplifier.Engine as Engine

data Entry = Entry { entryMemSummary :: MemSummary
                   , entryType :: Type
                   }

newtype AllocM a = AllocM (BinderT ExplicitMemory
                           (ReaderT (HM.HashMap VName Entry)
                            (State VNameSource))
                           a)
                 deriving (Applicative, Functor, Monad,
                           MonadReader (HM.HashMap VName Entry),
                           MonadWriter (DL.DList Binding))

instance MonadBinder AllocM where
  type Lore AllocM = ExplicitMemory

  mkLetM pat e = return $ Let pat () e

  mkLetNamesM names e = do
    (ts',sizes) <- instantiateShapes' =<< expExtType e
    let identForBindage name t BindVar =
          pure (Ident name t, BindVar)
        identForBindage name _ bindage@(BindInPlace _ src _) = do
          t <- lookupType src
          pure (Ident name t, bindage)
    vals <- sequence [ identForBindage name t bindage  |
                       ((name,bindage), t) <- zip names ts' ]
    basicMkLetM sizes vals e

  mkBodyM bnds res = return $ Body () bnds res

  addBinding =
    AllocM . addBinderBinding
  collectBindings (AllocM m) =
    AllocM $ collectBinderBindings m

instance HasTypeEnv AllocM where
  askTypeEnv = HM.map entryType <$> ask

instance MonadFreshNames AllocM where
  getNameSource = AllocM $ lift getNameSource
  putNameSource = AllocM . lift . putNameSource

basicMkLetM :: [Ident]
            -> [(Ident,Bindage)]
            -> Exp
            -> AllocM Binding
basicMkLetM shapes validents e = do
  (bnd, extrabnds) <- allocsForBinding shapes validents e
  case extrabnds of
    [] -> return bnd
    _  -> fail $ "Cannot make allocations for pattern of " ++ pretty e

allocForArray :: Type -> AllocM (SubExp, VName)
allocForArray t = do
  size <-
    computeSize
    (intconst (basicSize $ elemType t)) $
    arrayDims t
  m <- letExp "mem" $ PrimOp $ Alloc size
  return (size, m)

allocsForBinding :: [Ident] -> [(Ident,Bindage)] -> Exp
                 -> AllocM (Binding, [Binding])
allocsForBinding sizeidents validents e = do
  rts <- expReturns lookupSummary' e
  (patElems, extrabnds) <- allocsForPattern sizeidents validents rts
  return (Let (Pattern patElems) () e,
          extrabnds)

allocsForPattern :: [Ident] -> [(Ident,Bindage)] -> [ExpReturns]
                 -> AllocM ([PatElem], [Binding])
allocsForPattern sizeidents validents rts = do
  let sizes' = [ PatElem size BindVar Scalar | size <- sizeidents ]
  (vals,(memsizes, mems, extrabnds)) <-
    runWriterT $ forM (zip validents rts) $ \((ident,bindage), rt) ->
    case rt of
      ReturnsScalar _ -> do
        summary <- lift $ summaryForBindage (identType ident) bindage
        return $ PatElem ident bindage summary

      ReturnsMemory _ ->
        return $ PatElem ident bindage Scalar

      ReturnsArray _ _ u (Just (ReturnsInBlock mem ixfun)) ->
        case bindage of
          BindVar ->
            return $ PatElem ident bindage $ MemSummary mem ixfun
          BindInPlace _ src is -> do
            (destmem,destixfun) <- lift $ lookupArraySummary' src
            if destmem == mem && destixfun == ixfun then
              return $ PatElem ident bindage $ MemSummary mem ixfun
              else do
              -- The expression returns in some memory, but we want to
              -- put the result somewhere else.  This means we need to
              -- store it in the memory it wants to first, then copy
              -- it elsewhere in an extra binding.
              ident' <- lift $
                        newIdent (baseString (identName ident)<>"_buffer")
                        (stripArray (length is) $ identType ident
                         `setUniqueness` u)
              tell ([], [],
                    [Let (Pattern [PatElem ident bindage $
                                   MemSummary destmem destixfun]) () $
                     PrimOp $ Copy $ Var $ identName ident'])
              return $ PatElem ident' BindVar $
                MemSummary mem ixfun

      ReturnsArray _ extshape _ Nothing
        | Just _ <- knownShape extshape -> do
          summary <- lift $ summaryForBindage (identType ident) bindage
          return $ PatElem ident bindage summary

      _ -> do
        (memsize,mem,(ident',lore)) <- lift $ memForBindee ident
        tell ([PatElem memsize BindVar Scalar],
              [PatElem mem     BindVar Scalar],
              [])
        return $ PatElem ident' bindage lore
  return (memsizes <> mems <> sizes' <> vals,
          extrabnds)
  where knownShape = mapM known . extShapeDims
        known (Free v) = Just v
        known (Ext {}) = Nothing

summaryForBindage :: Type -> Bindage
                  -> AllocM MemSummary
summaryForBindage t BindVar
  | basicType t =
    return Scalar
  | otherwise = do
    (_, m) <- allocForArray t
    return $ directIndexFunction m t
summaryForBindage _ (BindInPlace _ src _) =
  lookupSummary' src

memForBindee :: (MonadFreshNames m) =>
                Ident
             -> m (Ident,
                   Ident,
                   (Ident, MemSummary))
memForBindee ident = do
  size <- newIdent (memname <> "_size") (Basic Int)
  mem <- newIdent memname $ Mem $ Var $ identName size
  return (size,
          mem,
          (ident, directIndexFunction (identName mem) t))
  where  memname = baseString (identName ident) <> "_mem"
         t       = identType ident

directIndexFunction :: VName -> Type -> MemSummary
directIndexFunction mem t =
  MemSummary mem $ IxFun.iota $ arrayDims t

computeSize :: MonadBinder m =>
               SubExp -> [SubExp] -> m SubExp
computeSize current [] = return current
computeSize current (x:xs) = do
  let pexp = pure . PrimOp . SubExp
  e <- eBinOp Times (pexp current) (pexp x) Int
  v <- letSubExp "x" e
  computeSize v xs

lookupSummary :: VName -> AllocM (Maybe MemSummary)
lookupSummary name = asks $ fmap entryMemSummary . HM.lookup name

lookupSummary' :: VName -> AllocM MemSummary
lookupSummary' name = do
  res <- asks $ fmap entryMemSummary . HM.lookup name
  case res of
    Just summary -> return summary
    Nothing ->
      fail $ "No memory summary for variable " ++ pretty name

lookupArraySummary' :: VName -> AllocM (VName, IxFun.IxFun)
lookupArraySummary' name = do
  summary <- lookupSummary' name
  case summary of MemSummary mem ixfun ->
                    return (mem, ixfun)
                  Scalar ->
                    fail $ "Variable " ++ pretty name ++ " does not look like an array."

bindeeSummary :: PatElem -> (VName, Entry)
bindeeSummary bindee = (patElemName bindee,
                        Entry (patElemLore bindee) (patElemType bindee))

bindeesSummary :: [PatElem] -> HM.HashMap VName Entry
bindeesSummary = HM.fromList . map bindeeSummary

fparamsSummary :: [FParam] -> HM.HashMap VName Entry
fparamsSummary = HM.fromList . map fparamSummary
  where fparamSummary fparam = (fparamName fparam,
                                Entry (fparamLore fparam) (fparamType fparam))

runAllocM :: MonadFreshNames m => AllocM a -> m a
runAllocM = runAllocMWithEnv HM.empty

runAllocMWithEnv :: MonadFreshNames m =>
                    HM.HashMap VName Entry
                 -> AllocM a
                 -> m a
runAllocMWithEnv env (AllocM m) =
  fst <$> modifyNameSource (runState (runReaderT (runBinderT m mempty) env))

allocInFParams :: [In.FParam] -> ([FParam] -> AllocM a)
               -> AllocM a
allocInFParams params m = do
  (valparams, (memsizeparams, memparams)) <- runWriterT $ forM params $ \param ->
    case fparamType param of
      Array {} -> do
        (memsize,mem,(param',paramlore)) <- lift $ memForBindee $ fparamIdent param
        tell ([FParam memsize Scalar], [FParam mem Scalar])
        return $ FParam param' paramlore
      _ -> return param { fparamLore = Scalar }
  let summary = fparamsSummary valparams
      params' = memsizeparams <> memparams <> valparams
  local (summary `HM.union`) $ m params'

isArray :: SubExp -> AllocM Bool
isArray (Var v) = not <$> (==Scalar) <$> lookupSummary' v
isArray (Constant _) = return False

ensureDirectArray :: VName -> AllocM (SubExp, VName, SubExp)
ensureDirectArray v = do
  res <- lookupSummary v
  t <- lookupType v
  case (res, t) of
    (Just (MemSummary mem ixfun), Mem size)
      | IxFun.isDirect ixfun ->
        return (size, mem, Var v)
    _ ->
      -- We need to do a new allocation, copy 'v', and make a new
      -- binding for the size of the memory block.
      allocLinearArray (baseString v) $ Var v

allocLinearArray :: String
                 -> SubExp -> AllocM (SubExp, VName, SubExp)
allocLinearArray s se = do
  t <- subExpType se
  (size, mem) <- allocForArray t
  v' <- newIdent s t
  let pat = Pattern [PatElem v' BindVar $ directIndexFunction mem t]
  addBinding $ Let pat () $ PrimOp $ Copy se
  return (size, mem, Var $ identName v')

funcallArgs :: [(SubExp,Diet)] -> AllocM [(SubExp,Diet)]
funcallArgs args = do
  (valargs, (memsizeargs, memargs)) <- runWriterT $ forM args $ \(arg,d) -> do
    array <- lift $ isArray arg
    case (arg, array) of
      (Var v, True) -> do
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
allocInFun (In.FunDec fname rettype params body) =
  runAllocM $ allocInFParams params $ \params' -> do
    body' <- insertBindingsM $ allocInBody body
    return $ FunDec fname (memoryInRetType rettype) params' body'

allocInBody :: In.Body -> AllocM Body
allocInBody (Body _ bnds res) =
  allocInBindings bnds $ \bnds' -> do
    (ses, allocs) <- collectBindings $ mapM ensureDirect $ resultSubExps res
    return $ Body () (bnds'<>allocs) res { resultSubExps = ses }
  where ensureDirect se@(Constant {}) = return se
        ensureDirect (Var v) = do
          bt <- basicType <$> lookupType v
          if bt
            then return $ Var v
            else do (_, _, v') <- ensureDirectArray v
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
                concatMap (patternElements . bindingPattern) allocbnds
          local (`HM.union` summaries) $
            allocInBindings' xs (bnds'++allocbnds)
        allocInBinding' bnd = do
          ((),bnds') <- collectBindings $ allocInBinding bnd
          return bnds'

allocInBinding :: In.Binding -> AllocM ()
allocInBinding (Let pat _ e) = do
  e' <- allocInExp e
  let (sizeidents, validents) =
        splitAt (patternSize pat - expExtTypeSize e') $
        patternElements pat
      sizeidents' = map patElemIdent sizeidents
      validents' = [ (ident, bindage) | PatElem ident bindage () <- validents ]
  (bnd, bnds) <- allocsForBinding sizeidents' validents' e'
  mapM_ addBinding $ bnd:bnds

funcallSubExps :: [SubExp] -> AllocM [SubExp]
funcallSubExps ses = map fst <$>
                     funcallArgs [ (se, Observe) | se <- ses ]

allocInExp :: In.Exp -> AllocM Exp
allocInExp (LoopOp (DoLoop res merge form
                    (Body () bodybnds bodyres))) =
  allocInFParams mergeparams $ \mergeparams' ->
  formBinds form $ do
    mergeinit' <- funcallSubExps mergeinit
    body' <- insertBindingsM $ allocInBindings bodybnds $ \bodybnds' -> do
      (ses,retbnds) <- collectBindings $
                       funcallSubExps $ resultSubExps bodyres
      let res' = bodyres { resultSubExps = ses }
      return $ Body () (bodybnds'<>retbnds) res'
    return $ LoopOp $
      DoLoop res (zip mergeparams' mergeinit') form body'
  where (mergeparams, mergeinit) = unzip merge
        formBinds (ForLoop i _) =
          local (HM.singleton i (Entry Scalar $ Basic Int)<>)
        formBinds (WhileLoop _) =
          id
allocInExp (LoopOp (Map {})) =
  fail "Cannot put explicit allocations in map yet."
allocInExp (LoopOp (Reduce {})) =
  fail "Cannot put explicit allocations in reduce yet."
allocInExp (LoopOp (Scan {})) =
  fail "Cannot put explicit allocations in scan yet."
allocInExp (LoopOp (Redomap {})) =
  fail "Cannot put explicit allocations in redomap yet."
allocInExp (Apply fname args rettype) = do
  args' <- funcallArgs args
  return $ Apply fname args' (memoryInRetType rettype)
allocInExp e = mapExpM alloc e
  where alloc =
          identityMapper { mapOnBinding = fail "Unhandled binding in ExplicitAllocations"
                         , mapOnBody = allocInBody
                         , mapOnLambda = allocInLambda
                         , mapOnExtLambda = allocInExtLambda
                         , mapOnRetType = return . memoryInRetType
                         , mapOnFParam = fail "Unhandled fparam in ExplicitAllocations"
                         }

allocInLambda :: In.Lambda -> AllocM Lambda
allocInLambda lam = do
  body <- allocInBody $ lambdaBody lam
  return $ lam { lambdaBody = body }

allocInExtLambda :: In.ExtLambda -> AllocM ExtLambda
allocInExtLambda lam = do
  body <- allocInBody $ extLambdaBody lam
  return $ lam { extLambdaBody = body }

vtableToAllocEnv :: ST.SymbolTable (Wise ExplicitMemory)
                 -> HM.HashMap VName Entry
vtableToAllocEnv = HM.fromList . mapMaybe entryToMemSummary .
                   HM.toList . ST.bindings
  where entryToMemSummary (k,entry) = do
          summary <- (snd <$> ST.entryLetBoundLore entry) <|>
                     ST.entryFParamLore entry
          return (k, Entry summary $ ST.entryType entry)

simplifiable :: (Engine.MonadEngine m,
                 Engine.InnerLore m ~ ExplicitMemory) =>
                SimpleOps m
simplifiable =
  SimpleOps mkLetS' mkBodyS' mkLetNamesS'
  simplifyMemSummary simplifyMemSummary
  simplifyRetType'
  where mkLetS' vtable pat e = do
          Let pat' lore _ <- runAllocMWithEnv env $
                             mkLetM (removePatternWisdom pat) $
                             removeExpWisdom e
          return $ mkWiseLetBinding pat' lore e
          where env = vtableToAllocEnv vtable

        mkBodyS' _ bnds res = return $ mkWiseBody () bnds res

        mkLetNamesS' vtable names e = do
          Let pat' lore _ <-
            runAllocMWithEnv env $ mkLetNamesM names $ removeExpWisdom e
          return $ mkWiseLetBinding pat' lore e
          where env = vtableToAllocEnv vtable

        simplifyMemSummary Scalar =
          return Scalar
        simplifyMemSummary (MemSummary ident ixfun) =
          MemSummary <$> Engine.simplifyVName ident <*> pure ixfun

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
                  ReturnsInBlock <$> Engine.simplifyVName v <*>
                  pure ixfun
