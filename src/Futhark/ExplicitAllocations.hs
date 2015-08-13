{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, FlexibleContexts #-}
module Futhark.ExplicitAllocations
       ( explicitAllocations
       , simplifiable
       )
where

import Control.Arrow (first, second)
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
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
import qualified Futhark.Analysis.ScalExp as SE
import Futhark.Optimise.Simplifier.Simplify (SimpleOps (..))
import qualified Futhark.Optimise.Simplifier.Engine as Engine

data Entry = Entry { entryMemSummary :: MemSummary
                   , entryType :: Type
                   }

type MemoryMap = HM.HashMap VName Entry

data AllocBinding = SizeComputation VName SE.ScalExp
                  | Allocation VName SubExp
                  | ArrayCopy VName Bindage VName
                    deriving (Eq, Ord, Show)

bindAllocBinding :: MonadBinder m => AllocBinding -> m ()
bindAllocBinding (SizeComputation name se) = do
  e <- SE.fromScalExp' se
  letBindNames'_ [name] e
bindAllocBinding (Allocation name size) =
  letBindNames'_ [name] $ PrimOp $ Alloc size
bindAllocBinding (ArrayCopy name bindage src) =
  letBindNames_ [(name,bindage)] $ PrimOp $ Copy src

class (MonadFreshNames m, HasTypeEnv m) => Allocator m where
  addAllocBinding :: AllocBinding -> m ()

  askMemoryMap :: m MemoryMap

  asksMemoryMap :: (MemoryMap -> a) -> m a
  asksMemoryMap f = f <$> askMemoryMap

allocateMemory :: Allocator m =>
                  String -> SubExp -> m VName
allocateMemory desc size = do
  v <- newVName desc
  addAllocBinding $ Allocation v size
  return v

computeSize :: Allocator m =>
               String -> SE.ScalExp -> m SubExp
computeSize desc se = do
  v <- newVName desc
  addAllocBinding $ SizeComputation v se
  return $ Var v

-- | Monad for adding allocations to an entire program.
newtype AllocM a = AllocM (BinderT ExplicitMemory
                            (State (MemoryMap, VNameSource))
                           a)
                 deriving (Applicative, Functor, Monad,
                           MonadState (MemoryMap, VNameSource))

instance MonadFreshNames AllocM where
  getNameSource = snd <$> get
  putNameSource src = modify $ \(memmap, _) -> (memmap, src)

instance MonadBinder AllocM where
  type Lore AllocM = ExplicitMemory

  mkLetM pat e = return $ Let pat () e

  mkLetNamesM names e = do
    pat <- patternWithAllocations names e
    return $ Let pat () e

  mkBodyM bnds res = return $ Body () bnds res

  addBinding binding = do
    AllocM $ addBinderBinding binding
    let summaries = HM.fromList $
                    map patElemSummary $ patternElements $ bindingPattern binding
    modify $ first (`HM.union` summaries)
  collectBindings (AllocM m) =
    AllocM $ collectBinderBindings m

instance HasTypeEnv AllocM where
  askTypeEnv = liftM2 HM.union (AllocM askTypeEnv) (HM.map entryType <$> askMemoryMap)

instance Allocator AllocM where
  askMemoryMap = gets fst

  addAllocBinding (SizeComputation name se) =
    letBindNames'_ [name] =<< SE.fromScalExp' se
  addAllocBinding (Allocation name size) =
    letBindNames'_ [name] $ PrimOp $ Alloc size
  addAllocBinding (ArrayCopy name bindage src) =
    letBindNames_ [(name, bindage)] $ PrimOp $ SubExp $ Var src

runAllocM :: MonadFreshNames m => AllocM a -> m a
runAllocM = runAllocMWithEnv HM.empty

runAllocMWithEnv :: MonadFreshNames m =>
                    MemoryMap
                 -> AllocM a
                 -> m a
runAllocMWithEnv env (AllocM m) = modifyNameSource $ \src ->
  first fst $ second snd $ runState (runBinderT m mempty) (env, src)

localMemoryMap :: (MemoryMap -> MemoryMap) -> AllocM a -> AllocM a
localMemoryMap f m = do old <- gets fst
                        modify $ first f
                        m <* modify (first (const old))

-- | Monad for adding allocations to a single pattern.
newtype PatAllocM a = PatAllocM (WriterT [AllocBinding]
                                 (ReaderT MemoryMap
                                  (State VNameSource))
                                 a)
                    deriving (Applicative, Functor, Monad,
                              MonadReader MemoryMap,
                              MonadWriter [AllocBinding],
                              MonadFreshNames)

instance Allocator PatAllocM where
  askMemoryMap = ask

  addAllocBinding = tell . pure

instance HasTypeEnv PatAllocM where
  askTypeEnv =
    HM.map entryType <$> ask

runPatAllocM :: MonadFreshNames m =>
                PatAllocM a -> MemoryMap -> m (a, [AllocBinding])
runPatAllocM (PatAllocM m) memoryMap =
  modifyNameSource $ runState $ runReaderT (runWriterT m) memoryMap

allocForArray :: Allocator m =>
                 Type -> m (SubExp, VName)
allocForArray t = do
  size <-
    computeSize "bytes" $
    SE.sproduct $
    (SE.Val $ IntVal $ fromIntegral $ basicSize $ elemType t) :
    map (`SE.subExpToScalExp` Int) (arrayDims t)
  m <- allocateMemory "mem" size
  return (size, m)

allocsForBinding :: Allocator m =>
                    [Ident] -> [(Ident,Bindage)] -> Exp
                 -> m (Binding, [AllocBinding])
allocsForBinding sizeidents validents e = do
  rts <- expReturns lookupSummary' e
  (ctxElems, valElems, postbnds) <- allocsForPattern sizeidents validents rts
  return (Let (Pattern ctxElems valElems) () e,
          postbnds)

patternWithAllocations :: Allocator m =>
                           [(VName, Bindage)]
                        -> Exp
                        -> m Pattern
patternWithAllocations names e = do
  (ts',sizes) <- instantiateShapes' =<< expExtType e
  let identForBindage name t BindVar =
        pure (Ident name t, BindVar)
      identForBindage name _ bindage@(BindInPlace _ src _) = do
        t <- lookupType src
        pure (Ident name t, bindage)
  vals <- sequence [ identForBindage name t bindage  |
                     ((name,bindage), t) <- zip names ts' ]
  (Let pat _ _, extrabnds) <- allocsForBinding sizes vals e
  case extrabnds of
    [] -> return pat
    _  -> fail $ "Cannot make allocations for pattern of " ++ pretty e

allocsForPattern :: Allocator m =>
                    [Ident] -> [(Ident,Bindage)] -> [ExpReturns]
                 -> m ([PatElem], [PatElem], [AllocBinding])
allocsForPattern sizeidents validents rts = do
  let sizes' = [ PatElem size BindVar Scalar | size <- sizeidents ]
  (vals,(memsizes, mems, postbnds)) <-
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
            if destmem == mem && destixfun == ixfun
              then return $ PatElem ident bindage $ MemSummary mem ixfun
              else do
              -- The expression returns at some specific memory
              -- location, but we want to put the result somewhere
              -- else.  This means we need to store it in the memory
              -- it wants to first, then copy it to our intended
              -- destination in an extra binding.
              tmp_buffer <- lift $
                            newIdent (baseString (identName ident)<>"_buffer")
                            (stripArray (length is) $ identType ident
                             `setUniqueness` u)
              tell ([], [],
                    [ArrayCopy (identName ident) bindage $
                     identName tmp_buffer])
              return $ PatElem tmp_buffer BindVar $
                MemSummary mem ixfun

      ReturnsArray _ extshape _ Nothing
        | Just _ <- knownShape extshape -> do
          summary <- lift $ summaryForBindage (identType ident) bindage
          return $ PatElem ident bindage summary

      ReturnsArray _ _ u (Just (ReturnsNewBlock {}))
        | BindInPlace _ _ is <- bindage -> do
            -- The expression returns its own memory, but the pattern
            -- wants to store it somewhere else.  We first let it
            -- store the value where it wants, then we copy it to the
            -- intended destination.  In some cases, the copy may be
            -- optimised away later, but in some cases it may not be
            -- possible (e.g. function calls).
            tmp_buffer <- lift $
                          newIdent (baseString (identName ident)<>"_ext_buffer")
                          (stripArray (length is) $ identType ident
                           `setUniqueness` u)
            (memsize,mem,(_,lore)) <- lift $ memForBindee tmp_buffer
            tell ([PatElem memsize BindVar Scalar],
                  [PatElem mem     BindVar Scalar],
                  [ArrayCopy (identName ident) bindage $
                   identName tmp_buffer])
            return $ PatElem tmp_buffer BindVar lore

      ReturnsArray {} -> do
        (memsize,mem,(ident',lore)) <- lift $ memForBindee ident
        tell ([PatElem memsize BindVar Scalar],
              [PatElem mem     BindVar Scalar],
              [])
        return $ PatElem ident' bindage lore

  return (memsizes <> mems <> sizes',
          vals,
          postbnds)
  where knownShape = mapM known . extShapeDims
        known (Free v) = Just v
        known (Ext {}) = Nothing

summaryForBindage :: Allocator m =>
                     Type -> Bindage
                  -> m MemSummary
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

sizeOfMemoryBlock :: (Monad m, HasTypeEnv m) => VName -> m SubExp
sizeOfMemoryBlock mem = do
  t <- lookupType mem
  case t of
    Mem size -> return size
    _        -> fail $ "sizeOfMemoryBlock: " <> pretty mem <> " not a memory block."

lookupSummary :: VName -> AllocM (Maybe MemSummary)
lookupSummary name = asksMemoryMap $ fmap entryMemSummary . HM.lookup name

lookupSummary' :: Allocator m =>
                  VName -> m MemSummary
lookupSummary' name = do
  res <- asksMemoryMap $ fmap entryMemSummary . HM.lookup name
  case res of
    Just summary -> return summary
    Nothing ->
      fail $ "No memory summary for variable " ++ pretty name

lookupArraySummary' :: Allocator m => VName -> m (VName, IxFun.IxFun)
lookupArraySummary' name = do
  summary <- lookupSummary' name
  case summary of MemSummary mem ixfun ->
                    return (mem, ixfun)
                  Scalar ->
                    fail $ "Variable " ++ pretty name ++ " does not look like an array."

patElemSummary :: PatElem -> (VName, Entry)
patElemSummary bindee = (patElemName bindee,
                        Entry (patElemLore bindee) (patElemType bindee))

bindeesSummary :: [PatElem] -> MemoryMap
bindeesSummary = HM.fromList . map patElemSummary

paramsSummary :: [ParamT MemSummary] -> MemoryMap
paramsSummary = HM.fromList . map paramSummary
  where paramSummary fparam = (paramName fparam,
                               Entry (paramLore fparam) (paramType fparam))

allocInFParams :: [In.FParam] -> ([FParam] -> AllocM a)
               -> AllocM a
allocInFParams params m = do
  (valparams, (memsizeparams, memparams)) <- runWriterT $ forM params $ \param ->
    case paramType param of
      Array {} -> do
        (memsize,mem,(param',paramlore)) <- lift $ memForBindee $ paramIdent param
        tell ([Param memsize Scalar], [Param mem Scalar])
        return $ Param param' paramlore
      _ -> return param { paramLore = Scalar }
  let params' = memsizeparams <> memparams <> valparams
      summary = paramsSummary params'
  localMemoryMap (summary `HM.union`) $ m params'

isArray :: SubExp -> AllocM Bool
isArray (Var v) = not <$> (==Scalar) <$> lookupSummary' v
isArray (Constant _) = return False

ensureDirectArray :: VName -> AllocM (SubExp, VName, SubExp)
ensureDirectArray v = do
  res <- lookupSummary v
  case res of
    Just (MemSummary mem ixfun)
      | IxFun.isDirect ixfun -> do
        memt <- lookupType mem
        case memt of
          Mem size -> return (size, mem, Var v)
          _        -> fail $
                      pretty mem ++
                      " should be a memory block but has type " ++
                      pretty memt
    _ ->
      -- We need to do a new allocation, copy 'v', and make a new
      -- binding for the size of the memory block.
      allocLinearArray (baseString v) v

allocLinearArray :: String
                 -> VName -> AllocM (SubExp, VName, SubExp)
allocLinearArray s v = do
  t <- lookupType v
  (size, mem) <- allocForArray t
  v' <- newIdent s t
  let pat = Pattern [] [PatElem v' BindVar $ directIndexFunction mem t]
  addBinding $ Let pat () $ PrimOp $ Copy v
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
explicitAllocations = intraproceduralTransformation allocInFun

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
    (ses, allocs) <- collectBindings $ mapM ensureDirect res
    return $ Body () (bnds'<>allocs) ses
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
          localMemoryMap (`HM.union` summaries) $
            allocInBindings' xs (bnds'++allocbnds)
        allocInBinding' bnd = do
          ((),bnds') <- collectBindings $ allocInBinding bnd
          return bnds'

allocInBinding :: In.Binding -> AllocM ()
allocInBinding (Let (Pattern sizeElems valElems) _ e) = do
  e' <- allocInExp e
  let sizeidents = map patElemIdent sizeElems
      validents = [ (ident, bindage) | PatElem ident bindage () <- valElems ]
  (bnd, bnds) <- allocsForBinding sizeidents validents e'
  addBinding bnd
  mapM_ bindAllocBinding bnds

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
      (ses,retbnds) <- collectBindings $ funcallSubExps bodyres
      return $ Body () (bodybnds'<>retbnds) ses
    return $ LoopOp $
      DoLoop res (zip mergeparams' mergeinit') form body'
  where (mergeparams, mergeinit) = unzip merge
        formBinds (ForLoop i _) =
          localMemoryMap (HM.insert i $ Entry Scalar $ Basic Int)
        formBinds (WhileLoop _) =
          id

allocInExp (LoopOp (Kernel cs w index ispace inps returns body)) = do
  inps' <- mapM allocInKernelInput inps
  let mem_map = paramsSummary (map kernelInputParam inps') <> ispace_map
  localMemoryMap (mem_map <>) $ do
    body' <- allocInBody body
    return $ LoopOp $ Kernel cs w index ispace inps' returns body'
  where ispace_map = HM.fromList [ (i, Entry Scalar $ Basic Int)
                                 | i <- index : map fst ispace ]
        allocInKernelInput inp
          | Basic _ <- kernelInputType inp =
              return inp { kernelInputParam = Param (kernelInputIdent inp) Scalar }
          | otherwise = do
              (mem, ixfun) <- lookupArraySummary' $ kernelInputArray inp
              let ixfun' = IxFun.applyInd ixfun $ map SE.intSubExpToScalExp $
                           kernelInputIndices inp
                  summary = MemSummary mem ixfun'
              return inp { kernelInputParam = Param (kernelInputIdent inp) summary }

allocInExp (LoopOp (Map cs w f arrs)) = do
  f' <- allocInMapLambda f =<< mapM lookupSummary' arrs
  return $ LoopOp $ Map cs w f' arrs

allocInExp (LoopOp (Reduce cs w f input)) = do
  -- Create new memory blocks but use same index functions.
  (new_summaries, new_arrs) <- unzip <$> mapM (duplicateArray "reduce") arrs
  f' <- allocInReduceLambda f new_summaries
  return $ LoopOp $ Reduce cs w f' $ zip accs new_arrs
  where (accs,arrs) = unzip input

allocInExp (LoopOp (Scan {})) =
  fail "Cannot put explicit allocations in scan yet."
allocInExp (LoopOp (Redomap {})) =
  fail "Cannot put explicit allocations in redomap yet."
allocInExp (Apply fname args rettype) = do
  args' <- funcallArgs args
  return $ Apply fname args' (memoryInRetType rettype)
allocInExp e = mapExpM alloc e
  where alloc =
          identityMapper { mapOnBody = allocInBody
                         , mapOnLambda = fail "Unhandled lambda in ExplicitAllocations"
                         , mapOnExtLambda = fail "Unhandled ext lambda in ExplicitAllocations"
                         , mapOnRetType = return . memoryInRetType
                         , mapOnFParam = fail "Unhandled fparam in ExplicitAllocations"
                         }

-- | Duplicate an array into a new memory block, but with the same
-- index function.
duplicateArray :: String -> VName -> AllocM ((VName, IxFun.IxFun), VName)
duplicateArray desc arr = do
  (mem, ixfun) <- lookupArraySummary' arr
  mem_size <- sizeOfMemoryBlock mem
  new_mem <- letExp (desc <> "_" <> baseString arr <> "_mem") $
             PrimOp $ Alloc mem_size
  new_arr <- newVName $ desc <> "_" <> baseString arr
  new_arr_ident <- Ident new_arr <$> lookupType arr
  let pat = Pattern [] [PatElem new_arr_ident BindVar $
                        MemSummary new_mem ixfun]
  addBinding =<< mkLetM pat (PrimOp $ Copy arr)
  return ((new_mem, ixfun), new_arr)

allocInMapLambda :: In.Lambda -> [MemSummary] -> AllocM Lambda
allocInMapLambda lam input_summaries = do
  let i = lambdaIndex lam
  params' <-
    forM (zip (lambdaParams lam) input_summaries) $ \(p,summary) ->
    case (paramType p, summary) of
     (_, Scalar) ->
       fail $ "Passed a scalar for lambda parameter " ++ pretty p
     (Array {}, MemSummary mem ixfun) ->
       return p { paramLore =
                     MemSummary mem $ IxFun.applyInd ixfun [SE.Id i Int]
                }
     _ ->
       return p { paramLore = Scalar }
  allocInLambda i params' (lambdaBody lam) (lambdaReturnType lam)

allocInReduceLambda :: In.Lambda
                    -> [(VName, IxFun.IxFun)]
                    -> AllocM Lambda
allocInReduceLambda lam input_summaries = do
  -- Idea: at index point 'i', the accumulator is at offset 'i' and
  -- the element is at offset 'i+1'.
  acc_params' <-
    forM (zip acc_params input_summaries) $ \(acc_param, (mem, ixfun)) ->
    return acc_param { paramLore =
                          MemSummary mem $ IxFun.applyInd ixfun [SE.Id i Int]
                     }
  arr_params' <-
    forM (zip arr_params input_summaries) $ \(arr_param, (mem, ixfun)) ->
    return arr_param { paramLore =
                          MemSummary mem $ IxFun.applyInd ixfun [SE.Id i Int + 1]
                     }
  allocInLambda i (acc_params' ++ arr_params') (lambdaBody lam) (lambdaReturnType lam)
  where n = length input_summaries
        i = lambdaIndex lam
        (acc_params, arr_params) = splitAt n $ lambdaParams lam

allocInLambda :: VName -> [LParam] -> In.Body -> [Type]
              -> AllocM Lambda
allocInLambda i params body rettype = do
  let param_summaries = paramsSummary params
      all_summaries = HM.insert i (Entry Scalar $ Basic Int) param_summaries
  body' <- localMemoryMap (HM.union all_summaries) $
           allocInBody body
  return $ Lambda i params body' rettype

vtableToAllocEnv :: ST.SymbolTable (Wise ExplicitMemory)
                 -> MemoryMap
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
  simplifyMemSummary simplifyMemSummary simplifyMemSummary
  simplifyRetType'
  where mkLetS' _ pat e =
          return $ mkWiseLetBinding (removePatternWisdom pat) () e

        mkBodyS' _ bnds res = return $ mkWiseBody () bnds res

        mkLetNamesS' vtable names e = do
          pat' <- bindPatternWithAllocations env names $
                  removeExpWisdom e
          return $ mkWiseLetBinding pat' () e
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

bindPatternWithAllocations :: MonadBinder m =>
                              MemoryMap -> [(VName, Bindage)] -> Exp
                           -> m Pattern
bindPatternWithAllocations memoryMap names e = do
  (pat,prebnds) <- runPatAllocM (patternWithAllocations names e) memoryMap
  mapM_ bindAllocBinding prebnds
  return pat
