{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, FlexibleContexts, TupleSections, LambdaCase #-}
module Futhark.Pass.ExplicitAllocations
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
import Futhark.Pass

data Entry = Entry { entryMemBound :: MemBound NoUniqueness
                   }

entryType :: Entry -> Type
entryType = typeOf . entryMemBound

type MemoryMap = HM.HashMap VName Entry

data AllocBinding = SizeComputation VName SE.ScalExp
                  | Allocation VName SubExp Space
                  | ArrayCopy VName Bindage VName
                    deriving (Eq, Ord, Show)

bindAllocBinding :: MonadBinder m => AllocBinding -> m ()
bindAllocBinding (SizeComputation name se) = do
  e <- SE.fromScalExp' se
  letBindNames'_ [name] e
bindAllocBinding (Allocation name size space) =
  letBindNames'_ [name] $ PrimOp $ Alloc size space
bindAllocBinding (ArrayCopy name bindage src) =
  letBindNames_ [(name,bindage)] $ PrimOp $ Copy src

class (MonadFreshNames m, HasTypeEnv m) => Allocator m where
  addAllocBinding :: AllocBinding -> m ()

  askMemoryMap :: m MemoryMap

  asksMemoryMap :: (MemoryMap -> a) -> m a
  asksMemoryMap f = f <$> askMemoryMap

allocateMemory :: Allocator m =>
                  String -> SubExp -> Space -> m VName
allocateMemory desc size space = do
  v <- newVName desc
  addAllocBinding $ Allocation v size space
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
  addAllocBinding (Allocation name size space) =
    letBindNames'_ [name] $ PrimOp $ Alloc size space
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

arraySizeInBytesExp :: Type -> SE.ScalExp
arraySizeInBytesExp t =
  SE.sproduct $
  (SE.Val $ IntVal $ basicSize $ elemType t) :
  map (`SE.subExpToScalExp` Int) (arrayDims t)

arraySizeInBytes :: Allocator m => Type -> m SubExp
arraySizeInBytes = computeSize "bytes" . arraySizeInBytesExp

allocForArray :: Allocator m =>
                 Type -> Space -> m (SubExp, VName)
allocForArray t space = do
  size <- arraySizeInBytes t
  m <- allocateMemory "mem" size space
  return (size, m)

-- | Allocate local-memory array.
allocForLocalArray :: Allocator m =>
                      SubExp -> Type -> m (SubExp, VName)
allocForLocalArray workgroup_size t = do
  size <- computeSize "local_bytes" $
          arraySizeInBytesExp t *
          SE.intSubExpToScalExp workgroup_size
  m <- allocateMemory "local_mem" size $ Space "local"
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
  let sizes' = [ PatElem size BindVar $ Scalar Int | size <- map identName sizeidents ]
  (vals,(memsizes, mems, postbnds)) <-
    runWriterT $ forM (zip validents rts) $ \((ident,bindage), rt) -> do
      let shape = arrayShape $ identType ident
      case rt of
        ReturnsScalar _ -> do
          summary <- lift $ summaryForBindage (identType ident) bindage
          return $ PatElem (identName ident) bindage summary

        ReturnsMemory size space ->
          return $ PatElem (identName ident) bindage $ MemMem size space

        ReturnsArray bt _ u (Just (ReturnsInBlock mem ixfun)) ->
          case bindage of
            BindVar ->
              return $ PatElem (identName ident) bindage $
              ArrayMem bt shape u mem ixfun
            BindInPlace _ src is -> do
              (destmem,destixfun) <- lift $ lookupArraySummary' src
              if destmem == mem && destixfun == ixfun
                then return $ PatElem (identName ident) bindage $
                     ArrayMem bt shape u mem ixfun
                else do
                -- The expression returns at some specific memory
                -- location, but we want to put the result somewhere
                -- else.  This means we need to store it in the memory
                -- it wants to first, then copy it to our intended
                -- destination in an extra binding.
                tmp_buffer <- lift $
                              newIdent (baseString (identName ident)<>"_buffer")
                              (stripArray (length is) $ identType ident)
                tell ([], [],
                      [ArrayCopy (identName ident) bindage $
                       identName tmp_buffer])
                return $ PatElem (identName tmp_buffer) BindVar $
                  ArrayMem bt (stripDims (length is) shape) u mem ixfun

        ReturnsArray _ extshape _ Nothing
          | Just _ <- knownShape extshape -> do
            summary <- lift $ summaryForBindage (identType ident) bindage
            return $ PatElem (identName ident) bindage summary

        ReturnsArray bt _ u (Just ReturnsNewBlock{})
          | BindInPlace _ _ is <- bindage -> do
              -- The expression returns its own memory, but the pattern
              -- wants to store it somewhere else.  We first let it
              -- store the value where it wants, then we copy it to the
              -- intended destination.  In some cases, the copy may be
              -- optimised away later, but in some cases it may not be
              -- possible (e.g. function calls).
              tmp_buffer <- lift $
                            newIdent (baseString (identName ident)<>"_ext_buffer")
                            (stripArray (length is) $ identType ident)
              (memsize,mem,(_,ixfun)) <- lift $ memForBindee tmp_buffer
              tell ([PatElem (identName memsize) BindVar $ Scalar Int],
                    [PatElem (identName mem)     BindVar $ MemMem (Var $ identName memsize) DefaultSpace],
                    [ArrayCopy (identName ident) bindage $
                     identName tmp_buffer])
              return $ PatElem (identName tmp_buffer) BindVar $
                ArrayMem bt (stripDims (length is) shape) u (identName mem) ixfun

        ReturnsArray bt _ u _ -> do
          (memsize,mem,(ident',ixfun)) <- lift $ memForBindee ident
          tell ([PatElem (identName memsize) BindVar $ Scalar Int],
                [PatElem (identName mem)     BindVar $ MemMem (Var $ identName memsize) DefaultSpace],
                [])
          return $ PatElem (identName ident') bindage $ ArrayMem bt shape u (identName mem) ixfun

  return (memsizes <> mems <> sizes',
          vals,
          postbnds)
  where knownShape = mapM known . extShapeDims
        known (Free v) = Just v
        known Ext{} = Nothing

summaryForBindage :: Allocator m =>
                     Type -> Bindage
                  -> m (MemBound NoUniqueness)
summaryForBindage (Basic bt) BindVar =
  return $ Scalar bt
summaryForBindage (Mem size space) BindVar =
  return $ MemMem size space
summaryForBindage t@(Array bt shape u) BindVar = do
  (_, m) <- allocForArray t DefaultSpace
  return $ directIndexFunction bt shape u m t
summaryForBindage _ (BindInPlace _ src _) =
  lookupSummary' src

memForBindee :: (MonadFreshNames m) =>
                Ident
             -> m (Ident,
                   Ident,
                   (Ident, IxFun.IxFun))
memForBindee ident = do
  size <- newIdent (memname <> "_size") (Basic Int)
  mem <- newIdent memname $ Mem (Var $ identName size) DefaultSpace
  return (size,
          mem,
          (ident, IxFun.iota $ IxFun.shapeFromSubExps $ arrayDims t))
  where  memname = baseString (identName ident) <> "_mem"
         t       = identType ident

directIndexFunction :: BasicType -> Shape -> u -> VName -> Type -> MemBound u
directIndexFunction bt shape u mem t =
  ArrayMem bt shape u mem $ IxFun.iota $ IxFun.shapeFromSubExps $ arrayDims t

lookupSummary :: VName -> AllocM (Maybe (MemBound NoUniqueness))
lookupSummary name = asksMemoryMap $ fmap entryMemBound . HM.lookup name

lookupSummary' :: Allocator m =>
                  VName -> m (MemBound NoUniqueness)
lookupSummary' name = do
  res <- asksMemoryMap $ fmap entryMemBound . HM.lookup name
  case res of
    Just summary -> return summary
    Nothing ->
      fail $ "No memory summary for variable " ++ pretty name

lookupArraySummary' :: Allocator m => VName -> m (VName, IxFun.IxFun)
lookupArraySummary' name = do
  summary <- lookupSummary' name
  case summary of
    ArrayMem _ _ _ mem ixfun ->
      return (mem, ixfun)
    _ ->
      fail $ "Variable " ++ pretty name ++ " does not look like an array."

patElemSummary :: PatElem -> (VName, Entry)
patElemSummary bindee = (patElemName bindee,
                         Entry (patElemAttr bindee))

bindeesSummary :: [PatElem] -> MemoryMap
bindeesSummary = HM.fromList . map patElemSummary

paramsSummary :: Typed (MemBound u) =>
                 [ParamT (MemBound u)] -> MemoryMap
paramsSummary = HM.fromList . map paramSummary
  where paramSummary fparam = (paramName fparam,
                               Entry
                               (const NoUniqueness <$> paramAttr fparam))

allocInFParams :: [In.FParam] -> ([FParam] -> AllocM a)
               -> AllocM a
allocInFParams params m = do
  (valparams, (memsizeparams, memparams)) <-
    runWriterT $ mapM allocInFParam params
  let params' = memsizeparams <> memparams <> valparams
      summary = paramsSummary params'
  localMemoryMap (summary `HM.union`) $ m params'

allocInFParam :: MonadFreshNames m =>
                 In.FParam -> WriterT ([FParam], [FParam]) m FParam
allocInFParam param =
  case paramDeclType param of
    Array bt shape u -> do
      let memname = baseString (paramName param) <> "_mem"
          ixfun = IxFun.iota $ IxFun.shapeFromSubExps $ shapeDims shape
      memsize <- lift $ newVName (memname <> "_size")
      mem <- lift $ newVName memname
      tell ([Param memsize $ Scalar Int],
            [Param mem $ MemMem (Var memsize) DefaultSpace])
      return param { paramAttr =  ArrayMem bt shape u mem ixfun }
    Basic bt ->
      return param { paramAttr = Scalar bt }
    Mem size space ->
      return param { paramAttr = MemMem size space }

allocInMergeParams :: [(In.FParam,SubExp)]
                   -> ([FParam] -> ([SubExp] -> AllocM [SubExp]) -> AllocM a)
                   -> AllocM a
allocInMergeParams merge m = do
  ((valparams, handle_loop_subexps), (memsizeparams, memparams)) <-
    runWriterT $ unzip <$> mapM allocInMergeParam merge
  let mergeparams' = memsizeparams <> memparams <> valparams
      summary = paramsSummary mergeparams'

      mk_loop_res :: [SubExp] -> AllocM [SubExp]
      mk_loop_res ses = do
        (valargs, (memsizeargs, memargs)) <-
          runWriterT $ zipWithM ($) handle_loop_subexps ses
        return $ memsizeargs <> memargs <> valargs

  localMemoryMap (summary `HM.union`) $ m mergeparams' mk_loop_res
  where param_names = map (paramName . fst) merge
        loopInvariantShape =
          not . any (`elem` param_names) . subExpVars . arrayDims . paramType
        allocInMergeParam (mergeparam, Var v)
          | Array bt shape Unique <- paramDeclType mergeparam,
            loopInvariantShape mergeparam = do
              (mem, ixfun) <- lift $ lookupArraySummary' v
              return (mergeparam { paramAttr = ArrayMem bt shape Unique mem ixfun },
                      lift . ensureArrayIn (paramType mergeparam) mem ixfun)
        allocInMergeParam (mergeparam, _) = do
          mergeparam' <- allocInFParam mergeparam
          return (mergeparam', linearFuncallArg $ paramType mergeparam)

ensureDirectArray :: VName -> AllocM (SubExp, VName, SubExp)
ensureDirectArray v = do
  res <- lookupSummary v
  case res of
    Just (ArrayMem _ _ _ mem ixfun)
      | IxFun.isDirect ixfun -> do
        memt <- lookupType mem
        case memt of
          Mem size _ -> return (size, mem, Var v)
          _          -> fail $
                        pretty mem ++
                        " should be a memory block but has type " ++
                        pretty memt
    _ ->
      -- We need to do a new allocation, copy 'v', and make a new
      -- binding for the size of the memory block.
      allocLinearArray (baseString v) v

ensureArrayIn :: Type -> VName -> IxFun.IxFun -> SubExp -> AllocM SubExp
ensureArrayIn _ _ _ (Constant v) =
  fail $ "ensureArrayIn: " ++ pretty v ++ " cannot be an array."
ensureArrayIn t mem ixfun (Var v) = do
  (src_mem, src_ixfun) <- lookupArraySummary' v
  if src_mem == mem && src_ixfun == ixfun
    then return $ Var v
    else do copy <- newIdent (baseString v ++ "_copy") t
            let summary = ArrayMem (elemType t) (arrayShape t) NoUniqueness mem ixfun
                pat = Pattern [] [PatElem (identName copy) BindVar summary]
            letBind_ pat $ PrimOp $ Copy v
            return $ Var $ identName copy

allocLinearArray :: String
                 -> VName -> AllocM (SubExp, VName, SubExp)
allocLinearArray s v = do
  t <- lookupType v
  (size, mem) <- allocForArray t DefaultSpace
  v' <- newIdent s t
  let pat = Pattern [] [PatElem (identName v') BindVar $
                        directIndexFunction (elemType t) (arrayShape t)
                        NoUniqueness mem t]
  addBinding $ Let pat () $ PrimOp $ Copy v
  return (size, mem, Var $ identName v')

funcallArgs :: [(SubExp,Diet)] -> AllocM [(SubExp,Diet)]
funcallArgs args = do
  (valargs, (memsizeargs, memargs)) <- runWriterT $ forM args $ \(arg,d) -> do
    t <- lift $ subExpType arg
    arg' <- linearFuncallArg t arg
    return (arg', d)
  return $ map (,Observe) (memsizeargs <> memargs) <> valargs

linearFuncallArg :: Type -> SubExp -> WriterT ([SubExp], [SubExp]) AllocM SubExp
linearFuncallArg Array{} (Var v) = do
  (size, mem, arg') <- lift $ ensureDirectArray v
  tell ([size], [Var mem])
  return arg'
linearFuncallArg _ arg =
  return arg

explicitAllocations :: Pass In.Basic ExplicitMemory
explicitAllocations = simplePass
                      "explicit allocations"
                      "Transform program to explicit memory representation" $
                      intraproceduralTransformation allocInFun

memoryInRetType :: In.RetType -> RetType
memoryInRetType (ExtRetType ts) =
  evalState (mapM addAttr ts) $ startOfFreeIDRange ts
  where addAttr (Basic t) = return $ ReturnsScalar t
        addAttr Mem{} = fail "memoryInRetType: too much memory"
        addAttr (Array bt shape u) = do
          i <- get
          put $ i + 1
          return $ ReturnsArray bt shape u $ ReturnsNewBlock i

startOfFreeIDRange :: [TypeBase ExtShape u] -> Int
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
  where ensureDirect se@Constant{} = return se
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
      validents = [ (Ident name t, bindage) | PatElem name bindage t <- valElems ]
  (bnd, bnds) <- allocsForBinding sizeidents validents e'
  addBinding bnd
  mapM_ bindAllocBinding bnds

allocInExp :: In.Exp -> AllocM Exp
allocInExp (LoopOp (DoLoop res merge form
                    (Body () bodybnds bodyres))) =
  allocInMergeParams merge $ \mergeparams' mk_loop_res ->
  formBinds form $ do
    mergeinit' <- mk_loop_res mergeinit
    body' <- insertBindingsM $ allocInBindings bodybnds $ \bodybnds' -> do
      (ses,retbnds) <- collectBindings $ mk_loop_res bodyres
      return $ Body () (bodybnds'<>retbnds) ses
    return $ LoopOp $
      DoLoop res (zip mergeparams' mergeinit') form body'
  where (_mergeparams, mergeinit) = unzip merge
        formBinds (ForLoop i _) =
          localMemoryMap $ HM.insert i $ Entry $ Scalar Int
        formBinds (WhileLoop _) =
          id

allocInExp (LoopOp (MapKernel cs w index ispace inps returns body)) = do
  inps' <- mapM allocInKernelInput inps
  let mem_map = paramsSummary (map kernelInputParam inps') <> ispace_map
  localMemoryMap (mem_map <>) $ do
    body' <- allocInBindings (bodyBindings body) $ \bnds' ->
      return $ Body () bnds' $ bodyResult body
    return $ LoopOp $ MapKernel cs w index ispace inps' returns body'
  where ispace_map = HM.fromList [ (i, Entry $ Scalar Int)
                                 | i <- index : map fst ispace ]
        allocInKernelInput inp =
          case kernelInputType inp of
            Basic bt ->
              return inp { kernelInputParam = Param (kernelInputName inp) $ Scalar bt }
            Array bt shape u -> do
              (mem, ixfun) <- lookupArraySummary' $ kernelInputArray inp
              let ixfun' = IxFun.applyInd ixfun $ map SE.intSubExpToScalExp $
                           kernelInputIndices inp
                  summary = ArrayMem bt shape u mem ixfun'
              return inp { kernelInputParam = Param (kernelInputName inp) summary }
            Mem size shape ->
              return inp { kernelInputParam = Param (kernelInputName inp) $ MemMem size shape }

allocInExp (LoopOp (ReduceKernel cs w size red_lam fold_lam nes arrs)) = do
  arr_summaries <- mapM lookupSummary' arrs
  fold_lam' <- allocInChunkedLambda (kernelThreadOffsetMultiple size)
               fold_lam arr_summaries
  red_lam' <- allocInReduceLambda red_lam (kernelWorkgroupSize size)
  return $ LoopOp $ ReduceKernel cs w size red_lam' fold_lam' nes arrs

allocInExp (LoopOp (ScanKernel cs w size order lam input)) = do
  lam' <- allocInReduceLambda lam (kernelWorkgroupSize size)
  return $ LoopOp $ ScanKernel cs w size order lam' input

allocInExp (LoopOp Scan{}) =
  fail "Cannot put explicit allocations in scan yet."
allocInExp (LoopOp Redomap{}) =
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
                         , mapOnFParam = fail "Unhandled FParam in ExplicitAllocations"
                         , mapOnLParam = fail "Unhandled LParam in ExplicitAllocations"
                         }

allocInChunkedLambda :: SubExp -> In.Lambda -> [MemBound NoUniqueness] -> AllocM Lambda
allocInChunkedLambda thread_chunk lam arr_summaries = do
  let i = lambdaIndex lam
      (chunk_size_param, chunked_params) =
        partitionChunkedLambdaParameters $ lambdaParams lam
  chunked_params' <-
    forM (zip chunked_params arr_summaries) $ \(p,summary) ->
    case summary of
      Scalar _ ->
        fail $ "Passed a scalar for lambda parameter " ++ pretty p
      ArrayMem bt shape u mem ixfun ->
        return p { paramAttr =
                      ArrayMem bt shape u mem $ IxFun.offsetIndex ixfun $
                      SE.Id i Int * SE.intSubExpToScalExp thread_chunk
                 }
      _ ->
        fail $ "Chunked lambda non-array lambda parameter " ++ pretty p
  allocInLambda i (Param (paramName chunk_size_param) (Scalar Int) : chunked_params')
    (lambdaBody lam) (lambdaReturnType lam)

allocInReduceLambda :: In.Lambda
                    -> SubExp
                    -> AllocM Lambda
allocInReduceLambda lam workgroup_size = do
  let i = lambdaIndex lam
      (other_index_param, actual_params) =
        partitionChunkedLambdaParameters $ lambdaParams lam
      (acc_params, arr_params) =
        splitAt (length actual_params `div` 2) actual_params
      this_index = SE.Id i Int `SE.SRem`
                   SE.intSubExpToScalExp workgroup_size
      other_index = SE.Id (paramName other_index_param) Int
  acc_params' <-
    allocInReduceParameters workgroup_size this_index acc_params
  arr_params' <-
    forM (zip arr_params $ map paramAttr acc_params') $ \(param, attr) ->
    case attr of
      ArrayMem bt shape u mem _ -> return param {
        paramAttr = ArrayMem bt shape u mem $
                    IxFun.applyInd
                    (IxFun.iota $ IxFun.shapeFromSubExps $
                     workgroup_size : arrayDims (paramType param))
                    [this_index + other_index]
        }
      _ ->
        return param { paramAttr = attr }

  allocInLambda i (other_index_param { paramAttr = Scalar Int } :
                   acc_params' ++ arr_params')
    (lambdaBody lam) (lambdaReturnType lam)

allocInReduceParameters :: SubExp
                        -> SE.ScalExp
                        -> [In.LParam]
                        -> AllocM [LParam]
allocInReduceParameters workgroup_size local_id = mapM allocInReduceParameter
  where allocInReduceParameter p =
          case paramType p of
            t@(Array bt shape u) -> do
              (_, shared_mem) <- allocForLocalArray workgroup_size t
              let ixfun = IxFun.applyInd
                          (IxFun.iota $ IxFun.shapeFromSubExps $
                           workgroup_size : arrayDims t)
                          [local_id]
              return p { paramAttr = ArrayMem bt shape u shared_mem ixfun
                       }
            Basic bt ->
              return p { paramAttr = Scalar bt }
            Mem size space ->
              return p { paramAttr = MemMem size space }

allocInLambda :: VName -> [LParam] -> In.Body -> [Type]
              -> AllocM Lambda
allocInLambda i params body rettype = do
  let param_summaries = paramsSummary params
      all_summaries = HM.insert i (Entry $ Scalar Int) param_summaries
  body' <- localMemoryMap (HM.union all_summaries) $
           allocInBody body
  return $ Lambda i params body' rettype

vtableToAllocEnv :: ST.SymbolTable (Wise ExplicitMemory)
                 -> MemoryMap
vtableToAllocEnv = HM.fromList . mapMaybe entryToMemBound .
                   HM.toList . ST.bindings
  where entryToMemBound (k,entry) = do
          summary <- (snd <$> ST.entryLetBoundLore entry) <|>
                     (fmap (const NoUniqueness) <$> ST.entryFParamLore entry)
          return (k, Entry summary)

simplifiable :: (Engine.MonadEngine m,
                 Engine.InnerLore m ~ ExplicitMemory) =>
                SimpleOps m
simplifiable =
  SimpleOps mkLetS' mkBodyS' mkLetNamesS'
  simplifyMemBound simplifyMemBound simplifyMemBound
  simplifyRetType'
  where mkLetS' _ pat e =
          return $ mkWiseLetBinding (removePatternWisdom pat) () e

        mkBodyS' _ bnds res = return $ mkWiseBody () bnds res

        mkLetNamesS' vtable names e = do
          pat' <- bindPatternWithAllocations env names $
                  removeExpWisdom e
          return $ mkWiseLetBinding pat' () e
          where env = vtableToAllocEnv vtable

        simplifyMemBound (Scalar bt) =
          return $ Scalar bt
        simplifyMemBound (MemMem size space) =
          MemMem <$> Engine.simplify size <*> pure space
        simplifyMemBound (ArrayMem bt shape u mem ixfun) =
          ArrayMem bt shape u <$> Engine.simplify mem <*> pure ixfun

        simplifyRetType' = mapM simplifyReturns
          where simplifyReturns (ReturnsScalar bt) =
                  return $ ReturnsScalar bt
                simplifyReturns (ReturnsArray bt shape u ret) =
                  ReturnsArray bt <$>
                  Engine.simplify shape <*>
                  pure u <*>
                  simplifyMemReturn ret
                simplifyReturns (ReturnsMemory size space) =
                  ReturnsMemory <$> Engine.simplify size <*> pure space
                simplifyMemReturn (ReturnsNewBlock i) =
                  return $ ReturnsNewBlock i
                simplifyMemReturn (ReturnsInBlock v ixfun) =
                  ReturnsInBlock <$> Engine.simplify v <*>
                  pure ixfun

bindPatternWithAllocations :: MonadBinder m =>
                              MemoryMap -> [(VName, Bindage)] -> Exp
                           -> m Pattern
bindPatternWithAllocations memoryMap names e = do
  (pat,prebnds) <- runPatAllocM (patternWithAllocations names e) memoryMap
  mapM_ bindAllocBinding prebnds
  return pat
