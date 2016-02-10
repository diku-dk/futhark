{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, FlexibleContexts, TupleSections, LambdaCase, FlexibleInstances, MultiParamTypeClasses #-}
module Futhark.Pass.ExplicitAllocations
       ( explicitAllocations
       , simplifiable

       , arraySizeInBytesExp
       )
where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS.Strict
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import qualified Futhark.Representation.Kernels as In
import Futhark.Optimise.Simplifier.Lore
  (mkWiseBody,
   mkWiseLetBinding,
   removeExpWisdom,
   removePatternWisdom,
   removeScopeWisdom)
import Futhark.MonadFreshNames
import Futhark.Representation.ExplicitMemory
import qualified Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe as IxFun
import Futhark.Tools
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.ScalExp as SE
import Futhark.Optimise.Simplifier.Simple (SimpleOps (..))
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import Futhark.Pass

import Prelude

data AllocBinding = SizeComputation VName SE.ScalExp
                  | Allocation VName SubExp Space
                  | ArrayCopy VName Bindage VName
                    deriving (Eq, Ord, Show)

bindAllocBinding :: (MonadBinder m, Op (Lore m) ~ MemOp (Lore m)) =>
                    AllocBinding -> m ()
bindAllocBinding (SizeComputation name se) = do
  e <- SE.fromScalExp se
  letBindNames'_ [name] e
bindAllocBinding (Allocation name size space) =
  letBindNames'_ [name] $ Op $ Alloc size space
bindAllocBinding (ArrayCopy name bindage src) =
  letBindNames_ [(name,bindage)] $ PrimOp $ Copy src

class (MonadFreshNames m, HasScope ExplicitMemory m) => Allocator m where
  addAllocBinding :: AllocBinding -> m ()

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
newtype AllocM a = AllocM (BinderT ExplicitMemory (State VNameSource) a)
                 deriving (Applicative, Functor, Monad,
                           MonadFreshNames,
                           HasScope ExplicitMemory,
                           LocalScope ExplicitMemory)

instance MonadBinder AllocM where
  type Lore AllocM = ExplicitMemory

  mkLetM pat e = return $ Let pat () e

  mkLetNamesM names e = do
    pat <- patternWithAllocations names e
    return $ Let pat () e

  mkBodyM bnds res = return $ Body () bnds res

  addBinding binding =
    AllocM $ addBinderBinding binding
  collectBindings (AllocM m) =
    AllocM $ collectBinderBindings m

instance Allocator AllocM where
  addAllocBinding (SizeComputation name se) =
    letBindNames'_ [name] =<< SE.fromScalExp se
  addAllocBinding (Allocation name size space) =
    letBindNames'_ [name] $ Op $ Alloc size space
  addAllocBinding (ArrayCopy name bindage src) =
    letBindNames_ [(name, bindage)] $ PrimOp $ SubExp $ Var src

runAllocM :: MonadFreshNames m => AllocM a -> m a
runAllocM (AllocM m) =
  fmap fst $ modifyNameSource $ runState $ runBinderT m mempty

-- | Monad for adding allocations to a single pattern.
newtype PatAllocM a = PatAllocM (RWS
                                 (Scope ExplicitMemory)
                                 [AllocBinding]
                                 VNameSource
                                 a)
                    deriving (Applicative, Functor, Monad,
                              HasScope ExplicitMemory,
                              MonadWriter [AllocBinding],
                              MonadFreshNames)

instance Allocator PatAllocM where
  addAllocBinding = tell . pure

runPatAllocM :: MonadFreshNames m =>
                PatAllocM a -> Scope ExplicitMemory
             -> m (a, [AllocBinding])
runPatAllocM (PatAllocM m) mems =
  modifyNameSource $ frob . runRWS m mems
  where frob (a,s,w) = ((a,w),s)

arraySizeInBytesExp :: Type -> SE.ScalExp
arraySizeInBytesExp t =
  SE.sproduct $
  primByteSize (elemType t) :
  map (`SE.subExpToScalExp` int32) (arrayDims t)

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
  rts <- expReturns e
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
  let sizes' = [ PatElem size BindVar $ Scalar int32 | size <- map identName sizeidents ]
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
              (destmem,destixfun) <- lift $ lookupArraySummary src
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
              tell ([PatElem (identName memsize) BindVar $ Scalar int32],
                    [PatElem (identName mem)     BindVar $ MemMem (Var $ identName memsize) DefaultSpace],
                    [ArrayCopy (identName ident) bindage $
                     identName tmp_buffer])
              return $ PatElem (identName tmp_buffer) BindVar $
                ArrayMem bt (stripDims (length is) shape) u (identName mem) ixfun

        ReturnsArray bt _ u _ -> do
          (memsize,mem,(ident',ixfun)) <- lift $ memForBindee ident
          tell ([PatElem (identName memsize) BindVar $ Scalar int32],
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
summaryForBindage (Prim bt) BindVar =
  return $ Scalar bt
summaryForBindage (Mem size space) BindVar =
  return $ MemMem size space
summaryForBindage t@(Array bt shape u) BindVar = do
  (_, m) <- allocForArray t DefaultSpace
  return $ directIndexFunction bt shape u m t
summaryForBindage _ (BindInPlace _ src _) =
  lookupMemBound src

memForBindee :: (MonadFreshNames m) =>
                Ident
             -> m (Ident,
                   Ident,
                   (Ident, IxFun.IxFun))
memForBindee ident = do
  size <- newIdent (memname <> "_size") (Prim int32)
  mem <- newIdent memname $ Mem (Var $ identName size) DefaultSpace
  return (size,
          mem,
          (ident, IxFun.iota $ IxFun.shapeFromSubExps $ arrayDims t))
  where  memname = baseString (identName ident) <> "_mem"
         t       = identType ident

directIndexFunction :: PrimType -> Shape -> u -> VName -> Type -> MemBound u
directIndexFunction bt shape u mem t =
  ArrayMem bt shape u mem $ IxFun.iota $ IxFun.shapeFromSubExps $ arrayDims t

patElemSummary :: PatElem -> (VName, NameInfo ExplicitMemory)
patElemSummary bindee = (patElemName bindee,
                         LetInfo $ patElemAttr bindee)

bindeesSummary :: [PatElem] -> Scope ExplicitMemory
bindeesSummary = HM.fromList . map patElemSummary

fparamsSummary :: [FParam] -> Scope ExplicitMemory
fparamsSummary = HM.fromList . map paramSummary
  where paramSummary fparam =
          (paramName fparam,
           FParamInfo $ paramAttr fparam)

lparamsSummary :: [LParam] -> Scope ExplicitMemory
lparamsSummary = HM.fromList . map paramSummary
  where paramSummary fparam =
          (paramName fparam,
           LParamInfo $ paramAttr fparam)

allocInFParams :: [In.FParam] -> ([FParam] -> AllocM a)
               -> AllocM a
allocInFParams params m = do
  (valparams, (memsizeparams, memparams)) <-
    runWriterT $ mapM allocInFParam params
  let params' = memsizeparams <> memparams <> valparams
      summary = fparamsSummary params'
  localScope summary $ m params'

allocInFParam :: MonadFreshNames m =>
                 In.FParam -> WriterT ([FParam], [FParam]) m FParam
allocInFParam param =
  case paramDeclType param of
    Array bt shape u -> do
      let memname = baseString (paramName param) <> "_mem"
          ixfun = IxFun.iota $ IxFun.shapeFromSubExps $ shapeDims shape
      memsize <- lift $ newVName (memname <> "_size")
      mem <- lift $ newVName memname
      tell ([Param memsize $ Scalar int32],
            [Param mem $ MemMem (Var memsize) DefaultSpace])
      return param { paramAttr =  ArrayMem bt shape u mem ixfun }
    Prim bt ->
      return param { paramAttr = Scalar bt }
    Mem size space ->
      return param { paramAttr = MemMem size space }

allocInMergeParams :: [(In.FParam,SubExp)]
                   -> ([FParam]
                       -> [FParam]
                       -> ([SubExp] -> AllocM ([SubExp], [SubExp]))
                       -> AllocM a)
                   -> AllocM a
allocInMergeParams merge m = do
  ((valparams, handle_loop_subexps), (memsizeparams, memparams)) <-
    runWriterT $ unzip <$> mapM allocInMergeParam merge
  let mergeparams' = memsizeparams <> memparams <> valparams
      summary = fparamsSummary mergeparams'

      mk_loop_res :: [SubExp] -> AllocM ([SubExp], [SubExp])
      mk_loop_res ses = do
        (valargs, (memsizeargs, memargs)) <-
          runWriterT $ zipWithM ($) handle_loop_subexps ses
        return (memsizeargs <> memargs, valargs)

  localScope summary $ m (memsizeparams<>memparams) valparams mk_loop_res
  where param_names = map (paramName . fst) merge
        loopInvariantShape =
          not . any (`elem` param_names) . subExpVars . arrayDims . paramType
        allocInMergeParam (mergeparam, Var v)
          | Array bt shape Unique <- paramDeclType mergeparam,
            loopInvariantShape mergeparam = do
              (mem, ixfun) <- lift $ lookupArraySummary v
              return (mergeparam { paramAttr = ArrayMem bt shape Unique mem ixfun },
                      lift . ensureArrayIn (paramType mergeparam) mem ixfun)
        allocInMergeParam (mergeparam, _) = do
          mergeparam' <- allocInFParam mergeparam
          return (mergeparam', linearFuncallArg $ paramType mergeparam)

ensureDirectArray :: VName -> AllocM (SubExp, VName, SubExp)
ensureDirectArray v = do
  res <- lookupMemBound v
  case res of
    ArrayMem _ _ _ mem ixfun
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
  (src_mem, src_ixfun) <- lookupArraySummary v
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

explicitAllocations :: Pass In.Kernels ExplicitMemory
explicitAllocations = simplePass
                      "explicit allocations"
                      "Transform program to explicit memory representation" $
                      intraproceduralTransformation allocInFun

memoryInRetType :: In.RetType -> RetType
memoryInRetType (ExtRetType ts) =
  evalState (mapM addAttr ts) $ startOfFreeIDRange ts
  where addAttr (Prim t) = return $ ReturnsScalar t
        addAttr Mem{} = fail "memoryInRetType: too much memory"
        addAttr (Array bt shape u) = do
          i <- get
          put $ i + 1
          return $ ReturnsArray bt shape u $ ReturnsNewBlock i Nothing

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
          bt <- primType <$> lookupType v
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
          localScope summaries $
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
allocInExp (DoLoop ctx val form (Body () bodybnds bodyres)) =
  allocInMergeParams ctx $ \_ ctxparams' _ ->
  allocInMergeParams val $ \new_ctx_params valparams' mk_loop_val ->
  formBinds form $ do
    (valinit_ctx, valinit') <- mk_loop_val valinit
    body' <- insertBindingsM $ allocInBindings bodybnds $ \bodybnds' -> do
      ((val_ses,valres'),val_retbnds) <- collectBindings $ mk_loop_val valres
      return $ Body ()
        (bodybnds'<>val_retbnds)
        (val_ses++ctxres++valres')
    return $
      DoLoop
      (zip (new_ctx_params++ctxparams') (valinit_ctx++ctxinit))
      (zip valparams' valinit')
      form body'
  where (_ctxparams, ctxinit) = unzip ctx
        (_valparams, valinit) = unzip val
        (ctxres, valres) = splitAt (length ctx) bodyres
        formBinds (ForLoop i _) =
          localScope $ HM.singleton i IndexInfo
        formBinds (WhileLoop _) =
          id

allocInExp (Op (MapKernel cs w index ispace inps returns body)) = do
  inps' <- mapM allocInKernelInput inps
  let mem_map = lparamsSummary (map kernelInputParam inps') <> ispace_map
  localScope mem_map $ do
    body' <- allocInBindings (bodyBindings body) $ \bnds' ->
      return $ Body () bnds' $ bodyResult body
    return $ Op $ Inner $ MapKernel cs w index ispace inps' returns body'
  where ispace_map = HM.fromList [ (i, IndexInfo)
                                 | i <- index : map fst ispace ]
        allocInKernelInput inp =
          case kernelInputType inp of
            Prim bt ->
              return inp { kernelInputParam = Param (kernelInputName inp) $ Scalar bt }
            Array bt shape u -> do
              (mem, ixfun) <- lookupArraySummary $ kernelInputArray inp
              let ixfun' = IxFun.applyInd ixfun $ map SE.intSubExpToScalExp $
                           kernelInputIndices inp
                  summary = ArrayMem bt shape u mem ixfun'
              return inp { kernelInputParam = Param (kernelInputName inp) summary }
            Mem size shape ->
              return inp { kernelInputParam = Param (kernelInputName inp) $ MemMem size shape }

allocInExp (Op (ReduceKernel cs w size comm red_lam fold_lam nes arrs)) = do
  arr_summaries <- mapM lookupMemBound arrs
  fold_lam' <- allocInFoldLambda
               comm
               (kernelElementsPerThread size)
               (kernelNumThreads size)
               fold_lam arr_summaries
  red_lam' <- allocInReduceLambda red_lam (kernelWorkgroupSize size)
  return $ Op $ Inner $ ReduceKernel cs w size comm red_lam' fold_lam' nes arrs

allocInExp (Op (ScanKernel cs w size order lam input)) = do
  lam' <- allocInReduceLambda lam (kernelWorkgroupSize size)
  return $ Op $ Inner $ ScanKernel cs w size order lam' input

allocInExp (Apply fname args rettype) = do
  args' <- funcallArgs args
  return $ Apply fname args' (memoryInRetType rettype)
allocInExp e = mapExpM alloc e
  where alloc =
          identityMapper { mapOnBody = allocInBody
                         , mapOnRetType = return . memoryInRetType
                         , mapOnFParam = fail "Unhandled FParam in ExplicitAllocations"
                         , mapOnOp = \op ->
                             fail $ "Unhandled Op in ExplicitAllocations: " ++ pretty op
                         }

allocInFoldLambda :: Commutativity
                  -> SubExp -> SubExp
                  -> In.Lambda -> [MemBound NoUniqueness]
                  -> AllocM Lambda
allocInFoldLambda comm elems_per_thread num_threads lam arr_summaries = do
  let i = lambdaIndex lam
      (chunk_size_param, chunked_params) =
        partitionChunkedLambdaParameters $ lambdaParams lam
  chunked_params' <-
    forM (zip chunked_params arr_summaries) $ \(p,summary) ->
    case summary of
      Scalar _ ->
        fail $ "Passed a scalar for lambda parameter " ++ pretty p
      ArrayMem bt shape u mem ixfun ->
        case comm of
          Noncommutative -> do
            let newshape = [DimNew num_threads, DimNew elems_per_thread]
            return p { paramAttr =
                       ArrayMem bt (arrayShape $ paramType p) u mem $
                       IxFun.applyInd
                       (IxFun.reshape ixfun $
                        newshape ++ map DimNew (drop 1 $ shapeDims shape))
                       [SE.Id i int32]
                     }
          Commutative -> do
            let newshape = [DimNew elems_per_thread, DimNew num_threads]
                perm = [1,0] ++ [2..IxFun.rank ixfun]
            return p { paramAttr =
                       ArrayMem bt (arrayShape $ paramType p) u mem $
                       IxFun.applyInd
                       (IxFun.permute (IxFun.reshape ixfun $
                        newshape ++ map DimNew (drop 1 $ shapeDims shape))
                        perm)
                       [SE.Id i int32]
                     }
      _ ->
        fail $ "Chunked lambda non-array lambda parameter " ++ pretty p
  allocInLambda i (Param (paramName chunk_size_param) (Scalar int32) : chunked_params')
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
      this_index = SE.Id i int32 `SE.SRem`
                   SE.intSubExpToScalExp workgroup_size
      other_index = SE.Id (paramName other_index_param) int32
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

  allocInLambda i (other_index_param { paramAttr = Scalar int32 } :
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
            Prim bt ->
              return p { paramAttr = Scalar bt }
            Mem size space ->
              return p { paramAttr = MemMem size space }

allocInLambda :: VName -> [LParam] -> In.Body -> [Type]
              -> AllocM Lambda
allocInLambda i params body rettype = do
  let param_summaries = lparamsSummary params
      all_summaries = HM.insert i IndexInfo param_summaries
  body' <- localScope all_summaries $
           allocInBindings (bodyBindings body) $ \bnds' ->
           return $ Body () bnds' $ bodyResult body
  return $ Lambda i params body' rettype

simplifiable :: (Engine.MonadEngine m,
                 Engine.InnerLore m ~ ExplicitMemory) =>
                SimpleOps m
simplifiable =
  SimpleOps mkLetS' mkBodyS' mkLetNamesS'
  where mkLetS' _ pat e =
          return $ mkWiseLetBinding (removePatternWisdom pat) () e

        mkBodyS' _ bnds res = return $ mkWiseBody () bnds res

        mkLetNamesS' vtable names e = do
          pat' <- bindPatternWithAllocations env names $
                  removeExpWisdom e
          return $ mkWiseLetBinding pat' () e
          where env = removeScopeWisdom $ ST.typeEnv vtable

bindPatternWithAllocations :: (MonadBinder m, Op (Lore m) ~ MemOp (Lore m)) =>
                              Scope ExplicitMemory -> [(VName, Bindage)] -> Exp
                           -> m Pattern
bindPatternWithAllocations types names e = do
  (pat,prebnds) <- runPatAllocM (patternWithAllocations names e) types
  mapM_ bindAllocBinding prebnds
  return pat
