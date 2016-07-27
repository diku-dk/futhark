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
import Control.Monad.Reader
import Control.Monad.RWS.Strict
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe

import qualified Futhark.Representation.Kernels as In
import Futhark.Optimise.Simplifier.Lore
  (mkWiseBody,
   mkWiseLetBinding,
   removeExpWisdom,
   removePatternWisdom,
   removeScopeWisdom)
import Futhark.MonadFreshNames
import Futhark.Representation.ExplicitMemory
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
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
  -- | The subexpression giving the number of elements we should
  -- allocate space for.  See 'ChunkMap' comment.
  dimAllocationSize :: SubExp -> m SubExp

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

-- | A mapping from chunk names to their maximum size.  XXX FIXME
-- HACK: This is part of a hack to add loop-invariant allocations to
-- reduce kernels, because memory expansion does not use range
-- analysis yet (it should).
type ChunkMap = HM.HashMap VName SubExp

-- | Monad for adding allocations to an entire program.
newtype AllocM a = AllocM (BinderT ExplicitMemory (ReaderT ChunkMap (State VNameSource)) a)
                 deriving (Applicative, Functor, Monad,
                           MonadFreshNames,
                           HasScope ExplicitMemory,
                           LocalScope ExplicitMemory,
                           MonadReader ChunkMap)

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

  dimAllocationSize (Var v) =
    fromMaybe (Var v) <$> asks (HM.lookup v)
  dimAllocationSize size =
    return size

runAllocM :: MonadFreshNames m => AllocM a -> m a
runAllocM (AllocM m) =
  fmap fst $ modifyNameSource $ runState $ runReaderT (runBinderT m mempty) mempty

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
  dimAllocationSize = return

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

arraySizeInBytesExpM :: Allocator m => Type -> m SE.ScalExp
arraySizeInBytesExpM t =
  SE.sproduct .
  (primByteSize (elemType t) :) .
  map (`SE.subExpToScalExp` int32) <$>
  mapM dimAllocationSize (arrayDims t)

arraySizeInBytes :: Allocator m => Type -> m SubExp
arraySizeInBytes = computeSize "bytes" <=< arraySizeInBytesExpM

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
  size <- computeSize "local_bytes" =<<
          (SE.intSubExpToScalExp workgroup_size*) <$>
          arraySizeInBytesExpM t
  m <- allocateMemory "local_mem" size $ Space "local"
  return (size, m)

allocsForBinding :: Allocator m =>
                    [Ident] -> [(Ident,Bindage)] -> Exp
                 -> m (Binding, [AllocBinding])
allocsForBinding sizeidents validents e = do
  rts <- expReturns e
  hints <- expHints e
  (ctxElems, valElems, postbnds) <- allocsForPattern sizeidents validents rts hints
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
                    [Ident] -> [(Ident,Bindage)] -> [ExpReturns] -> [ExpHint]
                 -> m ([PatElem], [PatElem], [AllocBinding])
allocsForPattern sizeidents validents rts hints = do
  let sizes' = [ PatElem size BindVar $ Scalar int32 | size <- map identName sizeidents ]
  (vals,(memsizes, mems, postbnds)) <-
    runWriterT $ forM (zip3 validents rts hints) $ \((ident,bindage), rt, hint) -> do
      let shape = arrayShape $ identType ident
      case rt of
        ReturnsScalar _ -> do
          summary <- lift $ summaryForBindage (identType ident) bindage hint
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
            summary <- lift $ summaryForBindage (identType ident) bindage hint
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
                     Type -> Bindage -> ExpHint
                  -> m (MemBound NoUniqueness)
summaryForBindage (Prim bt) BindVar _ =
  return $ Scalar bt
summaryForBindage (Mem size space) BindVar _ =
  return $ MemMem size space
summaryForBindage t@(Array bt shape u) BindVar NoHint = do
  (_, m) <- allocForArray t DefaultSpace
  return $ directIndexFunction bt shape u m t
summaryForBindage t BindVar (IxFunHint ixfun) = do
  let bt = elemType t
  bytes <- computeSize "bytes" $ product $ primByteSize bt : IxFun.base ixfun
  m <- allocateMemory "mem" bytes DefaultSpace
  return $ ArrayMem bt (arrayShape t) NoUniqueness m ixfun
summaryForBindage _ (BindInPlace _ src _) _ =
  lookupMemBound src

memForBindee :: (MonadFreshNames m) =>
                Ident
             -> m (Ident,
                   Ident,
                   (Ident, IxFun.IxFun SE.ScalExp))
memForBindee ident = do
  size <- newIdent (memname <> "_size") (Prim int32)
  mem <- newIdent memname $ Mem (Var $ identName size) DefaultSpace
  return (size,
          mem,
          (ident, IxFun.iota $ map SE.intSubExpToScalExp $ arrayDims t))
  where  memname = baseString (identName ident) <> "_mem"
         t       = identType ident

directIndexFunction :: PrimType -> Shape -> u -> VName -> Type -> MemBound u
directIndexFunction bt shape u mem t =
  ArrayMem bt shape u mem $ IxFun.iota $ map SE.intSubExpToScalExp $ arrayDims t

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
          ixfun = IxFun.iota $ map SE.intSubExpToScalExp $ shapeDims shape
      memsize <- lift $ newVName (memname <> "_size")
      mem <- lift $ newVName memname
      tell ([Param memsize $ Scalar int32],
            [Param mem $ MemMem (Var memsize) DefaultSpace])
      return param { paramAttr =  ArrayMem bt shape u mem ixfun }
    Prim bt ->
      return param { paramAttr = Scalar bt }
    Mem size space ->
      return param { paramAttr = MemMem size space }

allocInMergeParams :: [VName]
                   -> [(In.FParam,SubExp)]
                   -> ([FParam]
                       -> [FParam]
                       -> ([SubExp] -> AllocM ([SubExp], [SubExp]))
                       -> AllocM a)
                   -> AllocM a
allocInMergeParams variant merge m = do
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
  where variant_names = variant ++ map (paramName . fst) merge
        loopInvariantShape =
          not . any (`elem` variant_names) . subExpVars . arrayDims . paramType
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
    ArrayMem _ shape _ mem ixfun
      | fullyDirect shape ixfun -> do
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

ensureArrayIn :: Type -> VName -> IxFun.IxFun SE.ScalExp -> SubExp -> AllocM SubExp
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

allocInFun :: MonadFreshNames m => In.FunDef -> m FunDef
allocInFun (In.FunDef entry fname rettype params body) =
  runAllocM $ allocInFParams params $ \params' -> do
    body' <- insertBindingsM $ allocInBody body
    return $ FunDef entry fname (memoryInRetType rettype) params' body'

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
  allocInMergeParams mempty ctx $ \_ ctxparams' _ ->
  allocInMergeParams (map paramName ctxparams') val $
  \new_ctx_params valparams' mk_loop_val ->
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

allocInExp (Op (ScanKernel cs w size lam foldlam nes arrs)) = do
  lam' <- allocInScanLambda lam (length nes) $ kernelWorkgroupSize size
  foldlam' <- allocInScanLambda foldlam (length nes) $ kernelWorkgroupSize size
  return $ Op $ Inner $ ScanKernel cs w size lam' foldlam' nes arrs

allocInExp (Op (WriteKernel cs len lam ivs as)) = do
  -- We require Write to be in-place, so there is no need to allocate any
  -- memory, except for the parameters.
  let (tid_param, [], real_params) =
        partitionChunkedFoldParameters 0 $ lambdaParams lam
      tid_param' = tid_param { paramAttr = Scalar int32 }
  params' <- zipWithM (allocInWriteParam $ Var $ paramName tid_param)
             real_params ivs
  lam' <- allocInLambda (tid_param' : params') (lambdaBody lam) (lambdaReturnType lam)
  return $ Op $ Inner $ WriteKernel cs len lam' ivs as
  where allocInWriteParam tid param arr =
          case paramType param of
            Prim bt ->
              return param { paramAttr = Scalar bt }
            Array bt shape u -> do
              (mem, ixfun) <- lookupArraySummary arr
              let ixfun' = IxFun.applyInd ixfun [SE.intSubExpToScalExp tid]
                  summary = ArrayMem bt shape u mem ixfun'
              return param { paramAttr = summary }
            Mem size shape ->
              return param { paramAttr = MemMem size shape }

allocInExp (Op (Kernel cs space ts body)) = do
  body' <- localScope (scopeOfKernelSpace space) $
    allocInKernelBody space body
  return $ Op $ Inner $ Kernel cs space ts body'

allocInExp (Op GroupSize) =
  return $ Op $ Inner GroupSize

allocInExp (Op NumGroups) =
  return $ Op $ Inner NumGroups

allocInExp (Apply fname args rettype) = do
  args' <- funcallArgs args
  return $ Apply fname args' (memoryInRetType rettype)
allocInExp e = mapExpM alloc e
  where alloc =
          identityMapper { mapOnBody = allocInBody
                         , mapOnRetType = return . memoryInRetType
                         , mapOnFParam = fail "Unhandled FParam in ExplicitAllocations"
                         , mapOnOp = \op ->
                             fail $ "Unhandled Op in ExplicitAllocations:\n" ++ pretty op
                         }

allocInScanLambda :: In.Lambda
                  -> Int
                  -> SubExp
                  -> AllocM Lambda
allocInScanLambda lam num_accs workgroup_size = do
  let (i, other_index_param, actual_params) =
        partitionChunkedKernelLambdaParameters $ lambdaParams lam
      (acc_params, arr_params) =
        splitAt num_accs actual_params
      this_index = SE.Id i int32 `SE.SRem`
                   SE.intSubExpToScalExp workgroup_size
      other_index = SE.Id (paramName other_index_param) int32
  acc_params' <-
    allocInScanParameters workgroup_size this_index 0 acc_params
  arr_params' <-
    allocInScanParameters workgroup_size this_index other_index arr_params

  allocInLambda (Param i (Scalar int32) :
                 other_index_param { paramAttr = Scalar int32 } :
                 acc_params' ++ arr_params')
    (lambdaBody lam) (lambdaReturnType lam)

allocInScanParameters :: SubExp
                      -> SE.ScalExp
                      -> SE.ScalExp
                      -> [In.LParam]
                      -> AllocM [LParam]
allocInScanParameters workgroup_size my_id offset = mapM allocInScanParameter
  where allocInScanParameter p =
          case paramType p of
            t@(Array bt shape u) -> do
              (_, shared_mem) <- allocForLocalArray workgroup_size t
              let ixfun = IxFun.applyInd
                          (IxFun.iota $ map SE.intSubExpToScalExp $
                           workgroup_size : arrayDims t)
                          [my_id + offset]
              return p { paramAttr = ArrayMem bt shape u shared_mem ixfun
                       }
            Prim bt ->
              return p { paramAttr = Scalar bt }
            Mem size space ->
              return p { paramAttr = MemMem size space }

allocInReduceLambda :: In.Lambda
                    -> [(VName, IxFun.IxFun SE.ScalExp)]
                    -> AllocM Lambda
allocInReduceLambda lam input_summaries = do
  let (i, other_offset_param, actual_params) =
        partitionChunkedKernelLambdaParameters $ lambdaParams lam
      (acc_params, arr_params) =
        splitAt (length input_summaries) actual_params
      this_index = SE.Id i int32
      other_offset = SE.Id (paramName other_offset_param) int32
  acc_params' <-
    allocInReduceParameters this_index 0 $
    zip acc_params input_summaries
  arr_params' <-
    allocInReduceParameters this_index other_offset $
    zip arr_params input_summaries

  allocInLambda (Param i (Scalar int32) :
                 other_offset_param { paramAttr = Scalar int32 } :
                 acc_params' ++ arr_params')
    (lambdaBody lam) (lambdaReturnType lam)

allocInReduceParameters :: SE.ScalExp
                        -> SE.ScalExp
                        -> [(In.LParam,(VName, IxFun.IxFun SE.ScalExp))]
                        -> AllocM [LParam]
allocInReduceParameters my_id offset = mapM allocInReduceParameter
  where allocInReduceParameter (p, (mem, ixfun)) =
          case paramType p of
            (Array bt shape u) ->
              let ixfun' = IxFun.applyInd ixfun [my_id + offset]
              in return p { paramAttr = ArrayMem bt shape u mem ixfun' }
            Prim bt ->
              return p { paramAttr = Scalar bt }
            Mem size space ->
              return p { paramAttr = MemMem size space }

allocInChunkedParameters :: SE.ScalExp
                        -> [(In.LParam, (VName, IxFun.IxFun SE.ScalExp))]
                        -> AllocM [LParam]
allocInChunkedParameters offset = mapM allocInChunkedParameter
  where allocInChunkedParameter (p, (mem, ixfun)) =
          case paramType p of
            Array bt shape u ->
              let ixfun' = IxFun.offsetIndex ixfun offset
              in return p { paramAttr = ArrayMem bt shape u mem ixfun' }
            Prim bt ->
              return p { paramAttr = Scalar bt }
            Mem size space ->
              return p { paramAttr = MemMem size space }

allocInLambda :: [LParam] -> In.Body -> [Type]
              -> AllocM Lambda
allocInLambda params body rettype = do
  body' <- localScope (lparamsSummary params) $
           allocInBindings (bodyBindings body) $ \bnds' ->
           return $ Body () bnds' $ bodyResult body
  return $ Lambda params body' rettype

allocInKernelBody :: FreeIn res =>
                     KernelSpace
                  -> GenKernelBody res In.Kernels
                  -> AllocM (GenKernelBody res ExplicitMemory)
allocInKernelBody space (KernelBody stms res) =
  allocInKernelStms space returned stms $ \stms' ->
    return $ KernelBody stms' res
  where returned = freeIn res

allocInKernelStms :: KernelSpace
                  -> Names
                  -> [KernelStm In.Kernels]
                  -> ([KernelStm ExplicitMemory] -> AllocM a)
                  -> AllocM a
allocInKernelStms space returned origstms m = allocInStms' origstms []
  where allocInStms' [] stms' =
          m stms'
        allocInStms' (x:xs) stms' = do
          allocstms <- allocInKernelStm space returned x
          let summaries = mconcat $ map scopeOf allocstms
          local (<>mconcat (map sizeSubst allocstms)) $ localScope summaries $
            allocInStms' xs (stms'++allocstms)

sizeSubst :: KernelStm ExplicitMemory -> ChunkMap
sizeSubst (SplitArray (size,_) _ _ elems_per_thread _) =
  HM.singleton size elems_per_thread
sizeSubst _ = mempty

allocInKernelStm :: KernelSpace
                 -> Names
                 -> KernelStm In.Kernels
                 -> AllocM [KernelStm ExplicitMemory]
allocInKernelStm space _ (SplitArray (size,chunks) o w elems_per_thread arrs) = do
  chunks' <- forM (zip chunks arrs) $ \(chunk, arr) -> do
    (mem, ixfun) <- lookupArraySummary arr
    let num_threads' = SE.intSubExpToScalExp num_threads
        elems_per_thread' = SE.intSubExpToScalExp elems_per_thread
        num_threads = spaceNumThreads space
        thread_id = spaceGlobalId space
        shape = arrayShape $ patElemType chunk
        bt = elemType $ patElemType chunk
        attr =
          case o of
            InOrder ->
              let newshape = [DimNew num_threads', DimNew elems_per_thread']
              in ArrayMem bt shape NoUniqueness mem $
                 IxFun.applyInd
                 (IxFun.reshape ixfun $
                   newshape ++
                   map (DimNew . SE.intSubExpToScalExp)
                   (drop 1 $ shapeDims shape))
                 [SE.Id thread_id int32]
            Disorder ->
              let newshape = [DimNew elems_per_thread', DimNew num_threads']
                  perm = [1,0] ++ [2..IxFun.rank ixfun]
              in ArrayMem bt shape NoUniqueness mem $
                 IxFun.applyInd
                 (IxFun.permute (IxFun.reshape ixfun $
                                  newshape ++
                                  map (DimNew . SE.intSubExpToScalExp)
                                  (drop 1 $ shapeDims shape))
                   perm)
                 [SE.Id thread_id int32]
    return chunk { patElemAttr = attr }
  return [SplitArray (size, chunks') o w elems_per_thread arrs]

allocInKernelStm _ _ (Thread threads bnd) = do
  bnds <- collectBindings_ $ allocInBinding bnd
  return $ map (Thread threads) bnds

allocInKernelStm _ _ (Combine pe w v) = do
  let pe_t = patElemType pe
      shape = arrayShape pe_t
      bt = elemType pe_t
  alloc_dims <- mapM dimAllocationSize $ shapeDims shape
  (_, mem) <- allocForArray pe_t $ Space "local"
  let ixfun = IxFun.iota $ map SE.intSubExpToScalExp alloc_dims
      attr = ArrayMem bt shape NoUniqueness mem ixfun
  return [Combine pe { patElemAttr = attr} w v]

allocInKernelStm space _ (GroupReduce pes w lam input) = do
  summaries <- mapM lookupArraySummary arrs
  lam' <- allocInReduceLambda lam summaries
  let local_tid = SE.Id (spaceLocalId space) int32
  pes' <- forM (zip pes summaries) $ \(pe, (mem, ixfun)) ->
    case patElemType pe of
      Array bt shape u ->
        let ixfun' = IxFun.applyInd ixfun [local_tid]
        in return pe { patElemAttr = ArrayMem bt shape u mem ixfun' }
      t -> return pe { patElemAttr = Scalar $ elemType t }
  return [GroupReduce pes' w lam' input]
  where arrs = map snd input

allocInKernelStm space _ (GroupStream pes w maxchunk lam acc arrs) = do
  arr_summaries <- mapM lookupArraySummary arrs
  lam' <- allocInGroupStreamLambda maxchunk lam space arr_summaries
  pes' <- forM pes $ \pe ->
    case patElemType pe of
      Array{} ->
        fail "allocInKernelStm: cannot handle streams returning arrays yet."
      t -> return pe { patElemAttr = Scalar $ elemType t }
  return [GroupStream pes' w maxchunk lam' acc arrs]

allocInGroupStreamLambda :: SubExp
                         -> GroupStreamLambda In.Kernels
                         -> KernelSpace
                         -> [(VName, IxFun.IxFun SE.ScalExp)]
                         -> AllocM (GroupStreamLambda ExplicitMemory)
allocInGroupStreamLambda maxchunk lam space input_summaries = do
  let group_size = spaceGroupSize space
      local_tid = SE.Id (spaceLocalId space) int32
      GroupStreamLambda block_size block_offset acc_params arr_params body = lam

  acc_params' <-
    allocInScanParameters group_size local_tid 0 acc_params
  arr_params' <-
    allocInChunkedParameters (SE.Id block_offset int32) $
    zip arr_params input_summaries

  body' <- localScope (HM.insert block_size IndexInfo $
                       HM.insert block_offset IndexInfo $
                       scopeOfLParams $ acc_params' ++ arr_params')  $
           local (HM.insert block_size maxchunk) $
           allocInKernelBody space body
  return $
    GroupStreamLambda block_size block_offset acc_params' arr_params' body'

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

data ExpHint = NoHint
             | IxFunHint (IxFun.IxFun SE.ScalExp)

expHints :: Allocator m => Exp -> m [ExpHint]
expHints (Op (Inner (Kernel _ space rets kbody))) =
  zipWithM hint rets $ kernelBodyResult kbody
  where num_threads = spaceNumThreads space

        spacy AllThreads = Just [num_threads]
        spacy ThreadsInSpace = Just $ map snd $ spaceDimensions space
        spacy _ = Nothing

        innermost space_dims t_dims =
          let r = length t_dims
              dims = space_dims ++ t_dims
              perm = [length space_dims..length space_dims+r-1] ++
                     [0..length space_dims-1]
              perm_inv = rearrangeInverse perm
              dims_perm = rearrangeShape perm dims
              ixfun_base = IxFun.iota $ map SE.intSubExpToScalExp dims_perm
              ixfun_rearranged = IxFun.permute ixfun_base perm_inv
          in IxFunHint ixfun_rearranged

        hint t (ThreadsReturn threads _)
          | r <- arrayRank t,
            r > 0,
            Just space_dims <- spacy threads = do
            t_dims <- mapM dimAllocationSize $ arrayDims t
            return $ innermost space_dims t_dims

        hint t (ConcatReturns Disorder w _ _) = do
          t_dims <- mapM dimAllocationSize $ arrayDims t
          return $ innermost [w] t_dims

        hint _ _ = return NoHint
expHints e =
  return $ replicate (expExtTypeSize e) NoHint
