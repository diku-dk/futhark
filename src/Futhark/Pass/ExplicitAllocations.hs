{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, FlexibleContexts, TupleSections, LambdaCase, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
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

import Prelude hiding (div, mod, quot, rem)

import Futhark.Representation.Kernels
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
import Futhark.Optimise.Simplifier.Simple (SimpleOps (..))
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import Futhark.Pass
import Futhark.Util.IntegralExp

type InInKernel = Futhark.Representation.Kernels.InKernel
type OutInKernel = Futhark.Representation.ExplicitMemory.InKernel

data AllocBinding = SizeComputation VName (PrimExp VName)
                  | Allocation VName SubExp Space
                  | ArrayCopy VName Bindage VName
                    deriving (Eq, Ord, Show)

bindAllocBinding :: (MonadBinder m, Op (Lore m) ~ MemOp inner) =>
                    AllocBinding -> m ()
bindAllocBinding (SizeComputation name pe) =
  letBindNames'_ [name] =<< toExp pe
bindAllocBinding (Allocation name size space) =
  letBindNames'_ [name] $ Op $ Alloc size space
bindAllocBinding (ArrayCopy name bindage src) =
  letBindNames_ [(name,bindage)] $ BasicOp $ Copy src

class (MonadFreshNames m, HasScope lore m, ExplicitMemorish lore) =>
      Allocator lore m where
  addAllocBinding :: AllocBinding -> m ()
  -- | The subexpression giving the number of elements we should
  -- allocate space for.  See 'ChunkMap' comment.
  dimAllocationSize :: SubExp -> m SubExp

  expHints :: Exp lore -> m [ExpHint]
  expHints e = return $ replicate (expExtTypeSize e) NoHint

allocateMemory :: Allocator lore m =>
                  String -> SubExp -> Space -> m VName
allocateMemory desc size space = do
  v <- newVName desc
  addAllocBinding $ Allocation v size space
  return v

computeSize :: Allocator lore m =>
               String -> PrimExp VName -> m SubExp
computeSize desc se = do
  v <- newVName desc
  addAllocBinding $ SizeComputation v se
  return $ Var v

type Allocable fromlore tolore =
  (ExplicitMemorish tolore,
   SameScope fromlore Kernels,
   RetType fromlore ~ RetType Kernels,
   BodyAttr fromlore ~ (),
   BodyAttr tolore ~ (),
   ExpAttr tolore ~ (),
   SizeSubst (Op tolore))

-- | A mapping from chunk names to their maximum size.  XXX FIXME
-- HACK: This is part of a hack to add loop-invariant allocations to
-- reduce kernels, because memory expansion does not use range
-- analysis yet (it should).
type ChunkMap = HM.HashMap VName SubExp

data AllocEnv fromlore tolore  =
  AllocEnv { chunkMap :: ChunkMap
           , allocInOp :: Op fromlore -> AllocM fromlore tolore (Op tolore)
           }

boundDims :: ChunkMap -> AllocEnv fromlore tolore
          -> AllocEnv fromlore tolore
boundDims m env = env { chunkMap = m <> chunkMap env }

boundDim :: VName -> SubExp -> AllocEnv fromlore tolore
         -> AllocEnv fromlore tolore
boundDim name se = boundDims $ HM.singleton name se

-- | Monad for adding allocations to an entire program.
newtype AllocM fromlore tolore a =
  AllocM (BinderT tolore (ReaderT (AllocEnv fromlore tolore) (State VNameSource)) a)
  deriving (Applicative, Functor, Monad,
             MonadFreshNames,
             HasScope tolore,
             LocalScope tolore,
             MonadReader (AllocEnv fromlore tolore))

instance (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
         MonadBinder (AllocM fromlore tolore) where
  type Lore (AllocM fromlore tolore) = tolore

  mkLetM pat e = return $ Let pat () e

  mkLetNamesM names e = do
    pat <- patternWithAllocations names e
    return $ Let pat () e

  mkBodyM bnds res = return $ Body () bnds res

  addBinding binding =
    AllocM $ addBinderBinding binding
  collectBindings (AllocM m) =
    AllocM $ collectBinderBindings m

instance Allocable fromlore OutInKernel =>
         Allocator ExplicitMemory (AllocM fromlore ExplicitMemory) where
  addAllocBinding (SizeComputation name se) =
    letBindNames'_ [name] =<< toExp se
  addAllocBinding (Allocation name size space) =
    letBindNames'_ [name] $ Op $ Alloc size space
  addAllocBinding (ArrayCopy name bindage src) =
    letBindNames_ [(name, bindage)] $ BasicOp $ Copy src

  dimAllocationSize (Var v) =
    fromMaybe (Var v) <$> asks (HM.lookup v . chunkMap)
  dimAllocationSize size =
    return size

  expHints = kernelExpHints

instance Allocable fromlore OutInKernel =>
         Allocator OutInKernel (AllocM fromlore OutInKernel) where
  addAllocBinding (SizeComputation name se) =
    letBindNames'_ [name] =<< toExp se
  addAllocBinding (Allocation name size space) =
    letBindNames'_ [name] $ Op $ Alloc size space
  addAllocBinding (ArrayCopy name bindage src) =
    letBindNames_ [(name, bindage)] $ BasicOp $ Copy src

  dimAllocationSize (Var v) =
    fromMaybe (Var v) <$> asks (HM.lookup v . chunkMap)
  dimAllocationSize size =
    return size

  expHints = inKernelExpHints

runAllocM :: MonadFreshNames m =>
             (Op fromlore -> AllocM fromlore tolore (Op tolore))
          -> AllocM fromlore tolore a -> m a
runAllocM handleOp (AllocM m) =
  fmap fst $ modifyNameSource $ runState $ runReaderT (runBinderT m mempty) env
  where env = AllocEnv mempty handleOp

subAllocM :: (SameScope tolore1 tolore2, ExplicitMemorish tolore2) =>
             (Op fromlore1 -> AllocM fromlore1 tolore1 (Op tolore1))
          -> AllocM fromlore1 tolore1 a
          -> AllocM fromlore2 tolore2 a
subAllocM handleOp (AllocM m) = do
  scope <- castScope <$> askScope
  chunks <- asks chunkMap
  let env = AllocEnv chunks handleOp
  fmap fst $ modifyNameSource $ runState $ runReaderT (runBinderT m scope) env

-- | Monad for adding allocations to a single pattern.
newtype PatAllocM lore a = PatAllocM (RWS
                                      (Scope lore)
                                      [AllocBinding]
                                      VNameSource
                                      a)
                    deriving (Applicative, Functor, Monad,
                              HasScope lore,
                              MonadWriter [AllocBinding],
                              MonadFreshNames)

instance Allocator ExplicitMemory (PatAllocM ExplicitMemory) where
  addAllocBinding = tell . pure
  dimAllocationSize = return

instance Allocator OutInKernel (PatAllocM OutInKernel) where
  addAllocBinding = tell . pure
  dimAllocationSize = return

runPatAllocM :: MonadFreshNames m =>
                PatAllocM lore a -> Scope lore
             -> m (a, [AllocBinding])
runPatAllocM (PatAllocM m) mems =
  modifyNameSource $ frob . runRWS m mems
  where frob (a,s,w) = ((a,w),s)

arraySizeInBytesExp :: Type -> PrimExp VName
arraySizeInBytesExp t =
  product $
  (ValueExp $ IntValue $ Int32Value $ primByteSize $ elemType t) :
  map (primExpFromSubExp int32) (arrayDims t)

arraySizeInBytesExpM :: Allocator lore m => Type -> m (PrimExp VName)
arraySizeInBytesExpM t =
  product .
  ((ValueExp $ IntValue $ Int32Value $ primByteSize $ elemType t):) .
  map (primExpFromSubExp int32) <$>
  mapM dimAllocationSize (arrayDims t)

arraySizeInBytes :: Allocator lore m => Type -> m SubExp
arraySizeInBytes = computeSize "bytes" <=< arraySizeInBytesExpM

allocForArray :: Allocator lore m =>
                 Type -> Space -> m (SubExp, VName)
allocForArray t space = do
  size <- arraySizeInBytes t
  m <- allocateMemory "mem" size space
  return (size, m)

-- | Allocate local-memory array.
allocForLocalArray :: Allocator lore m =>
                      SubExp -> Type -> m (SubExp, VName)
allocForLocalArray workgroup_size t = do
  size <- computeSize "local_bytes" =<<
          (primExpFromSubExp int32 workgroup_size*) <$>
          arraySizeInBytesExpM t
  m <- allocateMemory "local_mem" size $ Space "local"
  return (size, m)

allocsForBinding :: (Allocator lore m, ExpAttr lore ~ ()) =>
                    [Ident] -> [(Ident,Bindage)] -> Exp lore
                 -> m (Binding lore, [AllocBinding])
allocsForBinding sizeidents validents e = do
  rts <- expReturns e
  hints <- expHints e
  (ctxElems, valElems, postbnds) <- allocsForPattern sizeidents validents rts hints
  return (Let (Pattern ctxElems valElems) () e,
          postbnds)

patternWithAllocations :: (Allocator lore m, ExpAttr lore ~ ()) =>
                          [(VName, Bindage)]
                       -> Exp lore
                       -> m (Pattern lore)
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

allocsForPattern :: Allocator lore m =>
                    [Ident] -> [(Ident,Bindage)] -> [ExpReturns] -> [ExpHint]
                 -> m ([PatElem ExplicitMemory],
                       [PatElem ExplicitMemory],
                       [AllocBinding])
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
            BindInPlace _ src slice -> do
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
                              newIdent (baseString (identName ident)<>"_ext_buffer")
                              (identType ident `setArrayDims` sliceDims slice)
                tell ([], [],
                      [ArrayCopy (identName ident) bindage $
                       identName tmp_buffer])
                return $ PatElem (identName tmp_buffer) BindVar $
                  ArrayMem bt (arrayShape $ identType tmp_buffer) u mem ixfun

        ReturnsArray _ extshape _ Nothing
          | Just _ <- knownShape extshape -> do
            summary <- lift $ summaryForBindage (identType ident) bindage hint
            return $ PatElem (identName ident) bindage summary

        ReturnsArray bt _ u (Just ReturnsNewBlock{})
          | BindInPlace _ _ slice <- bindage -> do
              -- The expression returns its own memory, but the pattern
              -- wants to store it somewhere else.  We first let it
              -- store the value where it wants, then we copy it to the
              -- intended destination.  In some cases, the copy may be
              -- optimised away later, but in some cases it may not be
              -- possible (e.g. function calls).
              tmp_buffer <- lift $
                            newIdent (baseString (identName ident)<>"_ext_buffer")
                            (identType ident `setArrayDims` sliceDims slice)
              (memsize,mem,(_,ixfun)) <- lift $ memForBindee tmp_buffer
              tell ([PatElem (identName memsize) BindVar $ Scalar int32],
                    [PatElem (identName mem)     BindVar $ MemMem (Var $ identName memsize) DefaultSpace],
                    [ArrayCopy (identName ident) bindage $
                     identName tmp_buffer])
              return $ PatElem (identName tmp_buffer) BindVar $
                ArrayMem bt (arrayShape $ identType tmp_buffer) u (identName mem) ixfun

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

summaryForBindage :: (ExplicitMemorish lore, Allocator lore m) =>
                     Type -> Bindage -> ExpHint
                  -> m (MemBound NoUniqueness)
summaryForBindage (Prim bt) BindVar _ =
  return $ Scalar bt
summaryForBindage (Mem size space) BindVar _ =
  return $ MemMem size space
summaryForBindage t@(Array bt shape u) BindVar NoHint = do
  (_, m) <- allocForArray t DefaultSpace
  return $ directIndexFunction bt shape u m t
summaryForBindage t BindVar (Hint ixfun space) = do
  let bt = elemType t
  bytes <- computeSize "bytes" $ product $
           fromIntegral (primByteSize (elemType t)::Int32) : IxFun.base ixfun
  m <- allocateMemory "mem" bytes space
  return $ ArrayMem bt (arrayShape t) NoUniqueness m ixfun
summaryForBindage _ (BindInPlace _ src _) _ =
  lookupMemBound src

memForBindee :: (MonadFreshNames m) =>
                Ident
             -> m (Ident,
                   Ident,
                   (Ident, IxFun))
memForBindee ident = do
  size <- newIdent (memname <> "_size") (Prim int32)
  mem <- newIdent memname $ Mem (Var $ identName size) DefaultSpace
  return (size,
          mem,
          (ident, IxFun.iota $ map (primExpFromSubExp int32) $ arrayDims t))
  where  memname = baseString (identName ident) <> "_mem"
         t       = identType ident

directIndexFunction :: PrimType -> Shape -> u -> VName -> Type -> MemBound u
directIndexFunction bt shape u mem t =
  ArrayMem bt shape u mem $ IxFun.iota $ map (primExpFromSubExp int32) $ arrayDims t

allocInFParams :: (Allocable fromlore tolore) =>
                  [FParam fromlore] ->
                  ([FParam tolore] -> AllocM fromlore tolore a)
               -> AllocM fromlore tolore a
allocInFParams params m = do
  (valparams, (memsizeparams, memparams)) <-
    runWriterT $ mapM allocInFParam params
  let params' = memsizeparams <> memparams <> valparams
      summary = scopeOfFParams params'
  localScope summary $ m params'

allocInFParam :: (Allocable fromlore tolore) =>
                 FParam fromlore
              -> WriterT ([FParam tolore], [FParam tolore])
                 (AllocM fromlore tolore) (FParam tolore)
allocInFParam param =
  case paramDeclType param of
    Array bt shape u -> do
      let memname = baseString (paramName param) <> "_mem"
          ixfun = IxFun.iota $ map (primExpFromSubExp int32) $ shapeDims shape
      memsize <- lift $ newVName (memname <> "_size")
      mem <- lift $ newVName memname
      tell ([Param memsize $ Scalar int32],
            [Param mem $ MemMem (Var memsize) DefaultSpace])
      return param { paramAttr =  ArrayMem bt shape u mem ixfun }
    Prim bt ->
      return param { paramAttr = Scalar bt }
    Mem size space ->
      return param { paramAttr = MemMem size space }

allocInMergeParams :: (Allocable fromlore tolore,
                       Allocator tolore (AllocM fromlore tolore)) =>
                      [VName]
                   -> [(FParam fromlore,SubExp)]
                   -> ([FParam tolore]
                       -> [FParam tolore]
                       -> ([SubExp] -> AllocM fromlore tolore ([SubExp], [SubExp]))
                       -> AllocM fromlore tolore a)
                   -> AllocM fromlore tolore a
allocInMergeParams variant merge m = do
  ((valparams, handle_loop_subexps), (memsizeparams, memparams)) <-
    runWriterT $ unzip <$> mapM allocInMergeParam merge
  let mergeparams' = memsizeparams <> memparams <> valparams
      summary = scopeOfFParams mergeparams'

      mk_loop_res ses = do
        (valargs, (memsizeargs, memargs)) <-
          runWriterT $ zipWithM ($) handle_loop_subexps ses
        return (memsizeargs <> memargs, valargs)

  localScope summary $ m (memsizeparams<>memparams) valparams mk_loop_res
  where allocInMergeParam (mergeparam, _) = do
          mergeparam' <- allocInFParam mergeparam
          return (mergeparam', linearFuncallArg $ paramType mergeparam)

ensureDirectArray :: (Allocable fromlore tolore,
                      Allocator tolore (AllocM fromlore tolore)) =>
                     VName -> AllocM fromlore tolore (SubExp, VName, SubExp)
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

allocLinearArray :: (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
                    String -> VName
                 -> AllocM fromlore tolore (SubExp, VName, SubExp)
allocLinearArray s v = do
  t <- lookupType v
  (size, mem) <- allocForArray t DefaultSpace
  v' <- newIdent s t
  let pat = Pattern [] [PatElem (identName v') BindVar $
                        directIndexFunction (elemType t) (arrayShape t)
                        NoUniqueness mem t]
  addBinding $ Let pat () $ BasicOp $ Copy v
  return (size, mem, Var $ identName v')

funcallArgs :: (Allocable fromlore tolore,
                Allocator tolore (AllocM fromlore tolore)) =>
               [(SubExp,Diet)] -> AllocM fromlore tolore [(SubExp,Diet)]
funcallArgs args = do
  (valargs, (memsizeargs, memargs)) <- runWriterT $ forM args $ \(arg,d) -> do
    t <- lift $ subExpType arg
    arg' <- linearFuncallArg t arg
    return (arg', d)
  return $ map (,Observe) (memsizeargs <> memargs) <> valargs

linearFuncallArg :: (Allocable fromlore tolore,
                     Allocator tolore (AllocM fromlore tolore)) =>
                    Type -> SubExp
                 -> WriterT ([SubExp], [SubExp]) (AllocM fromlore tolore) SubExp
linearFuncallArg Array{} (Var v) = do
  (size, mem, arg') <- lift $ ensureDirectArray v
  tell ([size], [Var mem])
  return arg'
linearFuncallArg _ arg =
  return arg

explicitAllocations :: Pass Kernels ExplicitMemory
explicitAllocations = simplePass
                      "explicit allocations"
                      "Transform program to explicit memory representation" $
                      intraproceduralTransformation allocInFun

memoryInRetType :: RetType Kernels -> RetType ExplicitMemory
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

allocInFun :: MonadFreshNames m => FunDef Kernels -> m (FunDef ExplicitMemory)
allocInFun (FunDef entry fname rettype params fbody) =
  runAllocM handleOp $ allocInFParams params $ \params' -> do
    fbody' <- insertBindingsM $ allocInBody fbody
    return $ FunDef entry fname (memoryInRetType rettype) params' fbody'
    where handleOp GroupSize =
            return $ Inner GroupSize
          handleOp NumGroups =
            return $ Inner NumGroups
          handleOp (ScanKernel cs w size lam foldlam nes arrs) = do
            lam' <- allocInScanLambda lam (length nes) $ kernelWorkgroupSize size
            foldlam' <- allocInScanLambda foldlam (length nes) $ kernelWorkgroupSize size
            return $ Inner $ ScanKernel cs w size lam' foldlam' nes arrs
          handleOp (Kernel cs space ts kbody) = subAllocM handleKernelExp $
            Inner . Kernel cs space ts <$>
            localScope (scopeOfKernelSpace space)
            (allocInKernelBody kbody)

          handleKernelExp (SplitArray o w i num_is elems_per_thread arrs) =
            return $ Inner $ SplitArray o w i num_is elems_per_thread arrs
          handleKernelExp (SplitSpace o w i num_is elems_per_thread) =
            return $ Inner $ SplitSpace o w i num_is elems_per_thread
          handleKernelExp (Combine cspace ts active body) =
            Inner . Combine cspace ts active <$> allocInBodyNoDirect body
          handleKernelExp (GroupReduce w lam input) = do
            summaries <- mapM lookupArraySummary arrs
            lam' <- allocInReduceLambda lam summaries
            return $ Inner $ GroupReduce w lam' input
            where arrs = map snd input
          handleKernelExp (GroupStream w maxchunk lam accs arrs) = do
            acc_summaries <- mapM accSummary accs
            arr_summaries <- mapM lookupArraySummary arrs
            lam' <- allocInGroupStreamLambda maxchunk lam acc_summaries arr_summaries
            return $ Inner $ GroupStream w maxchunk lam' accs arrs
            where accSummary (Constant v) = return $ Scalar $ primValueType v
                  accSummary (Var v) = lookupMemBound v

allocInBodyNoDirect :: (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
                       Body fromlore -> AllocM fromlore tolore (Body tolore)
allocInBodyNoDirect (Body _ bnds res) =
  allocInBindings bnds $ \bnds' ->
    return $ Body () bnds' res

allocInBody :: (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
               Body fromlore -> AllocM fromlore tolore (Body tolore)
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

allocInBindings :: (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
                   [Binding fromlore] -> ([Binding tolore] -> AllocM fromlore tolore a)
                -> AllocM fromlore tolore a
allocInBindings origbnds m = allocInBindings' origbnds []
  where allocInBindings' [] bnds' =
          m bnds'
        allocInBindings' (x:xs) bnds' = do
          allocbnds <- allocInBinding' x
          let summaries = scopeOf allocbnds
          localScope summaries $
            local (boundDims $ mconcat $ map sizeSubst allocbnds) $
            allocInBindings' xs (bnds'++allocbnds)
        allocInBinding' bnd = do
          ((),bnds') <- collectBindings $ allocInBinding bnd
          return bnds'

allocInBinding :: (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
                  Binding fromlore -> AllocM fromlore tolore ()
allocInBinding (Let (Pattern sizeElems valElems) _ e) = do
  e' <- allocInExp e
  let sizeidents = map patElemIdent sizeElems
      validents = [ (Ident name t, bindage) | PatElem name bindage t <- valElems ]
  (bnd, bnds) <- allocsForBinding sizeidents validents e'
  addBinding bnd
  mapM_ addAllocBinding bnds

allocInExp :: (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
              Exp fromlore -> AllocM fromlore tolore (Exp tolore)
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
allocInExp (Apply fname args rettype) = do
  args' <- funcallArgs args
  return $ Apply fname args' (memoryInRetType rettype)
allocInExp e = mapExpM alloc e
  where alloc =
          identityMapper { mapOnBody = allocInBody
                         , mapOnRetType = return . memoryInRetType
                         , mapOnFParam = fail "Unhandled FParam in ExplicitAllocations"
                         , mapOnOp = \op -> do handle <- asks allocInOp
                                               handle op
                         }

allocInScanLambda :: Lambda InInKernel
                  -> Int
                  -> SubExp
                  -> AllocM Kernels ExplicitMemory (Lambda OutInKernel)
allocInScanLambda lam num_accs workgroup_size = do
  let (i, other_index_param, actual_params) =
        partitionChunkedKernelLambdaParameters $ lambdaParams lam
      (acc_params, arr_params) =
        splitAt num_accs actual_params
      this_index = LeafExp i int32 `rem`
                   primExpFromSubExp int32 workgroup_size
      other_index = LeafExp (paramName other_index_param) int32
  acc_params' <-
    allocInScanParameters workgroup_size this_index 0 acc_params
  arr_params' <-
    allocInScanParameters workgroup_size this_index other_index arr_params

  subAllocM noOp $
    allocInLambda (Param i (Scalar int32) :
                   other_index_param { paramAttr = Scalar int32 } :
                   acc_params' ++ arr_params')
    (lambdaBody lam) (lambdaReturnType lam)

  where noOp = fail "Cannot handle kernel expressions inside scan kernels."

allocInScanParameters :: SubExp
                      -> PrimExp VName
                      -> PrimExp VName
                      -> [LParam InInKernel]
                      -> AllocM Kernels ExplicitMemory [LParam OutInKernel]
allocInScanParameters workgroup_size my_id offset = mapM allocInScanParameter
  where allocInScanParameter p =
          case paramType p of
            t@(Array bt shape u) -> do
              (_, shared_mem) <- allocForLocalArray workgroup_size t
              let ixfun_base = IxFun.iota $ map (primExpFromSubExp int32) $
                               workgroup_size : arrayDims t
                  ixfun = IxFun.slice ixfun_base $
                          fullSliceNum (IxFun.shape ixfun_base) [DimFix $ my_id + offset]
              return p { paramAttr = ArrayMem bt shape u shared_mem ixfun }
            Prim bt ->
              return p { paramAttr = Scalar bt }
            Mem size space ->
              return p { paramAttr = MemMem size space }

allocInReduceLambda :: Lambda InInKernel
                    -> [(VName, IxFun)]
                    -> AllocM InInKernel OutInKernel (Lambda OutInKernel)
allocInReduceLambda lam input_summaries = do
  let (i, other_offset_param, actual_params) =
        partitionChunkedKernelLambdaParameters $ lambdaParams lam
      (acc_params, arr_params) =
        splitAt (length input_summaries) actual_params
      this_index = LeafExp i int32
      other_offset = LeafExp (paramName other_offset_param) int32
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

allocInReduceParameters :: PrimExp VName
                        -> PrimExp VName
                        -> [(LParam InInKernel, (VName, IxFun))]
                        -> AllocM InInKernel OutInKernel [LParam ExplicitMemory]
allocInReduceParameters my_id offset = mapM allocInReduceParameter
  where allocInReduceParameter (p, (mem, ixfun)) =
          case paramType p of
            (Array bt shape u) ->
              let ixfun' = IxFun.slice ixfun $
                           fullSliceNum (IxFun.shape ixfun) [DimFix $ my_id + offset]
              in return p { paramAttr = ArrayMem bt shape u mem ixfun' }
            Prim bt ->
              return p { paramAttr = Scalar bt }
            Mem size space ->
              return p { paramAttr = MemMem size space }

allocInChunkedParameters :: PrimExp VName
                        -> [(LParam InInKernel, (VName, IxFun))]
                        -> AllocM InInKernel OutInKernel [LParam OutInKernel]
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

allocInLambda :: [LParam OutInKernel] -> Body InInKernel -> [Type]
              -> AllocM InInKernel OutInKernel (Lambda OutInKernel)
allocInLambda params body rettype = do
  body' <- localScope (scopeOfLParams params) $
           allocInBindings (bodyBindings body) $ \bnds' ->
           return $ Body () bnds' $ bodyResult body
  return $ Lambda params body' rettype

allocInKernelBody :: KernelBody InInKernel
                  -> AllocM InInKernel OutInKernel (KernelBody OutInKernel)
allocInKernelBody (KernelBody () stms res) =
  allocInBindings stms $ \stms' ->
    return $ KernelBody () stms' res

class SizeSubst op where
  opSizeSubst :: PatternT attr -> op -> ChunkMap

instance SizeSubst (Kernel lore) where
  opSizeSubst _ _ = mempty

instance SizeSubst op => SizeSubst (MemOp op) where
  opSizeSubst pat (Inner op) = opSizeSubst pat op
  opSizeSubst _ _ = mempty

instance SizeSubst (KernelExp lore) where
  opSizeSubst (Pattern [size] _) (SplitArray _ _ _ _ elems_per_thread _) =
    HM.singleton (patElemName size) elems_per_thread
  opSizeSubst (Pattern _ [size]) (SplitSpace _ _ _ _ elems_per_thread) =
    HM.singleton (patElemName size) elems_per_thread
  opSizeSubst _ _ = mempty

sizeSubst :: SizeSubst (Op lore) => Binding lore -> ChunkMap
sizeSubst (Let pat _ (Op op)) = opSizeSubst pat op
sizeSubst _ = mempty

allocInGroupStreamLambda :: SubExp
                         -> GroupStreamLambda InInKernel
                         -> [MemBound NoUniqueness]
                         -> [(VName, IxFun)]
                         -> AllocM InInKernel OutInKernel (GroupStreamLambda OutInKernel)
allocInGroupStreamLambda maxchunk lam acc_summaries arr_summaries = do
  let GroupStreamLambda block_size block_offset acc_params arr_params body = lam

  acc_params' <-
    allocInAccParameters acc_params acc_summaries
  arr_params' <-
    allocInChunkedParameters (LeafExp block_offset int32) $
    zip arr_params arr_summaries

  body' <- localScope (HM.insert block_size IndexInfo $
                       HM.insert block_offset IndexInfo $
                       scopeOfLParams $ acc_params' ++ arr_params')  $
           local (boundDim block_size maxchunk) $
           allocInBodyNoDirect body
  return $
    GroupStreamLambda block_size block_offset acc_params' arr_params' body'

allocInAccParameters :: [LParam InInKernel]
                     -> [MemBound NoUniqueness]
                     -> AllocM InInKernel OutInKernel [LParam OutInKernel]
allocInAccParameters = zipWithM allocInAccParameter
  where allocInAccParameter p attr = return p { paramAttr = attr }

simplifiable :: (Engine.MonadEngine m,
                 Engine.InnerLore m ~ lore,
                 ExpAttr lore ~ (),
                 BodyAttr lore ~ (),
                 Op lore ~ MemOp inner,
                 Allocator lore (PatAllocM lore)) =>
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

bindPatternWithAllocations :: (MonadBinder m,
                               ExpAttr lore ~ (),
                               Op (Lore m) ~ MemOp inner,
                               Allocator lore (PatAllocM lore)) =>
                              Scope lore -> [(VName, Bindage)] -> Exp lore
                           -> m (Pattern lore)
bindPatternWithAllocations types names e = do
  (pat,prebnds) <- runPatAllocM (patternWithAllocations names e) types
  mapM_ bindAllocBinding prebnds
  return pat

data ExpHint = NoHint
             | Hint IxFun Space

kernelExpHints :: (Allocator lore m, Op lore ~ MemOp (Kernel somelore)) =>
                  Exp lore -> m [ExpHint]
kernelExpHints (Op (Inner (Kernel _ space rets kbody))) =
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
              ixfun_base = IxFun.iota $ map (primExpFromSubExp int32) dims_perm
              ixfun_rearranged = IxFun.permute ixfun_base perm_inv
          in ixfun_rearranged

        hint t (ThreadsReturn threads _)
          | r <- arrayRank t,
            r > 0,
            Just space_dims <- spacy threads = do
            t_dims <- mapM dimAllocationSize $ arrayDims t
            return $ Hint (innermost space_dims t_dims) DefaultSpace

        hint t (ConcatReturns Disorder w _ _) = do
          t_dims <- mapM dimAllocationSize $ arrayDims t
          return $ Hint (innermost [w] t_dims) DefaultSpace

        hint Prim{} (ConcatReturns InOrder w elems_per_thread _) = do
          let ixfun_base = IxFun.iota $ map (primExpFromSubExp int32) [num_threads,elems_per_thread]
              ixfun_tr = IxFun.permute ixfun_base [1,0]
              ixfun = IxFun.reshape ixfun_tr $ map (DimNew . primExpFromSubExp int32) [w]
          return $ Hint ixfun DefaultSpace

        hint _ _ = return NoHint
kernelExpHints e =
  return $ replicate (expExtTypeSize e) NoHint

inKernelExpHints :: (Allocator lore m, Op lore ~ MemOp (KernelExp somelore)) =>
                    Exp lore -> m [ExpHint]
inKernelExpHints (Op (Inner (Combine cspace ts _ _))) =
  forM ts $ \t -> do
    alloc_dims <- mapM dimAllocationSize $ dims ++ arrayDims t
    let ixfun = IxFun.iota $ map (primExpFromSubExp int32) alloc_dims
    return $ Hint ixfun $ Space "local"
  where dims = map snd cspace

inKernelExpHints e =
  return $ replicate (expExtTypeSize e) NoHint
