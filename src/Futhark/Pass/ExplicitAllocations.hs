{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, FlexibleContexts, TupleSections, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futhark.Pass.ExplicitAllocations
       ( explicitAllocations
       , explicitAllocationsInStms
       , simplifiable

       , arraySizeInBytesExp
       )
where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.RWS.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Control.Monad.Fail as Fail
import Data.Maybe

import Futhark.Representation.Kernels
import Futhark.Optimise.Simplify.Lore
  (mkWiseBody, mkWiseLetStm, removeExpWisdom, removeScopeWisdom)
import Futhark.MonadFreshNames
import Futhark.Representation.ExplicitMemory
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.Tools
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.Optimise.Simplify.Engine (SimpleOps (..))
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Pass
import Futhark.Util (splitFromEnd, takeLast)

data AllocStm = SizeComputation VName (PrimExp VName)
              | Allocation VName SubExp Space
              | ArrayCopy VName VName
                    deriving (Eq, Ord, Show)

bindAllocStm :: (MonadBinder m, Op (Lore m) ~ MemOp inner) =>
                AllocStm -> m ()
bindAllocStm (SizeComputation name pe) =
  letBindNames_ [name] =<< toExp (coerceIntPrimExp Int64 pe)
bindAllocStm (Allocation name size space) =
  letBindNames_ [name] $ Op $ Alloc size space
bindAllocStm (ArrayCopy name src) =
  letBindNames_ [name] $ BasicOp $ Copy src

defaultExpHints :: (Monad m, Attributes lore) => Exp lore -> m [ExpHint]
defaultExpHints e = return $ replicate (expExtTypeSize e) NoHint

class (MonadFreshNames m, HasScope lore m, ExplicitMemorish lore) =>
      Allocator lore m where
  addAllocStm :: AllocStm -> m ()
  askDefaultSpace :: m Space

  default addAllocStm :: (Allocable fromlore lore,
                          Op lore ~ MemOp inner,
                          m ~ AllocM fromlore lore)
                      => AllocStm -> m ()
  addAllocStm (SizeComputation name se) =
    letBindNames_ [name] =<< toExp (coerceIntPrimExp Int64 se)
  addAllocStm (Allocation name size space) =
    letBindNames_ [name] $ Op $ Alloc size space
  addAllocStm (ArrayCopy name src) =
    letBindNames_ [name] $ BasicOp $ Copy src

  -- | The subexpression giving the number of elements we should
  -- allocate space for.  See 'ChunkMap' comment.
  dimAllocationSize :: SubExp -> m SubExp

  default dimAllocationSize :: m ~ AllocM fromlore lore
                               => SubExp -> m SubExp
  dimAllocationSize (Var v) =
    -- It is important to recurse here, as the substitution may itself
    -- be a chunk size.
    maybe (return $ Var v) dimAllocationSize =<< asks (M.lookup v . chunkMap)
  dimAllocationSize size =
    return size

  expHints :: Exp lore -> m [ExpHint]
  expHints = defaultExpHints

allocateMemory :: Allocator lore m =>
                  String -> SubExp -> Space -> m VName
allocateMemory desc size space = do
  v <- newVName desc
  addAllocStm $ Allocation v size space
  return v

computeSize :: Allocator lore m =>
               String -> PrimExp VName -> m SubExp
computeSize desc se = do
  v <- newVName desc
  addAllocStm $ SizeComputation v se
  return $ Var v

type Allocable fromlore tolore =
  (PrettyLore fromlore, PrettyLore tolore,
   ExplicitMemorish tolore,
   SameScope fromlore Kernels,
   RetType fromlore ~ RetType Kernels,
   BranchType fromlore ~ BranchType Kernels,
   BodyAttr fromlore ~ (),
   BodyAttr tolore ~ (),
   ExpAttr tolore ~ (),
   SizeSubst (Op tolore),
   BinderOps tolore)

-- | A mapping from chunk names to their maximum size.  XXX FIXME
-- HACK: This is part of a hack to add loop-invariant allocations to
-- reduce kernels, because memory expansion does not use range
-- analysis yet (it should).
type ChunkMap = M.Map VName SubExp

data AllocEnv fromlore tolore  =
  AllocEnv { chunkMap :: ChunkMap
           , aggressiveReuse :: Bool
             -- ^ Aggressively try to reuse memory in do-loops -
             -- should be True inside kernels, False outside.
           , allocSpace :: Space
             -- ^ When allocating memory, put it in this memory space.
             -- This is primarily used to ensure that group-wide
             -- statements store their results in local memory.
           , allocInOp :: Op fromlore -> AllocM fromlore tolore (Op tolore)
           , envExpHints :: Exp tolore -> AllocM fromlore tolore [ExpHint]
           }

boundDims :: ChunkMap -> AllocEnv fromlore tolore
          -> AllocEnv fromlore tolore
boundDims m env = env { chunkMap = m <> chunkMap env }

-- | Monad for adding allocations to an entire program.
newtype AllocM fromlore tolore a =
  AllocM (BinderT tolore (ReaderT (AllocEnv fromlore tolore) (State VNameSource)) a)
  deriving (Applicative, Functor, Monad,
             MonadFreshNames,
             HasScope tolore,
             LocalScope tolore,
             MonadReader (AllocEnv fromlore tolore))

instance Fail.MonadFail (AllocM fromlore tolore) where
  fail = error . ("AllocM.fail: "++)

instance (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
         MonadBinder (AllocM fromlore tolore) where
  type Lore (AllocM fromlore tolore) = tolore

  mkExpAttrM _ _ = return ()

  mkLetNamesM names e = do
    pat <- patternWithAllocations names e
    return $ Let pat (defAux ()) e

  mkBodyM bnds res = return $ Body () bnds res

  addStms binding = AllocM $ addBinderStms binding
  collectStms (AllocM m) = AllocM $ collectBinderStms m
  certifying cs (AllocM m) = AllocM $ certifyingBinder cs m

instance Allocable fromlore ExplicitMemory =>
         Allocator ExplicitMemory (AllocM fromlore ExplicitMemory) where
  expHints e = do
    f <- asks envExpHints
    f e
  askDefaultSpace = asks allocSpace

runAllocM :: MonadFreshNames m =>
             (Op fromlore -> AllocM fromlore tolore (Op tolore))
          -> (Exp tolore -> AllocM fromlore tolore [ExpHint])
          -> AllocM fromlore tolore a -> m a
runAllocM handleOp hints (AllocM m) =
  fmap fst $ modifyNameSource $ runState $ runReaderT (runBinderT m mempty) env
  where env = AllocEnv mempty False DefaultSpace handleOp hints

-- | Monad for adding allocations to a single pattern.
newtype PatAllocM lore a = PatAllocM (RWS
                                      (Scope lore)
                                      [AllocStm]
                                      VNameSource
                                      a)
                    deriving (Applicative, Functor, Monad,
                              HasScope lore,
                              MonadWriter [AllocStm],
                              MonadFreshNames)

instance Allocator ExplicitMemory (PatAllocM ExplicitMemory) where
  addAllocStm = tell . pure
  dimAllocationSize = return
  askDefaultSpace = return DefaultSpace

runPatAllocM :: MonadFreshNames m =>
                PatAllocM lore a -> Scope lore
             -> m (a, [AllocStm])
runPatAllocM (PatAllocM m) mems =
  modifyNameSource $ frob . runRWS m mems
  where frob (a,s,w) = ((a,w),s)

arraySizeInBytesExp :: Type -> PrimExp VName
arraySizeInBytesExp t =
  product
    [ toInt64 $ product $ map (primExpFromSubExp int32) (arrayDims t)
    , ValueExp $ IntValue $ Int64Value $ primByteSize $ elemType t ]
  where toInt64 = ConvOpExp $ SExt Int32 Int64

arraySizeInBytesExpM :: Allocator lore m => Type -> m (PrimExp VName)
arraySizeInBytesExpM t = do
  dims <- mapM dimAllocationSize (arrayDims t)
  let dim_prod_i32 = product $ map (toInt64 . primExpFromSubExp int32) dims
  let elm_size_i64 = ValueExp $ IntValue $ Int64Value $ primByteSize $ elemType t
  return $ product [ dim_prod_i32, elm_size_i64 ]
  where toInt64 = ConvOpExp $ SExt Int32 Int64

arraySizeInBytes :: Allocator lore m => Type -> m SubExp
arraySizeInBytes = computeSize "bytes" <=< arraySizeInBytesExpM

allocForArray :: Allocator lore m =>
                 Type -> Space -> m VName
allocForArray t space = do
  size <- arraySizeInBytes t
  allocateMemory "mem" size space

allocsForStm :: (Allocator lore m, ExpAttr lore ~ ()) =>
                [Ident] -> [Ident] -> Exp lore
             -> m (Stm lore, [AllocStm])
allocsForStm sizeidents validents e = do
  rts <- expReturns e
  hints <- expHints e
  (ctxElems, valElems, postbnds) <- allocsForPattern sizeidents validents rts hints
  return (Let (Pattern ctxElems valElems) (defAux ()) e,
          postbnds)

patternWithAllocations :: (Allocator lore m, ExpAttr lore ~ ()) =>
                          [VName]
                       -> Exp lore
                       -> m (Pattern lore)
patternWithAllocations names e = do
  (ts',sizes) <- instantiateShapes' =<< expExtType e
  let identForBindage name t =
        pure $ Ident name t
  vals <- sequence [ identForBindage name t | (name, t) <- zip names ts' ]
  (Let pat _ _, extrabnds) <- allocsForStm sizes vals e
  case extrabnds of
    [] -> return pat
    _  -> error $ "Cannot make allocations for pattern of " ++ pretty e

allocsForPattern :: Allocator lore m =>
                    [Ident] -> [Ident] -> [ExpReturns] -> [ExpHint]
                 -> m ([PatElem ExplicitMemory],
                       [PatElem ExplicitMemory],
                       [AllocStm])
allocsForPattern sizeidents validents rts hints = do
  let sizes' = [ PatElem size $ MemPrim int32 | size <- map identName sizeidents ]
  (vals, (mems, postbnds)) <-
    runWriterT $ forM (zip3 validents rts hints) $ \(ident, rt, hint) -> do
      let shape = arrayShape $ identType ident
      case rt of
        MemPrim _ -> do
          summary <- lift $ summaryForBindage (identType ident) hint
          return $ PatElem (identName ident) summary

        MemMem space ->
          return $ PatElem (identName ident) $
          MemMem space

        MemArray bt _ u (Just (ReturnsInBlock mem ixfun)) ->
          PatElem (identName ident) . MemArray bt shape u .
          ArrayIn mem <$> instantiateIxFun ixfun

        MemArray _ extshape _ Nothing
          | Just _ <- knownShape extshape -> do
            summary <- lift $ summaryForBindage (identType ident) hint
            return $ PatElem (identName ident) summary

        MemArray bt _ u ret -> do
          space <- case ret of
                     Just (ReturnsNewBlock mem_space _ _) -> return mem_space
                     _                                    -> lift askDefaultSpace
          (mem,(ident',ixfun)) <- lift $ memForBindee ident space
          tell ([PatElem (identName mem) $ MemMem space],
                [])
          return $ PatElem (identName ident') $ MemArray bt shape u $
            ArrayIn (identName mem) ixfun

  return (sizes' <> mems,
          vals,
          postbnds)
  where knownShape = mapM known . shapeDims
        known (Free v) = Just v
        known Ext{} = Nothing

instantiateIxFun :: Monad m => ExtIxFun -> m IxFun
instantiateIxFun = traverse $ traverse inst
  where inst Ext{} = error "instantiateIxFun: not yet"
        inst (Free x) = return x

summaryForBindage :: Allocator lore m =>
                     Type -> ExpHint
                  -> m (MemBound NoUniqueness)
summaryForBindage (Prim bt) _ =
  return $ MemPrim bt
summaryForBindage (Mem space) _ =
  return $ MemMem space
summaryForBindage t@(Array bt shape u) NoHint = do
  m <- allocForArray t =<< askDefaultSpace
  return $ directIndexFunction bt shape u m t
summaryForBindage t (Hint ixfun space) = do
  let bt = elemType t
  bytes <- computeSize "bytes" $
           product [ConvOpExp (SExt Int32 Int64) (product (IxFun.base ixfun)),
                    fromIntegral (primByteSize (elemType t)::Int64)]
  m <- allocateMemory "mem" bytes space
  return $ MemArray bt (arrayShape t) NoUniqueness $ ArrayIn m ixfun

memForBindee :: (MonadFreshNames m) =>
                Ident -> Space
             -> m (Ident,
                   (Ident, IxFun))
memForBindee ident space = do
  mem <- newIdent memname (Mem space)
  return (mem,
          (ident, IxFun.iota $ map (primExpFromSubExp int32) $ arrayDims t))
  where  memname = baseString (identName ident) <> "_mem"
         t       = identType ident

directIndexFunction :: PrimType -> Shape -> u -> VName -> Type -> MemBound u
directIndexFunction bt shape u mem t =
  MemArray bt shape u $ ArrayIn mem $
  IxFun.iota $ map (primExpFromSubExp int32) $ arrayDims t

allocInFParams :: (Allocable fromlore tolore) =>
                  [(FParam fromlore, Space)] ->
                  ([FParam tolore] -> AllocM fromlore tolore a)
               -> AllocM fromlore tolore a
allocInFParams params m = do
  (valparams, memparams) <-
    runWriterT $ mapM (uncurry allocInFParam) params
  let params' = memparams <> valparams
      summary = scopeOfFParams params'
  localScope summary $ m params'

allocInFParam :: (Allocable fromlore tolore) =>
                 FParam fromlore
              -> Space
              -> WriterT [FParam tolore]
                 (AllocM fromlore tolore) (FParam tolore)
allocInFParam param pspace =
  case paramDeclType param of
    Array bt shape u -> do
      let memname = baseString (paramName param) <> "_mem"
          ixfun = IxFun.iota $ map (primExpFromSubExp int32) $ shapeDims shape
      mem <- lift $ newVName memname
      tell [Param mem $ MemMem pspace]
      return param { paramAttr =  MemArray bt shape u $ ArrayIn mem ixfun }
    Prim bt ->
      return param { paramAttr = MemPrim bt }
    Mem space ->
      return param { paramAttr = MemMem space }

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
  ((valparams, handle_loop_subexps), mem_params) <-
    runWriterT $ unzip <$> mapM allocInMergeParam merge
  let mergeparams' = mem_params <> valparams
      summary = scopeOfFParams mergeparams'

      mk_loop_res ses = do
        (valargs, memargs) <-
          runWriterT $ zipWithM ($) handle_loop_subexps ses
        return (memargs, valargs)

  localScope summary $ m mem_params valparams mk_loop_res
  where allocInMergeParam (mergeparam, Var v)
          | Array bt shape u <- paramDeclType mergeparam = do
              (mem, ixfun) <- lift $ lookupArraySummary v
              Mem space <- lift $ lookupType mem
              reuse <- asks aggressiveReuse
              if space /= Space "local" &&
                 reuse &&
                 u == Unique &&
                 loopInvariantShape mergeparam
                then return (mergeparam { paramAttr = MemArray bt shape Unique $ ArrayIn mem ixfun },
                             lift . ensureArrayIn (paramType mergeparam) mem ixfun)
                else do def_space <- asks allocSpace
                        doDefault mergeparam def_space

        allocInMergeParam (mergeparam, _) = doDefault mergeparam =<< lift askDefaultSpace

        doDefault mergeparam space = do
          mergeparam' <- allocInFParam mergeparam space
          return (mergeparam', linearFuncallArg (paramType mergeparam) space)

        variant_names = variant ++ map (paramName . fst) merge
        loopInvariantShape =
          not . any (`elem` variant_names) . subExpVars . arrayDims . paramType

ensureArrayIn :: (Allocable fromlore tolore,
                  Allocator tolore (AllocM fromlore tolore)) =>
                 Type -> VName -> IxFun -> SubExp
              -> AllocM fromlore tolore SubExp
ensureArrayIn _ _ _ (Constant v) =
  fail $ "ensureArrayIn: " ++ pretty v ++ " cannot be an array."
ensureArrayIn t mem ixfun (Var v) = do
  (src_mem, src_ixfun) <- lookupArraySummary v
  if src_mem == mem && src_ixfun == ixfun
    then return $ Var v
    else do copy <- newIdent (baseString v ++ "_ensure_copy") t
            let summary = MemArray (elemType t) (arrayShape t) NoUniqueness $
                          ArrayIn mem ixfun
                pat = Pattern [] [PatElem (identName copy) summary]
            letBind_ pat $ BasicOp $ Copy v
            return $ Var $ identName copy

ensureDirectArray :: (Allocable fromlore tolore,
                      Allocator tolore (AllocM fromlore tolore)) =>
                     Maybe Space -> VName -> AllocM fromlore tolore (VName, SubExp)
ensureDirectArray space_ok v = do
  (mem, ixfun) <- lookupArraySummary v
  Mem mem_space <- lookupType mem
  default_space <- askDefaultSpace
  if IxFun.isDirect ixfun && maybe True (==mem_space) space_ok
    then return (mem, Var v)
    else needCopy (fromMaybe default_space space_ok)
  where needCopy space =
          -- We need to do a new allocation, copy 'v', and make a new
          -- binding for the size of the memory block.
          allocLinearArray space (baseString v) v

allocLinearArray :: (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
                    Space -> String -> VName
                 -> AllocM fromlore tolore (VName, SubExp)
allocLinearArray space s v = do
  t <- lookupType v
  mem <- allocForArray t space
  v' <- newIdent (s ++ "_linear") t
  let pat = Pattern [] [PatElem (identName v') $
                        directIndexFunction (elemType t) (arrayShape t)
                        NoUniqueness mem t]
  addStm $ Let pat (defAux ()) $ BasicOp $ Copy v
  return (mem, Var $ identName v')

funcallArgs :: (Allocable fromlore tolore,
                Allocator tolore (AllocM fromlore tolore)) =>
               [(SubExp,Diet)] -> AllocM fromlore tolore [(SubExp,Diet)]
funcallArgs args = do
  (valargs, mem_and_size_args) <- runWriterT $ forM args $ \(arg,d) -> do
    t <- lift $ subExpType arg
    space <- lift askDefaultSpace
    arg' <- linearFuncallArg t space arg
    return (arg', d)
  return $ map (,Observe) mem_and_size_args <> valargs

linearFuncallArg :: (Allocable fromlore tolore,
                     Allocator tolore (AllocM fromlore tolore)) =>
                    Type -> Space -> SubExp
                 -> WriterT [SubExp] (AllocM fromlore tolore) SubExp
linearFuncallArg Array{} space (Var v) = do
  (mem, arg') <- lift $ ensureDirectArray (Just space) v
  tell [Var mem]
  return arg'
linearFuncallArg _ _ arg =
  return arg

explicitAllocations :: Pass Kernels ExplicitMemory
explicitAllocations =
  Pass "explicit allocations" "Transform program to explicit memory representation" $
  intraproceduralTransformation allocInFun

explicitAllocationsInStms :: (MonadFreshNames m, HasScope ExplicitMemory m) =>
                             Stms Kernels -> m (Stms ExplicitMemory)
explicitAllocationsInStms stms = do
  scope <- askScope
  runAllocM handleHostOp kernelExpHints $ localScope scope $ allocInStms stms return

memoryInRetType :: [RetType Kernels] -> [RetType ExplicitMemory]
memoryInRetType ts = evalState (mapM addAttr ts) $ startOfFreeIDRange ts
  where addAttr (Prim t) = return $ MemPrim t
        addAttr Mem{} = error "memoryInRetType: too much memory"
        addAttr (Array bt shape u) = do
          i <- get <* modify (+1)
          return $ MemArray bt shape u $ ReturnsNewBlock DefaultSpace i $
            IxFun.iota $ map convert $ shapeDims shape

        convert (Ext i) = LeafExp (Ext i) int32
        convert (Free v) = Free <$> primExpFromSubExp int32 v

startOfFreeIDRange :: [TypeBase ExtShape u] -> Int
startOfFreeIDRange = S.size . shapeContext

allocInFun :: MonadFreshNames m => FunDef Kernels -> m (FunDef ExplicitMemory)
allocInFun (FunDef entry fname rettype params fbody) =
  runAllocM handleHostOp kernelExpHints $
  allocInFParams (zip params $ repeat DefaultSpace) $ \params' -> do
    fbody' <- insertStmsM $ allocInFunBody
              (map (const $ Just DefaultSpace) rettype) fbody
    return $ FunDef entry fname (memoryInRetType rettype) params' fbody'

handleHostOp :: HostOp Kernels (SOAC Kernels)
             -> AllocM Kernels ExplicitMemory (MemOp (HostOp ExplicitMemory ()))
handleHostOp (SizeOp op) =
  return $ Inner $ SizeOp op
handleHostOp (OtherOp op) =
  fail $ "Cannot allocate memory in SOAC: " ++ pretty op
handleHostOp (SegOp op) =
  Inner . SegOp <$> handleSegOp op

handleSegOp :: SegOp Kernels
            -> AllocM Kernels ExplicitMemory (SegOp ExplicitMemory)
handleSegOp op = allocAtLevel (segLevel op) $ mapSegOpM mapper op
  where scope = scopeOfSegSpace $ segSpace op
        mapper = identitySegOpMapper
             { mapOnSegOpBody = localScope scope . allocInKernelBody (segLevel op)
             , mapOnSegOpLambda = allocInBinOpLambda (segLevel op) $ segSpace op
             }

allocAtLevel :: SegLevel -> AllocM fromlore tlore a -> AllocM fromlore tlore a
allocAtLevel lvl = local $ \env -> env { allocSpace = space
                                       , aggressiveReuse = True
                                       }
  where space = case lvl of SegThread{} -> DefaultSpace
                            SegThreadScalar{} -> DefaultSpace
                            SegGroup{} -> Space "local"

bodyReturnMemCtx :: (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
                    SubExp -> AllocM fromlore tolore [SubExp]
bodyReturnMemCtx Constant{} =
  return []
bodyReturnMemCtx (Var v) = do
  info <- lookupMemInfo v
  case info of
    MemPrim{} -> return []
    MemMem{} -> return [] -- should not happen
    MemArray _ _ _ (ArrayIn mem _) -> return [Var mem]

allocInFunBody :: (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
                  [Maybe Space] -> Body fromlore -> AllocM fromlore tolore (Body tolore)
allocInFunBody space_oks (Body _ bnds res) =
  allocInStms bnds $ \bnds' -> do
    (res'', allocs) <- collectStms $ do
      res' <- zipWithM ensureDirect space_oks' res
      let (ctx_res, val_res) = splitFromEnd num_vals res'
      mem_ctx_res <- concat <$> mapM bodyReturnMemCtx val_res
      return $ ctx_res <> mem_ctx_res <> val_res
    return $ Body () (bnds'<>allocs) res''
  where num_vals = length space_oks
        space_oks' = replicate (length res - num_vals) Nothing ++ space_oks
        ensureDirect _ se@Constant{} = return se
        ensureDirect space_ok (Var v) = do
          bt <- primType <$> lookupType v
          if bt
            then return $ Var v
            else do (_, v') <- ensureDirectArray space_ok v
                    return v'

allocInStms :: (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
               Stms fromlore -> (Stms tolore -> AllocM fromlore tolore a)
            -> AllocM fromlore tolore a
allocInStms origbnds m = allocInStms' (stmsToList origbnds) mempty
  where allocInStms' [] bnds' =
          m bnds'
        allocInStms' (x:xs) bnds' = do
          allocbnds <- allocInStm' x
          let summaries = scopeOf allocbnds
          localScope summaries $
            local (boundDims $ mconcat $ map sizeSubst $ stmsToList allocbnds) $
            allocInStms' xs (bnds'<>allocbnds)
        allocInStm' bnd = do
          ((),bnds') <- collectStms $ certifying (stmCerts bnd) $ allocInStm bnd
          return bnds'

allocInStm :: (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
              Stm fromlore -> AllocM fromlore tolore ()
allocInStm (Let (Pattern sizeElems valElems) _ e) = do
  e' <- allocInExp e
  let sizeidents = map patElemIdent sizeElems
      validents = map patElemIdent valElems
  (bnd, bnds) <- allocsForStm sizeidents validents e'
  addStm bnd
  mapM_ addAllocStm bnds

allocInExp :: (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
              Exp fromlore -> AllocM fromlore tolore (Exp tolore)
allocInExp (DoLoop ctx val form (Body () bodybnds bodyres)) =
  allocInMergeParams mempty ctx $ \_ ctxparams' _ ->
  allocInMergeParams (map paramName ctxparams') val $
  \new_ctx_params valparams' mk_loop_val -> do
  form' <- allocInLoopForm form
  localScope (scopeOf form') $ do
    (valinit_ctx, valinit') <- mk_loop_val valinit
    body' <- insertStmsM $ allocInStms bodybnds $ \bodybnds' -> do
      ((val_ses,valres'),val_retbnds) <- collectStms $ mk_loop_val valres
      return $ Body () (bodybnds'<>val_retbnds) (ctxres++val_ses++valres')
    return $
      DoLoop
      (zip (ctxparams'++new_ctx_params) (ctxinit++valinit_ctx))
      (zip valparams' valinit')
      form' body'
  where (_ctxparams, ctxinit) = unzip ctx
        (_valparams, valinit) = unzip val
        (ctxres, valres) = splitAt (length ctx) bodyres
allocInExp (Apply fname args rettype loc) = do
  args' <- funcallArgs args
  return $ Apply fname args' (memoryInRetType rettype) loc
allocInExp (If cond tbranch fbranch (IfAttr rets ifsort)) = do
  tbranch' <- allocInFunBody (map (const Nothing) rets) tbranch
  space_oks <- mkSpaceOks (length rets) tbranch'
  fbranch' <- allocInFunBody space_oks fbranch
  let rets' = createBodyReturns rets space_oks
      res_then = bodyResult tbranch'
      res_else = bodyResult fbranch'
      size_ext = length res_then - length rets'
      (ind_ses0, r_then_else) =
        foldl (\(acc_ise,acc_ext) (r_then, r_else, i) ->
                if r_then == r_else then ((i,r_then):acc_ise, acc_ext)
                else (acc_ise, (r_then, r_else):acc_ext)
              ) ([],[]) $ reverse $ zip3 res_then res_else [0..size_ext-1]
      (r_then_ext, r_else_ext) = unzip r_then_else
      ind_ses = zipWith (\(i,se) k -> (i-k,se)) ind_ses0 [0..length ind_ses0 - 1]
      rets'' = foldl (\acc (i,se) -> fixExt i se acc) rets' ind_ses
      tbranch'' = tbranch' { bodyResult = r_then_ext ++ drop size_ext res_then }
      fbranch'' = fbranch' { bodyResult = r_else_ext ++ drop size_ext res_else }
  return $ If cond tbranch'' fbranch'' $ IfAttr rets'' ifsort
allocInExp e = mapExpM alloc e
  where alloc =
          identityMapper { mapOnBody = error "Unhandled Body in ExplicitAllocations"
                         , mapOnRetType = error "Unhandled RetType in ExplicitAllocations"
                         , mapOnBranchType = error "Unhandled BranchType in ExplicitAllocations"
                         , mapOnFParam = error "Unhandled FParam in ExplicitAllocations"
                         , mapOnLParam = error "Unhandled LParam in ExplicitAllocations"
                         , mapOnOp = \op -> do handle <- asks allocInOp
                                               handle op
                         }

mkSpaceOks :: (ExplicitMemorish tolore, LocalScope tolore m) =>
              Int -> Body tolore -> m [Maybe Space]
mkSpaceOks num_vals (Body _ stms res) =
  inScopeOf stms $
  mapM mkSpaceOK $ takeLast num_vals res
  where mkSpaceOK (Var v) = do
          v_info <- lookupMemInfo v
          case v_info of MemArray _ _ _ (ArrayIn mem _) -> do
                           mem_info <- lookupMemInfo mem
                           case mem_info of MemMem space -> return $ Just space
                                            _ -> return Nothing
                         _ -> return Nothing
        mkSpaceOK _ = return Nothing

createBodyReturns :: [ExtType] -> [Maybe Space] -> [BodyReturns]
createBodyReturns ts spaces =
  evalState (zipWithM inspect ts spaces) $ S.size $ shapeContext ts
  where inspect (Array pt shape u) space = do
          i <- get <* modify (+1)
          let space' = fromMaybe DefaultSpace space
          return $ MemArray pt shape u $ ReturnsNewBlock space' i $
            IxFun.iota $ map convert $ shapeDims shape
        inspect (Prim pt) _ =
          return $ MemPrim pt
        inspect (Mem space) _ =
          return $ MemMem space

        convert (Ext i) = LeafExp (Ext i) int32
        convert (Free v) = Free <$> primExpFromSubExp int32 v

allocInLoopForm :: (Allocable fromlore tolore,
                    Allocator tolore (AllocM fromlore tolore)) =>
                   LoopForm fromlore -> AllocM fromlore tolore (LoopForm tolore)
allocInLoopForm (WhileLoop v) = return $ WhileLoop v
allocInLoopForm (ForLoop i it n loopvars) =
  ForLoop i it n <$> mapM allocInLoopVar loopvars
  where allocInLoopVar (p,a) = do
          (mem, ixfun) <- lookupArraySummary a
          case paramType p of
            Array bt shape u ->
              let ixfun' = IxFun.slice ixfun $
                           fullSliceNum (IxFun.shape ixfun) [DimFix $ LeafExp i int32]
              in return (p { paramAttr = MemArray bt shape u $ ArrayIn mem ixfun' }, a)
            Prim bt ->
              return (p { paramAttr = MemPrim bt }, a)
            Mem space ->
              return (p { paramAttr = MemMem space }, a)

allocInBinOpLambda :: SegLevel -> SegSpace -> Lambda Kernels
                   -> AllocM Kernels ExplicitMemory (Lambda ExplicitMemory)
allocInBinOpLambda lvl (SegSpace flat _) lam = do
  num_threads <- letSubExp "num_threads" $ BasicOp $ BinOp (Mul Int32)
                 (unCount (segNumGroups lvl)) (unCount (segGroupSize lvl))
  let (acc_params, arr_params) =
        splitAt (length (lambdaParams lam) `div` 2) $ lambdaParams lam
      index_x = LeafExp flat int32
      index_y = index_x + primExpFromSubExp int32 num_threads
  (acc_params', arr_params') <-
    allocInBinOpParams num_threads index_x index_y acc_params arr_params

  local (\env -> env { envExpHints = inThreadExpHints }) $
    allocInLambda (acc_params' ++ arr_params')
    (lambdaBody lam) (lambdaReturnType lam)

allocInBinOpParams :: SubExp
                   -> PrimExp VName -> PrimExp VName
                   -> [LParam Kernels]
                   -> [LParam Kernels]
                   -> AllocM Kernels ExplicitMemory ([LParam ExplicitMemory], [LParam ExplicitMemory])
allocInBinOpParams num_threads my_id other_id xs ys = unzip <$> zipWithM alloc xs ys
  where alloc x y =
          case paramType x of
            Array bt shape u -> do
              twice_num_threads <-
                letSubExp "twice_num_threads" $
                BasicOp $ BinOp (Mul Int32) num_threads $ intConst Int32 2
              let t = paramType x `arrayOfRow` twice_num_threads
              mem <- allocForArray t DefaultSpace
              -- XXX: this iota ixfun is a bit inefficient; leading to uncoalesced access.
              let ixfun_base = IxFun.iota $
                               map (primExpFromSubExp int32) (arrayDims t)
                  ixfun_x = IxFun.slice ixfun_base $
                            fullSliceNum (IxFun.shape ixfun_base) [DimFix my_id]
                  ixfun_y = IxFun.slice ixfun_base $
                            fullSliceNum (IxFun.shape ixfun_base) [DimFix other_id]
              return (x { paramAttr = MemArray bt shape u $ ArrayIn mem ixfun_x },
                      y { paramAttr = MemArray bt shape u $ ArrayIn mem ixfun_y })
            Prim bt ->
              return (x { paramAttr = MemPrim bt },
                      y { paramAttr = MemPrim bt })
            Mem space ->
              return (x { paramAttr = MemMem space },
                      y { paramAttr = MemMem space })

allocInLambda :: [LParam ExplicitMemory] -> Body Kernels -> [Type]
              -> AllocM Kernels ExplicitMemory (Lambda ExplicitMemory)
allocInLambda params body rettype = do
  body' <- localScope (scopeOfLParams params) $
           allocInStms (bodyStms body) $ \bnds' ->
           return $ Body () bnds' $ bodyResult body
  return $ Lambda params body' rettype

allocInKernelBody :: SegLevel -> KernelBody Kernels
                  -> AllocM Kernels ExplicitMemory (KernelBody ExplicitMemory)
allocInKernelBody lvl (KernelBody () stms res) =
  local f $ allocInStms stms $ \stms' -> return $ KernelBody () stms' res
  where f = case lvl of SegThread{} -> inThread
                        SegThreadScalar{} -> inThread
                        SegGroup{} -> inGroup
        inThread env = env { envExpHints = inThreadExpHints }
        inGroup env = env { envExpHints = inGroupExpHints }

class SizeSubst op where
  opSizeSubst :: PatternT attr -> op -> ChunkMap

instance SizeSubst (HostOp lore op) where
  opSizeSubst (Pattern _ [size]) (SizeOp (SplitSpace _ _ _ elems_per_thread)) =
    M.singleton (patElemName size) elems_per_thread
  opSizeSubst _ _ = mempty

instance SizeSubst op => SizeSubst (MemOp op) where
  opSizeSubst pat (Inner op) = opSizeSubst pat op
  opSizeSubst _ _ = mempty

sizeSubst :: SizeSubst (Op lore) => Stm lore -> ChunkMap
sizeSubst (Let pat _ (Op op)) = opSizeSubst pat op
sizeSubst _ = mempty

mkLetNamesB' :: (Op (Lore m) ~ MemOp inner,
                 MonadBinder m, ExpAttr (Lore m) ~ (),
                 Allocator (Lore m) (PatAllocM (Lore m))) =>
                ExpAttr (Lore m) -> [VName] -> Exp (Lore m) -> m (Stm (Lore m))
mkLetNamesB' attr names e = do
  scope <- askScope
  pat <- bindPatternWithAllocations scope names e
  return $ Let pat (defAux attr) e

mkLetNamesB'' :: (Op (Lore m) ~ MemOp inner, ExpAttr lore ~ (),
                   HasScope (Engine.Wise lore) m, Allocator lore (PatAllocM lore),
                   MonadBinder m, Engine.CanBeWise (Op lore)) =>
                 [VName] -> Exp (Engine.Wise lore)
              -> m (Stm (Engine.Wise lore))
mkLetNamesB'' names e = do
  scope <- Engine.removeScopeWisdom <$> askScope
  (pat, prestms) <- runPatAllocM (patternWithAllocations names $ Engine.removeExpWisdom e) scope
  mapM_ bindAllocStm prestms
  let pat' = Engine.addWisdomToPattern pat e
      attr = Engine.mkWiseExpAttr pat' () e
  return $ Let pat' (defAux attr) e

instance BinderOps ExplicitMemory where
  mkExpAttrB _ _ = return ()
  mkBodyB stms res = return $ Body () stms res
  mkLetNamesB = mkLetNamesB' ()

instance BinderOps (Engine.Wise ExplicitMemory) where
  mkExpAttrB pat e = return $ Engine.mkWiseExpAttr pat () e
  mkBodyB stms res = return $ Engine.mkWiseBody () stms res
  mkLetNamesB = mkLetNamesB''

simplifiable :: (Engine.SimplifiableLore lore,
                 ExpAttr lore ~ (),
                 BodyAttr lore ~ (),
                 Op lore ~ MemOp inner,
                 Allocator lore (PatAllocM lore)) =>
                (inner -> Engine.SimpleM lore (Engine.OpWithWisdom inner, Stms (Engine.Wise lore)))
             -> SimpleOps lore
simplifiable simplifyInnerOp =
  SimpleOps mkExpAttrS' mkBodyS' mkLetNamesS' protectOp simplifyOp
  where mkExpAttrS' _ pat e =
          return $ Engine.mkWiseExpAttr pat () e

        mkBodyS' _ bnds res = return $ mkWiseBody () bnds res

        mkLetNamesS' vtable names e = do
          (pat', stms) <- runBinder $ bindPatternWithAllocations env names $
                          removeExpWisdom e
          return (mkWiseLetStm pat' (defAux ()) e, stms)
          where env = removeScopeWisdom $ ST.toScope vtable

        protectOp taken pat (Alloc size space) = Just $ do
          tbody <- resultBodyM [size]
          fbody <- resultBodyM [intConst Int64 0]
          size' <- letSubExp "hoisted_alloc_size" $
                   If taken tbody fbody $ IfAttr [MemPrim int64] IfFallback
          letBind_ pat $ Op $ Alloc size' space
        protectOp _ _ _ = Nothing

        simplifyOp (Alloc size space) =
          (,) <$> (Alloc <$> Engine.simplify size <*> pure space) <*> pure mempty
        simplifyOp (Inner k) = do (k', hoisted) <- simplifyInnerOp k
                                  return (Inner k', hoisted)

bindPatternWithAllocations :: (MonadBinder m,
                               ExpAttr lore ~ (),
                               Op (Lore m) ~ MemOp inner,
                               Allocator lore (PatAllocM lore)) =>
                              Scope lore -> [VName] -> Exp lore
                           -> m (Pattern lore)
bindPatternWithAllocations types names e = do
  (pat,prebnds) <- runPatAllocM (patternWithAllocations names e) types
  mapM_ bindAllocStm prebnds
  return pat

data ExpHint = NoHint
             | Hint IxFun Space

kernelExpHints :: Allocator ExplicitMemory m => Exp ExplicitMemory -> m [ExpHint]
kernelExpHints (BasicOp (Manifest perm v)) = do
  dims <- arrayDims <$> lookupType v
  let perm_inv = rearrangeInverse perm
      dims' = rearrangeShape perm dims
      ixfun = IxFun.permute (IxFun.iota $ map (primExpFromSubExp int32) dims')
              perm_inv
  return [Hint ixfun DefaultSpace]

kernelExpHints (Op (Inner (SegOp (SegMap lvl@SegThread{} space ts body)))) =
  zipWithM (mapResultHint lvl space) ts $ kernelBodyResult body

kernelExpHints (Op (Inner (SegOp (SegRed lvl@SegThread{} space reds ts body)))) =
  (map (const NoHint) red_res <>) <$> zipWithM (mapResultHint lvl space) (drop num_reds ts) map_res
  where num_reds = segRedResults reds
        (red_res, map_res) = splitAt num_reds $ kernelBodyResult body

kernelExpHints e =
  return $ replicate (expExtTypeSize e) NoHint

mapResultHint :: Allocator lore m =>
                 SegLevel -> SegSpace -> Type -> KernelResult -> m ExpHint
mapResultHint lvl space = hint
  where num_threads = primExpFromSubExp int32 (unCount $ segNumGroups lvl) *
                      primExpFromSubExp int32 (unCount $ segGroupSize lvl)

        -- Heuristic: do not rearrange for returned arrays that are
        -- sufficiently small.
        coalesceReturnOfShape _ [] = False
        coalesceReturnOfShape bs [Constant (IntValue (Int32Value d))] = bs * d > 4
        coalesceReturnOfShape _ _ = True

        hint t (Returns _)
          | coalesceReturnOfShape (primByteSize (elemType t)) $ arrayDims t = do
              let space_dims = segSpaceDims space
              t_dims <- mapM dimAllocationSize $ arrayDims t
              return $ Hint (innermost space_dims t_dims) DefaultSpace

        hint t (ConcatReturns SplitStrided{} w _ _) = do
          t_dims <- mapM dimAllocationSize $ arrayDims t
          return $ Hint (innermost [w] t_dims) DefaultSpace

        hint Prim{} (ConcatReturns SplitContiguous w elems_per_thread _) = do
          let ixfun_base = IxFun.iota [num_threads, primExpFromSubExp int32 elems_per_thread]
              ixfun_tr = IxFun.permute ixfun_base [1,0]
              ixfun = IxFun.reshape ixfun_tr $ map (DimNew . primExpFromSubExp int32) [w]
          return $ Hint ixfun DefaultSpace

        hint _ _ = return NoHint

innermost :: [SubExp] -> [SubExp] -> IxFun
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

inGroupExpHints :: Allocator ExplicitMemory m => Exp ExplicitMemory -> m [ExpHint]
inGroupExpHints (Op (Inner (SegOp (SegMap SegThreadScalar{} space ts _)))) = return $ do
  t <- ts
  case t of
    Prim pt ->
      return $ Hint (IxFun.iota $ map (primExpFromSubExp int32) $
                     segSpaceDims space ++ arrayDims t) $ Space $ scalarMemory pt
    _ ->
      return NoHint
inGroupExpHints e = return $ replicate (expExtTypeSize e) NoHint

inThreadExpHints :: Allocator ExplicitMemory m => Exp ExplicitMemory -> m [ExpHint]
inThreadExpHints e =
  mapM maybePrivate =<< expExtType e
  where maybePrivate t
          | arrayRank t > 0,
            Just t' <- hasStaticShape t,
            all semiStatic $ arrayDims t' = do
              alloc_dims <- mapM dimAllocationSize $ arrayDims t'
              let ixfun = IxFun.iota $ map (primExpFromSubExp int32) alloc_dims
              return $ Hint ixfun $ Space "private"
          | otherwise =
              return NoHint

        semiStatic Constant{} = True
        semiStatic _ = False
