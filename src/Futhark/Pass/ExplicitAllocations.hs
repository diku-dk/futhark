{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A generic transformation for adding memory allocations to a
-- Futhark program.  Specialised by specific representations in
-- submodules.
module Futhark.Pass.ExplicitAllocations
  ( explicitAllocationsGeneric,
    explicitAllocationsInStmsGeneric,
    ExpHint (..),
    defaultExpHints,
    Allocable,
    Allocator (..),
    AllocM,
    AllocEnv (..),
    SizeSubst (..),
    allocInStms,
    allocForArray,
    simplifiable,
    arraySizeInBytesExp,
    mkLetNamesB',
    mkLetNamesB'',

    -- * Module re-exports

    --
    -- These are highly likely to be needed by any downstream
    -- users.
    module Control.Monad.Reader,
    module Futhark.MonadFreshNames,
    module Futhark.Pass,
    module Futhark.Tools,
  )
where

import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.List (foldl', partition, sort, zip4)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.IR.Mem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplify.Engine (SimpleOps (..))
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Optimise.Simplify.Lore (mkWiseBody)
import Futhark.Pass
import Futhark.Tools
import Futhark.Util (splitFromEnd, takeLast)

data AllocStm
  = SizeComputation VName (PrimExp VName)
  | Allocation VName SubExp Space
  | ArrayCopy VName VName
  deriving (Eq, Ord, Show)

bindAllocStm ::
  (MonadBinder m, Op (Lore m) ~ MemOp inner) =>
  AllocStm ->
  m ()
bindAllocStm (SizeComputation name pe) =
  letBindNames [name] =<< toExp (coerceIntPrimExp Int64 pe)
bindAllocStm (Allocation name size space) =
  letBindNames [name] $ Op $ Alloc size space
bindAllocStm (ArrayCopy name src) =
  letBindNames [name] $ BasicOp $ Copy src

class
  (MonadFreshNames m, HasScope lore m, Mem lore) =>
  Allocator lore m
  where
  addAllocStm :: AllocStm -> m ()
  askDefaultSpace :: m Space

  default addAllocStm ::
    ( Allocable fromlore lore,
      m ~ AllocM fromlore lore
    ) =>
    AllocStm ->
    m ()
  addAllocStm (SizeComputation name se) =
    letBindNames [name] =<< toExp (coerceIntPrimExp Int64 se)
  addAllocStm (Allocation name size space) =
    letBindNames [name] $ Op $ allocOp size space
  addAllocStm (ArrayCopy name src) =
    letBindNames [name] $ BasicOp $ Copy src

  -- | The subexpression giving the number of elements we should
  -- allocate space for.  See 'ChunkMap' comment.
  dimAllocationSize :: SubExp -> m SubExp
  default dimAllocationSize ::
    m ~ AllocM fromlore lore =>
    SubExp ->
    m SubExp
  dimAllocationSize (Var v) =
    -- It is important to recurse here, as the substitution may itself
    -- be a chunk size.
    maybe (return $ Var v) dimAllocationSize =<< asks (M.lookup v . chunkMap)
  dimAllocationSize size =
    return size

  -- | Get those names that are known to be constants at run-time.
  askConsts :: m (S.Set VName)

  expHints :: Exp lore -> m [ExpHint]
  expHints = defaultExpHints

allocateMemory ::
  Allocator lore m =>
  String ->
  SubExp ->
  Space ->
  m VName
allocateMemory desc size space = do
  v <- newVName desc
  addAllocStm $ Allocation v size space
  return v

computeSize ::
  Allocator lore m =>
  String ->
  PrimExp VName ->
  m SubExp
computeSize desc se = do
  v <- newVName desc
  addAllocStm $ SizeComputation v se
  return $ Var v

type Allocable fromlore tolore =
  ( PrettyLore fromlore,
    PrettyLore tolore,
    Mem tolore,
    FParamInfo fromlore ~ DeclType,
    LParamInfo fromlore ~ Type,
    BranchType fromlore ~ ExtType,
    RetType fromlore ~ DeclExtType,
    BodyDec fromlore ~ (),
    BodyDec tolore ~ (),
    ExpDec tolore ~ (),
    SizeSubst (Op tolore),
    BinderOps tolore
  )

-- | A mapping from chunk names to their maximum size.  XXX FIXME
-- HACK: This is part of a hack to add loop-invariant allocations to
-- reduce kernels, because memory expansion does not use range
-- analysis yet (it should).
type ChunkMap = M.Map VName SubExp

data AllocEnv fromlore tolore = AllocEnv
  { chunkMap :: ChunkMap,
    -- | Aggressively try to reuse memory in do-loops -
    -- should be True inside kernels, False outside.
    aggressiveReuse :: Bool,
    -- | When allocating memory, put it in this memory space.
    -- This is primarily used to ensure that group-wide
    -- statements store their results in local memory.
    allocSpace :: Space,
    -- | The set of names that are known to be constants at
    -- kernel compile time.
    envConsts :: S.Set VName,
    allocInOp :: Op fromlore -> AllocM fromlore tolore (Op tolore),
    envExpHints :: Exp tolore -> AllocM fromlore tolore [ExpHint]
  }

-- | Monad for adding allocations to an entire program.
newtype AllocM fromlore tolore a
  = AllocM (BinderT tolore (ReaderT (AllocEnv fromlore tolore) (State VNameSource)) a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadFreshNames,
      HasScope tolore,
      LocalScope tolore,
      MonadReader (AllocEnv fromlore tolore)
    )

instance
  (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
  MonadBinder (AllocM fromlore tolore)
  where
  type Lore (AllocM fromlore tolore) = tolore

  mkExpDecM _ _ = return ()

  mkLetNamesM names e = do
    pat <- patternWithAllocations names e
    return $ Let pat (defAux ()) e

  mkBodyM bnds res = return $ Body () bnds res

  addStms = AllocM . addStms
  collectStms (AllocM m) = AllocM $ collectStms m

instance
  (Allocable fromlore tolore) =>
  Allocator tolore (AllocM fromlore tolore)
  where
  expHints e = do
    f <- asks envExpHints
    f e
  askDefaultSpace = asks allocSpace

  askConsts = asks envConsts

runAllocM ::
  MonadFreshNames m =>
  (Op fromlore -> AllocM fromlore tolore (Op tolore)) ->
  (Exp tolore -> AllocM fromlore tolore [ExpHint]) ->
  AllocM fromlore tolore a ->
  m a
runAllocM handleOp hints (AllocM m) =
  fmap fst $ modifyNameSource $ runState $ runReaderT (runBinderT m mempty) env
  where
    env =
      AllocEnv
        { chunkMap = mempty,
          aggressiveReuse = False,
          allocSpace = DefaultSpace,
          envConsts = mempty,
          allocInOp = handleOp,
          envExpHints = hints
        }

-- | Monad for adding allocations to a single pattern.
newtype PatAllocM lore a
  = PatAllocM
      ( RWS
          (Scope lore)
          [AllocStm]
          VNameSource
          a
      )
  deriving
    ( Applicative,
      Functor,
      Monad,
      HasScope lore,
      MonadWriter [AllocStm],
      MonadFreshNames
    )

instance Mem lore => Allocator lore (PatAllocM lore) where
  addAllocStm = tell . pure
  dimAllocationSize = return
  askDefaultSpace = return DefaultSpace
  askConsts = pure mempty

runPatAllocM ::
  MonadFreshNames m =>
  PatAllocM lore a ->
  Scope lore ->
  m (a, [AllocStm])
runPatAllocM (PatAllocM m) mems =
  modifyNameSource $ frob . runRWS m mems
  where
    frob (a, s, w) = ((a, w), s)

elemSize :: Num a => Type -> a
elemSize = primByteSize . elemType

arraySizeInBytesExp :: Type -> PrimExp VName
arraySizeInBytesExp t =
  untyped $ foldl' (*) (elemSize t) $ map (sExt64 . pe32) (arrayDims t)

arraySizeInBytesExpM :: Allocator lore m => Type -> m (PrimExp VName)
arraySizeInBytesExpM t = do
  dims <- mapM dimAllocationSize (arrayDims t)
  let dim_prod_i32 = product $ map (sExt64 . pe32) dims
      elm_size_i64 = primByteSize $ elemType t
  return $ untyped $ dim_prod_i32 * elm_size_i64

arraySizeInBytes :: Allocator lore m => Type -> m SubExp
arraySizeInBytes = computeSize "bytes" <=< arraySizeInBytesExpM

-- | Allocate memory for a value of the given type.
allocForArray ::
  Allocator lore m =>
  Type ->
  Space ->
  m VName
allocForArray t space = do
  size <- arraySizeInBytes t
  allocateMemory "mem" size space

allocsForStm ::
  (Allocator lore m, ExpDec lore ~ ()) =>
  [Ident] ->
  [Ident] ->
  Exp lore ->
  m (Stm lore)
allocsForStm sizeidents validents e = do
  rts <- expReturns e
  hints <- expHints e
  (ctxElems, valElems) <- allocsForPattern sizeidents validents rts hints
  return $ Let (Pattern ctxElems valElems) (defAux ()) e

patternWithAllocations ::
  (Allocator lore m, ExpDec lore ~ ()) =>
  [VName] ->
  Exp lore ->
  m (Pattern lore)
patternWithAllocations names e = do
  (ts', sizes) <- instantiateShapes' =<< expExtType e
  let identForBindage name t =
        pure $ Ident name t
  vals <- sequence [identForBindage name t | (name, t) <- zip names ts']
  stmPattern <$> allocsForStm sizes vals e

allocsForPattern ::
  Allocator lore m =>
  [Ident] ->
  [Ident] ->
  [ExpReturns] ->
  [ExpHint] ->
  m
    ( [PatElem lore],
      [PatElem lore]
    )
allocsForPattern sizeidents validents rts hints = do
  let sizes' = [PatElem size $ MemPrim int32 | size <- map identName sizeidents]
  (vals, (exts, mems)) <-
    runWriterT $
      forM (zip3 validents rts hints) $ \(ident, rt, hint) -> do
        let shape = arrayShape $ identType ident
        case rt of
          MemPrim _ -> do
            summary <- lift $ summaryForBindage (identType ident) hint
            return $ PatElem (identName ident) summary
          MemMem space ->
            return $
              PatElem (identName ident) $
                MemMem space
          MemArray bt _ u (Just (ReturnsInBlock mem extixfun)) -> do
            (patels, ixfn) <- instantiateExtIxFun ident extixfun
            tell (patels, [])

            return $
              PatElem (identName ident) $
                MemArray bt shape u $
                  ArrayIn mem ixfn
          MemArray _ extshape _ Nothing
            | Just _ <- knownShape extshape -> do
              summary <- lift $ summaryForBindage (identType ident) hint
              return $ PatElem (identName ident) summary
          MemArray bt _ u (Just (ReturnsNewBlock space _ extixfn)) -> do
            -- treat existential index function first
            (patels, ixfn) <- instantiateExtIxFun ident extixfn
            tell (patels, [])

            memid <- lift $ mkMemIdent ident space
            tell ([], [PatElem (identName memid) $ MemMem space])
            return $
              PatElem (identName ident) $
                MemArray bt shape u $
                  ArrayIn (identName memid) ixfn
          _ -> error "Impossible case reached in allocsForPattern!"

  return
    ( sizes' <> exts <> mems,
      vals
    )
  where
    knownShape = mapM known . shapeDims
    known (Free v) = Just v
    known Ext {} = Nothing

    mkMemIdent :: (MonadFreshNames m) => Ident -> Space -> m Ident
    mkMemIdent ident space = do
      let memname = baseString (identName ident) <> "_mem"
      newIdent memname $ Mem space

    instantiateExtIxFun ::
      MonadFreshNames m =>
      Ident ->
      ExtIxFun ->
      m ([PatElemT (MemInfo d u ret)], IxFun)
    instantiateExtIxFun idd ext_ixfn = do
      let isAndPtps =
            S.toList $
              foldMap onlyExts $
                foldMap (leafExpTypes . untyped) ext_ixfn

      -- Find the existentials that reuse the sizeidents, and
      -- those that need new pattern elements.  Assumes that the
      -- Exts form a contiguous interval of integers.
      let (size_exts, new_exts) =
            span ((< length sizeidents) . fst) $ sort isAndPtps
      (new_substs, patels) <-
        fmap unzip $
          forM new_exts $ \(i, t) -> do
            v <- newVName $ baseString (identName idd) <> "_ixfn"
            return
              ( (Ext i, LeafExp (Free v) t),
                PatElem v $ MemPrim t
              )
      let size_substs =
            zipWith
              ( \(i, t) ident ->
                  (Ext i, LeafExp (Free (identName ident)) t)
              )
              size_exts
              sizeidents
          substs = M.fromList $ new_substs <> size_substs
      ixfn <- instantiateIxFun $ IxFun.substituteInIxFun (fmap isInt32 substs) ext_ixfn

      return (patels, ixfn)

onlyExts :: (Ext a, PrimType) -> S.Set (Int, PrimType)
onlyExts (Free _, _) = S.empty
onlyExts (Ext i, t) = S.singleton (i, t)

instantiateIxFun :: Monad m => ExtIxFun -> m IxFun
instantiateIxFun = traverse $ traverse inst
  where
    inst Ext {} = error "instantiateIxFun: not yet"
    inst (Free x) = return x

summaryForBindage ::
  Allocator lore m =>
  Type ->
  ExpHint ->
  m (MemBound NoUniqueness)
summaryForBindage (Prim bt) _ =
  return $ MemPrim bt
summaryForBindage (Mem space) _ =
  return $ MemMem space
summaryForBindage t@(Array bt shape u) NoHint = do
  m <- allocForArray t =<< askDefaultSpace
  return $ directIxFun bt shape u m t
summaryForBindage t (Hint ixfun space) = do
  let bt = elemType t
  bytes <-
    computeSize "bytes" $
      untyped $
        product
          [ product $ map sExt64 $ IxFun.base ixfun,
            fromIntegral (primByteSize (elemType t) :: Int64)
          ]
  m <- allocateMemory "mem" bytes space
  return $ MemArray bt (arrayShape t) NoUniqueness $ ArrayIn m ixfun

lookupMemSpace :: (HasScope lore m, Monad m) => VName -> m Space
lookupMemSpace v = do
  t <- lookupType v
  case t of
    Mem space -> return space
    _ -> error $ "lookupMemSpace: " ++ pretty v ++ " is not a memory block."

directIxFun :: PrimType -> Shape -> u -> VName -> Type -> MemBound u
directIxFun bt shape u mem t =
  let ixf = IxFun.iota $ map pe32 $ arrayDims t
   in MemArray bt shape u $ ArrayIn mem ixf

allocInFParams ::
  (Allocable fromlore tolore) =>
  [(FParam fromlore, Space)] ->
  ([FParam tolore] -> AllocM fromlore tolore a) ->
  AllocM fromlore tolore a
allocInFParams params m = do
  (valparams, (ctxparams, memparams)) <-
    runWriterT $ mapM (uncurry allocInFParam) params
  let params' = ctxparams <> memparams <> valparams
      summary = scopeOfFParams params'
  localScope summary $ m params'

allocInFParam ::
  (Allocable fromlore tolore) =>
  FParam fromlore ->
  Space ->
  WriterT
    ([FParam tolore], [FParam tolore])
    (AllocM fromlore tolore)
    (FParam tolore)
allocInFParam param pspace =
  case paramDeclType param of
    Array bt shape u -> do
      let memname = baseString (paramName param) <> "_mem"
          ixfun = IxFun.iota $ map pe32 $ shapeDims shape
      mem <- lift $ newVName memname
      tell ([], [Param mem $ MemMem pspace])
      return param {paramDec = MemArray bt shape u $ ArrayIn mem ixfun}
    Prim bt ->
      return param {paramDec = MemPrim bt}
    Mem space ->
      return param {paramDec = MemMem space}

allocInMergeParams ::
  ( Allocable fromlore tolore,
    Allocator tolore (AllocM fromlore tolore)
  ) =>
  [(FParam fromlore, SubExp)] ->
  ( [FParam tolore] ->
    [FParam tolore] ->
    ([SubExp] -> AllocM fromlore tolore ([SubExp], [SubExp])) ->
    AllocM fromlore tolore a
  ) ->
  AllocM fromlore tolore a
allocInMergeParams merge m = do
  ((valparams, handle_loop_subexps), (ctx_params, mem_params)) <-
    runWriterT $ unzip <$> mapM allocInMergeParam merge
  let mergeparams' = ctx_params <> mem_params <> valparams
      summary = scopeOfFParams mergeparams'

      mk_loop_res ses = do
        (valargs, (ctxargs, memargs)) <-
          runWriterT $ zipWithM ($) handle_loop_subexps ses
        return (ctxargs <> memargs, valargs)

  localScope summary $ m (ctx_params <> mem_params) valparams mk_loop_res
  where
    allocInMergeParam ::
      (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
      (Param DeclType, SubExp) ->
      WriterT
        ([FParam tolore], [FParam tolore])
        (AllocM fromlore tolore)
        (FParam tolore, SubExp -> WriterT ([SubExp], [SubExp]) (AllocM fromlore tolore) SubExp)
    allocInMergeParam (mergeparam, Var v)
      | Array bt shape u <- paramDeclType mergeparam = do
        (mem', _) <- lift $ lookupArraySummary v
        mem_space <- lift $ lookupMemSpace mem'

        (_, ext_ixfun, substs, _) <- lift $ existentializeArray mem_space v

        (ctx_params, param_ixfun_substs) <-
          unzip
            <$> mapM
              ( \_ -> do
                  vname <- lift $ newVName "ctx_param_ext"
                  return
                    ( Param vname $ MemPrim int32,
                      fmap Free $ pe32 $ Var vname
                    )
              )
              substs

        tell (ctx_params, [])

        param_ixfun <-
          instantiateIxFun $
            IxFun.substituteInIxFun
              (M.fromList $ zip (fmap Ext [0 ..]) param_ixfun_substs)
              ext_ixfun

        mem_name <- newVName "mem_param"
        tell ([], [Param mem_name $ MemMem mem_space])

        return
          ( mergeparam {paramDec = MemArray bt shape u $ ArrayIn mem_name param_ixfun},
            ensureArrayIn mem_space
          )
    allocInMergeParam (mergeparam, _) = doDefault mergeparam =<< lift askDefaultSpace

    doDefault mergeparam space = do
      mergeparam' <- allocInFParam mergeparam space
      return (mergeparam', linearFuncallArg (paramType mergeparam) space)

-- Returns the existentialized index function, the list of substituted values and the memory location.
existentializeArray ::
  (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
  Space ->
  VName ->
  AllocM fromlore tolore (SubExp, ExtIxFun, [TPrimExp Int32 VName], VName)
existentializeArray space v = do
  (mem', ixfun) <- lookupArraySummary v
  sp <- lookupMemSpace mem'

  let (ext_ixfun', substs') = runState (IxFun.existentialize ixfun) []

  case (ext_ixfun', sp == space) of
    (Just x, True) -> return (Var v, x, substs', mem')
    _ -> do
      (mem, subexp) <- allocLinearArray space (baseString v) v
      ixfun' <- fromJust <$> subExpIxFun subexp
      let (ext_ixfun, substs) = runState (IxFun.existentialize ixfun') []
      return (subexp, fromJust ext_ixfun, substs, mem)

ensureArrayIn ::
  ( Allocable fromlore tolore,
    Allocator tolore (AllocM fromlore tolore)
  ) =>
  Space ->
  SubExp ->
  WriterT ([SubExp], [SubExp]) (AllocM fromlore tolore) SubExp
ensureArrayIn _ (Constant v) =
  error $ "ensureArrayIn: " ++ pretty v ++ " cannot be an array."
ensureArrayIn space (Var v) = do
  (sub_exp, _, substs, mem) <- lift $ existentializeArray space v
  (ctx_vals, _) <-
    unzip
      <$> mapM
        ( \s -> do
            vname <- lift $ letExp "ctx_val" =<< toExp s
            return (Var vname, fmap Free $ primExpFromSubExp int32 $ Var vname)
        )
        substs

  tell (ctx_vals, [Var mem])

  return sub_exp

ensureDirectArray ::
  ( Allocable fromlore tolore,
    Allocator tolore (AllocM fromlore tolore)
  ) =>
  Maybe Space ->
  VName ->
  AllocM fromlore tolore (VName, SubExp)
ensureDirectArray space_ok v = do
  (mem, ixfun) <- lookupArraySummary v
  mem_space <- lookupMemSpace mem
  default_space <- askDefaultSpace
  if IxFun.isDirect ixfun && maybe True (== mem_space) space_ok
    then return (mem, Var v)
    else needCopy (fromMaybe default_space space_ok)
  where
    needCopy space =
      -- We need to do a new allocation, copy 'v', and make a new
      -- binding for the size of the memory block.
      allocLinearArray space (baseString v) v

allocLinearArray ::
  (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
  Space ->
  String ->
  VName ->
  AllocM fromlore tolore (VName, SubExp)
allocLinearArray space s v = do
  t <- lookupType v
  mem <- allocForArray t space
  v' <- newIdent (s ++ "_linear") t
  let ixfun = directIxFun (elemType t) (arrayShape t) NoUniqueness mem t
  let pat = Pattern [] [PatElem (identName v') ixfun]
  addStm $ Let pat (defAux ()) $ BasicOp $ Copy v
  return (mem, Var $ identName v')

funcallArgs ::
  ( Allocable fromlore tolore,
    Allocator tolore (AllocM fromlore tolore)
  ) =>
  [(SubExp, Diet)] ->
  AllocM fromlore tolore [(SubExp, Diet)]
funcallArgs args = do
  (valargs, (ctx_args, mem_and_size_args)) <- runWriterT $
    forM args $ \(arg, d) -> do
      t <- lift $ subExpType arg
      space <- lift askDefaultSpace
      arg' <- linearFuncallArg t space arg
      return (arg', d)
  return $ map (,Observe) (ctx_args <> mem_and_size_args) <> valargs

linearFuncallArg ::
  ( Allocable fromlore tolore,
    Allocator tolore (AllocM fromlore tolore)
  ) =>
  Type ->
  Space ->
  SubExp ->
  WriterT ([SubExp], [SubExp]) (AllocM fromlore tolore) SubExp
linearFuncallArg Array {} space (Var v) = do
  (mem, arg') <- lift $ ensureDirectArray (Just space) v
  tell ([], [Var mem])
  return arg'
linearFuncallArg _ _ arg =
  return arg

explicitAllocationsGeneric ::
  ( Allocable fromlore tolore,
    Allocator tolore (AllocM fromlore tolore)
  ) =>
  (Op fromlore -> AllocM fromlore tolore (Op tolore)) ->
  (Exp tolore -> AllocM fromlore tolore [ExpHint]) ->
  Pass fromlore tolore
explicitAllocationsGeneric handleOp hints =
  Pass "explicit allocations" "Transform program to explicit memory representation" $
    intraproceduralTransformationWithConsts onStms allocInFun
  where
    onStms stms = runAllocM handleOp hints $ allocInStms stms pure

    allocInFun consts (FunDef entry attrs fname rettype params fbody foreigns) =
      runAllocM handleOp hints $
        inScopeOf consts $
          allocInFParams (zip params $ repeat DefaultSpace) $ \params' -> do
            fbody' <-
              insertStmsM $
                allocInFunBody
                  (map (const $ Just DefaultSpace) rettype)
                  fbody
            return $ FunDef entry attrs fname (memoryInDeclExtType rettype) params' fbody' foreigns

explicitAllocationsInStmsGeneric ::
  ( MonadFreshNames m,
    HasScope tolore m,
    Allocable fromlore tolore
  ) =>
  (Op fromlore -> AllocM fromlore tolore (Op tolore)) ->
  (Exp tolore -> AllocM fromlore tolore [ExpHint]) ->
  Stms fromlore ->
  m (Stms tolore)
explicitAllocationsInStmsGeneric handleOp hints stms = do
  scope <- askScope
  runAllocM handleOp hints $ localScope scope $ allocInStms stms return

memoryInDeclExtType :: [DeclExtType] -> [FunReturns]
memoryInDeclExtType ts = evalState (mapM addMem ts) $ startOfFreeIDRange ts
  where
    addMem (Prim t) = return $ MemPrim t
    addMem Mem {} = error "memoryInDeclExtType: too much memory"
    addMem (Array bt shape u) = do
      i <- get <* modify (+ 1)
      return $
        MemArray bt shape u $
          ReturnsNewBlock DefaultSpace i $
            IxFun.iota $ map convert $ shapeDims shape

    convert (Ext i) = le32 $ Ext i
    convert (Free v) = Free <$> pe32 v

startOfFreeIDRange :: [TypeBase ExtShape u] -> Int
startOfFreeIDRange = S.size . shapeContext

bodyReturnMemCtx ::
  (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
  SubExp ->
  AllocM fromlore tolore [SubExp]
bodyReturnMemCtx Constant {} =
  return []
bodyReturnMemCtx (Var v) = do
  info <- lookupMemInfo v
  case info of
    MemPrim {} -> return []
    MemMem {} -> return [] -- should not happen
    MemArray _ _ _ (ArrayIn mem _) -> return [Var mem]

allocInFunBody ::
  (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
  [Maybe Space] ->
  Body fromlore ->
  AllocM fromlore tolore (Body tolore)
allocInFunBody space_oks (Body _ bnds res) =
  allocInStms bnds $ \bnds' -> do
    (res'', allocs) <- collectStms $ do
      res' <- zipWithM ensureDirect space_oks' res
      let (ctx_res, val_res) = splitFromEnd num_vals res'
      mem_ctx_res <- concat <$> mapM bodyReturnMemCtx val_res
      return $ ctx_res <> mem_ctx_res <> val_res
    return $ Body () (bnds' <> allocs) res''
  where
    num_vals = length space_oks
    space_oks' = replicate (length res - num_vals) Nothing ++ space_oks

ensureDirect ::
  (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
  Maybe Space ->
  SubExp ->
  AllocM fromlore tolore SubExp
ensureDirect _ se@Constant {} = return se
ensureDirect space_ok (Var v) = do
  bt <- primType <$> lookupType v
  if bt
    then return $ Var v
    else do
      (_, v') <- ensureDirectArray space_ok v
      return v'

allocInStms ::
  (Allocable fromlore tolore) =>
  Stms fromlore ->
  (Stms tolore -> AllocM fromlore tolore a) ->
  AllocM fromlore tolore a
allocInStms origstms m = allocInStms' (stmsToList origstms) mempty
  where
    allocInStms' [] stms' =
      m stms'
    allocInStms' (x : xs) stms' = do
      allocstms <- allocInStm' x
      localScope (scopeOf allocstms) $ do
        let stms_substs = foldMap sizeSubst allocstms
            stms_consts = foldMap stmConsts allocstms
            f env =
              env
                { chunkMap = stms_substs <> chunkMap env,
                  envConsts = stms_consts <> envConsts env
                }
        local f $ allocInStms' xs (stms' <> allocstms)
    allocInStm' stm =
      collectStms_ $ auxing (stmAux stm) $ allocInStm stm

allocInStm ::
  (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
  Stm fromlore ->
  AllocM fromlore tolore ()
allocInStm (Let (Pattern sizeElems valElems) _ e) = do
  e' <- allocInExp e
  let sizeidents = map patElemIdent sizeElems
      validents = map patElemIdent valElems
  bnd <- allocsForStm sizeidents validents e'
  addStm bnd

allocInExp ::
  (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
  Exp fromlore ->
  AllocM fromlore tolore (Exp tolore)
allocInExp (DoLoop ctx val form (Body () bodybnds bodyres)) =
  allocInMergeParams ctx $ \_ ctxparams' _ ->
    allocInMergeParams val $
      \new_ctx_params valparams' mk_loop_val -> do
        form' <- allocInLoopForm form
        localScope (scopeOf form') $ do
          (valinit_ctx, valinit') <- mk_loop_val valinit
          body' <- insertStmsM $
            allocInStms bodybnds $ \bodybnds' -> do
              ((val_ses, valres'), val_retbnds) <- collectStms $ mk_loop_val valres
              return $ Body () (bodybnds' <> val_retbnds) (ctxres ++ val_ses ++ valres')
          return $
            DoLoop
              (zip (ctxparams' ++ new_ctx_params) (ctxinit ++ valinit_ctx))
              (zip valparams' valinit')
              form'
              body'
  where
    (_ctxparams, ctxinit) = unzip ctx
    (_valparams, valinit) = unzip val
    (ctxres, valres) = splitAt (length ctx) bodyres
allocInExp (Apply fname args rettype loc) = do
  args' <- funcallArgs args
  return $ Apply fname args' (memoryInDeclExtType rettype) loc
allocInExp (If cond tbranch0 fbranch0 (IfDec rets ifsort)) = do
  let num_rets = length rets
  -- switch to the explicit-mem rep, but do nothing about results
  (tbranch, tm_ixfs) <- allocInIfBody num_rets tbranch0
  (fbranch, fm_ixfs) <- allocInIfBody num_rets fbranch0
  tspaces <- mkSpaceOks num_rets tbranch
  fspaces <- mkSpaceOks num_rets fbranch
  -- try to generalize (antiunify) the index functions of the then and else bodies
  let sp_substs = zipWith generalize (zip tspaces tm_ixfs) (zip fspaces fm_ixfs)
      (spaces, subs) = unzip sp_substs
      tsubs = map (selectSub fst) subs
      fsubs = map (selectSub snd) subs
  (tbranch', trets) <- addResCtxInIfBody rets tbranch spaces tsubs
  (fbranch', frets) <- addResCtxInIfBody rets fbranch spaces fsubs
  if frets /= trets
    then error "In allocInExp, IF case: antiunification of then/else produce different ExtInFn!"
    else do
      -- above is a sanity check; implementation continues on else branch
      let res_then = bodyResult tbranch'
          res_else = bodyResult fbranch'
          size_ext = length res_then - length trets
          (ind_ses0, r_then_else) =
            partition (\(r_then, r_else, _) -> r_then == r_else) $
              zip3 res_then res_else [0 .. size_ext - 1]
          (r_then_ext, r_else_ext, _) = unzip3 r_then_else
          ind_ses =
            zipWith
              (\(se, _, i) k -> (i - k, se))
              ind_ses0
              [0 .. length ind_ses0 - 1]
          rets'' = foldl (\acc (i, se) -> fixExt i se acc) trets ind_ses
          tbranch'' = tbranch' {bodyResult = r_then_ext ++ drop size_ext res_then}
          fbranch'' = fbranch' {bodyResult = r_else_ext ++ drop size_ext res_else}
          res_if_expr = If cond tbranch'' fbranch'' $ IfDec rets'' ifsort
      return res_if_expr
  where
    generalize ::
      (Maybe Space, Maybe IxFun) ->
      (Maybe Space, Maybe IxFun) ->
      (Maybe Space, Maybe (ExtIxFun, [(TPrimExp Int32 VName, TPrimExp Int32 VName)]))
    generalize (Just sp1, Just ixf1) (Just sp2, Just ixf2) =
      if sp1 /= sp2
        then (Just sp1, Nothing)
        else case IxFun.leastGeneralGeneralization (fmap untyped ixf1) (fmap untyped ixf2) of
          Just (ixf, m) ->
            ( Just sp1,
              Just
                ( fmap TPrimExp ixf,
                  zip (map (TPrimExp . fst) m) (map (TPrimExp . snd) m)
                )
            )
          Nothing -> (Just sp1, Nothing)
    generalize (mbsp1, _) _ = (mbsp1, Nothing)

    selectSub ::
      ((a, a) -> a) ->
      Maybe (ExtIxFun, [(a, a)]) ->
      Maybe (ExtIxFun, [a])
    selectSub f (Just (ixfn, m)) = Just (ixfn, map f m)
    selectSub _ Nothing = Nothing
    allocInIfBody ::
      (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
      Int ->
      Body fromlore ->
      AllocM fromlore tolore (Body tolore, [Maybe IxFun])
    allocInIfBody num_vals (Body _ bnds res) =
      allocInStms bnds $ \bnds' -> do
        let (_, val_res) = splitFromEnd num_vals res
        mem_ixfs <- mapM subExpIxFun val_res
        return (Body () bnds' res, mem_ixfs)
allocInExp e = mapExpM alloc e
  where
    alloc =
      identityMapper
        { mapOnBody = error "Unhandled Body in ExplicitAllocations",
          mapOnRetType = error "Unhandled RetType in ExplicitAllocations",
          mapOnBranchType = error "Unhandled BranchType in ExplicitAllocations",
          mapOnFParam = error "Unhandled FParam in ExplicitAllocations",
          mapOnLParam = error "Unhandled LParam in ExplicitAllocations",
          mapOnOp = \op -> do
            handle <- asks allocInOp
            handle op
        }

subExpIxFun ::
  (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
  SubExp ->
  AllocM fromlore tolore (Maybe IxFun)
subExpIxFun Constant {} = return Nothing
subExpIxFun (Var v) = do
  info <- lookupMemInfo v
  case info of
    MemArray _ptp _shp _u (ArrayIn _ ixf) -> return $ Just ixf
    _ -> return Nothing

addResCtxInIfBody ::
  (Allocable fromlore tolore, Allocator tolore (AllocM fromlore tolore)) =>
  [ExtType] ->
  Body tolore ->
  [Maybe Space] ->
  [Maybe (ExtIxFun, [TPrimExp Int32 VName])] ->
  AllocM fromlore tolore (Body tolore, [BodyReturns])
addResCtxInIfBody ifrets (Body _ bnds res) spaces substs = do
  let num_vals = length ifrets
      (ctx_res, val_res) = splitFromEnd num_vals res
  ((res', bodyrets'), all_body_stms) <- collectStms $ do
    mapM_ addStm bnds
    (val_res', ext_ses_res, mem_ctx_res, bodyrets, total_existentials) <-
      foldM helper ([], [], [], [], length ctx_res) (zip4 ifrets val_res substs spaces)
    return
      ( ctx_res <> ext_ses_res <> mem_ctx_res <> val_res',
        -- We need to adjust the ReturnsNewBlock existentials, because they
        -- should always be numbered _after_ all other existentials in the
        -- return values.
        reverse $ fst $ foldl adjustNewBlockExistential ([], total_existentials) bodyrets
      )
  body' <- mkBodyM all_body_stms res'
  return (body', bodyrets')
  where
    helper (res_acc, ext_acc, ctx_acc, br_acc, k) (ifr, r, mbixfsub, sp) =
      case mbixfsub of
        Nothing -> do
          -- does NOT generalize/antiunify; ensure direct
          r' <- ensureDirect sp r
          mem_ctx_r <- bodyReturnMemCtx r'
          let body_ret = inspect ifr sp
          return
            ( res_acc ++ [r'],
              ext_acc,
              ctx_acc ++ mem_ctx_r,
              br_acc ++ [body_ret],
              k
            )
        Just (ixfn, m) -> do
          -- generalizes
          let i = length m
          ext_ses <- mapM (toSubExp "ixfn_exist") m
          mem_ctx_r <- bodyReturnMemCtx r
          let sp' = fromMaybe DefaultSpace sp
              ixfn' = fmap (adjustExtPE k) ixfn
              exttp = case ifr of
                Array pt shp' u ->
                  MemArray pt shp' u $
                    ReturnsNewBlock sp' 0 ixfn'
                _ -> error "Impossible case reached in addResCtxInIfBody"
          return
            ( res_acc ++ [r],
              ext_acc ++ ext_ses,
              ctx_acc ++ mem_ctx_r,
              br_acc ++ [exttp],
              k + i
            )

    adjustNewBlockExistential :: ([BodyReturns], Int) -> BodyReturns -> ([BodyReturns], Int)
    adjustNewBlockExistential (acc, k) (MemArray pt shp u (ReturnsNewBlock space _ ixfun)) =
      (MemArray pt shp u (ReturnsNewBlock space k ixfun) : acc, k + 1)
    adjustNewBlockExistential (acc, k) x = (x : acc, k)

    inspect (Array pt shape u) space =
      let space' = fromMaybe DefaultSpace space
          bodyret =
            MemArray pt shape u $
              ReturnsNewBlock space' 0 $
                IxFun.iota $ map convert $ shapeDims shape
       in bodyret
    inspect (Prim pt) _ = MemPrim pt
    inspect (Mem space) _ = MemMem space

    convert (Ext i) = le32 (Ext i)
    convert (Free v) = Free <$> pe32 v

    adjustExtV :: Int -> Ext VName -> Ext VName
    adjustExtV _ (Free v) = Free v
    adjustExtV k (Ext i) = Ext (k + i)

    adjustExtPE :: Int -> TPrimExp t (Ext VName) -> TPrimExp t (Ext VName)
    adjustExtPE k = fmap (adjustExtV k)

mkSpaceOks ::
  (Mem tolore, LocalScope tolore m) =>
  Int ->
  Body tolore ->
  m [Maybe Space]
mkSpaceOks num_vals (Body _ stms res) =
  inScopeOf stms $
    mapM mkSpaceOK $ takeLast num_vals res
  where
    mkSpaceOK (Var v) = do
      v_info <- lookupMemInfo v
      case v_info of
        MemArray _ _ _ (ArrayIn mem _) -> do
          mem_info <- lookupMemInfo mem
          case mem_info of
            MemMem space -> return $ Just space
            _ -> return Nothing
        _ -> return Nothing
    mkSpaceOK _ = return Nothing

allocInLoopForm ::
  ( Allocable fromlore tolore,
    Allocator tolore (AllocM fromlore tolore)
  ) =>
  LoopForm fromlore ->
  AllocM fromlore tolore (LoopForm tolore)
allocInLoopForm (WhileLoop v) = return $ WhileLoop v
allocInLoopForm (ForLoop i it n loopvars) =
  ForLoop i it n <$> mapM allocInLoopVar loopvars
  where
    allocInLoopVar (p, a) = do
      (mem, ixfun) <- lookupArraySummary a
      case paramType p of
        Array bt shape u -> do
          dims <- map pe32 . arrayDims <$> lookupType a
          let ixfun' =
                IxFun.slice ixfun $
                  fullSliceNum dims [DimFix $ le32 i]
          return (p {paramDec = MemArray bt shape u $ ArrayIn mem ixfun'}, a)
        Prim bt ->
          return (p {paramDec = MemPrim bt}, a)
        Mem space ->
          return (p {paramDec = MemMem space}, a)

class SizeSubst op where
  opSizeSubst :: PatternT dec -> op -> ChunkMap
  opIsConst :: op -> Bool
  opIsConst = const False

instance SizeSubst () where
  opSizeSubst _ _ = mempty

instance SizeSubst op => SizeSubst (MemOp op) where
  opSizeSubst pat (Inner op) = opSizeSubst pat op
  opSizeSubst _ _ = mempty

  opIsConst (Inner op) = opIsConst op
  opIsConst _ = False

sizeSubst :: SizeSubst (Op lore) => Stm lore -> ChunkMap
sizeSubst (Let pat _ (Op op)) = opSizeSubst pat op
sizeSubst _ = mempty

stmConsts :: SizeSubst (Op lore) => Stm lore -> S.Set VName
stmConsts (Let pat _ (Op op))
  | opIsConst op = S.fromList $ patternNames pat
stmConsts _ = mempty

mkLetNamesB' ::
  ( Op (Lore m) ~ MemOp inner,
    MonadBinder m,
    ExpDec (Lore m) ~ (),
    Allocator (Lore m) (PatAllocM (Lore m))
  ) =>
  ExpDec (Lore m) ->
  [VName] ->
  Exp (Lore m) ->
  m (Stm (Lore m))
mkLetNamesB' dec names e = do
  scope <- askScope
  pat <- bindPatternWithAllocations scope names e
  return $ Let pat (defAux dec) e

mkLetNamesB'' ::
  ( Op (Lore m) ~ MemOp inner,
    ExpDec lore ~ (),
    HasScope (Engine.Wise lore) m,
    Allocator lore (PatAllocM lore),
    MonadBinder m,
    Engine.CanBeWise (Op lore)
  ) =>
  [VName] ->
  Exp (Engine.Wise lore) ->
  m (Stm (Engine.Wise lore))
mkLetNamesB'' names e = do
  scope <- Engine.removeScopeWisdom <$> askScope
  (pat, prestms) <- runPatAllocM (patternWithAllocations names $ Engine.removeExpWisdom e) scope
  mapM_ bindAllocStm prestms
  let pat' = Engine.addWisdomToPattern pat e
      dec = Engine.mkWiseExpDec pat' () e
  return $ Let pat' (defAux dec) e

simplifiable ::
  ( Engine.SimplifiableLore lore,
    ExpDec lore ~ (),
    BodyDec lore ~ (),
    Op lore ~ MemOp inner,
    Allocator lore (PatAllocM lore)
  ) =>
  (Engine.OpWithWisdom inner -> UT.UsageTable) ->
  (inner -> Engine.SimpleM lore (Engine.OpWithWisdom inner, Stms (Engine.Wise lore))) ->
  SimpleOps lore
simplifiable innerUsage simplifyInnerOp =
  SimpleOps mkExpDecS' mkBodyS' protectOp opUsage simplifyOp
  where
    mkExpDecS' _ pat e =
      return $ Engine.mkWiseExpDec pat () e

    mkBodyS' _ bnds res = return $ mkWiseBody () bnds res

    protectOp taken pat (Alloc size space) = Just $ do
      tbody <- resultBodyM [size]
      fbody <- resultBodyM [intConst Int64 0]
      size' <-
        letSubExp "hoisted_alloc_size" $
          If taken tbody fbody $ IfDec [MemPrim int64] IfFallback
      letBind pat $ Op $ Alloc size' space
    protectOp _ _ _ = Nothing

    opUsage (Alloc (Var size) _) =
      UT.sizeUsage size
    opUsage (Alloc _ _) =
      mempty
    opUsage (Inner inner) =
      innerUsage inner

    simplifyOp (Alloc size space) =
      (,) <$> (Alloc <$> Engine.simplify size <*> pure space) <*> pure mempty
    simplifyOp (Inner k) = do
      (k', hoisted) <- simplifyInnerOp k
      return (Inner k', hoisted)

bindPatternWithAllocations ::
  ( MonadBinder m,
    ExpDec lore ~ (),
    Op (Lore m) ~ MemOp inner,
    Allocator lore (PatAllocM lore)
  ) =>
  Scope lore ->
  [VName] ->
  Exp lore ->
  m (Pattern lore)
bindPatternWithAllocations types names e = do
  (pat, prebnds) <- runPatAllocM (patternWithAllocations names e) types
  mapM_ bindAllocStm prebnds
  return pat

data ExpHint
  = NoHint
  | Hint IxFun Space

defaultExpHints :: (Monad m, ASTLore lore) => Exp lore -> m [ExpHint]
defaultExpHints e = return $ replicate (expExtTypeSize e) NoHint
