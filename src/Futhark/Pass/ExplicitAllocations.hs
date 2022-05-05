{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    AllocM,
    AllocEnv (..),
    SizeSubst (..),
    allocInStms,
    allocForArray,
    simplifiable,
    arraySizeInBytesExp,
    mkLetNamesB',
    mkLetNamesB'',
    dimAllocationSize,
    ChunkMap,

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
import Data.List (foldl', partition, zip5)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.IR.Mem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplify.Engine (SimpleOps (..))
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Optimise.Simplify.Rep (mkWiseBody)
import Futhark.Pass
import Futhark.Tools
import Futhark.Util (maybeNth, splitAt3, splitFromEnd, takeLast)

-- | The subexpression giving the number of elements we should
-- allocate space for.  See 'ChunkMap' comment.
dimAllocationSize :: ChunkMap -> SubExp -> SubExp
dimAllocationSize chunkmap (Var v) =
  -- It is important to recurse here, as the substitution may itself
  -- be a chunk size.
  maybe (Var v) (dimAllocationSize chunkmap) $ M.lookup v chunkmap
dimAllocationSize _ size =
  size

type Allocable fromrep torep inner =
  ( PrettyRep fromrep,
    PrettyRep torep,
    Mem torep inner,
    LetDec torep ~ LetDecMem,
    FParamInfo fromrep ~ DeclType,
    LParamInfo fromrep ~ Type,
    BranchType fromrep ~ ExtType,
    RetType fromrep ~ DeclExtType,
    BodyDec fromrep ~ (),
    BodyDec torep ~ (),
    ExpDec torep ~ (),
    SizeSubst inner,
    BuilderOps torep
  )

-- | A mapping from chunk names to their maximum size.  XXX FIXME
-- HACK: This is part of a hack to add loop-invariant allocations to
-- reduce kernels, because memory expansion does not use range
-- analysis yet (it should).
type ChunkMap = M.Map VName SubExp

data AllocEnv fromrep torep = AllocEnv
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
    allocInOp :: Op fromrep -> AllocM fromrep torep (Op torep),
    envExpHints :: Exp torep -> AllocM fromrep torep [ExpHint]
  }

-- | Monad for adding allocations to an entire program.
newtype AllocM fromrep torep a
  = AllocM (BuilderT torep (ReaderT (AllocEnv fromrep torep) (State VNameSource)) a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadFreshNames,
      HasScope torep,
      LocalScope torep,
      MonadReader (AllocEnv fromrep torep)
    )

instance (Allocable fromrep torep inner) => MonadBuilder (AllocM fromrep torep) where
  type Rep (AllocM fromrep torep) = torep

  mkExpDecM _ _ = pure ()

  mkLetNamesM names e = do
    def_space <- askDefaultSpace
    chunkmap <- asks chunkMap
    hints <- expHints e
    pat <- patWithAllocations def_space chunkmap names e hints
    pure $ Let pat (defAux ()) e

  mkBodyM stms res = pure $ Body () stms res

  addStms = AllocM . addStms
  collectStms (AllocM m) = AllocM $ collectStms m

expHints :: Exp torep -> AllocM fromrep torep [ExpHint]
expHints e = do
  f <- asks envExpHints
  f e

askDefaultSpace :: AllocM fromrep torep Space
askDefaultSpace = asks allocSpace

runAllocM ::
  MonadFreshNames m =>
  (Op fromrep -> AllocM fromrep torep (Op torep)) ->
  (Exp torep -> AllocM fromrep torep [ExpHint]) ->
  AllocM fromrep torep a ->
  m a
runAllocM handleOp hints (AllocM m) =
  fmap fst $ modifyNameSource $ runState $ runReaderT (runBuilderT m mempty) env
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

elemSize :: Num a => Type -> a
elemSize = primByteSize . elemType

arraySizeInBytesExp :: Type -> PrimExp VName
arraySizeInBytesExp t =
  untyped $ foldl' (*) (elemSize t) $ map pe64 (arrayDims t)

arraySizeInBytesExpM :: MonadBuilder m => ChunkMap -> Type -> m (PrimExp VName)
arraySizeInBytesExpM chunkmap t = do
  let dim_prod_i64 = product $ map (pe64 . dimAllocationSize chunkmap) (arrayDims t)
      elm_size_i64 = elemSize t
  pure $
    BinOpExp (SMax Int64) (ValueExp $ IntValue $ Int64Value 0) $
      untyped $ dim_prod_i64 * elm_size_i64

arraySizeInBytes :: MonadBuilder m => ChunkMap -> Type -> m SubExp
arraySizeInBytes chunkmap = letSubExp "bytes" <=< toExp <=< arraySizeInBytesExpM chunkmap

allocForArray' ::
  (MonadBuilder m, Op (Rep m) ~ MemOp inner) =>
  ChunkMap ->
  Type ->
  Space ->
  m VName
allocForArray' chunkmap t space = do
  size <- arraySizeInBytes chunkmap t
  letExp "mem" $ Op $ Alloc size space

-- | Allocate memory for a value of the given type.
allocForArray ::
  Allocable fromrep torep inner =>
  Type ->
  Space ->
  AllocM fromrep torep VName
allocForArray t space = do
  chunkmap <- asks chunkMap
  allocForArray' chunkmap t space

allocsForStm ::
  (Allocable fromrep torep inner) =>
  [Ident] ->
  Exp torep ->
  AllocM fromrep torep (Stm torep)
allocsForStm idents e = do
  def_space <- askDefaultSpace
  chunkmap <- asks chunkMap
  hints <- expHints e
  rts <- expReturns e
  pes <- allocsForPat def_space chunkmap idents rts hints
  dec <- mkExpDecM (Pat pes) e
  pure $ Let (Pat pes) (defAux dec) e

patWithAllocations ::
  (MonadBuilder m, Mem (Rep m) inner) =>
  Space ->
  ChunkMap ->
  [VName] ->
  Exp (Rep m) ->
  [ExpHint] ->
  m (Pat LetDecMem)
patWithAllocations def_space chunkmap names e hints = do
  ts' <- instantiateShapes' names <$> expExtType e
  let idents = zipWith Ident names ts'
  rts <- expReturns e
  Pat <$> allocsForPat def_space chunkmap idents rts hints

mkMissingIdents :: MonadFreshNames m => [Ident] -> [ExpReturns] -> m [Ident]
mkMissingIdents idents rts =
  reverse <$> zipWithM f (reverse rts) (map Just (reverse idents) ++ repeat Nothing)
  where
    f _ (Just ident) = pure ident
    f (MemMem space) Nothing = newIdent "ext_mem" $ Mem space
    f _ Nothing = newIdent "ext" $ Prim int64

allocsForPat ::
  (MonadBuilder m, Op (Rep m) ~ MemOp inner) =>
  Space ->
  ChunkMap ->
  [Ident] ->
  [ExpReturns] ->
  [ExpHint] ->
  m [PatElem LetDecMem]
allocsForPat def_space chunkmap some_idents rts hints = do
  idents <- mkMissingIdents some_idents rts

  forM (zip3 idents rts hints) $ \(ident, rt, hint) -> do
    let ident_shape = arrayShape $ identType ident
    case rt of
      MemPrim _ -> do
        summary <- summaryForBindage def_space chunkmap (identType ident) hint
        pure $ PatElem (identName ident) summary
      MemMem space ->
        pure $ PatElem (identName ident) $ MemMem space
      MemArray bt _ u (Just (ReturnsInBlock mem extixfun)) -> do
        let ixfn = instantiateExtIxFun idents extixfun
        pure . PatElem (identName ident) . MemArray bt ident_shape u $ ArrayIn mem ixfn
      MemArray _ extshape _ Nothing
        | Just _ <- knownShape extshape -> do
            summary <- summaryForBindage def_space chunkmap (identType ident) hint
            pure $ PatElem (identName ident) summary
      MemArray bt _ u (Just (ReturnsNewBlock _ i extixfn)) -> do
        let ixfn = instantiateExtIxFun idents extixfn
        pure . PatElem (identName ident) . MemArray bt ident_shape u $
          ArrayIn (getIdent idents i) ixfn
      MemAcc acc ispace ts u ->
        pure $ PatElem (identName ident) $ MemAcc acc ispace ts u
      _ -> error "Impossible case reached in allocsForPat!"
  where
    knownShape = mapM known . shapeDims
    known (Free v) = Just v
    known Ext {} = Nothing

    getIdent idents i =
      case maybeNth i idents of
        Just ident -> identName ident
        Nothing ->
          error $ "getIdent: Ext " <> show i <> " but pattern has " <> show (length idents) <> " elements: " <> pretty idents

    instantiateExtIxFun idents = fmap $ fmap inst
      where
        inst (Free v) = v
        inst (Ext i) = getIdent idents i

instantiateIxFun :: Monad m => ExtIxFun -> m IxFun
instantiateIxFun = traverse $ traverse inst
  where
    inst Ext {} = error "instantiateIxFun: not yet"
    inst (Free x) = pure x

summaryForBindage ::
  (MonadBuilder m, Op (Rep m) ~ MemOp inner) =>
  Space ->
  ChunkMap ->
  Type ->
  ExpHint ->
  m (MemBound NoUniqueness)
summaryForBindage _ _ (Prim bt) _ =
  pure $ MemPrim bt
summaryForBindage _ _ (Mem space) _ =
  pure $ MemMem space
summaryForBindage _ _ (Acc acc ispace ts u) _ =
  pure $ MemAcc acc ispace ts u
summaryForBindage def_space chunkmap t@(Array pt shape u) NoHint = do
  m <- allocForArray' chunkmap t def_space
  pure $ directIxFun pt shape u m t
summaryForBindage _ _ t@(Array pt _ _) (Hint ixfun space) = do
  bytes <-
    letSubExp "bytes" <=< toExp . untyped $
      product
        [ product $ IxFun.base ixfun,
          fromIntegral (primByteSize pt :: Int64)
        ]
  m <- letExp "mem" $ Op $ Alloc bytes space
  pure $ MemArray pt (arrayShape t) NoUniqueness $ ArrayIn m ixfun

lookupMemSpace :: (HasScope rep m, Monad m) => VName -> m Space
lookupMemSpace v = do
  t <- lookupType v
  case t of
    Mem space -> pure space
    _ -> error $ "lookupMemSpace: " ++ pretty v ++ " is not a memory block."

directIxFun :: PrimType -> Shape -> u -> VName -> Type -> MemBound u
directIxFun bt shape u mem t =
  let ixf = IxFun.iota $ map pe64 $ arrayDims t
   in MemArray bt shape u $ ArrayIn mem ixf

allocInFParams ::
  (Allocable fromrep torep inner) =>
  [(FParam fromrep, Space)] ->
  ([FParam torep] -> AllocM fromrep torep a) ->
  AllocM fromrep torep a
allocInFParams params m = do
  (valparams, (ctxparams, memparams)) <-
    runWriterT $ mapM (uncurry allocInFParam) params
  let params' = ctxparams <> memparams <> valparams
      summary = scopeOfFParams params'
  localScope summary $ m params'

allocInFParam ::
  (Allocable fromrep torep inner) =>
  FParam fromrep ->
  Space ->
  WriterT
    ([FParam torep], [FParam torep])
    (AllocM fromrep torep)
    (FParam torep)
allocInFParam param pspace =
  case paramDeclType param of
    Array pt shape u -> do
      let memname = baseString (paramName param) <> "_mem"
          ixfun = IxFun.iota $ map pe64 $ shapeDims shape
      mem <- lift $ newVName memname
      tell ([], [Param (paramAttrs param) mem $ MemMem pspace])
      pure param {paramDec = MemArray pt shape u $ ArrayIn mem ixfun}
    Prim pt ->
      pure param {paramDec = MemPrim pt}
    Mem space ->
      pure param {paramDec = MemMem space}
    Acc acc ispace ts u ->
      pure param {paramDec = MemAcc acc ispace ts u}

allocInMergeParams ::
  (Allocable fromrep torep inner) =>
  [(FParam fromrep, SubExp)] ->
  ( [(FParam torep, SubExp)] ->
    ([SubExp] -> AllocM fromrep torep ([SubExp], [SubExp])) ->
    AllocM fromrep torep a
  ) ->
  AllocM fromrep torep a
allocInMergeParams merge m = do
  ((valparams, valargs, handle_loop_subexps), (ctx_params, mem_params)) <-
    runWriterT $ unzip3 <$> mapM allocInMergeParam merge
  let mergeparams' = ctx_params <> mem_params <> valparams
      summary = scopeOfFParams mergeparams'

      mk_loop_res ses = do
        (ses', (ctxargs, memargs)) <-
          runWriterT $ zipWithM ($) handle_loop_subexps ses
        pure (ctxargs <> memargs, ses')

  (valctx_args, valargs') <- mk_loop_res valargs
  let merge' =
        zip
          (ctx_params <> mem_params <> valparams)
          (valctx_args <> valargs')
  localScope summary $ m merge' mk_loop_res
  where
    param_names = namesFromList $ map (paramName . fst) merge
    anyIsLoopParam names = names `namesIntersect` param_names

    scalarRes param_t v_mem_space v_ixfun (Var res) = do
      -- Try really hard to avoid copying needlessly, but the result
      -- _must_ be in ScalarSpace and have the right index function.
      (res_mem, res_ixfun) <- lift $ lookupArraySummary res
      res_mem_space <- lift $ lookupMemSpace res_mem
      chunkmap <- asks chunkMap
      (res_mem', res') <-
        if (res_mem_space, res_ixfun) == (v_mem_space, v_ixfun)
          then pure (res_mem, res)
          else lift $ arrayWithIxFun chunkmap v_mem_space v_ixfun (fromDecl param_t) res
      tell ([], [Var res_mem'])
      pure $ Var res'
    scalarRes _ _ _ se = pure se

    allocInMergeParam ::
      (Allocable fromrep torep inner) =>
      (Param DeclType, SubExp) ->
      WriterT
        ([FParam torep], [FParam torep])
        (AllocM fromrep torep)
        ( FParam torep,
          SubExp,
          SubExp -> WriterT ([SubExp], [SubExp]) (AllocM fromrep torep) SubExp
        )
    allocInMergeParam (mergeparam, Var v)
      | param_t@(Array pt shape u) <- paramDeclType mergeparam = do
          (v_mem, v_ixfun) <- lift $ lookupArraySummary v
          v_mem_space <- lift $ lookupMemSpace v_mem

          -- Loop-invariant array parameters that are in scalar space
          -- are special - we do not wish to existentialise their index
          -- function at all (but the memory block is still existential).
          case v_mem_space of
            ScalarSpace {} ->
              if anyIsLoopParam (freeIn shape)
                then do
                  -- Arrays with loop-variant shape cannot be in scalar
                  -- space, so copy them elsewhere and try again.
                  (_, v') <- lift $ allocLinearArray DefaultSpace (baseString v) v
                  allocInMergeParam (mergeparam, Var v')
                else do
                  p <- newParam "mem_param" $ MemMem v_mem_space
                  tell ([], [p])

                  pure
                    ( mergeparam {paramDec = MemArray pt shape u $ ArrayIn (paramName p) v_ixfun},
                      Var v,
                      scalarRes param_t v_mem_space v_ixfun
                    )
            _ -> do
              (v', ext_ixfun, substs, v_mem') <-
                lift $ existentializeArray v_mem_space v
              v_mem_space' <- lift $ lookupMemSpace v_mem'

              (ctx_params, param_ixfun_substs) <-
                fmap unzip . forM substs $ \e -> do
                  p <- newParam "ctx_param_ext" $ MemPrim $ primExpType $ untyped e
                  pure (p, fmap Free $ le64 $ paramName p)

              tell (ctx_params, [])

              param_ixfun <-
                instantiateIxFun $
                  IxFun.substituteInIxFun
                    (M.fromList $ zip (fmap Ext [0 ..]) param_ixfun_substs)
                    ext_ixfun

              mem_param <- newParam "mem_param" $ MemMem v_mem_space'
              tell ([], [mem_param])
              pure
                ( mergeparam {paramDec = MemArray pt shape u $ ArrayIn (paramName mem_param) param_ixfun},
                  v',
                  ensureArrayIn v_mem_space'
                )
    allocInMergeParam (mergeparam, se) = doDefault mergeparam se =<< lift askDefaultSpace

    doDefault mergeparam se space = do
      mergeparam' <- allocInFParam mergeparam space
      pure (mergeparam', se, linearFuncallArg (paramType mergeparam) space)

-- Returns the existentialized index function, the list of substituted values and the memory location.
existentializeArray ::
  (Allocable fromrep torep inner) =>
  Space ->
  VName ->
  AllocM fromrep torep (SubExp, ExtIxFun, [TPrimExp Int64 VName], VName)
existentializeArray space v = do
  (mem', ixfun) <- lookupArraySummary v
  sp <- lookupMemSpace mem'

  let (ext_ixfun', substs') = runState (IxFun.existentialize ixfun) []

  case (ext_ixfun', sp == space) of
    (Just x, True) -> pure (Var v, x, substs', mem')
    _ -> do
      (mem, v') <- allocLinearArray space (baseString v) v
      ixfun' <- fromJust <$> lookupIxFun v'
      let (ext_ixfun, substs) = runState (IxFun.existentialize ixfun') []
      pure (Var v', fromJust ext_ixfun, substs, mem)

arrayWithIxFun ::
  (MonadBuilder m, Op (Rep m) ~ MemOp inner, LetDec (Rep m) ~ LetDecMem) =>
  ChunkMap ->
  Space ->
  IxFun ->
  Type ->
  VName ->
  m (VName, VName)
arrayWithIxFun chunkmap space ixfun v_t v = do
  let Array pt shape u = v_t
  mem <- allocForArray' chunkmap v_t space
  v_copy <- newVName $ baseString v <> "_scalcopy"
  letBind (Pat [PatElem v_copy $ MemArray pt shape u $ ArrayIn mem ixfun]) $ BasicOp $ Copy v
  pure (mem, v_copy)

ensureArrayIn ::
  (Allocable fromrep torep inner) =>
  Space ->
  SubExp ->
  WriterT ([SubExp], [SubExp]) (AllocM fromrep torep) SubExp
ensureArrayIn _ (Constant v) =
  error $ "ensureArrayIn: " ++ pretty v ++ " cannot be an array."
ensureArrayIn space (Var v) = do
  (sub_exp, _, substs, mem) <- lift $ existentializeArray space v
  (ctx_vals, _) <-
    unzip
      <$> mapM
        ( \s -> do
            vname <- lift $ letExp "ctx_val" =<< toExp s
            pure (Var vname, fmap Free $ primExpFromSubExp int64 $ Var vname)
        )
        substs

  tell (ctx_vals, [Var mem])

  pure sub_exp

ensureDirectArray ::
  (Allocable fromrep torep inner) =>
  Maybe Space ->
  VName ->
  AllocM fromrep torep (VName, VName)
ensureDirectArray space_ok v = do
  (mem, ixfun) <- lookupArraySummary v
  mem_space <- lookupMemSpace mem
  default_space <- askDefaultSpace
  if IxFun.isDirect ixfun && maybe True (== mem_space) space_ok
    then pure (mem, v)
    else needCopy (fromMaybe default_space space_ok)
  where
    needCopy space =
      -- We need to do a new allocation, copy 'v', and make a new
      -- binding for the size of the memory block.
      allocLinearArray space (baseString v) v

allocLinearArray ::
  (Allocable fromrep torep inner) =>
  Space ->
  String ->
  VName ->
  AllocM fromrep torep (VName, VName)
allocLinearArray space s v = do
  t <- lookupType v
  case t of
    Array pt shape u -> do
      mem <- allocForArray t space
      v' <- newVName $ s <> "_linear"
      let ixfun = directIxFun pt shape u mem t
          pat = Pat [PatElem v' ixfun]
      addStm $ Let pat (defAux ()) $ BasicOp $ Copy v
      pure (mem, v')
    _ ->
      error $ "allocLinearArray: " ++ pretty t

funcallArgs ::
  (Allocable fromrep torep inner) =>
  [(SubExp, Diet)] ->
  AllocM fromrep torep [(SubExp, Diet)]
funcallArgs args = do
  (valargs, (ctx_args, mem_and_size_args)) <- runWriterT $
    forM args $ \(arg, d) -> do
      t <- lift $ subExpType arg
      space <- lift askDefaultSpace
      arg' <- linearFuncallArg t space arg
      pure (arg', d)
  pure $ map (,Observe) (ctx_args <> mem_and_size_args) <> valargs

linearFuncallArg ::
  (Allocable fromrep torep inner) =>
  Type ->
  Space ->
  SubExp ->
  WriterT ([SubExp], [SubExp]) (AllocM fromrep torep) SubExp
linearFuncallArg Array {} space (Var v) = do
  (mem, arg') <- lift $ ensureDirectArray (Just space) v
  tell ([], [Var mem])
  pure $ Var arg'
linearFuncallArg _ _ arg =
  pure arg

explicitAllocationsGeneric ::
  (Allocable fromrep torep inner) =>
  (Op fromrep -> AllocM fromrep torep (Op torep)) ->
  (Exp torep -> AllocM fromrep torep [ExpHint]) ->
  Pass fromrep torep
explicitAllocationsGeneric handleOp hints =
  Pass "explicit allocations" "Transform program to explicit memory representation" $
    intraproceduralTransformationWithConsts onStms allocInFun
  where
    onStms stms =
      runAllocM handleOp hints $ collectStms_ $ allocInStms stms $ pure ()

    allocInFun consts (FunDef entry attrs fname rettype params fbody) =
      runAllocM handleOp hints . inScopeOf consts $
        allocInFParams (zip params $ repeat DefaultSpace) $ \params' -> do
          (fbody', mem_rets) <-
            allocInFunBody (map (const $ Just DefaultSpace) rettype) fbody
          let rettype' = mem_rets ++ memoryInDeclExtType (length mem_rets) rettype
          pure $ FunDef entry attrs fname rettype' params' fbody'

explicitAllocationsInStmsGeneric ::
  ( MonadFreshNames m,
    HasScope torep m,
    Allocable fromrep torep inner
  ) =>
  (Op fromrep -> AllocM fromrep torep (Op torep)) ->
  (Exp torep -> AllocM fromrep torep [ExpHint]) ->
  Stms fromrep ->
  m (Stms torep)
explicitAllocationsInStmsGeneric handleOp hints stms = do
  scope <- askScope
  runAllocM handleOp hints $
    localScope scope $ collectStms_ $ allocInStms stms $ pure ()

memoryInDeclExtType :: Int -> [DeclExtType] -> [FunReturns]
memoryInDeclExtType k dets = evalState (mapM addMem dets) 0
  where
    addMem (Prim t) = pure $ MemPrim t
    addMem Mem {} = error "memoryInDeclExtType: too much memory"
    addMem (Array pt shape u) = do
      i <- get <* modify (+ 1)
      let shape' = fmap shift shape
      pure . MemArray pt shape' u . ReturnsNewBlock DefaultSpace i $
        IxFun.iota $ map convert $ shapeDims shape'
    addMem (Acc acc ispace ts u) = pure $ MemAcc acc ispace ts u

    convert (Ext i) = le64 $ Ext i
    convert (Free v) = Free <$> pe64 v

    shift (Ext i) = Ext (i + k)
    shift (Free x) = Free x

bodyReturnMemCtx ::
  (Allocable fromrep torep inner) =>
  SubExpRes ->
  AllocM fromrep torep [(SubExpRes, MemInfo ExtSize u MemReturn)]
bodyReturnMemCtx (SubExpRes _ Constant {}) =
  pure []
bodyReturnMemCtx (SubExpRes _ (Var v)) = do
  info <- lookupMemInfo v
  case info of
    MemPrim {} -> pure []
    MemAcc {} -> pure []
    MemMem {} -> pure [] -- should not happen
    MemArray _ _ _ (ArrayIn mem _) -> do
      mem_info <- lookupMemInfo mem
      case mem_info of
        MemMem space ->
          pure [(subExpRes $ Var mem, MemMem space)]
        _ -> error $ "bodyReturnMemCtx: not a memory block: " ++ pretty mem

allocInFunBody ::
  (Allocable fromrep torep inner) =>
  [Maybe Space] ->
  Body fromrep ->
  AllocM fromrep torep (Body torep, [FunReturns])
allocInFunBody space_oks (Body _ stms res) =
  buildBody . allocInStms stms $ do
    res' <- zipWithM ensureDirect space_oks' res
    (mem_ctx_res, mem_ctx_rets) <- unzip . concat <$> mapM bodyReturnMemCtx res'
    pure (mem_ctx_res <> res', mem_ctx_rets)
  where
    num_vals = length space_oks
    space_oks' = replicate (length res - num_vals) Nothing ++ space_oks

ensureDirect ::
  (Allocable fromrep torep inner) =>
  Maybe Space ->
  SubExpRes ->
  AllocM fromrep torep SubExpRes
ensureDirect space_ok (SubExpRes cs se) = do
  se_info <- subExpMemInfo se
  SubExpRes cs <$> case (se_info, se) of
    (MemArray {}, Var v) -> do
      (_, v') <- ensureDirectArray space_ok v
      pure $ Var v'
    _ ->
      pure se

allocInStms ::
  (Allocable fromrep torep inner) =>
  Stms fromrep ->
  AllocM fromrep torep a ->
  AllocM fromrep torep a
allocInStms origstms m = allocInStms' $ stmsToList origstms
  where
    allocInStms' [] = m
    allocInStms' (stm : stms) = do
      allocstms <- collectStms_ $ auxing (stmAux stm) $ allocInStm stm
      addStms allocstms
      let stms_substs = foldMap sizeSubst allocstms
          stms_consts = foldMap stmConsts allocstms
          f env =
            env
              { chunkMap = stms_substs <> chunkMap env,
                envConsts = stms_consts <> envConsts env
              }
      local f $ allocInStms' stms

allocInStm ::
  (Allocable fromrep torep inner) =>
  Stm fromrep ->
  AllocM fromrep torep ()
allocInStm (Let (Pat pes) _ e) =
  addStm =<< allocsForStm (map patElemIdent pes) =<< allocInExp e

allocInLambda ::
  Allocable fromrep torep inner =>
  [LParam torep] ->
  Body fromrep ->
  AllocM fromrep torep (Lambda torep)
allocInLambda params body =
  mkLambda params . allocInStms (bodyStms body) $ pure $ bodyResult body

allocInExp ::
  (Allocable fromrep torep inner) =>
  Exp fromrep ->
  AllocM fromrep torep (Exp torep)
allocInExp (DoLoop merge form (Body () bodystms bodyres)) =
  allocInMergeParams merge $ \merge' mk_loop_val -> do
    form' <- allocInLoopForm form
    localScope (scopeOf form') $ do
      body' <-
        buildBody_ . allocInStms bodystms $ do
          (val_ses, valres') <- mk_loop_val $ map resSubExp bodyres
          pure $ subExpsRes val_ses <> zipWith SubExpRes (map resCerts bodyres) valres'
      pure $ DoLoop merge' form' body'
allocInExp (Apply fname args rettype loc) = do
  args' <- funcallArgs args
  -- We assume that every array is going to be in its own memory.
  pure $ Apply fname args' (mems ++ memoryInDeclExtType num_arrays rettype) loc
  where
    mems = replicate num_arrays (MemMem DefaultSpace)
    num_arrays = length $ filter ((> 0) . arrayRank . declExtTypeOf) rettype
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
          rets'' = foldl (\acc (i, SubExpRes _ se) -> fixExt i se acc) trets ind_ses
          tbranch'' = tbranch' {bodyResult = r_then_ext ++ drop size_ext res_then}
          fbranch'' = fbranch' {bodyResult = r_else_ext ++ drop size_ext res_else}
          res_if_expr = If cond tbranch'' fbranch'' $ IfDec rets'' ifsort
      pure res_if_expr
  where
    generalize ::
      (Maybe Space, Maybe IxFun) ->
      (Maybe Space, Maybe IxFun) ->
      (Maybe Space, Maybe (ExtIxFun, [(TPrimExp Int64 VName, TPrimExp Int64 VName)]))
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
      (Allocable fromrep torep inner) =>
      Int ->
      Body fromrep ->
      AllocM fromrep torep (Body torep, [Maybe IxFun])
    allocInIfBody num_vals (Body _ stms res) =
      buildBody . allocInStms stms $ do
        let (_, val_res) = splitFromEnd num_vals res
        mem_ixfs <- mapM (subExpIxFun . resSubExp) val_res
        pure (res, mem_ixfs)
allocInExp (WithAcc inputs bodylam) =
  WithAcc <$> mapM onInput inputs <*> onLambda bodylam
  where
    onLambda lam = do
      params <- forM (lambdaParams lam) $ \(Param attrs pv t) ->
        case t of
          Prim Unit -> pure $ Param attrs pv $ MemPrim Unit
          Acc acc ispace ts u -> pure $ Param attrs pv $ MemAcc acc ispace ts u
          _ -> error $ "Unexpected WithAcc lambda param: " ++ pretty (Param attrs pv t)
      allocInLambda params (lambdaBody lam)

    onInput (shape, arrs, op) =
      (shape,arrs,) <$> traverse (onOp shape arrs) op

    onOp accshape arrs (lam, nes) = do
      let num_vs = length (lambdaReturnType lam)
          num_is = shapeRank accshape
          (i_params, x_params, y_params) =
            splitAt3 num_is num_vs $ lambdaParams lam
          i_params' = map (\(Param attrs v _) -> Param attrs v $ MemPrim int64) i_params
          is = map (DimFix . Var . paramName) i_params'
      x_params' <- zipWithM (onXParam is) x_params arrs
      y_params' <- zipWithM (onYParam is) y_params arrs
      lam' <-
        allocInLambda
          (i_params' <> x_params' <> y_params')
          (lambdaBody lam)
      pure (lam', nes)

    mkP attrs p pt shape u mem ixfun is =
      Param attrs p . MemArray pt shape u . ArrayIn mem . IxFun.slice ixfun $
        fmap pe64 $ Slice $ is ++ map sliceDim (shapeDims shape)

    onXParam _ (Param attrs p (Prim t)) _ =
      pure $ Param attrs p (MemPrim t)
    onXParam is (Param attrs p (Array pt shape u)) arr = do
      (mem, ixfun) <- lookupArraySummary arr
      pure $ mkP attrs p pt shape u mem ixfun is
    onXParam _ p _ =
      error $ "Cannot handle MkAcc param: " ++ pretty p

    onYParam _ (Param attrs p (Prim t)) _ =
      pure $ Param attrs p $ MemPrim t
    onYParam is (Param attrs p (Array pt shape u)) arr = do
      arr_t <- lookupType arr
      mem <- allocForArray arr_t DefaultSpace
      let base_dims = map pe64 $ arrayDims arr_t
          ixfun = IxFun.iota base_dims
      pure $ mkP attrs p pt shape u mem ixfun is
    onYParam _ p _ =
      error $ "Cannot handle MkAcc param: " ++ pretty p
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

lookupIxFun ::
  (Allocable fromrep torep inner) =>
  VName ->
  AllocM fromrep torep (Maybe IxFun)
lookupIxFun v = do
  info <- lookupMemInfo v
  case info of
    MemArray _ptp _shp _u (ArrayIn _ ixf) -> pure $ Just ixf
    _ -> pure Nothing

subExpIxFun ::
  (Allocable fromrep torep inner) =>
  SubExp ->
  AllocM fromrep torep (Maybe IxFun)
subExpIxFun Constant {} = pure Nothing
subExpIxFun (Var v) = lookupIxFun v

addResCtxInIfBody ::
  (Allocable fromrep torep inner) =>
  [ExtType] ->
  Body torep ->
  [Maybe Space] ->
  [Maybe (ExtIxFun, [TPrimExp Int64 VName])] ->
  AllocM fromrep torep (Body torep, [BodyReturns])
addResCtxInIfBody ifrets (Body _ stms res) spaces substs = buildBody $ do
  mapM_ addStm stms
  let offsets = scanl (+) 0 $ zipWith numCtxNeeded ifrets substs
      num_new_ctx = last offsets
  (ctx, ctx_rets, res', res_rets) <-
    foldM (helper num_new_ctx) ([], [], [], []) $
      zip5 ifrets res substs spaces offsets
  pure (ctx <> res', ctx_rets ++ res_rets)
  where
    numCtxNeeded Array {} Nothing = 1
    numCtxNeeded Array {} (Just (_, m)) = length m + 1
    numCtxNeeded _ _ = 0

    helper
      num_new_ctx
      (ctx_acc, ctx_rets_acc, res_acc, res_rets_acc)
      (ifr, r, mbixfsub, sp, ctx_offset) =
        case mbixfsub of
          Nothing -> do
            -- does NOT generalize/antiunify; ensure direct
            r' <- ensureDirect sp r
            (mem_ctx_ses, mem_ctx_rets) <- unzip <$> bodyReturnMemCtx r'
            let body_ret = inspect num_new_ctx ctx_offset ifr sp
            pure
              ( ctx_acc ++ mem_ctx_ses,
                ctx_rets_acc ++ mem_ctx_rets,
                res_acc ++ [r'],
                res_rets_acc ++ [body_ret]
              )
          Just (ixfn, m) -> do
            -- generalizes
            let i = length m
            ext_ses <- mapM (toSubExp "ixfn_exist") m
            (mem_ctx_ses, mem_ctx_rets) <- unzip <$> bodyReturnMemCtx r
            let sp' = fromMaybe DefaultSpace sp
                ixfn' = fmap (adjustExtPE ctx_offset) ixfn
                exttp = case ifr of
                  Array pt shape u ->
                    MemArray pt (fmap (adjustExt num_new_ctx) shape) u $
                      ReturnsNewBlock sp' (ctx_offset + i) ixfn'
                  _ -> error "Impossible case reached in addResCtxInIfBody"
            pure
              ( ctx_acc ++ subExpsRes ext_ses ++ mem_ctx_ses,
                ctx_rets_acc ++ map (const (MemPrim int64)) ext_ses ++ mem_ctx_rets,
                res_acc ++ [r],
                res_rets_acc ++ [exttp]
              )

    inspect num_new_ctx k (Array pt shape u) space =
      let space' = fromMaybe DefaultSpace space
          shape' = fmap (adjustExt num_new_ctx) shape
          bodyret =
            MemArray pt shape' u . ReturnsNewBlock space' k $
              IxFun.iota $ map convert $ shapeDims shape'
       in bodyret
    inspect _ _ (Acc acc ispace ts u) _ = MemAcc acc ispace ts u
    inspect _ _ (Prim pt) _ = MemPrim pt
    inspect _ _ (Mem space) _ = MemMem space

    convert (Ext i) = le64 (Ext i)
    convert (Free v) = Free <$> pe64 v

    adjustExt :: Int -> Ext a -> Ext a
    adjustExt _ (Free v) = Free v
    adjustExt k (Ext i) = Ext (k + i)

    adjustExtPE :: Int -> TPrimExp t (Ext VName) -> TPrimExp t (Ext VName)
    adjustExtPE k = fmap (adjustExt k)

mkSpaceOks ::
  (Mem torep inner, LocalScope torep m) =>
  Int ->
  Body torep ->
  m [Maybe Space]
mkSpaceOks num_vals (Body _ stms res) =
  inScopeOf stms $ mapM (mkSpaceOK . resSubExp) $ takeLast num_vals res
  where
    mkSpaceOK (Var v) = do
      v_info <- lookupMemInfo v
      case v_info of
        MemArray _ _ _ (ArrayIn mem _) -> do
          mem_info <- lookupMemInfo mem
          case mem_info of
            MemMem space -> pure $ Just space
            _ -> pure Nothing
        _ -> pure Nothing
    mkSpaceOK _ = pure Nothing

allocInLoopForm ::
  (Allocable fromrep torep inner) =>
  LoopForm fromrep ->
  AllocM fromrep torep (LoopForm torep)
allocInLoopForm (WhileLoop v) = pure $ WhileLoop v
allocInLoopForm (ForLoop i it n loopvars) =
  ForLoop i it n <$> mapM allocInLoopVar loopvars
  where
    allocInLoopVar (p, a) = do
      (mem, ixfun) <- lookupArraySummary a
      case paramType p of
        Array pt shape u -> do
          dims <- map pe64 . arrayDims <$> lookupType a
          let ixfun' =
                IxFun.slice ixfun $
                  fullSliceNum dims [DimFix $ le64 i]
          pure (p {paramDec = MemArray pt shape u $ ArrayIn mem ixfun'}, a)
        Prim bt ->
          pure (p {paramDec = MemPrim bt}, a)
        Mem space ->
          pure (p {paramDec = MemMem space}, a)
        Acc acc ispace ts u ->
          pure (p {paramDec = MemAcc acc ispace ts u}, a)

class SizeSubst op where
  opSizeSubst :: Pat dec -> op -> ChunkMap
  opIsConst :: op -> Bool
  opIsConst = const False

instance SizeSubst () where
  opSizeSubst _ _ = mempty

instance SizeSubst op => SizeSubst (MemOp op) where
  opSizeSubst pat (Inner op) = opSizeSubst pat op
  opSizeSubst _ _ = mempty

  opIsConst (Inner op) = opIsConst op
  opIsConst _ = False

sizeSubst :: SizeSubst (Op rep) => Stm rep -> ChunkMap
sizeSubst (Let pat _ (Op op)) = opSizeSubst pat op
sizeSubst _ = mempty

stmConsts :: SizeSubst (Op rep) => Stm rep -> S.Set VName
stmConsts (Let pat _ (Op op))
  | opIsConst op = S.fromList $ patNames pat
stmConsts _ = mempty

mkLetNamesB' ::
  ( LetDec (Rep m) ~ LetDecMem,
    Mem (Rep m) inner,
    MonadBuilder m,
    ExpDec (Rep m) ~ ()
  ) =>
  ExpDec (Rep m) ->
  [VName] ->
  Exp (Rep m) ->
  m (Stm (Rep m))
mkLetNamesB' dec names e = do
  pat <- patWithAllocations DefaultSpace mempty names e nohints
  pure $ Let pat (defAux dec) e
  where
    nohints = map (const NoHint) names

mkLetNamesB'' ::
  ( BuilderOps rep,
    Mem rep inner,
    LetDec rep ~ LetDecMem,
    OpReturns (Engine.OpWithWisdom inner),
    ExpDec rep ~ (),
    Rep m ~ Engine.Wise rep,
    HasScope (Engine.Wise rep) m,
    MonadBuilder m,
    Engine.CanBeWise inner
  ) =>
  [VName] ->
  Exp (Engine.Wise rep) ->
  m (Stm (Engine.Wise rep))
mkLetNamesB'' names e = do
  pat <- patWithAllocations DefaultSpace mempty names e nohints
  let pat' = Engine.addWisdomToPat pat e
      dec = Engine.mkWiseExpDec pat' () e
  pure $ Let pat' (defAux dec) e
  where
    nohints = map (const NoHint) names

simplifiable ::
  ( Engine.SimplifiableRep rep,
    ExpDec rep ~ (),
    BodyDec rep ~ (),
    Mem rep inner
  ) =>
  (Engine.OpWithWisdom inner -> UT.UsageTable) ->
  (Engine.OpWithWisdom inner -> Engine.SimpleM rep (Engine.OpWithWisdom inner, Stms (Engine.Wise rep))) ->
  SimpleOps rep
simplifiable innerUsage simplifyInnerOp =
  SimpleOps mkExpDecS' mkBodyS' protectOp opUsage simplifyOp
  where
    mkExpDecS' _ pat e =
      pure $ Engine.mkWiseExpDec pat () e

    mkBodyS' _ stms res = pure $ mkWiseBody () stms res

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
      pure (Inner k', hoisted)

data ExpHint
  = NoHint
  | Hint IxFun Space

defaultExpHints :: (Monad m, ASTRep rep) => Exp rep -> m [ExpHint]
defaultExpHints e = pure $ replicate (expExtTypeSize e) NoHint
