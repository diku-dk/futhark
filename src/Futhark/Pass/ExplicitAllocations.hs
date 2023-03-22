{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | A generic transformation for adding memory allocations to a
-- Futhark program.  Specialised by specific representations in
-- submodules.
module Futhark.Pass.ExplicitAllocations
  ( explicitAllocationsGeneric,
    explicitAllocationsInStmsGeneric,
    ExpHint (..),
    defaultExpHints,
    askDefaultSpace,
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

import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.List (foldl', transpose, zip4)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.Analysis.SymbolTable (IndexOp)
import Futhark.Analysis.UsageTable qualified as UT
import Futhark.IR.Mem
import Futhark.IR.Mem.IxFun qualified as IxFun
import Futhark.IR.Prop.Aliases (AliasedOp)
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplify.Engine (SimpleOps (..))
import Futhark.Optimise.Simplify.Engine qualified as Engine
import Futhark.Optimise.Simplify.Rep (mkWiseBody)
import Futhark.Pass
import Futhark.Tools
import Futhark.Util (maybeNth, splitAt3)

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
    SizeSubst (inner torep),
    BuilderOps torep
  )

data AllocEnv fromrep torep = AllocEnv
  { -- | Aggressively try to reuse memory in do-loops -
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
    hints <- expHints e
    pat <- patWithAllocations def_space names e hints
    pure $ Let pat (defAux ()) e

  mkBodyM stms res = pure $ Body () stms res

  addStms = AllocM . addStms
  collectStms (AllocM m) = AllocM $ collectStms m

expHints :: Exp torep -> AllocM fromrep torep [ExpHint]
expHints e = do
  f <- asks envExpHints
  f e

-- | The space in which we allocate memory if we have no other
-- preferences or constraints.
askDefaultSpace :: AllocM fromrep torep Space
askDefaultSpace = asks allocSpace

runAllocM ::
  MonadFreshNames m =>
  Space ->
  (Op fromrep -> AllocM fromrep torep (Op torep)) ->
  (Exp torep -> AllocM fromrep torep [ExpHint]) ->
  AllocM fromrep torep a ->
  m a
runAllocM space handleOp hints (AllocM m) =
  fmap fst $ modifyNameSource $ runState $ runReaderT (runBuilderT m mempty) env
  where
    env =
      AllocEnv
        { aggressiveReuse = False,
          allocSpace = space,
          envConsts = mempty,
          allocInOp = handleOp,
          envExpHints = hints
        }

elemSize :: Num a => Type -> a
elemSize = primByteSize . elemType

arraySizeInBytesExp :: Type -> PrimExp VName
arraySizeInBytesExp t =
  untyped $ foldl' (*) (elemSize t) $ map pe64 (arrayDims t)

arraySizeInBytesExpM :: MonadBuilder m => Type -> m (PrimExp VName)
arraySizeInBytesExpM t = do
  let dim_prod_i64 = product $ map pe64 (arrayDims t)
      elm_size_i64 = elemSize t
  pure $
    BinOpExp (SMax Int64) (ValueExp $ IntValue $ Int64Value 0) $
      untyped $
        dim_prod_i64 * elm_size_i64

arraySizeInBytes :: MonadBuilder m => Type -> m SubExp
arraySizeInBytes = letSubExp "bytes" <=< toExp <=< arraySizeInBytesExpM

allocForArray' ::
  (MonadBuilder m, Op (Rep m) ~ MemOp inner (Rep m)) =>
  Type ->
  Space ->
  m VName
allocForArray' t space = do
  size <- arraySizeInBytes t
  letExp "mem" $ Op $ Alloc size space

-- | Allocate memory for a value of the given type.
allocForArray ::
  Allocable fromrep torep inner =>
  Type ->
  Space ->
  AllocM fromrep torep VName
allocForArray t space = do
  allocForArray' t space

allocsForStm ::
  (Allocable fromrep torep inner) =>
  [Ident] ->
  Exp torep ->
  AllocM fromrep torep (Stm torep)
allocsForStm idents e = do
  def_space <- askDefaultSpace
  hints <- expHints e
  rts <- expReturns e
  pes <- allocsForPat def_space idents rts hints
  dec <- mkExpDecM (Pat pes) e
  pure $ Let (Pat pes) (defAux dec) e

patWithAllocations ::
  (MonadBuilder m, Mem (Rep m) inner) =>
  Space ->
  [VName] ->
  Exp (Rep m) ->
  [ExpHint] ->
  m (Pat LetDecMem)
patWithAllocations def_space names e hints = do
  ts' <- instantiateShapes' names <$> expExtType e
  let idents = zipWith Ident names ts'
  rts <- expReturns e
  Pat <$> allocsForPat def_space idents rts hints

mkMissingIdents :: MonadFreshNames m => [Ident] -> [ExpReturns] -> m [Ident]
mkMissingIdents idents rts =
  reverse <$> zipWithM f (reverse rts) (map Just (reverse idents) ++ repeat Nothing)
  where
    f _ (Just ident) = pure ident
    f (MemMem space) Nothing = newIdent "ext_mem" $ Mem space
    f _ Nothing = newIdent "ext" $ Prim int64

allocsForPat ::
  (MonadBuilder m, Op (Rep m) ~ MemOp inner (Rep m)) =>
  Space ->
  [Ident] ->
  [ExpReturns] ->
  [ExpHint] ->
  m [PatElem LetDecMem]
allocsForPat def_space some_idents rts hints = do
  idents <- mkMissingIdents some_idents rts

  forM (zip3 idents rts hints) $ \(ident, rt, hint) -> do
    let ident_shape = arrayShape $ identType ident
    case rt of
      MemPrim _ -> do
        summary <- summaryForBindage def_space (identType ident) hint
        pure $ PatElem (identName ident) summary
      MemMem space ->
        pure $ PatElem (identName ident) $ MemMem space
      MemArray bt _ u (Just (ReturnsInBlock mem extixfun)) -> do
        let ixfn = instantiateExtIxFun idents extixfun
        pure . PatElem (identName ident) . MemArray bt ident_shape u $ ArrayIn mem ixfn
      MemArray _ extshape _ Nothing
        | Just _ <- knownShape extshape -> do
            summary <- summaryForBindage def_space (identType ident) hint
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
          error $ "getIdent: Ext " <> show i <> " but pattern has " <> show (length idents) <> " elements: " <> prettyString idents

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
  (MonadBuilder m, Op (Rep m) ~ MemOp inner (Rep m)) =>
  Space ->
  Type ->
  ExpHint ->
  m (MemBound NoUniqueness)
summaryForBindage _ (Prim bt) _ =
  pure $ MemPrim bt
summaryForBindage _ (Mem space) _ =
  pure $ MemMem space
summaryForBindage _ (Acc acc ispace ts u) _ =
  pure $ MemAcc acc ispace ts u
summaryForBindage def_space t@(Array pt shape u) NoHint = do
  m <- allocForArray' t def_space
  pure $ MemArray pt shape u $ ArrayIn m $ IxFun.iota $ map pe64 $ arrayDims t
summaryForBindage _ t@(Array pt _ _) (Hint ixfun space) = do
  bytes <-
    letSubExp "bytes" <=< toExp . untyped $
      product
        [ product $ IxFun.base ixfun,
          fromIntegral (primByteSize pt :: Int64)
        ]
  m <- letExp "mem" $ Op $ Alloc bytes space
  pure $ MemArray pt (arrayShape t) NoUniqueness $ ArrayIn m ixfun

allocInFParams ::
  (Allocable fromrep torep inner) =>
  [(FParam fromrep, Space)] ->
  ([FParam torep] -> AllocM fromrep torep a) ->
  AllocM fromrep torep a
allocInFParams params m = do
  (valparams, (memparams, ctxparams)) <-
    runWriterT $ mapM (uncurry allocInFParam) params
  let params' = memparams <> ctxparams <> valparams
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
      tell ([Param (paramAttrs param) mem $ MemMem pspace], [])
      pure param {paramDec = MemArray pt shape u $ ArrayIn mem ixfun}
    Prim pt ->
      pure param {paramDec = MemPrim pt}
    Mem space ->
      pure param {paramDec = MemMem space}
    Acc acc ispace ts u ->
      pure param {paramDec = MemAcc acc ispace ts u}

ensureRowMajorArray ::
  (Allocable fromrep torep inner) =>
  Maybe Space ->
  VName ->
  AllocM fromrep torep (VName, VName)
ensureRowMajorArray space_ok v = do
  (mem, ixfun) <- lookupArraySummary v
  mem_space <- lookupMemSpace mem
  default_space <- askDefaultSpace
  let space = fromMaybe default_space space_ok
  if numLMADs ixfun == 1
    && ixFunPerm ixfun == [0 .. IxFun.rank ixfun - 1]
    && length (IxFun.base ixfun) == IxFun.rank ixfun
    && maybe True (== mem_space) space_ok
    && IxFun.contiguous ixfun
    then pure (mem, v)
    else allocLinearArray space (baseString v) v

ensureArrayIn ::
  (Allocable fromrep torep inner) =>
  Space ->
  SubExp ->
  WriterT ([SubExp], [SubExp]) (AllocM fromrep torep) SubExp
ensureArrayIn _ (Constant v) =
  error $ "ensureArrayIn: " ++ prettyString v ++ " cannot be an array."
ensureArrayIn space (Var v) = do
  (mem', v') <- lift $ ensureRowMajorArray (Just space) v
  (_, ixfun) <- lift $ lookupArraySummary v'
  ctx <- lift $ mapM (letSubExp "ixfun_arg" <=< toExp) (toList ixfun)
  tell ([Var mem'], ctx)
  pure $ Var v'

allocInMergeParams ::
  (Allocable fromrep torep inner) =>
  [(FParam fromrep, SubExp)] ->
  ( [(FParam torep, SubExp)] ->
    ([SubExp] -> AllocM fromrep torep ([SubExp], [SubExp])) ->
    AllocM fromrep torep a
  ) ->
  AllocM fromrep torep a
allocInMergeParams merge m = do
  ((valparams, valargs, handle_loop_subexps), (mem_params, ctx_params)) <-
    runWriterT $ unzip3 <$> mapM allocInMergeParam merge
  let mergeparams' = mem_params <> ctx_params <> valparams
      summary = scopeOfFParams mergeparams'

      mk_loop_res ses = do
        (ses', (memargs, ctxargs)) <-
          runWriterT $ zipWithM ($) handle_loop_subexps ses
        pure (memargs <> ctxargs, ses')

  (valctx_args, valargs') <- mk_loop_res valargs
  let merge' =
        zip (mem_params <> ctx_params <> valparams) (valctx_args <> valargs')
  localScope summary $ m merge' mk_loop_res
  where
    param_names = namesFromList $ map (paramName . fst) merge
    anyIsLoopParam names = names `namesIntersect` param_names

    scalarRes param_t v_mem_space v_ixfun (Var res) = do
      -- Try really hard to avoid copying needlessly, but the result
      -- _must_ be in ScalarSpace and have the right index function.
      (res_mem, res_ixfun) <- lift $ lookupArraySummary res
      res_mem_space <- lift $ lookupMemSpace res_mem
      (res_mem', res') <-
        if (res_mem_space, res_ixfun) == (v_mem_space, v_ixfun)
          then pure (res_mem, res)
          else lift $ arrayWithIxFun v_mem_space v_ixfun (fromDecl param_t) res
      tell ([Var res_mem'], [])
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
                  space <- lift askDefaultSpace
                  (_, v') <- lift $ allocLinearArray space (baseString v) v
                  allocInMergeParam (mergeparam, Var v')
                else do
                  p <- newParam "mem_param" $ MemMem v_mem_space
                  tell ([p], [])

                  pure
                    ( mergeparam {paramDec = MemArray pt shape u $ ArrayIn (paramName p) v_ixfun},
                      Var v,
                      scalarRes param_t v_mem_space v_ixfun
                    )
            _ -> do
              (v_mem', v') <- lift $ ensureRowMajorArray Nothing v
              (_, v_ixfun') <- lift $ lookupArraySummary v'
              v_mem_space' <- lift $ lookupMemSpace v_mem'

              ctx_params <-
                replicateM (length v_ixfun') $
                  newParam "ctx_param_ext" (MemPrim int64)

              param_ixfun <-
                instantiateIxFun $
                  IxFun.substituteInIxFun
                    ( M.fromList . zip (fmap Ext [0 ..]) $
                        map (le64 . Free . paramName) ctx_params
                    )
                    (IxFun.existentialize v_ixfun')

              mem_param <- newParam "mem_param" $ MemMem v_mem_space'
              tell ([mem_param], ctx_params)
              pure
                ( mergeparam {paramDec = MemArray pt shape u $ ArrayIn (paramName mem_param) param_ixfun},
                  Var v',
                  ensureArrayIn v_mem_space'
                )
    allocInMergeParam (mergeparam, se) = doDefault mergeparam se =<< lift askDefaultSpace

    doDefault mergeparam se space = do
      mergeparam' <- allocInFParam mergeparam space
      pure (mergeparam', se, linearFuncallArg (paramType mergeparam) space)

arrayWithIxFun ::
  (MonadBuilder m, Op (Rep m) ~ MemOp inner (Rep m), LetDec (Rep m) ~ LetDecMem) =>
  Space ->
  IxFun ->
  Type ->
  VName ->
  m (VName, VName)
arrayWithIxFun space ixfun v_t v = do
  let Array pt shape u = v_t
  mem <- allocForArray' v_t space
  v_copy <- newVName $ baseString v <> "_scalcopy"
  letBind (Pat [PatElem v_copy $ MemArray pt shape u $ ArrayIn mem ixfun]) $ BasicOp $ Copy v
  pure (mem, v_copy)

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

allocPermArray ::
  (Allocable fromrep torep inner) =>
  Space ->
  [Int] ->
  String ->
  VName ->
  AllocM fromrep torep (VName, VName)
allocPermArray space perm s v = do
  t <- lookupType v
  case t of
    Array pt shape u -> do
      mem <- allocForArray t space
      v' <- newVName $ s <> "_desired_form"
      let info =
            MemArray pt shape u . ArrayIn mem $
              IxFun.permute (IxFun.iota $ map pe64 $ arrayDims t) perm
          pat = Pat [PatElem v' info]
      addStm $ Let pat (defAux ()) $ BasicOp $ Manifest perm v
      pure (mem, v')
    _ ->
      error $ "allocPermArray: " ++ prettyString t

allocLinearArray ::
  (Allocable fromrep torep inner) =>
  Space ->
  String ->
  VName ->
  AllocM fromrep torep (VName, VName)
allocLinearArray space s v = do
  t <- lookupType v
  let perm = [0 .. arrayRank t - 1]
  allocPermArray space perm s v

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
  tell ([Var mem], [])
  pure $ Var arg'
linearFuncallArg _ _ arg =
  pure arg

explicitAllocationsGeneric ::
  (Allocable fromrep torep inner) =>
  Space ->
  (Op fromrep -> AllocM fromrep torep (Op torep)) ->
  (Exp torep -> AllocM fromrep torep [ExpHint]) ->
  Pass fromrep torep
explicitAllocationsGeneric space handleOp hints =
  Pass "explicit allocations" "Transform program to explicit memory representation" $
    intraproceduralTransformationWithConsts onStms allocInFun
  where
    onStms stms =
      runAllocM space handleOp hints $ collectStms_ $ allocInStms stms $ pure ()

    allocInFun consts (FunDef entry attrs fname rettype params fbody) =
      runAllocM space handleOp hints . inScopeOf consts $
        allocInFParams (zip params $ repeat space) $ \params' -> do
          (fbody', mem_rets) <-
            allocInFunBody (map (const $ Just space) rettype) fbody
          let rettype' = mem_rets ++ memoryInDeclExtType space (length mem_rets) rettype
          pure $ FunDef entry attrs fname rettype' params' fbody'

explicitAllocationsInStmsGeneric ::
  ( MonadFreshNames m,
    HasScope torep m,
    Allocable fromrep torep inner
  ) =>
  Space ->
  (Op fromrep -> AllocM fromrep torep (Op torep)) ->
  (Exp torep -> AllocM fromrep torep [ExpHint]) ->
  Stms fromrep ->
  m (Stms torep)
explicitAllocationsInStmsGeneric space handleOp hints stms = do
  scope <- askScope
  runAllocM space handleOp hints $
    localScope scope $
      collectStms_ $
        allocInStms stms $
          pure ()

memoryInDeclExtType :: Space -> Int -> [DeclExtType] -> [FunReturns]
memoryInDeclExtType space k dets = evalState (mapM addMem dets) 0
  where
    addMem (Prim t) = pure $ MemPrim t
    addMem Mem {} = error "memoryInDeclExtType: too much memory"
    addMem (Array pt shape u) = do
      i <- get <* modify (+ 1)
      let shape' = fmap shift shape
      pure . MemArray pt shape' u . ReturnsNewBlock space i $
        IxFun.iota $
          map convert $
            shapeDims shape'
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
        _ -> error $ "bodyReturnMemCtx: not a memory block: " ++ prettyString mem

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
      let stms_consts = foldMap stmConsts allocstms
          f env = env {envConsts = stms_consts <> envConsts env}
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

numLMADs :: IxFun -> Int
numLMADs = length . IxFun.ixfunLMADs

ixFunPerm :: IxFun -> [Int]
ixFunPerm = map IxFun.ldPerm . IxFun.lmadDims . NE.head . IxFun.ixfunLMADs

ixFunMon :: IxFun -> [IxFun.Monotonicity]
ixFunMon = map IxFun.ldMon . IxFun.lmadDims . NE.head . IxFun.ixfunLMADs

data MemReq
  = MemReq Space [Int] [IxFun.Monotonicity] Rank Bool
  | NeedsLinearisation Space
  deriving (Eq, Show)

combMemReqs :: MemReq -> MemReq -> MemReq
combMemReqs x@NeedsLinearisation {} _ = x
combMemReqs _ y@NeedsLinearisation {} = y
combMemReqs x@(MemReq x_space _ _ _ _) y@MemReq {} =
  if x == y then x else NeedsLinearisation x_space

type MemReqType = MemInfo (Ext SubExp) NoUniqueness MemReq

combMemReqTypes :: MemReqType -> MemReqType -> MemReqType
combMemReqTypes (MemArray pt shape u x) (MemArray _ _ _ y) =
  MemArray pt shape u $ combMemReqs x y
combMemReqTypes x _ = x

contextRets :: MemReqType -> [MemInfo d u r]
contextRets (MemArray _ shape _ (MemReq space _ _ (Rank base_rank) _)) =
  -- Memory + offset + base_rank + (stride,size)*rank.
  MemMem space
    : MemPrim int64
    : replicate base_rank (MemPrim int64)
    ++ replicate (2 * shapeRank shape) (MemPrim int64)
contextRets (MemArray _ shape _ (NeedsLinearisation space)) =
  -- Memory + offset + (base,stride,size)*rank.
  MemMem space
    : MemPrim int64
    : replicate (3 * shapeRank shape) (MemPrim int64)
contextRets _ = []

-- Add memory information to the body, but do not return memory/ixfun
-- information.  Instead, return restrictions on what the index
-- function should look like.  We will then (crudely) unify these
-- restrictions across all bodies.
allocInMatchBody ::
  (Allocable fromrep torep inner) =>
  [ExtType] ->
  Body fromrep ->
  AllocM fromrep torep (Body torep, [MemReqType])
allocInMatchBody rets (Body _ stms res) =
  buildBody . allocInStms stms $ do
    restrictions <- zipWithM restriction rets (map resSubExp res)
    pure (res, restrictions)
  where
    restriction t se = do
      v_info <- subExpMemInfo se
      case (t, v_info) of
        (Array pt shape u, MemArray _ _ _ (ArrayIn mem ixfun)) -> do
          space <- lookupMemSpace mem
          pure . MemArray pt shape u $
            if numLMADs ixfun == 1
              then
                MemReq
                  space
                  (ixFunPerm ixfun)
                  (ixFunMon ixfun)
                  (Rank $ length $ IxFun.base ixfun)
                  (IxFun.contiguous ixfun)
              else NeedsLinearisation space
        (_, MemMem space) -> pure $ MemMem space
        (_, MemPrim pt) -> pure $ MemPrim pt
        (_, MemAcc acc ispace ts u) -> pure $ MemAcc acc ispace ts u
        _ -> error $ "allocInMatchBody: mismatch: " ++ show (t, v_info)

mkBranchRet :: [MemReqType] -> [BranchTypeMem]
mkBranchRet reqs =
  let (ctx_rets, res_rets) = foldl helper ([], []) $ zip reqs offsets
   in ctx_rets ++ res_rets
  where
    numCtxNeeded = length . contextRets

    offsets = scanl (+) 0 $ map numCtxNeeded reqs
    num_new_ctx = last offsets

    helper (ctx_rets_acc, res_rets_acc) (req, ctx_offset) =
      ( ctx_rets_acc ++ contextRets req,
        res_rets_acc ++ [inspect ctx_offset req]
      )

    arrayInfo rank (NeedsLinearisation space) =
      (space, [0 .. rank - 1], repeat IxFun.Inc, rank, True)
    arrayInfo _ (MemReq space perm mon (Rank base_rank) contig) =
      (space, perm, mon, base_rank, contig)

    inspect ctx_offset (MemArray pt shape u req) =
      let shape' = fmap (adjustExt num_new_ctx) shape
          (space, perm, mon, base_rank, contig) = arrayInfo (shapeRank shape) req
       in MemArray pt shape' u . ReturnsNewBlock space ctx_offset $
            convert
              <$> IxFun.mkExistential base_rank (zip perm mon) contig (ctx_offset + 1)
    inspect _ (MemAcc acc ispace ts u) = MemAcc acc ispace ts u
    inspect _ (MemPrim pt) = MemPrim pt
    inspect _ (MemMem space) = MemMem space

    convert (Ext i) = le64 (Ext i)
    convert (Free v) = Free <$> pe64 v

    adjustExt :: Int -> Ext a -> Ext a
    adjustExt _ (Free v) = Free v
    adjustExt k (Ext i) = Ext (k + i)

addCtxToMatchBody ::
  (Allocable fromrep torep inner) =>
  [MemReqType] ->
  Body torep ->
  AllocM fromrep torep (Body torep)
addCtxToMatchBody reqs body = buildBody_ $ do
  res <- zipWithM linearIfNeeded reqs =<< bodyBind body
  ctx <- concat <$> mapM resCtx res
  pure $ ctx ++ res
  where
    linearIfNeeded (MemArray _ _ _ (NeedsLinearisation space)) (SubExpRes cs (Var v)) =
      SubExpRes cs . Var . snd <$> ensureRowMajorArray (Just space) v
    linearIfNeeded _ res =
      pure res

    resCtx (SubExpRes _ Constant {}) =
      pure []
    resCtx (SubExpRes _ (Var v)) = do
      info <- lookupMemInfo v
      case info of
        MemPrim {} -> pure []
        MemAcc {} -> pure []
        MemMem {} -> pure [] -- should not happen
        MemArray _ _ _ (ArrayIn mem ixfun) -> do
          ixfun_exts <- mapM (letSubExp "ixfun_ext" <=< toExp) $ toList ixfun
          pure $ subExpRes (Var mem) : subExpsRes ixfun_exts

-- Do a a simple form of invariance analysis to simplify a Match.  It
-- is unfortunate that we have to do it here, but functions such as
-- scalarRes will look carefully at the index functions before the
-- simplifier has a chance to run.  In a perfect world we would
-- simplify away those copies afterwards. XXX; this should be fixed by
-- a more general copy-removal pass. See
-- Futhark.Optimise.EntryPointMem for a very specialised version of
-- the idea, but which could perhaps be generalised.
simplifyMatch ::
  Mem rep inner =>
  [Case (Body rep)] ->
  Body rep ->
  [BranchTypeMem] ->
  ( [Case (Body rep)],
    Body rep,
    [BranchTypeMem]
  )
simplifyMatch cases defbody ts =
  let case_reses = map (bodyResult . caseBody) cases
      defbody_res = bodyResult defbody
      (ctx_fixes, variant) =
        partitionEithers . map branchInvariant $
          zip4 [0 ..] (transpose case_reses) defbody_res ts
      (cases_reses, defbody_reses, ts') = unzip3 variant
   in ( zipWith onCase cases (transpose cases_reses),
        onBody defbody defbody_reses,
        foldr (uncurry fixExt) ts' ctx_fixes
      )
  where
    bound_in_branches =
      namesFromList . concatMap (patNames . stmPat) $
        foldMap (bodyStms . caseBody) cases <> bodyStms defbody

    onCase c res = fmap (`onBody` res) c
    onBody body res = body {bodyResult = res}

    branchInvariant (i, case_reses, defres, t)
      -- If even one branch has a variant result, then we give up.
      | namesIntersect bound_in_branches $ freeIn $ defres : case_reses =
          Right (case_reses, defres, t)
      -- Do all branches return the same value?
      | all ((== resSubExp defres) . resSubExp) case_reses =
          Left (i, resSubExp defres)
      | otherwise =
          Right (case_reses, defres, t)

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
          (valctx, valres') <- mk_loop_val $ map resSubExp bodyres
          pure $ subExpsRes valctx <> zipWith SubExpRes (map resCerts bodyres) valres'
      pure $ DoLoop merge' form' body'
allocInExp (Apply fname args rettype loc) = do
  args' <- funcallArgs args
  space <- askDefaultSpace
  -- We assume that every array is going to be in its own memory.
  pure $ Apply fname args' (mems space ++ memoryInDeclExtType space num_arrays rettype) loc
  where
    mems space = replicate num_arrays (MemMem space)
    num_arrays = length $ filter ((> 0) . arrayRank . declExtTypeOf) rettype
allocInExp (Match ses cases defbody (MatchDec rets ifsort)) = do
  (defbody', def_reqs) <- allocInMatchBody rets defbody
  (cases', cases_reqs) <- mapAndUnzipM onCase cases
  let reqs = zipWith (foldl combMemReqTypes) def_reqs (transpose cases_reqs)
  defbody'' <- addCtxToMatchBody reqs defbody'
  cases'' <- mapM (traverse $ addCtxToMatchBody reqs) cases'
  let (cases''', defbody''', rets') =
        simplifyMatch cases'' defbody'' $ mkBranchRet reqs
  pure $ Match ses cases''' defbody''' $ MatchDec rets' ifsort
  where
    onCase (Case vs body) = first (Case vs) <$> allocInMatchBody rets body
allocInExp (WithAcc inputs bodylam) =
  WithAcc <$> mapM onInput inputs <*> onLambda bodylam
  where
    onLambda lam = do
      params <- forM (lambdaParams lam) $ \(Param attrs pv t) ->
        case t of
          Prim Unit -> pure $ Param attrs pv $ MemPrim Unit
          Acc acc ispace ts u -> pure $ Param attrs pv $ MemAcc acc ispace ts u
          _ -> error $ "Unexpected WithAcc lambda param: " ++ prettyString (Param attrs pv t)
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
        fmap pe64 $
          Slice $
            is ++ map sliceDim (shapeDims shape)

    onXParam _ (Param attrs p (Prim t)) _ =
      pure $ Param attrs p (MemPrim t)
    onXParam is (Param attrs p (Array pt shape u)) arr = do
      (mem, ixfun) <- lookupArraySummary arr
      pure $ mkP attrs p pt shape u mem ixfun is
    onXParam _ p _ =
      error $ "Cannot handle MkAcc param: " ++ prettyString p

    onYParam _ (Param attrs p (Prim t)) _ =
      pure $ Param attrs p $ MemPrim t
    onYParam is (Param attrs p (Array pt shape u)) arr = do
      arr_t <- lookupType arr
      space <- askDefaultSpace
      mem <- allocForArray arr_t space
      let base_dims = map pe64 $ arrayDims arr_t
          ixfun = IxFun.iota base_dims
      pure $ mkP attrs p pt shape u mem ixfun is
    onYParam _ p _ =
      error $ "Cannot handle MkAcc param: " ++ prettyString p
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
          let ixfun' = IxFun.slice ixfun $ fullSliceNum dims [DimFix $ le64 i]
          pure (p {paramDec = MemArray pt shape u $ ArrayIn mem ixfun'}, a)
        Prim bt ->
          pure (p {paramDec = MemPrim bt}, a)
        Mem space ->
          pure (p {paramDec = MemMem space}, a)
        Acc acc ispace ts u ->
          pure (p {paramDec = MemAcc acc ispace ts u}, a)

class SizeSubst op where
  opIsConst :: op -> Bool
  opIsConst = const False

instance SizeSubst (NoOp rep)

instance SizeSubst (op rep) => SizeSubst (MemOp op rep) where
  opIsConst (Inner op) = opIsConst op
  opIsConst _ = False

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
  Space ->
  ExpDec (Rep m) ->
  [VName] ->
  Exp (Rep m) ->
  m (Stm (Rep m))
mkLetNamesB' space dec names e = do
  pat <- patWithAllocations space names e nohints
  pure $ Let pat (defAux dec) e
  where
    nohints = map (const NoHint) names

mkLetNamesB'' ::
  ( Mem rep inner,
    LetDec rep ~ LetDecMem,
    OpReturns (inner (Engine.Wise rep)),
    ExpDec rep ~ (),
    Rep m ~ Engine.Wise rep,
    HasScope (Engine.Wise rep) m,
    MonadBuilder m,
    AliasedOp (inner (Engine.Wise rep)),
    RephraseOp (MemOp inner),
    Engine.CanBeWise inner
  ) =>
  Space ->
  [VName] ->
  Exp (Engine.Wise rep) ->
  m (Stm (Engine.Wise rep))
mkLetNamesB'' space names e = do
  pat <- patWithAllocations space names e nohints
  let pat' = Engine.addWisdomToPat pat e
      dec = Engine.mkWiseExpDec pat' () e
  pure $ Let pat' (defAux dec) e
  where
    nohints = map (const NoHint) names

simplifyMemOp ::
  Engine.SimplifiableRep rep =>
  ( inner (Engine.Wise rep) ->
    Engine.SimpleM rep (inner (Engine.Wise rep), Stms (Engine.Wise rep))
  ) ->
  MemOp inner (Engine.Wise rep) ->
  Engine.SimpleM rep (MemOp inner (Engine.Wise rep), Stms (Engine.Wise rep))
simplifyMemOp _ (Alloc size space) =
  (,) <$> (Alloc <$> Engine.simplify size <*> pure space) <*> pure mempty
simplifyMemOp onInner (Inner k) = do
  (k', hoisted) <- onInner k
  pure (Inner k', hoisted)

simplifiable ::
  ( Engine.SimplifiableRep rep,
    LetDec rep ~ LetDecMem,
    ExpDec rep ~ (),
    BodyDec rep ~ (),
    Mem (Engine.Wise rep) inner,
    Engine.CanBeWise inner,
    RephraseOp inner,
    IsOp (inner rep),
    OpReturns (inner (Engine.Wise rep)),
    AliasedOp (inner (Engine.Wise rep)),
    IndexOp (inner (Engine.Wise rep))
  ) =>
  (inner (Engine.Wise rep) -> UT.UsageTable) ->
  ( inner (Engine.Wise rep) ->
    Engine.SimpleM rep (inner (Engine.Wise rep), Stms (Engine.Wise rep))
  ) ->
  SimpleOps rep
simplifiable innerUsage simplifyInnerOp =
  SimpleOps mkExpDecS' mkBodyS' protectOp opUsage simplifyPat (simplifyMemOp simplifyInnerOp)
  where
    mkExpDecS' _ pat e =
      pure $ Engine.mkWiseExpDec pat () e

    mkBodyS' _ stms res = pure $ mkWiseBody () stms res

    protectOp taken pat (Alloc size space) = Just $ do
      tbody <- resultBodyM [size]
      fbody <- resultBodyM [intConst Int64 0]
      size' <-
        letSubExp "hoisted_alloc_size" $
          Match [taken] [Case [Just $ BoolValue True] tbody] fbody $
            MatchDec [MemPrim int64] MatchFallback
      letBind pat $ Op $ Alloc size' space
    protectOp _ _ _ = Nothing

    opUsage (Alloc (Var size) _) =
      UT.sizeUsage size
    opUsage (Alloc _ _) =
      mempty
    opUsage (Inner inner) =
      innerUsage inner

    simplifyPat (Pat pes) e = do
      rets <- expReturns e
      Pat <$> zipWithM update pes rets
      where
        names = map patElemName pes
        update
          (PatElem pe_v (MemArray pt shape u (ArrayIn mem _)))
          (MemArray _ _ _ (Just (ReturnsInBlock _ ixfun)))
            | Just ixfun' <- traverse (traverse inst) ixfun =
                PatElem pe_v
                  <$> ( MemArray pt
                          <$> Engine.simplify shape
                          <*> pure u
                          <*> (ArrayIn <$> Engine.simplify mem <*> pure ixfun')
                      )
            where
              inst (Ext i) = maybeNth i names
              inst (Free v) = Just v
        update pe _ = traverse Engine.simplify pe

data ExpHint
  = NoHint
  | Hint IxFun Space

defaultExpHints :: (ASTRep rep, HasScope rep m) => Exp rep -> m [ExpHint]
defaultExpHints e = map (const NoHint) <$> expExtType e
