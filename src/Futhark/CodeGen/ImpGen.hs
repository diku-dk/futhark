{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.CodeGen.ImpGen
  ( -- * Entry Points
    compileProg,

    -- * Pluggable Compiler
    OpCompiler,
    ExpCompiler,
    CopyCompiler,
    StmsCompiler,
    AllocCompiler,
    Operations (..),
    defaultOperations,
    MemLoc (..),
    sliceMemLoc,
    MemEntry (..),
    ScalarEntry (..),

    -- * Monadic Compiler Interface
    ImpM,
    localDefaultSpace,
    askFunction,
    newVNameForFun,
    nameForFun,
    askEnv,
    localEnv,
    localOps,
    VTable,
    getVTable,
    localVTable,
    subImpM,
    subImpM_,
    emit,
    emitFunction,
    hasFunction,
    collect,
    collect',
    comment,
    VarEntry (..),
    ArrayEntry (..),

    -- * Lookups
    lookupVar,
    lookupArray,
    lookupMemory,
    lookupAcc,

    -- * Building Blocks
    TV,
    mkTV,
    tvSize,
    tvExp,
    tvVar,
    ToExp (..),
    compileAlloc,
    everythingVolatile,
    compileBody,
    compileBody',
    compileLoopBody,
    defCompileStms,
    compileStms,
    compileExp,
    defCompileExp,
    fullyIndexArray,
    fullyIndexArray',
    copy,
    copyDWIM,
    copyDWIMFix,
    copyElementWise,
    typeSize,
    inBounds,
    isMapTransposeCopy,

    -- * Constructing code.
    dLParams,
    dFParams,
    addLoopVar,
    dScope,
    dArray,
    dPrim,
    dPrimVol,
    dPrim_,
    dPrimV_,
    dPrimV,
    dPrimVE,
    dIndexSpace,
    dIndexSpace',
    sFor,
    sWhile,
    sComment,
    sIf,
    sWhen,
    sUnless,
    sOp,
    sDeclareMem,
    sAlloc,
    sAlloc_,
    sArray,
    sArrayInMem,
    sAllocArray,
    sAllocArrayPerm,
    sStaticArray,
    sWrite,
    sUpdate,
    sLoopNest,
    (<--),
    (<~~),
    function,
    warn,
    module Language.Futhark.Warnings,
  )
where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Parallel.Strategies
import Data.Bifunctor (first)
import qualified Data.DList as DL
import Data.Either
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.String
import Futhark.CodeGen.ImpCode
  ( Bytes,
    Count,
    Elements,
    bytes,
    elements,
    withElemType,
  )
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.CodeGen.ImpGen.Transpose
import Futhark.Construct hiding (ToExp (..))
import Futhark.IR.Mem
import qualified Futhark.IR.Mem.IxFun as IxFun
import Futhark.IR.SOACS (SOACS)
import Futhark.Util
import Futhark.Util.IntegralExp
import Futhark.Util.Loc (noLoc)
import Language.Futhark.Warnings
import Prelude hiding (quot)

-- | How to compile an t'Op'.
type OpCompiler rep r op = Pat rep -> Op rep -> ImpM rep r op ()

-- | How to compile some 'Stms'.
type StmsCompiler rep r op = Names -> Stms rep -> ImpM rep r op () -> ImpM rep r op ()

-- | How to compile an 'Exp'.
type ExpCompiler rep r op = Pat rep -> Exp rep -> ImpM rep r op ()

type CopyCompiler rep r op =
  PrimType ->
  MemLoc ->
  MemLoc ->
  ImpM rep r op ()

-- | An alternate way of compiling an allocation.
type AllocCompiler rep r op = VName -> Count Bytes (Imp.TExp Int64) -> ImpM rep r op ()

data Operations rep r op = Operations
  { opsExpCompiler :: ExpCompiler rep r op,
    opsOpCompiler :: OpCompiler rep r op,
    opsStmsCompiler :: StmsCompiler rep r op,
    opsCopyCompiler :: CopyCompiler rep r op,
    opsAllocCompilers :: M.Map Space (AllocCompiler rep r op)
  }

-- | An operations set for which the expression compiler always
-- returns 'defCompileExp'.
defaultOperations ::
  (Mem rep inner, FreeIn op) =>
  OpCompiler rep r op ->
  Operations rep r op
defaultOperations opc =
  Operations
    { opsExpCompiler = defCompileExp,
      opsOpCompiler = opc,
      opsStmsCompiler = defCompileStms,
      opsCopyCompiler = defaultCopy,
      opsAllocCompilers = mempty
    }

-- | When an array is declared, this is where it is stored.
data MemLoc = MemLoc
  { memLocName :: VName,
    memLocShape :: [Imp.DimSize],
    memLocIxFun :: IxFun.IxFun (Imp.TExp Int64)
  }
  deriving (Eq, Show)

sliceMemLoc :: MemLoc -> Slice (Imp.TExp Int64) -> MemLoc
sliceMemLoc (MemLoc mem shape ixfun) slice =
  MemLoc mem shape $ IxFun.slice ixfun slice

flatSliceMemLoc :: MemLoc -> FlatSlice (Imp.TExp Int64) -> MemLoc
flatSliceMemLoc (MemLoc mem shape ixfun) slice =
  MemLoc mem shape $ IxFun.flatSlice ixfun slice

data ArrayEntry = ArrayEntry
  { entryArrayLoc :: MemLoc,
    entryArrayElemType :: PrimType
  }
  deriving (Show)

entryArrayShape :: ArrayEntry -> [Imp.DimSize]
entryArrayShape = memLocShape . entryArrayLoc

newtype MemEntry = MemEntry {entryMemSpace :: Imp.Space}
  deriving (Show)

newtype ScalarEntry = ScalarEntry
  { entryScalarType :: PrimType
  }
  deriving (Show)

-- | Every non-scalar variable must be associated with an entry.
data VarEntry rep
  = ArrayVar (Maybe (Exp rep)) ArrayEntry
  | ScalarVar (Maybe (Exp rep)) ScalarEntry
  | MemVar (Maybe (Exp rep)) MemEntry
  | AccVar (Maybe (Exp rep)) (VName, Shape, [Type])
  deriving (Show)

data ValueDestination
  = ScalarDestination VName
  | MemoryDestination VName
  | -- | The 'MemLoc' is 'Just' if a copy if
    -- required.  If it is 'Nothing', then a
    -- copy/assignment of a memory block somewhere
    -- takes care of this array.
    ArrayDestination (Maybe MemLoc)
  deriving (Show)

data Env rep r op = Env
  { envExpCompiler :: ExpCompiler rep r op,
    envStmsCompiler :: StmsCompiler rep r op,
    envOpCompiler :: OpCompiler rep r op,
    envCopyCompiler :: CopyCompiler rep r op,
    envAllocCompilers :: M.Map Space (AllocCompiler rep r op),
    envDefaultSpace :: Imp.Space,
    envVolatility :: Imp.Volatility,
    -- | User-extensible environment.
    envEnv :: r,
    -- | Name of the function we are compiling, if any.
    envFunction :: Maybe Name,
    -- | The set of attributes that are active on the enclosing
    -- statements (including the one we are currently compiling).
    envAttrs :: Attrs
  }

newEnv :: r -> Operations rep r op -> Imp.Space -> Env rep r op
newEnv r ops ds =
  Env
    { envExpCompiler = opsExpCompiler ops,
      envStmsCompiler = opsStmsCompiler ops,
      envOpCompiler = opsOpCompiler ops,
      envCopyCompiler = opsCopyCompiler ops,
      envAllocCompilers = mempty,
      envDefaultSpace = ds,
      envVolatility = Imp.Nonvolatile,
      envEnv = r,
      envFunction = Nothing,
      envAttrs = mempty
    }

-- | The symbol table used during compilation.
type VTable rep = M.Map VName (VarEntry rep)

data ImpState rep r op = ImpState
  { stateVTable :: VTable rep,
    stateFunctions :: Imp.Functions op,
    stateCode :: Imp.Code op,
    stateWarnings :: Warnings,
    -- | Maps the arrays backing each accumulator to their
    -- update function and neutral elements.  This works
    -- because an array name can only become part of a single
    -- accumulator throughout its lifetime.  If the arrays
    -- backing an accumulator is not in this mapping, the
    -- accumulator is scatter-like.
    stateAccs :: M.Map VName ([VName], Maybe (Lambda rep, [SubExp])),
    stateNameSource :: VNameSource
  }

newState :: VNameSource -> ImpState rep r op
newState = ImpState mempty mempty mempty mempty mempty

newtype ImpM rep r op a
  = ImpM (ReaderT (Env rep r op) (State (ImpState rep r op)) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState (ImpState rep r op),
      MonadReader (Env rep r op)
    )

instance MonadFreshNames (ImpM rep r op) where
  getNameSource = gets stateNameSource
  putNameSource src = modify $ \s -> s {stateNameSource = src}

-- Cannot be an KernelsMem scope because the index functions have
-- the wrong leaves (VName instead of Imp.Exp).
instance HasScope SOACS (ImpM rep r op) where
  askScope = gets $ M.map (LetName . entryType) . stateVTable
    where
      entryType (MemVar _ memEntry) =
        Mem (entryMemSpace memEntry)
      entryType (ArrayVar _ arrayEntry) =
        Array
          (entryArrayElemType arrayEntry)
          (Shape $ entryArrayShape arrayEntry)
          NoUniqueness
      entryType (ScalarVar _ scalarEntry) =
        Prim $ entryScalarType scalarEntry
      entryType (AccVar _ (acc, ispace, ts)) =
        Acc acc ispace ts NoUniqueness

runImpM ::
  ImpM rep r op a ->
  r ->
  Operations rep r op ->
  Imp.Space ->
  ImpState rep r op ->
  (a, ImpState rep r op)
runImpM (ImpM m) r ops space = runState (runReaderT m $ newEnv r ops space)

subImpM_ ::
  r' ->
  Operations rep r' op' ->
  ImpM rep r' op' a ->
  ImpM rep r op (Imp.Code op')
subImpM_ r ops m = snd <$> subImpM r ops m

subImpM ::
  r' ->
  Operations rep r' op' ->
  ImpM rep r' op' a ->
  ImpM rep r op (a, Imp.Code op')
subImpM r ops (ImpM m) = do
  env <- ask
  s <- get

  let env' =
        env
          { envExpCompiler = opsExpCompiler ops,
            envStmsCompiler = opsStmsCompiler ops,
            envCopyCompiler = opsCopyCompiler ops,
            envOpCompiler = opsOpCompiler ops,
            envAllocCompilers = opsAllocCompilers ops,
            envEnv = r
          }
      s' =
        ImpState
          { stateVTable = stateVTable s,
            stateFunctions = mempty,
            stateCode = mempty,
            stateNameSource = stateNameSource s,
            stateWarnings = mempty,
            stateAccs = stateAccs s
          }
      (x, s'') = runState (runReaderT m env') s'

  putNameSource $ stateNameSource s''
  warnings $ stateWarnings s''
  return (x, stateCode s'')

-- | Execute a code generation action, returning the code that was
-- emitted.
collect :: ImpM rep r op () -> ImpM rep r op (Imp.Code op)
collect = fmap snd . collect'

collect' :: ImpM rep r op a -> ImpM rep r op (a, Imp.Code op)
collect' m = do
  prev_code <- gets stateCode
  modify $ \s -> s {stateCode = mempty}
  x <- m
  new_code <- gets stateCode
  modify $ \s -> s {stateCode = prev_code}
  return (x, new_code)

-- | Execute a code generation action, wrapping the generated code
-- within a 'Imp.Comment' with the given description.
comment :: String -> ImpM rep r op () -> ImpM rep r op ()
comment desc m = do
  code <- collect m
  emit $ Imp.Comment desc code

-- | Emit some generated imperative code.
emit :: Imp.Code op -> ImpM rep r op ()
emit code = modify $ \s -> s {stateCode = stateCode s <> code}

warnings :: Warnings -> ImpM rep r op ()
warnings ws = modify $ \s -> s {stateWarnings = ws <> stateWarnings s}

-- | Emit a warning about something the user should be aware of.
warn :: Located loc => loc -> [loc] -> String -> ImpM rep r op ()
warn loc locs problem =
  warnings $ singleWarning' (srclocOf loc) (map srclocOf locs) (fromString problem)

-- | Emit a function in the generated code.
emitFunction :: Name -> Imp.Function op -> ImpM rep r op ()
emitFunction fname fun = do
  Imp.Functions fs <- gets stateFunctions
  modify $ \s -> s {stateFunctions = Imp.Functions $ (fname, fun) : fs}

-- | Check if a function of a given name exists.
hasFunction :: Name -> ImpM rep r op Bool
hasFunction fname = gets $ \s ->
  let Imp.Functions fs = stateFunctions s
   in isJust $ lookup fname fs

constsVTable :: Mem rep inner => Stms rep -> VTable rep
constsVTable = foldMap stmVtable
  where
    stmVtable (Let pat _ e) =
      foldMap (peVtable e) $ patElems pat
    peVtable e (PatElem name dec) =
      M.singleton name $ memBoundToVarEntry (Just e) $ letDecMem dec

compileProg ::
  (Mem rep inner, FreeIn op, MonadFreshNames m) =>
  r ->
  Operations rep r op ->
  Imp.Space ->
  Prog rep ->
  m (Warnings, Imp.Definitions op)
compileProg r ops space (Prog consts funs) =
  modifyNameSource $ \src ->
    let (_, ss) =
          unzip $ parMap rpar (compileFunDef' src) funs
        free_in_funs =
          freeIn $ mconcat $ map stateFunctions ss
        (consts', s') =
          runImpM (compileConsts free_in_funs consts) r ops space $
            combineStates ss
     in ( ( stateWarnings s',
            Imp.Definitions consts' (stateFunctions s')
          ),
          stateNameSource s'
        )
  where
    compileFunDef' src fdef =
      runImpM
        (compileFunDef fdef)
        r
        ops
        space
        (newState src) {stateVTable = constsVTable consts}

    combineStates ss =
      let Imp.Functions funs' = mconcat $ map stateFunctions ss
          src = mconcat (map stateNameSource ss)
       in (newState src)
            { stateFunctions =
                Imp.Functions $ M.toList $ M.fromList funs',
              stateWarnings =
                mconcat $ map stateWarnings ss
            }

compileConsts :: Names -> Stms rep -> ImpM rep r op (Imp.Constants op)
compileConsts used_consts stms = do
  code <- collect $ compileStms used_consts stms $ pure ()
  pure $ uncurry Imp.Constants $ first DL.toList $ extract code
  where
    -- Fish out those top-level declarations in the constant
    -- initialisation code that are free in the functions.
    extract (x Imp.:>>: y) =
      extract x <> extract y
    extract (Imp.DeclareMem name space)
      | name `nameIn` used_consts =
        ( DL.singleton $ Imp.MemParam name space,
          mempty
        )
    extract (Imp.DeclareScalar name _ t)
      | name `nameIn` used_consts =
        ( DL.singleton $ Imp.ScalarParam name t,
          mempty
        )
    extract s =
      (mempty, s)

compileInParam ::
  Mem rep inner =>
  FParam rep ->
  ImpM rep r op (Either Imp.Param ArrayDecl)
compileInParam fparam = case paramDec fparam of
  MemPrim bt ->
    pure $ Left $ Imp.ScalarParam name bt
  MemMem space ->
    pure $ Left $ Imp.MemParam name space
  MemArray bt shape _ (ArrayIn mem ixfun) ->
    pure $ Right $ ArrayDecl name bt $ MemLoc mem (shapeDims shape) ixfun
  MemAcc {} ->
    error "Functions may not have accumulator parameters."
  where
    name = paramName fparam

data ArrayDecl = ArrayDecl VName PrimType MemLoc

compileInParams ::
  Mem rep inner =>
  [FParam rep] ->
  [EntryParam] ->
  ImpM rep r op ([Imp.Param], [ArrayDecl], [(Name, Imp.ExternalValue)])
compileInParams params eparams = do
  let (ctx_params, val_params) =
        splitAt (length params - sum (map (entryPointSize . entryParamType) eparams)) params
  (inparams, arrayds) <- partitionEithers <$> mapM compileInParam (ctx_params ++ val_params)
  let findArray x = find (isArrayDecl x) arrayds

      summaries = M.fromList $ mapMaybe memSummary params
        where
          memSummary param
            | MemMem space <- paramDec param =
              Just (paramName param, space)
            | otherwise =
              Nothing

      findMemInfo :: VName -> Maybe Space
      findMemInfo = flip M.lookup summaries

      mkValueDesc fparam signedness =
        case (findArray $ paramName fparam, paramType fparam) of
          (Just (ArrayDecl _ bt (MemLoc mem shape _)), _) -> do
            memspace <- findMemInfo mem
            Just $ Imp.ArrayValue mem memspace bt signedness shape
          (_, Prim bt) ->
            Just $ Imp.ScalarValue bt signedness $ paramName fparam
          _ ->
            Nothing

      mkExts (EntryParam v (TypeOpaque u desc n) : epts) fparams =
        let (fparams', rest) = splitAt n fparams
         in ( v,
              Imp.OpaqueValue
                u
                desc
                (mapMaybe (`mkValueDesc` Imp.TypeDirect) fparams')
            ) :
            mkExts epts rest
      mkExts (EntryParam v (TypeUnsigned u) : epts) (fparam : fparams) =
        maybeToList ((v,) . Imp.TransparentValue u <$> mkValueDesc fparam Imp.TypeUnsigned)
          ++ mkExts epts fparams
      mkExts (EntryParam v (TypeDirect u) : epts) (fparam : fparams) =
        maybeToList ((v,) . Imp.TransparentValue u <$> mkValueDesc fparam Imp.TypeDirect)
          ++ mkExts epts fparams
      mkExts _ _ = []

  return (inparams, arrayds, mkExts eparams val_params)
  where
    isArrayDecl x (ArrayDecl y _ _) = x == y

compileOutParam ::
  FunReturns -> ImpM rep r op (Maybe Imp.Param, ValueDestination)
compileOutParam (MemPrim t) = do
  name <- newVName "prim_out"
  pure (Just $ Imp.ScalarParam name t, ScalarDestination name)
compileOutParam (MemMem space) = do
  name <- newVName "mem_out"
  pure (Just $ Imp.MemParam name space, MemoryDestination name)
compileOutParam MemArray {} =
  pure (Nothing, ArrayDestination Nothing)
compileOutParam MemAcc {} =
  error "Functions may not return accumulators."

compileExternalValues ::
  Mem rep inner =>
  [RetType rep] ->
  [EntryPointType] ->
  [Maybe Imp.Param] ->
  ImpM rep r op [Imp.ExternalValue]
compileExternalValues orig_rts orig_epts maybe_params = do
  let (ctx_rts, val_rts) =
        splitAt (length orig_rts - sum (map entryPointSize orig_epts)) orig_rts

  let nthOut i = case maybeNth i maybe_params of
        Just (Just p) -> Imp.paramName p
        Just Nothing -> error $ "Output " ++ show i ++ " not a param."
        Nothing -> error $ "Param " ++ show i ++ " does not exist."

      mkValueDesc _ signedness (MemArray t shape _ ret) = do
        (mem, space) <-
          case ret of
            ReturnsNewBlock space j _ixfun ->
              pure (nthOut j, space)
            ReturnsInBlock mem _ixfun -> do
              space <- entryMemSpace <$> lookupMemory mem
              pure (mem, space)
        pure $ Imp.ArrayValue mem space t signedness $ map f $ shapeDims shape
        where
          f (Free v) = v
          f (Ext i) = Var $ nthOut i
      mkValueDesc i signedness (MemPrim bt) =
        pure $ Imp.ScalarValue bt signedness $ nthOut i
      mkValueDesc _ _ MemAcc {} =
        error "mkValueDesc: unexpected MemAcc output."
      mkValueDesc _ _ MemMem {} =
        error "mkValueDesc: unexpected MemMem output."

      mkExts i (TypeOpaque u desc n : epts) rets = do
        let (rets', rest) = splitAt n rets
        vds <- zipWithM (`mkValueDesc` Imp.TypeDirect) [i ..] rets'
        (Imp.OpaqueValue u desc vds :) <$> mkExts (i + n) epts rest
      mkExts i (TypeUnsigned u : epts) (ret : rets) = do
        vd <- mkValueDesc i Imp.TypeUnsigned ret
        (Imp.TransparentValue u vd :) <$> mkExts (i + 1) epts rets
      mkExts i (TypeDirect u : epts) (ret : rets) = do
        vd <- mkValueDesc i Imp.TypeDirect ret
        (Imp.TransparentValue u vd :) <$> mkExts (i + 1) epts rets
      mkExts _ _ _ = pure []

  mkExts (length ctx_rts) orig_epts val_rts

compileOutParams ::
  Mem rep inner =>
  [RetType rep] ->
  Maybe [EntryPointType] ->
  ImpM rep r op ([Imp.ExternalValue], [Imp.Param], [ValueDestination])
compileOutParams orig_rts maybe_orig_epts = do
  (maybe_params, dests) <- unzip <$> mapM compileOutParam orig_rts
  evs <- case maybe_orig_epts of
    Just orig_epts -> compileExternalValues orig_rts orig_epts maybe_params
    Nothing -> pure []
  return (evs, catMaybes maybe_params, dests)

compileFunDef ::
  Mem rep inner =>
  FunDef rep ->
  ImpM rep r op ()
compileFunDef (FunDef entry _ fname rettype params body) =
  local (\env -> env {envFunction = name_entry `mplus` Just fname}) $ do
    ((outparams, inparams, results, args), body') <- collect' compile
    emitFunction fname $ Imp.Function name_entry outparams inparams body' results args
  where
    (name_entry, params_entry, ret_entry) = case entry of
      Nothing ->
        ( Nothing,
          replicate (length params) (EntryParam "" $ TypeDirect mempty),
          Nothing
        )
      Just (x, y, z) -> (Just x, y, Just z)
    compile = do
      (inparams, arrayds, args) <- compileInParams params params_entry
      (results, outparams, dests) <- compileOutParams rettype ret_entry
      addFParams params
      addArrays arrayds

      let Body _ stms ses = body
      compileStms (freeIn ses) stms $
        forM_ (zip dests ses) $ \(d, SubExpRes _ se) -> copyDWIMDest d [] se []

      return (outparams, inparams, results, args)

compileBody :: Pat rep -> Body rep -> ImpM rep r op ()
compileBody pat (Body _ stms ses) = do
  dests <- destinationFromPat pat
  compileStms (freeIn ses) stms $
    forM_ (zip dests ses) $ \(d, SubExpRes _ se) -> copyDWIMDest d [] se []

compileBody' :: [Param dec] -> Body rep -> ImpM rep r op ()
compileBody' params (Body _ stms ses) =
  compileStms (freeIn ses) stms $
    forM_ (zip params ses) $ \(param, SubExpRes _ se) -> copyDWIM (paramName param) [] se []

compileLoopBody :: Typed dec => [Param dec] -> Body rep -> ImpM rep r op ()
compileLoopBody mergeparams (Body _ stms ses) = do
  -- We cannot write the results to the merge parameters immediately,
  -- as some of the results may actually *be* merge parameters, and
  -- would thus be clobbered.  Therefore, we first copy to new
  -- variables mirroring the merge parameters, and then copy this
  -- buffer to the merge parameters.  This is efficient, because the
  -- operations are all scalar operations.
  tmpnames <- mapM (newVName . (++ "_tmp") . baseString . paramName) mergeparams
  compileStms (freeIn ses) stms $ do
    copy_to_merge_params <- forM (zip3 mergeparams tmpnames ses) $ \(p, tmp, SubExpRes _ se) ->
      case typeOf p of
        Prim pt -> do
          emit $ Imp.DeclareScalar tmp Imp.Nonvolatile pt
          emit $ Imp.SetScalar tmp $ toExp' pt se
          return $ emit $ Imp.SetScalar (paramName p) $ Imp.var tmp pt
        Mem space | Var v <- se -> do
          emit $ Imp.DeclareMem tmp space
          emit $ Imp.SetMem tmp v space
          return $ emit $ Imp.SetMem (paramName p) tmp space
        _ -> return $ return ()
    sequence_ copy_to_merge_params

compileStms :: Names -> Stms rep -> ImpM rep r op () -> ImpM rep r op ()
compileStms alive_after_stms all_stms m = do
  cb <- asks envStmsCompiler
  cb alive_after_stms all_stms m

defCompileStms ::
  (Mem rep inner, FreeIn op) =>
  Names ->
  Stms rep ->
  ImpM rep r op () ->
  ImpM rep r op ()
defCompileStms alive_after_stms all_stms m =
  -- We keep track of any memory blocks produced by the statements,
  -- and after the last time that memory block is used, we insert a
  -- Free.  This is very conservative, but can cut down on lifetimes
  -- in some cases.
  void $ compileStms' mempty $ stmsToList all_stms
  where
    compileStms' allocs (Let pat aux e : bs) = do
      dVars (Just e) (patElems pat)

      e_code <-
        localAttrs (stmAuxAttrs aux) $
          collect $ compileExp pat e
      (live_after, bs_code) <- collect' $ compileStms' (patternAllocs pat <> allocs) bs
      let dies_here v =
            not (v `nameIn` live_after)
              && v `nameIn` freeIn e_code
          to_free = S.filter (dies_here . fst) allocs

      emit e_code
      mapM_ (emit . uncurry Imp.Free) to_free
      emit bs_code

      return $ freeIn e_code <> live_after
    compileStms' _ [] = do
      code <- collect m
      emit code
      return $ freeIn code <> alive_after_stms

    patternAllocs = S.fromList . mapMaybe isMemPatElem . patElems
    isMemPatElem pe = case patElemType pe of
      Mem space -> Just (patElemName pe, space)
      _ -> Nothing

compileExp :: Pat rep -> Exp rep -> ImpM rep r op ()
compileExp pat e = do
  ec <- asks envExpCompiler
  ec pat e

defCompileExp ::
  (Mem rep inner) =>
  Pat rep ->
  Exp rep ->
  ImpM rep r op ()
defCompileExp pat (If cond tbranch fbranch _) =
  sIf (toBoolExp cond) (compileBody pat tbranch) (compileBody pat fbranch)
defCompileExp pat (Apply fname args _ _) = do
  dest <- destinationFromPat pat
  targets <- funcallTargets dest
  args' <- catMaybes <$> mapM compileArg args
  emit $ Imp.Call targets fname args'
  where
    compileArg (se, _) = do
      t <- subExpType se
      case (se, t) of
        (_, Prim pt) -> return $ Just $ Imp.ExpArg $ toExp' pt se
        (Var v, Mem {}) -> return $ Just $ Imp.MemArg v
        _ -> return Nothing
defCompileExp pat (BasicOp op) = defCompileBasicOp pat op
defCompileExp pat (DoLoop merge form body) = do
  attrs <- askAttrs
  when ("unroll" `inAttrs` attrs) $
    warn (noLoc :: SrcLoc) [] "#[unroll] on loop with unknown number of iterations." -- FIXME: no location.
  dFParams params
  forM_ merge $ \(p, se) ->
    when ((== 0) $ arrayRank $ paramType p) $
      copyDWIM (paramName p) [] se []

  let doBody = compileLoopBody params body

  case form of
    ForLoop i _ bound loopvars -> do
      let setLoopParam (p, a)
            | Prim _ <- paramType p =
              copyDWIM (paramName p) [] (Var a) [DimFix $ Imp.le64 i]
            | otherwise =
              return ()

      bound' <- toExp bound

      dLParams $ map fst loopvars
      sFor' i bound' $
        mapM_ setLoopParam loopvars >> doBody
    WhileLoop cond ->
      sWhile (TPrimExp $ Imp.var cond Bool) doBody

  pat_dests <- destinationFromPat pat
  forM_ (zip pat_dests $ map (Var . paramName . fst) merge) $ \(d, r) ->
    copyDWIMDest d [] r []
  where
    params = map fst merge
defCompileExp pat (WithAcc inputs lam) = do
  dLParams $ lambdaParams lam
  forM_ (zip inputs $ lambdaParams lam) $ \((_, arrs, op), p) ->
    modify $ \s ->
      s {stateAccs = M.insert (paramName p) (arrs, op) $ stateAccs s}
  compileStms mempty (bodyStms $ lambdaBody lam) $ do
    let nonacc_res = drop num_accs (bodyResult (lambdaBody lam))
        nonacc_pat_names = takeLast (length nonacc_res) (patNames pat)
    forM_ (zip nonacc_pat_names nonacc_res) $ \(v, SubExpRes _ se) ->
      copyDWIM v [] se []
  where
    num_accs = length inputs
defCompileExp pat (Op op) = do
  opc <- asks envOpCompiler
  opc pat op

tracePrim :: String -> PrimType -> SubExp -> ImpM rep r op ()
tracePrim s t se =
  emit . Imp.TracePrint $
    ErrorMsg [ErrorString (s <> ": "), ErrorVal t (toExp' t se), ErrorString "\n"]

traceArray :: String -> PrimType -> Shape -> SubExp -> ImpM rep r op ()
traceArray s t shape se = do
  emit . Imp.TracePrint $ ErrorMsg [ErrorString (s <> ": ")]
  sLoopNest shape $ \is -> do
    arr_elem <- dPrim "arr_elem" t
    copyDWIMFix (tvVar arr_elem) [] se is
    emit . Imp.TracePrint $ ErrorMsg [ErrorVal t (untyped (tvExp arr_elem)), " "]
  emit . Imp.TracePrint $ ErrorMsg ["\n"]

defCompileBasicOp ::
  Mem rep inner =>
  Pat rep ->
  BasicOp ->
  ImpM rep r op ()
defCompileBasicOp (Pat [pe]) (SubExp se) =
  copyDWIM (patElemName pe) [] se []
defCompileBasicOp (Pat [pe]) (Opaque op se) = do
  copyDWIM (patElemName pe) [] se []
  case op of
    OpaqueNil -> pure ()
    OpaqueTrace s -> comment ("Trace: " <> s) $ do
      se_t <- subExpType se
      case se_t of
        Prim t -> tracePrim s t se
        Array t shape _ -> traceArray s t shape se
        _ ->
          warn [mempty :: SrcLoc] mempty $
            s ++ ": cannot trace value of this (core) type: " <> pretty se_t
defCompileBasicOp (Pat [pe]) (UnOp op e) = do
  e' <- toExp e
  patElemName pe <~~ Imp.UnOpExp op e'
defCompileBasicOp (Pat [pe]) (ConvOp conv e) = do
  e' <- toExp e
  patElemName pe <~~ Imp.ConvOpExp conv e'
defCompileBasicOp (Pat [pe]) (BinOp bop x y) = do
  x' <- toExp x
  y' <- toExp y
  patElemName pe <~~ Imp.BinOpExp bop x' y'
defCompileBasicOp (Pat [pe]) (CmpOp bop x y) = do
  x' <- toExp x
  y' <- toExp y
  patElemName pe <~~ Imp.CmpOpExp bop x' y'
defCompileBasicOp _ (Assert e msg loc) = do
  e' <- toExp e
  msg' <- traverse toExp msg
  emit $ Imp.Assert e' msg' loc

  attrs <- askAttrs
  when (AttrComp "warn" ["safety_checks"] `inAttrs` attrs) $
    uncurry warn loc "Safety check required at run-time."
defCompileBasicOp (Pat [pe]) (Index src slice)
  | Just idxs <- sliceIndices slice =
    copyDWIM (patElemName pe) [] (Var src) $ map (DimFix . toInt64Exp) idxs
defCompileBasicOp _ Index {} =
  return ()
defCompileBasicOp (Pat [pe]) (Update safety _ slice se) =
  case safety of
    Unsafe -> write
    Safe -> sWhen (inBounds slice' dims) write
  where
    slice' = fmap toInt64Exp slice
    dims = map toInt64Exp $ arrayDims $ patElemType pe
    write = sUpdate (patElemName pe) slice' se
defCompileBasicOp _ FlatIndex {} =
  pure ()
defCompileBasicOp (Pat [pe]) (FlatUpdate _ slice v) = do
  pe_loc <- entryArrayLoc <$> lookupArray (patElemName pe)
  v_loc <- entryArrayLoc <$> lookupArray v
  copy (elemType (patElemType pe)) (flatSliceMemLoc pe_loc slice') v_loc
  where
    slice' = fmap toInt64Exp slice
defCompileBasicOp (Pat [pe]) (Replicate (Shape ds) se)
  | Acc {} <- patElemType pe = pure ()
  | otherwise = do
    ds' <- mapM toExp ds
    is <- replicateM (length ds) (newVName "i")
    copy_elem <- collect $ copyDWIM (patElemName pe) (map (DimFix . Imp.le64) is) se []
    emit $ foldl (.) id (zipWith Imp.For is ds') copy_elem
defCompileBasicOp _ Scratch {} =
  return ()
defCompileBasicOp (Pat [pe]) (Iota n e s it) = do
  e' <- toExp e
  s' <- toExp s
  sFor "i" (toInt64Exp n) $ \i -> do
    let i' = sExt it $ untyped i
    x <-
      dPrimV "x" . TPrimExp $
        BinOpExp (Add it OverflowUndef) e' $
          BinOpExp (Mul it OverflowUndef) i' s'
    copyDWIM (patElemName pe) [DimFix i] (Var (tvVar x)) []
defCompileBasicOp (Pat [pe]) (Copy src) =
  copyDWIM (patElemName pe) [] (Var src) []
defCompileBasicOp (Pat [pe]) (Manifest _ src) =
  copyDWIM (patElemName pe) [] (Var src) []
defCompileBasicOp (Pat [pe]) (Concat i x ys _) = do
  offs_glb <- dPrimV "tmp_offs" 0

  forM_ (x : ys) $ \y -> do
    y_dims <- arrayDims <$> lookupType y
    let rows = case drop i y_dims of
          [] -> error $ "defCompileBasicOp Concat: empty array shape for " ++ pretty y
          r : _ -> toInt64Exp r
        skip_dims = take i y_dims
        sliceAllDim d = DimSlice 0 d 1
        skip_slices = map (sliceAllDim . toInt64Exp) skip_dims
        destslice = skip_slices ++ [DimSlice (tvExp offs_glb) rows 1]
    copyDWIM (patElemName pe) destslice (Var y) []
    offs_glb <-- tvExp offs_glb + rows
defCompileBasicOp (Pat [pe]) (ArrayLit es _)
  | Just vs@(v : _) <- mapM isLiteral es = do
    dest_mem <- entryArrayLoc <$> lookupArray (patElemName pe)
    dest_space <- entryMemSpace <$> lookupMemory (memLocName dest_mem)
    let t = primValueType v
    static_array <- newVNameForFun "static_array"
    emit $ Imp.DeclareArray static_array dest_space t $ Imp.ArrayValues vs
    let static_src =
          MemLoc static_array [intConst Int64 $ fromIntegral $ length es] $
            IxFun.iota [fromIntegral $ length es]
        entry = MemVar Nothing $ MemEntry dest_space
    addVar static_array entry
    copy t dest_mem static_src
  | otherwise =
    forM_ (zip [0 ..] es) $ \(i, e) ->
      copyDWIM (patElemName pe) [DimFix $ fromInteger i] e []
  where
    isLiteral (Constant v) = Just v
    isLiteral _ = Nothing
defCompileBasicOp _ Rearrange {} =
  return ()
defCompileBasicOp _ Rotate {} =
  return ()
defCompileBasicOp _ Reshape {} =
  return ()
defCompileBasicOp _ (UpdateAcc acc is vs) = sComment "UpdateAcc" $ do
  -- We are abusing the comment mechanism to wrap the operator in
  -- braces when we end up generating code.  This is necessary because
  -- we might otherwise end up declaring lambda parameters (if any)
  -- multiple times, as they are duplicated every time we do an
  -- UpdateAcc for the same accumulator.
  let is' = map toInt64Exp is

  -- We need to figure out whether we are updating a scatter-like
  -- accumulator or a generalised reduction.  This also binds the
  -- index parameters.
  (_, _, arrs, dims, op) <- lookupAcc acc is'

  sWhen (inBounds (Slice (map DimFix is')) dims) $
    case op of
      Nothing ->
        -- Scatter-like.
        forM_ (zip arrs vs) $ \(arr, v) -> copyDWIMFix arr is' v []
      Just lam -> do
        -- Generalised reduction.
        dLParams $ lambdaParams lam
        let (x_params, y_params) =
              splitAt (length vs) $ map paramName $ lambdaParams lam

        forM_ (zip x_params arrs) $ \(xp, arr) ->
          copyDWIMFix xp [] (Var arr) is'

        forM_ (zip y_params vs) $ \(yp, v) ->
          copyDWIM yp [] v []

        compileStms mempty (bodyStms $ lambdaBody lam) $
          forM_ (zip arrs (bodyResult (lambdaBody lam))) $ \(arr, SubExpRes _ se) ->
            copyDWIMFix arr is' se []
defCompileBasicOp pat e =
  error $
    "ImpGen.defCompileBasicOp: Invalid pattern\n  "
      ++ pretty pat
      ++ "\nfor expression\n  "
      ++ pretty e

-- | Note: a hack to be used only for functions.
addArrays :: [ArrayDecl] -> ImpM rep r op ()
addArrays = mapM_ addArray
  where
    addArray (ArrayDecl name bt location) =
      addVar name $
        ArrayVar
          Nothing
          ArrayEntry
            { entryArrayLoc = location,
              entryArrayElemType = bt
            }

-- | Like 'dFParams', but does not create new declarations.
-- Note: a hack to be used only for functions.
addFParams :: Mem rep inner => [FParam rep] -> ImpM rep r op ()
addFParams = mapM_ addFParam
  where
    addFParam fparam =
      addVar (paramName fparam) $
        memBoundToVarEntry Nothing $ noUniquenessReturns $ paramDec fparam

-- | Another hack.
addLoopVar :: VName -> IntType -> ImpM rep r op ()
addLoopVar i it = addVar i $ ScalarVar Nothing $ ScalarEntry $ IntType it

dVars ::
  Mem rep inner =>
  Maybe (Exp rep) ->
  [PatElem rep] ->
  ImpM rep r op ()
dVars e = mapM_ dVar
  where
    dVar = dScope e . scopeOfPatElem

dFParams :: Mem rep inner => [FParam rep] -> ImpM rep r op ()
dFParams = dScope Nothing . scopeOfFParams

dLParams :: Mem rep inner => [LParam rep] -> ImpM rep r op ()
dLParams = dScope Nothing . scopeOfLParams

dPrimVol :: String -> PrimType -> Imp.TExp t -> ImpM rep r op (TV t)
dPrimVol name t e = do
  name' <- newVName name
  emit $ Imp.DeclareScalar name' Imp.Volatile t
  addVar name' $ ScalarVar Nothing $ ScalarEntry t
  name' <~~ untyped e
  return $ TV name' t

dPrim_ :: VName -> PrimType -> ImpM rep r op ()
dPrim_ name t = do
  emit $ Imp.DeclareScalar name Imp.Nonvolatile t
  addVar name $ ScalarVar Nothing $ ScalarEntry t

-- | The return type is polymorphic, so there is no guarantee it
-- actually matches the 'PrimType', but at least we have to use it
-- consistently.
dPrim :: String -> PrimType -> ImpM rep r op (TV t)
dPrim name t = do
  name' <- newVName name
  dPrim_ name' t
  return $ TV name' t

dPrimV_ :: VName -> Imp.TExp t -> ImpM rep r op ()
dPrimV_ name e = do
  dPrim_ name t
  TV name t <-- e
  where
    t = primExpType $ untyped e

dPrimV :: String -> Imp.TExp t -> ImpM rep r op (TV t)
dPrimV name e = do
  name' <- dPrim name $ primExpType $ untyped e
  name' <-- e
  return name'

dPrimVE :: String -> Imp.TExp t -> ImpM rep r op (Imp.TExp t)
dPrimVE name e = do
  name' <- dPrim name $ primExpType $ untyped e
  name' <-- e
  return $ tvExp name'

memBoundToVarEntry ::
  Maybe (Exp rep) ->
  MemBound NoUniqueness ->
  VarEntry rep
memBoundToVarEntry e (MemPrim bt) =
  ScalarVar e ScalarEntry {entryScalarType = bt}
memBoundToVarEntry e (MemMem space) =
  MemVar e $ MemEntry space
memBoundToVarEntry e (MemAcc acc ispace ts _) =
  AccVar e (acc, ispace, ts)
memBoundToVarEntry e (MemArray bt shape _ (ArrayIn mem ixfun)) =
  let location = MemLoc mem (shapeDims shape) ixfun
   in ArrayVar
        e
        ArrayEntry
          { entryArrayLoc = location,
            entryArrayElemType = bt
          }

infoDec ::
  Mem rep inner =>
  NameInfo rep ->
  MemInfo SubExp NoUniqueness MemBind
infoDec (LetName dec) = letDecMem dec
infoDec (FParamName dec) = noUniquenessReturns dec
infoDec (LParamName dec) = dec
infoDec (IndexName it) = MemPrim $ IntType it

dInfo ::
  Mem rep inner =>
  Maybe (Exp rep) ->
  VName ->
  NameInfo rep ->
  ImpM rep r op ()
dInfo e name info = do
  let entry = memBoundToVarEntry e $ infoDec info
  case entry of
    MemVar _ entry' ->
      emit $ Imp.DeclareMem name $ entryMemSpace entry'
    ScalarVar _ entry' ->
      emit $ Imp.DeclareScalar name Imp.Nonvolatile $ entryScalarType entry'
    ArrayVar _ _ ->
      return ()
    AccVar {} ->
      return ()
  addVar name entry

dScope ::
  Mem rep inner =>
  Maybe (Exp rep) ->
  Scope rep ->
  ImpM rep r op ()
dScope e = mapM_ (uncurry $ dInfo e) . M.toList

dArray :: VName -> PrimType -> ShapeBase SubExp -> VName -> IxFun -> ImpM rep r op ()
dArray name pt shape mem ixfun =
  addVar name $ ArrayVar Nothing $ ArrayEntry location pt
  where
    location =
      MemLoc mem (shapeDims shape) ixfun

everythingVolatile :: ImpM rep r op a -> ImpM rep r op a
everythingVolatile = local $ \env -> env {envVolatility = Imp.Volatile}

-- | Remove the array targets.
funcallTargets :: [ValueDestination] -> ImpM rep r op [VName]
funcallTargets dests =
  concat <$> mapM funcallTarget dests
  where
    funcallTarget (ScalarDestination name) =
      return [name]
    funcallTarget (ArrayDestination _) =
      return []
    funcallTarget (MemoryDestination name) =
      return [name]

-- | A typed variable, which we can turn into a typed expression, or
-- use as the target for an assignment.  This is used to aid in type
-- safety when doing code generation, by keeping the types straight.
-- It is still easy to cheat when you need to.
data TV t = TV VName PrimType

-- | Create a typed variable from a name and a dynamic type.  Note
-- that there is no guarantee that the dynamic type corresponds to the
-- inferred static type, but the latter will at least have to be used
-- consistently.
mkTV :: VName -> PrimType -> TV t
mkTV = TV

-- | Convert a typed variable to a size (a SubExp).
tvSize :: TV t -> Imp.DimSize
tvSize = Var . tvVar

-- | Convert a typed variable to a similarly typed expression.
tvExp :: TV t -> Imp.TExp t
tvExp (TV v t) = Imp.TPrimExp $ Imp.var v t

-- | Extract the underlying variable name from a typed variable.
tvVar :: TV t -> VName
tvVar (TV v _) = v

-- | Compile things to 'Imp.Exp'.
class ToExp a where
  -- | Compile to an 'Imp.Exp', where the type (must must still be a
  -- primitive) is deduced monadically.
  toExp :: a -> ImpM rep r op Imp.Exp

  -- | Compile where we know the type in advance.
  toExp' :: PrimType -> a -> Imp.Exp

  toInt64Exp :: a -> Imp.TExp Int64
  toInt64Exp = TPrimExp . toExp' int64

  toBoolExp :: a -> Imp.TExp Bool
  toBoolExp = TPrimExp . toExp' Bool

instance ToExp SubExp where
  toExp (Constant v) =
    return $ Imp.ValueExp v
  toExp (Var v) =
    lookupVar v >>= \case
      ScalarVar _ (ScalarEntry pt) ->
        return $ Imp.var v pt
      _ -> error $ "toExp SubExp: SubExp is not a primitive type: " ++ pretty v

  toExp' _ (Constant v) = Imp.ValueExp v
  toExp' t (Var v) = Imp.var v t

instance ToExp (PrimExp VName) where
  toExp = pure
  toExp' _ = id

addVar :: VName -> VarEntry rep -> ImpM rep r op ()
addVar name entry =
  modify $ \s -> s {stateVTable = M.insert name entry $ stateVTable s}

localDefaultSpace :: Imp.Space -> ImpM rep r op a -> ImpM rep r op a
localDefaultSpace space = local (\env -> env {envDefaultSpace = space})

askFunction :: ImpM rep r op (Maybe Name)
askFunction = asks envFunction

-- | Generate a 'VName', prefixed with 'askFunction' if it exists.
newVNameForFun :: String -> ImpM rep r op VName
newVNameForFun s = do
  fname <- fmap nameToString <$> askFunction
  newVName $ maybe "" (++ ".") fname ++ s

-- | Generate a 'Name', prefixed with 'askFunction' if it exists.
nameForFun :: String -> ImpM rep r op Name
nameForFun s = do
  fname <- askFunction
  return $ maybe "" (<> ".") fname <> nameFromString s

askEnv :: ImpM rep r op r
askEnv = asks envEnv

localEnv :: (r -> r) -> ImpM rep r op a -> ImpM rep r op a
localEnv f = local $ \env -> env {envEnv = f $ envEnv env}

-- | The active attributes, including those for the statement
-- currently being compiled.
askAttrs :: ImpM rep r op Attrs
askAttrs = asks envAttrs

-- | Add more attributes to what is returning by 'askAttrs'.
localAttrs :: Attrs -> ImpM rep r op a -> ImpM rep r op a
localAttrs attrs = local $ \env -> env {envAttrs = attrs <> envAttrs env}

localOps :: Operations rep r op -> ImpM rep r op a -> ImpM rep r op a
localOps ops = local $ \env ->
  env
    { envExpCompiler = opsExpCompiler ops,
      envStmsCompiler = opsStmsCompiler ops,
      envCopyCompiler = opsCopyCompiler ops,
      envOpCompiler = opsOpCompiler ops,
      envAllocCompilers = opsAllocCompilers ops
    }

-- | Get the current symbol table.
getVTable :: ImpM rep r op (VTable rep)
getVTable = gets stateVTable

putVTable :: VTable rep -> ImpM rep r op ()
putVTable vtable = modify $ \s -> s {stateVTable = vtable}

-- | Run an action with a modified symbol table.  All changes to the
-- symbol table will be reverted once the action is done!
localVTable :: (VTable rep -> VTable rep) -> ImpM rep r op a -> ImpM rep r op a
localVTable f m = do
  old_vtable <- getVTable
  putVTable $ f old_vtable
  a <- m
  putVTable old_vtable
  return a

lookupVar :: VName -> ImpM rep r op (VarEntry rep)
lookupVar name = do
  res <- gets $ M.lookup name . stateVTable
  case res of
    Just entry -> return entry
    _ -> error $ "Unknown variable: " ++ pretty name

lookupArray :: VName -> ImpM rep r op ArrayEntry
lookupArray name = do
  res <- lookupVar name
  case res of
    ArrayVar _ entry -> return entry
    _ -> error $ "ImpGen.lookupArray: not an array: " ++ pretty name

lookupMemory :: VName -> ImpM rep r op MemEntry
lookupMemory name = do
  res <- lookupVar name
  case res of
    MemVar _ entry -> return entry
    _ -> error $ "Unknown memory block: " ++ pretty name

lookupArraySpace :: VName -> ImpM rep r op Space
lookupArraySpace =
  fmap entryMemSpace . lookupMemory
    <=< fmap (memLocName . entryArrayLoc) . lookupArray

-- | In the case of a histogram-like accumulator, also sets the index
-- parameters.
lookupAcc ::
  VName ->
  [Imp.TExp Int64] ->
  ImpM rep r op (VName, Space, [VName], [Imp.TExp Int64], Maybe (Lambda rep))
lookupAcc name is = do
  res <- lookupVar name
  case res of
    AccVar _ (acc, ispace, _) -> do
      acc' <- gets $ M.lookup acc . stateAccs
      case acc' of
        Just ([], _) ->
          error $ "Accumulator with no arrays: " ++ pretty name
        Just (arrs@(arr : _), Just (op, _)) -> do
          space <- lookupArraySpace arr
          let (i_params, ps) = splitAt (length is) $ lambdaParams op
          zipWithM_ dPrimV_ (map paramName i_params) is
          return
            ( acc,
              space,
              arrs,
              map toInt64Exp (shapeDims ispace),
              Just op {lambdaParams = ps}
            )
        Just (arrs@(arr : _), Nothing) -> do
          space <- lookupArraySpace arr
          return (acc, space, arrs, map toInt64Exp (shapeDims ispace), Nothing)
        Nothing ->
          error $ "ImpGen.lookupAcc: unlisted accumulator: " ++ pretty name
    _ -> error $ "ImpGen.lookupAcc: not an accumulator: " ++ pretty name

destinationFromPat :: Pat rep -> ImpM rep r op [ValueDestination]
destinationFromPat = mapM inspect . patElems
  where
    inspect pe = do
      let name = patElemName pe
      entry <- lookupVar name
      case entry of
        ArrayVar _ (ArrayEntry MemLoc {} _) ->
          return $ ArrayDestination Nothing
        MemVar {} ->
          return $ MemoryDestination name
        ScalarVar {} ->
          return $ ScalarDestination name
        AccVar {} ->
          return $ ArrayDestination Nothing

fullyIndexArray ::
  VName ->
  [Imp.TExp Int64] ->
  ImpM rep r op (VName, Imp.Space, Count Elements (Imp.TExp Int64))
fullyIndexArray name indices = do
  arr <- lookupArray name
  fullyIndexArray' (entryArrayLoc arr) indices

fullyIndexArray' ::
  MemLoc ->
  [Imp.TExp Int64] ->
  ImpM rep r op (VName, Imp.Space, Count Elements (Imp.TExp Int64))
fullyIndexArray' (MemLoc mem _ ixfun) indices = do
  space <- entryMemSpace <$> lookupMemory mem
  return
    ( mem,
      space,
      elements $ IxFun.index ixfun indices
    )

-- More complicated read/write operations that use index functions.

copy :: CopyCompiler rep r op
copy bt dest src =
  unless
    ( memLocName dest == memLocName src
        && memLocIxFun dest `IxFun.equivalent` memLocIxFun src
    )
    $ do
      cc <- asks envCopyCompiler
      cc bt dest src

-- | Is this copy really a mapping with transpose?
isMapTransposeCopy ::
  PrimType ->
  MemLoc ->
  MemLoc ->
  Maybe
    ( Imp.TExp Int64,
      Imp.TExp Int64,
      Imp.TExp Int64,
      Imp.TExp Int64,
      Imp.TExp Int64
    )
isMapTransposeCopy bt (MemLoc _ _ destIxFun) (MemLoc _ _ srcIxFun)
  | Just (dest_offset, perm_and_destshape) <- IxFun.rearrangeWithOffset destIxFun bt_size,
    (perm, destshape) <- unzip perm_and_destshape,
    Just src_offset <- IxFun.linearWithOffset srcIxFun bt_size,
    Just (r1, r2, _) <- isMapTranspose perm =
    isOk destshape swap r1 r2 dest_offset src_offset
  | Just dest_offset <- IxFun.linearWithOffset destIxFun bt_size,
    Just (src_offset, perm_and_srcshape) <- IxFun.rearrangeWithOffset srcIxFun bt_size,
    (perm, srcshape) <- unzip perm_and_srcshape,
    Just (r1, r2, _) <- isMapTranspose perm =
    isOk srcshape id r1 r2 dest_offset src_offset
  | otherwise =
    Nothing
  where
    bt_size = primByteSize bt
    swap (x, y) = (y, x)

    isOk shape f r1 r2 dest_offset src_offset = do
      let (num_arrays, size_x, size_y) = getSizes shape f r1 r2
      return
        ( dest_offset,
          src_offset,
          num_arrays,
          size_x,
          size_y
        )

    getSizes shape f r1 r2 =
      let (mapped, notmapped) = splitAt r1 shape
          (pretrans, posttrans) = f $ splitAt r2 notmapped
       in (product mapped, product pretrans, product posttrans)

mapTransposeName :: PrimType -> String
mapTransposeName bt = "map_transpose_" ++ pretty bt

mapTransposeForType :: PrimType -> ImpM rep r op Name
mapTransposeForType bt = do
  let fname = nameFromString $ "builtin#" <> mapTransposeName bt

  exists <- hasFunction fname
  unless exists $ emitFunction fname $ mapTransposeFunction fname bt

  return fname

-- | Use an 'Imp.Copy' if possible, otherwise 'copyElementWise'.
defaultCopy :: CopyCompiler rep r op
defaultCopy pt dest src
  | Just (destoffset, srcoffset, num_arrays, size_x, size_y) <-
      isMapTransposeCopy pt dest src = do
    fname <- mapTransposeForType pt
    emit $
      Imp.Call
        []
        fname
        $ transposeArgs
          pt
          destmem
          (bytes destoffset)
          srcmem
          (bytes srcoffset)
          num_arrays
          size_x
          size_y
  | Just destoffset <-
      IxFun.linearWithOffset dest_ixfun pt_size,
    Just srcoffset <-
      IxFun.linearWithOffset src_ixfun pt_size = do
    srcspace <- entryMemSpace <$> lookupMemory srcmem
    destspace <- entryMemSpace <$> lookupMemory destmem
    if isScalarSpace srcspace || isScalarSpace destspace
      then copyElementWise pt dest src
      else
        emit $
          Imp.Copy
            destmem
            (bytes destoffset)
            destspace
            srcmem
            (bytes srcoffset)
            srcspace
            $ num_elems `withElemType` pt
  | otherwise =
    copyElementWise pt dest src
  where
    pt_size = primByteSize pt
    num_elems = Imp.elements $ product $ IxFun.shape $ memLocIxFun src

    MemLoc destmem _ dest_ixfun = dest
    MemLoc srcmem _ src_ixfun = src

    isScalarSpace ScalarSpace {} = True
    isScalarSpace _ = False

copyElementWise :: CopyCompiler rep r op
copyElementWise bt dest src = do
  let bounds = IxFun.shape $ memLocIxFun src
  is <- replicateM (length bounds) (newVName "i")
  let ivars = map Imp.le64 is
  (destmem, destspace, destidx) <- fullyIndexArray' dest ivars
  (srcmem, srcspace, srcidx) <- fullyIndexArray' src ivars
  vol <- asks envVolatility
  tmp <- newVName "tmp"
  emit $
    foldl (.) id (zipWith Imp.For is $ map untyped bounds) $
      mconcat
        [ Imp.DeclareScalar tmp vol bt,
          Imp.Read tmp srcmem srcidx bt srcspace vol,
          Imp.Write destmem destidx bt destspace vol $ Imp.var tmp bt
        ]

-- | Copy from here to there; both destination and source may be
-- indexeded.
copyArrayDWIM ::
  PrimType ->
  MemLoc ->
  [DimIndex (Imp.TExp Int64)] ->
  MemLoc ->
  [DimIndex (Imp.TExp Int64)] ->
  ImpM rep r op (Imp.Code op)
copyArrayDWIM
  bt
  destlocation@(MemLoc _ destshape _)
  destslice
  srclocation@(MemLoc _ srcshape _)
  srcslice
    | Just destis <- mapM dimFix destslice,
      Just srcis <- mapM dimFix srcslice,
      length srcis == length srcshape,
      length destis == length destshape = do
      (targetmem, destspace, targetoffset) <-
        fullyIndexArray' destlocation destis
      (srcmem, srcspace, srcoffset) <-
        fullyIndexArray' srclocation srcis
      vol <- asks envVolatility
      collect $ do
        tmp <- tvVar <$> dPrim "tmp" bt
        emit $ Imp.Read tmp srcmem srcoffset bt srcspace vol
        emit $ Imp.Write targetmem targetoffset bt destspace vol $ Imp.var tmp bt
    | otherwise = do
      let destslice' = fullSliceNum (map toInt64Exp destshape) destslice
          srcslice' = fullSliceNum (map toInt64Exp srcshape) srcslice
          destrank = length $ sliceDims destslice'
          srcrank = length $ sliceDims srcslice'
          destlocation' = sliceMemLoc destlocation destslice'
          srclocation' = sliceMemLoc srclocation srcslice'
      if destrank /= srcrank
        then
          error $
            "copyArrayDWIM: cannot copy to "
              ++ pretty (memLocName destlocation)
              ++ " from "
              ++ pretty (memLocName srclocation)
              ++ " because ranks do not match ("
              ++ pretty destrank
              ++ " vs "
              ++ pretty srcrank
              ++ ")"
        else
          if destlocation' == srclocation'
            then pure mempty -- Copy would be no-op.
            else collect $ copy bt destlocation' srclocation'

-- | Like 'copyDWIM', but the target is a 'ValueDestination'
-- instead of a variable name.
copyDWIMDest ::
  ValueDestination ->
  [DimIndex (Imp.TExp Int64)] ->
  SubExp ->
  [DimIndex (Imp.TExp Int64)] ->
  ImpM rep r op ()
copyDWIMDest _ _ (Constant v) (_ : _) =
  error $
    unwords ["copyDWIMDest: constant source", pretty v, "cannot be indexed."]
copyDWIMDest pat dest_slice (Constant v) [] =
  case mapM dimFix dest_slice of
    Nothing ->
      error $
        unwords ["copyDWIMDest: constant source", pretty v, "with slice destination."]
    Just dest_is ->
      case pat of
        ScalarDestination name ->
          emit $ Imp.SetScalar name $ Imp.ValueExp v
        MemoryDestination {} ->
          error $
            unwords ["copyDWIMDest: constant source", pretty v, "cannot be written to memory destination."]
        ArrayDestination (Just dest_loc) -> do
          (dest_mem, dest_space, dest_i) <-
            fullyIndexArray' dest_loc dest_is
          vol <- asks envVolatility
          emit $ Imp.Write dest_mem dest_i bt dest_space vol $ Imp.ValueExp v
        ArrayDestination Nothing ->
          error "copyDWIMDest: ArrayDestination Nothing"
  where
    bt = primValueType v
copyDWIMDest dest dest_slice (Var src) src_slice = do
  src_entry <- lookupVar src
  case (dest, src_entry) of
    (MemoryDestination mem, MemVar _ (MemEntry space)) ->
      emit $ Imp.SetMem mem src space
    (MemoryDestination {}, _) ->
      error $
        unwords ["copyDWIMDest: cannot write", pretty src, "to memory destination."]
    (_, MemVar {}) ->
      error $
        unwords ["copyDWIMDest: source", pretty src, "is a memory block."]
    (_, ScalarVar _ (ScalarEntry _))
      | not $ null src_slice ->
        error $
          unwords ["copyDWIMDest: prim-typed source", pretty src, "with slice", pretty src_slice]
    (ScalarDestination name, _)
      | not $ null dest_slice ->
        error $
          unwords ["copyDWIMDest: prim-typed target", pretty name, "with slice", pretty dest_slice]
    (ScalarDestination name, ScalarVar _ (ScalarEntry pt)) ->
      emit $ Imp.SetScalar name $ Imp.var src pt
    (ScalarDestination name, ArrayVar _ arr)
      | Just src_is <- mapM dimFix src_slice,
        length src_slice == length (entryArrayShape arr) -> do
        let bt = entryArrayElemType arr
        (mem, space, i) <-
          fullyIndexArray' (entryArrayLoc arr) src_is
        vol <- asks envVolatility
        emit $ Imp.Read name mem i bt space vol
      | otherwise ->
        error $
          unwords
            [ "copyDWIMDest: prim-typed target",
              pretty name,
              "and array-typed source",
              pretty src,
              "of shape",
              pretty (entryArrayShape arr),
              "sliced with",
              pretty src_slice
            ]
    (ArrayDestination (Just dest_loc), ArrayVar _ src_arr) -> do
      let src_loc = entryArrayLoc src_arr
          bt = entryArrayElemType src_arr
      emit =<< copyArrayDWIM bt dest_loc dest_slice src_loc src_slice
    (ArrayDestination (Just dest_loc), ScalarVar _ (ScalarEntry bt))
      | Just dest_is <- mapM dimFix dest_slice,
        length dest_is == length (memLocShape dest_loc) -> do
        (dest_mem, dest_space, dest_i) <- fullyIndexArray' dest_loc dest_is
        vol <- asks envVolatility
        emit $ Imp.Write dest_mem dest_i bt dest_space vol (Imp.var src bt)
      | otherwise ->
        error $
          unwords
            [ "copyDWIMDest: array-typed target and prim-typed source",
              pretty src,
              "with slice",
              pretty dest_slice
            ]
    (ArrayDestination Nothing, _) ->
      return () -- Nothing to do; something else set some memory
      -- somewhere.
    (_, AccVar {}) ->
      return () -- Nothing to do; accumulators are phantoms.

-- | Copy from here to there; both destination and source be
-- indexeded.  If so, they better be arrays of enough dimensions.
-- This function will generally just Do What I Mean, and Do The Right
-- Thing.  Both destination and source must be in scope.
copyDWIM ::
  VName ->
  [DimIndex (Imp.TExp Int64)] ->
  SubExp ->
  [DimIndex (Imp.TExp Int64)] ->
  ImpM rep r op ()
copyDWIM dest dest_slice src src_slice = do
  dest_entry <- lookupVar dest
  let dest_target =
        case dest_entry of
          ScalarVar _ _ ->
            ScalarDestination dest
          ArrayVar _ (ArrayEntry (MemLoc mem shape ixfun) _) ->
            ArrayDestination $ Just $ MemLoc mem shape ixfun
          MemVar _ _ ->
            MemoryDestination dest
          AccVar {} ->
            -- Does not matter; accumulators are phantoms.
            ArrayDestination Nothing
  copyDWIMDest dest_target dest_slice src src_slice

-- | As 'copyDWIM', but implicitly 'DimFix'es the indexes.
copyDWIMFix ::
  VName ->
  [Imp.TExp Int64] ->
  SubExp ->
  [Imp.TExp Int64] ->
  ImpM rep r op ()
copyDWIMFix dest dest_is src src_is =
  copyDWIM dest (map DimFix dest_is) src (map DimFix src_is)

-- | @compileAlloc pat size space@ allocates @n@ bytes of memory in @space@,
-- writing the result to @dest@, which must be a single
-- 'MemoryDestination',
compileAlloc ::
  Mem rep inner => Pat rep -> SubExp -> Space -> ImpM rep r op ()
compileAlloc (Pat [mem]) e space = do
  let e' = Imp.bytes $ toInt64Exp e
  allocator <- asks $ M.lookup space . envAllocCompilers
  case allocator of
    Nothing -> emit $ Imp.Allocate (patElemName mem) e' space
    Just allocator' -> allocator' (patElemName mem) e'
compileAlloc pat _ _ =
  error $ "compileAlloc: Invalid pattern: " ++ pretty pat

-- | The number of bytes needed to represent the array in a
-- straightforward contiguous format, as an t'Int64' expression.
typeSize :: Type -> Count Bytes (Imp.TExp Int64)
typeSize t =
  Imp.bytes $ primByteSize (elemType t) * product (map toInt64Exp (arrayDims t))

-- | Is this indexing in-bounds for an array of the given shape?  This
-- is useful for things like scatter, which ignores out-of-bounds
-- writes.
inBounds :: Slice (Imp.TExp Int64) -> [Imp.TExp Int64] -> Imp.TExp Bool
inBounds (Slice slice) dims =
  let condInBounds (DimFix i) d =
        0 .<=. i .&&. i .<. d
      condInBounds (DimSlice i n s) d =
        0 .<=. i .&&. i + (n - 1) * s .<. d
   in foldl1 (.&&.) $ zipWith condInBounds slice dims

--- Building blocks for constructing code.

sFor' :: VName -> Imp.Exp -> ImpM rep r op () -> ImpM rep r op ()
sFor' i bound body = do
  let it = case primExpType bound of
        IntType bound_t -> bound_t
        t -> error $ "sFor': bound " ++ pretty bound ++ " is of type " ++ pretty t
  addLoopVar i it
  body' <- collect body
  emit $ Imp.For i bound body'

sFor :: String -> Imp.TExp t -> (Imp.TExp t -> ImpM rep r op ()) -> ImpM rep r op ()
sFor i bound body = do
  i' <- newVName i
  sFor' i' (untyped bound) $
    body $ TPrimExp $ Imp.var i' $ primExpType $ untyped bound

sWhile :: Imp.TExp Bool -> ImpM rep r op () -> ImpM rep r op ()
sWhile cond body = do
  body' <- collect body
  emit $ Imp.While cond body'

sComment :: String -> ImpM rep r op () -> ImpM rep r op ()
sComment s code = do
  code' <- collect code
  emit $ Imp.Comment s code'

sIf :: Imp.TExp Bool -> ImpM rep r op () -> ImpM rep r op () -> ImpM rep r op ()
sIf cond tbranch fbranch = do
  tbranch' <- collect tbranch
  fbranch' <- collect fbranch
  -- Avoid generating branch if the condition is known statically.
  emit $
    if cond == true
      then tbranch'
      else
        if cond == false
          then fbranch'
          else Imp.If cond tbranch' fbranch'

sWhen :: Imp.TExp Bool -> ImpM rep r op () -> ImpM rep r op ()
sWhen cond tbranch = sIf cond tbranch (return ())

sUnless :: Imp.TExp Bool -> ImpM rep r op () -> ImpM rep r op ()
sUnless cond = sIf cond (return ())

sOp :: op -> ImpM rep r op ()
sOp = emit . Imp.Op

sDeclareMem :: String -> Space -> ImpM rep r op VName
sDeclareMem name space = do
  name' <- newVName name
  emit $ Imp.DeclareMem name' space
  addVar name' $ MemVar Nothing $ MemEntry space
  return name'

sAlloc_ :: VName -> Count Bytes (Imp.TExp Int64) -> Space -> ImpM rep r op ()
sAlloc_ name' size' space = do
  allocator <- asks $ M.lookup space . envAllocCompilers
  case allocator of
    Nothing -> emit $ Imp.Allocate name' size' space
    Just allocator' -> allocator' name' size'

sAlloc :: String -> Count Bytes (Imp.TExp Int64) -> Space -> ImpM rep r op VName
sAlloc name size space = do
  name' <- sDeclareMem name space
  sAlloc_ name' size space
  return name'

sArray :: String -> PrimType -> ShapeBase SubExp -> VName -> IxFun -> ImpM rep r op VName
sArray name bt shape mem ixfun = do
  name' <- newVName name
  dArray name' bt shape mem ixfun
  return name'

-- | Declare an array in row-major order in the given memory block.
sArrayInMem :: String -> PrimType -> ShapeBase SubExp -> VName -> ImpM rep r op VName
sArrayInMem name pt shape mem =
  sArray name pt shape mem $
    IxFun.iota $ map (isInt64 . primExpFromSubExp int64) $ shapeDims shape

-- | Like 'sAllocArray', but permute the in-memory representation of the indices as specified.
sAllocArrayPerm :: String -> PrimType -> ShapeBase SubExp -> Space -> [Int] -> ImpM rep r op VName
sAllocArrayPerm name pt shape space perm = do
  let permuted_dims = rearrangeShape perm $ shapeDims shape
  mem <- sAlloc (name ++ "_mem") (typeSize (Array pt shape NoUniqueness)) space
  let iota_ixfun = IxFun.iota $ map (isInt64 . primExpFromSubExp int64) permuted_dims
  sArray name pt shape mem $
    IxFun.permute iota_ixfun $ rearrangeInverse perm

-- | Uses linear/iota index function.
sAllocArray :: String -> PrimType -> ShapeBase SubExp -> Space -> ImpM rep r op VName
sAllocArray name pt shape space =
  sAllocArrayPerm name pt shape space [0 .. shapeRank shape - 1]

-- | Uses linear/iota index function.
sStaticArray :: String -> Space -> PrimType -> Imp.ArrayContents -> ImpM rep r op VName
sStaticArray name space pt vs = do
  let num_elems = case vs of
        Imp.ArrayValues vs' -> length vs'
        Imp.ArrayZeros n -> fromIntegral n
      shape = Shape [intConst Int64 $ toInteger num_elems]
  mem <- newVNameForFun $ name ++ "_mem"
  emit $ Imp.DeclareArray mem space pt vs
  addVar mem $ MemVar Nothing $ MemEntry space
  sArray name pt shape mem $ IxFun.iota [fromIntegral num_elems]

sWrite :: VName -> [Imp.TExp Int64] -> Imp.Exp -> ImpM rep r op ()
sWrite arr is v = do
  (mem, space, offset) <- fullyIndexArray arr is
  vol <- asks envVolatility
  emit $ Imp.Write mem offset (primExpType v) space vol v

sUpdate :: VName -> Slice (Imp.TExp Int64) -> SubExp -> ImpM rep r op ()
sUpdate arr slice v = copyDWIM arr (unSlice slice) v []

sLoopNest ::
  Shape ->
  ([Imp.TExp Int64] -> ImpM rep r op ()) ->
  ImpM rep r op ()
sLoopNest = sLoopNest' [] . shapeDims
  where
    sLoopNest' is [] f = f $ reverse is
    sLoopNest' is (d : ds) f =
      sFor "nest_i" (toInt64Exp d) $ \i -> sLoopNest' (i : is) ds f

-- | Untyped assignment.
(<~~) :: VName -> Imp.Exp -> ImpM rep r op ()
x <~~ e = emit $ Imp.SetScalar x e

infixl 3 <~~

-- | Typed assignment.
(<--) :: TV t -> Imp.TExp t -> ImpM rep r op ()
TV x _ <-- e = emit $ Imp.SetScalar x $ untyped e

infixl 3 <--

-- | Constructing an ad-hoc function that does not
-- correspond to any of the IR functions in the input program.
function ::
  Name ->
  [Imp.Param] ->
  [Imp.Param] ->
  ImpM rep r op () ->
  ImpM rep r op ()
function fname outputs inputs m = local newFunction $ do
  body <- collect $ do
    mapM_ addParam $ outputs ++ inputs
    m
  emitFunction fname $ Imp.Function Nothing outputs inputs body [] []
  where
    addParam (Imp.MemParam name space) =
      addVar name $ MemVar Nothing $ MemEntry space
    addParam (Imp.ScalarParam name bt) =
      addVar name $ ScalarVar Nothing $ ScalarEntry bt
    newFunction env = env {envFunction = Just fname}

dSlices :: [Imp.TExp Int64] -> ImpM rep r op [Imp.TExp Int64]
dSlices = fmap (drop 1 . snd) . dSlices'
  where
    dSlices' [] = pure (1, [1])
    dSlices' (n : ns) = do
      (prod, ns') <- dSlices' ns
      n' <- dPrimVE "slice" $ n * prod
      pure (n', n' : ns')

-- | @dIndexSpace f dims i@ computes a list of indices into an
-- array with dimension @dims@ given the flat index @i@.  The
-- resulting list will have the same size as @dims@.  Intermediate
-- results are passed to @f@.
dIndexSpace ::
  [(VName, Imp.TExp Int64)] ->
  Imp.TExp Int64 ->
  ImpM rep r op ()
dIndexSpace vs_ds j = do
  slices <- dSlices (map snd vs_ds)
  loop (zip (map fst vs_ds) slices) j
  where
    loop ((v, size) : rest) i = do
      dPrimV_ v (i `quot` size)
      i' <- dPrimVE "remnant" $ i - Imp.le64 v * size
      loop rest i'
    loop _ _ = pure ()

-- | Like 'dIndexSpace', but invent some new names for the indexes
-- based on the given template.
dIndexSpace' ::
  String ->
  [Imp.TExp Int64] ->
  Imp.TExp Int64 ->
  ImpM rep r op [Imp.TExp Int64]
dIndexSpace' desc ds j = do
  ivs <- replicateM (length ds) (newVName desc)
  dIndexSpace (zip ivs ds) j
  pure $ map Imp.le64 ivs
