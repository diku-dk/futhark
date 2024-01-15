{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}
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
    VarEntry (..),
    ArrayEntry (..),

    -- * Lookups
    lookupVar,
    lookupArray,
    lookupMemory,
    lookupAcc,
    askAttrs,

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
    lmadCopy,
    typeSize,
    inBounds,
    caseMatch,

    -- * Constructing code.
    newVName,
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
    sLoopSpace,
    (<--),
    (<~~),
    function,
    genConstants,
    warn,
    module Language.Futhark.Warnings,
  )
where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Parallel.Strategies
import Data.Bifunctor (first)
import Data.DList qualified as DL
import Data.Either
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.String
import Data.Text qualified as T
import Futhark.CodeGen.ImpCode
  ( Bytes,
    Count,
    Elements,
    elements,
  )
import Futhark.CodeGen.ImpCode qualified as Imp
import Futhark.Construct hiding (ToExp (..))
import Futhark.IR.Mem
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.IR.SOACS (SOACS)
import Futhark.Util
import Futhark.Util.IntegralExp
import Futhark.Util.Loc (noLoc)
import Futhark.Util.Pretty hiding (nest, space)
import Language.Futhark.Warnings
import Prelude hiding (mod, quot)

-- | How to compile an t'Op'.
type OpCompiler rep r op = Pat (LetDec rep) -> Op rep -> ImpM rep r op ()

-- | How to compile some 'Stms'.
type StmsCompiler rep r op = Names -> Stms rep -> ImpM rep r op () -> ImpM rep r op ()

-- | How to compile an 'Exp'.
type ExpCompiler rep r op = Pat (LetDec rep) -> Exp rep -> ImpM rep r op ()

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
      opsCopyCompiler = lmadCopy,
      opsAllocCompilers = mempty
    }

-- | When an array is declared, this is where it is stored.
data MemLoc = MemLoc
  { memLocName :: VName,
    memLocShape :: [Imp.DimSize],
    memLocLMAD :: LMAD.LMAD (Imp.TExp Int64)
  }
  deriving (Eq, Show)

sliceMemLoc :: MemLoc -> Slice (Imp.TExp Int64) -> MemLoc
sliceMemLoc (MemLoc mem shape lmad) slice =
  MemLoc mem shape $ LMAD.slice lmad slice

flatSliceMemLoc :: MemLoc -> FlatSlice (Imp.TExp Int64) -> MemLoc
flatSliceMemLoc (MemLoc mem shape lmad) slice =
  MemLoc mem shape $ LMAD.flatSlice lmad slice

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
    stateConstants :: Imp.Constants op,
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
newState = ImpState mempty mempty mempty mempty mempty mempty

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
            stateConstants = mempty,
            stateWarnings = mempty,
            stateAccs = stateAccs s
          }
      (x, s'') = runState (runReaderT m env') s'

  putNameSource $ stateNameSource s''
  warnings $ stateWarnings s''
  pure (x, stateCode s'')

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
  pure (x, new_code)

-- | Emit some generated imperative code.
emit :: Imp.Code op -> ImpM rep r op ()
emit code = modify $ \s -> s {stateCode = stateCode s <> code}

warnings :: Warnings -> ImpM rep r op ()
warnings ws = modify $ \s -> s {stateWarnings = ws <> stateWarnings s}

-- | Emit a warning about something the user should be aware of.
warn :: (Located loc) => loc -> [loc] -> T.Text -> ImpM rep r op ()
warn loc locs problem =
  warnings $ singleWarning' (srclocOf loc) (map srclocOf locs) (pretty problem)

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

constsVTable :: (Mem rep inner) => Stms rep -> VTable rep
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
compileProg r ops space (Prog types consts funs) =
  modifyNameSource $ \src ->
    let (_, ss) =
          unzip $ parMap rpar (compileFunDef' src) funs
        free_in_funs =
          freeIn $ mconcat $ map stateFunctions ss
        ((), s') =
          runImpM (compileConsts free_in_funs consts) r ops space $
            combineStates ss
     in ( ( stateWarnings s',
            Imp.Definitions
              types
              (stateConstants s' <> foldMap stateConstants ss)
              (stateFunctions s')
          ),
          stateNameSource s'
        )
  where
    compileFunDef' src fdef =
      runImpM
        (compileFunDef types fdef)
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

compileConsts :: Names -> Stms rep -> ImpM rep r op ()
compileConsts used_consts stms = genConstants $ do
  compileStms used_consts stms $ pure ()
  pure (used_consts, ())

lookupOpaqueType :: Name -> OpaqueTypes -> OpaqueType
lookupOpaqueType v (OpaqueTypes types) =
  case lookup v types of
    Just t -> t
    Nothing -> error $ "Unknown opaque type: " ++ show v

valueTypeSign :: ValueType -> Signedness
valueTypeSign (ValueType sign _ _) = sign

entryPointSignedness :: OpaqueTypes -> EntryPointType -> [Signedness]
entryPointSignedness _ (TypeTransparent vt) = [valueTypeSign vt]
entryPointSignedness types (TypeOpaque desc) =
  case lookupOpaqueType desc types of
    OpaqueType vts -> map valueTypeSign vts
    OpaqueRecord fs -> foldMap (entryPointSignedness types . snd) fs
    OpaqueSum vts _ -> map valueTypeSign vts

-- | How many value parameters are accepted by this entry point?  This
-- is used to determine which of the function parameters correspond to
-- the parameters of the original function (they must all come at the
-- end).
entryPointSize :: OpaqueTypes -> EntryPointType -> Int
entryPointSize _ (TypeTransparent _) = 1
entryPointSize types (TypeOpaque desc) =
  case lookupOpaqueType desc types of
    OpaqueType vts -> length vts
    OpaqueRecord fs -> sum $ map (entryPointSize types . snd) fs
    OpaqueSum vts _ -> length vts

compileInParam ::
  (Mem rep inner) =>
  FParam rep ->
  ImpM rep r op (Either Imp.Param ArrayDecl)
compileInParam fparam = case paramDec fparam of
  MemPrim bt ->
    pure $ Left $ Imp.ScalarParam name bt
  MemMem space ->
    pure $ Left $ Imp.MemParam name space
  MemArray bt shape _ (ArrayIn mem lmad) ->
    pure $ Right $ ArrayDecl name bt $ MemLoc mem (shapeDims shape) lmad
  MemAcc {} ->
    error "Functions may not have accumulator parameters."
  where
    name = paramName fparam

data ArrayDecl = ArrayDecl VName PrimType MemLoc

compileInParams ::
  (Mem rep inner) =>
  OpaqueTypes ->
  [FParam rep] ->
  Maybe [EntryParam] ->
  ImpM rep r op ([Imp.Param], [ArrayDecl], Maybe [((Name, Uniqueness), Imp.ExternalValue)])
compileInParams types params eparams = do
  (inparams, arrayds) <- partitionEithers <$> mapM compileInParam params
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

      mkExts (EntryParam v u et@(TypeOpaque desc) : epts) fparams =
        let signs = entryPointSignedness types et
            n = entryPointSize types et
            (fparams', rest) = splitAt n fparams
         in ( (v, u),
              Imp.OpaqueValue
                desc
                (catMaybes $ zipWith mkValueDesc fparams' signs)
            )
              : mkExts epts rest
      mkExts (EntryParam v u (TypeTransparent (ValueType s _ _)) : epts) (fparam : fparams) =
        maybeToList (((v, u),) . Imp.TransparentValue <$> mkValueDesc fparam s)
          ++ mkExts epts fparams
      mkExts _ _ = []

  pure
    ( inparams,
      arrayds,
      case eparams of
        Just eparams' ->
          let num_val_params = sum (map (entryPointSize types . entryParamType) eparams')
              (_ctx_params, val_params) = splitAt (length params - num_val_params) params
           in Just $ mkExts eparams' val_params
        Nothing -> Nothing
    )
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
  (Mem rep inner) =>
  OpaqueTypes ->
  [RetType rep] ->
  [EntryResult] ->
  [Maybe Imp.Param] ->
  ImpM rep r op [(Uniqueness, Imp.ExternalValue)]
compileExternalValues types orig_rts orig_epts maybe_params = do
  let (ctx_rts, val_rts) =
        splitAt
          (length orig_rts - sum (map (entryPointSize types . entryResultType) orig_epts))
          orig_rts

  let nthOut i = case maybeNth i maybe_params of
        Just (Just p) -> Imp.paramName p
        Just Nothing -> error $ "Output " ++ show i ++ " not a param."
        Nothing -> error $ "Param " ++ show i ++ " does not exist."

      mkValueDesc _ signedness (MemArray t shape _ ret) = do
        (mem, space) <-
          case ret of
            ReturnsNewBlock space j _lmad ->
              pure (nthOut j, space)
            ReturnsInBlock mem _lmad -> do
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

      mkExts i (EntryResult u et@(TypeOpaque desc) : epts) rets = do
        let signs = entryPointSignedness types et
            n = entryPointSize types et
            (rets', rest) = splitAt n rets
        vds <- forM (zip3 [i ..] signs rets') $ \(j, s, r) -> mkValueDesc j s r
        ((u, Imp.OpaqueValue desc vds) :) <$> mkExts (i + n) epts rest
      mkExts i (EntryResult u (TypeTransparent (ValueType s _ _)) : epts) (ret : rets) = do
        vd <- mkValueDesc i s ret
        ((u, Imp.TransparentValue vd) :) <$> mkExts (i + 1) epts rets
      mkExts _ _ _ = pure []

  mkExts (length ctx_rts) orig_epts val_rts

compileOutParams ::
  (Mem rep inner) =>
  OpaqueTypes ->
  [RetType rep] ->
  Maybe [EntryResult] ->
  ImpM rep r op (Maybe [(Uniqueness, Imp.ExternalValue)], [Imp.Param], [ValueDestination])
compileOutParams types orig_rts maybe_orig_epts = do
  (maybe_params, dests) <- mapAndUnzipM compileOutParam orig_rts
  evs <- case maybe_orig_epts of
    Just orig_epts ->
      Just <$> compileExternalValues types orig_rts orig_epts maybe_params
    Nothing -> pure Nothing
  pure (evs, catMaybes maybe_params, dests)

compileFunDef ::
  (Mem rep inner) =>
  OpaqueTypes ->
  FunDef rep ->
  ImpM rep r op ()
compileFunDef types (FunDef entry _ fname rettype params body) =
  local (\env -> env {envFunction = name_entry `mplus` Just fname}) $ do
    ((outparams, inparams, results, args), body') <- collect' compile
    let entry' = case (name_entry, results, args) of
          (Just name_entry', Just results', Just args') ->
            Just $ Imp.EntryPoint name_entry' results' args'
          _ ->
            Nothing
    emitFunction fname $ Imp.Function entry' outparams inparams body'
  where
    (name_entry, params_entry, ret_entry) = case entry of
      Nothing -> (Nothing, Nothing, Nothing)
      Just (x, y, z) -> (Just x, Just y, Just z)
    compile = do
      (inparams, arrayds, args) <- compileInParams types params params_entry
      (results, outparams, dests) <- compileOutParams types (map fst rettype) ret_entry
      addFParams params
      addArrays arrayds

      let Body _ stms ses = body
      compileStms (freeIn ses) stms $
        forM_ (zip dests ses) $
          \(d, SubExpRes _ se) -> copyDWIMDest d [] se []

      pure (outparams, inparams, results, args)

compileBody :: Pat (LetDec rep) -> Body rep -> ImpM rep r op ()
compileBody pat (Body _ stms ses) = do
  dests <- destinationFromPat pat
  compileStms (freeIn ses) stms $
    forM_ (zip dests ses) $
      \(d, SubExpRes _ se) -> copyDWIMDest d [] se []

compileBody' :: [Param dec] -> Body rep -> ImpM rep r op ()
compileBody' params (Body _ stms ses) =
  compileStms (freeIn ses) stms $
    forM_ (zip params ses) $
      \(param, SubExpRes _ se) -> copyDWIM (paramName param) [] se []

compileLoopBody :: (Typed dec) => [Param dec] -> Body rep -> ImpM rep r op ()
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
          pure $ emit $ Imp.SetScalar (paramName p) $ Imp.var tmp pt
        Mem space | Var v <- se -> do
          emit $ Imp.DeclareMem tmp space
          emit $ Imp.SetMem tmp v space
          pure $ emit $ Imp.SetMem (paramName p) tmp space
        _ -> pure $ pure ()
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
          collect $
            compileExp pat e
      (live_after, bs_code) <- collect' $ compileStms' (patternAllocs pat <> allocs) bs
      let dies_here v =
            (v `notNameIn` live_after) && (v `nameIn` freeIn e_code)
          to_free = S.filter (dies_here . fst) allocs

      emit e_code
      mapM_ (emit . uncurry Imp.Free) to_free
      emit bs_code

      pure $ freeIn e_code <> live_after
    compileStms' _ [] = do
      code <- collect m
      emit code
      pure $ freeIn code <> alive_after_stms

    patternAllocs = S.fromList . mapMaybe isMemPatElem . patElems
    isMemPatElem pe = case patElemType pe of
      Mem space -> Just (patElemName pe, space)
      _ -> Nothing

compileExp :: Pat (LetDec rep) -> Exp rep -> ImpM rep r op ()
compileExp pat e = do
  ec <- asks envExpCompiler
  ec pat e

-- | Generate an expression that is true if the subexpressions match
-- the case pasttern.
caseMatch :: [SubExp] -> [Maybe PrimValue] -> Imp.TExp Bool
caseMatch ses vs = foldl (.&&.) true (zipWith cmp ses vs)
  where
    cmp se (Just v) = isBool $ toExp' (primValueType v) se ~==~ ValueExp v
    cmp _ Nothing = true

defCompileExp ::
  (Mem rep inner) =>
  Pat (LetDec rep) ->
  Exp rep ->
  ImpM rep r op ()
defCompileExp pat (Match ses cases defbody _) =
  foldr f (compileBody pat defbody) cases
  where
    f (Case vs body) = sIf (caseMatch ses vs) (compileBody pat body)
defCompileExp pat (Apply fname args _ _) = do
  dest <- destinationFromPat pat
  targets <- funcallTargets dest
  args' <- catMaybes <$> mapM compileArg args
  emit $ Imp.Call targets fname args'
  where
    compileArg (se, _) = do
      t <- subExpType se
      case (se, t) of
        (_, Prim pt) -> pure $ Just $ Imp.ExpArg $ toExp' pt se
        (Var v, Mem {}) -> pure $ Just $ Imp.MemArg v
        _ -> pure Nothing
defCompileExp pat (BasicOp op) = defCompileBasicOp pat op
defCompileExp pat (Loop merge form body) = do
  attrs <- askAttrs
  when ("unroll" `inAttrs` attrs) $
    warn (noLoc :: SrcLoc) [] "#[unroll] on loop with unknown number of iterations." -- FIXME: no location.
  dFParams params
  forM_ merge $ \(p, se) ->
    when ((== 0) $ arrayRank $ paramType p) $
      copyDWIM (paramName p) [] se []

  let doBody = compileLoopBody params body

  case form of
    ForLoop i _ bound -> do
      bound' <- toExp bound
      sFor' i bound' doBody
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

tracePrim :: T.Text -> PrimType -> SubExp -> ImpM rep r op ()
tracePrim s t se =
  emit . Imp.TracePrint $
    ErrorMsg [ErrorString (s <> ": "), ErrorVal t (toExp' t se), ErrorString "\n"]

traceArray :: T.Text -> PrimType -> Shape -> SubExp -> ImpM rep r op ()
traceArray s t shape se = do
  emit . Imp.TracePrint $ ErrorMsg [ErrorString (s <> ": ")]
  sLoopNest shape $ \is -> do
    arr_elem <- dPrim "arr_elem" t
    copyDWIMFix (tvVar arr_elem) [] se is
    emit . Imp.TracePrint $ ErrorMsg [ErrorVal t (untyped (tvExp arr_elem)), " "]
  emit . Imp.TracePrint $ ErrorMsg ["\n"]

defCompileBasicOp ::
  (Mem rep inner) =>
  Pat (LetDec rep) ->
  BasicOp ->
  ImpM rep r op ()
defCompileBasicOp (Pat [pe]) (SubExp se) =
  copyDWIM (patElemName pe) [] se []
defCompileBasicOp (Pat [pe]) (Opaque op se) = do
  copyDWIM (patElemName pe) [] se []
  case op of
    OpaqueNil -> pure ()
    OpaqueTrace s -> sComment ("Trace: " <> s) $ do
      se_t <- subExpType se
      case se_t of
        Prim t -> tracePrim s t se
        Array t shape _ -> traceArray s t shape se
        _ ->
          warn [mempty :: SrcLoc] mempty $
            s <> ": cannot trace value of this (core) type: " <> prettyText se_t
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
      copyDWIM (patElemName pe) [] (Var src) $ map (DimFix . pe64) idxs
defCompileBasicOp _ Index {} =
  pure ()
defCompileBasicOp (Pat [pe]) (Update safety _ slice se) =
  case safety of
    Unsafe -> write
    Safe -> sWhen (inBounds slice' dims) write
  where
    slice' = fmap pe64 slice
    dims = map pe64 $ arrayDims $ patElemType pe
    write = sUpdate (patElemName pe) slice' se
defCompileBasicOp _ FlatIndex {} =
  pure ()
defCompileBasicOp (Pat [pe]) (FlatUpdate _ slice v) = do
  pe_loc <- entryArrayLoc <$> lookupArray (patElemName pe)
  v_loc <- entryArrayLoc <$> lookupArray v
  let pe_loc' = flatSliceMemLoc pe_loc $ fmap pe64 slice
  copy (elemType (patElemType pe)) pe_loc' v_loc
defCompileBasicOp (Pat [pe]) (Replicate shape se)
  | Acc {} <- patElemType pe = pure ()
  | shape == mempty =
      copyDWIM (patElemName pe) [] se []
  | otherwise =
      sLoopNest shape $ \is -> copyDWIMFix (patElemName pe) is se []
defCompileBasicOp _ Scratch {} =
  pure ()
defCompileBasicOp (Pat [pe]) (Iota n e s it) = do
  e' <- toExp e
  s' <- toExp s
  sFor "i" (pe64 n) $ \i -> do
    let i' = sExt it $ untyped i
    x <-
      dPrimV "x" . TPrimExp $
        BinOpExp (Add it OverflowUndef) e' $
          BinOpExp (Mul it OverflowUndef) i' s'
    copyDWIMFix (patElemName pe) [i] (Var (tvVar x)) []
defCompileBasicOp (Pat [pe]) (Manifest _ src) =
  copyDWIM (patElemName pe) [] (Var src) []
defCompileBasicOp (Pat [pe]) (Concat i (x :| ys) _) = do
  offs_glb <- dPrimV "tmp_offs" 0

  forM_ (x : ys) $ \y -> do
    y_dims <- arrayDims <$> lookupType y
    let rows = case drop i y_dims of
          [] -> error $ "defCompileBasicOp Concat: empty array shape for " ++ prettyString y
          r : _ -> pe64 r
        skip_dims = take i y_dims
        sliceAllDim d = DimSlice 0 d 1
        skip_slices = map (sliceAllDim . pe64) skip_dims
        destslice = skip_slices ++ [DimSlice (tvExp offs_glb) rows 1]
    copyDWIM (patElemName pe) destslice (Var y) []
    offs_glb <-- tvExp offs_glb + rows
defCompileBasicOp (Pat [pe]) (ArrayLit es _)
  | Just vs@(v : _) <- mapM isLiteral es = do
      dest_mem <- entryArrayLoc <$> lookupArray (patElemName pe)
      let t = primValueType v
      static_array <- newVNameForFun "static_array"
      emit $ Imp.DeclareArray static_array t $ Imp.ArrayValues vs
      let static_src =
            MemLoc static_array [intConst Int64 $ fromIntegral $ length es] $
              LMAD.iota 0 [fromIntegral $ length es]
      addVar static_array $ MemVar Nothing $ MemEntry DefaultSpace
      copy t dest_mem static_src
  | otherwise =
      forM_ (zip [0 ..] es) $ \(i, e) ->
        copyDWIMFix (patElemName pe) [fromInteger i] e []
  where
    isLiteral (Constant v) = Just v
    isLiteral _ = Nothing
defCompileBasicOp _ Rearrange {} =
  pure ()
defCompileBasicOp _ Reshape {} =
  pure ()
defCompileBasicOp _ (UpdateAcc acc is vs) = sComment "UpdateAcc" $ do
  -- We are abusing the comment mechanism to wrap the operator in
  -- braces when we end up generating code.  This is necessary because
  -- we might otherwise end up declaring lambda parameters (if any)
  -- multiple times, as they are duplicated every time we do an
  -- UpdateAcc for the same accumulator.
  let is' = map pe64 is

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
      ++ prettyString pat
      ++ "\nfor expression\n  "
      ++ prettyString e

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
addFParams :: (Mem rep inner) => [FParam rep] -> ImpM rep r op ()
addFParams = mapM_ addFParam
  where
    addFParam fparam =
      addVar (paramName fparam) $
        memBoundToVarEntry Nothing $
          noUniquenessReturns $
            paramDec fparam

-- | Another hack.
addLoopVar :: VName -> IntType -> ImpM rep r op ()
addLoopVar i it = addVar i $ ScalarVar Nothing $ ScalarEntry $ IntType it

dVars ::
  (Mem rep inner) =>
  Maybe (Exp rep) ->
  [PatElem (LetDec rep)] ->
  ImpM rep r op ()
dVars e = mapM_ dVar
  where
    dVar = dScope e . scopeOfPatElem

dFParams :: (Mem rep inner) => [FParam rep] -> ImpM rep r op ()
dFParams = dScope Nothing . scopeOfFParams

dLParams :: (Mem rep inner) => [LParam rep] -> ImpM rep r op ()
dLParams = dScope Nothing . scopeOfLParams

dPrimVol :: String -> PrimType -> Imp.TExp t -> ImpM rep r op (TV t)
dPrimVol name t e = do
  name' <- newVName name
  emit $ Imp.DeclareScalar name' Imp.Volatile t
  addVar name' $ ScalarVar Nothing $ ScalarEntry t
  name' <~~ untyped e
  pure $ TV name' t

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
  pure $ TV name' t

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
  pure name'

dPrimVE :: String -> Imp.TExp t -> ImpM rep r op (Imp.TExp t)
dPrimVE name e = do
  name' <- dPrim name $ primExpType $ untyped e
  name' <-- e
  pure $ tvExp name'

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
memBoundToVarEntry e (MemArray bt shape _ (ArrayIn mem lmad)) =
  let location = MemLoc mem (shapeDims shape) lmad
   in ArrayVar
        e
        ArrayEntry
          { entryArrayLoc = location,
            entryArrayElemType = bt
          }

infoDec ::
  (Mem rep inner) =>
  NameInfo rep ->
  MemInfo SubExp NoUniqueness MemBind
infoDec (LetName dec) = letDecMem dec
infoDec (FParamName dec) = noUniquenessReturns dec
infoDec (LParamName dec) = dec
infoDec (IndexName it) = MemPrim $ IntType it

dInfo ::
  (Mem rep inner) =>
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
      pure ()
    AccVar {} ->
      pure ()
  addVar name entry

dScope ::
  (Mem rep inner) =>
  Maybe (Exp rep) ->
  Scope rep ->
  ImpM rep r op ()
dScope e = mapM_ (uncurry $ dInfo e) . M.toList

dArray :: VName -> PrimType -> ShapeBase SubExp -> VName -> LMAD -> ImpM rep r op ()
dArray name pt shape mem lmad =
  addVar name $ ArrayVar Nothing $ ArrayEntry location pt
  where
    location = MemLoc mem (shapeDims shape) lmad

everythingVolatile :: ImpM rep r op a -> ImpM rep r op a
everythingVolatile = local $ \env -> env {envVolatility = Imp.Volatile}

funcallTargets :: [ValueDestination] -> ImpM rep r op [VName]
funcallTargets dests =
  concat <$> mapM funcallTarget dests
  where
    funcallTarget (ScalarDestination name) =
      pure [name]
    funcallTarget (ArrayDestination _) =
      pure []
    funcallTarget (MemoryDestination name) =
      pure [name]

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
  -- | Compile to an 'Imp.Exp', where the type (which must still be a
  -- primitive) is deduced monadically.
  toExp :: a -> ImpM rep r op Imp.Exp

  -- | Compile where we know the type in advance.
  toExp' :: PrimType -> a -> Imp.Exp

instance ToExp SubExp where
  toExp (Constant v) =
    pure $ Imp.ValueExp v
  toExp (Var v) =
    lookupVar v >>= \case
      ScalarVar _ (ScalarEntry pt) ->
        pure $ Imp.var v pt
      _ -> error $ "toExp SubExp: SubExp is not a primitive type: " ++ prettyString v

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
  pure $ maybe "" (<> ".") fname <> nameFromString s

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
  pure a

lookupVar :: VName -> ImpM rep r op (VarEntry rep)
lookupVar name = do
  res <- gets $ M.lookup name . stateVTable
  case res of
    Just entry -> pure entry
    _ -> error $ "Unknown variable: " ++ prettyString name

lookupArray :: VName -> ImpM rep r op ArrayEntry
lookupArray name = do
  res <- lookupVar name
  case res of
    ArrayVar _ entry -> pure entry
    _ -> error $ "ImpGen.lookupArray: not an array: " ++ prettyString name

lookupMemory :: VName -> ImpM rep r op MemEntry
lookupMemory name = do
  res <- lookupVar name
  case res of
    MemVar _ entry -> pure entry
    _ -> error $ "Unknown memory block: " ++ prettyString name

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
          error $ "Accumulator with no arrays: " ++ prettyString name
        Just (arrs@(arr : _), Just (op, _)) -> do
          space <- lookupArraySpace arr
          let (i_params, ps) = splitAt (length is) $ lambdaParams op
          zipWithM_ dPrimV_ (map paramName i_params) is
          pure
            ( acc,
              space,
              arrs,
              map pe64 (shapeDims ispace),
              Just op {lambdaParams = ps}
            )
        Just (arrs@(arr : _), Nothing) -> do
          space <- lookupArraySpace arr
          pure (acc, space, arrs, map pe64 (shapeDims ispace), Nothing)
        Nothing ->
          error $ "ImpGen.lookupAcc: unlisted accumulator: " ++ prettyString name
    _ -> error $ "ImpGen.lookupAcc: not an accumulator: " ++ prettyString name

destinationFromPat :: Pat (LetDec rep) -> ImpM rep r op [ValueDestination]
destinationFromPat = mapM inspect . patElems
  where
    inspect pe = do
      let name = patElemName pe
      entry <- lookupVar name
      case entry of
        ArrayVar _ (ArrayEntry MemLoc {} _) ->
          pure $ ArrayDestination Nothing
        MemVar {} ->
          pure $ MemoryDestination name
        ScalarVar {} ->
          pure $ ScalarDestination name
        AccVar {} ->
          pure $ ArrayDestination Nothing

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
fullyIndexArray' (MemLoc mem _ lmad) indices = do
  space <- entryMemSpace <$> lookupMemory mem
  pure
    ( mem,
      space,
      elements $ LMAD.index lmad indices
    )

-- More complicated read/write operations that use index functions.

copy :: CopyCompiler rep r op
copy
  bt
  dst@(MemLoc dst_name _ dst_ixfn@dst_lmad)
  src@(MemLoc src_name _ src_ixfn@src_lmad) = do
    -- If we can statically determine that the two index-functions
    -- are equivalent, don't do anything
    unless (dst_name == src_name && dst_ixfn `LMAD.equivalent` src_ixfn)
      $
      -- It's also possible that we can dynamically determine that the two
      -- index-functions are equivalent.
      sUnless
        ( fromBool (dst_name == src_name)
            .&&. LMAD.dynamicEqualsLMAD dst_lmad src_lmad
        )
      $ do
        -- If none of the above is true, actually do the copy
        cc <- asks envCopyCompiler
        cc bt dst src

lmadCopy :: CopyCompiler rep r op
lmadCopy t dstloc srcloc = do
  let dstmem = memLocName dstloc
      srcmem = memLocName srcloc
      dstlmad = memLocLMAD dstloc
      srclmad = memLocLMAD srcloc
  srcspace <- entryMemSpace <$> lookupMemory srcmem
  dstspace <- entryMemSpace <$> lookupMemory dstmem
  emit $
    Imp.Copy
      t
      (elements <$> LMAD.shape dstlmad)
      (dstmem, dstspace)
      ( LMAD.offset $ elements <$> dstlmad,
        map LMAD.ldStride $ LMAD.dims $ elements <$> dstlmad
      )
      (srcmem, srcspace)
      ( LMAD.offset $ elements <$> srclmad,
        map LMAD.ldStride $ LMAD.dims $ elements <$> srclmad
      )

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
        let destslice' = fullSliceNum (map pe64 destshape) destslice
            srcslice' = fullSliceNum (map pe64 srcshape) srcslice
            destrank = length $ sliceDims destslice'
            srcrank = length $ sliceDims srcslice'
            destlocation' = sliceMemLoc destlocation destslice'
            srclocation' = sliceMemLoc srclocation srcslice'
        if destrank /= srcrank
          then
            error $
              "copyArrayDWIM: cannot copy to "
                ++ prettyString (memLocName destlocation)
                ++ " from "
                ++ prettyString (memLocName srclocation)
                ++ " because ranks do not match ("
                ++ prettyString destrank
                ++ " vs "
                ++ prettyString srcrank
                ++ ")"
          else
            if destlocation' == srclocation'
              then pure mempty -- Copy would be no-op.
              else collect $ copy bt destlocation' srclocation'

-- Like 'copyDWIM', but the target is a 'ValueDestination' instead of
-- a variable name.
copyDWIMDest ::
  ValueDestination ->
  [DimIndex (Imp.TExp Int64)] ->
  SubExp ->
  [DimIndex (Imp.TExp Int64)] ->
  ImpM rep r op ()
copyDWIMDest _ _ (Constant v) (_ : _) =
  error $
    unwords ["copyDWIMDest: constant source", prettyString v, "cannot be indexed."]
copyDWIMDest pat dest_slice (Constant v) [] =
  case mapM dimFix dest_slice of
    Nothing ->
      error $
        unwords ["copyDWIMDest: constant source", prettyString v, "with slice destination."]
    Just dest_is ->
      case pat of
        ScalarDestination name ->
          emit $ Imp.SetScalar name $ Imp.ValueExp v
        MemoryDestination {} ->
          error $
            unwords ["copyDWIMDest: constant source", prettyString v, "cannot be written to memory destination."]
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
        unwords ["copyDWIMDest: cannot write", prettyString src, "to memory destination."]
    (_, MemVar {}) ->
      error $
        unwords ["copyDWIMDest: source", prettyString src, "is a memory block."]
    (_, ScalarVar _ (ScalarEntry _))
      | not $ null src_slice ->
          error $
            unwords ["copyDWIMDest: prim-typed source", prettyString src, "with slice", prettyString src_slice]
    (ScalarDestination name, _)
      | not $ null dest_slice ->
          error $
            unwords ["copyDWIMDest: prim-typed target", prettyString name, "with slice", prettyString dest_slice]
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
                prettyString name,
                "and array-typed source",
                prettyString src,
                "of shape",
                prettyString (entryArrayShape arr),
                "sliced with",
                prettyString src_slice
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
                prettyString src,
                "with slice",
                prettyString dest_slice
              ]
    (ArrayDestination Nothing, _) ->
      pure () -- Nothing to do; something else set some memory
      -- somewhere.
    (_, AccVar {}) ->
      pure () -- Nothing to do; accumulators are phantoms.

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
          ArrayVar _ (ArrayEntry (MemLoc mem shape lmad) _) ->
            ArrayDestination $ Just $ MemLoc mem shape lmad
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

-- | @compileAlloc pat size space@ allocates @n@ bytes of memory in
-- @space@, writing the result to @pat@, which must contain a single
-- memory-typed element.
compileAlloc ::
  (Mem rep inner) => Pat (LetDec rep) -> SubExp -> Space -> ImpM rep r op ()
compileAlloc (Pat [mem]) e space = do
  let e' = Imp.bytes $ pe64 e
  allocator <- asks $ M.lookup space . envAllocCompilers
  case allocator of
    Nothing -> emit $ Imp.Allocate (patElemName mem) e' space
    Just allocator' -> allocator' (patElemName mem) e'
compileAlloc pat _ _ =
  error $ "compileAlloc: Invalid pattern: " ++ prettyString pat

-- | The number of bytes needed to represent the array in a
-- straightforward contiguous format, as an t'Int64' expression.
typeSize :: Type -> Count Bytes (Imp.TExp Int64)
typeSize t =
  Imp.bytes $ primByteSize (elemType t) * product (map pe64 (arrayDims t))

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
        t -> error $ "sFor': bound " ++ prettyString bound ++ " is of type " ++ prettyString t
  addLoopVar i it
  body' <- collect body
  emit $ Imp.For i bound body'

sFor :: String -> Imp.TExp t -> (Imp.TExp t -> ImpM rep r op ()) -> ImpM rep r op ()
sFor i bound body = do
  i' <- newVName i
  sFor' i' (untyped bound) $
    body $
      TPrimExp $
        Imp.var i' $
          primExpType $
            untyped bound

sWhile :: Imp.TExp Bool -> ImpM rep r op () -> ImpM rep r op ()
sWhile cond body = do
  body' <- collect body
  emit $ Imp.While cond body'

-- | Execute a code generation action, wrapping the generated code
-- within a 'Imp.Comment' with the given description.
sComment :: T.Text -> ImpM rep r op () -> ImpM rep r op ()
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
sWhen cond tbranch = sIf cond tbranch (pure ())

sUnless :: Imp.TExp Bool -> ImpM rep r op () -> ImpM rep r op ()
sUnless cond = sIf cond (pure ())

sOp :: op -> ImpM rep r op ()
sOp = emit . Imp.Op

sDeclareMem :: String -> Space -> ImpM rep r op VName
sDeclareMem name space = do
  name' <- newVName name
  emit $ Imp.DeclareMem name' space
  addVar name' $ MemVar Nothing $ MemEntry space
  pure name'

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
  pure name'

sArray :: String -> PrimType -> ShapeBase SubExp -> VName -> LMAD -> ImpM rep r op VName
sArray name bt shape mem lmad = do
  name' <- newVName name
  dArray name' bt shape mem lmad
  pure name'

-- | Declare an array in row-major order in the given memory block.
sArrayInMem :: String -> PrimType -> ShapeBase SubExp -> VName -> ImpM rep r op VName
sArrayInMem name pt shape mem =
  sArray name pt shape mem $
    LMAD.iota 0 $
      map (isInt64 . primExpFromSubExp int64) $
        shapeDims shape

-- | Like 'sAllocArray', but permute the in-memory representation of the indices as specified.
sAllocArrayPerm :: String -> PrimType -> ShapeBase SubExp -> Space -> [Int] -> ImpM rep r op VName
sAllocArrayPerm name pt shape space perm = do
  let permuted_dims = rearrangeShape perm $ shapeDims shape
  mem <- sAlloc (name ++ "_mem") (typeSize (Array pt shape NoUniqueness)) space
  let iota_lmad = LMAD.iota 0 $ map (isInt64 . primExpFromSubExp int64) permuted_dims
  sArray name pt shape mem $
    LMAD.permute iota_lmad $
      rearrangeInverse perm

-- | Uses linear/iota index function.
sAllocArray :: String -> PrimType -> ShapeBase SubExp -> Space -> ImpM rep r op VName
sAllocArray name pt shape space =
  sAllocArrayPerm name pt shape space [0 .. shapeRank shape - 1]

-- | Uses linear/iota index function.
sStaticArray :: String -> PrimType -> Imp.ArrayContents -> ImpM rep r op VName
sStaticArray name pt vs = do
  let num_elems = case vs of
        Imp.ArrayValues vs' -> length vs'
        Imp.ArrayZeros n -> fromIntegral n
      shape = Shape [intConst Int64 $ toInteger num_elems]
  mem <- newVNameForFun $ name ++ "_mem"
  emit $ Imp.DeclareArray mem pt vs
  addVar mem $ MemVar Nothing $ MemEntry DefaultSpace
  sArray name pt shape mem $ LMAD.iota 0 [fromIntegral num_elems]

sWrite :: VName -> [Imp.TExp Int64] -> Imp.Exp -> ImpM rep r op ()
sWrite arr is v = do
  (mem, space, offset) <- fullyIndexArray arr is
  vol <- asks envVolatility
  emit $ Imp.Write mem offset (primExpType v) space vol v

sUpdate :: VName -> Slice (Imp.TExp Int64) -> SubExp -> ImpM rep r op ()
sUpdate arr slice v = copyDWIM arr (unSlice slice) v []

-- | Create a sequential 'Imp.For' loop covering a space of the given
-- shape.  The function is calling with the indexes for a given
-- iteration.
sLoopSpace ::
  [Imp.TExp t] ->
  ([Imp.TExp t] -> ImpM rep r op ()) ->
  ImpM rep r op ()
sLoopSpace = nest []
  where
    nest is [] f = f $ reverse is
    nest is (d : ds) f = sFor "nest_i" d $ \i -> nest (i : is) ds f

sLoopNest ::
  Shape ->
  ([Imp.TExp Int64] -> ImpM rep r op ()) ->
  ImpM rep r op ()
sLoopNest = sLoopSpace . map pe64 . shapeDims

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
  emitFunction fname $ Imp.Function Nothing outputs inputs body
  where
    addParam (Imp.MemParam name space) =
      addVar name $ MemVar Nothing $ MemEntry space
    addParam (Imp.ScalarParam name bt) =
      addVar name $ ScalarVar Nothing $ ScalarEntry bt
    newFunction env = env {envFunction = Just fname}

-- Fish out those top-level declarations in the constant
-- initialisation code that are free in the functions.
constParams :: Names -> Imp.Code a -> (DL.DList Imp.Param, Imp.Code a)
constParams used (x Imp.:>>: y) =
  constParams used x <> constParams used y
constParams used (Imp.DeclareMem name space)
  | name `nameIn` used =
      ( DL.singleton $ Imp.MemParam name space,
        mempty
      )
constParams used (Imp.DeclareScalar name _ t)
  | name `nameIn` used =
      ( DL.singleton $ Imp.ScalarParam name t,
        mempty
      )
constParams used s@(Imp.DeclareArray name _ _)
  | name `nameIn` used =
      ( DL.singleton $ Imp.MemParam name DefaultSpace,
        s
      )
constParams _ s =
  (mempty, s)

-- | Generate constants that get put outside of all functions.  Will
-- be executed at program startup.  Action must return the names that
-- should should be made available.  This one has real sharp edges. Do
-- not use inside 'subImpM'.  Do not use any variable from the context.
genConstants :: ImpM rep r op (Names, a) -> ImpM rep r op a
genConstants m = do
  ((avail, a), code) <- collect' m
  let consts = uncurry Imp.Constants $ first DL.toList $ constParams avail code
  modify $ \s -> s {stateConstants = stateConstants s <> consts}
  pure a

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
