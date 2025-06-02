{-# LANGUAGE LambdaCase #-}

-- | Code generation for ImpCode with WebGPU.
module Futhark.CodeGen.ImpGen.WebGPU
  ( compileProg,
    Warnings,
  )
where

import Control.Monad (forM, forM_, liftM2, liftM3, unless, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS
import Control.Monad.Trans.State qualified as State
import Data.Bifunctor (first, second)
import Data.Bits qualified as Bits
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Futhark.CodeGen.ImpCode.GPU qualified as ImpGPU
import Futhark.CodeGen.ImpCode.WebGPU
import Futhark.CodeGen.ImpGen.GPU qualified as ImpGPU
import Futhark.CodeGen.RTS.WGSL qualified as RTS
import Futhark.Error (compilerLimitation)
import Futhark.IR.GPUMem qualified as F
import Futhark.MonadFreshNames
import Futhark.Util (convFloat, nubOrd, zEncodeText)
import Futhark.Util.Pretty (align, docText, indent, pretty, (</>))
import Language.Futhark.Warnings (Warnings)
import Language.WGSL qualified as WGSL

-- State carried during WebGPU translation.
data WebGPUS = WebGPUS
  { -- | Accumulated code.
    wsCode :: T.Text,
    wsSizes :: M.Map Name SizeClass,
    wsMacroDefs :: [(Name, KernelConstExp)],
    -- | Interface of kernels already generated into wsCode.
    wsKernels :: [(WGSL.Ident, KernelInterface)],
    wsNextBindSlot :: Int,
    -- | TODO comment on this
    wsDevFuns :: S.Set Name,
    wsFuns :: ImpGPU.Functions ImpGPU.HostOp,
    wsFunsMayFail :: S.Set Name
  }

-- The monad in which we perform the overall translation.
type WebGPUM = State.State WebGPUS

addSize :: Name -> SizeClass -> WebGPUM ()
addSize key sclass =
  State.modify $ \s -> s {wsSizes = M.insert key sclass $ wsSizes s}

addMacroDef :: Name -> KernelConstExp -> WebGPUM ()
addMacroDef key e =
  State.modify $ \s -> s {wsMacroDefs = (key, e) : wsMacroDefs s}

addCode :: T.Text -> WebGPUM ()
addCode code =
  State.modify $ \s -> s {wsCode = wsCode s <> code}

newtype KernelR = KernelR {krKernel :: ImpGPU.Kernel}

data KernelState = KernelState
  { -- Kernel declarations and body
    ksDecls :: [WGSL.Declaration],
    ksInits :: [WGSL.Stmt],
    ksBody :: [WGSL.Stmt],
    -- | Identifier replacement map. We have to rename some identifiers; when
    -- translating Imp Code and PrimExps this map is consulted to respect the
    -- renaming.
    ksNameReplacements :: M.Map WGSL.Ident WGSL.Ident,
    -- These describe the kernel interface.
    ksOverrides :: [WGSL.Ident],
    ksBlockDims :: [(Int, WGSL.Ident, Bool)],
    -- TODO: Might be nice to combine sharedMem and atomicMem into some more
    -- general information about memory in scope
    ksSharedMem :: [(WGSL.Ident, Exp)],
    ksAtomicMem :: [WGSL.Ident],
    ksScalars :: [WGSL.PrimType],
    ksBindSlots :: [Int]
  }

type KernelM = RWST KernelR () KernelState WebGPUM

addRename :: WGSL.Ident -> WGSL.Ident -> KernelM ()
addRename old new = modify $
  \s -> s {ksNameReplacements = M.insert old new (ksNameReplacements s)}

-- | Some names generated are unique in the scope of a single kernel but are
-- translated to module-scope identifiers in WGSL. This modifies an identifier
-- to be unique in that scope.
mkGlobalIdent :: WGSL.Ident -> KernelM WGSL.Ident
mkGlobalIdent ident = do
  kernelName <- asks (textToIdent . nameToText . ImpGPU.kernelName . krKernel)
  pure $ kernelName <> "_" <> ident

addDecl :: WGSL.Declaration -> KernelM ()
addDecl decl = modify $ \s -> s {ksDecls = ksDecls s ++ [decl]}

prependDecl :: WGSL.Declaration -> KernelM ()
prependDecl decl = modify $ \s -> s {ksDecls = decl : ksDecls s}

addInitStmt :: WGSL.Stmt -> KernelM ()
addInitStmt stmt = modify $ \s -> s {ksInits = ksInits s ++ [stmt]}

addBodyStmt :: WGSL.Stmt -> KernelM ()
addBodyStmt stmt = modify $ \s -> s {ksBody = ksBody s ++ [stmt]}

-- | Produces an identifier for the given name, respecting the name replacements
-- map.
getIdent :: (F.Pretty a) => a -> KernelM WGSL.Ident
getIdent name = gets (M.findWithDefault t t . ksNameReplacements)
  where
    t = zEncodeText $ prettyText name

-- | Get a new, unused binding index and add it to the list of bind slots used
-- by the current kernel.
assignBindSlot :: KernelM Int
assignBindSlot = do
  wState <- lift State.get
  let slot = wsNextBindSlot wState
  modify $ \s -> s {ksBindSlots = ksBindSlots s ++ [slot]}
  lift $ State.put (wState {wsNextBindSlot = slot + 1})
  pure slot

-- | Add an override declaration to the current kernel's interface and into the
-- module.
addOverride :: WGSL.Ident -> WGSL.Typ -> Maybe WGSL.Exp -> KernelM ()
addOverride ident typ e = do
  addDecl $ WGSL.OverrideDecl ident typ e
  modify $ \s -> s {ksOverrides = ksOverrides s ++ [ident]}

-- | Register an override identifier as describing the given dimension of the
-- block size of the current kernel.
addBlockDim :: Int -> WGSL.Ident -> Bool -> KernelM ()
addBlockDim dim ident dynamic =
  modify $ \s -> s {ksBlockDims = (dim, ident, dynamic) : ksBlockDims s}

-- | Registers an override identifier as describing the size of a shared memory
-- buffer, with the expression being evaluated to get the size when launching
-- the kernel.
addSharedMem :: WGSL.Ident -> Exp -> KernelM ()
addSharedMem ident e =
  modify $ \s -> s {ksSharedMem = (ident, e) : ksSharedMem s}

addAtomicMem :: WGSL.Ident -> KernelM ()
addAtomicMem ident = modify $ \s -> s {ksAtomicMem = ident : ksAtomicMem s}

-- | Whether the identifier is the name of a shared memory allocation.
-- TODO: Should probably store the allocation name in the state instead of
-- reconstructing the _size name here.
isShared :: WGSL.Ident -> KernelM Bool
isShared ident = any (\(sz, _) -> sz == ident <> "_size") <$> gets ksSharedMem

isAtomic :: WGSL.Ident -> KernelM Bool
isAtomic ident = elem ident <$> gets ksAtomicMem

-- | Add a scalar struct field.
addScalar :: WGSL.PrimType -> KernelM ()
addScalar typ = modify $ \s -> s {ksScalars = ksScalars s ++ [typ]}

entryParams :: [WGSL.Param]
entryParams =
  [ WGSL.Param
      "workgroup_id"
      (WGSL.Prim (WGSL.Vec3 WGSL.UInt32))
      [WGSL.Attrib "builtin" [WGSL.VarExp "workgroup_id"]],
    WGSL.Param
      "local_id"
      (WGSL.Prim (WGSL.Vec3 WGSL.UInt32))
      [WGSL.Attrib "builtin" [WGSL.VarExp "local_invocation_id"]]
  ]

builtinLockstepWidth :: KernelM WGSL.Ident
builtinLockstepWidth = mkGlobalIdent "lockstep_width"

builtinBlockSize :: Int -> KernelM WGSL.Ident
builtinBlockSize 0 = mkGlobalIdent "block_size_x"
builtinBlockSize 1 = mkGlobalIdent "block_size_y"
builtinBlockSize 2 = mkGlobalIdent "block_size_z"
builtinBlockSize _ = error "invalid block size dimension"

-- Main function for translating an ImpGPU kernel to a WebGPU kernel.
genKernel :: ImpGPU.Kernel -> WebGPUM (KernelName, [(Exp, PrimType)])
genKernel kernel = do
  let initial =
        KernelState
          { ksDecls = mempty,
            ksInits = mempty,
            ksBody = mempty,
            ksNameReplacements = mempty,
            ksOverrides = mempty,
            ksBlockDims = mempty,
            ksAtomicMem = mempty,
            ksSharedMem = mempty,
            ksScalars = mempty,
            ksBindSlots = mempty
          }

  ((), s, ()) <- runRWST gen (KernelR kernel) initial

  addCode $ docText $ WGSL.prettyDecls (ksDecls s)
  addCode "\n\n"

  let name = nameToText $ ImpGPU.kernelName kernel
  let blockDimNames = [n | (_, n, _) <- ksBlockDims s]
  let attribs =
        [ WGSL.Attrib "compute" [],
          WGSL.Attrib "workgroup_size" (map WGSL.VarExp blockDimNames)
        ]
  let wgslFun =
        WGSL.Function
          { WGSL.funName = textToIdent name,
            WGSL.funAttribs = attribs,
            WGSL.funParams = entryParams,
            WGSL.funOutput = Nothing,
            WGSL.funBody = WGSL.stmts (ksInits s ++ ksBody s)
          }
  addCode $ prettyText wgslFun

  let (offsets, _align, size) =
        -- dummy layout with single i32 instead of empty structs
        fromMaybe ([], 4, 4) (WGSL.structLayout (ksScalars s))
  let dynamicBlockDims = [(dim, n) | (dim, n, True) <- ksBlockDims s]
  let (sharedMemOverrides, sharedMemExps) = unzip $ ksSharedMem s
  let interface =
        KernelInterface
          { safety = SafetyNone, -- TODO
            scalarsOffsets = offsets,
            scalarsSize = size,
            scalarsBindSlot = head (ksBindSlots s),
            memBindSlots = tail (ksBindSlots s),
            overrideNames = ksOverrides s,
            dynamicBlockDims = dynamicBlockDims,
            sharedMemoryOverrides = sharedMemOverrides,
            gpuProgram = T.empty
          }
  State.modify $ \ws -> ws {wsKernels = wsKernels ws <> [(name, interface)]}
  pure (nameFromText name, map (,IntType Int32) sharedMemExps)
  where
    gen = do
      genConstAndBuiltinDecls
      genScalarDecls
      genMemoryDecls

      genDeviceFuns $ ImpGPU.kernelBody kernel

      -- FIXME: This is only required to work around a Chrome bug that otherwise
      -- causes the shader to fail compilation if the kernel never accesses the
      -- lockstep width. See `gpu_create_kernel` in `rts/c/backends/webgpu.h`
      -- for more details.
      lsw <- builtinLockstepWidth
      addInitStmt $
        WGSL.Let "_dummy_lockstep_width" (WGSL.VarExp lsw)

      wgslBody <- genWGSLStm (ImpGPU.kernelBody kernel)
      addBodyStmt wgslBody

      pure ()

calledInKernelOp :: ImpGPU.KernelOp -> S.Set Name
calledInKernelOp = const mempty

lookupFunction :: Name -> WebGPUS -> Maybe (ImpGPU.Function ImpGPU.HostOp)
lookupFunction fname = lookup fname . unFunctions . wsFuns

functionMayFail :: Name -> WebGPUS -> Bool
functionMayFail fname = S.member fname . wsFunsMayFail

genFunParams :: [Param] -> KernelM [WGSL.Param]
genFunParams =
  mapM $ \case
    MemParam _ _ -> error ""
    ScalarParam name tp -> do
      ident <- getIdent name
      pure $ WGSL.Param ident (WGSL.Prim $ wgslPrimType tp) []

generateDeviceFun :: Name -> ImpGPU.Function ImpGPU.KernelOp -> KernelM ()
generateDeviceFun fname device_func = do
  when (any memParam $ functionInput device_func) $
    compilerLimitation "WebGPU backend cannot generate GPU functions that use arrays."
  ws <- lift State.get
  ks <- get
  r <- ask
  (body, _, _) <- lift $ runRWST (genWGSLStm (functionBody device_func)) r ks

  if functionMayFail fname ws
    then compilerLimitation "WebGPU backend Cannot handle GPU functions that may fail."
    else do
      params <- genFunParams (functionInput device_func)
      output <- case functionOutput device_func of
        [] -> pure Nothing
        [ScalarParam name tp] -> do
          ident <- getIdent name
          pure $ Just (ident, WGSL.Prim $ wgslPrimType tp)
        _ -> compilerLimitation "WebGPU backend cannot generate GPU functions that return memory parameters."
      let wgslFun =
            WGSL.Function
              { WGSL.funName = "futrts_" <> nameToText fname,
                WGSL.funAttribs = mempty,
                WGSL.funParams = params,
                WGSL.funOutput = output,
                WGSL.funBody = body
              }
       in prependDecl $ WGSL.FunDecl wgslFun

  lift $ State.modify $ \s ->
    s
      { wsDevFuns = S.insert fname $ wsDevFuns s
      }

  genDeviceFuns $ functionBody device_func
  where
    memParam MemParam {} = True
    memParam ScalarParam {} = False

-- Ensure that this device function is available, but don't regenerate
-- it if it already exists.
ensureDeviceFun :: Name -> ImpGPU.Function ImpGPU.KernelOp -> KernelM ()
ensureDeviceFun fname host_func = do
  exists <- lift $ State.gets $ S.member fname . wsDevFuns
  unless exists $ generateDeviceFun fname host_func

genDeviceFuns :: ImpGPU.KernelCode -> KernelM ()
genDeviceFuns code = do
  let called = calledFuncs calledInKernelOp code
  forM_ (S.toList called) $ \fname -> do
    def <- lift $ State.gets $ lookupFunction fname
    case def of
      Just host_func -> do
        let device_func = fmap toDevice host_func
        ensureDeviceFun fname device_func
      Nothing -> pure ()
  where
    toDevice :: ImpGPU.HostOp -> ImpGPU.KernelOp
    toDevice _ = compilerLimitation "WebGPU backend cannot handle GPU functions that contain parallelism."

onKernel :: ImpGPU.Kernel -> WebGPUM HostOp
onKernel kernel = do
  (name, extraArgExps) <- genKernel kernel
  let numBlocks = ImpGPU.kernelNumBlocks kernel
  let blockDim = ImpGPU.kernelBlockSize kernel
  let extraArgs = [ValueKArg e t | (e, t) <- extraArgExps]
  let scalarArgs =
        [ ValueKArg (LeafExp n t) t
          | ImpGPU.ScalarUse n t <- ImpGPU.kernelUses kernel
        ]
  let memArgs = [MemKArg n | ImpGPU.MemoryUse n <- ImpGPU.kernelUses kernel]
  let args = extraArgs ++ scalarArgs ++ memArgs

  pure $ LaunchKernel SafetyNone name 0 args numBlocks blockDim

onHostOp :: ImpGPU.HostOp -> WebGPUM HostOp
onHostOp (ImpGPU.CallKernel k) = onKernel k
onHostOp (ImpGPU.GetSize v key size_class) = do
  addSize key size_class
  pure $ GetSize v key
onHostOp (ImpGPU.CmpSizeLe v key size_class x) = do
  addSize key size_class
  pure $ CmpSizeLe v key x
onHostOp (ImpGPU.GetSizeMax v size_class) =
  pure $ GetSizeMax v size_class

-- | Generate WebGPU host and device code.
kernelsToWebGPU :: ImpGPU.Program -> Program
kernelsToWebGPU prog =
  let ImpGPU.Definitions
        types
        (ImpGPU.Constants ps consts)
        (ImpGPU.Functions funs) = prog

      initial_state =
        WebGPUS
          { wsCode = mempty,
            wsSizes = mempty,
            wsMacroDefs = mempty,
            wsKernels = mempty,
            wsNextBindSlot = 0,
            wsDevFuns = mempty,
            wsFuns = defFuns prog,
            wsFunsMayFail = S.empty
          }

      ((consts', funs'), translation) =
        flip State.runState initial_state $
          (,) <$> traverse onHostOp consts <*> traverse (traverse (traverse onHostOp)) funs

      prog' =
        Definitions types (Constants ps consts') (Functions funs')

      kernels = M.fromList $ map (first nameFromText) (wsKernels translation)
      constants = wsMacroDefs translation
      -- TODO: Compute functions using tuning params
      params = M.map (,S.empty) $ wsSizes translation
      failures = mempty
   in Program
        { webgpuProgram = wsCode translation,
          webgpuPrelude = RTS.wgsl_prelude,
          webgpuMacroDefs = constants,
          webgpuKernels = kernels,
          webgpuParams = params,
          webgpuFailures = failures,
          hostDefinitions = prog'
        }

-- | Compile the program to ImpCode with WebGPU kernels.
compileProg :: (MonadFreshNames m) => F.Prog F.GPUMem -> m (Warnings, Program)
compileProg prog = second kernelsToWebGPU <$> ImpGPU.compileProgOpenCL prog

wgslInt8, wgslInt16, wgslInt64 :: WGSL.PrimType
wgslInt8 = WGSL.Int32
wgslInt16 = WGSL.Int32
wgslInt64 = WGSL.Vec2 WGSL.Int32

wgslPrimType :: PrimType -> WGSL.PrimType
wgslPrimType (IntType Int8) = wgslInt8
wgslPrimType (IntType Int16) = wgslInt16
wgslPrimType (IntType Int32) = WGSL.Int32
wgslPrimType (IntType Int64) = wgslInt64
wgslPrimType (FloatType Float16) = WGSL.Float16
wgslPrimType (FloatType Float32) = WGSL.Float32
wgslPrimType (FloatType Float64) = compilerLimitation "WebGPU backend does not support f64."
wgslPrimType Bool = WGSL.Bool
-- TODO: Make sure we do not ever codegen statements involving Unit variables
wgslPrimType Unit = WGSL.Float16 -- error "TODO: no unit in WGSL"

wgslBufferType :: (PrimType, Bool, Signedness) -> Maybe WGSL.Exp -> WGSL.Typ
wgslBufferType (Bool, _, _) = WGSL.Array $ WGSL.Atomic wgslInt8
wgslBufferType (IntType Int8, _, _) = WGSL.Array $ WGSL.Atomic wgslInt8
wgslBufferType (IntType Int16, _, _) = WGSL.Array $ WGSL.Atomic wgslInt16
wgslBufferType (IntType Int32, False, _) = WGSL.Array WGSL.Int32
wgslBufferType (IntType Int32, True, Signed) =
  WGSL.Array $ WGSL.Atomic WGSL.Int32
wgslBufferType (IntType Int32, True, Unsigned) =
  WGSL.Array $ WGSL.Atomic WGSL.UInt32
wgslBufferType (FloatType Float32, True, _) =
  WGSL.Array $ WGSL.Atomic WGSL.Int32
wgslBufferType (FloatType Float16, True, _) =
  WGSL.Array $ WGSL.Atomic WGSL.Int32
wgslBufferType (t, _, _) = WGSL.Array $ wgslPrimType t

wgslSharedBufferType ::
  (PrimType, Bool, Signedness) ->
  Maybe WGSL.Exp ->
  WGSL.Typ
wgslSharedBufferType (Bool, _, _) = WGSL.Array WGSL.Bool
wgslSharedBufferType (IntType Int8, True, _) = WGSL.Array $ WGSL.Atomic WGSL.Int32
wgslSharedBufferType (IntType Int16, True, _) = WGSL.Array $ WGSL.Atomic WGSL.Int32
wgslSharedBufferType (IntType Int8, False, _) = WGSL.Array WGSL.Int32
wgslSharedBufferType (IntType Int16, False, _) = WGSL.Array WGSL.Int32
wgslSharedBufferType (IntType Int32, False, _) = WGSL.Array WGSL.Int32
wgslSharedBufferType (IntType Int32, True, Signed) =
  WGSL.Array $ WGSL.Atomic WGSL.Int32
wgslSharedBufferType (IntType Int32, True, Unsigned) =
  WGSL.Array $ WGSL.Atomic WGSL.UInt32
wgslSharedBufferType (FloatType Float32, True, _) =
  WGSL.Array $ WGSL.Atomic WGSL.Int32
wgslSharedBufferType (FloatType Float16, True, _) =
  WGSL.Array $ WGSL.Atomic WGSL.Int32
wgslSharedBufferType (t, _, _) = WGSL.Array $ wgslPrimType t

packedElemIndex :: PrimType -> WGSL.Exp -> WGSL.Exp
packedElemIndex Bool i = WGSL.BinOpExp "/" i (WGSL.IntExp 4)
packedElemIndex (IntType Int8) i = WGSL.BinOpExp "/" i (WGSL.IntExp 4)
packedElemIndex (IntType Int16) i = WGSL.BinOpExp "/" i (WGSL.IntExp 2)
packedElemIndex (IntType Int32) i = i
packedElemIndex (FloatType Float16) i = WGSL.BinOpExp "/" i (WGSL.IntExp 2)
packedElemIndex (FloatType Float32) i = i
packedElemIndex _ _ = error "CodeGen.ImpGen.WebGPU:packedElemIndex: Unsupported Type"

packedElemOffset :: PrimType -> WGSL.Exp -> WGSL.Exp
packedElemOffset Bool i = WGSL.BinOpExp "%" i (WGSL.IntExp 4)
packedElemOffset (IntType Int8) i = WGSL.BinOpExp "%" i (WGSL.IntExp 4)
packedElemOffset (IntType Int16) i = WGSL.BinOpExp "%" i (WGSL.IntExp 2)
packedElemOffset (IntType Int32) _ = WGSL.IntExp 0
packedElemOffset (FloatType Float16) i = WGSL.BinOpExp "%" i (WGSL.IntExp 2)
packedElemOffset (FloatType Float32) _ = WGSL.IntExp 0
packedElemOffset _ _ = error "CodeGen.ImpGen.WebGPU:packedElemOffset: Unsupported Type"

nativeAccess :: PrimType -> Bool
nativeAccess (IntType Int64) = True
nativeAccess (IntType Int32) = True
nativeAccess (FloatType Float32) = True
nativeAccess (FloatType Float16) = True
nativeAccess _ = False

genArrayAccess ::
  Bool ->
  PrimType ->
  WGSL.Ident ->
  WGSL.Exp ->
  Bool ->
  [WGSL.Exp]
genArrayAccess atomic t mem i packed = do
  if not atomic
    then [WGSL.UnOpExp "&" (WGSL.VarExp mem), i]
    else
      if packed
        then
          let packedIndex = packedElemIndex t i
              packedOffset = packedElemOffset t i
           in [WGSL.UnOpExp "&" (WGSL.IndexExp mem packedIndex), packedOffset]
        else [WGSL.UnOpExp "&" (WGSL.IndexExp mem i)]

genArrayFun :: WGSL.Ident -> PrimType -> Bool -> Bool -> WGSL.Ident
genArrayFun fun t atomic shared =
  if atomic
    then
      let scope = if shared then "_shared" else "_global"
          prefix = if atomic then "atomic_" else ""
       in prefix <> fun <> "_" <> prettyText t <> scope
    else fun <> "_" <> prettyText t

genReadExp ::
  PrimType ->
  VName ->
  Count Elements (TExp Int64) ->
  KernelM WGSL.Exp
genReadExp t mem i = do
  mem' <- getIdent mem
  shared <- isShared mem'
  atomic <- isAtomic mem'
  i' <- indexExp i

  if (nativeAccess t || shared) && not atomic
    then pure $ WGSL.IndexExp mem' i'
    else
      let access = genArrayAccess atomic t mem' i' (not shared)
          fun = genArrayFun "read" t atomic shared
       in pure $ WGSL.CallExp fun access

genArrayRead ::
  PrimType ->
  VName ->
  VName ->
  Count Elements (TExp Int64) ->
  KernelM WGSL.Stmt
genArrayRead t tgt mem i = do
  tgt' <- getIdent tgt
  read' <- genReadExp t mem i
  pure $ WGSL.Assign tgt' read'

genArrayWrite ::
  PrimType ->
  VName ->
  Count Elements (TExp Int64) ->
  KernelM WGSL.Exp ->
  KernelM WGSL.Stmt
genArrayWrite t mem i v = do
  mem' <- getIdent mem
  shared <- isShared mem'
  atomic <- isAtomic mem'
  i' <- indexExp i
  v' <- v

  if (nativeAccess t || shared) && not atomic
    then pure $ WGSL.AssignIndex mem' i' v'
    else
      let access = genArrayAccess atomic t mem' i' (not shared) ++ [v']
          fun = genArrayFun "write" t atomic shared
       in pure $ WGSL.Call fun access

genAtomicOp ::
  WGSL.Ident ->
  PrimType ->
  VName ->
  VName ->
  Count Elements (TExp Int64) ->
  Exp ->
  KernelM WGSL.Stmt
genAtomicOp f t dest mem i e = do
  mem' <- getIdent mem
  shared <- isShared mem'
  i' <- indexExp i
  v' <- genWGSLExp e

  let fun = genArrayFun f t True shared
      args = genArrayAccess True t mem' i' (not shared) ++ [v']
   in WGSL.Assign <$> getIdent dest <*> pure (WGSL.CallExp fun args)

genCopy ::
  PrimType ->
  [Count Elements (TExp Int64)] ->
  (VName, Space) ->
  ( Count Elements (TExp Int64),
    [Count Elements (TExp Int64)]
  ) ->
  (VName, Space) ->
  ( Count Elements (TExp Int64),
    [Count Elements (TExp Int64)]
  ) ->
  KernelM WGSL.Stmt
genCopy pt shape (dst, _) (dst_offset, dst_strides) (src, _) (src_offset, src_strides) = do
  shape' <- mapM (genWGSLExp . untyped . unCount) shape
  body <- do
    let dst_i = dst_offset + sum (zipWith (*) is' dst_strides)
        src_i = src_offset + sum (zipWith (*) is' src_strides)
        read' = genReadExp pt src src_i
     in genArrayWrite pt dst dst_i read'

  pure $ loops (zip iis shape') body
  where
    (zero, one) = (WGSL.VarExp "zero_i64", WGSL.VarExp "one_i64")
    is = map (VName "i") [0 .. length shape - 1]
    is' :: [Count Elements (TExp Int64)]
    is' = map (elements . le64) is
    iis = map nameToIdent is

    loops :: [(WGSL.Ident, WGSL.Exp)] -> WGSL.Stmt -> WGSL.Stmt
    loops [] body = body
    loops ((i, n) : ins) body =
      WGSL.For
        i
        zero
        (wgslCmpOp (CmpUlt Int64) (WGSL.VarExp i) n)
        (WGSL.Assign i $ wgslBinOp (Add Int64 OverflowWrap) (WGSL.VarExp i) one)
        (loops ins body)

unsupported :: Code ImpGPU.KernelOp -> KernelM WGSL.Stmt
unsupported stmt = pure $ WGSL.Comment $ "Unsupported stmt: " <> prettyText stmt

wgslProduct :: [SubExp] -> WGSL.Exp
wgslProduct [] = WGSL.IntExp 1
wgslProduct [Constant (IntValue v)] = WGSL.IntExp $ valueIntegral v
wgslProduct ((Constant (IntValue v)) : vs) =
  wgslBinOp (Mul Int32 OverflowWrap) (WGSL.IntExp $ valueIntegral v) (wgslProduct vs)
wgslProduct _ = error "wgslProduct: non-constant product"

genWGSLStm :: Code ImpGPU.KernelOp -> KernelM WGSL.Stmt
genWGSLStm Skip = pure WGSL.Skip
genWGSLStm (s1 :>>: s2) = liftM2 WGSL.Seq (genWGSLStm s1) (genWGSLStm s2)
genWGSLStm (For iName bound body) = do
  boundExp <- genWGSLExp bound
  bodyStm <- genWGSLStm body
  pure $
    WGSL.For
      i
      zero
      (lt (WGSL.VarExp i) boundExp)
      (WGSL.Assign i $ add (WGSL.VarExp i) one)
      bodyStm
  where
    i = nameToIdent iName
    boundIntType = case primExpType bound of
      IntType t -> t
      _ -> error "non-integer Exp for loop bound"
    add = wgslBinOp $ Add boundIntType OverflowWrap
    lt = wgslCmpOp $ CmpUlt boundIntType
    (zero, one) = case boundIntType of
      Int64 -> (WGSL.VarExp "zero_i64", WGSL.VarExp "one_i64")
      _ -> (WGSL.IntExp 0, WGSL.IntExp 1)
genWGSLStm (While cond body) =
  liftM2
    WGSL.While
    (genWGSLExp $ untyped cond)
    (genWGSLStm body)
genWGSLStm (DeclareMem name (Space "shared")) = do
  let name' = nameToIdent name
  moduleName <- mkGlobalIdent name'
  sizeName <- mkGlobalIdent $ name' <> "_size"

  maybeElemPrimType <- findSingleMemoryType name
  case maybeElemPrimType of
    Just elemPrimType@(_, atomic, _) -> do
      let bufType =
            wgslSharedBufferType
              elemPrimType
              (Just $ WGSL.VarExp sizeName)

      addOverride sizeName (WGSL.Prim WGSL.Int32) (Just $ WGSL.IntExp 0)
      addDecl $ WGSL.VarDecl [] WGSL.Workgroup moduleName bufType
      when atomic $ addAtomicMem moduleName
      addRename name' moduleName
      pure $ WGSL.Comment $ "declare_shared: " <> name'
    Nothing ->
      pure $ WGSL.Comment $ "discard declare_shared: " <> name'
genWGSLStm (DeclareMem name (ScalarSpace vs pt)) =
  pure $
    WGSL.DeclareVar (nameToIdent name) (WGSL.Array (wgslPrimType pt) (Just $ wgslProduct vs))
genWGSLStm s@(DeclareMem _ _) = unsupported s
genWGSLStm (DeclareScalar name _ typ) =
  pure $
    WGSL.DeclareVar (nameToIdent name) (WGSL.Prim $ wgslPrimType typ)
genWGSLStm s@(DeclareArray {}) = unsupported s
genWGSLStm s@(Allocate {}) = unsupported s
genWGSLStm s@(Free _ _) = pure $ WGSL.Comment $ "free: " <> prettyText s
genWGSLStm (Copy pt shape dst dst_lmad src src_lmad) = genCopy pt shape dst dst_lmad src src_lmad
genWGSLStm (Write mem i Bool _ _ v) = genArrayWrite Bool mem i (genWGSLExp v)
genWGSLStm (Write mem i (IntType Int8) _ _ v) = genArrayWrite (IntType Int8) mem i (genWGSLExp v)
genWGSLStm (Write mem i (IntType Int16) _ _ v) = genArrayWrite (IntType Int16) mem i (genWGSLExp v)
genWGSLStm (Write mem i t _ _ v) = do
  mem' <- getIdent mem
  i' <- indexExp i
  v' <- genWGSLExp v
  atomic <- isAtomic mem'
  if atomic
    then genArrayWrite t mem i (genWGSLExp v)
    else pure $ WGSL.AssignIndex mem' i' v'
genWGSLStm (SetScalar name e) =
  liftM2 WGSL.Assign (getIdent name) (genWGSLExp e)
genWGSLStm (Read tgt mem i Bool _ _) = genArrayRead Bool tgt mem i
genWGSLStm (Read tgt mem i (IntType Int8) _ _) = genArrayRead (IntType Int8) tgt mem i
genWGSLStm (Read tgt mem i (IntType Int16) _ _) = genArrayRead (IntType Int16) tgt mem i
genWGSLStm (Read tgt mem i t _ _) = do
  tgt' <- getIdent tgt
  mem' <- getIdent mem
  i' <- indexExp i
  atomic <- isAtomic mem'
  if atomic
    then genArrayRead t tgt mem i
    else pure $ WGSL.Assign tgt' (WGSL.IndexExp mem' i')
genWGSLStm stm@(SetMem {}) =
  compilerLimitation . docText $
    "WebGPU backend Cannot handle SetMem statement"
      </> indent 2 (align (pretty stm))
      </> "in GPU kernel."
genWGSLStm (Call [dest] f args) = do
  fun <- WGSL.CallExp . ("futrts_" <>) <$> getIdent f
  let getArg (ExpArg e) = genWGSLExp e
      getArg (MemArg n) = WGSL.VarExp <$> getIdent n
  argExps <- mapM getArg args
  WGSL.Assign <$> getIdent dest <*> pure (fun argExps)
genWGSLStm (Call {}) =
  pure $
    WGSL.Comment "TODO: Multi-destination calls not supported"
genWGSLStm (If cond cThen cElse) =
  liftM3
    WGSL.If
    (genWGSLExp $ untyped cond)
    (genWGSLStm cThen)
    (genWGSLStm cElse)
genWGSLStm s@(Assert {}) = pure $ WGSL.Comment $ "assert: " <> prettyText s
genWGSLStm (Comment c s) = WGSL.Seq (WGSL.Comment c) <$> genWGSLStm s
genWGSLStm (DebugPrint _ _) = pure WGSL.Skip
genWGSLStm (TracePrint _) = pure WGSL.Skip
genWGSLStm (Op (ImpGPU.GetBlockId dest i)) = do
  destId <- getIdent dest
  pure $
    WGSL.Assign destId $
      WGSL.to_i32 (WGSL.IndexExp "workgroup_id" (WGSL.IntExp i))
genWGSLStm (Op (ImpGPU.GetLocalId dest i)) = do
  destId <- getIdent dest
  pure $
    WGSL.Assign destId $
      WGSL.to_i32 (WGSL.IndexExp "local_id" (WGSL.IntExp i))
genWGSLStm (Op (ImpGPU.GetLocalSize dest i)) = do
  destId <- getIdent dest
  WGSL.Assign destId . WGSL.VarExp <$> builtinBlockSize i
genWGSLStm (Op (ImpGPU.GetLockstepWidth dest)) = do
  destId <- getIdent dest
  WGSL.Assign destId . WGSL.VarExp <$> builtinLockstepWidth
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicAdd t dest mem i e))) =
  genAtomicOp "add" (IntType t) dest mem i e
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicSMax t dest mem i e))) =
  genAtomicOp "smax" (IntType t) dest mem i e
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicSMin t dest mem i e))) =
  genAtomicOp "smin" (IntType t) dest mem i e
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicUMax t dest mem i e))) =
  genAtomicOp "umax" (IntType t) dest mem i e
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicUMin t dest mem i e))) =
  genAtomicOp "umin" (IntType t) dest mem i e
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicAnd t dest mem i e))) =
  genAtomicOp "and" (IntType t) dest mem i e
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicOr t dest mem i e))) =
  genAtomicOp "or" (IntType t) dest mem i e
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicXor t dest mem i e))) =
  genAtomicOp "xor" (IntType t) dest mem i e
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicCmpXchg _ dest mem i cmp val))) = do
  val' <- genWGSLExp val
  cmp' <- genWGSLExp cmp
  i' <- WGSL.IndexExp <$> getIdent mem <*> indexExp i
  liftM2 WGSL.Assign (getIdent dest) (pure $ WGSL.FieldExp (WGSL.CallExp "atomicCompareExchangeWeak" [WGSL.UnOpExp "&" i', cmp', val']) "old_value")
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicXchg _ dest mem i e))) = do
  idx <- WGSL.IndexExp <$> getIdent mem <*> indexExp i
  val <- genWGSLExp e
  let call = WGSL.CallExp "atomicExchange" [WGSL.UnOpExp "&" idx, val]
  WGSL.Assign <$> getIdent dest <*> pure call
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicWrite t mem i v))) = genArrayWrite t mem i (genWGSLExp v)
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicFAdd t dest mem i e))) = genAtomicOp "fadd" (FloatType t) dest mem i e
genWGSLStm (Op (ImpGPU.Barrier ImpGPU.FenceLocal)) =
  pure $ WGSL.Call "workgroupBarrier" []
genWGSLStm (Op (ImpGPU.Barrier ImpGPU.FenceGlobal)) =
  pure $ WGSL.Call "storageBarrier" []
genWGSLStm s@(Op (ImpGPU.MemFence _)) = unsupported s
genWGSLStm (Op (ImpGPU.SharedAlloc name size)) = do
  let name' = nameToIdent name
  sizeName <- mkGlobalIdent $ name' <> "_size"

  maybeElemPrimType <- findSingleMemoryType name
  case maybeElemPrimType of
    Just (elemPrimType, _, _) -> do
      let elemSize = primByteSize elemPrimType :: Int32
      let sizeExp =
            zExt Int32 (untyped (unCount size))
              ~/~ ValueExp (IntValue $ Int32Value elemSize)

      addSharedMem sizeName sizeExp
      pure $ WGSL.Comment $ "shared_alloc: " <> name'
    Nothing ->
      pure $ WGSL.Comment $ "discard shared_alloc: " <> name'
genWGSLStm (Op (ImpGPU.UniformRead tgt mem i _ _)) = do
  tgt' <- getIdent tgt
  mem' <- getIdent mem
  i' <- indexExp i
  pure $
    WGSL.Assign tgt' $
      WGSL.CallExp "workgroupUniformLoad" [WGSL.UnOpExp "&" $ WGSL.IndexExp mem' i']
genWGSLStm (Op (ImpGPU.ErrorSync f)) = genWGSLStm $ Op (ImpGPU.Barrier f)

call1 :: WGSL.Ident -> WGSL.Exp -> WGSL.Exp
call1 f a = WGSL.CallExp f [a]

call2 :: WGSL.Ident -> WGSL.Exp -> WGSL.Exp -> WGSL.Exp
call2 f a b = WGSL.CallExp f [a, b]

call2Suffix :: WGSL.Ident -> IntType -> WGSL.Exp -> WGSL.Exp -> WGSL.Exp
call2Suffix f t a b = WGSL.CallExp (f <> "_" <> prettyText t) [a, b]

wgslBinOp :: BinOp -> WGSL.Exp -> WGSL.Exp -> WGSL.Exp
wgslBinOp (Add Int32 _) = WGSL.BinOpExp "+"
wgslBinOp (Add t _) = call2Suffix "add" t
wgslBinOp (FAdd _) = WGSL.BinOpExp "+"
wgslBinOp (Sub Int32 _) = WGSL.BinOpExp "-"
wgslBinOp (Sub t _) = call2Suffix "sub" t
wgslBinOp (FSub _) = WGSL.BinOpExp "-"
wgslBinOp (Mul Int32 _) = WGSL.BinOpExp "*"
wgslBinOp (Mul t _) = call2Suffix "mul" t
wgslBinOp (FMul _) = WGSL.BinOpExp "*"
-- Division is always safe in WGSL, so we can ignore the Safety parameter.
wgslBinOp (UDiv t _) = call2Suffix "udiv" t
wgslBinOp (UDivUp t _) = call2Suffix "udiv_up" t
wgslBinOp (SDiv t _) = call2Suffix "sdiv" t
wgslBinOp (SDivUp t _) = call2Suffix "sdiv_up" t
wgslBinOp (FDiv _) = WGSL.BinOpExp "/"
wgslBinOp (FMod _) = WGSL.BinOpExp "%"
wgslBinOp (UMod t _) = call2Suffix "umod" t
wgslBinOp (SMod t _) = call2Suffix "smod" t
wgslBinOp (SQuot Int8 _) = WGSL.BinOpExp "/"
wgslBinOp (SQuot Int16 _) = WGSL.BinOpExp "/"
wgslBinOp (SQuot Int32 _) = WGSL.BinOpExp "/"
wgslBinOp (SQuot Int64 _) = call2 "squot_i64"
wgslBinOp (SRem Int8 _) = WGSL.BinOpExp "%"
wgslBinOp (SRem Int16 _) = WGSL.BinOpExp "%"
wgslBinOp (SRem Int32 _) = WGSL.BinOpExp "%"
wgslBinOp (SRem Int64 _) = call2 "srem_i64"
wgslBinOp (SMin Int64) = call2 "smin_i64"
wgslBinOp (SMin _) = call2 "min"
wgslBinOp (UMin t) = call2Suffix "umin" t
wgslBinOp (FMin _) = call2 "min"
wgslBinOp (SMax Int64) = call2 "smax_i64"
wgslBinOp (SMax _) = call2 "max"
wgslBinOp (UMax t) = call2Suffix "umax" t
wgslBinOp (FMax _) = call2 "max"
wgslBinOp (Shl t) = call2Suffix "shl" t
wgslBinOp (LShr t) = call2Suffix "lshr" t
wgslBinOp (AShr t) = call2Suffix "ashr" t
wgslBinOp (And _) = WGSL.BinOpExp "&"
wgslBinOp (Or _) = WGSL.BinOpExp "|"
wgslBinOp (Xor _) = WGSL.BinOpExp "^"
wgslBinOp (Pow t) = call2Suffix "pow" t
wgslBinOp (FPow _) = call2 "pow"
wgslBinOp LogAnd = call2 "log_and"
wgslBinOp LogOr = call2 "log_or"

-- Because we (in e.g. scalar8.wgsl) make sure to always sign-extend an i8 value
-- across its whole i32 representation, we can just use the normal comparison
-- operators for smaller integers. The same applies for i16.
wgslCmpOp :: CmpOp -> WGSL.Exp -> WGSL.Exp -> WGSL.Exp
wgslCmpOp (CmpEq (IntType Int64)) = call2 "eq_i64"
wgslCmpOp (CmpEq _) = WGSL.BinOpExp "=="
wgslCmpOp (CmpUlt Int64) = call2 "ult_i64"
wgslCmpOp (CmpUlt _) = call2 "ult_i32"
wgslCmpOp (CmpUle Int64) = call2 "ule_i64"
wgslCmpOp (CmpUle _) = call2 "ule_i32"
wgslCmpOp (CmpSlt Int64) = call2 "slt_i64"
wgslCmpOp (CmpSlt _) = WGSL.BinOpExp "<"
wgslCmpOp (CmpSle Int64) = call2 "sle_i64"
wgslCmpOp (CmpSle _) = WGSL.BinOpExp "<="
wgslCmpOp (FCmpLt _) = WGSL.BinOpExp "<"
wgslCmpOp (FCmpLe _) = WGSL.BinOpExp "<="
wgslCmpOp CmpLlt = call2 "llt"
wgslCmpOp CmpLle = call2 "lle"

-- Similarly to CmpOps above, the defaults work for smaller integers already
-- given our representation.
wgslUnOp :: UnOp -> WGSL.Exp -> WGSL.Exp
wgslUnOp (Neg (FloatType _)) = WGSL.UnOpExp "-"
wgslUnOp (Neg (IntType t)) = call1 $ "neg_" <> prettyText t
wgslUnOp (Neg _) = WGSL.UnOpExp "!"
wgslUnOp (Complement _) = WGSL.UnOpExp "~"
wgslUnOp (Abs Int64) = call1 "abs_i64"
wgslUnOp (Abs _) = call1 "abs"
wgslUnOp (FAbs _) = call1 "abs"
wgslUnOp (SSignum Int64) = call1 "ssignum_i64"
wgslUnOp (SSignum _) = call1 "sign"
wgslUnOp (USignum Int64) = call1 "usignum_i64"
wgslUnOp (USignum _) = call1 "usignum_i32"
wgslUnOp (FSignum _) = call1 "sign"

wgslConvOp :: ConvOp -> WGSL.Exp -> WGSL.Exp
wgslConvOp op a = WGSL.CallExp (fun op) [a]
  where
    fun (ZExt Int8 Int16) = "zext_i8_i16"
    fun (SExt Int8 Int16) = "sext_i8_i16"
    fun (ZExt Int8 Int32) = "zext_i8_i32"
    fun (SExt Int8 Int32) = "sext_i8_i32"
    fun (ZExt Int8 Int64) = "zext_i8_i64"
    fun (SExt Int8 Int64) = "sext_i8_i64"
    fun (ZExt Int16 Int32) = "zext_i16_i32"
    fun (SExt Int16 Int32) = "sext_i16_i32"
    fun (ZExt Int16 Int64) = "zext_i16_i64"
    fun (SExt Int16 Int64) = "sext_i16_i64"
    fun (ZExt Int32 Int64) = "zext_i32_i64"
    fun (SExt Int32 Int64) = "sext_i32_i64"
    fun (ZExt Int16 Int8) = "trunc_i16_i8"
    fun (SExt Int16 Int8) = "trunc_i16_i8"
    fun (ZExt Int32 Int8) = "trunc_i32_i8"
    fun (SExt Int32 Int8) = "trunc_i32_i8"
    fun (ZExt Int64 Int8) = "trunc_i64_i8"
    fun (SExt Int64 Int8) = "trunc_i64_i8"
    fun (ZExt Int32 Int16) = "trunc_i32_i16"
    fun (SExt Int32 Int16) = "trunc_i32_i16"
    fun (ZExt Int64 Int16) = "trunc_i64_i16"
    fun (SExt Int64 Int16) = "trunc_i64_i16"
    fun (ZExt Int64 Int32) = "trunc_i64_i32"
    fun (SExt Int64 Int32) = "trunc_i64_i32"
    fun (FPToUI Float16 Int8) = "f16_to_u8"
    fun (FPToUI Float16 Int16) = "f16_to_u16"
    fun (FPToUI Float16 Int32) = "f16_to_u32"
    fun (FPToUI Float32 Int8) = "f32_to_u8"
    fun (FPToUI Float32 Int16) = "f32_to_u16"
    fun (FPToUI Float32 Int32) = "f32_to_u32"
    fun (FPToSI Float16 Int8) = "f16_to_i8"
    fun (FPToSI Float16 Int16) = "f16_to_i16"
    fun (FPToSI Float16 Int32) = "i32"
    fun (FPToSI Float32 Int8) = "f32_to_i8"
    fun (FPToSI Float32 Int16) = "f32_to_i16"
    fun (FPToSI Float32 Int32) = "i32"
    fun (UIToFP Int8 Float16) = "u8_to_f16"
    fun (UIToFP Int16 Float16) = "u16_to_f16"
    fun (UIToFP Int32 Float16) = "u32_to_f16"
    fun (UIToFP Int64 Float16) = "u64_to_f16"
    fun (UIToFP Int8 Float32) = "u8_to_f32"
    fun (UIToFP Int16 Float32) = "u16_to_f32"
    fun (UIToFP Int32 Float32) = "u32_to_f32"
    fun (UIToFP Int64 Float32) = "u64_to_f32"
    fun (SIToFP Int64 Float16) = "i64_to_f16"
    fun (SIToFP _ Float16) = "f16"
    fun (SIToFP Int64 Float32) = "i64_to_f32"
    fun (SIToFP _ Float32) = "f32"
    fun (IToB Int64) = "i64_to_bool"
    fun (IToB _) = "bool"
    fun (BToI Int8) = "bool_to_i8"
    fun (BToI Int16) = "bool_to_i16"
    fun (BToI Int32) = "i32"
    fun (BToI Int64) = "bool_to_i64"
    fun o = "not_implemented(" <> prettyText o <> ")"

intLiteral :: IntValue -> WGSL.Exp
intLiteral (Int8Value v) =
  WGSL.CallExp "norm_i8" [WGSL.IntExp $ fromIntegral v]
intLiteral (Int16Value v) =
  WGSL.CallExp "norm_i16" [WGSL.IntExp $ fromIntegral v]
intLiteral (Int64Value v) = WGSL.CallExp "i64" [low, high]
  where
    low = WGSL.IntExp $ fromIntegral $ v Bits..&. 0xffffff
    high = WGSL.IntExp $ fromIntegral $ (v `Bits.shift` (-32)) Bits..&. 0xffffff
intLiteral v = WGSL.IntExp (valueIntegral v)

handleSpecialFloats :: T.Text -> Double -> WGSL.Exp
handleSpecialFloats s v
  | isInfinite v, v > 0 = WGSL.CallExp (s <> "_inf") []
  | isInfinite v, v < 0 = WGSL.CallExp (s <> "_neg_inf") []
  | isNaN v = WGSL.CallExp (s <> "_nan") []
  | otherwise = WGSL.FloatExp v

genFloatExp :: FloatValue -> WGSL.Exp
genFloatExp (Float16Value v) = handleSpecialFloats "f16" (convFloat v)
genFloatExp (Float32Value v) = handleSpecialFloats "f32" (convFloat v)
genFloatExp (Float64Value v) = handleSpecialFloats "f64" v

genWGSLExp :: Exp -> KernelM WGSL.Exp
genWGSLExp (LeafExp name _) = WGSL.VarExp <$> getIdent name
genWGSLExp (ValueExp (IntValue v)) = pure $ intLiteral v
genWGSLExp (ValueExp (FloatValue v)) = pure $ genFloatExp v
genWGSLExp (ValueExp (BoolValue v)) = pure $ WGSL.BoolExp v
genWGSLExp (ValueExp UnitValue) =
  error "should not attempt to generate unit expressions"
genWGSLExp (BinOpExp op e1 e2) =
  liftM2 (wgslBinOp op) (genWGSLExp e1) (genWGSLExp e2)
genWGSLExp (CmpOpExp op e1 e2) =
  liftM2 (wgslCmpOp op) (genWGSLExp e1) (genWGSLExp e2)
genWGSLExp (UnOpExp op e) = wgslUnOp op <$> genWGSLExp e
genWGSLExp (ConvOpExp op e) = wgslConvOp op <$> genWGSLExp e
genWGSLExp e = pure $ WGSL.StringExp $ "<not implemented: " <> prettyText e <> ">"

-- We support 64-bit arithmetic, but since WGSL does not have support for it,
-- we cannot use a 64-bit value as an index, so we have to truncate it to 32
-- bits.
indexExp :: Count Elements (TExp Int64) -> KernelM WGSL.Exp
-- There are many occasions where we would end up extending to 64 bit and
-- immediately truncating, avoid that.
indexExp (Count (TPrimExp (ConvOpExp (SExt Int32 Int64) e))) = genWGSLExp e
indexExp c = (genWGSLExp . ConvOpExp (ZExt Int64 Int32) . untyped . unCount) c

-- | Generate a struct declaration and corresponding uniform binding declaration
-- for all the scalar 'KernelUse's. Also generate a block of statements that
-- copies the struct fields into local variables so the kernel body can access
-- them unmodified.
genScalarDecls :: KernelM ()
genScalarDecls = do
  structName <- mkGlobalIdent "Scalars"
  bufferName <- mkGlobalIdent "scalars"
  uses <- asks (ImpGPU.kernelUses . krKernel)

  let scalarUses = [(nameToIdent name, typ) | ImpGPU.ScalarUse name typ <- uses]
  scalars <- forM scalarUses $
    \(name, typ) -> do
      let varPrimTyp = wgslPrimType typ
      let fieldPrimTyp = case typ of
            Bool -> WGSL.Int32 -- bool is not host-shareable
            _ -> varPrimTyp
      let wrapCopy e = case typ of
            Bool -> WGSL.CallExp "bool" [e]
            _ -> e

      addScalar fieldPrimTyp
      addInitStmt $ WGSL.DeclareVar name (WGSL.Prim varPrimTyp)
      addInitStmt $
        WGSL.Assign name (wrapCopy $ WGSL.FieldExp (WGSL.VarExp bufferName) name)

      pure (name, WGSL.Prim fieldPrimTyp)

  let scalarFields = case scalars of
        [] -> [("_dummy_scalar", WGSL.Prim WGSL.Int32)]
        sclrs -> sclrs
  addDecl $
    WGSL.StructDecl $
      WGSL.Struct structName (map (uncurry WGSL.Field) scalarFields)

  slot <- assignBindSlot
  let bufferAttribs = WGSL.bindingAttribs 0 slot
  addDecl $
    WGSL.VarDecl bufferAttribs WGSL.Uniform bufferName (WGSL.Named structName)

atomicOpArray :: ImpGPU.AtomicOp -> VName
atomicOpArray (ImpGPU.AtomicAdd _ _ n _ _) = n
atomicOpArray (ImpGPU.AtomicFAdd _ _ n _ _) = n
atomicOpArray (ImpGPU.AtomicSMax _ _ n _ _) = n
atomicOpArray (ImpGPU.AtomicSMin _ _ n _ _) = n
atomicOpArray (ImpGPU.AtomicUMax _ _ n _ _) = n
atomicOpArray (ImpGPU.AtomicUMin _ _ n _ _) = n
atomicOpArray (ImpGPU.AtomicAnd _ _ n _ _) = n
atomicOpArray (ImpGPU.AtomicOr _ _ n _ _) = n
atomicOpArray (ImpGPU.AtomicXor _ _ n _ _) = n
atomicOpArray (ImpGPU.AtomicCmpXchg _ _ n _ _ _) = n
atomicOpArray (ImpGPU.AtomicXchg _ _ n _ _) = n
atomicOpArray (ImpGPU.AtomicWrite _ n _ _) = n

-- Usually we declare all our variables as the signed type and re-interpret when
-- necessary for operations. We can't do an AtomicU{Min,Max} on an `atomic<i32>`
-- however. If the only atomic op is a UMin/UMax, we thus declare the buffer
-- unsigned. Otherwise, we have no chance.
-- TODO: This is actually not right. Most of these should have an
-- "indeterminate" signedness, so that any combination of those and one of
-- s{min,max} and u{min,max} is valid.
atomicOpType :: ImpGPU.AtomicOp -> (PrimType, Signedness)
atomicOpType (ImpGPU.AtomicAdd t _ _ _ _) = (IntType t, Signed)
atomicOpType (ImpGPU.AtomicFAdd t _ _ _ _) = (FloatType t, Signed)
atomicOpType (ImpGPU.AtomicSMax t _ _ _ _) = (IntType t, Signed)
atomicOpType (ImpGPU.AtomicSMin t _ _ _ _) = (IntType t, Signed)
atomicOpType (ImpGPU.AtomicUMax t _ _ _ _) = (IntType t, Unsigned)
atomicOpType (ImpGPU.AtomicUMin t _ _ _ _) = (IntType t, Unsigned)
atomicOpType (ImpGPU.AtomicAnd t _ _ _ _) = (IntType t, Signed)
atomicOpType (ImpGPU.AtomicOr t _ _ _ _) = (IntType t, Signed)
atomicOpType (ImpGPU.AtomicXor t _ _ _ _) = (IntType t, Signed)
atomicOpType (ImpGPU.AtomicCmpXchg t _ _ _ _ _) = (t, Signed)
atomicOpType (ImpGPU.AtomicXchg t _ _ _ _) = (t, Signed)
atomicOpType (ImpGPU.AtomicWrite t _ _ _) = (t, Signed)

-- | Internally, memory buffers are untyped but WGSL requires us to annotate the
-- binding with a type. Search the kernel body for any reads and writes to the
-- given buffer and return all types it is accessed at.
-- The bool indicates an atomic type. The Signedness is only relevant for atomic
-- types as described for `atomicOpType`.
findMemoryTypes :: VName -> KernelM [(PrimType, Bool, Signedness)]
findMemoryTypes name = S.elems . find <$> asks (ImpGPU.kernelBody . krKernel)
  where
    find (ImpGPU.Write n _ t _ _ _) | n == name = S.singleton (t, False, Signed)
    find (ImpGPU.Read _ n _ t _ _) | n == name = S.singleton (t, False, Signed)
    find (ImpGPU.Copy t _ (n, _) _ _ _) | n == name = S.singleton (t, False, Signed)
    find (ImpGPU.Copy t _ _ _ (n, _) _) | n == name = S.singleton (t, False, Signed)
    find (Op (ImpGPU.Atomic _ op))
      | atomicOpArray op == name =
          let (t, sgn) = atomicOpType op
           in S.singleton (t, True, sgn)
    find (s1 :>>: s2) = find s1 <> find s2
    find (For _ _ body) = find body
    find (While _ body) = find body
    find (If _ s1 s2) = find s1 <> find s2
    find (Comment _ s) = find s
    find _ = S.empty

findSingleMemoryType :: VName -> KernelM (Maybe (PrimType, Bool, Signedness))
findSingleMemoryType name = do
  types <- findMemoryTypes name
  let prims = nubOrd $ map (\(t, _, _) -> t) types
  case prims of
    [] -> pure Nothing
    [prim] -> do
      -- Only used at one primitive type. If it is an integer <=32 bit and there
      -- are atomic accesses, make it the appropriate atomic type. Otherwise,
      -- make sure there is only one type combination total.
      let atomic = L.find (\(_, a, _) -> a) types
      case atomic of
        Just (_, _, sgn)
          | canBeAtomic prim ->
              if all (\(_, _, s) -> s == sgn) types
                then pure $ Just (prim, True, sgn)
                else error "Atomic type used at multiple signednesses"
        Just (t, _, _) ->
          error $ "Atomics not supported for values of type " <> show t
        Nothing -> pure $ Just (prim, False, Signed)
    _tooMany -> error "Buffer used at multiple types"
  where
    canBeAtomic (IntType Int64) = False
    canBeAtomic (IntType _) = True
    canBeAtomic (FloatType _) = True
    canBeAtomic Bool = True
    canBeAtomic _ = False

-- | Generate binding declarations for memory buffers used by kernel. Produces
-- additional name replacements because it makes the binding names unique.
--
-- We can't use the same trick as for e.g. scalars where we make a local copy to
-- avoid the name replacements because WGSL does not allow function-local
-- variables in the 'storage' address space.
genMemoryDecls :: KernelM ()
genMemoryDecls = do
  uses <- asks (ImpGPU.kernelUses . krKernel)
  memUses <- catMaybes <$> sequence [withType n | ImpGPU.MemoryUse n <- uses]
  when (length memUses > 8) $ error "WeBGPU does not support binding more than 8 storage buffers!"
  mapM_ moduleDecl memUses
  mapM_ rename memUses
  where
    withType name = do
      typ <- findSingleMemoryType name
      pure $ (nameToIdent name,) <$> typ
    moduleDecl (name, typ) = do
      ident <- mkGlobalIdent name
      slot <- assignBindSlot
      let (_, atomic, _) = typ
      when atomic $ addAtomicMem ident
      addDecl $
        WGSL.VarDecl
          (WGSL.bindingAttribs 0 slot)
          (WGSL.Storage WGSL.ReadWrite)
          ident
          (wgslBufferType typ Nothing)
    rename (name, _) = mkGlobalIdent name >>= addRename name

-- | Generate `override` declarations for kernel 'ConstUse's and
-- backend-provided values (like block size and lockstep width).
genConstAndBuiltinDecls :: KernelM ()
genConstAndBuiltinDecls = do
  kernel <- asks krKernel

  -- Start off with handling the block size parameters.
  let blockDimExps = zip [0 ..] $ ImpGPU.kernelBlockSize kernel
  blockDimNames <- mapM (builtinBlockSize . fst) blockDimExps
  let blockDims = zip blockDimExps blockDimNames
  let constBlockDims = [(i, n, e) | ((i, Right e), n) <- blockDims]
  let dynBlockDims = [(i, n, e) | ((i, Left e), n) <- blockDims]

  forM_ blockDimNames $
    \n -> addOverride n (WGSL.Prim WGSL.Int32) zeroInit

  -- KernelConstExp block dims get generated into the general macro/override
  -- machinery.
  lift $
    mapM_ (uncurry addMacroDef) [(nameFromText n, e) | (_, n, e) <- constBlockDims]

  mapM_ (\(i, n, _e) -> addBlockDim i n True) dynBlockDims
  mapM_ (\(i, n, _e) -> addBlockDim i n False) constBlockDims

  -- Next we generate builtin override declarations.
  lsWidth <- builtinLockstepWidth
  addOverride lsWidth (WGSL.Prim WGSL.Int32) zeroInit
  lift $ addMacroDef (nameFromText lsWidth) $ ValueExp (IntValue (Int32Value 1))

  -- And lastly we handle ConstUses.
  let consts = [(n, e) | ImpGPU.ConstUse n e <- ImpGPU.kernelUses kernel]

  let mkLo e =
        untyped $
          (TPrimExp e :: TPrimExp Int64 KernelConst) .&. 0x00000000ffffffff
  let mkHi e =
        untyped $
          (TPrimExp e :: TPrimExp Int64 KernelConst) .>>. 32

  let mkConst (name, e) = do
        let n = nameToIdent name
        lo <- mkGlobalIdent (n <> "_lo")
        hi <- mkGlobalIdent (n <> "_hi")
        case primExpType e of
          IntType Int64 -> do
            addOverride lo (WGSL.Prim WGSL.Int32) zeroInit
            addOverride hi (WGSL.Prim WGSL.Int32) zeroInit
            lift $ addMacroDef (nameFromText lo) (mkLo e)
            lift $ addMacroDef (nameFromText hi) (mkHi e)
            addInitStmt $
              WGSL.Seq
                (WGSL.DeclareVar n (WGSL.Prim wgslInt64))
                ( WGSL.Assign n $
                    WGSL.CallExp "i64" [WGSL.VarExp lo, WGSL.VarExp hi]
                )
          _ -> do
            addOverride lo (WGSL.Prim WGSL.Int32) zeroInit
            lift $ addMacroDef (nameFromText lo) e
            addInitStmt $
              WGSL.Seq
                (WGSL.DeclareVar n (WGSL.Prim wgslInt64))
                ( WGSL.Assign n $
                    WGSL.CallExp "i64" [WGSL.VarExp lo, WGSL.IntExp 0]
                )

  mapM_ mkConst consts
  where
    zeroInit = Just $ WGSL.IntExp 0

nameToIdent :: VName -> WGSL.Ident
nameToIdent = zEncodeText . prettyText

textToIdent :: T.Text -> WGSL.Ident
textToIdent = zEncodeText
