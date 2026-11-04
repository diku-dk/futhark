{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

-- | Code generation for ImpCode with WebGPU.
module Futhark.CodeGen.ImpGen.WebGPU
  ( compileProg,
    Warnings,
  )
where

import Control.Monad (forM, forM_, liftM2, unless, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS
import Control.Monad.Trans.State qualified as State
import Data.Bifunctor (first, second)
import Data.Bits qualified as Bits
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
import Futhark.Util.Pretty (align, docText, indent, pretty, stack, (</>))
import Language.Futhark.Warnings (Warnings)
import Language.WGSL qualified as WGSL

-- State carried during WebGPU translation.
data WebGPUS = WebGPUS
  { -- | Accumulated code.
    wsCode :: T.Text,
    wsMacroDefs :: [(Name, KernelConstExp)],
    -- | Interface of kernels already generated into wsCode.
    wsKernels :: [(WGSL.Ident, KernelInterface)],
    wsNextBindSlot :: Int,
    -- | Device functions already generated.
    wsDevFuns :: S.Set Name,
    wsFuns :: ImpGPU.Functions ImpGPU.HostOp
  }

-- The monad in which we perform the overall translation.
type WebGPUM = State.State WebGPUS

addMacroDef :: Name -> KernelConstExp -> WebGPUM ()
addMacroDef key e =
  State.modify $ \s -> s {wsMacroDefs = (key, e) : wsMacroDefs s}

addCode :: T.Text -> WebGPUM ()
addCode code =
  State.modify $ \s -> s {wsCode = wsCode s <> code}

newtype KernelR = KernelR {krKernel :: ImpGPU.Kernel}

-- | What we know about a memory block used by a kernel.
data MemInfo = MemInfo
  { -- | Workgroup (shared) memory, rather than a storage buffer.
    memShared :: Bool,
    memRepr :: MemRepr
  }

-- | How the elements of a memory block are stored.
data MemRepr
  = -- | As an array of the element type.
    MemElems
  | -- | As atomic words (see 'wordStorage'), so all accesses go through the
    -- @atomic_@ functions of @rts/wgsl/atomics.wgsl@.
    MemAtomicWords
  | -- | As plain words, in a read-only storage buffer, which cannot hold
    -- atomics.  Elements are extracted with the @unpack_@ functions.
    MemReadOnlyWords
  deriving (Eq)

data KernelState = KernelState
  { -- Kernel declarations and body
    ksDecls :: [WGSL.GlobalDecl],
    ksInits :: [WGSL.Statement],
    ksBody :: [WGSL.Statement],
    -- | Identifier replacement map. We have to rename some identifiers; when
    -- translating Imp Code and PrimExps this map is consulted to respect the
    -- renaming.
    ksNameReplacements :: M.Map WGSL.Ident WGSL.Ident,
    -- These describe the kernel interface.
    ksBlockDims :: [(Int, WGSL.Ident, Bool)],
    -- | Memory blocks declared by the kernel, by their (renamed) identifier.
    ksMem :: M.Map WGSL.Ident MemInfo,
    -- | Overrides giving the sizes of shared memory blocks, and the expressions
    -- computing them at launch.
    ksSharedMemSizes :: [(WGSL.Ident, Exp)],
    ksScalars :: [WGSL.StructMember],
    ksBindSlots :: [Int],
    -- | The bind slots of read-only storage buffers.
    ksReadOnlySlots :: S.Set Int,
    -- | Workgroup variables used by 'uniformValue', by type.
    ksUniformSlots :: M.Map PrimType WGSL.Ident
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

addDecl :: WGSL.GlobalDecl -> KernelM ()
addDecl decl = modify $ \s -> s {ksDecls = ksDecls s ++ [decl]}

prependDecl :: WGSL.GlobalDecl -> KernelM ()
prependDecl decl = modify $ \s -> s {ksDecls = decl : ksDecls s}

addInitStmt :: WGSL.Statement -> KernelM ()
addInitStmt stmt = modify $ \s -> s {ksInits = ksInits s ++ [stmt]}

addBodyStmt :: WGSL.Statement -> KernelM ()
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

-- | Add an override declaration to the module. Overrides are set when the
-- kernel is launched, and are only used for dynamic block sizes and shared
-- memory sizes.
addOverride :: WGSL.Ident -> WGSL.Type -> Maybe WGSL.Expression -> KernelM ()
addOverride ident typ e = addDecl $ WGSL.GlobalOverride [] ident (Just typ) e

-- | Make an @i32@ constant available to the module, with a value that the
-- host computes when creating the context, and which the runtime then
-- declares as a WGSL @const@ in front of the program (see
-- @backend_context_setup@ in @rts/c/backends/webgpu.h@). This corresponds to
-- the preprocessor macros of the other GPU backends. Unlike an override, a
-- constant can be the length of a function-scope array.
addConst :: WGSL.Ident -> KernelConstExp -> KernelM ()
addConst ident = lift . addMacroDef (nameFromText ident)

-- | The constant holding the low 32 bits of a kernel constant (see
-- 'genConstAndBuiltinDecls').
kernelConstLo :: VName -> KernelM WGSL.Ident
kernelConstLo name = mkGlobalIdent $ nameToIdent name <> "_lo"

-- | Register an override identifier as describing the given dimension of the
-- block size of the current kernel.
addBlockDim :: Int -> WGSL.Ident -> Bool -> KernelM ()
addBlockDim dim ident dynamic =
  modify $ \s -> s {ksBlockDims = ksBlockDims s ++ [(dim, ident, dynamic)]}

-- | Registers an override identifier as describing the size of a shared memory
-- buffer, with the expression being evaluated to get the size when launching
-- the kernel.
addSharedMemSize :: WGSL.Ident -> Exp -> KernelM ()
addSharedMemSize ident e =
  modify $ \s -> s {ksSharedMemSizes = (ident, e) : ksSharedMemSizes s}

addMem :: WGSL.Ident -> MemInfo -> KernelM ()
addMem ident info = modify $ \s -> s {ksMem = M.insert ident info $ ksMem s}

-- | Memory not declared by 'addMem' (such as 'ScalarSpace' arrays) is neither
-- shared nor atomic.
memInfo :: WGSL.Ident -> KernelM MemInfo
memInfo ident = gets $ M.findWithDefault (MemInfo False MemElems) ident . ksMem

-- | Add a scalar struct field.
addScalar :: WGSL.StructMember -> KernelM ()
addScalar field = modify $ \s -> s {ksScalars = ksScalars s ++ [field]}

entryParams :: [WGSL.Param]
entryParams =
  [ WGSL.Param
      [WGSL.Attribute "builtin" [WGSL.ExpIdent "workgroup_id"]]
      "workgroup_id"
      (WGSL.Vec3 WGSL.U32),
    WGSL.Param
      [WGSL.Attribute "builtin" [WGSL.ExpIdent "local_invocation_id"]]
      "local_id"
      (WGSL.Vec3 WGSL.U32)
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
            ksBlockDims = mempty,
            ksMem = mempty,
            ksSharedMemSizes = mempty,
            ksScalars = mempty,
            ksBindSlots = mempty,
            ksReadOnlySlots = mempty,
            ksUniformSlots = mempty
          }

  ((), s, ()) <- runRWST gen (KernelR kernel) initial

  addCode $ docText $ stack $ map pretty $ ksDecls s
  addCode "\n\n"

  let name = nameToText $ ImpGPU.kernelName kernel
  let blockDimNames = [n | (_, n, _) <- ksBlockDims s]
  let attribs =
        [ WGSL.Attribute "compute" [],
          WGSL.Attribute "workgroup_size" (map WGSL.ExpIdent blockDimNames)
        ]
  let wgslFun =
        WGSL.FunctionDecl
          { WGSL.functionName = textToIdent name,
            WGSL.functionAttributes = attribs,
            WGSL.functionParams = entryParams,
            WGSL.functionReturn = Nothing,
            WGSL.functionBody = ksInits s ++ ksBody s
          }
  addCode $ prettyText wgslFun

  -- Dummy layout with single i32 instead of empty structs.
  let scalarMembers = ksScalars s
      offsets = fromMaybe [] $ WGSL.offsetOfMembers scalarMembers
      size = fromMaybe 4 $ WGSL.sizeOfStruct scalarMembers
  let dynamicBlockDims = [(dim, n) | (dim, n, True) <- ksBlockDims s]
  let (sharedMemOverrides, sharedMemExps) = unzip $ ksSharedMemSizes s
  let interface =
        KernelInterface
          { safety = SafetyNone, -- TODO
            scalarsOffsets = offsets,
            scalarsSize = size,
            scalarsBindSlot = head (ksBindSlots s),
            memBindSlots = tail (ksBindSlots s),
            memBindReadOnly = map (`S.member` ksReadOnlySlots s) $ tail (ksBindSlots s),
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

      wgslBody <- genWGSLStms (ImpGPU.kernelBody kernel)
      mapM_ addBodyStmt wgslBody

lookupFunction :: Name -> WebGPUS -> Maybe (ImpGPU.Function ImpGPU.HostOp)
lookupFunction fname = lookup fname . unFunctions . wsFuns

genFunParams :: [Param] -> KernelM [WGSL.Param]
genFunParams =
  mapM $ \case
    MemParam _ _ -> compilerLimitation "WebGPU backend cannot handle GPU functions with memory parameters."
    ScalarParam name tp -> do
      ident <- getIdent name
      pure $ WGSL.Param [] ident (wgslPrimType tp)

generateDeviceFun :: Name -> ImpGPU.Function ImpGPU.KernelOp -> KernelM ()
generateDeviceFun fname device_func = do
  when (any memParam $ functionInput device_func) $
    compilerLimitation "WebGPU backend cannot generate GPU functions that use arrays."
  ks <- get
  r <- ask
  (body, _, _) <- lift $ runRWST (genWGSLStms (functionBody device_func)) r ks

  in_params <- genFunParams (functionInput device_func)
  out_params <- genFunParams (functionOutput device_func)
  -- The body assigns the results to variables named after the output
  -- parameters.  A single result is returned directly; several results are
  -- written through pointer parameters, as WGSL functions return at most one
  -- value.
  let declare (WGSL.Param _ v typ) = WGSL.Var v (Just typ) Nothing
      outPtrParam (WGSL.Param a v t) =
        WGSL.Param a (v <> "_out") (WGSL.Ptr t WGSL.Function Nothing)
      writeOutPtr (WGSL.Param _ v _) =
        WGSL.Assign
          (WGSL.ExpUnary WGSL.Indirection (WGSL.ExpIdent (v <> "_out")))
          WGSL.AssignSimple
          (WGSL.ExpIdent v)
      (fun_params, fun_return, fun_body) = case out_params of
        [] -> (in_params, Nothing, body)
        [p@(WGSL.Param _ v typ)] ->
          ( in_params,
            Just ([], typ),
            [declare p] ++ body ++ [WGSL.Return $ Just $ WGSL.ExpIdent v]
          )
        _ ->
          ( in_params ++ map outPtrParam out_params,
            Nothing,
            map declare out_params ++ body ++ map writeOutPtr out_params
          )
  prependDecl . WGSL.GlobalFunction $
    WGSL.FunctionDecl
      { WGSL.functionName = "futrts_" <> textToIdent (nameToText fname),
        WGSL.functionAttributes = mempty,
        WGSL.functionParams = fun_params,
        WGSL.functionReturn = fun_return,
        WGSL.functionBody = fun_body
      }

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
  let called = calledFuncs (const mempty) code
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
onHostOp (ImpGPU.GetSize v key _) =
  pure $ GetSize v key
onHostOp (ImpGPU.CmpSizeLe v key _ x) =
  pure $ CmpSizeLe v key x
onHostOp (ImpGPU.GetSizeMax v size_class) =
  pure $ GetSizeMax v size_class

-- | Generate WebGPU host and device code.
kernelsToWebGPU :: ImpGPU.Program -> Program
kernelsToWebGPU prog =
  let ImpGPU.Definitions
        params
        types
        (ImpGPU.Constants ps consts)
        (ImpGPU.Functions funs) = prog

      initial_state =
        WebGPUS
          { wsCode = mempty,
            wsMacroDefs = mempty,
            wsKernels = mempty,
            wsNextBindSlot = 0,
            wsDevFuns = mempty,
            wsFuns = defFuns prog
          }

      ((consts', funs'), translation) =
        flip State.runState initial_state $
          (,) <$> traverse onHostOp consts <*> traverse (traverse (traverse onHostOp)) funs

      prog' =
        Definitions params types (Constants ps consts') (Functions funs')

      kernels = M.fromList $ map (first nameFromText) (wsKernels translation)
      constants = wsMacroDefs translation
      failures = mempty
   in Program
        { webgpuProgram = wsCode translation,
          webgpuPrelude = RTS.wgslPrelude,
          webgpuMacroDefs = constants,
          webgpuKernels = kernels,
          webgpuFailures = failures,
          hostDefinitions = prog'
        }

-- | Compile the program to ImpCode with WebGPU kernels.
compileProg :: (MonadFreshNames m) => F.Prog F.GPUMem -> m (Warnings, Program)
compileProg prog = second kernelsToWebGPU <$> ImpGPU.compileProgOpenCL prog

wgslInt8, wgslInt16, wgslInt64 :: WGSL.Type
wgslInt8 = WGSL.I32
wgslInt16 = WGSL.I32
wgslInt64 = WGSL.Vec2 WGSL.I32

wgslPrimType :: PrimType -> WGSL.Type
wgslPrimType (IntType Int8) = wgslInt8
wgslPrimType (IntType Int16) = wgslInt16
wgslPrimType (IntType Int32) = WGSL.I32
wgslPrimType (IntType Int64) = wgslInt64
wgslPrimType (FloatType Float16) = WGSL.F16
wgslPrimType (FloatType Float32) = WGSL.F32
wgslPrimType (FloatType Float64) = compilerLimitation "WebGPU backend does not support f64."
wgslPrimType Bool = WGSL.Bool
-- TODO: Make sure we do not ever codegen statements involving Unit variables
wgslPrimType Unit = WGSL.F16 -- error "TODO: no unit in WGSL"

-- | Types smaller than a word. These are packed into words, which WGSL can only
-- modify in part with atomics, as neighbouring elements may be written
-- concurrently.
packedType :: PrimType -> Bool
packedType t = t `elem` [Bool, IntType Int8, IntType Int16]

-- | Whether memory whose elements have this type, and which is accessed
-- atomically or not, is stored as an array of atomic words. This is the same
-- for storage buffers and workgroup memory, and for the latter ensures that
-- the memory occupies no more than the number of bytes Futhark asks for.
wordStorage :: (PrimType, Bool) -> Bool
wordStorage (t, atomic) = atomic || packedType t

-- | The representation of writable memory with elements of this type, accessed
-- atomically or not.
writableRepr :: (PrimType, Bool) -> MemRepr
writableRepr typ = if wordStorage typ then MemAtomicWords else MemElems

-- | The type of memory with the given element type and atomicity. An atomic
-- i64 occupies two words; see 'i64Words'.
wgslBufferType :: (PrimType, Bool) -> Maybe WGSL.Expression -> WGSL.Type
wgslBufferType (t, atomic)
  | wordStorage (t, atomic) = WGSL.Array $ WGSL.Atomic WGSL.I32
  | otherwise = WGSL.Array $ wgslPrimType t

-- | How many elements of this type are stored in each word of an atomic
-- buffer.
elemsPerWord :: PrimType -> Int
elemsPerWord Bool = 4
elemsPerWord (IntType Int8) = 4
elemsPerWord (IntType Int16) = 2
elemsPerWord (FloatType Float16) = 2
elemsPerWord (IntType Int32) = 1
elemsPerWord (FloatType Float32) = 1
elemsPerWord t = error $ "elemsPerWord: unsupported type " <> prettyString t

-- | A pointer to the word holding an element, and the position of the element
-- within it. These are the first arguments of the @atomic_@ functions.
wordAccess :: PrimType -> WGSL.Ident -> WGSL.Expression -> (WGSL.Expression, WGSL.Expression)
wordAccess t mem i = (WGSL.ExpUnary WGSL.AddressOf $ WGSL.ExpIndex (WGSL.ExpIdent mem) word_i, word_offset)
  where
    (word_i, word_offset) = wordIndex t i

-- | The index of the word holding an element, and the position of the element
-- within it.
wordIndex :: PrimType -> WGSL.Expression -> (WGSL.Expression, WGSL.Expression)
wordIndex t i = case elemsPerWord t of
  1 -> (i, WGSL.ExpInt 0 WGSL.AbstractInt)
  n ->
    ( WGSL.ExpBinary WGSL.Divide i (WGSL.ExpInt n WGSL.AbstractInt),
      WGSL.ExpBinary WGSL.Remainder i (WGSL.ExpInt n WGSL.AbstractInt)
    )

-- | The name of an @atomic_@ function from @rts/wgsl/atomics.wgsl@.
atomicFun :: WGSL.Ident -> PrimType -> MemInfo -> WGSL.Ident
atomicFun f t info =
  "atomic_" <> f <> "_" <> prettyText t <> if memShared info then "_shared" else "_global"

-- | Read an element. Memory not stored as words is read directly.
genReadExp :: PrimType -> VName -> Count Elements (TExp Int64) -> KernelM WGSL.Expression
genReadExp t mem i = do
  mem' <- getIdent mem
  info <- memInfo mem'
  i' <- indexExp i
  pure $
    if
      | memRepr info == MemElems -> WGSL.ExpIndex (WGSL.ExpIdent mem') i'
      | memRepr info == MemReadOnlyWords ->
          let (word, offset) = wordIndex t i'
           in call ("unpack_" <> prettyText t) [WGSL.ExpIndex (WGSL.ExpIdent mem') word, offset]
      | t == IntType Int64 ->
          let (lo, hi) = i64Words mem' i'
              load w = call "atomicLoad" [WGSL.ExpUnary WGSL.AddressOf w]
           in call "i64" [load lo, load hi]
      | otherwise ->
          let (word, offset) = wordAccess t mem' i'
           in call (atomicFun "read" t info) [word, offset]

-- | Write an element. Memory not stored as words is written directly.
genArrayWrite ::
  PrimType ->
  VName ->
  Count Elements (TExp Int64) ->
  KernelM WGSL.Expression ->
  KernelM WGSL.Statement
genArrayWrite t mem i v = do
  mem' <- getIdent mem
  info <- memInfo mem'
  i' <- indexExp i
  v' <- v
  pure $
    if
      | memRepr info == MemReadOnlyWords -> error "genArrayWrite: read-only memory"
      | memRepr info == MemElems ->
          WGSL.Assign (WGSL.ExpIndex (WGSL.ExpIdent mem') i') WGSL.AssignSimple v'
      | t == IntType Int64 ->
          let (lo, hi) = i64Words mem' i'
              tmp = WGSL.ExpIdent "i64_tmp"
              store w j =
                callStm "atomicStore" [WGSL.ExpUnary WGSL.AddressOf w, WGSL.ExpIndex tmp (WGSL.ExpInt j WGSL.AbstractInt)]
           in WGSL.Compound [WGSL.Let "i64_tmp" Nothing v', store lo 0, store hi 1]
      | otherwise ->
          let (word, offset) = wordAccess t mem' i'
           in callStm (atomicFun "write" t info) [word, offset, v']

-- | WGSL has no 64-bit atomics, so an i64 buffer that is accessed atomically is
-- an array of word pairs (low, high), and each half is accessed atomically by
-- itself. This suffices for the only operations we support, which are reads
-- and writes ('ImpGPU.AtomicWrite') that must be visible to other threads once
-- these have synchronised with the writer, e.g. through an atomic counter, as
-- the value is then not concurrently modified. Returns the two words of the
-- element at the given index.
i64Words :: WGSL.Ident -> WGSL.Expression -> (WGSL.Expression, WGSL.Expression)
i64Words mem i =
  ( WGSL.ExpIndex (WGSL.ExpIdent mem) lo,
    WGSL.ExpIndex (WGSL.ExpIdent mem) (WGSL.ExpBinary WGSL.Add lo (WGSL.ExpInt 1 WGSL.AbstractInt))
  )
  where
    lo = WGSL.ExpBinary WGSL.Multiply (WGSL.ExpInt 2 WGSL.AbstractInt) i

genAtomicOp ::
  WGSL.Ident ->
  PrimType ->
  VName ->
  VName ->
  Count Elements (TExp Int64) ->
  [Exp] ->
  KernelM WGSL.Statement
genAtomicOp f t dest mem i es = do
  mem' <- getIdent mem
  info <- memInfo mem'
  i' <- indexExp i
  es' <- mapM genWGSLExp es
  let (word, offset) = wordAccess t mem' i'
  assign <$> getIdent dest <*> pure (call (atomicFun f t info) (word : offset : es'))

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
  KernelM WGSL.Statement
genCopy pt shape (dst, _) (dst_offset, dst_strides) (src, _) (src_offset, src_strides) = do
  shape' <- mapM (genWGSLExp . untyped . unCount) shape
  let dst_i = dst_offset + sum (zipWith (*) is' dst_strides)
      src_i = src_offset + sum (zipWith (*) is' src_strides)
  body <- genArrayWrite pt dst dst_i $ genReadExp pt src src_i
  pure . WGSL.Compound $ foldr loop [body] (zip iis shape')
  where
    is = map (VName "i") [0 .. length shape - 1]
    is' :: [Count Elements (TExp Int64)]
    is' = map (elements . le64) is
    iis = map nameToIdent is
    loop (i, n) body = [forLoop Int64 i n body]

-- | @for (var i = 0; i < bound; i++) { body }@, for an unsigned bound of the
-- given type.
forLoop :: IntType -> WGSL.Ident -> WGSL.Expression -> [WGSL.Statement] -> WGSL.Statement
forLoop t i bound =
  WGSL.For
    i
    zero
    (wgslCmpOp (CmpUlt t) (WGSL.ExpIdent i) bound)
    (WGSL.UpdateAssign (WGSL.ExpIdent i) WGSL.AssignSimple $ wgslBinOp (Add t OverflowWrap) (WGSL.ExpIdent i) one)
  where
    (zero, one) = case t of
      Int64 -> (WGSL.ExpIdent "zero_i64", WGSL.ExpIdent "one_i64")
      _ -> (WGSL.ExpInt 0 WGSL.AbstractInt, WGSL.ExpInt 1 WGSL.AbstractInt)

-- | Assign to a variable.
assign :: WGSL.Ident -> WGSL.Expression -> WGSL.Statement
assign v = WGSL.Assign (WGSL.ExpIdent v) WGSL.AssignSimple

-- | Call a function with no template arguments.
call :: WGSL.Ident -> [WGSL.Expression] -> WGSL.Expression
call f = WGSL.ExpCall f []

-- | Call a function with no template arguments, as a statement.
callStm :: WGSL.Ident -> [WGSL.Expression] -> WGSL.Statement
callStm f = WGSL.Call f []

unsupported :: Code ImpGPU.KernelOp -> KernelM WGSL.Statement
unsupported stmt = pure $ WGSL.Comment $ "Unsupported stmt: " <> prettyText stmt

-- | The number of elements in a 'ScalarSpace' block of the given shape. WGSL
-- requires this to be a constant expression, so each dimension must be a
-- constant or a kernel constant (see 'addConst').
scalarSpaceSize :: [SubExp] -> KernelM WGSL.Expression
scalarSpaceSize ds
  -- As in the C backends: a zero can occur for some empty array literals.
  | null ds || Constant (IntValue (Int64Value 0)) `elem` ds = pure one
  | otherwise = foldl (WGSL.ExpBinary WGSL.Multiply) one <$> mapM dim ds
  where
    one = WGSL.ExpInt 1 WGSL.AbstractInt
    dim (Constant (IntValue v)) = pure $ WGSL.ExpInt (valueIntegral v) WGSL.AbstractInt
    dim (Constant v) = error $ "scalarSpaceSize: non-integer size " <> prettyString v
    dim (Var v) = do
      uses <- asks (ImpGPU.kernelUses . krKernel)
      if v `elem` [n | ImpGPU.ConstUse n _ <- uses]
        then WGSL.ExpIdent <$> kernelConstLo v
        else
          compilerLimitation $
            "WebGPU backend cannot handle per-thread array of non-constant size: "
              <> prettyText v

-- | Whether the WGSL translation of this code contains a barrier. Besides
-- 'ImpGPU.isBarrier', this includes 'ImpGPU.UniformRead', as
-- @workgroupUniformLoad@ is a barrier.
hasBarrier :: Code ImpGPU.KernelOp -> Bool
hasBarrier = any $ \op -> ImpGPU.isBarrier op || isUniformRead op
  where
    isUniformRead ImpGPU.UniformRead {} = True
    isUniformRead _ = False

-- | Produce statements and an expression that yields the value of the given
-- expression in a form that WGSL's uniformity analysis considers uniform. The
-- analysis treats any value read from workgroup memory or a read_write storage
-- buffer as non-uniform, and so rejects barriers under conditions that Futhark
-- guarantees to be uniform across the block. Rather than replicate the analysis,
-- conditions of control flow containing a barrier always go through
-- @workgroupUniformLoad@ (see 'hasBarrier'). All threads write the value to a
-- per-kernel workgroup variable; the preceding barrier ensures no thread is
-- still reading a previous value from it, as the spec does not say whether the
-- barrier in @workgroupUniformLoad@ precedes or follows the load.
uniformValue :: PrimType -> WGSL.Expression -> KernelM ([WGSL.Statement], WGSL.Expression)
uniformValue t e = do
  slots <- gets ksUniformSlots
  slot <- case M.lookup t slots of
    Just slot -> pure slot
    Nothing -> do
      slot <- mkGlobalIdent $ "uniform_" <> prettyText t
      addDecl $
        WGSL.GlobalVariable [] WGSL.Workgroup Nothing slot (Just $ wgslPrimType t) Nothing
      modify $ \s -> s {ksUniformSlots = M.insert t slot $ ksUniformSlots s}
      pure slot
  pure
    ( [callStm "workgroupBarrier" [], assign slot e],
      call "workgroupUniformLoad" [WGSL.ExpUnary WGSL.AddressOf $ WGSL.ExpIdent slot]
    )

-- | The statements of a code tree, with sequencing flattened away.
codeStms :: Code ImpGPU.KernelOp -> [Code ImpGPU.KernelOp]
codeStms Skip = []
codeStms (s1 :>>: s2) = codeStms s1 ++ codeStms s2
codeStms s = [s]

-- | Generate the statements of a code tree.
genWGSLStms :: Code ImpGPU.KernelOp -> KernelM [WGSL.Statement]
genWGSLStms = mapM genWGSLStm . codeStms

genWGSLStm :: Code ImpGPU.KernelOp -> KernelM WGSL.Statement
-- 'genWGSLStms' flattens these two away; the cases are here for totality.
genWGSLStm Skip = pure WGSL.Empty
genWGSLStm c@(_ :>>: _) = WGSL.Compound <$> genWGSLStms c
genWGSLStm (For iName bound body) = do
  boundExp <- genWGSLExp bound
  bodyStm <- genWGSLStms body
  let loop bound' = forLoop boundIntType i bound' bodyStm
  if hasBarrier body
    then do
      (pre, uniform_bound) <- uniformValue (IntType boundIntType) boundExp
      let bound_ident = i <> "_bound"
      pure . WGSL.Compound $
        pre
          ++ [ WGSL.Let bound_ident Nothing uniform_bound,
               loop $ WGSL.ExpIdent bound_ident
             ]
    else pure $ loop boundExp
  where
    i = nameToIdent iName
    boundIntType = case primExpType bound of
      IntType t -> t
      _ -> error "non-integer Exp for loop bound"
genWGSLStm (While cond body) = do
  cond' <- genWGSLExp $ untyped cond
  body' <- genWGSLStms body
  if hasBarrier body
    then do
      (pre, uniform_cond) <- uniformValue Bool cond'
      let exit = WGSL.If (WGSL.ExpUnary WGSL.LogicalNegate uniform_cond) [WGSL.Break] [] Nothing
      pure $ WGSL.Loop (pre ++ [exit] ++ body') Nothing
    else pure $ WGSL.While cond' body'
genWGSLStm (DeclareMem name (Space "shared")) = do
  let name' = nameToIdent name
  moduleName <- mkGlobalIdent name'
  sizeName <- mkGlobalIdent $ name' <> "_size"

  maybeElemPrimType <- findSingleMemoryType name
  case maybeElemPrimType of
    Just typ -> do
      let bufType = wgslBufferType typ (Just $ WGSL.ExpIdent sizeName)

      addOverride sizeName WGSL.I32 (Just $ WGSL.ExpInt 0 WGSL.AbstractInt)
      addDecl $
        WGSL.GlobalVariable [] WGSL.Workgroup Nothing moduleName (Just bufType) Nothing
      addMem moduleName $ MemInfo {memShared = True, memRepr = writableRepr typ}
      addRename name' moduleName
      pure $ WGSL.Comment $ "declare_shared: " <> name'
    Nothing ->
      pure $ WGSL.Comment $ "discard declare_shared: " <> name'
genWGSLStm (DeclareMem name (ScalarSpace vs pt)) = do
  size <- scalarSpaceSize vs
  pure $
    WGSL.Var
      (nameToIdent name)
      (Just $ WGSL.Array (wgslPrimType pt) (Just size))
      Nothing
genWGSLStm s@(DeclareMem _ _) = unsupported s
genWGSLStm (DeclareScalar name _ typ) =
  pure $
    WGSL.Var (nameToIdent name) (Just $ wgslPrimType typ) Nothing
genWGSLStm s@(DeclareArray {}) = unsupported s
genWGSLStm s@(Allocate {}) = unsupported s
genWGSLStm s@(Free _ _) = pure $ WGSL.Comment $ "free: " <> prettyText s
genWGSLStm (Copy pt shape dst dst_lmad src src_lmad) = genCopy pt shape dst dst_lmad src src_lmad
genWGSLStm (Write mem i t _ _ v) = genArrayWrite t mem i (genWGSLExp v)
genWGSLStm (SetScalar name e) =
  liftM2 assign (getIdent name) (genWGSLExp e)
genWGSLStm (Read tgt mem i t _ _) = assign <$> getIdent tgt <*> genReadExp t mem i
genWGSLStm stm@(SetMem {}) =
  compilerLimitation . docText $
    "WebGPU backend Cannot handle SetMem statement"
      </> indent 2 (align (pretty stm))
      </> "in GPU kernel."
-- See 'generateDeviceFun' for how results are returned.
genWGSLStm (Call dests f args) = do
  fname <- ("futrts_" <>) <$> getIdent f
  args' <- mapM getArg args
  dests' <- mapM getIdent dests
  pure $ case dests' of
    [dest] -> assign dest $ call fname args'
    _ -> callStm fname $ args' ++ map (WGSL.ExpUnary WGSL.AddressOf . WGSL.ExpIdent) dests'
  where
    getArg (ExpArg e) = genWGSLExp e
    getArg (MemArg n) = WGSL.ExpIdent <$> getIdent n
genWGSLStm (If cond cThen cElse) = do
  cond' <- genWGSLExp $ untyped cond
  cThen' <- genWGSLStms cThen
  cElse' <- genWGSLStms cElse
  let branch cond'' =
        WGSL.If cond'' cThen' [] $
          if null cElse' then Nothing else Just cElse'
  if hasBarrier cThen || hasBarrier cElse
    then do
      (pre, uniform_cond) <- uniformValue Bool cond'
      pure $ WGSL.Compound $ pre ++ [branch uniform_cond]
    else pure $ branch cond'
genWGSLStm s@(Assert {}) = pure $ WGSL.Comment $ "assert: " <> prettyText s
genWGSLStm (Meta (MetaComment c)) =
  pure $ WGSL.Comment c
genWGSLStm (Meta (MetaProvenance (Provenance _ l))) =
  pure $ WGSL.Comment $ locText l
genWGSLStm (DebugPrint _ _) = pure WGSL.Empty
genWGSLStm (TracePrint _) = pure WGSL.Empty
genWGSLStm (Op (ImpGPU.GetBlockId dest i)) = do
  destId <- getIdent dest
  pure $
    assign destId $
      WGSL.bitcast WGSL.I32 (WGSL.ExpIndex (WGSL.ExpIdent "workgroup_id") (WGSL.ExpInt i WGSL.AbstractInt))
genWGSLStm (Op (ImpGPU.GetLocalId dest i)) = do
  destId <- getIdent dest
  pure $
    assign destId $
      WGSL.bitcast WGSL.I32 (WGSL.ExpIndex (WGSL.ExpIdent "local_id") (WGSL.ExpInt i WGSL.AbstractInt))
genWGSLStm (Op (ImpGPU.GetLocalSize dest i)) = do
  destId <- getIdent dest
  assign destId . WGSL.ExpIdent <$> builtinBlockSize i
genWGSLStm (Op (ImpGPU.GetLockstepWidth dest)) = do
  destId <- getIdent dest
  assign destId . WGSL.ExpIdent <$> builtinLockstepWidth
-- WGSL has no 64-bit atomics. An atomic write is only a write that must be
-- visible to other threads, and this is supported (see 'i64Words'), but
-- read-modify-write operations are not.
genWGSLStm (Op (ImpGPU.Atomic _ op))
  | ImpGPU.atomicOpType op == IntType Int64,
    not (isWrite op) =
      compilerLimitation "WebGPU backend does not support 64-bit atomic operations."
  where
    isWrite ImpGPU.AtomicWrite {} = True
    isWrite _ = False
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicAdd t dest mem i e))) =
  genAtomicOp "add" (IntType t) dest mem i [e]
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicSMax t dest mem i e))) =
  genAtomicOp "smax" (IntType t) dest mem i [e]
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicSMin t dest mem i e))) =
  genAtomicOp "smin" (IntType t) dest mem i [e]
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicUMax t dest mem i e))) =
  genAtomicOp "umax" (IntType t) dest mem i [e]
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicUMin t dest mem i e))) =
  genAtomicOp "umin" (IntType t) dest mem i [e]
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicAnd t dest mem i e))) =
  genAtomicOp "and" (IntType t) dest mem i [e]
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicOr t dest mem i e))) =
  genAtomicOp "or" (IntType t) dest mem i [e]
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicXor t dest mem i e))) =
  genAtomicOp "xor" (IntType t) dest mem i [e]
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicCmpXchg t dest mem i cmp val))) =
  genAtomicOp "cmp_xchg" t dest mem i [cmp, val]
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicXchg t dest mem i e))) =
  genAtomicOp "xchg" t dest mem i [e]
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicWrite t mem i v))) = genArrayWrite t mem i (genWGSLExp v)
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicFAdd t dest mem i e))) = genAtomicOp "fadd" (FloatType t) dest mem i [e]
genWGSLStm (Op (ImpGPU.Barrier ImpGPU.FenceLocal)) =
  pure $ callStm "workgroupBarrier" []
genWGSLStm (Op (ImpGPU.Barrier ImpGPU.FenceGlobal)) =
  pure $ WGSL.Compound [callStm "storageBarrier" [], callStm "workgroupBarrier" []]
genWGSLStm s@(Op (ImpGPU.MemFence _)) = unsupported s
genWGSLStm (Op (ImpGPU.SharedAlloc name size)) = do
  let name' = nameToIdent name
  sizeName <- mkGlobalIdent $ name' <> "_size"

  maybeElemPrimType <- findSingleMemoryType name
  case maybeElemPrimType of
    Just typ@(elemPrimType, _) -> do
      let elemSize
            | wordStorage typ = 4
            | otherwise = primByteSize elemPrimType :: Int32
      -- WGSL does not permit arrays with zero elements, but the
      -- allocation is empty whenever the kernel has no work to do. Words may
      -- be partially used, so we round up.
      let sizeExp =
            BinOpExp
              (SMax Int32)
              (ValueExp (IntValue $ Int32Value 1))
              ( BinOpExp
                  (SCeilDiv Int32 Unsafe)
                  (zExt Int32 (untyped (unCount size)))
                  (ValueExp (IntValue $ Int32Value elemSize))
              )

      addSharedMemSize sizeName sizeExp
      pure $ WGSL.Comment $ "shared_alloc: " <> name'
    Nothing ->
      pure $ WGSL.Comment $ "discard shared_alloc: " <> name'
genWGSLStm (Op (ImpGPU.UniformRead tgt mem i t _)) = do
  tgt' <- getIdent tgt
  mem' <- getIdent mem
  info <- memInfo mem'
  i' <- indexExp i
  pure . assign tgt' $
    if memRepr info /= MemElems
      then
        -- Load the whole word uniformly, then extract the element.
        let (word, offset) = wordAccess t mem' i'
         in call ("unpack_" <> prettyText t) [call "workgroupUniformLoad" [word], offset]
      else call "workgroupUniformLoad" [WGSL.ExpUnary WGSL.AddressOf $ WGSL.ExpIndex (WGSL.ExpIdent mem') i']
genWGSLStm (Op (ImpGPU.ErrorSync f)) = genWGSLStm $ Op (ImpGPU.Barrier f)
genWGSLStm (Op ImpGPU.WarpSync {}) = error "genWGSLStm: WarpSync not handled."
genWGSLStm GetUserParam {} = error "genWGSLStm: GetUserParam not handled."

call1 :: WGSL.Ident -> WGSL.Expression -> WGSL.Expression
call1 f a = call f [a]

call2 :: WGSL.Ident -> WGSL.Expression -> WGSL.Expression -> WGSL.Expression
call2 f a b = call f [a, b]

call2Suffix :: WGSL.Ident -> IntType -> WGSL.Expression -> WGSL.Expression -> WGSL.Expression
call2Suffix f t a b = call (f <> "_" <> prettyText t) [a, b]

wgslBinOp :: BinOp -> WGSL.Expression -> WGSL.Expression -> WGSL.Expression
wgslBinOp (Add Int32 _) = WGSL.ExpBinary WGSL.Add
wgslBinOp (Add t _) = call2Suffix "add" t
wgslBinOp (FAdd _) = WGSL.ExpBinary WGSL.Add
wgslBinOp (Sub Int32 _) = WGSL.ExpBinary WGSL.Subtract
wgslBinOp (Sub t _) = call2Suffix "sub" t
wgslBinOp (FSub _) = WGSL.ExpBinary WGSL.Subtract
wgslBinOp (Mul Int32 _) = WGSL.ExpBinary WGSL.Multiply
wgslBinOp (Mul t _) = call2Suffix "mul" t
wgslBinOp (FMul _) = WGSL.ExpBinary WGSL.Multiply
-- Division is always safe in WGSL, so we can ignore the Safety parameter.
wgslBinOp (UDiv t _) = call2Suffix "udiv" t
wgslBinOp (UCeilDiv t _) = call2Suffix "uceil_div" t
wgslBinOp (SDiv t _) = call2Suffix "sdiv" t
wgslBinOp (SCeilDiv t _) = call2Suffix "sceil_div" t
wgslBinOp (FDiv _) = WGSL.ExpBinary WGSL.Divide
wgslBinOp (FMod _) = WGSL.ExpBinary WGSL.Remainder
wgslBinOp (UMod t _) = call2Suffix "umod" t
wgslBinOp (SMod t _) = call2Suffix "smod" t
wgslBinOp (SQuot Int8 _) = call2 "squot_i8"
wgslBinOp (SQuot Int16 _) = call2 "squot_i16"
wgslBinOp (SQuot Int32 _) = WGSL.ExpBinary WGSL.Divide
wgslBinOp (SQuot Int64 _) = call2 "squot_i64"
wgslBinOp (SRem Int8 _) = WGSL.ExpBinary WGSL.Remainder
wgslBinOp (SRem Int16 _) = WGSL.ExpBinary WGSL.Remainder
wgslBinOp (SRem Int32 _) = WGSL.ExpBinary WGSL.Remainder
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
wgslBinOp (And _) = WGSL.ExpBinary WGSL.BitwiseAnd
wgslBinOp (Or _) = WGSL.ExpBinary WGSL.BitwiseOr
wgslBinOp (Xor _) = WGSL.ExpBinary WGSL.BitwiseExclusiveOr
wgslBinOp (SPow t) = call2Suffix "pow" t
wgslBinOp (UPow t) = call2Suffix "pow" t
wgslBinOp (FPow _) = call2 "pow"
wgslBinOp LogAnd = call2 "log_and"
wgslBinOp LogOr = call2 "log_or"

-- Because we (in e.g. scalar8.wgsl) make sure to always sign-extend an i8 value
-- across its whole i32 representation, we can just use the normal comparison
-- operators for smaller integers. The same applies for i16.
wgslCmpOp :: CmpOp -> WGSL.Expression -> WGSL.Expression -> WGSL.Expression
wgslCmpOp (CmpEq (IntType Int64)) = call2 "eq_i64"
wgslCmpOp (CmpEq _) = WGSL.ExpBinary WGSL.Equal
wgslCmpOp (CmpUlt Int64) = call2 "ult_i64"
wgslCmpOp (CmpUlt _) = call2 "ult_i32"
wgslCmpOp (CmpUle Int64) = call2 "ule_i64"
wgslCmpOp (CmpUle _) = call2 "ule_i32"
wgslCmpOp (CmpSlt Int64) = call2 "slt_i64"
wgslCmpOp (CmpSlt _) = WGSL.ExpBinary WGSL.Less
wgslCmpOp (CmpSle Int64) = call2 "sle_i64"
wgslCmpOp (CmpSle _) = WGSL.ExpBinary WGSL.LessEqual
wgslCmpOp (FCmpLt _) = WGSL.ExpBinary WGSL.Less
wgslCmpOp (FCmpLe _) = WGSL.ExpBinary WGSL.LessEqual
wgslCmpOp CmpLlt = call2 "llt"
wgslCmpOp CmpLle = call2 "lle"

-- Similarly to CmpOps above, the defaults work for smaller integers already
-- given our representation.
wgslUnOp :: UnOp -> WGSL.Expression -> WGSL.Expression
wgslUnOp (Neg (FloatType _)) = WGSL.ExpUnary WGSL.Negate
wgslUnOp (Neg (IntType t)) = call1 $ "neg_" <> prettyText t
wgslUnOp (Neg _) = WGSL.ExpUnary WGSL.LogicalNegate
wgslUnOp (Complement _) = WGSL.ExpUnary WGSL.BitwiseComplement
wgslUnOp (Abs Int64) = call1 "abs_i64"
wgslUnOp (Abs _) = call1 "abs"
wgslUnOp (FAbs _) = call1 "abs"
wgslUnOp (SSignum Int64) = call1 "ssignum_i64"
wgslUnOp (SSignum _) = call1 "sign"
wgslUnOp (USignum Int64) = call1 "usignum_i64"
wgslUnOp (USignum _) = call1 "usignum_i32"
wgslUnOp (FSignum _) = call1 "sign"

wgslConvOp :: ConvOp -> WGSL.Expression -> WGSL.Expression
wgslConvOp op a = call (fun op) [a]
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
    fun (FPToUI Float16 Int64) = "f16_to_u64"
    fun (FPToUI Float32 Int64) = "f32_to_u64"
    fun (FPToSI Float16 Int8) = "f16_to_i8"
    fun (FPToSI Float16 Int16) = "f16_to_i16"
    fun (FPToSI Float16 Int32) = "f16_to_i32"
    fun (FPToSI Float32 Int8) = "f32_to_i8"
    fun (FPToSI Float32 Int16) = "f32_to_i16"
    fun (FPToSI Float32 Int32) = "f32_to_i32"
    fun (FPToSI Float16 Int64) = "f16_to_i64"
    fun (FPToSI Float32 Int64) = "f32_to_i64"
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
    fun (FPConv _ Float16) = "f16"
    fun (FPConv _ Float32) = "f32"
    fun (FPToBits Float16) = "futrts_to_bits16"
    fun (FPToBits Float32) = "futrts_to_bits32"
    fun (BitsToFP Float16) = "futrts_from_bits16"
    fun (BitsToFP Float32) = "futrts_from_bits32"
    fun (FToB _) = "bool"
    fun (BToF Float16) = "f16"
    fun (BToF Float32) = "f32"
    fun o = "not_implemented(" <> prettyText o <> ")"

intLiteral :: IntValue -> WGSL.Expression
intLiteral (Int8Value v) =
  call "norm_i8" [WGSL.ExpInt (fromIntegral v) WGSL.AbstractInt]
intLiteral (Int16Value v) =
  call "norm_i16" [WGSL.ExpInt (fromIntegral v) WGSL.AbstractInt]
intLiteral (Int64Value v) = call "i64" [low, high]
  where
    -- Helper function transforming a Int64 -> Int32 -> WGSL.Expression.
    -- If we just do Int64 -> WGSL.Expression directly, low/high can end
    -- up being larger than what fits in an i32.
    toWGSLInt :: Int64 -> WGSL.Expression
    toWGSLInt x = WGSL.ExpInt (fromIntegral (fromIntegral x :: Int32)) WGSL.AbstractInt
    low = toWGSLInt $ v Bits..&. 0xffffffff
    high = toWGSLInt $ (v `Bits.shift` (-32)) Bits..&. 0xffffffff
intLiteral v =
  fromMaybe (compilerLimitation msg) $
    WGSL.intExp (valueIntegral v) WGSL.AbstractInt
  where
    msg = "WebGPU backend cannot represent integer literal: " <> prettyText v

handleSpecialFloats :: T.Text -> WGSL.FloatLiteralType -> Double -> WGSL.Expression
handleSpecialFloats s t v
  | isInfinite v, v > 0 = call (s <> "_inf") []
  | isInfinite v, v < 0 = call (s <> "_neg_inf") []
  | isNaN v = call (s <> "_nan") []
  | otherwise = fromMaybe (compilerLimitation msg) $ WGSL.floatExp v t
  where
    msg = "WebGPU backend cannot represent " <> s <> " literal: " <> prettyText v

genFloatExp :: FloatValue -> WGSL.Expression
genFloatExp (Float16Value v) = handleSpecialFloats "f16" WGSL.LitF16 (convFloat v)
genFloatExp (Float32Value v) = handleSpecialFloats "f32" WGSL.LitF32 (convFloat v)
genFloatExp (Float64Value v) = handleSpecialFloats "f64" WGSL.AbstractFloat v

genWGSLExp :: Exp -> KernelM WGSL.Expression
genWGSLExp (LeafExp name _) = WGSL.ExpIdent <$> getIdent name
genWGSLExp (ValueExp (IntValue v)) = pure $ intLiteral v
genWGSLExp (ValueExp (FloatValue v)) = pure $ genFloatExp v
genWGSLExp (ValueExp (BoolValue v)) = pure $ WGSL.ExpBool v
genWGSLExp (ValueExp UnitValue) =
  error "should not attempt to generate unit expressions"
genWGSLExp (BinOpExp op e1 e2) =
  liftM2 (wgslBinOp op) (genWGSLExp e1) (genWGSLExp e2)
genWGSLExp (CmpOpExp op e1 e2) =
  liftM2 (wgslCmpOp op) (genWGSLExp e1) (genWGSLExp e2)
genWGSLExp (UnOpExp op e) = wgslUnOp op <$> genWGSLExp e
genWGSLExp (ConvOpExp op e) = wgslConvOp op <$> genWGSLExp e
genWGSLExp e =
  compilerLimitation $ "WebGPU backend cannot generate expression: " <> prettyText e

-- We support 64-bit arithmetic, but since WGSL does not have support for it,
-- we cannot use a 64-bit value as an index, so we have to truncate it to 32
-- bits.
indexExp :: Count Elements (TExp Int64) -> KernelM WGSL.Expression
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
            Bool -> WGSL.I32 -- bool is not host-shareable
            _ -> varPrimTyp
      let wrapCopy e = case typ of
            Bool -> call "bool" [e]
            _ -> e

      addScalar $ WGSL.StructMember [] name fieldPrimTyp
      addInitStmt $ WGSL.Var name (Just varPrimTyp) Nothing
      addInitStmt $
        assign name (wrapCopy $ WGSL.ExpMember (WGSL.ExpIdent bufferName) name)

      pure (name, fieldPrimTyp)

  let scalarFields = case scalars of
        [] -> [("_dummy_scalar", WGSL.I32)]
        sclrs -> sclrs
  addDecl $
    WGSL.GlobalStruct $
      WGSL.StructDecl structName (map (uncurry (WGSL.StructMember [])) scalarFields)

  slot <- assignBindSlot
  let bufferAttribs = WGSL.bindingAttribs 0 slot
  addDecl $
    WGSL.GlobalVariable
      bufferAttribs
      WGSL.Uniform
      Nothing
      bufferName
      (Just $ WGSL.Named structName)
      Nothing

-- | Internally, memory buffers are untyped but WGSL requires us to annotate the
-- binding with a type. Search the kernel body for any reads and writes to the
-- given buffer and return all types it is accessed at, and whether the access
-- is atomic.
findMemoryTypes :: VName -> KernelM [(PrimType, Bool)]
findMemoryTypes name = S.elems . find <$> asks (ImpGPU.kernelBody . krKernel)
  where
    find (ImpGPU.Write n _ t _ _ _) | n == name = S.singleton (t, False)
    find (ImpGPU.Read _ n _ t _ _) | n == name = S.singleton (t, False)
    find (ImpGPU.Copy t _ (n, _) _ _ _) | n == name = S.singleton (t, False)
    find (ImpGPU.Copy t _ _ _ (n, _) _) | n == name = S.singleton (t, False)
    find (Op (ImpGPU.Atomic _ op))
      | ImpGPU.atomicOpMem op == name = S.singleton (ImpGPU.atomicOpType op, True)
    find (s1 :>>: s2) = find s1 <> find s2
    find (For _ _ body) = find body
    find (While _ body) = find body
    find (If _ s1 s2) = find s1 <> find s2
    find _ = S.empty

-- | Whether the kernel writes to the buffer.  Atomic operations count as
-- writes.
writesMemory :: VName -> KernelM Bool
writesMemory name = find <$> asks (ImpGPU.kernelBody . krKernel)
  where
    find (ImpGPU.Write n _ _ _ _ _) = n == name
    find (ImpGPU.Copy _ _ (n, _) _ _ _) = n == name
    find (Op (ImpGPU.Atomic _ op)) = ImpGPU.atomicOpMem op == name
    find (s1 :>>: s2) = find s1 || find s2
    find (For _ _ body) = find body
    find (While _ body) = find body
    find (If _ s1 s2) = find s1 || find s2
    find _ = False

-- | The single type at which the kernel accesses the buffer, and whether any
-- access is atomic.
findSingleMemoryType :: VName -> KernelM (Maybe (PrimType, Bool))
findSingleMemoryType name = do
  types <- findMemoryTypes name
  case nubOrd $ map fst types of
    [] -> pure Nothing
    [t] -> pure $ Just (t, any snd types)
    _tooMany -> error "Buffer used at multiple types"

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
  mapM_ moduleDecl memUses
  mapM_ rename memUses
  where
    withType name = do
      typ <- findSingleMemoryType name
      written <- writesMemory name
      pure $ (nameToIdent name,,written) <$> typ
    -- Several read-only bindings may refer to the same buffer, as when an
    -- entry point is passed the same array twice, but a writable binding must
    -- not overlap any other.
    moduleDecl (name, typ, written) = do
      ident <- mkGlobalIdent name
      slot <- assignBindSlot
      let (access, repr, buf_type)
            | written = (WGSL.ReadWrite, writableRepr typ, wgslBufferType typ Nothing)
            | wordStorage typ = (WGSL.Read, MemReadOnlyWords, WGSL.Array WGSL.I32 Nothing)
            | otherwise = (WGSL.Read, MemElems, wgslBufferType typ Nothing)
      unless written . modify $ \s -> s {ksReadOnlySlots = S.insert slot $ ksReadOnlySlots s}
      addMem ident $ MemInfo {memShared = False, memRepr = repr}
      addDecl $
        WGSL.GlobalVariable
          (WGSL.bindingAttribs 0 slot)
          WGSL.Storage
          (Just access)
          ident
          (Just buf_type)
          Nothing
    rename (name, _, _) = mkGlobalIdent name >>= addRename name

-- | Generate `override` declarations for kernel 'ConstUse's and
-- backend-provided values (like block size and lockstep width).
genConstAndBuiltinDecls :: KernelM ()
genConstAndBuiltinDecls = do
  kernel <- asks krKernel

  -- Start off with handling the block size parameters. Dynamic ones are set
  -- when launching the kernel.
  forM_ (zip [0 ..] $ ImpGPU.kernelBlockSize kernel) $ \(i, size) -> do
    n <- builtinBlockSize i
    case size of
      Left _ -> do
        addOverride n WGSL.I32 zeroInit
        addBlockDim i n True
      Right e -> do
        addConst n e
        addBlockDim i n False

  lsWidth <- builtinLockstepWidth
  addConst lsWidth $ ValueExp (IntValue (Int32Value 1))

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
        lo <- kernelConstLo name
        hi_exp <- case primExpType e of
          IntType Int64 -> do
            hi <- mkGlobalIdent (n <> "_hi")
            addConst lo $ mkLo e
            addConst hi $ mkHi e
            pure $ WGSL.ExpIdent hi
          _ -> do
            addConst lo e
            pure $ WGSL.ExpInt 0 WGSL.AbstractInt
        addInitStmt $ WGSL.Var n (Just wgslInt64) Nothing
        addInitStmt . assign n $ call "i64" [WGSL.ExpIdent lo, hi_exp]

  mapM_ mkConst consts
  where
    zeroInit = Just $ WGSL.ExpInt 0 WGSL.AbstractInt

nameToIdent :: VName -> WGSL.Ident
nameToIdent = zEncodeText . prettyText

textToIdent :: T.Text -> WGSL.Ident
textToIdent = zEncodeText
