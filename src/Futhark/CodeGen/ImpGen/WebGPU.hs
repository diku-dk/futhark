-- | Code generation for ImpCode with WebGPU.
module Futhark.CodeGen.ImpGen.WebGPU
  ( compileProg,
    Warnings,
  )
where

import Control.Monad (forM, forM_, liftM2, liftM3)
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS
import Control.Monad.Trans.State qualified as StateT
import Control.Monad.State.Class qualified as State
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
import Futhark.IR.GPUMem qualified as F
import Futhark.MonadFreshNames
import Futhark.Util (convFloat, zEncodeText, nubOrd)
import Futhark.Util.Pretty (docText)
import Language.Futhark.Warnings (Warnings)
import Language.WGSL qualified as WGSL

-- State carried during WebGPU translation.
data WebGPUS = WebGPUS
  { wsNameSrc :: VNameSource,
    -- | Accumulated code.
    wsCode :: T.Text,
    wsSizes :: M.Map Name SizeClass,
    wsMacroDefs :: [(Name, KernelConstExp)],
    -- | Interface of kernels already generated into wsCode.
    wsKernels :: [(WGSL.Ident, KernelInterface)],
    wsNextBindSlot :: Int
  }

-- The monad in which we perform the overall translation.
newtype WebGPUM a = WebGPUM (StateT.State WebGPUS a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      State.MonadState WebGPUS
    )

instance MonadFreshNames WebGPUM where
  getNameSource = State.gets wsNameSrc
  putNameSource src = State.modify $ \s -> s {wsNameSrc = src}

runWebGPUM :: WebGPUM a -> WebGPUS -> (a, WebGPUS)
runWebGPUM (WebGPUM m) = StateT.runState m

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
    ksSharedMem :: [(WGSL.Ident, Exp)],
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

-- | Whether the identifier is the name of a shared memory allocation.
-- TODO: Should probably store the allocation name in the state instead of
-- reconstructing the _size name here.
isShared :: WGSL.Ident -> KernelM Bool
isShared ident = any (\(sz, _) -> sz == ident <> "_size") <$> gets ksSharedMem

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
            sharedMemoryOverrides = sharedMemOverrides
          }
  State.modify $ \ws -> ws {wsKernels = wsKernels ws <> [(name, interface)]}
  pure (nameFromText name, map (,IntType Int32) sharedMemExps)
  where
    gen = do
      genConstAndBuiltinDecls
      genScalarDecls
      genMemoryDecls

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
          { wsNameSrc = blankNameSource,
            wsCode = mempty,
            wsSizes = mempty,
            wsMacroDefs = mempty,
            wsKernels = mempty,
            wsNextBindSlot = 0
          }

      ((consts', funs'), translation) =
        flip runWebGPUM initial_state $
          (,) <$> traverse onHostOp consts <*> traverse (traverse (traverse onHostOp)) funs

      prog' =
        Definitions types (Constants ps consts') (Functions funs')

      kernels = M.fromList $ map (first nameFromText) (wsKernels translation)
      -- Put scalar32 in front of the other integer types since they are all
      -- internally represented using i32.
      webgpu_prelude =
        mconcat
          [ "enable f16;\n",
            RTS.scalar,
            RTS.scalar32,
            RTS.scalar8,
            RTS.scalar16,
            RTS.scalar64
          ]
      constants = wsMacroDefs translation
      -- TODO: Compute functions using tuning params
      params = M.map (,S.empty) $ wsSizes translation
      failures = mempty
   in Program
        { webgpuProgram = wsCode translation,
          webgpuPrelude = webgpu_prelude,
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
wgslPrimType (FloatType Float64) = error "TODO: WGSL has no f64"
wgslPrimType Bool = WGSL.Bool
-- TODO: Make sure we do not ever codegen statements involving Unit variables
wgslPrimType Unit = WGSL.Float16 -- error "TODO: no unit in WGSL"

wgslBufferType :: (PrimType, Bool, Signedness) -> Maybe WGSL.Exp -> WGSL.Typ
wgslBufferType (Bool, _, _) = WGSL.Array $ WGSL.Atomic wgslInt8
wgslBufferType (IntType Int8, _, _) = WGSL.Array $ WGSL.Atomic wgslInt8
wgslBufferType (IntType Int16, _, _) = WGSL.Array $ WGSL.Atomic wgslInt16
wgslBufferType (IntType Int32, False, _) = WGSL.Array WGSL.Int32
wgslBufferType (IntType Int32, True, Signed) = WGSL.Array $ WGSL.Atomic WGSL.Int32
wgslBufferType (IntType Int32, True, Unsigned) = WGSL.Array $ WGSL.Atomic WGSL.UInt32
wgslBufferType (t, _, _) = WGSL.Array $ wgslPrimType t

genFunWrite ::
  WGSL.Ident ->
  VName ->
  Count Elements (TExp Int64) ->
  Exp ->
  KernelM WGSL.Stmt
genFunWrite fun mem i v = do
  mem' <- getIdent mem
  shared <- isShared mem'
  if shared
     then do
       let buf = pure $ WGSL.UnOpExp "&" (WGSL.VarExp mem')
       WGSL.Call fun <$> sequence [buf, indexExp i, genWGSLExp v]
     else do
       let fun' = fun <> "_wg"
       idxName <- newVName "wgpu_elem_idx"
       -- TODO: do the right thing here
       let buf = pure $ WGSL.UnOpExp "&" (WGSL.VarExp mem')
       WGSL.Call fun <$> sequence [buf, indexExp i, genWGSLExp v]

genFunRead ::
  WGSL.Ident ->
  VName ->
  VName ->
  Count Elements (TExp Int64) ->
  KernelM WGSL.Stmt
genFunRead fun tgt mem i = do
  mem' <- getIdent mem
  shared <- isShared mem'
  let fun' = if shared then fun <> "_wg" else fun
  let buf = pure $ WGSL.UnOpExp "&" (WGSL.VarExp mem')
  let call = WGSL.CallExp fun' <$> sequence [buf, indexExp i]
  WGSL.Assign <$> getIdent tgt <*> call

unsupported :: Code ImpGPU.KernelOp -> KernelM WGSL.Stmt
unsupported stmt = pure $ WGSL.Comment $ "Unsupported stmt: " <> prettyText stmt

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
    Just elemPrimType -> do
      let bufType = wgslBufferType elemPrimType (Just $ WGSL.VarExp sizeName)

      addOverride sizeName (WGSL.Prim WGSL.Int32) (Just $ WGSL.IntExp 0)
      addDecl $ WGSL.VarDecl [] WGSL.Workgroup moduleName bufType
      addRename name' moduleName
      pure $ WGSL.Comment $ "declare_shared: " <> name'
    Nothing ->
      pure $ WGSL.Comment $ "discard declare_shared: " <> name'
genWGSLStm s@(DeclareMem _ _) = unsupported s
genWGSLStm (DeclareScalar name _ typ) =
  pure $
    WGSL.DeclareVar (nameToIdent name) (WGSL.Prim $ wgslPrimType typ)
genWGSLStm s@(DeclareArray {}) = unsupported s
genWGSLStm s@(Allocate {}) = unsupported s
genWGSLStm s@(Free _ _) = unsupported s
genWGSLStm s@(Copy {}) = unsupported s
genWGSLStm (Write mem i Bool _ _ v) = genFunWrite "write_bool" mem i v
genWGSLStm (Write mem i (IntType Int8) _ _ v) = genFunWrite "write_i8" mem i v
genWGSLStm (Write mem i (IntType Int16) _ _ v) = genFunWrite "write_i16" mem i v
genWGSLStm (Write mem i _ _ _ v) =
  liftM3 WGSL.AssignIndex (getIdent mem) (indexExp i) (genWGSLExp v)
genWGSLStm (SetScalar name e) =
  liftM2 WGSL.Assign (getIdent name) (genWGSLExp e)
genWGSLStm (Read tgt mem i Bool _ _) = genFunRead "read_bool" tgt mem i
genWGSLStm (Read tgt mem i (IntType Int8) _ _) = genFunRead "read_i8" tgt mem i
genWGSLStm (Read tgt mem i (IntType Int16) _ _) = genFunRead "read_i16" tgt mem i
genWGSLStm (Read tgt mem i _ _ _) =
  let index = liftM2 WGSL.IndexExp (getIdent mem) (indexExp i)
   in liftM2 WGSL.Assign (getIdent tgt) index
genWGSLStm s@(SetMem {}) = unsupported s
genWGSLStm (Call [dest] f args) = do
  fun <- WGSL.CallExp . ("futrts_" <>) <$> getIdent f
  let getArg (ExpArg e) = genWGSLExp e
      getArg (MemArg n) = WGSL.VarExp <$> getIdent n
  argExps <- mapM getArg args
  WGSL.Assign <$> getIdent dest <*> pure (fun argExps)
genWGSLStm (Call {}) =
  pure $
    WGSL.Comment "TODO: Multi-dest calls not supported"
genWGSLStm (If cond cThen cElse) =
  liftM3
    WGSL.If
    (genWGSLExp $ untyped cond)
    (genWGSLStm cThen)
    (genWGSLStm cElse)
genWGSLStm s@(Assert {}) = unsupported s
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
genWGSLStm (Op (ImpGPU.Atomic _ (ImpGPU.AtomicAdd _ dest mem i e))) = do
  idx <- WGSL.IndexExp <$> getIdent mem <*> indexExp i
  val <- genWGSLExp e
  let call = WGSL.CallExp "atomicAdd" [WGSL.UnOpExp "&" idx, val]
  WGSL.Assign <$> getIdent dest <*> pure call
  --let buf = WGSL.UnOpExp "&" . WGSL.VarExp <$> getIdent mem
  --    call = WGSL.CallExp fun <$> sequence [buf, indexExp i]
  -- in WGSL.Assign <$> getIdent tgt <*> call
genWGSLStm s@(Op (ImpGPU.Atomic _ (ImpGPU.AtomicFAdd {}))) = unsupported s
genWGSLStm s@(Op (ImpGPU.Atomic _ _)) = unsupported s
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
genWGSLStm s@(Op (ImpGPU.ErrorSync _)) = unsupported s

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
wgslUnOp Not = WGSL.UnOpExp "!"
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

valueFloat :: FloatValue -> Double
valueFloat (Float16Value v) = convFloat v
valueFloat (Float32Value v) = convFloat v
valueFloat (Float64Value v) = v

genWGSLExp :: Exp -> KernelM WGSL.Exp
genWGSLExp (LeafExp name _) = WGSL.VarExp <$> getIdent name
genWGSLExp (ValueExp (IntValue v)) = pure $ intLiteral v
genWGSLExp (ValueExp (FloatValue v)) = pure $ WGSL.FloatExp (valueFloat v)
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
      addInitStmt $ WGSL.Assign name (wrapCopy $ WGSL.FieldExp bufferName name)

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

-- | Internally, memory buffers are untyped but WGSL requires us to annotate the
-- binding with a type. Search the kernel body for any reads and writes to the
-- given buffer and return all types it is accessed at.
-- The bool indicates an atomic type. The Signedness is only relevant for atomic
-- types as described for `atomicOpType`.
-- TODO: Should we worry about the Space in the Atomic?
findMemoryTypes :: VName -> KernelM [(PrimType, Bool, Signedness)]
findMemoryTypes name = S.elems . find <$> asks (ImpGPU.kernelBody . krKernel)
  where
    find (ImpGPU.Write n _ t _ _ _) | n == name = S.singleton (t, False, Signed)
    find (ImpGPU.Read _ n _ t _ _) | n == name = S.singleton (t, False, Signed)
    find (Op (ImpGPU.Atomic _ op)) | atomicOpArray op == name =
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
        Just (_, _, sgn) | canBeAtomic prim ->
          if all (\(_, _, s) -> s == sgn) types
             then pure $ Just (prim, True, sgn)
             else error "Atomic type used at multiple signednesses"
        Just _ -> error "Non i32 or u32 used atomically"
        Nothing -> pure $ Just (prim, False, Signed)
    _tooMany -> error "Buffer used at multiple types"
  where
    canBeAtomic (IntType Int64) = False
    canBeAtomic (IntType _) = True
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
  mapM_ moduleDecl memUses
  mapM_ rename memUses
  where
    withType name = do
      typ <- findSingleMemoryType name
      pure $ (nameToIdent name,) <$> typ
    moduleDecl (name, typ) = do
      ident <- mkGlobalIdent name
      slot <- assignBindSlot
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
