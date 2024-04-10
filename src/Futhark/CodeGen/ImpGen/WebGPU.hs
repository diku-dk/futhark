-- | Code generation for ImpCode with WebGPU.
module Futhark.CodeGen.ImpGen.WebGPU
  ( compileProg,
    Warnings,
  )
where

import Control.Monad (liftM2, liftM3)
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS hiding (get, modify, put)
import Control.Monad.Trans.State
import Data.Bifunctor (first, second)
import Data.Bits qualified as Bits
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Data.Text qualified as T
import Futhark.CodeGen.ImpCode.GPU qualified as ImpGPU
import Futhark.CodeGen.ImpCode.WebGPU
import Futhark.CodeGen.ImpGen.WGSL qualified as WGSL
import Futhark.CodeGen.ImpGen.GPU qualified as ImpGPU
import Futhark.CodeGen.RTS.WGSL qualified as RTS
import Futhark.IR.GPUMem qualified as F
import Futhark.MonadFreshNames
import Futhark.Util (convFloat, zEncodeText)
import Futhark.Util.Pretty (docText)
import Language.Futhark.Warnings (Warnings)
import Data.List (foldl')
import Data.Functor ((<&>))

-- State carried during WebGPU translation.
data WebGPUS = WebGPUS
  { -- | Accumulated code.
    wsCode :: T.Text,
    wsSizes :: M.Map Name SizeClass,
    wsMacroDefs :: [(Name, KernelConstExp)],
    -- | Interface of kernels already generated into wsCode.
    wsKernels :: [(WGSL.Ident, KernelInterface)],
    wsNextBindSlot :: Int
  }

-- The monad in which we perform the translation. The state will
-- probably need to be extended, and maybe we will add a Reader.
type WebGPUM = State WebGPUS

addSize :: Name -> SizeClass -> WebGPUM ()
addSize key sclass =
  modify $ \s -> s {wsSizes = M.insert key sclass $ wsSizes s}

addMacroDef :: Name -> KernelConstExp -> WebGPUM ()
addMacroDef key e =
  modify $ \s -> s {wsMacroDefs = (key, e) : wsMacroDefs s}

addCode :: T.Text -> WebGPUM ()
addCode code =
  modify $ \s -> s {wsCode = wsCode s <> code}

data KernelR = KernelR
  { -- | Kernel currently being translated.
    krKernel :: ImpGPU.Kernel,
    -- | Identifier replacement map. We have to rename some identifiers; when
    -- translating Imp Code and PrimExps this map is consulted to respect the
    -- renaming.
    krNameReplacements :: M.Map WGSL.Ident WGSL.Ident
  }

addRenames :: [(WGSL.Ident, WGSL.Ident)] -> KernelR -> KernelR
addRenames renames r = r { krNameReplacements = insert (krNameReplacements r) }
  where
    insert m' = foldl' (\m (k, v) -> M.insert k v m) m' renames

data KernelW = KernelW
  { kwOverrides :: [WGSL.Ident],
    kwScalars :: [WGSL.PrimType],
    kwBindSlots :: [Int]
  }

instance Semigroup KernelW where
  (KernelW ao as ab) <> (KernelW bo bs bb) =
    KernelW (ao <> bo) (as <> bs) (ab <> bb)

instance Monoid KernelW where
  mempty = KernelW [] [] []

type KernelM = RWST KernelR KernelW () WebGPUM

-- | Some names generated are unique in the scope of a single kernel but are
-- translated to module-scope identifiers in WGSL. This modifies an identifier
-- to be unique in that scope.
mkGlobalIdent :: WGSL.Ident -> KernelM WGSL.Ident
mkGlobalIdent ident = do
  kernelName <- asks (textToIdent . nameToText . ImpGPU.kernelName . krKernel)
  pure $ kernelName <> "_" <> ident

-- | Produces an identifier for the given name, respecting the name replacements
-- map.
getIdent :: (F.Pretty a) => a -> KernelM WGSL.Ident
getIdent name = asks (M.findWithDefault t t . krNameReplacements)
  where t = zEncodeText $ prettyText name

-- | Get a new, unused binding index and add it to the list of bind slots used
-- by the current kernel.
assignBindSlot :: KernelM Int
assignBindSlot = do
  wState <- lift get
  let slot = wsNextBindSlot wState
  tell (KernelW [] [] [slot])
  lift $ put (wState {wsNextBindSlot = slot + 1})
  pure slot

-- | Write an override declaration to add to the current kernel's interface.
addOverride :: WGSL.Ident -> KernelM ()
addOverride ident = tell (KernelW [ident] [] [])

addScalar :: WGSL.PrimType -> KernelM ()
addScalar typ = tell (KernelW [] [typ] [])

finishKernel :: KernelR -> KernelW -> WebGPUM KernelName
finishKernel (KernelR kernel _) kw = do
  s <- get
  let (offsets, _align, size) = case WGSL.structLayout (kwScalars kw) of
                                  Just t -> t
                                  Nothing -> error "invalid scalars struct"
  let name = nameToText $ ImpGPU.kernelName kernel
  let interface = KernelInterface {
    safety = SafetyNone, -- TODO
    scalarsOffsets = offsets,
    scalarsSize = size,
    scalarsBindSlot = head (kwBindSlots kw),
    memBindSlots = tail (kwBindSlots kw),
    overrideNames = kwOverrides kw
  }
  put $ s {wsKernels = wsKernels s <> [(name, interface)]}
  pure (nameFromText name)

entryParams :: [WGSL.Param]
entryParams =
  [ WGSL.Param "workgroup_id" (WGSL.Prim (WGSL.Vec3 WGSL.UInt32))
      [WGSL.Attrib "builtin" [WGSL.VarExp "workgroup_id"]],
    WGSL.Param "local_id" (WGSL.Prim (WGSL.Vec3 WGSL.UInt32))
      [WGSL.Attrib "builtin" [WGSL.VarExp "local_invocation_id"]]
  ]

builtinLockstepWidth, builtinBlockSize :: KernelM WGSL.Ident
builtinLockstepWidth = mkGlobalIdent "lockstep_width"
builtinBlockSize = mkGlobalIdent "block_size"

-- Main function for translating an ImpGPU kernel to a WebGPU kernel.
genKernel :: KernelM ()
genKernel = do
  kernel <- asks krKernel
  let name = textToIdent $ nameToText (ImpGPU.kernelName kernel)

  (overrideDecls, overrideMacroDefs, overrideInits) <- genConstAndBuiltinDecls
  gen $ docText (WGSL.prettyDecls overrideDecls <> "\n\n")
  lift $ mapM_ (uncurry addMacroDef) overrideMacroDefs

  (scalarDecls, scalarCopies) <- genScalarDecls
  gen $ docText (WGSL.prettyDecls scalarDecls <> "\n\n")

  (memDecls, memRenames) <- genMemoryDecls
  gen $ docText (WGSL.prettyDecls memDecls <> "\n\n")

  wgslBody <- local (addRenames memRenames) $
    genWGSLStm (ImpGPU.kernelBody kernel)
  let body = WGSL.stmts [overrideInits, scalarCopies, wgslBody]

  blockSize <- builtinBlockSize
  let attribs = [WGSL.Attrib "compute" [],
                 WGSL.Attrib "workgroup_size" [WGSL.VarExp blockSize]]

  let wgslFun = WGSL.Function
                  { WGSL.funName = name,
                    WGSL.funAttribs = attribs,
                    WGSL.funParams = entryParams,
                    WGSL.funBody = body
                  }
  gen $ prettyText wgslFun
  gen "\n"

  pure ()
    where
      gen = lift . addCode


onKernel :: ImpGPU.Kernel -> WebGPUM HostOp
onKernel kernel = do
  let r = KernelR kernel M.empty
  ((), (), w) <- runRWST genKernel r ()
  name <- finishKernel r w
  let numBlocks = ImpGPU.kernelNumBlocks kernel
  let blockDim = ImpGPU.kernelBlockSize kernel
  let scalarArgs = [ValueKArg (LeafExp n t) t | ImpGPU.ScalarUse n t <- ImpGPU.kernelUses kernel]
  let memArgs = [MemKArg n | ImpGPU.MemoryUse n <- ImpGPU.kernelUses kernel]
  let args = scalarArgs ++ memArgs
  -- TODO: return something sensible.
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

      initial_state = WebGPUS {
        wsCode = mempty,
        wsSizes = mempty,
        wsMacroDefs = mempty,
        wsKernels = mempty,
        wsNextBindSlot = 0
      }

      ((consts', funs'), translation) =
        flip runState initial_state $
          (,) <$> traverse onHostOp consts <*> traverse (traverse (traverse onHostOp)) funs

      prog' =
        Definitions types (Constants ps consts') (Functions funs')

      kernels = M.fromList $ map (first nameFromText) (wsKernels translation)
      webgpu_prelude = RTS.scalar <> RTS.scalar8 <> RTS.scalar16 <> RTS.scalar64
      constants = wsMacroDefs translation
      -- TODO: Compute functions using tuning params
      params = M.map (, S.empty) $ wsSizes translation
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

wgslBufferType :: PrimType -> WGSL.Typ
wgslBufferType Bool = WGSL.Array $ WGSL.Atomic wgslInt8
wgslBufferType (IntType Int8) = WGSL.Array $ WGSL.Atomic wgslInt8
wgslBufferType (IntType Int16) = WGSL.Array $ WGSL.Atomic wgslInt16
wgslBufferType t = WGSL.Array $ wgslPrimType t

genFunWrite :: WGSL.Ident -> VName -> Count Elements (TExp Int64) -> Exp
            -> KernelM WGSL.Stmt
genFunWrite fun mem i v =
  let buf = WGSL.UnOpExp "&" . WGSL.VarExp <$> getIdent mem
   in WGSL.Call fun <$> sequence [buf, indexExp i, genWGSLExp v]

genFunRead :: WGSL.Ident -> VName -> VName -> Count Elements (TExp Int64)
            -> KernelM WGSL.Stmt
genFunRead fun tgt mem i =
  let buf = WGSL.UnOpExp "&" . WGSL.VarExp <$> getIdent mem
      call = WGSL.CallExp fun <$> sequence [buf, indexExp i]
   in WGSL.Assign <$> getIdent tgt <*> call

genWGSLStm :: Code ImpGPU.KernelOp -> KernelM WGSL.Stmt
genWGSLStm Skip = pure WGSL.Skip
genWGSLStm (s1 :>>: s2) = liftM2 WGSL.Seq (genWGSLStm s1) (genWGSLStm s2)
genWGSLStm (For iName bound body) = do
  boundExp <- genWGSLExp bound
  bodyStm <- genWGSLStm body
  pure $ WGSL.For i zero (lt (WGSL.VarExp i) boundExp)
    (WGSL.Assign i $ add (WGSL.VarExp i) (WGSL.IntExp 1)) bodyStm
  where
    i = nameToIdent iName
    boundIntType = case primExpType bound of
                     IntType t -> t
                     _ -> error "non-integer Exp for loop bound"
    add = wgslBinOp $ Add boundIntType OverflowWrap
    lt = wgslCmpOp $ CmpUlt boundIntType
    zero = case boundIntType of
             Int64 -> WGSL.VarExp "zero_i64"
             _ -> WGSL.IntExp 0
genWGSLStm (While cond body) = liftM2
  WGSL.While (genWGSLExp $ untyped cond) (genWGSLStm body)
genWGSLStm (DeclareMem _ _) = pure $ WGSL.Comment "TODO: Unimplemented statement"
genWGSLStm (DeclareScalar name _ typ) = pure $
  WGSL.DeclareVar (nameToIdent name) (WGSL.Prim $ wgslPrimType typ)
genWGSLStm (DeclareArray {}) = pure $ WGSL.Comment "TODO: Unimplemented statement"
genWGSLStm (Allocate {}) = pure $ WGSL.Comment "TODO: Unimplemented statement"
genWGSLStm (Free _ _) = pure $ WGSL.Comment "TODO: Unimplemented statement"
genWGSLStm (Copy {}) = pure $ WGSL.Comment "TODO: Unimplemented statement"
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
genWGSLStm (SetMem {}) = pure $ WGSL.Comment "TODO: Unimplemented statement"
genWGSLStm (Call [dest] f args) = do
  fun <- WGSL.CallExp . ("futrts_" <>) <$> getIdent f
  let getArg (ExpArg e) = genWGSLExp e
      getArg (MemArg n) = WGSL.VarExp <$> getIdent n
  argExps <- mapM getArg args
  WGSL.Assign <$> getIdent dest <*> pure (fun argExps)
genWGSLStm (Call {}) = pure $
  WGSL.Comment "TODO: Multi-dest calls not supported"
genWGSLStm (If cond cThen cElse) = liftM3
  WGSL.If (genWGSLExp $ untyped cond) (genWGSLStm cThen) (genWGSLStm cElse)
genWGSLStm (Assert {}) = pure $ WGSL.Comment "TODO: Unimplemented statement"
genWGSLStm (Comment c s) = WGSL.Seq (WGSL.Comment c) <$> genWGSLStm s
genWGSLStm (DebugPrint _ _) = pure WGSL.Skip
genWGSLStm (TracePrint _) = pure WGSL.Skip
genWGSLStm (Op (ImpGPU.GetBlockId dest i)) = do
  destId <- getIdent dest
  pure $ WGSL.Assign destId $
    WGSL.to_i32 (WGSL.IndexExp "workgroup_id" (WGSL.IntExp i))
genWGSLStm (Op (ImpGPU.GetLocalId dest i)) = do
  destId <- getIdent dest
  pure $ WGSL.Assign destId $
    WGSL.to_i32 (WGSL.IndexExp "local_id" (WGSL.IntExp i))
genWGSLStm (Op (ImpGPU.GetLocalSize dest _)) = do
  destId <- getIdent dest
  WGSL.Assign destId . WGSL.VarExp <$> builtinBlockSize
genWGSLStm (Op (ImpGPU.GetLockstepWidth dest)) = do
  destId <- getIdent dest
  WGSL.Assign destId . WGSL.VarExp <$> builtinLockstepWidth
genWGSLStm (Op (ImpGPU.Atomic _ _)) = pure $
  WGSL.Comment "TODO: Unimplemented statement"
genWGSLStm (Op (ImpGPU.Barrier _)) = pure $
  WGSL.Comment "TODO: Unimplemented statement"
genWGSLStm (Op (ImpGPU.MemFence _)) = pure $
  WGSL.Comment "TODO: Unimplemented statement"
genWGSLStm (Op (ImpGPU.SharedAlloc _ _)) = pure $
  WGSL.Comment "TODO: Unimplemented statement"
genWGSLStm (Op (ImpGPU.ErrorSync _)) = pure $
  WGSL.Comment "TODO: Unimplemented statement"

call1 :: WGSL.Ident -> WGSL.Exp -> WGSL.Exp
call1 f a = WGSL.CallExp f [a]

call2 :: WGSL.Ident -> WGSL.Exp -> WGSL.Exp -> WGSL.Exp
call2 f a b = WGSL.CallExp f [a, b]

wgslBinOp :: BinOp -> WGSL.Exp -> WGSL.Exp -> WGSL.Exp
wgslBinOp (Add Int8 _) = call2 "add_i8"
wgslBinOp (Add Int16 _) = call2 "add_i16"
wgslBinOp (Add Int32 _) = WGSL.BinOpExp "+"
wgslBinOp (Add Int64 _) = call2 "add_i64"
wgslBinOp (FAdd _) = WGSL.BinOpExp "+"
wgslBinOp (Sub Int8 _) = call2 "sub_i8"
wgslBinOp (Sub Int16 _) = call2 "sub_i16"
wgslBinOp (Sub Int32 _) = WGSL.BinOpExp "-"
wgslBinOp (Sub Int64 _) = call2 "sub_i64"
wgslBinOp (FSub _) = WGSL.BinOpExp "-"
wgslBinOp (Mul Int8 _) = call2 "mul_i8"
wgslBinOp (Mul Int16 _) = call2 "mul_i16"
wgslBinOp (Mul Int32 _) = WGSL.BinOpExp "*"
wgslBinOp (Mul Int64 _) = call2 "mul_i64"
wgslBinOp (FMul _) = WGSL.BinOpExp "*"
-- Division is always safe in WGSL, so we can ignore the Safety parameter.
wgslBinOp (UDiv Int8 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (UDiv Int16 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (UDiv Int32 _) = call2 "udiv_i32"
wgslBinOp (UDiv Int64 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (UDivUp Int8 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (UDivUp Int16 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (UDivUp Int32 _) = call2 "udiv_up_i32"
wgslBinOp (UDivUp Int64 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (SDiv Int8 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (SDiv Int16 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (SDiv Int32 _) = call2 "sdiv_i32"
wgslBinOp (SDiv Int64 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (SDivUp Int8 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (SDivUp Int16 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (SDivUp Int32 _) = call2 "sdiv_up_i32"
wgslBinOp (SDivUp Int64 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (FDiv _) = WGSL.BinOpExp "/"
wgslBinOp (FMod _) = WGSL.BinOpExp "%"
wgslBinOp (UMod Int8 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (UMod Int16 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (UMod Int32 _) = call2 "umod_i32"
wgslBinOp (UMod Int64 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (SMod Int8 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (SMod Int16 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (SMod Int32 _) = call2 "smod_i32"
wgslBinOp (SMod Int64 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (SQuot Int8 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (SQuot Int16 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (SQuot Int32 _) = WGSL.BinOpExp "/"
wgslBinOp (SQuot Int64 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (SRem Int8 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (SRem Int16 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (SRem Int32 _) = WGSL.BinOpExp "%"
wgslBinOp (SRem Int64 _) = WGSL.BinOpExp "<TODO: unimplemented binop>"
wgslBinOp (SMin Int64) = call2 "smin_i64"
wgslBinOp (SMin _) = call2 "min"
wgslBinOp (UMin Int8) = call2 "umin_i8"
wgslBinOp (UMin Int16) = call2 "umin_i16"
wgslBinOp (UMin Int32) = call2 "umin_i32"
wgslBinOp (UMin Int64) = call2 "umin_i64"
wgslBinOp (FMin _) = call2 "min"
wgslBinOp (SMax Int64) = call2 "smax_i64"
wgslBinOp (SMax _) = call2 "max"
wgslBinOp (UMax Int8) = call2 "umax_i8"
wgslBinOp (UMax Int16) = call2 "umax_i16"
wgslBinOp (UMax Int32) = call2 "umax_i32"
wgslBinOp (UMax Int64) = call2 "umax_i64"
wgslBinOp (FMax _) = call2 "max"
wgslBinOp (Shl Int8) = call2 "shl_i8"
wgslBinOp (Shl Int16) = call2 "shl_i16"
wgslBinOp (Shl Int32) = call2 "shl_i32"
wgslBinOp (Shl Int64) = call2 "shl_i64"
wgslBinOp (LShr Int8) = call2 "lshr_i8"
wgslBinOp (LShr Int16) = call2 "lshr_i16"
wgslBinOp (LShr Int32) = call2 "lshr_i32"
wgslBinOp (LShr Int64) = call2 "lshr_i64"
wgslBinOp (AShr Int8) = call2 "ashr_i8"
wgslBinOp (AShr Int16) = call2 "ashr_i16"
wgslBinOp (AShr Int32) = call2 "ashr_i32"
wgslBinOp (AShr Int64) = call2 "ashr_i64"
wgslBinOp (And _) = WGSL.BinOpExp "&"
wgslBinOp (Or _) = WGSL.BinOpExp "|"
wgslBinOp (Xor _) = WGSL.BinOpExp "^"
wgslBinOp (Pow Int8) = call2 "pow_i8"
wgslBinOp (Pow Int16) = call2 "pow_i16"
wgslBinOp (Pow Int32) = call2 "pow_i32"
wgslBinOp (Pow Int64) = call2 "pow_i64"
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
    fun (IToB Int64) = "i64_to_bool"
    fun (IToB _) = "bool"
    fun (BToI Int8) = "bool_to_i8"
    fun (BToI Int16) = "bool_to_i16"
    fun (BToI Int32) = "i32"
    fun (BToI Int64) = "bool_to_i64"
    fun _ = "TODO_not_implemented"

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
genWGSLExp _ = pure $ WGSL.StringExp "<not implemented>"

indexExp :: Count Elements (TExp Int64) -> KernelM WGSL.Exp
-- We support 64-bit arithmetic, but since WGSL does not have support for it,
-- we cannot use a 64-bit value as an index, so we have to truncate it to 32
-- bits.
indexExp = genWGSLExp . ConvOpExp (ZExt Int64 Int32) . untyped . unCount

-- | Generate a struct declaration and corresponding uniform binding declaration
-- for all the scalar 'KernelUse's. Also generate a block of statements that
-- copies the struct fields into local variables so the kernel body can access
-- them unmodified.
genScalarDecls :: KernelM ([WGSL.Declaration], WGSL.Stmt)
genScalarDecls = do
  structName <- mkGlobalIdent "Scalars"
  bufferName <- mkGlobalIdent "scalars"
  uses <- asks (ImpGPU.kernelUses . krKernel)

  -- TODO: This probably needs to use the wgslBufferType to ensure
  -- host-shareable scalar structs. Should then convert in the copy stmt below.
  let scalars = [(nameToIdent name, WGSL.Prim $ wgslPrimType typ)
                  | ImpGPU.ScalarUse name typ <- uses]
  let structDecl = WGSL.StructDecl $
        WGSL.Struct structName (map (uncurry WGSL.Field) scalars)

  mapM_ addScalar [t | (_, WGSL.Prim t) <- scalars]

  slot <- assignBindSlot
  let bufferAttribs = WGSL.bindingAttribs 0 slot
  let bufferDecl = WGSL.VarDecl
        bufferAttribs WGSL.Uniform bufferName (WGSL.Named structName)

  let copy (name, typ) = [WGSL.DeclareVar name typ,
                          WGSL.Assign name (WGSL.FieldExp bufferName name)]
  let copies = WGSL.stmts $ concatMap copy scalars

  pure ([structDecl, bufferDecl], copies)

-- | Internally, memory buffers are untyped but WGSL requires us to annotate the
-- binding with a type. Search the kernel body for any reads and writes to the
-- given buffer and return all types it is accessed at.
findMemoryTypes :: VName -> KernelM [ImpGPU.PrimType]
findMemoryTypes name = S.elems . find <$> asks (ImpGPU.kernelBody . krKernel)
  where
    find (ImpGPU.Write n _ t _ _ _) | n == name = S.singleton t
    find (ImpGPU.Read _ n _ t _ _) | n == name = S.singleton t
    find (s1 :>>: s2) = find s1 <> find s2
    find (For _ _ body) = find body
    find (While _ body) = find body
    find (If _ s1 s2) = find s1 <> find s2
    find _ = S.empty

-- | Generate binding declarations for memory buffers used by kernel. Produces
-- additional name replacements because it makes the binding names unique.
--
-- We can't use the same trick as for e.g. scalars where we make a local copy to
-- avoid the name replacements because WGSL does not allow function-local
-- variables in the 'storage' address space.
genMemoryDecls :: KernelM ([WGSL.Declaration], [(WGSL.Ident, WGSL.Ident)])
genMemoryDecls = do
  uses <- asks (ImpGPU.kernelUses . krKernel)
  memUses <- catMaybes <$> sequence [withType n | ImpGPU.MemoryUse n <- uses]
  decls <- mapM moduleDecl memUses
  renames <- mapM rename memUses
  pure (decls, renames)
  where
    withType name = do
      types <- findMemoryTypes name
      case types of
        [] -> pure Nothing -- No declarations for unused buffers
        [t] -> pure $ Just (nameToIdent name, t)
        _more ->
          error "Using buffer at multiple types not supported in WebGPU backend"
    moduleDecl (name, typ) = do
      ident <- mkGlobalIdent name
      slot <- assignBindSlot
      pure $ WGSL.VarDecl (WGSL.bindingAttribs 0 slot)
        (WGSL.Storage WGSL.ReadWrite) ident (wgslBufferType typ)
    rename (name, _) = (name, ) <$> mkGlobalIdent name

-- | Generate `override` declarations for kernel 'ConstUse's and
-- backend-provided values (like block size and lockstep width). The returned
-- (name, expression) pairs are the corresponding expressions to be evaluated
-- on the host and passed to the kernel.
-- Some ConstUses can require additional code inserted at the beginning of the
-- kernel before they can be used, these are contained in the returned
-- statement.
genConstAndBuiltinDecls :: KernelM
  ([WGSL.Declaration], [(Name, KernelConstExp)], WGSL.Stmt)
genConstAndBuiltinDecls = do
  kernel <- asks krKernel

  let blockDimExp = case ImpGPU.kernelBlockSize kernel of
                      [Right e] -> e
                      _ -> error "TODO: no non-const or >1dim block sizes yet"
  builtins <- sequence
                [builtinLockstepWidth <&> (,ValueExp (IntValue (Int32Value 1))),
                 builtinBlockSize <&> (,blockDimExp)]
  let builtinDecls =
        [WGSL.OverrideDecl n (WGSL.Prim WGSL.Int32) (Just $ WGSL.IntExp 0)
          | (n, _) <- builtins]
  let builtinMap =
        [(nameFromText n, e) | (n, e) <- builtins]

  let consts = [(n, e) | ImpGPU.ConstUse n e <- ImpGPU.kernelUses kernel]
  moduleNames <- mapM (mkGlobalIdent . (<> "_x") . nameToIdent . fst) consts
  let constMap = [(nameFromText i, e) | ((_, e), i) <- zip consts moduleNames]
  let constDecls = [WGSL.OverrideDecl i (WGSL.Prim WGSL.Int32)
                     (Just $ WGSL.IntExp 0) | i <- moduleNames]
  let constInits =
        [WGSL.Seq (WGSL.DeclareVar (nameToIdent n) (WGSL.Prim wgslInt64))
          (WGSL.Assign (nameToIdent n) (WGSL.CallExp "i64" [WGSL.VarExp i,
                                                            WGSL.IntExp 0]))
          | ((n, _), i) <- zip consts moduleNames]

  let decls = builtinDecls ++ constDecls
  let fullMap = builtinMap ++ constMap
  sequence_ [addOverride n | WGSL.OverrideDecl n _ _ <- decls]
  pure (decls, fullMap, WGSL.stmts constInits)

nameToIdent :: VName -> WGSL.Ident
nameToIdent = zEncodeText . prettyText

textToIdent :: T.Text -> WGSL.Ident
textToIdent = zEncodeText
