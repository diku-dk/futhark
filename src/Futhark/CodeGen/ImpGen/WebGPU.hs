-- | Code generation for ImpCode with WebGPU.
module Futhark.CodeGen.ImpGen.WebGPU
  ( compileProg,
    Warnings,
  )
where

import Control.Monad.State
import Data.Bifunctor (second)
import Data.Map qualified as M
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

-- State carried during WebGPU translation.
data WebGPUS = WebGPUS
  { -- | Accumulated code.
    wsCode :: T.Text,
    wsSizes :: M.Map Name SizeClass
  }

-- The monad in which we perform the translation. The state will
-- probably need to be extended, and maybe we will add a Reader.
type WebGPUM = State WebGPUS

addSize :: Name -> SizeClass -> WebGPUM ()
addSize key sclass =
  modify $ \s -> s {wsSizes = M.insert key sclass $ wsSizes s}

addCode :: T.Text -> WebGPUM ()
addCode code =
  modify $ \s -> s {wsCode = wsCode s <> code}

entryParams :: [WGSL.Param]
entryParams =
  [ WGSL.Param "workgroup_id" (WGSL.Prim (WGSL.Vec3 WGSL.UInt32))
      [WGSL.Attrib "builtin" [WGSL.VarExp "workgroup_id"]],
    WGSL.Param "local_id" (WGSL.Prim (WGSL.Vec3 WGSL.UInt32))
      [WGSL.Attrib "builtin" [WGSL.VarExp "local_invocation_id"]]
  ]

builtinLockstepWidth, builtinBlockSize :: WGSL.Ident
builtinLockstepWidth = "_lockstep_width"
builtinBlockSize = "_block_size"

-- Main function for translating an ImpGPU kernel to a WebGPU kernel.
onKernel :: ImpGPU.Kernel -> WebGPUM HostOp
onKernel kernel = do
  addCode $ "Input for " <> name <> "\n"
  addCode $ prettyText kernel <> "\n\n"
  addCode $ "Code for " <> name <> ":\n"
  addCode "== SHADER START ==\n"

  -- TODO: Temporary for testing, this should ultimately appear in the shader
  -- through `webgpuPrelude`
  addCode RTS.arith64

  let overrideDecls = genConstAndBuiltinDecls kernel
  addCode $ docText (WGSL.prettyDecls overrideDecls <> "\n\n")

  let (scalarDecls, copies) = genScalarCopies kernel
  addCode $ docText (WGSL.prettyDecls scalarDecls <> "\n\n")

  let memDecls = genMemoryDecls kernel
  addCode $ docText (WGSL.prettyDecls memDecls <> "\n\n")

  let wgslBody = WGSL.Seq copies $ genWGSLStm (ImpGPU.kernelBody kernel)
  let attribs = [WGSL.Attrib "compute" [],
                 WGSL.Attrib "workgroup_size" [WGSL.VarExp builtinBlockSize]]
  let wgslFun = WGSL.Function
                  { WGSL.funName = name,
                    WGSL.funAttribs = attribs,
                    WGSL.funParams = entryParams,
                    WGSL.funBody = wgslBody
                  }
  addCode $ prettyText wgslFun
  addCode "\n"

  addCode "== SHADER END ==\n"

  -- TODO: return something sensible.
  pure $ LaunchKernel SafetyNone (ImpGPU.kernelName kernel) 0 [] [] []
    where name = textToIdent $ nameToText (ImpGPU.kernelName kernel)

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

      initial_state = WebGPUS {wsCode = mempty, wsSizes = mempty}

      ((consts', funs'), translation) =
        flip runState initial_state $
          (,) <$> traverse onHostOp consts <*> traverse (traverse (traverse onHostOp)) funs

      prog' =
        Definitions types (Constants ps consts') (Functions funs')

      webgpu_prelude = RTS.arith64
      constants = mempty
      kernels = mempty
      params = mempty
      failures = mempty
   in Program
        { webgpuProgram = wsCode translation,
          webgpuPrelude = webgpu_prelude,
          webgpuMacroDefs = constants,
          webgpuKernelNames = kernels,
          webgpuParams = params,
          webgpuFailures = failures,
          hostDefinitions = prog'
        }

-- | Compile the program to ImpCode with WebGPU kernels.
compileProg :: (MonadFreshNames m) => F.Prog F.GPUMem -> m (Warnings, Program)
compileProg prog = second kernelsToWebGPU <$> ImpGPU.compileProgOpenCL prog

primWGSLType :: PrimType -> WGSL.PrimType
primWGSLType (IntType Int32) = WGSL.Int32
primWGSLType (IntType Int64) = WGSL.Vec2 WGSL.Int32
primWGSLType (FloatType Float16) = WGSL.Float16
primWGSLType (FloatType Float32) = WGSL.Float32
primWGSLType (FloatType Float64) = error "TODO: WGSL has no f64"
primWGSLType Bool = WGSL.Bool
-- TODO: Deal with smaller integers
primWGSLType (IntType Int8) = WGSL.Int32
primWGSLType (IntType Int16) = WGSL.Int32
-- TODO: Make sure we do not ever codegen statements involving Unit variables
primWGSLType Unit = WGSL.Float16 -- error "TODO: no unit in WGSL"

genWGSLStm :: Code ImpGPU.KernelOp -> WGSL.Stmt
genWGSLStm Skip = WGSL.Skip
genWGSLStm (s1 :>>: s2) = WGSL.Seq (genWGSLStm s1) (genWGSLStm s2)
genWGSLStm (For iName bound body) =
  WGSL.For i zero (lt (WGSL.VarExp i) (genWGSLExp bound))
    (WGSL.Assign i $ add (WGSL.VarExp i) (WGSL.IntExp 1))
    (genWGSLStm body)
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
genWGSLStm (While cond body) =
  WGSL.While (genWGSLExp $ untyped cond) (genWGSLStm body)
genWGSLStm (DeclareScalar name _ typ) =
  WGSL.DeclareVar (nameToIdent name) (WGSL.Prim $ primWGSLType typ)
genWGSLStm (If cond cThen cElse) =
  WGSL.If (genWGSLExp $ untyped cond) (genWGSLStm cThen) (genWGSLStm cElse)
genWGSLStm (Write mem i _ _ _ v) =
  WGSL.AssignIndex (nameToIdent mem) (indexExp i) (genWGSLExp v)
genWGSLStm (SetScalar name e) = WGSL.Assign (nameToIdent name) (genWGSLExp e)
genWGSLStm (Read tgt mem i _ _ _) =
  WGSL.Assign (nameToIdent tgt) (WGSL.IndexExp (nameToIdent mem) (indexExp i))
genWGSLStm (Op (ImpGPU.GetBlockId dest i)) =
  WGSL.Assign (nameToIdent dest) $
    WGSL.to_i32 (WGSL.IndexExp "workgroup_id" (WGSL.IntExp i))
genWGSLStm (Op (ImpGPU.GetLocalId dest i)) =
  WGSL.Assign (nameToIdent dest) $
    WGSL.to_i32 (WGSL.IndexExp "local_id" (WGSL.IntExp i))
genWGSLStm (Op (ImpGPU.GetLocalSize dest _)) =
  WGSL.Assign (nameToIdent dest) (WGSL.VarExp builtinBlockSize)
genWGSLStm (Op (ImpGPU.GetLockstepWidth dest)) =
  WGSL.Assign (nameToIdent dest) (WGSL.VarExp builtinLockstepWidth)
genWGSLStm _ = WGSL.Comment "TODO: Unimplemented statement"

wgslBinOp :: BinOp -> WGSL.Exp -> WGSL.Exp -> WGSL.Exp
wgslBinOp (Add Int64 _) = \a b -> WGSL.CallExp "add_i64" [a, b]
wgslBinOp (Add _ _) = WGSL.BinOpExp "+"
wgslBinOp (FAdd _) = WGSL.BinOpExp "+"
wgslBinOp (Sub Int64 _) = \a b -> WGSL.CallExp "sub_i64" [a, b]
wgslBinOp (Sub _ _) = WGSL.BinOpExp "-"
wgslBinOp (FSub _) = WGSL.BinOpExp "-"
wgslBinOp (Mul Int64 _) = \a b -> WGSL.CallExp "mul_i64" [a, b]
wgslBinOp (Mul _ _) = WGSL.BinOpExp "*"
wgslBinOp (FMul _) = WGSL.BinOpExp "*"
wgslBinOp _ = WGSL.BinOpExp "<TODO: unimplemented binop>"

wgslCmpOp :: CmpOp -> WGSL.Exp -> WGSL.Exp -> WGSL.Exp
wgslCmpOp (CmpEq _) = WGSL.BinOpExp "=="
wgslCmpOp (CmpUlt _) = WGSL.BinOpExp "<"
wgslCmpOp (CmpUle _) = WGSL.BinOpExp "<="
wgslCmpOp (CmpSlt Int64) = \a b -> WGSL.CallExp "slt_i64" [a, b]
wgslCmpOp (CmpSlt _) = WGSL.BinOpExp "<"
wgslCmpOp (CmpSle _) = WGSL.BinOpExp "<="
wgslCmpOp (FCmpLt _) = WGSL.BinOpExp "<"
wgslCmpOp (FCmpLe _) = WGSL.BinOpExp "<="
-- TODO: This does not actually work for bools.
wgslCmpOp CmpLlt = WGSL.BinOpExp "<" 
wgslCmpOp CmpLle = WGSL.BinOpExp "=="

wgslConvOp :: ConvOp -> WGSL.Exp -> WGSL.Exp
wgslConvOp op a = WGSL.CallExp (fun op) [a]
  where
    fun (ZExt Int32 Int64) = "zext_i32_i64"
    fun (SExt Int32 Int64) = "sext_i32_i64"
    fun (ZExt Int64 Int32) = "trunc_i64_i32"
    fun (SExt Int64 Int32) = "trunc_i64_i32"
    fun _ = "TODO_not_implemented"

valueFloat :: FloatValue -> Double
valueFloat (Float16Value v) = convFloat v
valueFloat (Float32Value v) = convFloat v
valueFloat (Float64Value v) = v

genWGSLExp :: Exp -> WGSL.Exp
genWGSLExp (LeafExp name _) = WGSL.VarExp $ nameToIdent name
genWGSLExp (ValueExp (IntValue v)) = WGSL.IntExp (valueIntegral v)
genWGSLExp (ValueExp (FloatValue v)) = WGSL.FloatExp (valueFloat v)
genWGSLExp (ValueExp (BoolValue v)) = WGSL.BoolExp v
genWGSLExp (ValueExp UnitValue) =
  error "should not attempt to generate unit expressions"
genWGSLExp (BinOpExp op e1 e2) =
  wgslBinOp op (genWGSLExp e1) (genWGSLExp e2)
genWGSLExp (CmpOpExp op e1 e2) =
  wgslCmpOp op (genWGSLExp e1) (genWGSLExp e2)
genWGSLExp (ConvOpExp op e) = wgslConvOp op (genWGSLExp e)
genWGSLExp _ = WGSL.StringExp "<not implemented>"

indexExp :: Count Elements (TExp Int64) -> WGSL.Exp
-- We support 64-bit arithmetic, but since WGSL does not have support for it,
-- we cannot use a 64-bit value as an index, so we have to truncate it to 32
-- bits.
indexExp = genWGSLExp . ConvOpExp (ZExt Int64 Int32) . untyped . unCount

-- | Generate a struct declaration and corresponding uniform binding declaration
-- for all the scalar 'KernelUse's. Also generate a block of statements that
-- copies the struct fields into local variables so the kernel body can access
-- them unmodified.
genScalarCopies :: ImpGPU.Kernel -> ([WGSL.Declaration], WGSL.Stmt)
genScalarCopies kernel = ([structDecl, bufferDecl], copies)
  where
    structName = textToIdent $
      "Scalars_" <> nameToText (ImpGPU.kernelName kernel)
    bufferName = textToIdent $
      "scalars_" <> nameToText (ImpGPU.kernelName kernel)
    scalars = [(nameToIdent name, WGSL.Prim (primWGSLType typ))
                | ImpGPU.ScalarUse name typ <- ImpGPU.kernelUses kernel]
    structDecl = WGSL.StructDecl $
      WGSL.Struct structName (map (uncurry WGSL.Field) scalars)
    bufferAttribs = WGSL.bindingAttribs 0 0
    bufferDecl =
      WGSL.VarDecl bufferAttribs WGSL.Uniform bufferName (WGSL.Named structName)
    copies = WGSL.stmts $ concatMap copy scalars
    copy (name, typ) =
      [WGSL.DeclareVar name typ,
       WGSL.Assign name (WGSL.FieldExp bufferName name)]

-- | Internally, memory buffers are untyped but WGSL requires us to annotate the
-- binding with a type. Search the kernel body for any reads and writes to the
-- given buffer and return all types it is accessed at.
findMemoryTypes :: ImpGPU.Kernel -> VName -> [ImpGPU.PrimType]
findMemoryTypes kernel name = S.elems $ find (ImpGPU.kernelBody kernel)
  where
    find (ImpGPU.Write n _ t _ _ _) | n == name = S.singleton t
    find (ImpGPU.Read _ n _ t _ _) | n == name = S.singleton t
    find (s1 :>>: s2) = find s1 <> find s2
    find (For _ _ body) = find body
    find (While _ body) = find body
    find (If _ s1 s2) = find s1 <> find s2
    find _ = S.empty

genMemoryDecls :: ImpGPU.Kernel -> [WGSL.Declaration]
genMemoryDecls kernel = zipWith memDecl [1..] uses
  where
    uses = do
      ImpGPU.MemoryUse name <- ImpGPU.kernelUses kernel
      let types = findMemoryTypes kernel name
      case types of
        [] -> [] -- Do not need to generate declarations for unused buffers
        [t] -> pure (name, t)
        _more ->
          error "Using buffer at multiple types not supported in WebGPU backend"
    memDecl i (name, typ) =
      WGSL.VarDecl (WGSL.bindingAttribs 0 i) (WGSL.Storage WGSL.ReadWrite)
                   (nameToIdent name) (WGSL.Array $ primWGSLType typ)

-- | Generate `override` declarations for kernel 'ConstUse's and
-- backend-provided values (like block size and lockstep width).
genConstAndBuiltinDecls :: ImpGPU.Kernel -> [WGSL.Declaration]
genConstAndBuiltinDecls kernel = constDecls ++ builtinDecls
  where
    constDecls =
      -- TODO: constDecls should be i64s.
      -- Override declarations must have scalar type however (i.e. we can't use
      -- a vec2<i32> directly).
      [ WGSL.OverrideDecl (nameToIdent name) (WGSL.Prim WGSL.Int32)
        | ImpGPU.ConstUse name _ <- ImpGPU.kernelUses kernel ]
    builtinDecls =
      [WGSL.OverrideDecl builtinLockstepWidth (WGSL.Prim WGSL.Int32),
       WGSL.OverrideDecl builtinBlockSize (WGSL.Prim WGSL.Int32)]

nameToIdent :: VName -> WGSL.Ident
nameToIdent = zEncodeText . prettyText

textToIdent :: T.Text -> WGSL.Ident
textToIdent = zEncodeText
