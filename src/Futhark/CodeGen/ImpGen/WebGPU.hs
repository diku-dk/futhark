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

      webgpu_prelude = mempty
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
-- TODO: WGSL only has 32-bit primitive integers
primWGSLType (IntType Int8) = WGSL.Int32
primWGSLType (IntType Int16) = WGSL.Int32
primWGSLType (IntType Int64) = WGSL.Int32
primWGSLType (FloatType Float16) = WGSL.Float16
primWGSLType (FloatType Float32) = WGSL.Float32
primWGSLType (FloatType Float64) = error "TODO: WGSL has no f64"
primWGSLType Bool = WGSL.Bool
-- TODO: Make sure we do not ever codegen statements involving Unit variables
primWGSLType Unit = error "TODO: no unit in WGSL"

genWGSLStm :: Code ImpGPU.KernelOp -> WGSL.Stmt
genWGSLStm Skip = WGSL.Skip
genWGSLStm (s1 :>>: s2) = WGSL.Seq (genWGSLStm s1) (genWGSLStm s2)
genWGSLStm (DeclareScalar name _ typ) =
  WGSL.DeclareVar (nameToIdent name) (WGSL.Prim $ primWGSLType typ)
genWGSLStm (If cond cThen cElse) = 
  WGSL.If (genWGSLExp $ untyped cond) (genWGSLStm cThen) (genWGSLStm cElse)
genWGSLStm (SetScalar name e) = WGSL.Assign (nameToIdent name) (genWGSLExp e)
genWGSLStm (Op (ImpGPU.GetBlockId dest i)) = 
  WGSL.Assign (nameToIdent dest) (WGSL.IndexExp "workgroup_id" (WGSL.IntExp i))
genWGSLStm (Op (ImpGPU.GetLocalId dest i)) = 
  WGSL.Assign (nameToIdent dest) (WGSL.IndexExp "local_id" (WGSL.IntExp i))
genWGSLStm (Op (ImpGPU.GetLocalSize dest _)) = 
  WGSL.Assign (nameToIdent dest) (WGSL.VarExp builtinBlockSize)
genWGSLStm (Op (ImpGPU.GetLockstepWidth dest)) = 
  WGSL.Assign (nameToIdent dest) (WGSL.VarExp builtinLockstepWidth)
genWGSLStm _ = WGSL.Skip

-- TODO: This does not respect the indicated sizes and signedness currently, so
-- we will always perform operations according to the declared types of the
-- involved variables.
wgslBinOp :: BinOp -> WGSL.BinOp
wgslBinOp (Add _ _) = "+"
wgslBinOp (FAdd _) = "+"
wgslBinOp (Sub _ _) = "-"
wgslBinOp (FSub _) = "-"
wgslBinOp (Mul _ _) = "*"
wgslBinOp (FMul _) = "*"
wgslBinOp _ = "???"

-- TODO: Similar to above, this does not respect signedness properly right now.
wgslCmpOp :: CmpOp -> WGSL.BinOp
wgslCmpOp (CmpEq _) = "=="
wgslCmpOp (CmpUlt _) = "<"
wgslCmpOp (CmpUle _) = "<="
wgslCmpOp (CmpSlt _) = "<"
wgslCmpOp (CmpSle _) = "<="
wgslCmpOp (FCmpLt _) = "<"
wgslCmpOp (FCmpLe _) = "<="
wgslCmpOp CmpLlt = "<" -- TODO: This does not actually work for bools.
wgslCmpOp CmpLle = "=="

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
  WGSL.BinOpExp (wgslBinOp op) (genWGSLExp e1) (genWGSLExp e2)
genWGSLExp (CmpOpExp op e1 e2) =
  WGSL.BinOpExp (wgslCmpOp op) (genWGSLExp e1) (genWGSLExp e2)
-- don't support different integer types currently
genWGSLExp (ConvOpExp (ZExt _ _) e) = genWGSLExp e
-- don't support different integer types currently
genWGSLExp (ConvOpExp (SExt _ _) e) = genWGSLExp e
genWGSLExp _ = WGSL.StringExp "<not implemented>"

scalarUses :: [ImpGPU.KernelUse] -> [(WGSL.Ident, WGSL.Typ)]
scalarUses [] = []
scalarUses ((ImpGPU.ScalarUse name typ):us) =
  (nameToIdent name, WGSL.Prim (primWGSLType typ)) : scalarUses us
scalarUses (_:us) = scalarUses us

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
    scalars = scalarUses (ImpGPU.kernelUses kernel)
    structDecl = WGSL.StructDecl $
      WGSL.Struct structName (map (uncurry WGSL.Field) scalars)
    bufferDecl = WGSL.VarDecl WGSL.Uniform bufferName (WGSL.Named structName)
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
genMemoryDecls kernel = do
  ImpGPU.MemoryUse name <- ImpGPU.kernelUses kernel
  let types = findMemoryTypes kernel name
  case types of
    [] -> [] -- Do not need to generate declarations for unused buffers
    [t] -> pure $ WGSL.VarDecl (WGSL.Storage WGSL.ReadWrite) (nameToIdent name)
                    (WGSL.Array $ primWGSLType t)
    _more ->
      error "Accessing buffer at multiple type not supported in WebGPU backend"

-- | Generate `override` declarations for kernel 'ConstUse's and
-- backend-provided values (like block size and lockstep width).
genConstAndBuiltinDecls :: ImpGPU.Kernel -> [WGSL.Declaration]
genConstAndBuiltinDecls kernel = constDecls ++ builtinDecls
  where 
    constDecls = do
      ImpGPU.ConstUse name _ <- ImpGPU.kernelUses kernel
      pure $ WGSL.OverrideDecl (nameToIdent name) (WGSL.Prim WGSL.Int32)
    builtinDecls =
      [WGSL.OverrideDecl builtinLockstepWidth (WGSL.Prim WGSL.Int32),
       WGSL.OverrideDecl builtinBlockSize (WGSL.Prim WGSL.Int32)]

nameToIdent :: VName -> WGSL.Ident
nameToIdent = zEncodeText . prettyText

textToIdent :: T.Text -> WGSL.Ident
textToIdent = zEncodeText
