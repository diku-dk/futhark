-- | Code generation for ImpCode with WebGPU.
module Futhark.CodeGen.ImpGen.WebGPU
  ( compileProg,
    Warnings,
  )
where

import Control.Monad.State
import Data.Bifunctor (second)
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.CodeGen.ImpCode.GPU qualified as ImpGPU
import Futhark.CodeGen.ImpCode.WebGPU
import Futhark.CodeGen.ImpGen.WGSL qualified as WGSL
import Futhark.CodeGen.ImpGen.GPU qualified as ImpGPU
import Futhark.IR.GPUMem qualified as F
import Futhark.MonadFreshNames
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

-- Main function for translating an ImpGPU kernel to a WebGPU kernel.
onKernel :: ImpGPU.Kernel -> WebGPUM HostOp
onKernel kernel = do
  addCode $ "Input for " <> name <> "\n"
  addCode $ prettyText (ImpGPU.kernelBody kernel) <> "\n\n"
  addCode $ "Code for " <> name <> ":\n"
  let wgslBody = genWGSLStm (ImpGPU.kernelBody kernel)
  let attribs = [WGSL.Attrib "compute" [],
                 WGSL.Attrib "workgroup_size" [WGSL.VarExp "todo"]]
  let wgslFun = WGSL.Function
                  { WGSL.funName = name,
                    WGSL.funAttribs = attribs,
                    WGSL.funParams = entryParams,
                    WGSL.funBody = [wgslBody]
                  }
  addCode $ prettyText wgslFun
  -- TODO: return something sensible.
  pure $ LaunchKernel SafetyNone (ImpGPU.kernelName kernel) 0 [] [] []
    where name = nameToText (ImpGPU.kernelName kernel)

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

-- | Generate HIP host and device code.
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
  WGSL.If (genWGSLExp $ untyped cond) [genWGSLStm cThen] [genWGSLStm cElse]
genWGSLStm (Op (ImpGPU.GetBlockId dest i)) = 
  WGSL.Assign (nameToIdent dest) (WGSL.IndexExp "workgroup_id" (WGSL.IntExp i))
genWGSLStm (Op (ImpGPU.GetLocalId dest i)) = 
  WGSL.Assign (nameToIdent dest) (WGSL.IndexExp "local_id" (WGSL.IntExp i))
genWGSLStm (Op (ImpGPU.GetLocalSize dest _)) = 
  WGSL.Assign (nameToIdent dest) (WGSL.StringExp "TODO: Deal with block size")
genWGSLStm (Op (ImpGPU.GetLockstepWidth dest)) = 
  WGSL.Assign (nameToIdent dest) (WGSL.StringExp "TODO: Can't get lockstep width")
genWGSLStm _ = WGSL.Skip

genWGSLExp :: Exp -> WGSL.Exp
genWGSLExp _ = WGSL.BoolExp False

nameToIdent :: VName -> WGSL.Ident
nameToIdent = prettyText
