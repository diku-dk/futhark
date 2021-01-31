module Futhark.CodeGen.Backends.SPIRV.Operations
  ( signedRemainder,
    unsignedDivisionUp,
    signedDivision,
    signedDivisionUp,
    floatModulo,
    integerPower,
  )
where

import Futhark.CodeGen.Backends.SPIRV.Builder
import Futhark.IR.Primitive

signedRemainder :: SPIRVType -> Id -> Id -> Builder s Id
signedRemainder t@(TScalar (IntType _)) x y = do
  xa <- glslInstr t $ GlslSAbs x
  ya <- glslInstr t $ GlslSAbs y
  m <- instrRT t $ OpUMod xa ya
  sign <- glslInstr t $ GlslSSign x
  instrRT t $ OpIMul sign m
signedRemainder _ _ _ = error "signedRemainder on non-integer type"

unsignedDivisionUp :: SPIRVType -> Id -> Id -> Builder s Id
unsignedDivisionUp t@(TScalar (IntType it)) x y = do
  -- (x + y - 1) / y
  one <- getConstId $ Const $ IntValue $ intValue it (1 :: Integer)
  x_plus_y <- instrRT t $ OpIAdd x y
  dividend <- instrRT t $ OpISub x_plus_y one
  instrRT t $ OpUDiv dividend y
unsignedDivisionUp _ _ _ = error "unsignedDivisionUp on non-integer type"

signedDivision :: SPIRVType -> Id -> Id -> Builder s Id
signedDivision t@(TScalar (IntType it)) x y = do
  let bool = TScalar Bool
  zero <- getConstId $ Const $ IntValue $ intValue it (0 :: Integer)
  one <- getConstId $ Const $ IntValue $ intValue it (1 :: Integer)
  q <- instrRT t $ OpSDiv x y
  r <- signedRemainder t x y
  -- flag == (r != 0 && r < 0 != y < 0)
  r_nz <- instrRT bool $ OpINotEqual r zero
  r_lz <- instrRT bool $ OpSLessThan r zero
  y_lz <- instrRT bool $ OpSLessThan y zero
  y_lz_neq_r_lz <- instrRT bool $ OpLogicalNotEqual r_lz y_lz
  flag <- instrRT bool $ OpLogicalAnd r_nz y_lz_neq_r_lz
  -- one_or_zero == flag ? 1 : 0
  one_or_zero <- instrRT t $ OpSelect flag one zero
  instrRT t $ OpISub q one_or_zero
signedDivision _ _ _ = error "signedDivision on non-integer type"

signedDivisionUp :: SPIRVType -> Id -> Id -> Builder s Id
signedDivisionUp t@(TScalar (IntType it)) x y = do
  --  sdiv_up(x, y) == sdiv(x + y - 1, y)
  one <- getConstId $ Const $ IntValue $ intValue it (1 :: Integer)
  x_plus_y <- instrRT t $ OpIAdd x y
  new_x <- instrRT t $ OpISub x_plus_y one
  signedDivision t new_x y
signedDivisionUp _ _ _ = error "signedDivisionUp on non-integer type"

floatModulo :: SPIRVType -> Id -> Id -> Builder s Id
floatModulo t@(TScalar (FloatType _)) x y = do
  -- fmod(x,y) == x-y*trunc(x/y)
  divided <- instrRT t $ OpFDiv x y
  truncated <- glslInstr t $ GlslTrunc divided
  multiplied <- instrRT t $ OpFMul y truncated
  instrRT t $ OpFSub x multiplied
floatModulo _ _ _ = error "floatModulo on non-float type"

integerPower :: SPIRVType -> Id -> Id -> Builder s Id
integerPower t@(TScalar (IntType it)) x y = do
-- Pseudocode for this:
-- ipow(base, exp) {
--   res = 1;
--   while (exp != 0) {
--     if (exp & 1 != 0) {
--       res *= base;
--     }
--     exp >>= 1
--     base *= base;
--   }
--   return res;
-- }
  let bool = TScalar Bool
  zero <- getConstId $ Const $ IntValue $ intValue it (0 :: Integer)
  one <- getConstId $ Const $ IntValue $ intValue it (1 :: Integer)
  preLoopLabel <- newId
  startLabel <- newId
  checkLabel <- newId
  bodyLabel <- newId
  continueLabel <- newId
  endLabel <- newId

  res' <- newId
  exp_' <- newId
  base' <- newId

  instr $ OpBranch preLoopLabel
  instr $ OpLabel preLoopLabel
  instr $ OpBranch startLabel

  --  Loop start
  instr $ OpLabel startLabel
  res <- instrRT t $ OpPhi [PairIdRefIdRef (one, preLoopLabel),
                            PairIdRefIdRef (res', continueLabel)]
  exp_ <- instrRT t $ OpPhi [PairIdRefIdRef (y, preLoopLabel),
                             PairIdRefIdRef (exp_', continueLabel)]
  base <- instrRT t $ OpPhi [PairIdRefIdRef (x, preLoopLabel),
                             PairIdRefIdRef (base', continueLabel)]
  instr $ OpLoopMerge endLabel continueLabel $ LoopControl [LoopControlNone]
  instr $ OpBranch checkLabel

  -- Loop condition check: (exp_ != 0)
  instr $ OpLabel checkLabel
  exp_nz <- instrRT bool $ OpINotEqual exp_ zero
  instr $ OpBranchConditional exp_nz bodyLabel endLabel []

  -- Loop body
  instr $ OpLabel bodyLabel
  exp_and1 <- instrRT t $ OpBitwiseAnd exp_ one
  exp_and1_nz <- instrRT bool $ OpINotEqual exp_and1 zero
  -- tmp = res * base
  tmp <- instrRT t $ OpIMul res base
  -- if (exp & 1 == 1) res = tmp; else res = res;
  instrRTId t res' $ OpSelect exp_and1_nz tmp res
  -- exp >>= 1
  instrRTId t exp_' $ OpShiftRightLogical exp_ one
  -- base *= base
  instrRTId t base' $ OpIMul base base
  instr $ OpBranch continueLabel

  -- Continue (nothing to do)
  instr $ OpLabel continueLabel
  instr $ OpBranch startLabel

  -- Loop end
  instr $ OpLabel endLabel
  return res
integerPower _ _ _ = error "integerPower on non-integer type"
