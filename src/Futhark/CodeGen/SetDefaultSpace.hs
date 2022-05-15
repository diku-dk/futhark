-- | Change 'DefaultSpace' in a program to some other memory space.
-- This is needed because the GPU backends use 'DefaultSpace' to refer
-- to GPU memory for most of the pipeline, but final code generation
-- assumes that 'DefaultSpace' is CPU memory.
module Futhark.CodeGen.SetDefaultSpace
  ( setDefaultSpace,
    setDefaultCodeSpace,
  )
where

import Futhark.CodeGen.ImpCode

-- | Set all uses of 'DefaultSpace' in the given definitions to another
-- memory space.
setDefaultSpace :: Space -> Definitions op -> Definitions op
setDefaultSpace space (Definitions (Constants ps consts) (Functions fundecs)) =
  Definitions
    (Constants (map (setParamSpace space) ps) (setCodeSpace space consts))
    ( Functions
        [ (fname, setFunctionSpace space func)
          | (fname, func) <- fundecs
        ]
    )

-- | Like 'setDefaultSpace', but for 'Code'.
setDefaultCodeSpace :: Space -> Code op -> Code op
setDefaultCodeSpace = setCodeSpace

setFunctionSpace :: Space -> Function op -> Function op
setFunctionSpace space (Function entry outputs inputs body results args) =
  Function
    entry
    (map (setParamSpace space) outputs)
    (map (setParamSpace space) inputs)
    (setCodeSpace space body)
    (map (setExtValueSpace space) results)
    (map (fmap $ setExtValueSpace space) args)

setParamSpace :: Space -> Param -> Param
setParamSpace space (MemParam name DefaultSpace) =
  MemParam name space
setParamSpace _ param =
  param

setExtValueSpace :: Space -> ExternalValue -> ExternalValue
setExtValueSpace space (OpaqueValue u desc vs) =
  OpaqueValue u desc $ map (setValueSpace space) vs
setExtValueSpace space (TransparentValue u v) =
  TransparentValue u $ setValueSpace space v

setValueSpace :: Space -> ValueDesc -> ValueDesc
setValueSpace space (ArrayValue mem _ bt ept shape) =
  ArrayValue mem space bt ept shape
setValueSpace _ (ScalarValue bt ept v) =
  ScalarValue bt ept v

setCodeSpace :: Space -> Code op -> Code op
setCodeSpace space (Allocate v e old_space) =
  Allocate v e $ setSpace space old_space
setCodeSpace space (Free v old_space) =
  Free v $ setSpace space old_space
setCodeSpace space (DeclareMem name old_space) =
  DeclareMem name $ setSpace space old_space
setCodeSpace space (DeclareArray name _ t vs) =
  DeclareArray name space t vs
setCodeSpace space (Copy t dest dest_offset dest_space src src_offset src_space n) =
  Copy t dest dest_offset dest_space' src src_offset src_space' n
  where
    dest_space' = setSpace space dest_space
    src_space' = setSpace space src_space
setCodeSpace space (Write dest dest_offset bt dest_space vol e) =
  Write dest dest_offset bt (setSpace space dest_space) vol e
setCodeSpace space (Read x dest dest_offset bt dest_space vol) =
  Read x dest dest_offset bt (setSpace space dest_space) vol
setCodeSpace space (c1 :>>: c2) =
  setCodeSpace space c1 :>>: setCodeSpace space c2
setCodeSpace space (For i e body) =
  For i e $ setCodeSpace space body
setCodeSpace space (While e body) =
  While e $ setCodeSpace space body
setCodeSpace space (If e c1 c2) =
  If e (setCodeSpace space c1) (setCodeSpace space c2)
setCodeSpace space (Comment s c) =
  Comment s $ setCodeSpace space c
setCodeSpace _ Skip =
  Skip
setCodeSpace _ (DeclareScalar name vol bt) =
  DeclareScalar name vol bt
setCodeSpace _ (SetScalar name e) =
  SetScalar name e
setCodeSpace space (SetMem to from old_space) =
  SetMem to from $ setSpace space old_space
setCodeSpace _ (Call dests fname args) =
  Call dests fname args
setCodeSpace _ (Assert e msg loc) =
  Assert e msg loc
setCodeSpace _ (DebugPrint s v) =
  DebugPrint s v
setCodeSpace _ (TracePrint msg) =
  TracePrint msg
setCodeSpace _ (Op op) =
  Op op

setSpace :: Space -> Space -> Space
setSpace space DefaultSpace = space
setSpace _ space = space
