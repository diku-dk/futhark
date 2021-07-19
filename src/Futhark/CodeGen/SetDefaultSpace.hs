-- | Change 'DefaultSpace' in a program to some other memory space.
-- This is needed because the GPU backends use 'DefaultSpace' to refer
-- to GPU memory for most of the pipeline, but final code generation
-- assumes that 'DefaultSpace' is CPU memory.
module Futhark.CodeGen.SetDefaultSpace
  ( setDefaultSpace,
  )
where

import Futhark.CodeGen.ImpCode

-- | Set all uses of 'DefaultSpace' in the given definitions to another
-- memory space.
setDefaultSpace :: Space -> Definitions op -> Definitions op
setDefaultSpace space (Definitions (Constants ps consts) (Functions fundecs)) =
  Definitions
    (Constants (map (setParamSpace space) ps) (setBodySpace space consts))
    ( Functions
        [ (fname, setFunctionSpace space func)
          | (fname, func) <- fundecs
        ]
    )

setFunctionSpace :: Space -> Function op -> Function op
setFunctionSpace space (Function entry outputs inputs body results args) =
  Function
    entry
    (map (setParamSpace space) outputs)
    (map (setParamSpace space) inputs)
    (setBodySpace space body)
    (map (setExtValueSpace space) results)
    (map (setExtValueSpace space) args)

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

setBodySpace :: Space -> Code op -> Code op
setBodySpace space (Allocate v e old_space) =
  Allocate v (fmap (setTExpSpace space) e) $ setSpace space old_space
setBodySpace space (Free v old_space) =
  Free v $ setSpace space old_space
setBodySpace space (DeclareMem name old_space) =
  DeclareMem name $ setSpace space old_space
setBodySpace space (DeclareArray name _ t vs) =
  DeclareArray name space t vs
setBodySpace space (Copy dest dest_offset dest_space src src_offset src_space n) =
  Copy
    dest
    (fmap (setTExpSpace space) dest_offset)
    dest_space'
    src
    (fmap (setTExpSpace space) src_offset)
    src_space'
    $ fmap (setTExpSpace space) n
  where
    dest_space' = setSpace space dest_space
    src_space' = setSpace space src_space
setBodySpace space (Write dest dest_offset bt dest_space vol e) =
  Write
    dest
    (fmap (setTExpSpace space) dest_offset)
    bt
    (setSpace space dest_space)
    vol
    (setExpSpace space e)
setBodySpace space (c1 :>>: c2) =
  setBodySpace space c1 :>>: setBodySpace space c2
setBodySpace space (For i e body) =
  For i (setExpSpace space e) $ setBodySpace space body
setBodySpace space (While e body) =
  While (setTExpSpace space e) $ setBodySpace space body
setBodySpace space (If e c1 c2) =
  If (setTExpSpace space e) (setBodySpace space c1) (setBodySpace space c2)
setBodySpace space (Comment s c) =
  Comment s $ setBodySpace space c
setBodySpace _ Skip =
  Skip
setBodySpace _ (DeclareScalar name vol bt) =
  DeclareScalar name vol bt
setBodySpace space (SetScalar name e) =
  SetScalar name $ setExpSpace space e
setBodySpace space (SetMem to from old_space) =
  SetMem to from $ setSpace space old_space
setBodySpace space (Call dests fname args) =
  Call dests fname $ map setArgSpace args
  where
    setArgSpace (MemArg m) = MemArg m
    setArgSpace (ExpArg e) = ExpArg $ setExpSpace space e
setBodySpace space (Assert e msg loc) =
  Assert (setExpSpace space e) (fmap (setExpSpace space) msg) loc
setBodySpace space (DebugPrint s v) =
  DebugPrint s $ fmap (setExpSpace space) v
setBodySpace space (TracePrint msg) =
  TracePrint $ fmap (setExpSpace space) msg
setBodySpace _ (Op op) =
  Op op

setExpSpace :: Space -> Exp -> Exp
setExpSpace space = fmap setLeafSpace
  where
    setLeafSpace (Index mem i bt DefaultSpace vol) =
      Index mem i bt space vol
    setLeafSpace e = e

setTExpSpace :: Space -> TExp t -> TExp t
setTExpSpace space = TPrimExp . setExpSpace space . untyped

setSpace :: Space -> Space -> Space
setSpace space DefaultSpace = space
setSpace _ space = space
