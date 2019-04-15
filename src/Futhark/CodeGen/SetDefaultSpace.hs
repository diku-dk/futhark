module Futhark.CodeGen.SetDefaultSpace
       ( setDefaultSpace
       )
       where

import Futhark.CodeGen.ImpCode
import Futhark.CodeGen.ImpCode.Kernels (HostOp(..))

-- | Set all uses of 'DefaultSpace' in the given functions to another memory space.
setDefaultSpace :: Space -> Functions HostOp -> Functions HostOp
setDefaultSpace space (Functions fundecs) =
  Functions [ (fname, setFunctionSpace space func)
            | (fname, func) <- fundecs ]

setFunctionSpace :: Space -> Function HostOp -> Function HostOp
setFunctionSpace space (Function entry outputs inputs body results args) =
  Function entry
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
setExtValueSpace space (OpaqueValue desc vs) =
  OpaqueValue desc $ map (setValueSpace space) vs
setExtValueSpace space (TransparentValue v) =
  TransparentValue $ setValueSpace space v

setValueSpace :: Space -> ValueDesc -> ValueDesc
setValueSpace space (ArrayValue mem memsize _ bt ept shape) =
  ArrayValue mem memsize space bt ept shape
setValueSpace _ (ScalarValue bt ept v) =
  ScalarValue bt ept v

setBodySpace :: Space -> Code HostOp -> Code HostOp
setBodySpace space (Allocate v e old_space) =
  Allocate v (setCountSpace space e) $ setSpace space old_space
setBodySpace space (Free v old_space) =
  Free v $ setSpace space old_space
setBodySpace space (DeclareMem name old_space) =
  DeclareMem name $ setSpace space old_space
setBodySpace space (DeclareArray name _ t vs) =
  DeclareArray name space t vs
setBodySpace space (Copy dest dest_offset dest_space src src_offset src_space n) =
  Copy
  dest (setCountSpace space dest_offset) dest_space'
  src (setCountSpace space src_offset) src_space' $
  setCountSpace space n
  where dest_space' = setSpace space dest_space
        src_space' = setSpace space src_space
setBodySpace space (Write dest dest_offset bt dest_space vol e) =
  Write dest (setCountSpace space dest_offset) bt (setSpace space dest_space)
  vol (setExpSpace space e)
setBodySpace space (c1 :>>: c2) =
  setBodySpace space c1 :>>: setBodySpace space c2
setBodySpace space (For i it e body) =
  For i it (setExpSpace space e) $ setBodySpace space body
setBodySpace space (While e body) =
  While (setExpSpace space e) $ setBodySpace space body
setBodySpace space (If e c1 c2) =
  If (setExpSpace space e) (setBodySpace space c1) (setBodySpace space c2)
setBodySpace space (Comment s c) =
  Comment s $ setBodySpace space c
setBodySpace _ Skip =
  Skip
setBodySpace _ (DeclareScalar name bt) =
  DeclareScalar name bt
setBodySpace space (SetScalar name e) =
  SetScalar name $ setExpSpace space e
setBodySpace space (SetMem to from old_space) =
  SetMem to from $ setSpace space old_space
setBodySpace space (Call dests fname args) =
  Call dests fname $ map setArgSpace args
  where setArgSpace (MemArg m) = MemArg m
        setArgSpace (ExpArg e) = ExpArg $ setExpSpace space e
setBodySpace space (Assert e msg loc) =
  Assert (setExpSpace space e) msg loc
setBodySpace space (DebugPrint s t e) =
  DebugPrint s t (setExpSpace space e)
setBodySpace space (Op op) =
  Op $ setHostOpDefaultSpace space op

setHostOpDefaultSpace :: Space -> HostOp -> HostOp
setHostOpDefaultSpace space (Husk hspace interm red body after) =
  Husk hspace interm (setBodySpace space red) (setBodySpace space body)
       (setBodySpace space after)
setHostOpDefaultSpace _ op = op

setCountSpace :: Space -> Count a -> Count a
setCountSpace space (Count e) =
  Count $ setExpSpace space e

setExpSpace :: Space -> Exp -> Exp
setExpSpace space = fmap setLeafSpace
  where setLeafSpace (Index mem i bt DefaultSpace vol) =
          Index mem i bt space vol
        setLeafSpace e = e

setSpace :: Space -> Space -> Space
setSpace space DefaultSpace = space
setSpace _     space        = space
