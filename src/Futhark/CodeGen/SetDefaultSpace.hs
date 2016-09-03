module Futhark.CodeGen.SetDefaultSpace
       ( setDefaultSpace
       , setBodySpace
       )
       where

import Futhark.CodeGen.ImpCode

setDefaultSpace :: Space -> Functions op -> Functions op
setDefaultSpace space (Functions fundecs) =
  Functions [ (fname, setFunctionSpace space func)
            | (fname, func) <- fundecs ]

setFunctionSpace :: Space -> Function op -> Function op
setFunctionSpace space (Function entry outputs inputs body results args) =
  Function entry
  (map (setParamSpace space) outputs)
  (map (setParamSpace space) inputs)
  (setBodySpace space body)
  results
  args

setParamSpace :: Space -> Param -> Param
setParamSpace space (MemParam name size DefaultSpace) =
  MemParam name size space
setParamSpace _ param =
  param

setBodySpace :: Space -> Code op -> Code op
setBodySpace space (Allocate v e old_space) =
  Allocate v (setCountSpace space e) $ setSpace space old_space
setBodySpace space (DeclareMem name old_space) =
  DeclareMem name $ setSpace space old_space
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
setBodySpace space (For i e body) =
  For i (setExpSpace space e) $ setBodySpace space body
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
setBodySpace space (Assert e loc) =
  Assert (setExpSpace space e) loc
setBodySpace _ (Op op) =
  Op op

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
