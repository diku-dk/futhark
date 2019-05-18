module Futhark.CodeGen.SetDefaultSpace
       ( setDefaultSpace
       )
       where

import qualified Data.Set as S
import Control.Monad.Reader

import Futhark.CodeGen.ImpCode
import Futhark.CodeGen.ImpCode.Kernels (HostOp(..))

type SetDefaultSpaceM = Reader (S.Set VName)

localExclude :: S.Set VName -> SetDefaultSpaceM a -> SetDefaultSpaceM a
localExclude s = local (<>s)

-- | Set all uses of 'DefaultSpace' in the given functions to another memory space.
setDefaultSpace :: Space -> Functions HostOp -> Functions HostOp
setDefaultSpace space (Functions fundecs) =
  Functions [ (fname, runReader (setFunctionSpace space func) S.empty)
            | (fname, func) <- fundecs ]

setFunctionSpace :: Space -> Function HostOp -> SetDefaultSpaceM (Function HostOp)
setFunctionSpace space (Function entry outputs inputs body results args node_id) = do
  outputs' <- mapM (setParamSpace space) outputs
  inputs' <- mapM (setParamSpace space) inputs
  body' <- setBodySpace space body 
  results' <- mapM (setExtValueSpace space) results
  args' <- mapM (setExtValueSpace space) args
  node_id' <- setExpSpace space node_id
  return $ Function entry outputs' inputs' body' results' args' node_id'

setParamSpace :: Space -> Param -> SetDefaultSpaceM Param
setParamSpace space (MemParam name old_space) =
  MemParam name <$> setSpace name space old_space
setParamSpace _ param =
  return param

setExtValueSpace :: Space -> ExternalValue -> SetDefaultSpaceM ExternalValue
setExtValueSpace space (OpaqueValue desc vs) =
  OpaqueValue desc <$> mapM (setValueSpace space) vs
setExtValueSpace space (TransparentValue v) =
  TransparentValue <$> setValueSpace space v

setValueSpace :: Space -> ValueDesc -> SetDefaultSpaceM ValueDesc
setValueSpace space (ArrayValue mem memsize _ bt ept shape) =
  return $ ArrayValue mem memsize space bt ept shape
setValueSpace _ (ScalarValue bt ept v) =
  return $ ScalarValue bt ept v

setBodySpace :: Space -> Code HostOp -> SetDefaultSpaceM (Code HostOp)
setBodySpace space (Allocate v e old_space) =
  Allocate v <$> setCountSpace space e <*> setSpace v space old_space
setBodySpace space (Free v old_space) =
  Free v <$> setSpace v space old_space
setBodySpace space (DeclareMem name old_space) =
  DeclareMem name <$> setSpace name space old_space
setBodySpace space (DeclareArray name _ t vs) =
  return $ DeclareArray name space t vs
setBodySpace space (Copy dest dest_offset dest_space src src_offset src_space n) = do
  dest_offset' <- setCountSpace space dest_offset
  src_offset' <- setCountSpace space src_offset
  dest_space' <- setSpace dest space dest_space
  src_space' <- setSpace src space src_space
  Copy dest dest_offset' dest_space' src src_offset' src_space' <$>
    setCountSpace space n
setBodySpace space (PeerCopy dest dest_offset dest_peer dest_space
                             src src_offset src_peer src_space n) = do
  dest_offset' <- setCountSpace space dest_offset
  src_offset' <- setCountSpace space src_offset
  dest_peer' <- setExpSpace space dest_peer
  src_peer' <- setExpSpace space src_peer
  dest_space' <- setSpace dest space dest_space
  src_space' <- setSpace src space src_space
  PeerCopy dest dest_offset' dest_peer' dest_space' src src_offset'
    src_peer' src_space' <$> setCountSpace space n
setBodySpace space (Write dest dest_offset bt dest_space vol e) = do
  dest_offset' <- setCountSpace space dest_offset
  dest_space' <- setSpace dest space dest_space
  Write dest dest_offset' bt dest_space' vol <$> setExpSpace space e
setBodySpace space (c1 :>>: c2) = do
  c1' <- setBodySpace space c1
  c2' <- setBodySpace space c2
  return $ c1' :>>: c2'
setBodySpace space (For i it e body) =
  For i it <$> setExpSpace space e <*> setBodySpace space body
setBodySpace space (While e body) =
  While <$> setExpSpace space e <*> setBodySpace space body
setBodySpace space (If e c1 c2) =
  If <$> setExpSpace space e <*> setBodySpace space c1 <*> setBodySpace space c2
setBodySpace space (Comment s c) =
  Comment s <$> setBodySpace space c
setBodySpace _ Skip =
  return Skip
setBodySpace _ (DeclareScalar name bt) =
  return $ DeclareScalar name bt
setBodySpace space (SetScalar name e) =
  SetScalar name <$> setExpSpace space e
setBodySpace space (SetMem to from old_space) =
  SetMem to from <$> setSpace to space old_space
setBodySpace space (Call dests fname args) =
  Call dests fname <$> mapM setArgSpace args
  where setArgSpace (MemArg m) = return $ MemArg m
        setArgSpace (ExpArg e) = ExpArg <$> setExpSpace space e
setBodySpace space (Assert e msg loc) = do
  e' <- setExpSpace space e
  return $ Assert e' msg loc
setBodySpace space (DebugPrint s t e) =
  DebugPrint s t <$> setExpSpace space e
setBodySpace space (Op op) =
  Op <$> setHostOpDefaultSpace space op

setHostOpDefaultSpace :: Space -> HostOp -> SetDefaultSpaceM HostOp
setHostOpDefaultSpace space (Husk keep_host num_nodes bparams husk_func interm body red) =
  localExclude (S.fromList keep_host) $
    Husk keep_host num_nodes
      <$> mapM (setParamSpace space) bparams
      <*> pure husk_func
      <*> pure interm -- Keep intermediate allocations on host
      <*> setBodySpace space body
      <*> pure red -- Keep reduce on host
setHostOpDefaultSpace _ op = return op

setCountSpace :: Space -> Count a -> SetDefaultSpaceM (Count a)
setCountSpace space (Count e) =
  Count <$> setExpSpace space e

setExpSpace :: Space -> Exp -> SetDefaultSpaceM Exp
setExpSpace space = traverse setLeafSpace
  where setLeafSpace (Index mem i bt old_space vol) = do
          new_space <- setSpace mem space old_space
          return $ Index mem i bt new_space vol
        setLeafSpace e = return e

setSpace :: VName -> Space -> Space -> SetDefaultSpaceM Space
setSpace name space DefaultSpace = do
  exclude <- ask
  if name `S.member` exclude then return DefaultSpace else return space
setSpace _    _     space        = return space
