{-# LANGUAGE QuasiQuotes #-}

-- | C code generation for functions.
module Futhark.CodeGen.Backends.GenericC.Fun
  ( compileFun,
    compileVoidFun,
    module Futhark.CodeGen.Backends.GenericC.Monad,
    module Futhark.CodeGen.Backends.GenericC.Code,
  )
where

import Control.Monad
import Futhark.CodeGen.Backends.GenericC.Code
import Futhark.CodeGen.Backends.GenericC.Monad
import Futhark.CodeGen.ImpCode
import Futhark.MonadFreshNames
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C

compileFunBody :: [C.Exp] -> [Param] -> Code op -> CompilerM op s ()
compileFunBody output_ptrs outputs code = do
  mapM_ declareOutput outputs
  compileCode code
  zipWithM_ setRetVal' output_ptrs outputs
  where
    declareOutput (MemParam name space) =
      declMem name space
    declareOutput (ScalarParam name pt) = do
      let ctp = primTypeToCType pt
      decl [C.cdecl|$ty:ctp $id:name;|]

    setRetVal' p (MemParam name space) =
      -- It is required that the memory block is already initialised
      -- (although it may be NULL).
      setMem [C.cexp|*$exp:p|] name space
    setRetVal' p (ScalarParam name _) =
      stm [C.cstm|*$exp:p = $id:name;|]

compileInput :: Param -> CompilerM op s C.Param
compileInput (ScalarParam name bt) = do
  let ctp = primTypeToCType bt
  pure [C.cparam|$ty:ctp $id:name|]
compileInput (MemParam name space) = do
  ty <- memToCType name space
  pure [C.cparam|$ty:ty $id:name|]

compileOutput :: Param -> CompilerM op s (C.Param, C.Exp)
compileOutput (ScalarParam name bt) = do
  let ctp = primTypeToCType bt
  p_name <- newVName $ "out_" ++ baseString name
  pure ([C.cparam|$ty:ctp *$id:p_name|], [C.cexp|$id:p_name|])
compileOutput (MemParam name space) = do
  ty <- memToCType name space
  p_name <- newVName $ baseString name ++ "_p"
  pure ([C.cparam|$ty:ty *$id:p_name|], [C.cexp|$id:p_name|])

compileFun :: [C.BlockItem] -> [C.Param] -> (Name, Function op) -> CompilerM op s (C.Definition, C.Func)
compileFun get_constants extra (fname, func@(Function _ outputs inputs body)) = inNewFunction $ do
  (outparams, out_ptrs) <- mapAndUnzipM compileOutput outputs
  inparams <- mapM compileInput inputs

  cachingMemory (lexicalMemoryUsage func) $ \decl_cached free_cached -> do
    body' <- collect $ compileFunBody out_ptrs outputs body
    decl_mem <- declAllocatedMem
    free_mem <- freeAllocatedMem
    let futhark_function =
          C.DeclSpec [] [C.EscTypeQual "FUTHARK_FUN_ATTR" mempty] (C.Tint Nothing mempty) mempty

    pure
      ( [C.cedecl|$spec:futhark_function $id:(funName fname)($params:extra, $params:outparams, $params:inparams);|],
        [C.cfun|$spec:futhark_function $id:(funName fname)($params:extra, $params:outparams, $params:inparams) {
               $stms:ignores
               int err = 0;
               $items:decl_cached
               $items:decl_mem
               $items:get_constants
               $items:body'
              cleanup:
               {
               $stms:free_cached
               $items:free_mem
               }
               return err;
  }|]
      )
  where
    -- Ignore all the boilerplate parameters, just in case we don't
    -- actually need to use them.
    ignores = [[C.cstm|(void)$id:p;|] | C.Param (Just p) _ _ _ <- extra]

-- | Generate code for a function that returns void (meaning it cannot
-- fail) and has no extra parameters (meaning it cannot allocate
-- memory non-lexxical or do anything fancy).
compileVoidFun :: [C.BlockItem] -> (Name, Function op) -> CompilerM op s (C.Definition, C.Func)
compileVoidFun get_constants (fname, func@(Function _ outputs inputs body)) = inNewFunction $ do
  (outparams, out_ptrs) <- mapAndUnzipM compileOutput outputs
  inparams <- mapM compileInput inputs

  cachingMemory (lexicalMemoryUsage func) $ \decl_cached free_cached -> do
    body' <- collect $ compileFunBody out_ptrs outputs body
    let futhark_function =
          C.DeclSpec [] [C.EscTypeQual "FUTHARK_FUN_ATTR" mempty] (C.Tvoid mempty) mempty

    pure
      ( [C.cedecl|$spec:futhark_function $id:(funName fname)($params:outparams, $params:inparams);|],
        [C.cfun|$spec:futhark_function $id:(funName fname)($params:outparams, $params:inparams) {
               $items:decl_cached
               $items:get_constants
               $items:body'
               $stms:free_cached
               }|]
      )
