{-# LANGUAGE QuasiQuotes #-}

module Futhark.CodeGen.Backends.TensorCore (compileGemmFun) where

import Data.Text.Internal as T
import Futhark.CodeGen.Backends.GenericC.Code
import Futhark.CodeGen.Backends.GenericC.Monad
import Futhark.CodeGen.ImpCode
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C
import NeatInterpolation (untrimming)

-- TODO: remove or use
gemmName :: T.Text
gemmName = "gemm_123456"

compileGemmFun ::
  [C.BlockItem] ->
  Function op ->
  CompilerM op s ()
compileGemmFun get_constants func@(Function _ outputs inputs body) =
  mapM_ earlyDecl [C.cunit| $esc:("#define memblock_shared memblock_device") |]

--  inNewFunction $ do
--  cachingMemory (lexicalMemoryUsage func) $ \decl_cached free_cached -> do
--    let futhark_function =
--          C.DeclSpec [] [C.EscTypeQual "FUTHARK_FUN_ATTR" mempty] (C.Tvoid mempty) mempty
--        test_item = [C.citem|// Test
--                          {}|]
--    pure
--      ( [C.cedecl|$spec:futhark_function $id:gemmName();|],
--        [C.cfun|$spec:futhark_function $id:gemmName() {
--                $item:test_item
--        }|]
--      )
