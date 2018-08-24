{-# LANGUAGE QuasiQuotes #-}
-- | C code generator.  This module can convert a correct ImpCode
-- program to an equivalent C program. The C code is strictly
-- sequential, but can handle the full Futhark language.
module Futhark.CodeGen.Backends.SequentialC
  ( compileProg
  , GC.CParts(..)
  , GC.asLibrary
  , GC.asExecutable
  ) where

import Control.Monad

import qualified Language.C.Quote.OpenCL as C

import Futhark.Error
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.ImpCode.Sequential as Imp
import qualified Futhark.CodeGen.ImpGen.Sequential as ImpGen
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either InternalError GC.CParts)
compileProg =
  traverse (GC.compileProg operations generateContext () [DefaultSpace] []) <=<
  ImpGen.compileProg
  where operations :: GC.Operations Imp.Sequential ()
        operations = GC.defaultOperations
                     { GC.opsCompiler = const $ return ()
                     , GC.opsCopy = copySequentialMemory
                     }

        generateContext = do
          cfg <- GC.publicDef "context_config" GC.InitDecl $ \s ->
            ([C.cedecl|struct $id:s;|],
             [C.cedecl|struct $id:s { int debugging; };|])
          GC.publicDef_ "context_config_new" GC.InitDecl $ \s ->
            ([C.cedecl|struct $id:cfg* $id:s();|],
             [C.cedecl|struct $id:cfg* $id:s() {
                                 struct $id:cfg *cfg = malloc(sizeof(struct $id:cfg));
                                 if (cfg == NULL) {
                                   return NULL;
                                 }
                                 cfg->debugging = 0;
                                 return cfg;
                               }|])
          GC.publicDef_ "context_config_free" GC.InitDecl $ \s ->
            ([C.cedecl|void $id:s(struct $id:cfg* cfg);|],
             [C.cedecl|void $id:s(struct $id:cfg* cfg) {
                                 free(cfg);
                               }|])
          GC.publicDef_ "context_config_set_debugging" GC.InitDecl $ \s ->
             ([C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
              [C.cedecl|void $id:s(struct $id:cfg* cfg, int detail) {
                          cfg->debugging = detail;
                        }|])
          GC.publicDef_ "context_config_set_logging" GC.InitDecl $ \s ->
             ([C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
              [C.cedecl|void $id:s(struct $id:cfg* cfg, int detail) {
                                 /* Does nothing for this backend. */
                                 cfg = cfg; detail=detail;
                               }|])

          ctx <- GC.publicName "context"
          new_ctx <- GC.publicName "context_new"
          free_ctx <- GC.publicName "context_free"
          sync_ctx <- GC.publicName "context_sync"
          error_ctx <- GC.publicName "context_get_error"

          GC.headerDecl GC.InitDecl [C.cedecl|struct $id:ctx;|]
          GC.headerDecl GC.InitDecl [C.cedecl|struct $id:ctx* $id:new_ctx(struct $id:cfg* cfg);|]
          GC.headerDecl GC.InitDecl [C.cedecl|void $id:free_ctx(struct $id:ctx* ctx);|]
          GC.headerDecl GC.InitDecl [C.cedecl|int $id:sync_ctx(struct $id:ctx* ctx);|]
          GC.headerDecl GC.InitDecl [C.cedecl|char* $id:error_ctx(struct $id:ctx* ctx);|]

          (fields, init_fields) <- GC.contextContents

          GC.libDecl [C.cedecl|struct $id:ctx {
                                 int detail_memory;
                                 int debugging;
                                 typename lock_t lock;
                                 char *error;
                                 $sdecls:fields
                               };|]
          GC.libDecl [C.cedecl|struct $id:ctx* $id:new_ctx(struct $id:cfg* cfg) {
                                  struct $id:ctx* ctx = malloc(sizeof(struct $id:ctx));
                                  if (ctx == NULL) {
                                    return NULL;
                                  }
                                  ctx->detail_memory = cfg->debugging;
                                  ctx->debugging = cfg->debugging;
                                  ctx->error = NULL;
                                  create_lock(&ctx->lock);
                                  $stms:init_fields
                                  return ctx;
                               }|]
          GC.libDecl [C.cedecl|void $id:free_ctx(struct $id:ctx* ctx) {
                                 free_lock(&ctx->lock);
                                 free(ctx);
                               }|]
          GC.libDecl [C.cedecl|int $id:sync_ctx(struct $id:ctx* ctx) {
                                 ctx=ctx;
                                 return 0;
                               }|]
          GC.libDecl [C.cedecl|char* $id:error_ctx(struct $id:ctx* ctx) {
                                 char* error = ctx->error;
                                 ctx->error = NULL;
                                 return error;
                               }|]


copySequentialMemory :: GC.Copy Imp.Sequential ()
copySequentialMemory destmem destidx DefaultSpace srcmem srcidx DefaultSpace nbytes =
  GC.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes
copySequentialMemory _ _ destspace _ _ srcspace _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace
