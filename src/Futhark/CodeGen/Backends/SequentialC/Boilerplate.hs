{-# LANGUAGE QuasiQuotes #-}

-- | Boilerplate for sequential C code.
module Futhark.CodeGen.Backends.SequentialC.Boilerplate (generateBoilerplate) where

import qualified Futhark.CodeGen.Backends.GenericC as GC
import qualified Language.C.Quote.OpenCL as C

-- | Generate the necessary boilerplate.
generateBoilerplate :: GC.CompilerM op s ()
generateBoilerplate = do
  cfg <- GC.publicDef "context_config" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:s;|],
      [C.cedecl|struct $id:s { int debugging; int in_use; };|]
    )

  GC.publicDef_ "context_config_new" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:cfg* $id:s(void);|],
      [C.cedecl|struct $id:cfg* $id:s(void) {
                                 struct $id:cfg *cfg = (struct $id:cfg*) malloc(sizeof(struct $id:cfg));
                                 if (cfg == NULL) {
                                   return NULL;
                                 }
                                 cfg->in_use = 0;
                                 cfg->debugging = 0;
                                 return cfg;
                               }|]
    )

  GC.publicDef_ "context_config_free" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg) {
                                 assert(!cfg->in_use);
                                 free(cfg);
                               }|]
    )

  GC.publicDef_ "context_config_set_debugging" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int detail) {
                          cfg->debugging = detail;
                        }|]
    )

  GC.publicDef_ "context_config_set_profiling" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag) {
                         (void)cfg; (void)flag;
                       }|]
    )

  GC.publicDef_ "context_config_set_logging" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
      [C.cedecl|void $id:s(struct $id:cfg* cfg, int detail) {
                                 // Does nothing for this backend.
                                 (void)cfg; (void)detail;
                               }|]
    )

  (fields, init_fields, free_fields) <- GC.contextContents

  ctx <- GC.publicDef "context" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:s;|],
      [C.cedecl|struct $id:s {
                          struct $id:cfg* cfg;
                          int detail_memory;
                          int debugging;
                          int profiling;
                          int logging;
                          typename lock_t lock;
                          char *error;
                          typename FILE *log;
                          int profiling_paused;
                          $sdecls:fields
                        };|]
    )

  GC.publicDef_ "context_new" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg);|],
      [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg) {
                                  assert(!cfg->in_use);
                                  struct $id:ctx* ctx = (struct $id:ctx*) malloc(sizeof(struct $id:ctx));
                                  if (ctx == NULL) {
                                    return NULL;
                                  }
                                  ctx->cfg = cfg;
                                  ctx->cfg->in_use = 1;

                                  ctx->detail_memory = cfg->debugging;
                                  ctx->debugging = cfg->debugging;
                                  ctx->profiling = cfg->debugging;
                                  ctx->logging = cfg->debugging;
                                  ctx->error = NULL;
                                  ctx->log = stderr;
                                  create_lock(&ctx->lock);
                                  $stms:init_fields
                                  init_constants(ctx);
                                  return ctx;
                               }|]
    )

  GC.publicDef_ "context_free" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:ctx* ctx);|],
      [C.cedecl|void $id:s(struct $id:ctx* ctx) {
                                 $stms:free_fields
                                 free_constants(ctx);
                                 free_lock(&ctx->lock);
                                 ctx->cfg->in_use = 0;
                                 free(ctx);
                               }|]
    )

  GC.publicDef_ "context_sync" GC.MiscDecl $ \s ->
    ( [C.cedecl|int $id:s(struct $id:ctx* ctx);|],
      [C.cedecl|int $id:s(struct $id:ctx* ctx) {
                                 (void)ctx;
                                 return 0;
                               }|]
    )

  GC.earlyDecl [C.cedecl|static const char *tuning_param_names[0];|]
  GC.earlyDecl [C.cedecl|static const char *tuning_param_vars[0];|]
  GC.earlyDecl [C.cedecl|static const char *tuning_param_classes[0];|]

  GC.publicDef_ "context_config_set_tuning_param" GC.InitDecl $ \s ->
    ( [C.cedecl|int $id:s(struct $id:cfg* cfg, const char *param_name, size_t param_value);|],
      [C.cedecl|int $id:s(struct $id:cfg* cfg, const char *param_name, size_t param_value) {
                         (void)cfg; (void)param_name; (void)param_value;
                         return 1;
                       }|]
    )
