{-# LANGUAGE QuasiQuotes #-}

-- | Boilerplate for sequential C code.
module Futhark.CodeGen.Backends.SequentialC.Boilerplate (generateBoilerplate) where

import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.RTS.C (backendsCH)
import Language.C.Quote.OpenCL qualified as C

-- | Generate the necessary boilerplate.
generateBoilerplate :: GC.CompilerM op s ()
generateBoilerplate = do
  (fields, init_fields, free_fields) <- GC.contextContents

  GC.earlyDecl [C.cedecl|$esc:(T.unpack backendsCH)|]

  ctx <- GC.publicDef "context" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:s;|],
      [C.cedecl|struct $id:s {
                          struct futhark_context_config* cfg;
                          int detail_memory;
                          int debugging;
                          int profiling;
                          int logging;
                          typename lock_t lock;
                          char *error;
                          typename lock_t error_lock;
                          typename FILE *log;
                          int profiling_paused;
                          struct free_list free_list;

                          $sdecls:fields
                        };|]
    )

  GC.publicDef_ "context_new" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:ctx* $id:s(struct futhark_context_config* cfg);|],
      [C.cedecl|struct $id:ctx* $id:s(struct futhark_context_config* cfg) {
                                  struct $id:ctx* ctx = (struct $id:ctx*) malloc(sizeof(struct $id:ctx));
                                  if (ctx == NULL) {
                                    return NULL;
                                  }
                                  context_setup(cfg, ctx);
                                  $stms:init_fields
                                  init_constants(ctx);
                                  return ctx;
                               }|]
    )

  GC.publicDef_ "context_free" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:ctx* ctx);|],
      [C.cedecl|void $id:s(struct $id:ctx* ctx) {
                                 $stms:free_fields
                                 context_teardown(ctx);
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

  GC.earlyDecl [C.cedecl|static const int num_tuning_params = 0;|]
  GC.earlyDecl [C.cedecl|static const char *tuning_param_names[1];|]
  GC.earlyDecl [C.cedecl|static const char *tuning_param_vars[1];|]
  GC.earlyDecl [C.cedecl|static const char *tuning_param_classes[1];|]
  GC.earlyDecl [C.cedecl|static typename int64_t *tuning_param_defaults[1];|]

  GC.publicDef_ "context_config_set_tuning_param" GC.InitDecl $ \s ->
    ( [C.cedecl|int $id:s(struct futhark_context_config* cfg, const char *param_name, size_t param_value);|],
      [C.cedecl|int $id:s(struct futhark_context_config* cfg, const char *param_name, size_t param_value) {
                         (void)cfg; (void)param_name; (void)param_value;
                         return 1;
                       }|]
    )
{-# NOINLINE generateBoilerplate #-}
