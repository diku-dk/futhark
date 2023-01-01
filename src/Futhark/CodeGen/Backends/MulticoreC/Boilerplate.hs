{-# LANGUAGE QuasiQuotes #-}

-- | Boilerplate for multicore C code.
module Futhark.CodeGen.Backends.MulticoreC.Boilerplate (generateBoilerplate) where

import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.RTS.C (backendsMulticoreH, schedulerH)
import Language.C.Quote.OpenCL qualified as C

-- | Generate the necessary boilerplate.
generateBoilerplate :: GC.CompilerM op s ()
generateBoilerplate = do
  mapM_ GC.earlyDecl [C.cunit|$esc:(T.unpack schedulerH)|]
  mapM_ GC.earlyDecl [C.cunit|$esc:(T.unpack backendsMulticoreH)|]

  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_num_threads(struct futhark_context_config *cfg, int n);|]

  (fields, init_fields, free_fields) <- GC.contextContents

  ctx <- GC.publicDef "context" GC.InitDecl $ \s ->
    ( [C.cedecl|struct $id:s;|],
      [C.cedecl|struct $id:s {
                      struct futhark_context_config* cfg;
                      struct scheduler scheduler;
                      int detail_memory;
                      int debugging;
                      int profiling;
                      int profiling_paused;
                      int logging;
                      typename lock_t lock;
                      char *error;
                      typename lock_t error_lock;
                      typename FILE *log;
                      int total_runs;
                      long int total_runtime;
                      struct free_list free_list;
                      typename int64_t peak_mem_usage_default;
                      typename int64_t cur_mem_usage_default;
                      struct constants *constants;

                      typename int64_t tuning_timing;
                      typename int64_t tuning_iter;

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
             // Initialize rand()
             fast_srand(time(0));

             int tune_kappa = 0;
             double kappa = 5.1f * 1000;

             if (tune_kappa) {
               if (determine_kappa(&kappa) != 0) {
                 return NULL;
               }
             }

             if (scheduler_init(&ctx->scheduler,
                                cfg->num_threads > 0 ?
                                cfg->num_threads : num_processors(),
                                kappa) != 0) {
               return NULL;
             }
             $stms:init_fields
             context_setup(cfg, ctx);
             init_constants(ctx);
             return ctx;
          }|]
    )

  GC.publicDef_ "context_free" GC.InitDecl $ \s ->
    ( [C.cedecl|void $id:s(struct $id:ctx* ctx);|],
      [C.cedecl|void $id:s(struct $id:ctx* ctx) {
             $stms:free_fields
             context_teardown(ctx);
             (void)scheduler_destroy(&ctx->scheduler);
             free(ctx);
           }|]
    )

  GC.publicDef_ "context_sync" GC.InitDecl $ \s ->
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
