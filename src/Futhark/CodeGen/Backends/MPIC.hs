{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | C code generator.  This module can convert a correct ImpCode
-- program to an equivalent C program.
module Futhark.CodeGen.Backends.MPIC
  ( compileProg,
    GC.CParts (..),
    GC.asLibrary,
    GC.asExecutable,
  )
where

import Control.Monad
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.ImpCode.MPI
import qualified Futhark.CodeGen.ImpGen.MPI as ImpGen
import Futhark.IR.MCMem (MCMem, Prog)
import Futhark.MonadFreshNames
import qualified Language.C.Quote.C as C

--import qualified Language.C.Syntax as C

compileProg ::
  MonadFreshNames m =>
  Prog MCMem ->
  m (ImpGen.Warnings, GC.CParts)
compileProg =
  traverse
    ( GC.compileProg
        "mpi"
        operations
        generateContext
        mpi_includes
        [DefaultSpace]
        cliOptions
    )
    <=< ImpGen.compileProg
  where
    mpi_includes =
      unlines
        ["#include <mpi.h>"]
    generateContext = do
      cfg <- GC.publicDef "context_config" GC.InitDecl $ \s ->
        ( [C.cedecl|struct $id:s;|],
          [C.cedecl|struct $id:s { int debugging;
                                   int profiling;
                                 };|]
        )

      GC.publicDef_ "context_config_new" GC.InitDecl $ \s ->
        ( [C.cedecl|struct $id:cfg* $id:s(void);|],
          [C.cedecl|struct $id:cfg* $id:s(void) {
                                 struct $id:cfg *cfg = (struct $id:cfg*) malloc(sizeof(struct $id:cfg));
                                 if (cfg == NULL) {
                                   return NULL;
                                 }
                                 cfg->debugging = 0;
                                 cfg->profiling = 0;
                                 return cfg;
                               }|]
        )

      GC.publicDef_ "context_config_free" GC.InitDecl $ \s ->
        ( [C.cedecl|void $id:s(struct $id:cfg* cfg);|],
          [C.cedecl|void $id:s(struct $id:cfg* cfg) {
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
                          cfg->profiling = flag;
                        }|]
        )

      GC.publicDef_ "context_config_set_logging" GC.InitDecl $ \s ->
        ( [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
          [C.cedecl|void $id:s(struct $id:cfg* cfg, int detail) {
                                 /* Does nothing for this backend. */
                                 (void)cfg; (void)detail;
                               }|]
        )

      (fields, init_fields) <- GC.contextContents

      ctx <- GC.publicDef "context" GC.InitDecl $ \s ->
        ( [C.cedecl|struct $id:s;|],
          [C.cedecl|struct $id:s {
                          int detail_memory;
                          int debugging;
                          int profiling;
                          typename lock_t lock;
                          char *error;
                          int profiling_paused;
                          $sdecls:fields
                          int world_size;
                          int rank;
                        };|]
        )

      GC.publicDef_ "context_new" GC.InitDecl $ \s ->
        ( [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg);|],
          [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg) {
                 struct $id:ctx* ctx = (struct $id:ctx*) malloc(sizeof(struct $id:ctx));
                 if (ctx == NULL) {
                   return NULL;
                 }
                 ctx->detail_memory = cfg->debugging;
                 ctx->debugging = cfg->debugging;
                 ctx->profiling = cfg->profiling;
                 ctx->profiling_paused = 0;
                 ctx->error = NULL;
                 create_lock(&ctx->lock);
                 $stms:init_fields
                 init_constants(ctx);

                 MPI_Init (NULL, NULL);
                 MPI_Comm_size(MPI_COMM_WORLD, &(ctx->world_size));
                 MPI_Comm_rank(MPI_COMM_WORLD, &(ctx->rank));

                 return ctx;
              }|]
        )

      GC.publicDef_ "context_free" GC.InitDecl $ \s ->
        ( [C.cedecl|void $id:s(struct $id:ctx* ctx);|],
          [C.cedecl|void $id:s(struct $id:ctx* ctx) {
                 free_constants(ctx);
                 free_lock(&ctx->lock);
                 free(ctx);
                 MPI_Finalize();
               }|]
        )

      GC.publicDef_ "context_sync" GC.InitDecl $ \s ->
        ( [C.cedecl|int $id:s(struct $id:ctx* ctx);|],
          [C.cedecl|int $id:s(struct $id:ctx* ctx) {
                                 (void)ctx;
                                 return 0;
                               }|]
        )

cliOptions :: [Option]
cliOptions =
  [ Option
      { optionLongName = "profile",
        optionShortName = Just 'P',
        optionArgument = NoArgument,
        optionAction = [C.cstm|futhark_context_config_set_profiling(cfg, 1);|],
        optionDescription = "Gather profiling information."
      }
  ]

operations :: GC.Operations MPIOp ()
operations =
  GC.defaultOperations {GC.opsCompiler = compileOp}

compileOp :: GC.OpCompiler MPIOp ()
compileOp (CrashWithThisMessage s) = do
  GC.stm [C.cstm|fprintf(stderr, "%s\n", $string:s);|]
  GC.stm [C.cstm|exit(1);|]
