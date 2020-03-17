{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | C code generator.  This module can convert a correct ImpCode
-- program to an equivalent C program.
module Futhark.CodeGen.Backends.MulticoreC
  ( compileProg
  , GC.CParts(..)
  , GC.asLibrary
  , GC.asExecutable
  ) where

import Control.Monad

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C

import Futhark.Error
import Futhark.Representation.ExplicitMemory (Prog, ExplicitMemory)
import Futhark.CodeGen.ImpCode.Multicore
import qualified Futhark.CodeGen.ImpGen.Multicore as ImpGen
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.MonadFreshNames
-- import Data.FileEmbed (embedStringFile)

compileProg :: MonadFreshNames m => Prog ExplicitMemory
            -> m (Either InternalError GC.CParts)
compileProg =
  traverse (GC.compileProg operations generateContext "" [DefaultSpace] []) <=<
  ImpGen.compileProg
  where operations :: GC.Operations Multicore ()
        operations = GC.defaultOperations
                     { GC.opsCompiler = compileOp
                     , GC.opsCopy = copyMulticoreMemory
                     }

        generateContext = do

          cfg <- GC.publicDef "context_config" GC.InitDecl $ \s ->
            ([C.cedecl|struct $id:s;|],
             [C.cedecl|struct $id:s { int debugging; };|])

          GC.publicDef_ "context_config_new" GC.InitDecl $ \s ->
            ([C.cedecl|struct $id:cfg* $id:s(void);|],
             [C.cedecl|struct $id:cfg* $id:s(void) {
                                 struct $id:cfg *cfg = (struct $id:cfg*) malloc(sizeof(struct $id:cfg));
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
                                 (void)cfg; (void)detail;
                               }|])

          (fields, init_fields) <- GC.contextContents

          ctx <- GC.publicDef "context" GC.InitDecl $ \s ->
            ([C.cedecl|struct $id:s;|],
             [C.cedecl|struct $id:s {
                          struct job_queue q;
                          int detail_memory;
                          int debugging;
                          int profiling;
                          typename lock_t lock;
                          char *error;
                          $sdecls:fields
                        };|])

          GC.publicDef_ "context_new" GC.InitDecl $ \s ->
            ([C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg);|],
             [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg) {
                                  struct $id:ctx* ctx = (struct $id:ctx*) malloc(sizeof(struct $id:ctx));
                                  if (ctx == NULL) {
                                    return NULL;
                                  }
                                  if (job_queue_init(&ctx->q, 64)) return NULL;
                                  ctx->detail_memory = cfg->debugging;
                                  ctx->debugging = cfg->debugging;
                                  ctx->error = NULL;
                                  create_lock(&ctx->lock);
                                  $stms:init_fields
                                  return ctx;
                               }|])

          GC.publicDef_ "context_free" GC.InitDecl $ \s ->
            ([C.cedecl|void $id:s(struct $id:ctx* ctx);|],
             [C.cedecl|void $id:s(struct $id:ctx* ctx) {
                                 free_lock(&ctx->lock);
                                 free(ctx);
                               }|])

          GC.publicDef_ "context_sync" GC.InitDecl $ \s ->
            ([C.cedecl|int $id:s(struct $id:ctx* ctx);|],
             [C.cedecl|int $id:s(struct $id:ctx* ctx) {
                                 (void)ctx;
                                 return 0;
                               }|])
          GC.publicDef_ "context_get_error" GC.InitDecl $ \s ->
            ([C.cedecl|char* $id:s(struct $id:ctx* ctx);|],
             [C.cedecl|char* $id:s(struct $id:ctx* ctx) {
                                 char* error = ctx->error;
                                 ctx->error = NULL;
                                 return error;
                               }|])

          GC.publicDef_ "context_pause_profiling" GC.InitDecl $ \s ->
            ([C.cedecl|void $id:s(struct $id:ctx* ctx);|],
             [C.cedecl|void $id:s(struct $id:ctx* ctx) {
                         (void)ctx;
                       }|])

          GC.publicDef_ "context_unpause_profiling" GC.InitDecl $ \s ->
            ([C.cedecl|void $id:s(struct $id:ctx* ctx);|],
             [C.cedecl|void $id:s(struct $id:ctx* ctx) {
                         (void)ctx;
                       }|])

          GC.publicDef_ "context_get_jobqueue" GC.InitDecl $ \s ->
            ([C.cedecl|struct job_queue *$id:s(struct $id:ctx* ctx);|],
             [C.cedecl|struct job_queue *$id:s(struct $id:ctx* ctx) {
                         return &ctx->q;
                       }|])

          GC.publicDef_ "context_init_jobqueue" GC.InitDecl $ \s ->
            ([C.cedecl|int $id:s(struct $id:ctx* ctx);|],
             [C.cedecl|int $id:s(struct $id:ctx* ctx) {
                         return job_queue_init(&ctx->q, 64);
                       }|])

          GC.publicDef_ "context_kill_jobqueue" GC.InitDecl $ \s ->
            ([C.cedecl|int $id:s(struct $id:ctx* ctx);|],
             [C.cedecl|int $id:s(struct $id:ctx* ctx) {
                         return job_queue_destroy(&ctx->q);
                       }|])






copyMulticoreMemory :: GC.Copy Multicore ()
copyMulticoreMemory destmem destidx DefaultSpace srcmem srcidx DefaultSpace nbytes =
  GC.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes
copyMulticoreMemory _ _ destspace _ _ srcspace _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace


compileStructFields :: C.ToIdent a =>
                       [a] -> [C.Type] -> [C.FieldGroup]
compileStructFields  fargs fctypes =
  [ [C.csdecl|$ty:ty *$id:name;|]| (name, ty) <- zip fargs fctypes]


-- | Sets the field values of the struct
--   Assummes that vnames are fields of the struct
compileSetStructValues :: (C.ToIdent a1, C.ToIdent a2) =>
                           a1 -> [a2] -> [C.Stm]
compileSetStructValues struct vnames =
  [ [C.cstm|$id:struct->$id:name=&$id:name;|] | name <- vnames]


compileGetStructVals :: (C.ToIdent a1, C.ToIdent a2) =>
                         a2 -> [a1] -> [C.Type] -> [C.InitGroup]
compileGetStructVals struct fargs fctypes =
  [ [C.cdecl|$ty:ty $id:name = *$id:struct->$id:name;|]
            | (name, ty) <- zip fargs fctypes ]


compileSetStructVals :: (C.ToIdent a1, C.ToIdent a2) =>
                         a1 -> [a2] -> [C.Stm]
compileSetStructVals struct vals =
  [ [C.cstm|*$id:struct->$id:name=$id:name;|]
           | name <- vals ]

getCType :: Type -> C.Type
getCType t = case t of
               Scalar pt  -> GC.primTypeToCType pt
               Mem space' -> GC.fatMemType space'

compileOp :: GC.OpCompiler Multicore ()
compileOp (ParLoop i e (MulticoreFunc fargs ftypes body)) = do
  let fctypes = map getCType ftypes
  e' <- GC.compileExp e
  body' <- GC.blockScope $ GC.compileCode body

  fstruct <- GC.publicMulticoreDef "parloop_struct" GC.MiscDecl $ \s ->
    ([C.cedecl|struct $id:s;|],
     [C.cedecl|struct $id:s {
             $sdecls:(compileStructFields fargs fctypes)
           };|])

  -- Maybe I should drop this and just use the task function
  fbody <- GC.publicMulticoreDef "parloop_body" GC.MiscDecl $ \s ->
   ([C.cedecl|int $id:s(struct $id:fstruct *$id:fstruct, int $id:i);|],
    [C.cedecl|int $id:s(struct $id:fstruct *$id:fstruct, int $id:i) {
            $decls:(compileGetStructVals fstruct fargs fctypes)
            $items:body'
            return 0;
   }|])



  -- A task function that a thread should execute
  ftask <- GC.publicMulticoreDef "parloop_task" GC.MiscDecl $ \s ->
   ([C.cedecl|int $id:s(void *args, int start, int end);|],
    [C.cedecl|int $id:s(void *args, int start, int end) {
              for (int i = start; i < end; i++) {
                   $id:fbody((struct $id:fstruct*) args, i);
              }
              return 0;
   }|])

  -- Declare and set values needed by function body
  GC.decl [C.cdecl|struct $id:fstruct *$id:fstruct = malloc(sizeof(struct $id:fstruct));|]
  GC.stms [C.cstms|$stms:(compileSetStructValues fstruct fargs)|]

  GC.stm [C.cstm|scheduler_do_task(ctx, $id:ftask, $id:fstruct, $exp:e');|]

  GC.stm [C.cstm|free($id:fstruct);|]


compileOp (ParLoopAcc i e (MulticoreFunc fargs ftypes body)) = do
  let fctypes = map getCType ftypes
  e' <- GC.compileExp e
  body' <- GC.blockScope $ GC.compileCode body

  struct <- GC.publicMulticoreDef "parloop_struct" GC.MiscDecl $ \s ->
    ([C.cedecl|struct $id:s;|],
     [C.cedecl|struct $id:s {
                 $sdecls:(compileStructFields fargs fctypes)
               };|])

  f <- GC.publicMulticoreDef "parloop" GC.MiscDecl $ \s ->
   ([C.cedecl|int $id:s(struct $id:struct *$id:struct, int $id:i);|],
    [C.cedecl|int $id:s(struct $id:struct *$id:struct, int $id:i) {
            $decls:(compileGetStructVals  struct fargs fctypes)
            $items:body'
            $stms:(compileSetStructVals struct fargs)
            return 0;
  }|])

  -- Declare and set values
  GC.decl [C.cdecl|struct $id:struct $id:struct;|]
  GC.stms [C.cstms|$stms:(compileSetStructValues struct fargs)|]


  GC.stms [[C.cstm|for (int $id:i = 0; $id:i < $exp:e'; $id:i++) {
                   int retval = $id:f(&$id:struct, $id:i);
                 }|]]
