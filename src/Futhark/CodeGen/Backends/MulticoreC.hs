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

import Data.Maybe
import Data.FileEmbed

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C
import Futhark.Representation.MCMem (Prog, MCMem)
import Futhark.CodeGen.ImpCode.Multicore
import qualified Futhark.CodeGen.ImpGen.Multicore as ImpGen
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.MonadFreshNames
import Futhark.CodeGen.Backends.SimpleRepresentation

compileProg :: MonadFreshNames m => Prog MCMem
            -> m GC.CParts
compileProg =
  GC.compileProg operations generateContext "" [DefaultSpace] cliOptions <=<
  ImpGen.compileProg
  where generateContext = do

          let subtask_queue_h  = $(embedStringFile "rts/c/subtask_queue.h")
              scheduler_h = $(embedStringFile "rts/c/scheduler.h")

          mapM_ GC.earlyDecl [C.cunit|
                              $esc:subtask_queue_h
                              $esc:scheduler_h
                             |]

          cfg <- GC.publicDef "context_config" GC.InitDecl $ \s ->
            ([C.cedecl|struct $id:s;|],
             [C.cedecl|struct $id:s { int debugging; int profiling; };|])

          GC.publicDef_ "context_config_new" GC.InitDecl $ \s ->
            ([C.cedecl|struct $id:cfg* $id:s(void);|],
             [C.cedecl|struct $id:cfg* $id:s(void) {
                                 struct $id:cfg *cfg = (struct $id:cfg*) malloc(sizeof(struct $id:cfg));
                                 if (cfg == NULL) {
                                   return NULL;
                                 }
                                 cfg->debugging = 0;
                                 cfg->profiling = 0;
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

          GC.publicDef_ "context_config_set_profiling" GC.InitDecl $ \s ->
             ([C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
              [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag) {
                          cfg->profiling = flag;
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
                          struct scheduler scheduler;
                          int detail_memory;
                          int debugging;
                          int profiling;
                          int profiling_paused;
                          typename lock_t lock;
                          char *error;
                          int total_runs;
                          long int total_runtime;
                          typename pthread_mutex_t profile_mutex;
                          $sdecls:fields
                        };|])

          GC.publicDef_ "context_new" GC.InitDecl $ \s ->
            ([C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg);|],
             [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg) {
                 struct $id:ctx* ctx = (struct $id:ctx*) malloc(sizeof(struct $id:ctx));
                 if (ctx == NULL) {
                   return NULL;
                 }

                 // Initialize rand()
                 srand(time(0));
                 ctx->detail_memory = cfg->debugging;
                 ctx->debugging = cfg->debugging;
                 ctx->profiling = cfg->profiling;
                 ctx->profiling_paused = 0;
                 ctx->error = NULL;
                 create_lock(&ctx->lock);
                 ctx->scheduler.num_threads = num_processors();
                 if (ctx->scheduler.num_threads < 1) return NULL;

                 $stms:init_fields

                 ctx->scheduler.workers = calloc(ctx->scheduler.num_threads, sizeof(struct worker));
                 if (ctx->scheduler.workers == NULL) return NULL;

                 for (int i = 0; i < ctx->scheduler.num_threads; i++) {
                   struct worker *cur_worker = &ctx->scheduler.workers[i];
                   cur_worker->tid = i;
                   cur_worker->time_spent_working = 0;
                   cur_worker->cur_working = 0;
                   cur_worker->scheduler = &ctx->scheduler;
                   CHECK_ERR(subtask_queue_init(&cur_worker->q, 32),
                             "failed to init jobqueue for worker %d\n", i);
                   CHECK_ERR(pthread_create(&cur_worker->thread, NULL, &scheduler_worker,
                                            cur_worker),
                             "Failed to create worker %d\n", i);
                 }
                 CHECK_ERR(pthread_mutex_init(&ctx->profile_mutex, NULL), "pthred_mutex_init");

                 init_constants(ctx);

                 return ctx;
              }|])

          GC.publicDef_ "context_free" GC.InitDecl $ \s ->
            ([C.cedecl|void $id:s(struct $id:ctx* ctx);|],
             [C.cedecl|void $id:s(struct $id:ctx* ctx) {
                 free_constants(ctx);
                 for (int i = 0; i < ctx->scheduler.num_threads; i++) {
                   CHECK_ERR(subtask_queue_destroy(&ctx->scheduler.workers[i].q), "subtask_queue_destroy");
                   CHECK_ERR(pthread_join(ctx->scheduler.workers[i].thread, NULL), "pthread_join");
                 }
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
                         ctx->profiling_paused = 1;
                       }|])

          GC.publicDef_ "context_unpause_profiling" GC.InitDecl $ \s ->
            ([C.cedecl|void $id:s(struct $id:ctx* ctx);|],
             [C.cedecl|void $id:s(struct $id:ctx* ctx) {
                         ctx->profiling_paused = 0;
                       }|])

          GC.publicDef_ "context_get_num_threads" GC.InitDecl $ \s ->
            ([C.cedecl|int $id:s(struct $id:ctx* ctx);|],
             [C.cedecl|int $id:s(struct $id:ctx* ctx) {
                        return ctx->scheduler.num_threads;
                       }|])


cliOptions :: [Option]
cliOptions =
  [ Option { optionLongName = "profile"
           , optionShortName = Just 'P'
           , optionArgument = NoArgument
           , optionAction = [C.cstm|futhark_context_config_set_profiling(cfg, 1);|]
           }
  ]

operations :: GC.Operations Multicore ()
operations = GC.defaultOperations
             { GC.opsCompiler = compileOp
             , GC.opsCopy = copyMulticoreMemory
             }

copyMulticoreMemory :: GC.Copy Multicore ()
copyMulticoreMemory destmem destidx DefaultSpace srcmem srcidx DefaultSpace nbytes =
  GC.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes
copyMulticoreMemory _ _ destspace _ _ srcspace _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

closureStructField :: VName -> Name
closureStructField v =
  nameFromString "free_" <> nameFromString (pretty v)

compileStructFields :: [VName] -> [(C.Type, ValueType)] -> [C.FieldGroup]
compileStructFields = zipWith field
  where
    field name (ty, Prim) =
      [C.csdecl|$ty:ty $id:(closureStructField name);|]
    field name (_, MemBlock) =
      [C.csdecl|$ty:(defaultMemBlockType) $id:(closureStructField name);|]
    field name (ty, Other) =
      [C.csdecl|$ty:ty *$id:(closureStructField name);|]

compileSetStructValues :: C.ToIdent a =>
                          a -> [VName] -> [(C.Type, ValueType)] -> [C.Stm]
compileSetStructValues struct = zipWith field
  where
    field name (_, Prim) =
      [C.cstm|$id:struct.$id:(closureStructField name)=$id:name;|]
    field name (_, MemBlock) =
      [C.cstm|$id:struct.$id:(closureStructField name)=$id:name.mem;|]
    field name (_, Other) =
      [C.cstm|$id:struct.$id:(closureStructField name)=&$id:name;|]



compileGetStructVals :: C.ToIdent a =>
                        a -> [VName] -> [(C.Type, ValueType)] -> [C.InitGroup]
compileGetStructVals struct = zipWith field
  where
    field name (ty, Prim) =
      [C.cdecl|$ty:ty $id:name = $id:struct->$id:(closureStructField name);|]
    field name (ty, MemBlock) =
      let name' = baseString name ++ "_" ++ show (baseTag name)
      in [C.cdecl|$ty:ty $id:name = {.desc = $string:name',
                                     .mem = $id:struct->$id:(closureStructField name),
                                     .size = 0, .references = NULL};|]
    field name (ty, Other) =
      [C.cdecl|$ty:ty $id:name = *$id:struct->$id:(closureStructField name);|]



compileSchedulingVal :: Scheduling -> GC.CompilerM op s C.Exp
compileSchedulingVal Static =  return [C.cexp|0|]
compileSchedulingVal (Dynamic granularity) =  return [C.cexp|$exp:granularity|]

paramToCType :: Param -> GC.CompilerM op s (C.Type, ValueType)
paramToCType (ScalarParam _ pt)     = do
  let t = GC.primTypeToCType pt
  return (t, Prim)
paramToCType (MemParam name space') = mcMemToCType name space'

mcMemToCType :: VName -> Space -> GC.CompilerM op s (C.Type, ValueType)
mcMemToCType v space = do
  refcount <- GC.fatMemory space
  cached <- isJust <$> GC.cacheMem v
  if refcount && not cached
     then return (GC.fatMemType space, MemBlock)
     else do t <- GC.rawMemCType space
             return (t, Other)


functionRuntime :: String -> String
functionRuntime = (++"_total_runtime")

functionRuns :: String -> String
functionRuns = (++"_runs")

functionIter :: String -> String
functionIter = (++"_iter")


multiCoreReport :: [(String, Bool)] -> [C.BlockItem]
multiCoreReport names = report_kernels
  where longest_name = foldl max 0 $ map (length . fst) names
        report_kernels = concatMap reportKernel names
        format_string name True =
          let padding = replicate (longest_name - length name) ' '
          in unwords ["tid %2d -", name ++ padding, "ran %7d times; avg: %10ldus; total: %10ldus; iterations %9ld; avg %ld\n"]
        format_string name False =
          let padding = replicate (longest_name - length name) ' '
          in unwords ["        ", name ++ padding, "ran %7d times; avg: %10ldus; total: %10ldus; iterations %9ld; avg %ld\n"]
        reportKernel (name, is_array) =
          let runs = functionRuns name
              total_runtime = functionRuntime name
              iters = functionIter name
          in if is_array then
                   [[C.citem|
                     for (int i = 0; i < ctx->scheduler.num_threads; i++) {
                       fprintf(stderr,
                         $string:(format_string name is_array),
                         i,
                         ctx->$id:runs[i],
                         (long int) ctx->$id:total_runtime[i] / (ctx->$id:runs[i] != 0 ? ctx->$id:runs[i] : 1),
                         (long int) ctx->$id:total_runtime[i],
                         (long int) (ctx->$id:iters[i]),
                         (long int) (ctx->$id:iters[i]) / (ctx->$id:runs[i] != 0 ? ctx->$id:runs[i] : 1)
                         );
                     }
                   |]]


             else [[C.citem|
                    fprintf(stderr,
                       $string:(format_string name is_array),
                       ctx->$id:runs,
                       (long int) ctx->$id:total_runtime / (ctx->$id:runs != 0 ? ctx->$id:runs : 1),
                       (long int) ctx->$id:total_runtime,
                       (long int) (ctx->$id:iters),
                       (long int) (ctx->$id:iters) / (ctx->$id:runs != 0 ? ctx->$id:runs : 1));
                   |],
                   [C.citem|ctx->total_runtime += ctx->$id:total_runtime;|],
                   [C.citem|ctx->total_runs += ctx->$id:runs;|]]


addBenchmarkFields :: String -> Maybe VName -> GC.CompilerM op s ()
addBenchmarkFields name (Just _) = do
  GC.contextField (functionRuntime name) [C.cty|typename int64_t*|] $ Just [C.cexp|calloc(sizeof(typename int64_t), ctx->scheduler.num_threads)|]
  GC.contextField (functionRuns name) [C.cty|int*|] $ Just [C.cexp|calloc(sizeof(int), ctx->scheduler.num_threads)|]
  GC.contextField (functionIter name) [C.cty|typename int64_t*|] $ Just [C.cexp|calloc(sizeof(sizeof(typename int64_t)), ctx->scheduler.num_threads)|]
addBenchmarkFields name Nothing = do
  GC.contextField (functionRuntime name) [C.cty|typename int64_t|] $ Just [C.cexp|0|]
  GC.contextField (functionRuns name) [C.cty|int|] $ Just [C.cexp|0|]
  GC.contextField (functionIter name) [C.cty|typename int64_t|] $ Just [C.cexp|0|]


benchmarkCode :: String -> Maybe VName -> [C.BlockItem] -> GC.CompilerM op s [C.BlockItem]
benchmarkCode name tid code = do
  addBenchmarkFields name tid
  return [C.citems|
     typename int64_t $id:start;
     if (ctx->profiling && !ctx->profiling_paused) {
       $id:start = get_wall_time();
     }
     $items:code

     if (ctx->profiling && !ctx->profiling_paused) {
       typename int64_t $id:end = get_wall_time();
       typename uint64_t elapsed = $id:end - $id:start;
       $items:(updateFields tid)
     }
     |]

  where start = name ++ "_start"
        end = name ++ "_end"
        -- This case should be mutex protected
        updateFields Nothing    = [C.citems|ctx->$id:(functionRuns name)++;
                                            ctx->$id:(functionRuntime name) += elapsed;
                                            ctx->$id:(functionIter name) += $id:name.iterations;|]
        updateFields (Just tid') = [C.citems|ctx->$id:(functionRuns name)[$id:tid']++;
                                            ctx->$id:(functionRuntime name)[$id:tid'] += elapsed;
                                            ctx->$id:(functionIter name)[$id:tid'] += (end-start);|]


multicoreName :: String -> GC.CompilerM op s String
multicoreName s = do
  s' <- newVName ("futhark_mc_" ++ s)
  return $ baseString s' ++ "_" ++ show (baseTag s')

multicoreDef :: String -> (String -> GC.CompilerM op s C.Definition) -> GC.CompilerM op s String
multicoreDef s f = do
  s' <- multicoreName s
  GC.libDecl =<< f s'
  return s'

compileOp :: GC.OpCompiler Multicore ()
compileOp (ParLoop scheduling ntasks i e (MulticoreFunc params prebody body tid)) = do
  fctypes <- mapM paramToCType params
  let fargs = map paramName params
  e' <- GC.compileExp e
  granularity <- compileSchedulingVal scheduling

  let lexical = lexicalMemoryUsage $
                Function False [] params body [] []

  fstruct <- multicoreDef "parloop_struct" $ \s ->
     return [C.cedecl|struct $id:s {
                        struct futhark_context *ctx;
                        $sdecls:(compileStructFields fargs fctypes)
                      };|]

  ftask <- multicoreDef "parloop" $ \s -> do

    fbody <- benchmarkCode s (Just tid) <=<
             GC.inNewFunction True $ GC.cachingMemory lexical $
             \decl_cached free_cached -> GC.blockScope $ do
      mapM_ GC.item
        [C.citems|$decls:(compileGetStructVals fstruct fargs fctypes)|]

      mapM_ GC.item decl_cached

      GC.decl [C.cdecl|int $id:i = start;|]
      GC.compileCode prebody
      body' <- GC.blockScope $ GC.compileCode body
      GC.stm [C.cstm|for (; $id:i < end; $id:i++) {
                       $items:body'
                     }|]
      GC.stm [C.cstm|cleanup: {}|]
      mapM_ GC.stm free_cached

    return [C.cedecl|int $id:s(void *args, int start, int end, int $id:tid) {
                       int err = 0;
                       struct $id:fstruct *$id:fstruct = (struct $id:fstruct*) args;
                       struct futhark_context *ctx = $id:fstruct->ctx;
                       $items:fbody
                       return err;
                     }|]

  GC.decl [C.cdecl|struct $id:fstruct $id:fstruct;|]
  GC.stm [C.cstm|$id:fstruct.ctx = ctx;|]
  GC.stms [C.cstms|$stms:(compileSetStructValues fstruct fargs fctypes)|]

  let ftask_name = ftask ++ "_task"
  GC.decl [C.cdecl|struct scheduler_task $id:ftask_name;|]
  GC.stm  [C.cstm|$id:ftask_name.name = $string:ftask;|]
  GC.stm  [C.cstm|$id:ftask_name.fn = $id:ftask;|]
  GC.stm  [C.cstm|$id:ftask_name.args = &$id:fstruct;|]
  GC.stm  [C.cstm|$id:ftask_name.iterations = $exp:e';|]
  GC.stm  [C.cstm|$id:ftask_name.granularity = $exp:granularity;|]

  let ftask_err = ftask ++ "_err"
  code' <- benchmarkCode ftask_name Nothing [C.citems|int $id:ftask_err = scheduler_do_task(&ctx->scheduler, &$id:ftask_name, &$id:ntasks);
                                              if ($id:ftask_err != 0) {
                                                futhark_panic($id:ftask_err, futhark_context_get_error(ctx));
                                              }|]

  mapM_ GC.item code'
  mapM_ GC.profileReport $ multiCoreReport $ zip [ftask, ftask_name] [True, False]


compileOp (MulticoreCall Nothing f) =
  GC.stm [C.cstm|$id:f(ctx);|]

compileOp (MulticoreCall (Just retval) f) =
  GC.stm [C.cstm|$id:retval = $id:f(ctx);|]
