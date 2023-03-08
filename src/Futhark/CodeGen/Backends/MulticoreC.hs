{-# LANGUAGE QuasiQuotes #-}

-- | C code generator.  This module can convert a correct ImpCode
-- program to an equivalent C program.
module Futhark.CodeGen.Backends.MulticoreC
  ( compileProg,
    GC.CParts (..),
    GC.asLibrary,
    GC.asExecutable,
    GC.asServer,
    operations,
    cliOptions,
    compileOp,
    ValueType (..),
    paramToCType,
    prepareTaskStruct,
    closureFreeStructField,
    generateParLoopFn,
    addTimingFields,
    functionTiming,
    functionIterations,
    multiCoreReport,
    multicoreDef,
    multicoreName,
    DefSpecifier,
    atomicOps,
  )
where

import Control.Monad
import Data.Loc
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.MulticoreC.Boilerplate (generateBoilerplate)
import Futhark.CodeGen.Backends.SimpleRep
import Futhark.CodeGen.ImpCode.Multicore hiding (ValueType)
import Futhark.CodeGen.ImpGen.Multicore qualified as ImpGen
import Futhark.IR.MCMem (MCMem, Prog)
import Futhark.MonadFreshNames
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C

-- | Compile the program to ImpCode with multicore operations.
compileProg ::
  MonadFreshNames m => T.Text -> Prog MCMem -> m (ImpGen.Warnings, GC.CParts)
compileProg version =
  traverse
    ( GC.compileProg
        "multicore"
        version
        mempty
        operations
        generateBoilerplate
        ""
        (DefaultSpace, [DefaultSpace])
        cliOptions
    )
    <=< ImpGen.compileProg

-- | Multicore-related command line options.
cliOptions :: [Option]
cliOptions =
  [ Option
      { optionLongName = "profile",
        optionShortName = Just 'P',
        optionArgument = NoArgument,
        optionAction = [C.cstm|futhark_context_config_set_profiling(cfg, 1);|],
        optionDescription = "Gather profiling information."
      },
    Option
      { optionLongName = "num-threads",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "INT",
        optionAction = [C.cstm|futhark_context_config_set_num_threads(cfg, atoi(optarg));|],
        optionDescription = "Set number of threads used for execution."
      }
  ]

-- | Operations for generating multicore code.
operations :: GC.Operations Multicore s
operations =
  GC.defaultOperations
    { GC.opsCompiler = compileOp,
      GC.opsCritical =
        -- The thread entering an API function is always considered
        -- the "first worker" - note that this might differ from the
        -- thread that created the context!  This likely only matters
        -- for entry points, since they are the only API functions
        -- that contain parallel operations.
        ( [C.citems|worker_local = &ctx->scheduler.workers[0];|],
          []
        )
    }

closureFreeStructField :: VName -> Name
closureFreeStructField v =
  nameFromString "free_" <> nameFromString (prettyString v)

closureRetvalStructField :: VName -> Name
closureRetvalStructField v =
  nameFromString "retval_" <> nameFromString (prettyString v)

data ValueType = Prim PrimType | MemBlock | RawMem

compileFreeStructFields :: [VName] -> [(C.Type, ValueType)] -> [C.FieldGroup]
compileFreeStructFields = zipWith field
  where
    field name (ty, Prim _) =
      [C.csdecl|$ty:ty $id:(closureFreeStructField name);|]
    field name (_, _) =
      [C.csdecl|$ty:defaultMemBlockType $id:(closureFreeStructField name);|]

compileRetvalStructFields :: [VName] -> [(C.Type, ValueType)] -> [C.FieldGroup]
compileRetvalStructFields = zipWith field
  where
    field name (ty, Prim _) =
      [C.csdecl|$ty:ty *$id:(closureRetvalStructField name);|]
    field name (_, _) =
      [C.csdecl|$ty:defaultMemBlockType $id:(closureRetvalStructField name);|]

compileSetStructValues ::
  C.ToIdent a =>
  a ->
  [VName] ->
  [(C.Type, ValueType)] ->
  [C.Stm]
compileSetStructValues struct = zipWith field
  where
    field name (_, Prim pt) =
      [C.cstm|$id:struct.$id:(closureFreeStructField name)=$exp:(toStorage pt (C.toExp name noLoc));|]
    field name (_, MemBlock) =
      [C.cstm|$id:struct.$id:(closureFreeStructField name)=$id:name.mem;|]
    field name (_, RawMem) =
      [C.cstm|$id:struct.$id:(closureFreeStructField name)=$id:name;|]

compileSetRetvalStructValues ::
  C.ToIdent a =>
  a ->
  [VName] ->
  [(C.Type, ValueType)] ->
  [C.Stm]
compileSetRetvalStructValues struct vnames we = concat $ zipWith field vnames we
  where
    field name (ct, Prim _) =
      [C.cstms|$id:struct.$id:(closureRetvalStructField name)=(($ty:ct*)&$id:name);
               $escstm:("#if ISPC")
               $id:struct.$id:(closureRetvalStructField name)+= programIndex;
               $escstm:("#endif")|]
    field name (_, MemBlock) =
      [C.cstms|$id:struct.$id:(closureRetvalStructField name)=$id:name.mem;|]
    field name (_, RawMem) =
      [C.cstms|$id:struct.$id:(closureRetvalStructField name)=$id:name;|]

compileGetRetvalStructVals :: C.ToIdent a => a -> [VName] -> [(C.Type, ValueType)] -> [C.InitGroup]
compileGetRetvalStructVals struct = zipWith field
  where
    field name (ty, Prim pt) =
      let inner = [C.cexp|*$id:struct->$id:(closureRetvalStructField name)|]
       in [C.cdecl|$ty:ty $id:name = $exp:(fromStorage pt inner);|]
    field name (ty, _) =
      [C.cdecl|$ty:ty $id:name =
                 {.desc = $string:(prettyString name),
                 .mem = $id:struct->$id:(closureRetvalStructField name),
                 .size = 0, .references = NULL};|]

compileGetStructVals ::
  C.ToIdent a =>
  a ->
  [VName] ->
  [(C.Type, ValueType)] ->
  [C.InitGroup]
compileGetStructVals struct = zipWith field
  where
    field name (ty, Prim pt) =
      let inner = [C.cexp|$id:struct->$id:(closureFreeStructField name)|]
       in [C.cdecl|$ty:ty $id:name = $exp:(fromStorage pt inner);|]
    field name (ty, _) =
      [C.cdecl|$ty:ty $id:name =
                 {.desc = $string:(prettyString name),
                  .mem = $id:struct->$id:(closureFreeStructField name),
                  .size = 0, .references = NULL};|]

compileWriteBackResVals :: C.ToIdent a => a -> [VName] -> [(C.Type, ValueType)] -> [C.Stm]
compileWriteBackResVals struct = zipWith field
  where
    field name (_, Prim pt) =
      [C.cstm|*$id:struct->$id:(closureRetvalStructField name) = $exp:(toStorage pt (C.toExp name noLoc));|]
    field name (_, _) =
      [C.cstm|$id:struct->$id:(closureRetvalStructField name) = $id:name.mem;|]

paramToCType :: Param -> GC.CompilerM op s (C.Type, ValueType)
paramToCType (ScalarParam _ pt) = do
  let t = primStorageType pt
  pure (t, Prim pt)
paramToCType (MemParam name space') = mcMemToCType name space'

mcMemToCType :: VName -> Space -> GC.CompilerM op s (C.Type, ValueType)
mcMemToCType v space = do
  refcount <- GC.fatMemory space
  cached <- isJust <$> GC.cacheMem v
  pure
    ( GC.fatMemType space,
      if refcount && not cached
        then MemBlock
        else RawMem
    )

functionRuntime :: Name -> C.Id
functionRuntime = (`C.toIdent` mempty) . (<> "_total_runtime")

functionRuns :: Name -> C.Id
functionRuns = (`C.toIdent` mempty) . (<> "_runs")

functionIter :: Name -> C.Id
functionIter = (`C.toIdent` mempty) . (<> "_iter")

multiCoreReport :: [(Name, Bool)] -> [C.BlockItem]
multiCoreReport names = report_kernels
  where
    report_kernels = concatMap reportKernel names
    max_name_len_pad = 40
    format_string name True =
      let name_s = nameToString name
          padding = replicate (max_name_len_pad - length name_s) ' '
       in unwords ["tid %2d -", name_s ++ padding, "ran %10d times; avg: %10ldus; total: %10ldus; time pr. iter %9.6f; iters %9ld; avg %ld\n"]
    format_string name False =
      let name_s = nameToString name
          padding = replicate (max_name_len_pad - length name_s) ' '
       in unwords ["        ", name_s ++ padding, "ran %10d times; avg: %10ldus; total: %10ldus; time pr. iter %9.6f; iters %9ld; avg %ld\n"]
    reportKernel (name, is_array) =
      let runs = functionRuns name
          total_runtime = functionRuntime name
          iters = functionIter name
       in if is_array
            then
              [ [C.citem|
                     for (int i = 0; i < ctx->scheduler.num_threads; i++) {
                       fprintf(ctx->log,
                         $string:(format_string name is_array),
                         i,
                         ctx->program->$id:runs[i],
                         (long int) ctx->program->$id:total_runtime[i] / (ctx->program->$id:runs[i] != 0 ? ctx->program->$id:runs[i] : 1),
                         (long int) ctx->program->$id:total_runtime[i],
                         (double) ctx->program->$id:total_runtime[i] /  (ctx->program->$id:iters[i] == 0 ? 1 : (double)ctx->program->$id:iters[i]),
                         (long int) (ctx->program->$id:iters[i]),
                         (long int) (ctx->program->$id:iters[i]) / (ctx->program->$id:runs[i] != 0 ? ctx->program->$id:runs[i] : 1)
                         );
                     }
                   |]
              ]
            else
              [ [C.citem|
                    fprintf(ctx->log,
                       $string:(format_string name is_array),
                       ctx->program->$id:runs,
                       (long int) ctx->program->$id:total_runtime / (ctx->program->$id:runs != 0 ? ctx->program->$id:runs : 1),
                       (long int) ctx->program->$id:total_runtime,
                       (double) ctx->program->$id:total_runtime /  (ctx->program->$id:iters == 0 ? 1 : (double)ctx->program->$id:iters),
                       (long int) (ctx->program->$id:iters),
                       (long int) (ctx->program->$id:iters) / (ctx->program->$id:runs != 0 ? ctx->program->$id:runs : 1));
                   |],
                [C.citem|ctx->total_runtime += ctx->program->$id:total_runtime;|],
                [C.citem|ctx->total_runs += ctx->program->$id:runs;|]
              ]

addBenchmarkFields :: Name -> Maybe C.Id -> GC.CompilerM op s ()
addBenchmarkFields name (Just _) = do
  GC.contextFieldDyn
    (functionRuntime name)
    [C.cty|typename int64_t*|]
    [C.cstm|ctx->program->$id:(functionRuntime name) = calloc(sizeof(typename int64_t), ctx->scheduler.num_threads);|]
    [C.cstm|free(ctx->program->$id:(functionRuntime name));|]
  GC.contextFieldDyn
    (functionRuns name)
    [C.cty|int*|]
    [C.cstm|ctx->program->$id:(functionRuns name) = calloc(sizeof(int), ctx->scheduler.num_threads);|]
    [C.cstm|free(ctx->program->$id:(functionRuns name));|]
  GC.contextFieldDyn
    (functionIter name)
    [C.cty|typename int64_t*|]
    [C.cstm|ctx->program->$id:(functionIter name) = calloc(sizeof(sizeof(typename int64_t)), ctx->scheduler.num_threads);|]
    [C.cstm|free(ctx->program->$id:(functionIter name));|]
addBenchmarkFields name Nothing = do
  GC.contextField (functionRuntime name) [C.cty|typename int64_t|] $ Just [C.cexp|0|]
  GC.contextField (functionRuns name) [C.cty|int|] $ Just [C.cexp|0|]
  GC.contextField (functionIter name) [C.cty|typename int64_t|] $ Just [C.cexp|0|]

benchmarkCode :: Name -> Maybe C.Id -> [C.BlockItem] -> GC.CompilerM op s [C.BlockItem]
benchmarkCode name tid code = do
  addBenchmarkFields name tid
  pure
    [C.citems|
     typename uint64_t $id:start = 0;
     if (ctx->profiling && !ctx->profiling_paused) {
       $id:start = get_wall_time();
     }
     $items:code
     if (ctx->profiling && !ctx->profiling_paused) {
       typename uint64_t $id:end = get_wall_time();
       typename uint64_t elapsed = $id:end - $id:start;
       $items:(updateFields tid)
     }
     |]
  where
    start = name <> "_start"
    end = name <> "_end"
    updateFields Nothing =
      [C.citems|__atomic_fetch_add(&ctx->program->$id:(functionRuns name), 1, __ATOMIC_RELAXED);
                                            __atomic_fetch_add(&ctx->program->$id:(functionRuntime name), elapsed, __ATOMIC_RELAXED);
                                            __atomic_fetch_add(&ctx->program->$id:(functionIter name), iterations, __ATOMIC_RELAXED);|]
    updateFields (Just _tid') =
      [C.citems|ctx->program->$id:(functionRuns name)[tid]++;
                                            ctx->program->$id:(functionRuntime name)[tid] += elapsed;
                                            ctx->program->$id:(functionIter name)[tid] += iterations;|]

functionTiming :: Name -> C.Id
functionTiming = (`C.toIdent` mempty) . (<> "_total_time")

functionIterations :: Name -> C.Id
functionIterations = (`C.toIdent` mempty) . (<> "_total_iter")

addTimingFields :: Name -> GC.CompilerM op s ()
addTimingFields name = do
  GC.contextField (functionTiming name) [C.cty|typename int64_t|] $ Just [C.cexp|0|]
  GC.contextField (functionIterations name) [C.cty|typename int64_t|] $ Just [C.cexp|0|]

multicoreName :: String -> GC.CompilerM op s Name
multicoreName s = do
  s' <- newVName ("futhark_mc_" ++ s)
  pure $ nameFromString $ baseString s' ++ "_" ++ show (baseTag s')

type DefSpecifier s = String -> (Name -> GC.CompilerM Multicore s C.Definition) -> GC.CompilerM Multicore s Name

multicoreDef :: DefSpecifier s
multicoreDef s f = do
  s' <- multicoreName s
  GC.libDecl =<< f s'
  pure s'

generateParLoopFn ::
  C.ToIdent a =>
  M.Map VName Space ->
  String ->
  MCCode ->
  a ->
  [(VName, (C.Type, ValueType))] ->
  [(VName, (C.Type, ValueType))] ->
  GC.CompilerM Multicore s Name
generateParLoopFn lexical basename code fstruct free retval = do
  let (fargs, fctypes) = unzip free
  let (retval_args, retval_ctypes) = unzip retval
  multicoreDef basename $ \s -> do
    fbody <- benchmarkCode s (Just "tid") <=< GC.inNewFunction $
      GC.cachingMemory lexical $ \decl_cached free_cached -> GC.collect $ do
        mapM_ GC.item [C.citems|$decls:(compileGetStructVals fstruct fargs fctypes)|]
        mapM_ GC.item [C.citems|$decls:(compileGetRetvalStructVals fstruct retval_args retval_ctypes)|]
        code' <- GC.collect $ GC.compileCode code
        mapM_ GC.item decl_cached
        mapM_ GC.item =<< GC.declAllocatedMem
        mapM_ GC.item code'
        free_mem <- GC.freeAllocatedMem
        GC.stm [C.cstm|cleanup: {$stms:free_cached $items:free_mem}|]
    pure
      [C.cedecl|int $id:s(void *args, typename int64_t iterations, int tid, struct scheduler_info info) {
                           int err = 0;
                           int subtask_id = tid;
                           struct $id:fstruct *$id:fstruct = (struct $id:fstruct*) args;
                           struct futhark_context *ctx = $id:fstruct->ctx;
                           $items:fbody
                           if (err == 0) {
                             $stms:(compileWriteBackResVals fstruct retval_args retval_ctypes)
                           }
                           return err;
                      }|]

prepareTaskStruct ::
  DefSpecifier s ->
  String ->
  [VName] ->
  [(C.Type, ValueType)] ->
  [VName] ->
  [(C.Type, ValueType)] ->
  GC.CompilerM Multicore s Name
prepareTaskStruct def name free_args free_ctypes retval_args retval_ctypes = do
  let makeStruct s =
        pure
          [C.cedecl|struct $id:s {
                       struct futhark_context *ctx;
                       $sdecls:(compileFreeStructFields free_args free_ctypes)
                       $sdecls:(compileRetvalStructFields retval_args retval_ctypes)
                     };|]
  fstruct <- def name makeStruct
  let fstruct' = fstruct <> "_"
  GC.decl [C.cdecl|struct $id:fstruct $id:fstruct';|]
  GC.stm [C.cstm|$id:fstruct'.ctx = ctx;|]
  GC.stms [C.cstms|$stms:(compileSetStructValues fstruct' free_args free_ctypes)|]
  GC.stms [C.cstms|$stms:(compileSetRetvalStructValues fstruct' retval_args retval_ctypes)|]
  pure fstruct

-- Generate a segop function for top_level and potentially nested SegOp code
compileOp :: GC.OpCompiler Multicore s
compileOp (GetLoopBounds start end) = do
  GC.stm [C.cstm|$id:start = start;|]
  GC.stm [C.cstm|$id:end = end;|]
compileOp (GetTaskId v) =
  GC.stm [C.cstm|$id:v = subtask_id;|]
compileOp (GetNumTasks v) =
  GC.stm [C.cstm|$id:v = info.nsubtasks;|]
compileOp (SegOp name params seq_task par_task retvals (SchedulerInfo e sched)) = do
  let (ParallelTask seq_code) = seq_task
  free_ctypes <- mapM paramToCType params
  retval_ctypes <- mapM paramToCType retvals
  let free_args = map paramName params
      retval_args = map paramName retvals
      free = zip free_args free_ctypes
      retval = zip retval_args retval_ctypes

  e' <- GC.compileExp e

  let lexical = lexicalMemoryUsageMC TraverseKernels $ Function Nothing [] params seq_code

  fstruct <-
    prepareTaskStruct multicoreDef "task" free_args free_ctypes retval_args retval_ctypes

  fpar_task <- generateParLoopFn lexical (name ++ "_task") seq_code fstruct free retval
  addTimingFields fpar_task

  let ftask_name = fstruct <> "_task"
  GC.decl [C.cdecl|struct scheduler_segop $id:ftask_name;|]
  GC.stm [C.cstm|$id:ftask_name.args = &$id:(fstruct <> "_");|]
  GC.stm [C.cstm|$id:ftask_name.top_level_fn = $id:fpar_task;|]
  GC.stm [C.cstm|$id:ftask_name.name = $string:(nameToString fpar_task);|]
  GC.stm [C.cstm|$id:ftask_name.iterations = $exp:e';|]
  -- Create the timing fields for the task
  GC.stm [C.cstm|$id:ftask_name.task_time = &ctx->program->$id:(functionTiming fpar_task);|]
  GC.stm [C.cstm|$id:ftask_name.task_iter = &ctx->program->$id:(functionIterations fpar_task);|]

  case sched of
    Dynamic -> GC.stm [C.cstm|$id:ftask_name.sched = DYNAMIC;|]
    Static -> GC.stm [C.cstm|$id:ftask_name.sched = STATIC;|]

  -- Generate the nested segop function if available
  fnpar_task <- case par_task of
    Just (ParallelTask nested_code) -> do
      let lexical_nested = lexicalMemoryUsageMC TraverseKernels $ Function Nothing [] params nested_code
      fnpar_task <- generateParLoopFn lexical_nested (name ++ "_nested_task") nested_code fstruct free retval
      GC.stm [C.cstm|$id:ftask_name.nested_fn = $id:fnpar_task;|]
      pure $ zip [fnpar_task] [True]
    Nothing -> do
      GC.stm [C.cstm|$id:ftask_name.nested_fn=NULL;|]
      pure mempty

  let ftask_err = fpar_task <> "_err"
      code =
        [C.citems|int $id:ftask_err = scheduler_prepare_task(&ctx->scheduler, &$id:ftask_name);
                  if ($id:ftask_err != 0) {
                    err = $id:ftask_err;
                    goto cleanup;
                  }|]

  mapM_ GC.item code

  -- Add profile fields for -P option
  mapM_ GC.profileReport $ multiCoreReport $ (fpar_task, True) : fnpar_task
compileOp (ParLoop s' body free) = do
  free_ctypes <- mapM paramToCType free
  let free_args = map paramName free

  let lexical = lexicalMemoryUsageMC TraverseKernels $ Function Nothing [] free body

  fstruct <-
    prepareTaskStruct multicoreDef (s' ++ "_parloop_struct") free_args free_ctypes mempty mempty

  ftask <- multicoreDef (s' ++ "_parloop") $ \s -> do
    fbody <- benchmarkCode s (Just "tid") <=< GC.inNewFunction $
      GC.cachingMemory lexical $ \decl_cached free_cached -> GC.collect $ do
        mapM_
          GC.item
          [C.citems|$decls:(compileGetStructVals fstruct free_args free_ctypes)|]

        GC.decl [C.cdecl|typename int64_t iterations = end-start;|]

        body' <- GC.collect $ GC.compileCode body

        mapM_ GC.item decl_cached
        mapM_ GC.item =<< GC.declAllocatedMem
        free_mem <- GC.freeAllocatedMem
        mapM_ GC.item body'
        GC.stm [C.cstm|cleanup: {$stms:free_cached $items:free_mem}|]
    pure
      [C.cedecl|static int $id:s(void *args,
                                 typename int64_t start,
                                 typename int64_t end,
                                 int subtask_id,
                                 int tid) {
                       int err = 0;
                       struct $id:fstruct *$id:fstruct = (struct $id:fstruct*) args;
                       struct futhark_context *ctx = $id:fstruct->ctx;
                       $items:fbody
                       return err;
                }|]

  let ftask_name = ftask <> "_task"
  GC.decl [C.cdecl|struct scheduler_parloop $id:ftask_name;|]
  GC.stm [C.cstm|$id:ftask_name.name = $string:(nameToString ftask);|]
  GC.stm [C.cstm|$id:ftask_name.fn = $id:ftask;|]
  GC.stm [C.cstm|$id:ftask_name.args = &$id:(fstruct <> "_");|]
  GC.stm [C.cstm|$id:ftask_name.iterations = iterations;|]
  GC.stm [C.cstm|$id:ftask_name.info = info;|]

  let ftask_err = ftask <> "_err"
      ftask_total = ftask <> "_total"
  code' <-
    benchmarkCode
      ftask_total
      Nothing
      [C.citems|int $id:ftask_err = scheduler_execute_task(&ctx->scheduler,
                                                           &$id:ftask_name);
               if ($id:ftask_err != 0) {
                 err = $id:ftask_err;
                 goto cleanup;
               }|]

  mapM_ GC.item code'
  mapM_ GC.profileReport $ multiCoreReport $ zip [ftask, ftask_total] [True, False]
compileOp (Atomic aop) =
  atomicOps aop (\ty _ -> pure [C.cty|$ty:ty*|])
compileOp (ISPCKernel body _) =
  scopedBlock body
compileOp (ForEach i from bound body) = do
  let i' = C.toIdent i
      t = primTypeToCType $ primExpType bound
  from' <- GC.compileExp from
  bound' <- GC.compileExp bound
  body' <- GC.collect $ GC.compileCode body
  GC.stm
    [C.cstm|for ($ty:t $id:i' = $exp:from'; $id:i' < $exp:bound'; $id:i'++) {
            $items:body'
          }|]
compileOp (ForEachActive i body) = do
  GC.decl [C.cdecl|typename int64_t $id:i = 0;|]
  scopedBlock body
compileOp (ExtractLane dest tar _) = do
  tar' <- GC.compileExp tar
  GC.stm [C.cstm|$id:dest = $exp:tar';|]

scopedBlock :: MCCode -> GC.CompilerM Multicore s ()
scopedBlock code = do
  inner <- GC.collect $ GC.compileCode code
  GC.stm [C.cstm|{$items:inner}|]

doAtomic ::
  (C.ToIdent a1) =>
  a1 ->
  VName ->
  Count u (TExp Int32) ->
  Exp ->
  String ->
  C.Type ->
  (C.Type -> VName -> GC.CompilerM op s C.Type) ->
  GC.CompilerM op s ()
doAtomic old arr ind val op ty castf = do
  ind' <- GC.compileExp $ untyped $ unCount ind
  val' <- GC.compileExp val
  cast <- castf ty arr
  arr' <- GC.rawMem arr
  GC.stm [C.cstm|$id:old = $id:op(&(($ty:cast)$exp:arr')[$exp:ind'], ($ty:ty) $exp:val', __ATOMIC_RELAXED);|]

atomicOps :: AtomicOp -> (C.Type -> VName -> GC.CompilerM op s C.Type) -> GC.CompilerM op s ()
atomicOps (AtomicCmpXchg t old arr ind res val) castf = do
  ind' <- GC.compileExp $ untyped $ unCount ind
  new_val' <- GC.compileExp val
  cast <- castf [C.cty|$ty:(GC.primTypeToCType t)|] arr
  arr' <- GC.rawMem arr
  GC.stm
    [C.cstm|$id:res = $id:op(&(($ty:cast)$exp:arr')[$exp:ind'],
                 &$id:old,
                 $exp:new_val',
                 0, __ATOMIC_SEQ_CST, __ATOMIC_RELAXED);|]
  where
    op :: String
    op = "__atomic_compare_exchange_n"
atomicOps (AtomicXchg t old arr ind val) castf = do
  ind' <- GC.compileExp $ untyped $ unCount ind
  val' <- GC.compileExp val
  cast <- castf [C.cty|$ty:(GC.primTypeToCType t)|] arr
  GC.stm [C.cstm|$id:old = $id:op(&(($ty:cast)$id:arr.mem)[$exp:ind'], $exp:val', __ATOMIC_SEQ_CST);|]
  where
    op :: String
    op = "__atomic_exchange_n"
atomicOps (AtomicAdd t old arr ind val) castf =
  doAtomic old arr ind val "__atomic_fetch_add" [C.cty|$ty:(GC.intTypeToCType t)|] castf
atomicOps (AtomicSub t old arr ind val) castf =
  doAtomic old arr ind val "__atomic_fetch_sub" [C.cty|$ty:(GC.intTypeToCType t)|] castf
atomicOps (AtomicAnd t old arr ind val) castf =
  doAtomic old arr ind val "__atomic_fetch_and" [C.cty|$ty:(GC.intTypeToCType t)|] castf
atomicOps (AtomicOr t old arr ind val) castf =
  doAtomic old arr ind val "__atomic_fetch_or" [C.cty|$ty:(GC.intTypeToCType t)|] castf
atomicOps (AtomicXor t old arr ind val) castf =
  doAtomic old arr ind val "__atomic_fetch_xor" [C.cty|$ty:(GC.intTypeToCType t)|] castf
