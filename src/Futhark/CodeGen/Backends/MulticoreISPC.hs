{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | C code generator.  This module can convert a correct ImpCode
-- program to an equivalent C program.
module Futhark.CodeGen.Backends.MulticoreISPC
  ( compileProg,
    GC.CParts (..),
    GC.asLibrary,
    GC.asExecutable,
    GC.asISPCExecutable,
    GC.asISPCServer,
    GC.asServer,
    operations,
  )
where

import Control.Monad
import qualified Data.Text as T
import Futhark.CodeGen.ImpCode.Multicore
import qualified Futhark.CodeGen.ImpGen.Multicore as ImpGen
import Futhark.IR.MCMem (MCMem, Prog)
import Futhark.MonadFreshNames
import qualified Language.C.Quote.ISPC as C
import qualified Language.C.Syntax as C
import qualified Futhark.CodeGen.Backends.GenericC as GC
import qualified Futhark.CodeGen.Backends.MulticoreC as MC

-- | Compile the program to ImpCode with multicore operations.
compileProg ::
  MonadFreshNames m => T.Text -> T.Text -> Prog MCMem -> m (ImpGen.Warnings, GC.CParts)
compileProg header version =
  traverse
    ( GC.compileProg
        "ispc_multicore"
        version
        operations
        MC.generateContext
        header
        [DefaultSpace]
        MC.cliOptions
    )
    <=< ImpGen.compileProg

operations :: GC.Operations Multicore ()
operations =
  MC.operations
    { GC.opsCompiler = compileOp
    }

-- TODO(pema.malling): Better error handling
ispcOperations :: GC.Operations Multicore ()
ispcOperations =
  operations
    { GC.opsError = \_ _ ->
        GC.items [C.citems|
          err = FUTHARK_PROGRAM_ERROR;
          |]
    }
    
getName :: VName -> Name
getName name = nameFromString $ pretty name

isMemblock :: Param -> Bool
isMemblock (MemParam _ _) = True
isMemblock _  = False

-- Escaped memory name to immediately deref within function scope.
freshMemName :: Param -> Param
freshMemName (MemParam v s) = MemParam (VName (nameFromString ('_' : baseString v)) (baseTag v)) s
freshMemName param = param

-- Compile parameter definitions to pass to ISPC kernel
compileKernelParams :: [VName] -> [(C.Type, MC.ValueType)] -> [C.Param]
compileKernelParams = zipWith field
  where
    field name (ty, MC.Prim) =
      [C.cparam|uniform $ty:ty $id:(getName name)|]
    field name (_, MC.RawMem) =
      [C.cparam|unsigned char uniform * uniform $id:(getName name)|]
    field name (_, _) =
      [C.cparam|uniform typename memblock * uniform $id:(getName name)|]

-- Compile parameter values to ISPC kernel
compileKernelInputs :: [VName] -> [(C.Type, MC.ValueType)] -> [C.Exp]
compileKernelInputs = zipWith field
  where
    field name (_, MC.Prim) = [C.cexp|$id:(getName name)|]
    field name (_, MC.RawMem) = [C.cexp|$id:(getName name)|]
    field name (_, MC.MemBlock) = [C.cexp|&$id:(getName name)|]

compileKernelRetvalParams :: [VName] -> [(C.Type, MC.ValueType)] -> [C.Param]
compileKernelRetvalParams = zipWith field
  where
    field name (ty, MC.Prim) =
      [C.cparam|uniform $ty:ty * uniform $id:(getName name <> "_esc")|]
    field name (_, MC.RawMem) =
      [C.cparam|unsigned char uniform * uniform $id:(getName name <> "_esc")|]
    field name (_, _) =
      [C.cparam|uniform typename memblock * uniform $id:(getName name <> "_esc")|]

compileKernelRetvalInputs :: [VName] -> [(C.Type, MC.ValueType)] -> [C.Exp]
compileKernelRetvalInputs = zipWith field
  where
    field name (_, MC.Prim) = [C.cexp|&$id:(getName name)|]
    field name (_, MC.RawMem) = [C.cexp|$id:(getName name)|]
    field name (_, MC.MemBlock) = [C.cexp|&$id:(getName name)|]

compileKernelRetvalReadback :: [VName] -> [(C.Type, MC.ValueType)] -> [C.Stm]
compileKernelRetvalReadback = zipWith field
  where
    field name (_, MC.Prim) = [C.cstm|*$id:(getName name <> "_esc") = extract($id:(getName name), 0);|]
    field name (_, MC.RawMem) = [C.cstm|$id:(getName name <> "_esc") = extract($id:(getName name), 0);|]
    field name (_, MC.MemBlock) = [C.cstm|*$id:(getName name <> "_esc") = extract($id:(getName name), 0);|]

-- Immediately dereference a memblock passed to the kernel, so we can use it normally
compileMemblockDeref :: [(VName, VName)] -> [(C.Type, MC.ValueType)] -> [C.InitGroup]
compileMemblockDeref = zipWith deref
  where
    deref (v1, v2) (ty, MC.Prim) = [C.cdecl|$ty:ty $id:v1 = $id:(getName v2);|]
    deref (v1, v2) (_, MC.RawMem) = [C.cdecl|unsigned char uniform * uniform $id:v1 = $id:(getName v2);|]
    deref (v1, v2) (ty, MC.MemBlock) = [C.cdecl|uniform $ty:ty $id:v1 = *$id:(getName v2);|]

ispcDef :: MC.DefSpecifier
ispcDef s f = do
  s' <- MC.multicoreName s
  GC.ispcDecl =<< GC.withOperations ispcOperations (f s')
  return s'

sharedDef :: MC.DefSpecifier
sharedDef s f = do
  s' <- MC.multicoreName s
  GC.libDecl [C.cedecl|$esc:("#ifndef __ISPC_STRUCT_" <> (nameToString s') <> "__")|]
  GC.libDecl [C.cedecl|$esc:("#define __ISPC_STRUCT_" <> (nameToString s') <> "__")|] -- TODO:(K) - refacor this shit
  GC.libDecl =<< f s'
  GC.libDecl [C.cedecl|$esc:("#endif")|]
  GC.ispcDecl =<< f s'
  return s'

-- TODO(pema, K): This is a ugly hack to work around the 'size' fields
-- of lexical memory blocks. At the ImpCode level, we cannot know if the size
-- field exists or not, since we can't know if the memory block is lexical.
-- Therefore, the size field is not a free variable in Allocation statements
-- and will be missing the from the free variable set. We manually add it here.
addLexicalMemSize :: Param -> GC.CompilerM op s [Param]
addLexicalMemSize param@(MemParam name _) = do
  cached <- GC.cacheMem name
  case cached of
    Just cur_size -> pure [param, ScalarParam cur_size (IntType Int64)]
    _ -> pure [param]
addLexicalMemSize param = pure [param]

-- Compile a block of code with ISPC specific semantics, falling back
-- to generic C when this semantics is not needed.
compileCodeISPC :: Code -> GC.CompilerM Multicore s ()
compileCodeISPC (Comment s code) = do
  xs <- GC.collect $ compileCodeISPC code
  let comment = "// " ++ s
  GC.stm
    [C.cstm|$comment:comment
              { $items:xs }
             |]
compileCodeISPC (c1 :>>: c2) = go (GC.linearCode (c1 :>>: c2))
  where
    go (DeclareScalar name vol t : SetScalar dest e : code)
      | name == dest = do
        let ct = GC.primTypeToCType t
        e' <- GC.compileExp e
        GC.item [C.citem|$tyquals:(GC.volQuals vol) $ty:ct $id:name = $exp:e';|]
        go code
    go (x : xs) = compileCodeISPC x >> go xs
    go [] = pure ()
compileCodeISPC (Allocate name (Count (TPrimExp e)) space) = do
  size <- GC.compileExp e
  cached <- GC.cacheMem name
  case cached of
    Just cur_size ->
      GC.stm
        [C.cstm|if ($exp:cur_size < $exp:size) {
                 err = lexical_realloc_ispc(&$exp:name, &$exp:cur_size, $exp:size);
                }|]
    _ ->
      GC.allocMem name size space [C.cstm|{err = 1; goto cleanup;}|]
compileCodeISPC (For i bound body) = do
  let i' = C.toIdent i
      t = GC.primTypeToCType $ primExpType bound
  bound' <- GC.compileExp bound
  body' <- GC.collect $ compileCodeISPC body
  GC.stm
    [C.cstm|for ($ty:t $id:i' = 0; $id:i' < $exp:bound'; $id:i'++) {
            $items:body'
          }|]
compileCodeISPC (While cond body) = do
  cond' <- GC.compileExp $ untyped cond
  body' <- GC.collect $ compileCodeISPC body
  GC.stm
    [C.cstm|while ($exp:cond') {
            $items:body'
          }|]
compileCodeISPC (If cond tbranch fbranch) = do
  cond' <- GC.compileExp $ untyped cond
  tbranch' <- GC.collect $ compileCodeISPC tbranch
  fbranch' <- GC.collect $ compileCodeISPC fbranch
  GC.stm $ case (tbranch', fbranch') of
    (_, []) ->
      [C.cstm|if ($exp:cond') { $items:tbranch' }|]
    ([], _) ->
      [C.cstm|if (!($exp:cond')) { $items:fbranch' }|]
    _ ->
      [C.cstm|if ($exp:cond') { $items:tbranch' } else { $items:fbranch' }|]
compileCodeISPC code =
  GC.compileCode code

-- Generate a segop function for top_level and potentially nested SegOp code
compileOp :: GC.OpCompiler Multicore ()
compileOp (SegOp name params seq_task par_task retvals (SchedulerInfo e sched)) = do
  let (ParallelTask seq_code) = seq_task
  free_ctypes <- mapM MC.paramToCType params
  retval_ctypes <- mapM MC.paramToCType retvals
  let free_args = map paramName params
      retval_args = map paramName retvals
      free = zip free_args free_ctypes
      retval = zip retval_args retval_ctypes

  e' <- GC.compileExp e

  let lexical = lexicalMemoryUsage $ Function Nothing [] params seq_code [] []

  fstruct <-
    MC.prepareTaskStruct sharedDef "task" free_args free_ctypes retval_args retval_ctypes

  fpar_task <- MC.generateParLoopFn lexical (name ++ "_task") seq_code fstruct free retval
  MC.addTimingFields fpar_task

  let ftask_name = fstruct <> "_task"

  toC <- GC.collect $ do
    GC.decl [C.cdecl|struct scheduler_segop $id:ftask_name;|]
    GC.stm [C.cstm|$id:ftask_name.args = args;|]
    GC.stm [C.cstm|$id:ftask_name.top_level_fn = $id:fpar_task;|]
    GC.stm [C.cstm|$id:ftask_name.name = $string:(nameToString fpar_task);|]
    GC.stm [C.cstm|$id:ftask_name.iterations = iterations;|]
    -- Create the timing fields for the task
    GC.stm [C.cstm|$id:ftask_name.task_time = &ctx->$id:(MC.functionTiming fpar_task);|]
    GC.stm [C.cstm|$id:ftask_name.task_iter = &ctx->$id:(MC.functionIterations fpar_task);|]

    case sched of
        Dynamic -> GC.stm [C.cstm|$id:ftask_name.sched = DYNAMIC;|]
        Static -> GC.stm [C.cstm|$id:ftask_name.sched = STATIC;|]

    -- Generate the nested segop function if available
    fnpar_task <- case par_task of
        Just (ParallelTask nested_code) -> do
            let lexical_nested = lexicalMemoryUsage $ Function Nothing [] params nested_code [] []
            fnpar_task <- MC.generateParLoopFn lexical_nested (name ++ "_nested_task") nested_code fstruct free retval
            GC.stm [C.cstm|$id:ftask_name.nested_fn = $id:fnpar_task;|]
            return $ zip [fnpar_task] [True]
        Nothing -> do
            GC.stm [C.cstm|$id:ftask_name.nested_fn=NULL;|]
            return mempty

    GC.stm [C.cstm|return scheduler_prepare_task(&ctx->scheduler, &$id:ftask_name);|]

    -- Add profile fields for -P option
    mapM_ GC.profileReport $ MC.multiCoreReport $ (fpar_task, True) : fnpar_task

  schedn <- MC.multicoreDef "schedule_shim" $ \s ->
    return [C.cedecl|int $id:s(struct futhark_context* ctx, void* args, typename int64_t iterations) {
        $items:toC
    }|]

  free_all_mem <- GC.freeAllocatedMem -- TODO(pema): Should this free be here?

  void $ ispcDef "" $ \_ -> return [C.cedecl| extern "C" unmasked uniform int $id:schedn 
                                                (struct futhark_context uniform * uniform ctx, 
                                                struct $id:fstruct uniform * uniform args, 
                                                uniform int iterations);|]
  aos_name <- newVName "aos"
  GC.stm [C.cstm|$escstm:("#if ISPC")|]
  GC.items [C.citems|
    #if ISPC
    uniform struct $id:fstruct $id:aos_name[programCount];
    $id:aos_name[programIndex] = $id:(fstruct <> "_");
    foreach_active (i) {
      if (err == 0) {
        err = $id:schedn(ctx, &$id:aos_name[i], extract($exp:e', i));
      }
    }|]
  -- TODO(pema): We can't do much else here^^ than set the error code and hope for the best
  GC.stm [C.cstm|$escstm:("#else")|]
  GC.items [C.citems|
    err = $id:schedn(ctx, &$id:(fstruct <> "_"), $exp:e');
    if (err != 0) {
      $items:free_all_mem
      goto cleanup;
    }|]
  GC.stm [C.cstm|$escstm:("#endif")|]

compileOp (ISPCKernel body free' retvals) = do
  free <- concat <$> mapM addLexicalMemSize free'

  free_ctypes <- mapM MC.paramToCType free
  let free_args = map paramName free

  -- rename memblocks so we can pass them as pointers, compile the parameters
  let free_names_esc = map freshMemName free
  let free_args_esc = map paramName free_names_esc
  let inputs_free = compileKernelParams free_args_esc free_ctypes

  -- dereference memblock pointers into the unescaped names
  let mem_args = map paramName $ filter isMemblock free
  let mem_args_esc = map paramName $ filter isMemblock free_names_esc
  mem_ctypes <- mapM MC.paramToCType (filter isMemblock free)
  let memderef = compileMemblockDeref (zip mem_args mem_args_esc) mem_ctypes

  -- handle return values
  retval_ctypes <- mapM MC.paramToCType retvals
  let retval_args = map paramName retvals
  let inputs_retvals = compileKernelRetvalParams retval_args retval_ctypes
  let readback = compileKernelRetvalReadback retval_args retval_ctypes

  -- TODO(pema): We generate code for a new function without calling GC.inNewFunction,
  -- I think this is a hack. If I understand correctly, it can result in double-frees
  -- when an inner scope thinks that it owns memory from an outer scope.
  let lexical = lexicalMemoryUsage $ Function Nothing [] free body [] []
  -- Generate ISPC kernel
  ispcShim <- ispcDef "loop_ispc" $ \s -> do
    mainBody <- GC.cachingMemory lexical $ \decl_cached free_cached ->
      GC.collect $ do
        GC.decl [C.cdecl|uniform int err = 0;|]
        mapM_ GC.item decl_cached
        mapM_ GC.item =<< GC.declAllocatedMem
        free_mem <- GC.freeAllocatedMem

        compileCodeISPC body
        GC.stms readback

        GC.stm [C.cstm|cleanup: {$stms:free_cached $items:free_mem}|]
        GC.stm [C.cstm|return err;|]

    return
      [C.cedecl|
        export static uniform int $id:s(struct futhark_context uniform * uniform ctx,
                                 uniform typename int64_t start,
                                 uniform typename int64_t end,
                                 $params:inputs_free, $params:inputs_retvals) {
          $decls:memderef
          $items:mainBody
        }|]

  -- Generate C code to call into ISPC kernel
  let ispc_inputs_free = compileKernelInputs free_args free_ctypes
  let ispc_inputs_retvals = compileKernelRetvalInputs retval_args retval_ctypes
  free_all_mem <- GC.freeAllocatedMem -- TODO(pema): Should this be here?
  GC.items [C.citems|
    err = $id:ispcShim(ctx, start, end, $args:ispc_inputs_free, $args:ispc_inputs_retvals);
    if (err != 0) {
      $items:free_all_mem
      goto cleanup;
    }|]

compileOp (ForEach i bound body) = do
  bound' <- GC.compileExp bound
  body' <- GC.collect $ compileCodeISPC body
  GC.stm [C.cstm|
    foreach ($id:i = 0 ... extract($exp:bound', 0)) {
      $items:body'
    }|]

compileOp (ForEachActive name body) = do
  body' <- GC.collect $ compileCodeISPC body
  GC.stm [C.cstm|
    foreach_active ($id:name) {
      $items:body'
    }|]

compileOp (UnmaskedBlock code) = do
  body <- GC.collect $ compileCodeISPC code
  GC.items [C.citems|
    $escstm:("unmasked") {
      $items:body
    }|]

compileOp (ExtractLane dest tar lane) = do
  tar' <- GC.compileExp tar
  lane' <- GC.compileExp lane
  GC.stm [C.cstm|$id:dest = extract($exp:tar', $exp:lane');|]

compileOp op = MC.compileOp op