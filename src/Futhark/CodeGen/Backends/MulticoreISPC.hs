{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | C code generator.  This module can convert a correct ImpCode
-- program to an equivalent C program.
module Futhark.CodeGen.Backends.MulticoreISPC
  ( compileProg,
    GC.CParts (..),
    GC.asLibrary,
    GC.asExecutable,
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
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.CodeGen.RTS.C (errorsH, ispcUtilH, uniformH)
import Futhark.CodeGen.Backends.SimpleRep (toStorage, primStorageType, cScalarDefs, funName)
import Futhark.IR.Prop (isBuiltInFunction)
import Data.Maybe
import Data.Loc (noLoc)
import qualified Data.DList as DL
import NeatInterpolation (untrimming)
import Futhark.Util.Pretty (prettyText)

type ISPCState = (DL.DList C.Definition)
type ISPCCompilerM a = GC.CompilerM Multicore ISPCState a


compileBuiltinFunc :: (Name, Function op) -> ISPCCompilerM ()
compileBuiltinFunc (fname, func@(Function _ outputs inputs _ _ _)) =  do
  let extra = [[C.cparam|struct futhark_context *ctx|]]
  case functionEntry func of
    Nothing -> do

      let extra_ispc = [[C.cparam|uniform struct futhark_context * uniform ctx|]]
          extraToExp_ispc = [[C.cexp|$id:p|] | C.Param (Just p) _ _ _ <- extra_ispc]

      args_noderef <- mapM compileNoDerefArguments (inputs <> outputs)      
      args_extracted <- mapM compileExtractedArguments (inputs <> outputs)      
      inparams_extern <- mapM compileInputsExtern inputs
      inparams_uni <- mapM compileInputsUniform inputs
      inparams_varying <- mapM compileInputsVarying inputs
      (outparams_extern,_) <- unzip <$> mapM compileOutputsExtern outputs
      (outparams_uni,_) <- unzip <$> mapM compileOutputsUniform outputs
      (outparams_varying,_) <- unzip <$> mapM compileOutputsVarying outputs

      pb' <- mapM uniformizeMemParam $ filter isMemParam (outputs <> inputs)
      let pb = concat pb'

      let ispc_extern = [C.cedecl|extern "C" uniform int $id:((funName fname) ++ "_extern")
                                  ($params:extra_ispc, $params:outparams_extern, $params:inparams_extern);|]
          ispc_uniform = [C.cfun|uniform int $id:(funName fname)
                                 ($params:extra_ispc, $params:outparams_uni, $params:inparams_uni){ 
                                      return $id:(funName $ fname<>"_extern")
                                          ($args:extraToExp_ispc, $args:args_noderef);}|]

          
          ispc_varying = [C.cfun|uniform int $id:(funName fname)
                                 ($params:extra_ispc, $params:outparams_varying, $params:inparams_varying){ 
                                     
                                     $items:pb
                                     foreach_active(i){
                                        $id:(funName $ fname<>"_extern")($args:extraToExp_ispc, $args:args_extracted);
                                     }
                                 return 1;}|]

      --mapM_ GC.earlyDecl [funcToDef cfun]
      mapM_ ispcDeclPrepend [funcToDef ispc_varying, funcToDef ispc_uniform, ispc_extern]
         
    Just _ -> pure ()
  where    
    isMemParam (MemParam _ _) = True
    isMemParam _ = False

    uniformizeMemParam (MemParam name space) = do
      typ <- GC.memToCType name space
      let newvn = "aos_" <> pretty name
      return [C.citems|uniform $ty:typ $id:(newvn)[programCount];
                       $id:(newvn)[programIndex] = $id:name;|]
    --uniformizeMemParam (ScalarParam name _) = return [C.citems|{}|]
                   
    compileNoDerefArguments (ScalarParam name _) = return [C.cexp|$id:name|]
    compileNoDerefArguments (MemParam name _) = return [C.cexp|&$id:name|]

    compileExtractedArguments (ScalarParam name _) = return [C.cexp|extract($id:name,i)|]
    compileExtractedArguments (MemParam name space) = do
      typ <- GC.memToCType name space
      let newvn = "aos_" <> pretty name
      return [C.cexp|&$id:(newvn)[i]|]

    compileInputsExtern (ScalarParam name bt) = do
      let ctp = GC.primTypeToCType bt
      pure [C.cparam|uniform $ty:ctp $id:name|]
    compileInputsExtern (MemParam name space) = do
      ty <- GC.memToCType name space
      pure [C.cparam|uniform $ty:ty * uniform $id:name|]

    compileOutputsExtern (ScalarParam name bt) = do
      let ctp = GC.primTypeToCType bt
      p_name <- newVName $ "out_" ++ baseString name
      return ([C.cparam|uniform $ty:ctp * uniform $id:p_name|], [C.cexp|$id:p_name|])
    compileOutputsExtern (MemParam name space) = do
      ty <- GC.memToCType name space
      p_name <- newVName $ baseString name ++ "_p"
      return ([C.cparam|uniform $ty:ty * uniform $id:p_name|], [C.cexp|$id:p_name|])

    compileInputsUniform (ScalarParam name bt) = do
      let ctp = GC.primTypeToCType bt
      pure [C.cparam|uniform $ty:ctp $id:name|]
    compileInputsUniform (MemParam name space) = do
      ty <- GC.memToCType name space
      pure [C.cparam|uniform $ty:ty $id:name|]

    compileOutputsUniform (ScalarParam name bt) = do
      let ctp = GC.primTypeToCType bt
      p_name <- newVName $ "out_" ++ baseString name
      return ([C.cparam|uniform $ty:ctp $id:p_name|], [C.cexp|$id:p_name|])
    compileOutputsUniform (MemParam name space) = do
      ty <- GC.memToCType name space
      p_name <- newVName $ baseString name ++ "_p"
      return ([C.cparam|uniform $ty:ty $id:p_name|], [C.cexp|$id:p_name|])

    compileInputsVarying (ScalarParam name bt) = do
      let ctp = GC.primTypeToCType bt
      pure [C.cparam|$ty:ctp $id:name|]
    compileInputsVarying (MemParam name space) = do
      ty <- GC.memToCType name space
      pure [C.cparam|$ty:ty $id:name|]

    compileOutputsVarying (ScalarParam name bt) = do
      let ctp = GC.primTypeToCType bt
      p_name <- newVName $ "out_" ++ baseString name
      return ([C.cparam|$ty:ctp $id:p_name|], [C.cexp|$id:p_name|])
    compileOutputsVarying (MemParam name space) = do
      ty <- GC.memToCType name space
      p_name <- newVName $ baseString name ++ "_p"
      return ([C.cparam|$ty:ty $id:p_name|], [C.cexp|$id:p_name|])


    funcToDef f = C.FuncDef f loc
      where
        loc = case f of
          C.OldFunc _ _ _ _ _ _ l -> l
          C.Func _ _ _ _ _ l -> l 

-- | Compile the program to C and ISPC code using multicore operations.
compileProg ::
  MonadFreshNames m => T.Text -> T.Text -> Prog MCMem -> m (ImpGen.Warnings, (GC.CParts, T.Text))
compileProg header version prog = do
  (ws, defs) <- ImpGen.compileProg prog
  let Functions funs = defFuns defs

  (ws', (cparts, endstate)) <-
    traverse
      (GC.compileProg'
        "ispc_multicore"
        version
        operations
        (MC.generateContext >> mapM_ compileBuiltinFunc funs)
        header
        [DefaultSpace]
        MC.cliOptions
      ) (ws, defs)

  let ispc_decls = T.unlines $ map prettyText $ DL.toList $ GC.compUserState endstate

  let ispcdefs =
        [untrimming|
#define bool uint8 // This is a workaround around an ISPC bug, stdbool doesn't get included
typedef int64 int64_t;
typedef int32 int32_t;
typedef int16 int16_t;
typedef int8 int8_t;
typedef int8 char;
typedef unsigned int64 uint64_t;
typedef unsigned int32 uint32_t;
typedef unsigned int16 uint16_t;
typedef unsigned int8 uint8_t;

$errorsH

#define INFINITY (floatbits((uniform int)0x7f800000))
#define NAN (floatbits((uniform int)0x7fc00000))
#define fabs(x) abs(x)
#define FUTHARK_F64_ENABLED
$cScalarDefs

$uniformH

$ispcUtilH

#ifndef __ISPC_STRUCT_memblock__
#define __ISPC_STRUCT_memblock__
struct memblock {
    int32_t * references;
    uint8_t * mem;
    int64_t size;
    const int8_t * desc;
};
#endif

$ispc_decls|]

  pure (ws', (cparts, ispcdefs))

operations :: GC.Operations Multicore ISPCState
operations =
  MC.operations
    { GC.opsCompiler = compileOp,
      GC.opsCall = defCallIspc
    }

ispcDecl :: C.Definition  -> ISPCCompilerM ()
ispcDecl def =
  GC.modifyUserState (\s -> s <> DL.singleton def)

-- TODO (obp): is this actually fucking stupid?
ispcDeclPrepend ::C.Definition  -> ISPCCompilerM ()
ispcDeclPrepend def =
  GC.modifyUserState (\s -> DL.singleton def <> s)

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

-- Immediately dereference a memblock passed to the kernel, so we can use it normally
compileMemblockDeref :: [(VName, VName)] -> [(C.Type, MC.ValueType)] -> [C.InitGroup]
compileMemblockDeref = zipWith deref
  where
    deref (v1, v2) (ty, MC.Prim) = [C.cdecl|$ty:ty $id:v1 = $id:(getName v2);|]
    deref (v1, v2) (_, MC.RawMem) = [C.cdecl|unsigned char uniform * uniform $id:v1 = $id:(getName v2);|]
    deref (v1, v2) (ty, MC.MemBlock) = [C.cdecl|uniform $ty:ty $id:v1 = *$id:(getName v2);|]

ispcDef :: MC.DefSpecifier ISPCState
ispcDef s f = do
  s' <- MC.multicoreName s
  ispcDecl =<< f s'
  pure s'

sharedDef :: MC.DefSpecifier ISPCState
sharedDef s f = do
  s' <- MC.multicoreName s
  ispcDecl =<< f s'
  -- Workaround for https://github.com/ispc/ispc/issues/2277
  dummy <- newVName "dummy_struct_usage"
  ispcDecl [C.cedecl|export void $id:dummy(uniform struct $id:s' * uniform a) { (void)a; }|]
  pure s'

makeStringLiteral :: String -> ISPCCompilerM Name
makeStringLiteral str = do
  name <- MC.multicoreDef "strlit_shim" $ \s ->
    pure [C.cedecl|char* $id:s() { return $string:str; }|]
  ispcDecl
    [C.cedecl|extern "C" unmasked uniform char* uniform $id:name();|]
  pure name

allocMemISPC :: (C.ToExp a, C.ToExp b) => a -> b -> Space -> C.Stm -> ISPCCompilerM ()
allocMemISPC mem size space on_failure = GC.allocMem' mem size space on_failure stmt
  where
    stmt space' mem' size' mem_s' on_failure' = do
      strlit <- makeStringLiteral mem_s'
      pure
        [C.cstm|if ($id:(GC.fatMemAlloc space')(ctx, &$exp:mem', $exp:size',
                  $id:strlit())) {
                  $stm:on_failure'
        }|]

setMemISPC :: (C.ToExp a, C.ToExp b) => a -> b -> Space -> ISPCCompilerM ()
setMemISPC dest src space = GC.setMem' dest src space stmt
  where
    stmt space' dest' src' src_s' = do
      strlit <- makeStringLiteral src_s'
      pure
        [C.cstm|if ($id:(GC.fatMemSet space')(ctx, &$exp:dest', &$exp:src',
          $id:strlit()) != 0) {
          err = 1;
        }|]

unRefMemISPC :: C.ToExp a => a -> Space -> ISPCCompilerM ()
unRefMemISPC mem space = GC.unRefMem' mem space cstm
  where
    cstm s m m_s = do
      strlit <- makeStringLiteral m_s
      pure
        [C.cstm|if ($id:(GC.fatMemUnRef s)(ctx, &$exp:m, $id:strlit()) != 0) {
          err = 1;
        }|]

freeAllocatedMemISPC :: ISPCCompilerM [C.BlockItem]
freeAllocatedMemISPC = GC.freeAllocatedMem' unRefMemISPC

-- Handle printing an error message in ISPC
errorInISPC :: ErrorMsg Exp -> String -> ISPCCompilerM ()
errorInISPC msg stacktrace = do
  -- Get format sting
  (formatstr, formatargs) <- GC.errorMsgString msg
  let formatstr' = "Error: " <> formatstr <> "\n\nBacktrace:\n%s"
  -- Get args types and names for shim
  let arg_types = errorMsgArgTypes msg
  arg_names <- mapM (newVName . const "arg") arg_types
  let params = zipWith (\ty name -> [C.cparam|$ty:(GC.primTypeToCType ty) $id:name|]) arg_types arg_names
  let params_uni = zipWith (\ty name -> [C.cparam|uniform $ty:(GC.primTypeToCType ty) $id:name|]) arg_types arg_names
  -- Make shim
  let formatargs' = mapArgNames msg formatargs arg_names
  shim <- MC.multicoreDef "assert_shim" $ \s -> do
    pure
      [C.cedecl|void $id:s(struct futhark_context* ctx, $params:params) {
        ctx->error = msgprintf($string:formatstr', $args:formatargs', $string:stacktrace);
      }|]
  ispcDecl
    [C.cedecl|extern "C" unmasked void $id:shim(uniform struct futhark_context* uniform, $params:params_uni);|]
  -- Call the shim
  args <- getErrorValExps msg
  uni <- newVName "uni"
  let args' = map (\x -> [C.cexp|extract($exp:x, $id:uni)|]) args
  GC.items
    [C.citems|
      foreach_active($id:uni) {
        $id:shim(ctx, $args:args');
        err = FUTHARK_PROGRAM_ERROR;
      }|]
  where
    getErrorVal (ErrorString _) = Nothing
    getErrorVal (ErrorVal _ v) = Just v

    getErrorValExps (ErrorMsg m) = mapM GC.compileExp $ mapMaybe getErrorVal m

    mapArgNames' (x:xs) (y:ys) (t:ts)
      | isJust $ getErrorVal x = [C.cexp|$id:t|] : mapArgNames' xs ys ts
      | otherwise = y : mapArgNames' xs ys (t:ts)
    mapArgNames' _ ys [] = ys
    mapArgNames' _ _ _ = []

    mapArgNames (ErrorMsg parts) = mapArgNames' parts

isConstExp :: PrimExp VName -> Bool
isConstExp = isSimple . constFoldPrimExp
  where isSimple (ValueExp _) = True
        isSimple _ = False

-- TODO(pema): Deduplicate some of the C code in the generic C generator
-- Compile a block of code with ISPC specific semantics, falling back
-- to generic C when this semantics is not needed.
compileCodeISPC :: Imp.Variability -> MCCode -> ISPCCompilerM ()
compileCodeISPC vari (Comment s code) = do
  xs <- GC.collect $ compileCodeISPC vari code
  let comment = "// " ++ s
  GC.stm
    [C.cstm|$comment:comment
              { $items:xs }
             |]
compileCodeISPC vari (DeclareScalar name vol t) = do
  let ct = GC.primTypeToCType t
  GC.decl [C.cdecl|$tyquals:(GC.volQuals vol) $tyquals:(GC.variQuals vari) $ty:ct $id:name;|]
compileCodeISPC _ (DeclareArray name DefaultSpace t vs) = do
  name_realtype <- newVName $ baseString name ++ "_realtype"
  let ct = GC.primTypeToCType t
  case vs of
    ArrayValues vs' -> do
      let vs'' = [[C.cinit|$exp:v|] | v <- vs']
      GC.earlyDecl [C.cedecl|static $ty:ct $id:name_realtype[$int:(length vs')] = {$inits:vs''};|]
    ArrayZeros n ->
      GC.earlyDecl [C.cedecl|static $ty:ct $id:name_realtype[$int:n];|]
  -- Fake a memory block.
  GC.contextField
    (C.toIdent name noLoc)
    [C.cty|struct memblock|]
    $ Just [C.cexp|(struct memblock){NULL, (char*)$id:name_realtype, 0}|]
  -- Make an exported C shim to access it
  shim <- MC.multicoreDef "get_static_array_shim" $ \f ->
    pure [C.cedecl|struct memblock* $id:f(struct futhark_context* ctx) { return &ctx->$id:name; }|]
  ispcDecl [C.cedecl|extern "C" unmasked uniform struct memblock * uniform
                        $id:shim(uniform struct futhark_context* uniform ctx);|]
  -- Call it
  GC.item [C.citem|uniform struct memblock $id:name = *$id:shim(ctx);|]
compileCodeISPC vari (c1 :>>: c2) = go (GC.linearCode (c1 :>>: c2))
  where
    go (DeclareScalar name vol t : SetScalar dest e : code)
      | name == dest = do
        let ct = GC.primTypeToCType t
        e' <- GC.compileExp e
        GC.item [C.citem|$tyquals:(GC.volQuals vol) $tyquals:(GC.variQuals vari)  $ty:ct $id:name = $exp:e';|]
        go code
    go (x : xs) = compileCodeISPC vari x >> go xs
    go [] = pure ()
compileCodeISPC _ (Allocate name (Count (TPrimExp e)) space) = do
  size <- GC.compileExp e
  cached <- GC.cacheMem name
  case cached of
    Just cur_size ->
      GC.stm -- TODO(pema): Handle errors here
        [C.cstm|if ($exp:cur_size < $exp:size) {
                  lexical_realloc_ispc(&$exp:name, &$exp:cur_size, $exp:size);
                }|]
    _ ->
      allocMemISPC name size space [C.cstm|{err = 1;}|]
compileCodeISPC _ (SetMem dest src space) =
  setMemISPC dest src space
compileCodeISPC vari code@(Write dest (Count idx) elemtype DefaultSpace vol elemexp)
  | isConstExp (untyped idx) = do
    dest' <- GC.rawMem dest
    idxexp <- GC.compileExp (untyped idx)
    tmp <- newVName "tmp_idx"
    -- Disambiguate the variability of the constant index
    GC.decl [C.cdecl|$tyquals:(GC.variQuals vari) typename int64_t $id:tmp = $exp:idxexp;|]
    let deref =
          GC.derefPointer
            dest'
            [C.cexp|$id:tmp|]
            [C.cty|$tyquals:(GC.volQuals vol) $ty:(primStorageType elemtype)*|]
    elemexp' <- toStorage elemtype <$> GC.compileExp elemexp
    GC.stm [C.cstm|$exp:deref = $exp:elemexp';|]
  | otherwise = GC.compileCode code
compileCodeISPC _ (Free name space) = do
  cached <- isJust <$> GC.cacheMem name
  unless cached $ unRefMemISPC name space
compileCodeISPC vari (For i bound body) = do
  let i' = C.toIdent i
      t = GC.primTypeToCType $ primExpType bound
  bound' <- GC.compileExp bound
  body' <- GC.collect $ compileCodeISPC vari body
  GC.stm -- TODO(pema): This is unsafe is the bound is varying
    [C.cstm|for (uniform $ty:t $id:i' = 0; $id:i' < $exp:bound'; $id:i'++) {
            $items:body'
          }|]
compileCodeISPC vari (While cond body) = do
  cond' <- GC.compileExp $ untyped cond
  body' <- GC.collect $ compileCodeISPC vari body
  GC.stm
    [C.cstm|while ($exp:cond') {
            $items:body'
          }|]
compileCodeISPC vari (If cond tbranch fbranch) = do
  cond' <- GC.compileExp $ untyped cond
  tbranch' <- GC.collect $ compileCodeISPC vari tbranch
  fbranch' <- GC.collect $ compileCodeISPC vari fbranch
  GC.stm $ case (tbranch', fbranch') of
    (_, []) ->
      [C.cstm|if ($exp:cond') { $items:tbranch' }|]
    ([], _) ->
      [C.cstm|if (!($exp:cond')) { $items:fbranch' }|]
    _ ->
      [C.cstm|if ($exp:cond') { $items:tbranch' } else { $items:fbranch' }|]
compileCodeISPC _ (Assert e msg (loc, locs)) = do
  e' <- GC.compileExp e
  err <- GC.collect $ errorInISPC msg stacktrace
  GC.stm [C.cstm|if (!$exp:e') { $items:err }|]
  where
    stacktrace = prettyStacktrace 0 $ map locStr $ loc : locs
compileCodeISPC _ code =
  GC.compileCode code

-- Generate a segop function for top_level and potentially nested SegOp code
compileOp :: GC.OpCompiler Multicore ISPCState
compileOp (SegOp name params seq_task par_task retvals (SchedulerInfo e sched)) = do
  let (ParallelTask seq_code) = seq_task
  free_ctypes <- mapM MC.paramToCType params
  retval_ctypes <- mapM MC.paramToCType retvals
  let free_args = map paramName params
      retval_args = map paramName retvals
      free = zip free_args free_ctypes
      retval = zip retval_args retval_ctypes

  e' <- GC.compileExp e

  let lexical = lexicalMemoryUsageMC $ Function Nothing [] params seq_code [] []

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
            let lexical_nested = lexicalMemoryUsageMC $ Function Nothing [] params nested_code [] []
            fnpar_task <- MC.generateParLoopFn lexical_nested (name ++ "_nested_task") nested_code fstruct free retval
            GC.stm [C.cstm|$id:ftask_name.nested_fn = $id:fnpar_task;|]
            pure $ zip [fnpar_task] [True]
        Nothing -> do
            GC.stm [C.cstm|$id:ftask_name.nested_fn=NULL;|]
            pure mempty

    GC.stm [C.cstm|return scheduler_prepare_task(&ctx->scheduler, &$id:ftask_name);|]

    -- Add profile fields for -P option
    mapM_ GC.profileReport $ MC.multiCoreReport $ (fpar_task, True) : fnpar_task

  schedn <- MC.multicoreDef "schedule_shim" $ \s ->
    pure [C.cedecl|int $id:s(struct futhark_context* ctx, void* args, typename int64_t iterations) {
        $items:toC
    }|]

  ispcDecl [C.cedecl|extern "C" unmasked uniform int $id:schedn 
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
      goto cleanup;
    }|]
  GC.stm [C.cstm|$escstm:("#endif")|]

compileOp (ISPCKernel body free) = do
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

  let lexical = lexicalMemoryUsageMC $ Function Nothing [] free body [] []
  -- Generate ISPC kernel
  ispcShim <- ispcDef "loop_ispc" $ \s -> do
    mainBody <- GC.inNewFunction $ GC.cachingMemory lexical $ \decl_cached free_cached ->
      GC.collect $ do
        GC.decl [C.cdecl|uniform int err = 0;|]
        body' <- GC.collect $ compileCodeISPC Imp.Varying body
        mapM_ GC.item decl_cached
        mapM_ GC.item =<< GC.declAllocatedMem
        mapM_ GC.item body'
        free_mem <- freeAllocatedMemISPC

        GC.stm [C.cstm|cleanup: {$stms:free_cached $items:free_mem}|]
        GC.stm [C.cstm|return err;|]

    pure
      [C.cedecl|
        export static uniform int $id:s(struct futhark_context uniform * uniform ctx,
                                 uniform typename int64_t start,
                                 uniform typename int64_t end,
                                 $params:inputs_free) {
          $decls:memderef
          $items:mainBody
        }|]

  -- Generate C code to call into ISPC kernel
  let ispc_inputs_free = compileKernelInputs free_args free_ctypes
  GC.items [C.citems|
    err = $id:ispcShim(ctx, start, end, $args:ispc_inputs_free);
    if (err != 0) {
      goto cleanup;
    }|]

compileOp (ForEach i bound body) = do
  bound' <- GC.compileExp bound
  body' <- GC.collect $ compileCodeISPC Imp.Varying body
  GC.stm [C.cstm|
    foreach ($id:i = 0 ... extract($exp:bound', 0)) {
      $items:body'
    }|]

compileOp (ForEachActive name body) = do
  body' <- GC.collect $ compileCodeISPC Imp.Uniform body
  GC.stm [C.cstm|
    foreach_active ($id:name) {
      $items:body'
    }|]

compileOp (ExtractLane dest tar lane) = do
  tar' <- GC.compileExp tar
  lane' <- GC.compileExp lane
  GC.stm [C.cstm|$id:dest = extract($exp:tar', $exp:lane');|]

compileOp (VariabilityBlock vari code) = do
  compileCodeISPC vari code

compileOp op = MC.compileOp op

  
defCallIspc :: GC.CallCompiler op s
defCallIspc dests fname args = do
  let out_args = [[C.cexp|&$id:d|] | d <- dests]
      args'
        | isBuiltInFunction fname = args
        | otherwise = [C.cexp|ctx|] : out_args ++ args
  case dests of
    [dest]
      | isBuiltInFunction fname ->
          GC.stm [C.cstm|$id:dest = $id:(funName fname)($args:args');|]
    _ ->
      GC.item [C.citem|if ($id:(funName fname)($args:args') != 0) { err = 1; }|]

