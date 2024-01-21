{-# LANGUAGE QuasiQuotes #-}

-- | C code generation for whole programs, built on
-- "Futhark.CodeGen.Backends.GenericC.Monad".  Most of this module is
-- concerned with constructing the C API.
module Futhark.CodeGen.Backends.GenericC
  ( compileProg,
    compileProg',
    defaultOperations,
    ParamMap,
    CParts (..),
    asLibrary,
    asExecutable,
    asServer,
    module Futhark.CodeGen.Backends.GenericC.Monad,
    module Futhark.CodeGen.Backends.GenericC.Code,
  )
where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (second)
import Data.DList qualified as DL
import Data.List qualified as L
import Data.Loc
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC.CLI (cliDefs)
import Futhark.CodeGen.Backends.GenericC.Code
import Futhark.CodeGen.Backends.GenericC.EntryPoints
import Futhark.CodeGen.Backends.GenericC.Fun
import Futhark.CodeGen.Backends.GenericC.Monad
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.GenericC.Pretty
import Futhark.CodeGen.Backends.GenericC.Server (serverDefs)
import Futhark.CodeGen.Backends.GenericC.Types
import Futhark.CodeGen.ImpCode
import Futhark.CodeGen.RTS.C (cacheH, contextH, contextPrototypesH, copyH, errorsH, eventListH, freeListH, halfH, lockH, timingH, utilH)
import Futhark.IR.GPU.Sizes
import Futhark.Manifest qualified as Manifest
import Futhark.MonadFreshNames
import Futhark.Util (zEncodeText)
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C
import NeatInterpolation (untrimming)

defCall :: CallCompiler op s
defCall dests fname args = do
  let out_args = [[C.cexp|&$id:d|] | d <- dests]
      args' = [C.cexp|ctx|] : out_args ++ args
  item [C.citem|if ($id:(funName fname)($args:args') != 0) { err = 1; goto cleanup; }|]

defError :: ErrorCompiler op s
defError msg stacktrace = do
  (formatstr, formatargs) <- errorMsgString msg
  let formatstr' = "Error: " <> formatstr <> "\n\nBacktrace:\n%s"
  items
    [C.citems|set_error(ctx, msgprintf($string:formatstr', $args:formatargs, $string:stacktrace));
              err = FUTHARK_PROGRAM_ERROR;
              goto cleanup;|]

lmadcopyCPU :: DoCopy op s
lmadcopyCPU _ t shape dst (dstoffset, dststride) src (srcoffset, srcstride) = do
  let fname :: String
      (fname, ty) =
        case primByteSize t :: Int of
          1 -> ("lmad_copy_1b", [C.cty|typename uint8_t|])
          2 -> ("lmad_copy_2b", [C.cty|typename uint16_t|])
          4 -> ("lmad_copy_4b", [C.cty|typename uint32_t|])
          8 -> ("lmad_copy_8b", [C.cty|typename uint64_t|])
          k -> error $ "lmadcopyCPU: " <> show k
      r = length shape
      dststride_inits = [[C.cinit|$exp:e|] | Count e <- dststride]
      srcstride_inits = [[C.cinit|$exp:e|] | Count e <- srcstride]
      shape_inits = [[C.cinit|$exp:e|] | Count e <- shape]
  stm
    [C.cstm|
         $id:fname(ctx, $int:r,
                   ($ty:ty*) $exp:dst, $exp:(unCount dstoffset),
                   (typename int64_t[]){ $inits:dststride_inits },
                   ($ty:ty*) $exp:src, $exp:(unCount srcoffset),
                   (typename int64_t[]){ $inits:srcstride_inits },
                   (typename int64_t[]){ $inits:shape_inits });|]

-- | A set of operations that fail for every operation involving
-- non-default memory spaces.  Uses plain pointers and @malloc@ for
-- memory management.
defaultOperations :: Operations op s
defaultOperations =
  Operations
    { opsWriteScalar = defWriteScalar,
      opsReadScalar = defReadScalar,
      opsAllocate = defAllocate,
      opsDeallocate = defDeallocate,
      opsCopy = defCopy,
      opsCopies = M.singleton (DefaultSpace, DefaultSpace) lmadcopyCPU,
      opsMemoryType = defMemoryType,
      opsCompiler = defCompiler,
      opsFatMemory = True,
      opsError = defError,
      opsCall = defCall,
      opsCritical = mempty
    }
  where
    defWriteScalar _ _ _ _ _ =
      error "Cannot write to non-default memory space because I am dumb"
    defReadScalar _ _ _ _ =
      error "Cannot read from non-default memory space"
    defAllocate _ _ _ =
      error "Cannot allocate in non-default memory space"
    defDeallocate _ _ =
      error "Cannot deallocate in non-default memory space"
    defCopy _ destmem destoffset DefaultSpace srcmem srcoffset DefaultSpace size =
      copyMemoryDefaultSpace destmem destoffset srcmem srcoffset size
    defCopy _ _ _ _ _ _ _ _ =
      error "Cannot copy to or from non-default memory space"
    defMemoryType _ =
      error "Has no type for non-default memory space"
    defCompiler _ =
      error "The default compiler cannot compile extended operations"

declsCode :: (HeaderSection -> Bool) -> CompilerState s -> T.Text
declsCode p =
  definitionsText
    . concatMap (DL.toList . snd)
    . filter (p . fst)
    . M.toList
    . compHeaderDecls

initDecls, arrayDecls, opaqueDecls, opaqueTypeDecls, entryDecls, miscDecls :: CompilerState s -> T.Text
initDecls = declsCode (== InitDecl)
arrayDecls = declsCode isArrayDecl
  where
    isArrayDecl ArrayDecl {} = True
    isArrayDecl _ = False
opaqueTypeDecls = declsCode isOpaqueTypeDecl
  where
    isOpaqueTypeDecl OpaqueTypeDecl {} = True
    isOpaqueTypeDecl _ = False
opaqueDecls = declsCode isOpaqueDecl
  where
    isOpaqueDecl OpaqueDecl {} = True
    isOpaqueDecl _ = False
entryDecls = declsCode (== EntryDecl)
miscDecls = declsCode (== MiscDecl)

defineMemorySpace :: Space -> CompilerM op s ([C.Definition], C.BlockItem)
defineMemorySpace space = do
  rm <- rawMemCType space
  earlyDecl
    [C.cedecl|struct $id:sname { int *references;
                                 $ty:rm mem;
                                 typename int64_t size;
                                 const char *desc; };|]

  -- Unreferencing a memory block consists of decreasing its reference
  -- count and freeing the corresponding memory if the count reaches
  -- zero.
  free <- collect $ freeRawMem [C.cexp|block->mem|] [C.cexp|block->size|] space [C.cexp|desc|]
  ctx_ty <- contextType
  let unrefdef =
        [C.cedecl|int $id:(fatMemUnRef space) ($ty:ctx_ty *ctx, $ty:mty *block, const char *desc) {
  if (block->references != NULL) {
    *(block->references) -= 1;
    if (ctx->detail_memory) {
      fprintf(ctx->log, "Unreferencing block %s (allocated as %s) in %s: %d references remaining.\n",
                      desc, block->desc, $string:spacedesc, *(block->references));
    }
    if (*(block->references) == 0) {
      ctx->$id:usagename -= block->size;
      $items:free
      free(block->references);
      if (ctx->detail_memory) {
        fprintf(ctx->log, "%lld bytes freed (now allocated: %lld bytes)\n",
                (long long) block->size, (long long) ctx->$id:usagename);
      }
    }
    block->references = NULL;
  }
  return 0;
}|]

  -- When allocating a memory block we initialise the reference count to 1.
  alloc <-
    collect $
      allocRawMem [C.cexp|block->mem|] [C.cexp|size|] space [C.cexp|desc|]
  let allocdef =
        [C.cedecl|int $id:(fatMemAlloc space) ($ty:ctx_ty *ctx, $ty:mty *block, typename int64_t size, const char *desc) {
  if (size < 0) {
    futhark_panic(1, "Negative allocation of %lld bytes attempted for %s in %s.\n",
          (long long)size, desc, $string:spacedesc, ctx->$id:usagename);
  }
  int ret = $id:(fatMemUnRef space)(ctx, block, desc);

  if (ret != FUTHARK_SUCCESS) {
    return ret;
  }

  if (ctx->detail_memory) {
    fprintf(ctx->log, "Allocating %lld bytes for %s in %s (currently allocated: %lld bytes).\n",
            (long long) size,
            desc, $string:spacedesc,
            (long long) ctx->$id:usagename);
  }

  $items:alloc

  if (ctx->error == NULL) {
    block->references = (int*) malloc(sizeof(int));
    *(block->references) = 1;
    block->size = size;
    block->desc = desc;
    long long new_usage = ctx->$id:usagename + size;
    if (ctx->detail_memory) {
      fprintf(ctx->log, "Received block of %lld bytes; now allocated: %lld bytes",
              (long long)block->size, new_usage);
    }
    ctx->$id:usagename = new_usage;
    if (new_usage > ctx->$id:peakname) {
      ctx->$id:peakname = new_usage;
      if (ctx->detail_memory) {
        fprintf(ctx->log, " (new peak).\n");
      }
    } else if (ctx->detail_memory) {
        fprintf(ctx->log, ".\n");
    }
    return FUTHARK_SUCCESS;
  } else {
    // We are naively assuming that any memory allocation error is due to OOM.
    // We preserve the original error so that a savvy user can perhaps find
    // glory despite our naiveté.

    // We cannot use set_error() here because we want to replace the old error.
    lock_lock(&ctx->error_lock);
    char *old_error = ctx->error;
    ctx->error = msgprintf("Failed to allocate memory in %s.\nAttempted allocation: %12lld bytes\nCurrently allocated:  %12lld bytes\n%s",
                           $string:spacedesc,
                           (long long) size,
                           (long long) ctx->$id:usagename,
                           old_error);
    free(old_error);
    lock_unlock(&ctx->error_lock);
    return FUTHARK_OUT_OF_MEMORY;
  }
  }|]

  -- Memory setting - unreference the destination and increase the
  -- count of the source by one.
  let setdef =
        [C.cedecl|int $id:(fatMemSet space) ($ty:ctx_ty *ctx, $ty:mty *lhs, $ty:mty *rhs, const char *lhs_desc) {
  int ret = $id:(fatMemUnRef space)(ctx, lhs, lhs_desc);
  if (rhs->references != NULL) {
    (*(rhs->references))++;
  }
  *lhs = *rhs;
  return ret;
}
|]

  onClear [C.citem|ctx->$id:peakname = 0;|]

  let peakmsg = "\"" <> spacedesc <> "\": %lld"
  pure
    ( [unrefdef, allocdef, setdef],
      [C.citem|str_builder(&builder, $string:peakmsg, (long long) ctx->$id:peakname);|]
    )
  where
    mty = fatMemType space
    (peakname, usagename, sname, spacedesc) = case space of
      Space sid ->
        ( C.toIdent ("peak_mem_usage_" ++ sid) noLoc,
          C.toIdent ("cur_mem_usage_" ++ sid) noLoc,
          C.toIdent ("memblock_" ++ sid) noLoc,
          "space '" ++ sid ++ "'"
        )
      _ ->
        ( "peak_mem_usage_default",
          "cur_mem_usage_default",
          "memblock",
          "default space"
        )

-- | The result of compilation to C is multiple parts, which can be
-- put together in various ways.  The obvious way is to concatenate
-- all of them, which yields a CLI program.  Another is to compile the
-- library part by itself, and use the header file to call into it.
data CParts = CParts
  { cHeader :: T.Text,
    -- | Utility definitions that must be visible
    -- to both CLI and library parts.
    cUtils :: T.Text,
    cCLI :: T.Text,
    cServer :: T.Text,
    cLib :: T.Text,
    -- | The manifest, in JSON format.
    cJsonManifest :: T.Text
  }

gnuSource :: T.Text
gnuSource =
  [untrimming|
// We need to define _GNU_SOURCE before
// _any_ headers files are imported to get
// the usage statistics of a thread (i.e. have RUSAGE_THREAD) on GNU/Linux
// https://manpages.courier-mta.org/htmlman2/getrusage.2.html
#ifndef _GNU_SOURCE // Avoid possible double-definition warning.
#define _GNU_SOURCE
#endif
|]

-- We may generate variables that are never used (e.g. for
-- certificates) or functions that are never called (e.g. unused
-- intrinsics), and generated code may have other cosmetic issues that
-- compilers warn about.  We disable these warnings to not clutter the
-- compilation logs.
disableWarnings :: T.Text
disableWarnings =
  [untrimming|
#ifdef __clang__
#pragma clang diagnostic ignored "-Wunused-function"
#pragma clang diagnostic ignored "-Wunused-variable"
#pragma clang diagnostic ignored "-Wunused-const-variable"
#pragma clang diagnostic ignored "-Wparentheses"
#pragma clang diagnostic ignored "-Wunused-label"
#pragma clang diagnostic ignored "-Wunused-but-set-variable"
#elif __GNUC__
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-const-variable"
#pragma GCC diagnostic ignored "-Wparentheses"
#pragma GCC diagnostic ignored "-Wunused-label"
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#endif
|]

-- | Produce header, implementation, and manifest files.
asLibrary :: CParts -> (T.Text, T.Text, T.Text)
asLibrary parts =
  ( "#pragma once\n\n" <> cHeader parts,
    gnuSource <> disableWarnings <> cHeader parts <> cUtils parts <> cLib parts,
    cJsonManifest parts
  )

-- | As executable with command-line interface.
asExecutable :: CParts -> T.Text
asExecutable parts =
  gnuSource <> disableWarnings <> cHeader parts <> cUtils parts <> cCLI parts <> cLib parts

-- | As server executable.
asServer :: CParts -> T.Text
asServer parts =
  gnuSource <> disableWarnings <> cHeader parts <> cUtils parts <> cServer parts <> cLib parts

relevantParams :: Name -> ParamMap -> [Name]
relevantParams fname m =
  map fst $ filter ((fname `S.member`) . snd . snd) $ M.toList m

compileProg' ::
  (MonadFreshNames m) =>
  T.Text ->
  T.Text ->
  ParamMap ->
  Operations op s ->
  s ->
  CompilerM op s () ->
  T.Text ->
  (Space, [Space]) ->
  [Option] ->
  Definitions op ->
  m (CParts, CompilerState s)
compileProg' backend version params ops def extra header_extra (arr_space, spaces) options prog = do
  src <- getNameSource
  let ((prototypes, definitions, entry_point_decls, manifest), endstate) =
        runCompilerM ops src def compileProgAction
      initdecls = initDecls endstate
      entrydecls = entryDecls endstate
      arraydecls = arrayDecls endstate
      opaquetypedecls = opaqueTypeDecls endstate
      opaquedecls = opaqueDecls endstate
      miscdecls = miscDecls endstate

  let headerdefs =
        [untrimming|
// Headers
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>
#include <float.h>
$header_extra
#ifdef __cplusplus
extern "C" {
#endif

// Initialisation
$initdecls

// Arrays
$arraydecls

// Opaque values
$opaquetypedecls
$opaquedecls

// Entry points
$entrydecls

// Miscellaneous
$miscdecls
#define FUTHARK_BACKEND_$backend
$errorsH

#ifdef __cplusplus
}
#endif
|]

  let utildefs =
        [untrimming|
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <stdint.h>
// If NDEBUG is set, the assert() macro will do nothing. Since Futhark
// (unfortunately) makes use of assert() for error detection (and even some
// side effects), we want to avoid that.
#undef NDEBUG
#include <assert.h>
#include <stdarg.h>
#define SCALAR_FUN_ATTR static inline
$utilH
$cacheH
$halfH
$timingH
$lockH
$freeListH
$eventListH
|]

  let early_decls = definitionsText $ DL.toList $ compEarlyDecls endstate
      lib_decls = definitionsText $ DL.toList $ compLibDecls endstate
      clidefs = cliDefs options manifest
      serverdefs = serverDefs options manifest
      libdefs =
        [untrimming|
#ifdef _MSC_VER
#define inline __inline
#endif
#include <string.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include <ctype.h>

$header_extra

#define FUTHARK_F64_ENABLED

$cScalarDefs

$contextPrototypesH

$early_decls

$contextH

$copyH

#define FUTHARK_FUN_ATTR static

$prototypes

$lib_decls

$definitions

$entry_point_decls
  |]

  pure
    ( CParts
        { cHeader = headerdefs,
          cUtils = utildefs,
          cCLI = clidefs,
          cServer = serverdefs,
          cLib = libdefs,
          cJsonManifest = Manifest.manifestToJSON manifest
        },
      endstate
    )
  where
    Definitions types consts (Functions funs) = prog

    compileProgAction = do
      (memfuns, memreport) <- mapAndUnzipM defineMemorySpace spaces

      get_consts <- compileConstants consts

      ctx_ty <- contextType

      (prototypes, functions) <-
        mapAndUnzipM (compileFun get_consts [[C.cparam|$ty:ctx_ty *ctx|]]) funs

      (entry_points, entry_points_manifest) <-
        fmap (unzip . catMaybes) $ forM funs $ \(fname, fun) ->
          onEntryPoint get_consts (relevantParams fname params) fname fun

      headerDecl InitDecl [C.cedecl|struct futhark_context_config;|]
      headerDecl InitDecl [C.cedecl|struct futhark_context_config* futhark_context_config_new(void);|]
      headerDecl InitDecl [C.cedecl|void futhark_context_config_free(struct futhark_context_config* cfg);|]
      headerDecl InitDecl [C.cedecl|int futhark_context_config_set_tuning_param(struct futhark_context_config *cfg, const char *param_name, size_t new_value);|]

      headerDecl InitDecl [C.cedecl|struct futhark_context;|]
      headerDecl InitDecl [C.cedecl|struct futhark_context* futhark_context_new(struct futhark_context_config* cfg);|]
      headerDecl InitDecl [C.cedecl|void futhark_context_free(struct futhark_context* cfg);|]
      headerDecl MiscDecl [C.cedecl|int futhark_context_sync(struct futhark_context* ctx);|]

      generateTuningParams params
      extra

      let set_tuning_params =
            zipWith
              (\i k -> [C.cstm|ctx->tuning_params.$id:k = &ctx->cfg->tuning_params[$int:i];|])
              [(0 :: Int) ..]
              $ M.keys params
      earlyDecl
        [C.cedecl|static void set_tuning_params(struct futhark_context* ctx) {
                    (void)ctx;
                    $stms:set_tuning_params
                  }|]

      mapM_ earlyDecl $ concat memfuns
      type_funs <- generateAPITypes arr_space types
      generateCommonLibFuns memreport

      pure
        ( definitionsText prototypes,
          funcsText functions,
          definitionsText entry_points,
          Manifest.Manifest (M.fromList entry_points_manifest) type_funs backend version
        )

-- | Compile imperative program to a C program.  Always uses the
-- function named "main" as entry point, so make sure it is defined.
compileProg ::
  (MonadFreshNames m) =>
  T.Text ->
  T.Text ->
  ParamMap ->
  Operations op () ->
  CompilerM op () () ->
  T.Text ->
  (Space, [Space]) ->
  [Option] ->
  Definitions op ->
  m CParts
compileProg backend version params ops extra header_extra (arr_space, spaces) options prog =
  fst <$> compileProg' backend version params ops () extra header_extra (arr_space, spaces) options prog

generateTuningParams :: ParamMap -> CompilerM op a ()
generateTuningParams params = do
  let (param_names, (param_classes, _param_users)) =
        second unzip $ unzip $ M.toList params
      strinit s = [C.cinit|$string:(T.unpack s)|]
      intinit x = [C.cinit|$int:x|]
      size_name_inits = map (strinit . prettyText) param_names
      size_var_inits = map (strinit . zEncodeText . prettyText) param_names
      size_class_inits = map (strinit . prettyText) param_classes
      size_default_inits = map (intinit . fromMaybe 0 . sizeDefault) param_classes
      size_decls = map (\k -> [C.csdecl|typename int64_t *$id:k;|]) param_names
      num_params = length params
  earlyDecl [C.cedecl|struct tuning_params { int dummy; $sdecls:size_decls };|]
  earlyDecl [C.cedecl|static const int num_tuning_params = $int:num_params;|]
  earlyDecl [C.cedecl|static const char *tuning_param_names[] = { $inits:size_name_inits, NULL };|]
  earlyDecl [C.cedecl|static const char *tuning_param_vars[] = { $inits:size_var_inits, NULL };|]
  earlyDecl [C.cedecl|static const char *tuning_param_classes[] = { $inits:size_class_inits, NULL };|]
  earlyDecl [C.cedecl|static typename int64_t tuning_param_defaults[] = { $inits:size_default_inits, 0 };|]

generateCommonLibFuns :: [C.BlockItem] -> CompilerM op s ()
generateCommonLibFuns memreport = do
  ctx <- contextType
  cfg <- configType
  ops <- asks envOperations

  publicDef_ "context_config_set_debugging" InitDecl $ \s ->
    ( [C.cedecl|void $id:s($ty:cfg* cfg, int flag);|],
      [C.cedecl|void $id:s($ty:cfg* cfg, int flag) {
                         cfg->profiling = cfg->logging = cfg->debugging = flag;
                       }|]
    )

  publicDef_ "context_config_set_profiling" InitDecl $ \s ->
    ( [C.cedecl|void $id:s($ty:cfg* cfg, int flag);|],
      [C.cedecl|void $id:s($ty:cfg* cfg, int flag) {
                         cfg->profiling = flag;
                       }|]
    )

  publicDef_ "context_config_set_logging" InitDecl $ \s ->
    ( [C.cedecl|void $id:s($ty:cfg* cfg, int flag);|],
      [C.cedecl|void $id:s($ty:cfg* cfg, int flag) {
                         cfg->logging = flag;
                       }|]
    )

  publicDef_ "context_config_set_cache_file" MiscDecl $ \s ->
    ( [C.cedecl|void $id:s($ty:cfg* cfg, const char *f);|],
      [C.cedecl|void $id:s($ty:cfg* cfg, const char *f) {
                 cfg->cache_fname = strdup(f);
               }|]
    )

  publicDef_ "get_tuning_param_count" InitDecl $ \s ->
    ( [C.cedecl|int $id:s(void);|],
      [C.cedecl|int $id:s(void) {
                return num_tuning_params;
              }|]
    )

  publicDef_ "get_tuning_param_name" InitDecl $ \s ->
    ( [C.cedecl|const char* $id:s(int);|],
      [C.cedecl|const char* $id:s(int i) {
                return tuning_param_names[i];
              }|]
    )

  publicDef_ "get_tuning_param_class" InitDecl $ \s ->
    ( [C.cedecl|const char* $id:s(int);|],
      [C.cedecl|const char* $id:s(int i) {
                return tuning_param_classes[i];
              }|]
    )

  sync <- publicName "context_sync"
  let comma = [C.citem|str_builder_char(&builder, ',');|]
  publicDef_ "context_report" MiscDecl $ \s ->
    ( [C.cedecl|char* $id:s($ty:ctx *ctx);|],
      [C.cedecl|char* $id:s($ty:ctx *ctx) {
                 if ($id:sync(ctx) != 0) {
                   return NULL;
                 }

                 struct str_builder builder;
                 str_builder_init(&builder);
                 str_builder_char(&builder, '{');
                 str_builder_str(&builder, "\"memory\":{");
                 $items:(L.intersperse comma memreport)
                 str_builder_str(&builder, "},\"events\":[");
                 report_events_in_list(&ctx->event_list, &builder);
                 str_builder_str(&builder, "]}");
                 return builder.str;
               }|]
    )

  publicDef_ "context_get_error" MiscDecl $ \s ->
    ( [C.cedecl|char* $id:s($ty:ctx* ctx);|],
      [C.cedecl|char* $id:s($ty:ctx* ctx) {
                         char* error = ctx->error;
                         ctx->error = NULL;
                         return error;
                       }|]
    )

  publicDef_ "context_set_logging_file" MiscDecl $ \s ->
    ( [C.cedecl|void $id:s($ty:ctx* ctx, typename FILE* f);|],
      [C.cedecl|void $id:s($ty:ctx* ctx, typename FILE* f) {
                  ctx->log = f;
                }|]
    )

  publicDef_ "context_pause_profiling" MiscDecl $ \s ->
    ( [C.cedecl|void $id:s($ty:ctx* ctx);|],
      [C.cedecl|void $id:s($ty:ctx* ctx) {
                 ctx->profiling_paused = 1;
               }|]
    )

  publicDef_ "context_unpause_profiling" MiscDecl $ \s ->
    ( [C.cedecl|void $id:s($ty:ctx* ctx);|],
      [C.cedecl|void $id:s($ty:ctx* ctx) {
                 ctx->profiling_paused = 0;
               }|]
    )

  clears <- gets $ DL.toList . compClearItems
  publicDef_ "context_clear_caches" MiscDecl $ \s ->
    ( [C.cedecl|int $id:s($ty:ctx* ctx);|],
      [C.cedecl|int $id:s($ty:ctx* ctx) {
                  $items:(criticalSection ops clears)
                  return ctx->error != NULL;
                }|]
    )

compileConstants :: Constants op -> CompilerM op s [C.BlockItem]
compileConstants (Constants ps init_consts) = do
  ctx_ty <- contextType
  const_fields <- mapM constParamField ps
  earlyDecl [C.cedecl|struct constants { int dummy; $sdecls:const_fields };|]

  inNewFunction $ do
    -- We locally define macros for the constants, so that when we
    -- generate assignments to local variables, we actually assign into
    -- the constants struct.  This is not needed for functions, because
    -- they can only read constants, not write them.
    let (defs, undefs) = unzip $ map constMacro ps
    init_consts' <- collect $ do
      mapM_ resetMemConst ps
      compileCode init_consts
    decl_mem <- declAllocatedMem
    free_mem <- freeAllocatedMem
    libDecl
      [C.cedecl|static int init_constants($ty:ctx_ty *ctx) {
        (void)ctx;
        int err = 0;
        $items:defs
        $items:decl_mem
        $items:init_consts'
        $items:free_mem
        $items:undefs
        cleanup:
        return err;
      }|]

  inNewFunction $ do
    free_consts <- collect $ mapM_ freeConst ps
    libDecl
      [C.cedecl|static int free_constants($ty:ctx_ty *ctx) {
        (void)ctx;
        $items:free_consts
        return 0;
      }|]

  mapM getConst ps
  where
    constParamField (ScalarParam name bt) = do
      let ctp = primTypeToCType bt
      pure [C.csdecl|$ty:ctp $id:name;|]
    constParamField (MemParam name space) = do
      ty <- memToCType name space
      pure [C.csdecl|$ty:ty $id:name;|]

    constMacro p = ([C.citem|$escstm:def|], [C.citem|$escstm:undef|])
      where
        p' = T.unpack $ idText (C.toIdent (paramName p) mempty)
        def = "#define " ++ p' ++ " (" ++ "ctx->constants->" ++ p' ++ ")"
        undef = "#undef " ++ p'

    resetMemConst ScalarParam {} = pure ()
    resetMemConst (MemParam name space) = resetMem name space

    freeConst ScalarParam {} = pure ()
    freeConst (MemParam name space) = unRefMem [C.cexp|ctx->constants->$id:name|] space

    getConst (ScalarParam name bt) = do
      let ctp = primTypeToCType bt
      pure [C.citem|$ty:ctp $id:name = ctx->constants->$id:name;|]
    getConst (MemParam name space) = do
      ty <- memToCType name space
      pure [C.citem|$ty:ty $id:name = ctx->constants->$id:name;|]
