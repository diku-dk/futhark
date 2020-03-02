{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Futhark.CodeGen.Backends.COpenGL.Boilerplate
  ( generateBoilerplate
  ) where

import Data.FileEmbed
import qualified Data.Map as M
import qualified Language.C.Syntax as C
import qualified Language.C.Quote as C

import qualified Language.C.Quote.OpenCL as C

import Futhark.CodeGen.ImpCode.OpenGL
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.Util (chunk, zEncodeString)

generateBoilerplate :: String -> String -> [String]
                    -> M.Map Name SizeClass
                    -> GC.CompilerM OpenGL () ()
generateBoilerplate opengl_code opengl_prelude shader_names sizes = do

  let free_list_h = $(embedStringFile "rts/c/free_list.h")
      openGL_h    = $(embedStringFile "rts/c/opengl.h")
      -- fragments might need ctx_fields, ctx_inits and openGL_load
      fragments   = map (\s -> [C.cinit|$string:s|])
                        $ chunk 2000 (opengl_prelude ++ opengl_code)
      size_name_inits  = map (\k -> [C.cinit|$string:(pretty k)|]) $ M.keys sizes
      size_var_inits   = map (\k -> [C.cinit|$string:(zEncodeString (pretty k))|])
                                                                   $ M.keys sizes
      size_class_inits = map (\c -> [C.cinit|$string:(pretty c)|]) $ M.elems sizes
      num_sizes        = M.size sizes

  GC.earlyDecls [C.cunit|$esc:("#include <X11/X.h>")
                         $esc:("#include <X11/Xlib.h>")
                         $esc:("#include <GL/gl.h>")
                         $esc:("#include <GL/glx.h>")
                         $esc:("typedef GLuint fl_mem_t;")
                         $esc:free_list_h
                         $esc:openGL_h
                         const char *opengl_program[] = {$inits:fragments, NULL};|]

  GC.libDecl [C.cedecl|static const char *size_names[] = { $inits:size_name_inits };|]
  GC.libDecl [C.cedecl|static const char *size_vars[] = { $inits:size_var_inits };|]
  GC.libDecl [C.cedecl|static const char *size_classes[] = { $inits:size_class_inits };|]

  GC.publicDef_ "get_num_sizes" GC.InitDecl $ \s ->
    ([C.cedecl|int $id:s(void);|],
     [C.cedecl|int $id:s(void) {
                return $int:num_sizes;
              }|])

  GC.publicDef_ "get_size_name" GC.InitDecl $ \s ->
    ([C.cedecl|const char* $id:s(int);|],
     [C.cedecl|const char* $id:s(int i) {
                return size_names[i];
              }|])

  GC.publicDef_ "get_size_class" GC.InitDecl $ \s ->
    ([C.cedecl|const char* $id:s(int);|],
     [C.cedecl|const char* $id:s(int i) {
                return size_classes[i];
              }|])

  let size_decls = map (\k -> [C.csdecl|size_t $id:k;|]) $ M.keys sizes

  GC.libDecl [C.cedecl|struct sizes { $sdecls:size_decls };|]

  cfg <- GC.publicDef "context_config" GC.InitDecl $ \s ->
    ([C.cedecl|struct $id:s;|],
     [C.cedecl|struct $id:s { struct opengl_config opengl;
                              size_t sizes[$int:num_sizes];
                              int num_build_opts;
                              const char **build_opts;
                            };|])

  let size_value_inits = zipWith sizeInit [0..M.size sizes-1] (M.elems sizes)
      sizeInit i size = [C.cstm|cfg->sizes[$int:i] = $int:val;|]
         where val = case size of SizeBespoke _ x -> x
                                  _               -> 0

  GC.publicDef_ "context_config_new" GC.InitDecl $ \s ->
    ([C.cedecl|struct $id:cfg* $id:s(void);|],
     [C.cedecl|struct $id:cfg* $id:s(void) {
                         struct $id:cfg *cfg = (struct $id:cfg*) malloc(sizeof(struct $id:cfg));
                         if (cfg == NULL) {
                           return NULL;
                         }

                         cfg->num_build_opts = 0;
                         cfg->build_opts = (const char**) malloc(sizeof(const char*));
                         cfg->build_opts[0] = NULL;
                         $stms:size_value_inits
                         opengl_config_init(&cfg->opengl, $int:num_sizes,
                                            size_names, size_vars,
                                            cfg->sizes, size_classes);
                         return cfg;
                       }|])

  GC.publicDef_ "context_config_free" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg) {
                         free(cfg->build_opts);
                         free(cfg);
                       }|])

  GC.publicDef_ "context_config_add_build_option" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, const char *opt);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *opt) {
                         cfg->build_opts[cfg->num_build_opts] = opt;
                         cfg->num_build_opts++;
                         cfg->build_opts = (const char**) realloc(cfg->build_opts,
                                     (cfg->num_build_opts+1) * sizeof(const char*));
                         cfg->build_opts[cfg->num_build_opts] = NULL;
                       }|])

  GC.publicDef_ "context_config_set_debugging" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag) {
                         cfg->opengl.profiling = cfg->opengl.logging = cfg->opengl.debugging = flag;
                       }|])

  GC.publicDef_ "context_config_set_profiling" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag) {
                         cfg->opengl.profiling = flag;
                       }|])

  GC.publicDef_ "context_config_set_logging" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag) {
                         cfg->opengl.logging = flag;
                       }|])

  GC.publicDef_ "context_config_dump_program_to" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                         cfg->opengl.dump_program_to = path;
                       }|])

  GC.publicDef_ "context_config_load_program_from" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                         cfg->opengl.load_program_from = path;
                       }|])

  GC.publicDef_ "context_config_dump_binary_to" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                         cfg->opengl.dump_binary_to = path;
                       }|])

  GC.publicDef_ "context_config_load_binary_from" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                         cfg->opengl.load_binary_from = path;
                       }|])

  GC.publicDef_ "context_config_set_default_group_size" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int size);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int size) {
                         cfg->opengl.default_group_size = size;
                         cfg->opengl.default_group_size_changed = 1;
                       }|])

  GC.publicDef_ "context_config_set_default_num_groups" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int num);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int num) {
                         cfg->opengl.default_num_groups = num;
                       }|])

  GC.publicDef_ "context_config_set_default_tile_size" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int num);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int size) {
                         cfg->opengl.default_tile_size = size;
                         cfg->opengl.default_tile_size_changed = 1;
                       }|])

  GC.publicDef_ "context_config_set_default_threshold" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int num);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int size) {
                         cfg->opengl.default_threshold = size;
                       }|])

  GC.publicDef_ "context_config_set_size" GC.InitDecl $ \s ->
    ([C.cedecl|int $id:s(struct $id:cfg* cfg, const char *size_name, size_t size_value);|],
     [C.cedecl|int $id:s(struct $id:cfg* cfg, const char *size_name, size_t size_value) {

                         for (int i = 0; i < $int:num_sizes; i++) {
                           if (strcmp(size_name, size_names[i]) == 0) {
                             cfg->sizes[i] = size_value;
                             return 0;
                           }
                         }

                         if (strcmp(size_name, "default_group_size") == 0) {
                           cfg->opengl.default_group_size = size_value;
                           return 0;
                         }

                         if (strcmp(size_name, "default_num_groups") == 0) {
                           cfg->opengl.default_num_groups = size_value;
                           return 0;
                         }

                         if (strcmp(size_name, "default_threshold") == 0) {
                           cfg->opengl.default_threshold = size_value;
                           return 0;
                         }

                         if (strcmp(size_name, "default_tile_size") == 0) {
                           cfg->opengl.default_tile_size = size_value;
                           return 0;
                         }

                         return 1;
                       }|])

  (fields, init_fields) <- GC.contextContents
  let ctx_opengl_fields = map (\k -> [C.csdecl|typename CUfunction $id:k;|])
                              shader_names

  ctx <- GC.publicDef "context" GC.InitDecl $ \s ->
    ([C.cedecl|struct $id:s;|],
     [C.cedecl|struct $id:s {
                         int detail_memory;
                         int debugging;
                         int profiling;
                         int profiling_paused;
                         int logging;
                         typename lock_t lock;
                         char *error;
                         $sdecls:fields
                         $sdecls:ctx_opengl_fields
                         struct opengl_context opengl;
                         struct sizes sizes;
                       };|])

  let set_sizes = zipWith (\i k -> [C.cstm|ctx->sizes.$id:k = cfg->sizes[$int:i];|])
                          [(0::Int)..] $ M.keys sizes

--TODO:
-- GC.libDecl

-- GC.libDecl

  GC.publicDef_ "context_new" GC.InitDecl $ \s ->
    ([C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg);|],
     [C.cedecl|struct $id:ctx* $id:s(struct $id:cfg* cfg) {
                          struct $id:ctx* ctx = (struct $id:ctx*) malloc(sizeof(struct $id:ctx));
                          if (ctx == NULL) {
                            return NULL;
                          }

                          setup_opengl(&ctx->opengl, opengl_program, cfg->build_opts);

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
                         return ctx->error != NULL;
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
