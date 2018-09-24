{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Futhark.CodeGen.Backends.CVulkan.Boilerplate
  ( generateBoilerplate
  ) where

import Debug.Trace -- REMOVE

import Data.Word
import Data.FileEmbed
import Data.List
import qualified Data.Map as M
import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C

import Futhark.CodeGen.ImpCode.Vulkan
import qualified Futhark.CodeGen.Backends.GenericC as GC

generateBoilerplate :: [Word32] -> [String] -> GC.CompilerM Vulkan () ()
generateBoilerplate spirv_code kernel_names = do
  traceM "1"
  let vulkan_h = $(embedStringFile "rts/c/vulkan.h")
      shader = "{" ++ intercalate "," (map show spirv_code) ++ "}"
      vulkan_boilerplate = [C.cunit|
        $esc:vulkan_h
        $esc:("const uint32_t *spirv_shader[] = " ++ shader ++ ";")|]

  traceM "2"
            
  cfg <- GC.publicDef "context_config" GC.InitDecl $ \s ->
    ([C.cedecl|struct $id:s;|],
     [C.cedecl|struct $id:s { struct vulkan_config vulkan; };|])

  traceM "3"
  
  GC.publicDef_ "context_config_new" GC.InitDecl $ \s ->
    ([C.cedecl|struct $id:cfg* $id:s(void);|],
     [C.cedecl|struct $id:cfg* $id:s(void) {
                         struct $id:cfg *cfg = malloc(sizeof(struct $id:cfg));
                         if (cfg == NULL) {
                           return NULL;
                         }

                         vulkan_config_init(&cfg->vulkan);

                         return cfg;
                       }|])

  traceM "4"

  GC.publicDef_ "context_config_free" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg) {
                         free(cfg);
                       }|])
                       
  traceM "5"

  GC.publicDef_ "context_config_set_debugging" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag) {
                         cfg->vulkan.logging = cfg->vulkan.debugging = flag;
                       }|])
                       
  traceM "6"

  GC.publicDef_ "context_config_set_logging" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, int flag);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, int flag) {
                         cfg->vulkan.logging = flag;
                       }|])
                       
  traceM "7"

  GC.publicDef_ "context_config_dump_program_to" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                         cfg->vulkan.dump_program_to = path;
                       }|])

  traceM "6"

  GC.publicDef_ "context_config_load_program_from" GC.InitDecl $ \s ->
    ([C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path);|],
     [C.cedecl|void $id:s(struct $id:cfg* cfg, const char *path) {
                         cfg->vulkan.load_program_from = path;
                       }|])

  traceM "7"
  
  return ()