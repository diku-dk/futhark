{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TupleSections #-}

module Futhark.CodeGen.Backends.GenericC.Server
  ( serverDefs,
  )
where

import Data.Bifunctor (first, second)
import Data.FileEmbed
import qualified Data.Map as M
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.SimpleRep
import Futhark.CodeGen.ImpCode
import qualified Language.C.Quote.OpenCL as C
import qualified Language.C.Syntax as C

genericOptions :: [Option]
genericOptions =
  [ Option
      { optionLongName = "debugging",
        optionShortName = Just 'D',
        optionArgument = NoArgument,
        optionDescription = "Perform possibly expensive internal correctness checks and verbose logging.",
        optionAction = [C.cstm|futhark_context_config_set_debugging(cfg, 1);|]
      },
    Option
      { optionLongName = "log",
        optionShortName = Just 'L',
        optionArgument = NoArgument,
        optionDescription = "Print various low-overhead logging information while running.",
        optionAction = [C.cstm|futhark_context_config_set_logging(cfg, 1);|]
      },
    Option
      { optionLongName = "help",
        optionShortName = Just 'h',
        optionArgument = NoArgument,
        optionDescription = "Print help information and exit.",
        optionAction =
          [C.cstm|{
                   printf("Usage: %s [OPTION]...\nOptions:\n\n%s\nFor more information, consult the Futhark User's Guide or the man pages.\n",
                          fut_progname, option_descriptions);
                   exit(0);
                  }|]
      }
  ]

typeStructName :: ExternalValue -> String
typeStructName (TransparentValue (ScalarValue pt signed _)) =
  let name = prettySigned (signed == TypeUnsigned) pt
   in "type_" ++ name
typeStructName (TransparentValue (ArrayValue _ _ pt signed shape)) =
  let rank = length shape
      name = arrayName pt signed rank
   in "type_" ++ name
typeStructName (OpaqueValue name vds) =
  "type_" ++ opaqueName name vds

valueDescBoilerplate :: ExternalValue -> (String, (C.Initializer, [C.Definition]))
valueDescBoilerplate ev@(TransparentValue (ScalarValue pt signed _)) =
  let name = prettySigned (signed == TypeUnsigned) pt
      type_name = typeStructName ev
   in (name, ([C.cinit|&$id:type_name|], mempty))
valueDescBoilerplate ev@(TransparentValue (ArrayValue _ _ pt signed shape)) =
  let rank = length shape
      name = arrayName pt signed rank
      pt_name = prettySigned (signed == TypeUnsigned) pt
      pretty_name = concat (replicate rank "[]") ++ pt_name
      type_name = typeStructName ev
      aux_name = type_name ++ "_aux"
      info_name = pt_name ++ "_info"
      array_new = "futhark_new_" ++ name
      array_new_wrap = "futhark_new_" ++ name ++ "_wrap"
      array_free = "futhark_free_" ++ name
      array_shape = "futhark_shape_" ++ name
      array_values = "futhark_values_" ++ name
      shape_args = [[C.cexp|shape[$int:i]|] | i <- [0 .. rank -1]]
   in ( name,
        ( [C.cinit|&$id:type_name|],
          [C.cunit|
              void* $id:array_new_wrap(struct futhark_context *ctx,
                                       const void* p,
                                       const typename int64_t* shape) {
                return $id:array_new(ctx, p, $args:shape_args);
              }
              struct array_aux $id:aux_name = {
                .name = $string:pretty_name,
                .rank = $int:rank,
                .info = &$id:info_name,
                .new = (typename array_new_fn)$id:array_new_wrap,
                .free = (typename array_free_fn)$id:array_free,
                .shape = (typename array_shape_fn)$id:array_shape,
                .values = (typename array_values_fn)$id:array_values
              };
              struct type $id:type_name = {
                .name = $string:pretty_name,
                .restore = (typename restore_fn)restore_array,
                .store = (typename store_fn)store_array,
                .free = (typename free_fn)free_array,
                .aux = &$id:aux_name
              };|]
        )
      )
valueDescBoilerplate ev@(OpaqueValue name vds) =
  let type_name = typeStructName ev
      aux_name = type_name ++ "_aux"
      opaque_free = "futhark_free_" ++ opaqueName name vds
      opaque_store = "futhark_store_" ++ opaqueName name vds
      opaque_restore = "futhark_restore_" ++ opaqueName name vds
   in ( name,
        ( [C.cinit|&$id:type_name|],
          [C.cunit|
              struct opaque_aux $id:aux_name = {
                .free = (typename opaque_free_fn)$id:opaque_free
              };
              struct type $id:type_name = {
                .name = $string:name,
                .restore = (typename restore_fn)$id:opaque_store,
                .store = (typename store_fn)$id:opaque_restore,
                .free = (typename free_fn)free_opaque,
                .aux = &$id:aux_name
              };|]
        )
      )

functionExternalValues :: Function a -> [ExternalValue]
functionExternalValues fun = functionResult fun ++ functionArgs fun

entryTypeBoilerplate :: Functions a -> ([C.Initializer], [C.Definition])
entryTypeBoilerplate (Functions funs) =
  second concat $
    unzip $
      M.elems $
        M.fromList $
          map valueDescBoilerplate $
            concatMap (functionExternalValues . snd) $
              filter (functionEntry . snd) funs

oneEntryBoilerplate :: (Name, Function a) -> ([C.Definition], C.Initializer)
oneEntryBoilerplate (name, fun) =
  let entry_f = "futhark_entry_" ++ pretty name
      call_f = "call_" ++ pretty name
      out_types = functionResult fun
      in_types = functionArgs fun
      out_types_name = pretty name ++ "_out_types"
      in_types_name = pretty name ++ "_in_types"
      (out_items, out_args)
        | null out_types = ([C.citems|(void)outs;|], mempty)
        | otherwise = unzip $ zipWith loadOut [0 ..] out_types
      (in_items, in_args)
        | null in_types = ([C.citems|(void)ins;|], mempty)
        | otherwise = unzip $ zipWith loadIn [0 ..] in_types
   in ( [C.cunit|
                struct type* $id:out_types_name[] = {
                  $inits:(map typeStructInit out_types),
                  NULL
                };
                struct type* $id:in_types_name[] = {
                  $inits:(map typeStructInit in_types),
                  NULL
                };
                int $id:call_f(struct futhark_context *ctx, void **outs, void **ins) {
                  $items:out_items
                  $items:in_items
                  return $id:entry_f(ctx, $args:out_args, $args:in_args);
                }
                |],
        [C.cinit|{
            .name = $string:(pretty name),
            .f = $id:call_f,
            .in_types = $id:in_types_name,
            .out_types = $id:out_types_name
            }|]
      )
  where
    typeStructInit t = [C.cinit|&$id:(typeStructName t)|]

    loadOut i ev =
      let v = "out" ++ show (i :: Int)
       in ( [C.citem|$ty:(externalValueType ev) *$id:v = outs[$int:i];|],
            [C.cexp|$id:v|]
          )
    loadIn i ev =
      let v = "in" ++ show (i :: Int)
          evt = externalValueType ev
       in ( [C.citem|$ty:evt $id:v = *($ty:evt*)ins[$int:i];|],
            [C.cexp|$id:v|]
          )

entryBoilerplate :: Functions a -> ([C.Definition], [C.Initializer])
entryBoilerplate (Functions funs) =
  first concat $ unzip $ map oneEntryBoilerplate $ filter (functionEntry . snd) funs

mkBoilerplate ::
  Functions a ->
  ([C.Definition], [C.Initializer], [C.Initializer])
mkBoilerplate funs =
  let (type_inits, type_defs) = entryTypeBoilerplate funs
      (entry_defs, entry_inits) = entryBoilerplate funs
   in (type_defs ++ entry_defs, type_inits, entry_inits)

{-# NOINLINE serverDefs #-}
serverDefs :: [Option] -> Functions a -> [C.Definition]
serverDefs options funs =
  let server_h = $(embedStringFile "rts/c/server.h")
      values_h = $(embedStringFile "rts/c/values.h")
      tuning_h = $(embedStringFile "rts/c/tuning.h")
      option_parser =
        generateOptionParser "parse_options" $ genericOptions ++ options
      (boilerplate_defs, type_inits, entry_point_inits) =
        mkBoilerplate funs
   in [C.cunit|
$esc:("#include <getopt.h>")
$esc:("#include <ctype.h>")
$esc:("#include <inttypes.h>")

// If the entry point is NULL, the program will terminate after doing initialisation and such.  It is not used for anything else in server mode.
static const char *entry_point = "main";

$esc:values_h
$esc:server_h
$esc:tuning_h

$edecls:boilerplate_defs

struct type* types[] = {
  $inits:type_inits,
  NULL
};

struct entry_point entry_points[] = {
  $inits:entry_point_inits,
  { .name = NULL }
};

struct futhark_prog prog = {
  .types = types,
  .entry_points = entry_points
};

$func:option_parser

int main(int argc, char** argv) {
  fut_progname = argv[0];

  struct futhark_context_config *cfg = futhark_context_config_new();
  assert(cfg != NULL);

  int parsed_options = parse_options(cfg, argc, argv);
  argc -= parsed_options;
  argv += parsed_options;

  if (argc != 0) {
    futhark_panic(1, "Excess non-option: %s\n", argv[0]);
  }

  struct futhark_context *ctx = futhark_context_new(cfg);
  assert (ctx != NULL);

  futhark_context_set_logging_file(ctx, stdout);

  char* error = futhark_context_get_error(ctx);
  if (error != NULL) {
    futhark_panic(1, "Error during context initialisation:\n%s", error);
  }

  if (entry_point != NULL) {
    run_server(&prog, ctx);
  }

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
|]
