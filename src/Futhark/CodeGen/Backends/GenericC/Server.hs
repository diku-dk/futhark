{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TupleSections #-}

-- | Code generation for server executables.
module Futhark.CodeGen.Backends.GenericC.Server
  ( serverDefs,
  )
where

import Data.Bifunctor (first, second)
import qualified Data.Map as M
import qualified Data.Text as T
import Futhark.CodeGen.Backends.GenericC.Manifest
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.SimpleRep
import Futhark.CodeGen.RTS.C (serverH, tuningH, valuesH)
import Futhark.Util (zEncodeString)
import Futhark.Util.Pretty (prettyText)
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
                   printf("Usage: %s [OPTIONS]...\nOptions:\n\n%s\nFor more information, consult the Futhark User's Guide or the man pages.\n",
                          fut_progname, option_descriptions);
                   exit(0);
                  }|]
      },
    Option
      { optionLongName = "print-params",
        optionShortName = Nothing,
        optionArgument = NoArgument,
        optionDescription = "Print all tuning parameters that can be set with --param or --tuning.",
        optionAction =
          [C.cstm|{
                int n = futhark_get_tuning_param_count();
                for (int i = 0; i < n; i++) {
                  printf("%s (%s)\n", futhark_get_tuning_param_name(i),
                                      futhark_get_tuning_param_class(i));
                }
                exit(0);
              }|]
      },
    Option
      { optionLongName = "param",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "ASSIGNMENT",
        optionDescription = "Set a tuning parameter to the given value.",
        optionAction =
          [C.cstm|{
                char *name = optarg;
                char *equals = strstr(optarg, "=");
                char *value_str = equals != NULL ? equals+1 : optarg;
                int value = atoi(value_str);
                if (equals != NULL) {
                  *equals = 0;
                  if (futhark_context_config_set_tuning_param(cfg, name, value) != 0) {
                    futhark_panic(1, "Unknown size: %s\n", name);
                  }
                } else {
                  futhark_panic(1, "Invalid argument for size option: %s\n", optarg);
                }}|]
      },
    Option
      { optionLongName = "tuning",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "FILE",
        optionDescription = "Read size=value assignments from the given file.",
        optionAction =
          [C.cstm|{
                char *ret = load_tuning_file(optarg, cfg, (int(*)(void*, const char*, size_t))
                                                          futhark_context_config_set_tuning_param);
                if (ret != NULL) {
                  futhark_panic(1, "When loading tuning from '%s': %s\n", optarg, ret);
                }}|]
      }
  ]

typeStructName :: T.Text -> String
typeStructName tname = "type_" <> zEncodeString (T.unpack tname)

typeBoilerplate :: (T.Text, Type) -> (C.Initializer, [C.Definition])
typeBoilerplate (tname, TypeArray _ et rank ops) =
  let type_name = typeStructName tname
      aux_name = type_name ++ "_aux"
      info_name = T.unpack et ++ "_info"
      shape_args = [[C.cexp|shape[$int:i]|] | i <- [0 .. rank -1]]
      array_new_wrap = arrayNew ops <> "_wrap"
   in ( [C.cinit|&$id:type_name|],
        [C.cunit|
              void* $id:array_new_wrap(struct futhark_context *ctx,
                                       const void* p,
                                       const typename int64_t* shape) {
                return $id:(arrayNew ops)(ctx, p, $args:shape_args);
              }
              struct array_aux $id:aux_name = {
                .name = $string:(T.unpack tname),
                .rank = $int:rank,
                .info = &$id:info_name,
                .new = (typename array_new_fn)$id:array_new_wrap,
                .free = (typename array_free_fn)$id:(arrayFree ops),
                .shape = (typename array_shape_fn)$id:(arrayShape ops),
                .values = (typename array_values_fn)$id:(arrayValues ops)
              };
              struct type $id:type_name = {
                .name = $string:(T.unpack tname),
                .restore = (typename restore_fn)restore_array,
                .store = (typename store_fn)store_array,
                .free = (typename free_fn)free_array,
                .aux = &$id:aux_name
              };|]
      )
typeBoilerplate (tname, TypeOpaque _ ops) =
  let type_name = typeStructName tname
      aux_name = type_name ++ "_aux"
   in ( [C.cinit|&$id:type_name|],
        [C.cunit|
              struct opaque_aux $id:aux_name = {
                .store = (typename opaque_store_fn)$id:(opaqueStore ops),
                .restore = (typename opaque_restore_fn)$id:(opaqueRestore ops),
                .free = (typename opaque_free_fn)$id:(opaqueFree ops)
              };
              struct type $id:type_name = {
                .name = $string:(T.unpack tname),
                .restore = (typename restore_fn)restore_opaque,
                .store = (typename store_fn)store_opaque,
                .free = (typename free_fn)free_opaque,
                .aux = &$id:aux_name
              };|]
      )

entryTypeBoilerplate :: Manifest -> ([C.Initializer], [C.Definition])
entryTypeBoilerplate =
  second concat . unzip . map typeBoilerplate . M.toList . manifestTypes

oneEntryBoilerplate :: Manifest -> (T.Text, EntryPoint) -> ([C.Definition], C.Initializer)
oneEntryBoilerplate manifest (name, EntryPoint cfun outputs inputs) =
  let call_f = "call_" ++ T.unpack name
      out_types = map outputType outputs
      in_types = map inputType inputs
      out_types_name = T.unpack name ++ "_out_types"
      in_types_name = T.unpack name ++ "_in_types"
      out_unique_name = T.unpack name ++ "_out_unique"
      in_unique_name = T.unpack name ++ "_in_unique"
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
                bool $id:out_unique_name[] = {
                  $inits:(map outputUniqueInit outputs)
                };
                struct type* $id:in_types_name[] = {
                  $inits:(map typeStructInit in_types),
                  NULL
                };
                bool $id:in_unique_name[] = {
                  $inits:(map inputUniqueInit inputs)
                };
                int $id:call_f(struct futhark_context *ctx, void **outs, void **ins) {
                  $items:out_items
                  $items:in_items
                  return $id:cfun(ctx, $args:out_args, $args:in_args);
                }
                |],
        [C.cinit|{
            .name = $string:(T.unpack name),
            .f = $id:call_f,
            .in_types = $id:in_types_name,
            .out_types = $id:out_types_name,
            .in_unique = $id:in_unique_name,
            .out_unique = $id:out_unique_name
            }|]
      )
  where
    typeStructInit tname = [C.cinit|&$id:(typeStructName tname)|]
    inputUniqueInit = uniqueInit . inputUnique
    outputUniqueInit = uniqueInit . outputUnique
    uniqueInit True = [C.cinit|true|]
    uniqueInit False = [C.cinit|false|]

    cType tname =
      case M.lookup tname $ manifestTypes manifest of
        Just (TypeArray ctype _ _ _) -> [C.cty|typename $id:(T.unpack ctype)|]
        Just (TypeOpaque ctype _) -> [C.cty|typename $id:(T.unpack ctype)|]
        Nothing -> uncurry primAPIType $ scalarToPrim tname

    loadOut i tname =
      let v = "out" ++ show (i :: Int)
       in ( [C.citem|$ty:(cType tname) *$id:v = outs[$int:i];|],
            [C.cexp|$id:v|]
          )
    loadIn i tname =
      let v = "in" ++ show (i :: Int)
       in ( [C.citem|$ty:(cType tname) $id:v = *($ty:(cType tname)*)ins[$int:i];|],
            [C.cexp|$id:v|]
          )

entryBoilerplate :: Manifest -> ([C.Definition], [C.Initializer])
entryBoilerplate manifest =
  first concat $
    unzip $
      map (oneEntryBoilerplate manifest) $
        M.toList $ manifestEntryPoints manifest

mkBoilerplate ::
  Manifest ->
  ([C.Definition], [C.Initializer], [C.Initializer])
mkBoilerplate manifest =
  let (type_inits, type_defs) = entryTypeBoilerplate manifest
      (entry_defs, entry_inits) = entryBoilerplate manifest
      scalar_type_inits = map scalarTypeInit scalar_types
   in (type_defs ++ entry_defs, scalar_type_inits ++ type_inits, entry_inits)
  where
    scalarTypeInit tname = [C.cinit|&$id:(typeStructName tname)|]
    scalar_types =
      [ "i8",
        "i16",
        "i32",
        "i64",
        "u8",
        "u16",
        "u32",
        "u64",
        "f16",
        "f32",
        "f64",
        "bool"
      ]

{-# NOINLINE serverDefs #-}

-- | Generate Futhark server executable code.
serverDefs :: [Option] -> Manifest -> T.Text
serverDefs options manifest =
  let option_parser =
        generateOptionParser "parse_options" $ genericOptions ++ options
      (boilerplate_defs, type_inits, entry_point_inits) =
        mkBoilerplate manifest
   in prettyText
        [C.cunit|
$esc:("#include <getopt.h>")
$esc:("#include <ctype.h>")
$esc:("#include <inttypes.h>")

// If the entry point is NULL, the program will terminate after doing initialisation and such.  It is not used for anything else in server mode.
static const char *entry_point = "main";

$esc:(T.unpack valuesH)
$esc:(T.unpack serverH)
$esc:(T.unpack tuningH)

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
    run_server(&prog, cfg, ctx);
  }

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
|]
