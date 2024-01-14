{-# LANGUAGE QuasiQuotes #-}

-- | Code generation for server executables.
module Futhark.CodeGen.Backends.GenericC.Server
  ( serverDefs,
  )
where

import Data.Bifunctor (first, second)
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.GenericC.Pretty
import Futhark.CodeGen.Backends.SimpleRep
import Futhark.CodeGen.RTS.C (serverH, tuningH, valuesH)
import Futhark.Manifest
import Futhark.Util (zEncodeText)
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C
import Language.Futhark.Core (nameFromText)

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
      { optionLongName = "profile",
        optionShortName = Just 'P',
        optionArgument = NoArgument,
        optionDescription = "Enable the collection of profiling information.",
        optionAction = [C.cstm|futhark_context_config_set_profiling(cfg, 1);|]
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
      },
    Option
      { optionLongName = "cache-file",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "FILE",
        optionDescription = "Store program cache here.",
        optionAction =
          [C.cstm|futhark_context_config_set_cache_file(cfg, optarg);|]
      }
  ]

typeStructName :: T.Text -> T.Text
typeStructName tname = "type_" <> zEncodeText tname

cType :: Manifest -> TypeName -> C.Type
cType manifest tname =
  case M.lookup tname $ manifestTypes manifest of
    Just (TypeArray ctype _ _ _) -> [C.cty|typename $id:(T.unpack ctype)|]
    Just (TypeOpaque ctype _ _ _) -> [C.cty|typename $id:(T.unpack ctype)|]
    Nothing -> uncurry primAPIType $ scalarToPrim tname

-- First component is forward declaration so we don't have to worry
-- about ordering.
typeBoilerplate :: Manifest -> (T.Text, Type) -> (C.Definition, C.Initializer, [C.Definition])
typeBoilerplate _ (tname, TypeArray _ et rank ops) =
  let type_name = typeStructName tname
      aux_name = type_name <> "_aux"
      info_name = et <> "_info"
      shape_args = [[C.cexp|shape[$int:i]|] | i <- [0 .. rank - 1]]
      array_new_wrap = arrayNew ops <> "_wrap"
   in ( [C.cedecl|const struct type $id:type_name;|],
        [C.cinit|&$id:type_name|],
        [C.cunit|
              void* $id:array_new_wrap(struct futhark_context *ctx,
                                       const void* p,
                                       const typename int64_t* shape) {
                return $id:(arrayNew ops)(ctx, p, $args:shape_args);
              }
              const struct array_aux $id:aux_name = {
                .name = $string:(T.unpack tname),
                .rank = $int:rank,
                .info = &$id:info_name,
                .new = (typename array_new_fn)$id:array_new_wrap,
                .free = (typename array_free_fn)$id:(arrayFree ops),
                .shape = (typename array_shape_fn)$id:(arrayShape ops),
                .values = (typename array_values_fn)$id:(arrayValues ops)
              };
              const struct type $id:type_name = {
                .name = $string:(T.unpack tname),
                .restore = (typename restore_fn)restore_array,
                .store = (typename store_fn)store_array,
                .free = (typename free_fn)free_array,
                .aux = &$id:aux_name
              };|]
      )
typeBoilerplate manifest (tname, TypeOpaque c_type_name ops record _sumops) =
  let type_name = typeStructName tname
      aux_name = type_name <> "_aux"
      (record_edecls, record_init) = recordDefs type_name record
   in ( [C.cedecl|const struct type $id:type_name;|],
        [C.cinit|&$id:type_name|],
        record_edecls
          ++ [C.cunit|
              const struct opaque_aux $id:aux_name = {
                .store = (typename opaque_store_fn)$id:(opaqueStore ops),
                .restore = (typename opaque_restore_fn)$id:(opaqueRestore ops),
                .free = (typename opaque_free_fn)$id:(opaqueFree ops)
              };
              const struct type $id:type_name = {
                .name = $string:(T.unpack tname),
                .restore = (typename restore_fn)restore_opaque,
                .store = (typename store_fn)store_opaque,
                .free = (typename free_fn)free_opaque,
                .aux = &$id:aux_name,
                .record = $init:record_init
              };|]
      )
  where
    recordDefs _ Nothing = ([], [C.cinit|NULL|])
    recordDefs type_name (Just (RecordOps fields new)) =
      let new_wrap = new <> "_wrap"
          record_name = type_name <> "_record"
          fields_name = type_name <> "_fields"
          onField i (RecordField name field_tname project) =
            let field_c_type = cType manifest field_tname
                field_v = "v" <> show (i :: Int)
             in ( [C.cinit|{.name = $string:(T.unpack name),
                            .type = &$id:(typeStructName field_tname),
                            .project = (typename project_fn)$id:project
                           }|],
                  [C.citem|const $ty:field_c_type $id:field_v =
                            *(const $ty:field_c_type*)fields[$int:i];|],
                  [C.cexp|$id:field_v|]
                )
          (field_inits, get_fields, field_args) = unzip3 $ zipWith onField [0 ..] fields
       in ( [C.cunit|
             const struct field $id:fields_name[] = {
               $inits:field_inits
             };
             int $id:new_wrap(struct futhark_context* ctx, void** outp, const void* fields[]) {
               typename $id:c_type_name *out = (typename $id:c_type_name*) outp;
               $items:get_fields
               return $id:new(ctx, out, $args:field_args);
             }
             const struct record $id:record_name = {
               .num_fields = $int:(length fields),
               .fields = $id:fields_name,
               .new = $id:new_wrap
             };|],
            [C.cinit|&$id:record_name|]
          )

entryTypeBoilerplate :: Manifest -> ([C.Definition], [C.Initializer], [C.Definition])
entryTypeBoilerplate manifest =
  second concat . unzip3 . map (typeBoilerplate manifest) . M.toList . manifestTypes $
    manifest

oneEntryBoilerplate :: Manifest -> (T.Text, EntryPoint) -> ([C.Definition], C.Initializer)
oneEntryBoilerplate manifest (name, EntryPoint cfun tuning_params outputs inputs) =
  let call_f = "call_" <> nameFromText name
      out_types = map outputType outputs
      in_types = map inputType inputs
      out_types_name = nameFromText name <> "_out_types"
      in_types_name = nameFromText name <> "_in_types"
      out_unique_name = nameFromText name <> "_out_unique"
      in_unique_name = nameFromText name <> "_in_unique"
      tuning_params_name = nameFromText name <> "_tuning_params"
      (out_items, out_args)
        | null out_types = ([C.citems|(void)outs;|], mempty)
        | otherwise = unzip $ zipWith loadOut [0 ..] out_types
      (in_items, in_args)
        | null in_types = ([C.citems|(void)ins;|], mempty)
        | otherwise = unzip $ zipWith loadIn [0 ..] in_types
   in ( [C.cunit|
                const struct type* $id:out_types_name[] = {
                  $inits:(map typeStructInit out_types),
                  NULL
                };
                bool $id:out_unique_name[] = {
                  $inits:(map outputUniqueInit outputs)
                };
                const struct type* $id:in_types_name[] = {
                  $inits:(map typeStructInit in_types),
                  NULL
                };
                bool $id:in_unique_name[] = {
                  $inits:(map inputUniqueInit inputs)
                };
                const char* $id:tuning_params_name[] = {
                  $inits:(map textInit tuning_params),
                  NULL
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
            .tuning_params = $id:tuning_params_name,
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

    loadOut i tname =
      let v = "out" ++ show (i :: Int)
       in ( [C.citem|$ty:(cType manifest tname) *$id:v = outs[$int:i];|],
            [C.cexp|$id:v|]
          )
    loadIn i tname =
      let v = "in" ++ show (i :: Int)
       in ( [C.citem|$ty:(cType manifest tname) $id:v = *($ty:(cType manifest tname)*)ins[$int:i];|],
            [C.cexp|$id:v|]
          )

    textInit t = [C.cinit|$string:(T.unpack t)|]

entryBoilerplate :: Manifest -> ([C.Definition], [C.Initializer])
entryBoilerplate manifest =
  first concat $
    unzip $
      map (oneEntryBoilerplate manifest) $
        M.toList $
          manifestEntryPoints manifest

mkBoilerplate ::
  Manifest ->
  ([C.Definition], [C.Initializer], [C.Initializer])
mkBoilerplate manifest =
  let (type_decls, type_inits, type_defs) = entryTypeBoilerplate manifest
      (entry_defs, entry_inits) = entryBoilerplate manifest
      scalar_type_inits = map scalarTypeInit scalar_types
   in (type_decls ++ type_defs ++ entry_defs, scalar_type_inits ++ type_inits, entry_inits)
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
   in definitionsText
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

const struct type* types[] = {
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
