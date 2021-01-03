{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TupleSections #-}

module Futhark.CodeGen.Backends.GenericC.CLI
  ( cliDefs,
  )
where

import Data.FileEmbed
import Data.List (unzip5)
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.SimpleRep
import Futhark.CodeGen.ImpCode
import qualified Language.C.Quote.OpenCL as C
import qualified Language.C.Syntax as C

genericOptions :: [Option]
genericOptions =
  [ Option
      { optionLongName = "write-runtime-to",
        optionShortName = Just 't',
        optionArgument = RequiredArgument "FILE",
        optionDescription = "Print the time taken to execute the program to the indicated file, an integral number of microseconds.",
        optionAction = set_runtime_file
      },
    Option
      { optionLongName = "runs",
        optionShortName = Just 'r',
        optionArgument = RequiredArgument "INT",
        optionDescription = "Perform NUM runs of the program.",
        optionAction = set_num_runs
      },
    Option
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
        optionDescription = "Print various low-overhead logging information to stderr while running.",
        optionAction = [C.cstm|futhark_context_config_set_logging(cfg, 1);|]
      },
    Option
      { optionLongName = "entry-point",
        optionShortName = Just 'e',
        optionArgument = RequiredArgument "NAME",
        optionDescription = "The entry point to run. Defaults to main.",
        optionAction = [C.cstm|if (entry_point != NULL) entry_point = optarg;|]
      },
    Option
      { optionLongName = "binary-output",
        optionShortName = Just 'b',
        optionArgument = NoArgument,
        optionDescription = "Print the program result in the binary output format.",
        optionAction = [C.cstm|binary_output = 1;|]
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
  where
    set_runtime_file =
      [C.cstm|{
          runtime_file = fopen(optarg, "w");
          if (runtime_file == NULL) {
            futhark_panic(1, "Cannot open %s: %s\n", optarg, strerror(errno));
          }
        }|]
    set_num_runs =
      [C.cstm|{
          num_runs = atoi(optarg);
          perform_warmup = 1;
          if (num_runs <= 0) {
            futhark_panic(1, "Need a positive number of runs, not %s\n", optarg);
          }
        }|]

valueDescToCType :: ValueDesc -> C.Type
valueDescToCType (ScalarValue pt signed _) =
  signedPrimTypeToCType signed pt
valueDescToCType (ArrayValue _ _ pt signed shape) =
  let name = "futhark_" ++ arrayName pt signed (length shape)
   in [C.cty|struct $id:name|]

opaqueToCType :: String -> [ValueDesc] -> C.Type
opaqueToCType desc vds =
  let name = "futhark_" ++ opaqueName desc vds
   in [C.cty|struct $id:name|]

externalValueToCType :: ExternalValue -> C.Type
externalValueToCType (TransparentValue vd) = valueDescToCType vd
externalValueToCType (OpaqueValue desc vds) = opaqueToCType desc vds

primTypeInfo :: PrimType -> Signedness -> C.Exp
primTypeInfo (IntType it) t = case (it, t) of
  (Int8, TypeUnsigned) -> [C.cexp|u8_info|]
  (Int16, TypeUnsigned) -> [C.cexp|u16_info|]
  (Int32, TypeUnsigned) -> [C.cexp|u32_info|]
  (Int64, TypeUnsigned) -> [C.cexp|u64_info|]
  (Int8, _) -> [C.cexp|i8_info|]
  (Int16, _) -> [C.cexp|i16_info|]
  (Int32, _) -> [C.cexp|i32_info|]
  (Int64, _) -> [C.cexp|i64_info|]
primTypeInfo (FloatType Float32) _ = [C.cexp|f32_info|]
primTypeInfo (FloatType Float64) _ = [C.cexp|f64_info|]
primTypeInfo Bool _ = [C.cexp|bool_info|]
primTypeInfo Cert _ = [C.cexp|bool_info|]

readPrimStm :: C.ToIdent a => a -> Int -> PrimType -> Signedness -> C.Stm
readPrimStm place i t ept =
  [C.cstm|if (read_scalar(stdin, &$exp:(primTypeInfo t ept), &$id:place) != 0) {
            futhark_panic(1, "Error when reading input #%d of type %s (errno: %s).\n",
                          $int:i,
                          $exp:(primTypeInfo t ept).type_name,
                          strerror(errno));
          }|]

readInput :: Int -> ExternalValue -> ([C.BlockItem], C.Stm, C.Stm, C.Stm, C.Exp)
readInput i (OpaqueValue desc _) =
  ( [C.citems|futhark_panic(1, "Cannot read input #%d of type %s\n", $int:i, $string:desc);|],
    [C.cstm|;|],
    [C.cstm|;|],
    [C.cstm|;|],
    [C.cexp|NULL|]
  )
readInput i (TransparentValue (ScalarValue t ept _)) =
  let dest = "read_value_" ++ show i
   in ( [C.citems|$ty:(primTypeToCType t) $id:dest;
                  $stm:(readPrimStm dest i t ept);|],
        [C.cstm|;|],
        [C.cstm|;|],
        [C.cstm|;|],
        [C.cexp|$id:dest|]
      )
readInput i (TransparentValue (ArrayValue _ _ t ept dims)) =
  let dest = "read_value_" ++ show i
      shape = "read_shape_" ++ show i
      arr = "read_arr_" ++ show i

      name = arrayName t ept rank
      arr_ty_name = "futhark_" ++ name
      ty = [C.cty|struct $id:arr_ty_name|]
      rank = length dims
      dims_exps = [[C.cexp|$id:shape[$int:j]|] | j <- [0 .. rank -1]]
      dims_s = concat $ replicate rank "[]"
      t' = signedPrimTypeToCType ept t

      new_array = "futhark_new_" ++ name
      free_array = "futhark_free_" ++ name

      items =
        [C.citems|
           $ty:ty *$id:dest;
           typename int64_t $id:shape[$int:rank];
           $ty:t' *$id:arr = NULL;
           errno = 0;
           if (read_array(stdin,
                          &$exp:(primTypeInfo t ept),
                          (void**) &$id:arr,
                          $id:shape,
                          $int:(length dims))
               != 0) {
             futhark_panic(1, "Cannot read input #%d of type %s%s (errno: %s).\n",
                           $int:i,
                           $string:dims_s,
                           $exp:(primTypeInfo t ept).type_name,
                           strerror(errno));
           }|]
   in ( items,
        [C.cstm|assert(($id:dest = $id:new_array(ctx, $id:arr, $args:dims_exps)) != NULL);|],
        [C.cstm|assert($id:free_array(ctx, $id:dest) == 0);|],
        [C.cstm|free($id:arr);|],
        [C.cexp|$id:dest|]
      )

readInputs :: [ExternalValue] -> [([C.BlockItem], C.Stm, C.Stm, C.Stm, C.Exp)]
readInputs = zipWith readInput [0 ..]

prepareOutputs :: [ExternalValue] -> [(C.BlockItem, C.Exp, C.Stm)]
prepareOutputs = zipWith prepareResult [(0 :: Int) ..]
  where
    prepareResult i ev = do
      let ty = externalValueToCType ev
          result = "result_" ++ show i

      case ev of
        TransparentValue ScalarValue {} ->
          ( [C.citem|$ty:ty $id:result;|],
            [C.cexp|$id:result|],
            [C.cstm|;|]
          )
        TransparentValue (ArrayValue _ _ t ept dims) ->
          let name = arrayName t ept $ length dims
              free_array = "futhark_free_" ++ name
           in ( [C.citem|$ty:ty *$id:result;|],
                [C.cexp|$id:result|],
                [C.cstm|assert($id:free_array(ctx, $id:result) == 0);|]
              )
        OpaqueValue desc vds ->
          let free_opaque = "futhark_free_" ++ opaqueName desc vds
           in ( [C.citem|$ty:ty *$id:result;|],
                [C.cexp|$id:result|],
                [C.cstm|assert($id:free_opaque(ctx, $id:result) == 0);|]
              )

printPrimStm :: (C.ToExp a, C.ToExp b) => a -> b -> PrimType -> Signedness -> C.Stm
printPrimStm dest val bt ept =
  [C.cstm|write_scalar($exp:dest, binary_output, &$exp:(primTypeInfo bt ept), &$exp:val);|]

-- | Return a statement printing the given external value.
printStm :: ExternalValue -> C.Exp -> C.Stm
printStm (OpaqueValue desc _) _ =
  [C.cstm|printf("#<opaque %s>", $string:desc);|]
printStm (TransparentValue (ScalarValue bt ept _)) e =
  printPrimStm [C.cexp|stdout|] e bt ept
printStm (TransparentValue (ArrayValue _ _ bt ept shape)) e =
  let values_array = "futhark_values_" ++ name
      shape_array = "futhark_shape_" ++ name
      num_elems = cproduct [[C.cexp|$id:shape_array(ctx, $exp:e)[$int:i]|] | i <- [0 .. rank -1]]
   in [C.cstm|{
        $ty:bt' *arr = calloc(sizeof($ty:bt'), $exp:num_elems);
        assert(arr != NULL);
        assert($id:values_array(ctx, $exp:e, arr) == 0);
        write_array(stdout, binary_output, &$exp:(primTypeInfo bt ept), arr,
                    $id:shape_array(ctx, $exp:e), $int:rank);
        free(arr);
      }|]
  where
    rank = length shape
    bt' = primTypeToCType bt
    name = arrayName bt ept rank

printResult :: [(ExternalValue, C.Exp)] -> [C.Stm]
printResult = concatMap f
  where
    f (v, e) = [printStm v e, [C.cstm|printf("\n");|]]

cliEntryPoint ::
  Name ->
  FunctionT a ->
  (C.Definition, C.Initializer)
cliEntryPoint fname (Function _ _ _ _ results args) =
  let (input_items, pack_input, free_input, free_parsed, input_args) =
        unzip5 $ readInputs args

      (output_decls, output_vals, free_outputs) =
        unzip3 $ prepareOutputs results

      printstms = printResult $ zip results output_vals

      ctx_ty = [C.cty|struct futhark_context|]
      sync_ctx = "futhark_context_sync" :: Name
      error_ctx = "futhark_context_get_error" :: Name

      entry_point_name = nameToString fname
      cli_entry_point_function_name = "futrts_cli_entry_" ++ entry_point_name
      entry_point_function_name = "futhark_entry_" ++ entry_point_name

      pause_profiling = "futhark_context_pause_profiling" :: Name
      unpause_profiling = "futhark_context_unpause_profiling" :: Name

      addrOf e = [C.cexp|&$exp:e|]

      run_it =
        [C.citems|
                  int r;
                  // Run the program once.
                  $stms:pack_input
                  if ($id:sync_ctx(ctx) != 0) {
                    futhark_panic(1, "%s", $id:error_ctx(ctx));
                  };
                  // Only profile last run.
                  if (profile_run) {
                    $id:unpause_profiling(ctx);
                  }
                  t_start = get_wall_time();
                  r = $id:entry_point_function_name(ctx,
                                                    $args:(map addrOf output_vals),
                                                    $args:input_args);
                  if (r != 0) {
                    futhark_panic(1, "%s", $id:error_ctx(ctx));
                  }
                  if ($id:sync_ctx(ctx) != 0) {
                    futhark_panic(1, "%s", $id:error_ctx(ctx));
                  };
                  if (profile_run) {
                    $id:pause_profiling(ctx);
                  }
                  t_end = get_wall_time();
                  long int elapsed_usec = t_end - t_start;
                  if (time_runs && runtime_file != NULL) {
                    fprintf(runtime_file, "%lld\n", (long long) elapsed_usec);
                    fflush(runtime_file);
                  }
                  $stms:free_input
                |]
   in ( [C.cedecl|
  static void $id:cli_entry_point_function_name($ty:ctx_ty *ctx) {
    typename int64_t t_start, t_end;
    int time_runs = 0, profile_run = 0;

    // We do not want to profile all the initialisation.
    $id:pause_profiling(ctx);

    // Declare and read input.
    set_binary_mode(stdin);
    $items:(mconcat input_items)

    if (end_of_input(stdin) != 0) {
      futhark_panic(1, "Expected EOF on stdin after reading input for %s.\n", $string:(quote (pretty fname)));
    }

    $items:output_decls

    // Warmup run
    if (perform_warmup) {
      $items:run_it
      $stms:free_outputs
    }
    time_runs = 1;
    // Proper run.
    for (int run = 0; run < num_runs; run++) {
      // Only profile last run.
      profile_run = run == num_runs -1;
      $items:run_it
      if (run < num_runs-1) {
        $stms:free_outputs
      }
    }

    // Free the parsed input.
    $stms:free_parsed

    // Print the final result.
    if (binary_output) {
      set_binary_mode(stdout);
    }
    $stms:printstms

    $stms:free_outputs
  }|],
        [C.cinit|{ .name = $string:entry_point_name,
                      .fun = $id:cli_entry_point_function_name }|]
      )

{-# NOINLINE cliDefs #-}
cliDefs :: [Option] -> Functions a -> [C.Definition]
cliDefs options (Functions funs) =
  let values_h = $(embedStringFile "rts/c/values.h")
      tuning_h = $(embedStringFile "rts/c/tuning.h")

      option_parser =
        generateOptionParser "parse_options" $ genericOptions ++ options
      (cli_entry_point_decls, entry_point_inits) =
        unzip $ map (uncurry cliEntryPoint) funs
   in [C.cunit|
$esc:("#include <getopt.h>")
$esc:("#include <ctype.h>")
$esc:("#include <inttypes.h>")

$esc:values_h

static int binary_output = 0;
static typename FILE *runtime_file;
static int perform_warmup = 0;
static int num_runs = 1;
// If the entry point is NULL, the program will terminate after doing initialisation and such.
static const char *entry_point = "main";

$esc:tuning_h

$func:option_parser

$edecls:cli_entry_point_decls

typedef void entry_point_fun(struct futhark_context*);

struct entry_point_entry {
  const char *name;
  entry_point_fun *fun;
};

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

  char* error = futhark_context_get_error(ctx);
  if (error != NULL) {
    futhark_panic(1, "%s", error);
  }

  struct entry_point_entry entry_points[] = {
    $inits:entry_point_inits
  };

  if (entry_point != NULL) {
    int num_entry_points = sizeof(entry_points) / sizeof(entry_points[0]);
    entry_point_fun *entry_point_fun = NULL;
    for (int i = 0; i < num_entry_points; i++) {
      if (strcmp(entry_points[i].name, entry_point) == 0) {
        entry_point_fun = entry_points[i].fun;
        break;
      }
    }

    if (entry_point_fun == NULL) {
      fprintf(stderr, "No entry point '%s'.  Select another with --entry-point.  Options are:\n",
                      entry_point);
      for (int i = 0; i < num_entry_points; i++) {
        fprintf(stderr, "%s\n", entry_points[i].name);
      }
      return 1;
    }

    entry_point_fun(ctx);

    if (runtime_file != NULL) {
      fclose(runtime_file);
    }

    char *report = futhark_context_report(ctx);
    fputs(report, stderr);
    free(report);
  }

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
  return 0;
}|]
