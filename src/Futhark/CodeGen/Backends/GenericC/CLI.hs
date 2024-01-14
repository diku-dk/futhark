{-# LANGUAGE QuasiQuotes #-}

-- | Code generation for standalone executables.
module Futhark.CodeGen.Backends.GenericC.CLI
  ( cliDefs,
  )
where

import Data.List (unzip5)
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.GenericC.Pretty
import Futhark.CodeGen.Backends.SimpleRep
  ( cproduct,
    escapeName,
    primAPIType,
    primStorageType,
    scalarToPrim,
  )
import Futhark.CodeGen.RTS.C (tuningH, valuesH)
import Futhark.Manifest
import Futhark.Util.Pretty (prettyString)
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C

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
        optionAction =
          [C.cstm|{futhark_context_config_set_debugging(cfg, 1);}|]
      },
    Option
      { optionLongName = "log",
        optionShortName = Just 'L',
        optionArgument = NoArgument,
        optionDescription = "Print various low-overhead logging information to stderr while running.",
        optionAction =
          [C.cstm|{futhark_context_config_set_logging(cfg, 1);}|]
      },
    Option
      { optionLongName = "profile",
        optionShortName = Just 'P',
        optionArgument = NoArgument,
        optionDescription = "Enable the collection of profiling information.",
        optionAction = [C.cstm|futhark_context_config_set_profiling(cfg, 1);|]
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
      { optionLongName = "no-print-result",
        optionShortName = Just 'n',
        optionArgument = NoArgument,
        optionDescription = "Do not print the program result.",
        optionAction = [C.cstm|print_result = 0;|]
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
                  if (futhark_context_config_set_tuning_param(cfg, name, (size_t)value) != 0) {
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

readInput :: Manifest -> Int -> T.Text -> ([C.BlockItem], C.Stm, C.Stm, C.Stm, C.Exp)
readInput manifest i tname =
  case M.lookup tname $ manifestTypes manifest of
    Nothing ->
      let (_, t) = scalarToPrim tname
          dest = "read_value_" ++ show i
          info = T.unpack tname <> "_info"
       in ( [C.citems|
             $ty:(primStorageType t) $id:dest;
             if (read_scalar(stdin, &$id:info, &$id:dest) != 0) {
             futhark_panic(1, "Error when reading input #%d of type %s (errno: %s).\n",
                           $int:i,
                           $string:(T.unpack tname),
                           strerror(errno));
                           };|],
            [C.cstm|;|],
            [C.cstm|;|],
            [C.cstm|;|],
            [C.cexp|$id:dest|]
          )
    Just (TypeOpaque desc _ _ _) ->
      ( [C.citems|futhark_panic(1, "Cannot read input #%d of type %s\n", $int:i, $string:(T.unpack desc));|],
        [C.cstm|;|],
        [C.cstm|;|],
        [C.cstm|;|],
        [C.cexp|NULL|]
      )
    Just (TypeArray t et rank ops) ->
      let dest = "read_value_" ++ show i
          shape = "read_shape_" ++ show i
          arr = "read_arr_" ++ show i

          ty = [C.cty|typename $id:t|]
          dims_exps = [[C.cexp|$id:shape[$int:j]|] | j <- [0 .. rank - 1]]
          t' = uncurry primAPIType $ scalarToPrim et

          new_array = arrayNew ops
          free_array = arrayFree ops
          info = T.unpack et <> "_info"

          items =
            [C.citems|
               $ty:ty $id:dest;
               typename int64_t $id:shape[$int:rank];
               $ty:t' *$id:arr = NULL;
               errno = 0;
               if (read_array(stdin,
                              &$id:info,
                              (void**) &$id:arr,
                              $id:shape,
                              $int:rank)
                   != 0) {
                 futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n",
                               $int:i,
                               $string:(T.unpack tname),
                               strerror(errno));
               }|]
       in ( items,
            [C.cstm|assert(($id:dest = $id:new_array(ctx, $id:arr, $args:dims_exps)) != NULL);|],
            [C.cstm|assert($id:free_array(ctx, $id:dest) == 0);|],
            [C.cstm|free($id:arr);|],
            [C.cexp|$id:dest|]
          )

readInputs :: Manifest -> [T.Text] -> [([C.BlockItem], C.Stm, C.Stm, C.Stm, C.Exp)]
readInputs manifest = zipWith (readInput manifest) [0 ..]

prepareOutputs :: Manifest -> [T.Text] -> [(C.BlockItem, C.Exp, C.Stm)]
prepareOutputs manifest = zipWith prepareResult [(0 :: Int) ..]
  where
    prepareResult i tname = do
      let result = "result_" ++ show i

      case M.lookup tname $ manifestTypes manifest of
        Nothing ->
          let (s, pt) = scalarToPrim tname
              ty = primAPIType s pt
           in ( [C.citem|$ty:ty $id:result;|],
                [C.cexp|$id:result|],
                [C.cstm|;|]
              )
        Just (TypeArray t _ _ ops) ->
          ( [C.citem|typename $id:t $id:result;|],
            [C.cexp|$id:result|],
            [C.cstm|assert($id:(arrayFree ops)(ctx, $id:result) == 0);|]
          )
        Just (TypeOpaque t ops _ _) ->
          ( [C.citem|typename $id:t $id:result;|],
            [C.cexp|$id:result|],
            [C.cstm|assert($id:(opaqueFree ops)(ctx, $id:result) == 0);|]
          )

-- | Return a statement printing the given external value.
printStm :: Manifest -> T.Text -> C.Exp -> C.Stm
printStm manifest tname e =
  case M.lookup tname $ manifestTypes manifest of
    Nothing ->
      let info = tname <> "_info"
       in [C.cstm|write_scalar(stdout, binary_output, &$id:info, &$exp:e);|]
    Just (TypeOpaque desc _ _ _) ->
      [C.cstm|{
         fprintf(stderr, "Values of type \"%s\" have no external representation.\n", $string:(T.unpack desc));
         retval = 1;
         goto print_end;
       }|]
    Just (TypeArray _ et rank ops) ->
      let et' = uncurry primAPIType $ scalarToPrim et
          values_array = arrayValues ops
          shape_array = arrayShape ops
          num_elems =
            cproduct [[C.cexp|$id:shape_array(ctx, $exp:e)[$int:i]|] | i <- [0 .. rank - 1]]
          info = et <> "_info"
       in [C.cstm|{
                 $ty:et' *arr = calloc($exp:num_elems, $id:info.size);
                 assert(arr != NULL);
                 assert($id:values_array(ctx, $exp:e, arr) == 0);
                 assert(futhark_context_sync(ctx) == 0);
                 write_array(stdout, binary_output, &$id:info, arr,
                             $id:shape_array(ctx, $exp:e), $int:rank);
                 free(arr);
                 }|]

printResult :: Manifest -> [(T.Text, C.Exp)] -> [C.Stm]
printResult manifest = concatMap f
  where
    f (v, e) = [printStm manifest v e, [C.cstm|printf("\n");|]]

cliEntryPoint ::
  Manifest -> T.Text -> EntryPoint -> (C.Definition, C.Initializer)
cliEntryPoint manifest entry_point_name (EntryPoint cfun _tuning_params outputs inputs) =
  let (input_items, pack_input, free_input, free_parsed, input_args) =
        unzip5 $ readInputs manifest $ map inputType inputs

      (output_decls, output_vals, free_outputs) =
        unzip3 $ prepareOutputs manifest $ map outputType outputs

      printstms =
        printResult manifest $ zip (map outputType outputs) output_vals

      cli_entry_point_function_name =
        "futrts_cli_entry_" <> T.unpack (escapeName entry_point_name)

      pause_profiling = "futhark_context_pause_profiling" :: T.Text
      unpause_profiling = "futhark_context_unpause_profiling" :: T.Text

      addrOf e = [C.cexp|&$exp:e|]

      run_it =
        [C.citems|
                int r;
                // Run the program once.
                $stms:pack_input
                if (futhark_context_sync(ctx) != 0) {
                  futhark_panic(1, "%s", futhark_context_get_error(ctx));
                };
                // Only profile last run.
                if (profile_run) {
                  $id:unpause_profiling(ctx);
                }
                t_start = get_wall_time();
                r = $id:cfun(ctx,
                             $args:(map addrOf output_vals),
                             $args:input_args);
                if (r != 0) {
                  futhark_panic(1, "%s", futhark_context_get_error(ctx));
                }
                if (futhark_context_sync(ctx) != 0) {
                  futhark_panic(1, "%s", futhark_context_get_error(ctx));
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
   static int $id:cli_entry_point_function_name(struct futhark_context *ctx) {
     typename int64_t t_start, t_end;
     int time_runs = 0, profile_run = 0;
     int retval = 0;

     // We do not want to profile all the initialisation.
     $id:pause_profiling(ctx);

     // Declare and read input.
     set_binary_mode(stdin);
     $items:(mconcat input_items)

     if (end_of_input(stdin) != 0) {
       futhark_panic(1, "Expected EOF on stdin after reading input for \"%s\".\n", $string:(prettyString entry_point_name));
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

     if (print_result) {
       // Print the final result.
       if (binary_output) {
         set_binary_mode(stdout);
       }
       $stms:printstms
     }
     print_end: {}
     $stms:free_outputs
     return retval;
   }|],
        [C.cinit|{ .name = $string:(T.unpack entry_point_name),
                   .fun = $id:cli_entry_point_function_name }|]
      )

{-# NOINLINE cliDefs #-}

-- | Generate Futhark standalone executable code.
cliDefs :: [Option] -> Manifest -> T.Text
cliDefs options manifest =
  let option_parser =
        generateOptionParser "parse_options" $ genericOptions ++ options
      (cli_entry_point_decls, entry_point_inits) =
        unzip $
          map (uncurry (cliEntryPoint manifest)) $
            M.toList $
              manifestEntryPoints manifest
   in definitionsText
        [C.cunit|
$esc:("#include <getopt.h>")
$esc:("#include <ctype.h>")
$esc:("#include <inttypes.h>")
$esc:("#include <unistd.h>")

$esc:(T.unpack valuesH)

static int binary_output = 0;
static int print_result = 1;
static typename FILE *runtime_file;
static int perform_warmup = 0;
static int num_runs = 1;
// If the entry point is NULL, the program will terminate after doing initialisation and such.
static const char *entry_point = "main";

$esc:(T.unpack tuningH)

$func:option_parser

$edecls:cli_entry_point_decls

typedef int entry_point_fun(struct futhark_context*);

struct entry_point_entry {
  const char *name;
  entry_point_fun *fun;
};

int main(int argc, char** argv) {
  int retval = 0;
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

    if (isatty(fileno(stdin))) {
      fprintf(stderr, "Reading input from TTY.\n");
      fprintf(stderr, "Send EOF (CTRL-d) after typing all input values.\n");
    }

    retval = entry_point_fun(ctx);

    if (runtime_file != NULL) {
      fclose(runtime_file);
    }
  }

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
  return retval;
}|]
