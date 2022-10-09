{-# LANGUAGE QuasiQuotes #-}

-- | This module defines a generator for @getopt_long@ based command
-- line argument parsing.  Each option is associated with arbitrary C
-- code that will perform side effects, usually by setting some global
-- variables.
module Futhark.CodeGen.Backends.GenericC.Options
  ( Option (..),
    OptionArgument (..),
    generateOptionParser,
  )
where

import Data.Char (isSpace)
import Data.Function ((&))
import Data.List (intercalate)
import Data.Maybe
import Language.C.Quote.C qualified as C
import Language.C.Syntax qualified as C

-- | Specification if a single command line option.  The option must
-- have a long name, and may also have a short name.
--
-- In the action, the option argument (if any) is stored as in the
-- @char*@-typed variable @optarg@.
data Option = Option
  { optionLongName :: String,
    optionShortName :: Maybe Char,
    optionArgument :: OptionArgument,
    optionDescription :: String,
    optionAction :: C.Stm
  }

-- | Whether an option accepts an argument.
data OptionArgument
  = NoArgument
  | -- | The 'String' becomes part of the help pretty.
    RequiredArgument String
  | OptionalArgument

-- | Generate an option parser as a function of the given name, that
-- accepts the given command line options.  The result is a function
-- that should be called with @argc@ and @argv@.  The function returns
-- the number of @argv@ elements that have been processed.
--
-- If option parsing fails for any reason, the entire process will
-- terminate with error code 1.
generateOptionParser :: String -> [Option] -> C.Func
generateOptionParser fname options =
  [C.cfun|int $id:fname(struct futhark_context_config *cfg, int argc, char* const argv[]) {
       int $id:chosen_option;

       static struct option long_options[] = { $inits:option_fields, {0, 0, 0, 0} };

       static char* option_descriptions = $string:option_descriptions;

       while (($id:chosen_option =
                 getopt_long(argc, argv, $string:option_string, long_options, NULL)) != -1) {
         $stms:option_applications
         if ($id:chosen_option == ':') {
           futhark_panic(-1, "Missing argument for option %s\n", argv[optind-1]);
         }
         if ($id:chosen_option == '?') {
           fprintf(stderr, "Usage: %s [OPTIONS]...\nOptions:\n\n%s\n", fut_progname, $string:option_descriptions);
           futhark_panic(1, "Unknown option: %s\n", argv[optind-1]);
         }
       }
       return optind;
     }
         |]
  where
    chosen_option = "ch"
    option_string = ':' : optionString options
    option_applications = optionApplications chosen_option options
    option_fields = optionFields options
    option_descriptions = describeOptions options

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

describeOptions :: [Option] -> String
describeOptions opts =
  let
   in unlines $ fmap extendDesc with_short_descs
  where
    with_short_descs = fmap (\opt -> (opt, shortDesc opt)) opts
    max_short_desc_len = maximum $ fmap (length . snd) with_short_descs
    extendDesc :: (Option, String) -> String
    extendDesc (opt, short) =
      take (max_short_desc_len + 1) (short ++ repeat ' ')
        ++ ( optionDescription opt
               & lines
               & fmap trim
               & intercalate ('\n' : replicate (max_short_desc_len + 1) ' ')
           )
    shortDesc :: Option -> String
    shortDesc opt =
      concat
        [ "  ",
          maybe "" (\c -> "-" ++ [c] ++ "/") $ optionShortName opt,
          "--" ++ optionLongName opt,
          case optionArgument opt of
            NoArgument -> ""
            RequiredArgument what -> " " ++ what
            OptionalArgument -> " [ARG]"
        ]

optionFields :: [Option] -> [C.Initializer]
optionFields = zipWith field [(1 :: Int) ..]
  where
    field i option =
      [C.cinit| { $string:(optionLongName option), $id:arg, NULL, $int:i } |]
      where
        arg = case optionArgument option of
          NoArgument -> "no_argument" :: String
          RequiredArgument _ -> "required_argument"
          OptionalArgument -> "optional_argument"

optionApplications :: String -> [Option] -> [C.Stm]
optionApplications chosen_option = zipWith check [(1 :: Int) ..]
  where
    check i option =
      [C.cstm|if ($exp:cond) $stm:(optionAction option)|]
      where
        cond = case optionShortName option of
          Nothing -> [C.cexp|$id:chosen_option == $int:i|]
          Just c ->
            [C.cexp|($id:chosen_option == $int:i) ||
                                            ($id:chosen_option == $char:c)|]

optionString :: [Option] -> String
optionString = concat . mapMaybe optionStringChunk
  where
    optionStringChunk option = do
      short <- optionShortName option
      pure $
        short
          : case optionArgument option of
            NoArgument -> ""
            RequiredArgument _ -> ":"
            OptionalArgument -> "::"
