-- | This module defines a generator for @getopt@ based command
-- line argument parsing.  Each option is associated with arbitrary
-- Python code that will perform side effects, usually by setting some
-- global variables.
module Futhark.CodeGen.Backends.GenericCSharp.Options
       ( Option (..)
       , OptionArgument (..)
       , generateOptionParser
       )
       where

import Futhark.CodeGen.Backends.GenericCSharp.AST

-- | Specification if a single command line option.  The option must
-- have a long name, and may also have a short name.
--
-- When the statement is being executed, the argument (if any) will be
-- stored in the variable @optarg@.
data Option = Option { optionLongName :: String
                     , optionShortName :: Maybe Char
                     , optionArgument :: OptionArgument
                     , optionAction :: [CSStmt]
                     }

-- | Whether an option accepts an argument.
data OptionArgument = NoArgument
                    | RequiredArgument
                    | OptionalArgument

-- | Generate option parsing code that accepts the given command line options.  Will read from @sys.argv@.
--
-- If option parsing fails for any reason, the entire process will
-- terminate with error code 1.
generateOptionParser :: [Option] -> [CSStmt]
generateOptionParser options =
  [ Assign (Var "options") (Collection "OptionSet" $ map parseOption options)
  , Assign (Var "extra") (Call (Var "options.Parse") [Arg Nothing (Var "args")])
  ]
  where parseOption option = Array [ String $ option_string option
                                   , Lambda (Var "optarg") $ optionAction option ]
        option_string option = case optionArgument option of
          RequiredArgument ->
            concat [maybe "" prefix $ optionShortName option,optionLongName option,"="]
          _ ->
            maybe "" prefix (optionShortName option) ++ optionLongName option
        prefix = flip (:) "|"
