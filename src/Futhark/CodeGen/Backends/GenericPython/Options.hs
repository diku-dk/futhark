-- | This module defines a generator for @getopt@ based command
-- line argument parsing.  Each option is associated with arbitrary
-- Python code that will perform side effects, usually by setting some
-- global variables.
module Futhark.CodeGen.Backends.GenericPython.Options
  ( Option (..),
    OptionArgument (..),
    generateOptionParser,
  )
where

import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericPython.AST

-- | Specification if a single command line option.  The option must
-- have a long name, and may also have a short name.
--
-- When the statement is being executed, the argument (if any) will be
-- stored in the variable @optarg@.
data Option = Option
  { optionLongName :: T.Text,
    optionShortName :: Maybe Char,
    optionArgument :: OptionArgument,
    optionAction :: [PyStmt]
  }

-- | Whether an option accepts an argument.
data OptionArgument
  = NoArgument
  | RequiredArgument String
  | OptionalArgument

-- | Generate option parsing code that accepts the given command line options.  Will read from @sys.argv@.
--
-- If option parsing fails for any reason, the entire process will
-- terminate with error code 1.
generateOptionParser :: [Option] -> [PyStmt]
generateOptionParser options =
  [ Assign
      (Var "parser")
      ( Call
          (Var "argparse.ArgumentParser")
          [ ArgKeyword "description" $
              String "A compiled Futhark program."
          ]
      )
  ]
    ++ map parseOption options
    ++ [ Assign (Var "parser_result") $
           Call (Var "vars") [Arg $ Call (Var "parser.parse_args") [Arg $ Var "sys.argv[1:]"]]
       ]
    ++ map executeOption options
  where
    parseOption option =
      Exp $
        Call (Var "parser.add_argument") $
          map (Arg . String) name_args ++ argument_args
      where
        name_args =
          maybe
            id
            (\x l -> ("-" <> T.singleton x) : l)
            (optionShortName option)
            ["--" <> optionLongName option]
        argument_args = case optionArgument option of
          RequiredArgument t ->
            [ ArgKeyword "action" (String "append"),
              ArgKeyword "default" $ List [],
              ArgKeyword "type" $ Var t
            ]
          NoArgument ->
            [ ArgKeyword "action" (String "append_const"),
              ArgKeyword "default" $ List [],
              ArgKeyword "const" None
            ]
          OptionalArgument ->
            [ ArgKeyword "action" (String "append"),
              ArgKeyword "default" $ List [],
              ArgKeyword "nargs" $ String "?"
            ]

    executeOption option =
      For
        "optarg"
        (Index (Var "parser_result") $ IdxExp $ String $ fieldName option)
        $ optionAction option

    fieldName = T.map escape . optionLongName
      where
        escape '-' = '_'
        escape c = c
