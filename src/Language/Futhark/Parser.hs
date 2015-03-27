-- | Interface to the Futhark parser.
module Language.Futhark.Parser
  ( parseFuthark
  , parseExp
  , parsePattern
  , parseType
  , parseLambda

  , parseInt
  , parseReal
  , parseBool
  , parseChar
  , parseString
  , parseArray
  , parseTuple
  , parseValue
  , parseValues

  , parseExpIncr
  , parseExpIncrIO

  , ParseError (..)
  )
  where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.State
import Control.Monad.Except

import Language.Futhark.Syntax
import Language.Futhark.Attributes
import Language.Futhark.Parser.Parser
import Language.Futhark.Parser.Lexer

-- | A parse error.  Use 'show' to get a human-readable description.
data ParseError = ParseError String

instance Show ParseError where
  show (ParseError s) = s

parseInMonad :: ParserMonad a -> FilePath -> String -> ReadLineMonad (Either ParseError a)
parseInMonad p file program = liftM (either (Left . ParseError) Right)
                              $ either (return . Left)
                              (evalStateT (runReaderT (runExceptT p) file))
                              (alexScanTokens file program)

parseIncrementalIO :: ParserMonad a -> FilePath -> String -> IO (Either ParseError a)
parseIncrementalIO p file program = getLinesFromIO $ parseInMonad p file program

parseIncremental :: ParserMonad a -> FilePath -> String -> Either ParseError a
parseIncremental p file program = either (Left . ParseError) id
                                  $ getLinesFromStrings (lines program)
                                  $ parseInMonad p file ""

parse :: ParserMonad a -> FilePath -> String -> Either ParseError a
parse p file program = either (Left . ParseError) id
                       $ getNoLines $ parseInMonad p file program

-- | Parse an Futhark expression greedily from the given 'String', only parsing
-- enough lines to get a correct expression, using the 'FilePath' as the source
-- name for error messages.
parseExpIncr :: FilePath -> String -> Either ParseError UncheckedExp
parseExpIncr = parseIncremental expression

-- | Parse an Futhark expression incrementally from IO 'getLine' calls, using the
-- 'FilePath' as the source name for error messages.
parseExpIncrIO :: FilePath -> String -> IO (Either ParseError UncheckedExp)
parseExpIncrIO = parseIncrementalIO expression

-- | Parse an entire Futhark program from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseFuthark :: FilePath -> String -> Either ParseError UncheckedProg
parseFuthark = parse prog

-- | Parse an Futhark expression from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseExp :: FilePath -> String -> Either ParseError UncheckedExp
parseExp = parse expression

-- | Parse an Futhark pattern from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parsePattern :: FilePath -> String -> Either ParseError UncheckedTupIdent
parsePattern = parse pattern

-- | Parse an Futhark type from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseType :: FilePath -> String -> Either ParseError UncheckedType
parseType = parse futharktype

-- | Parse an Futhark anonymous function from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseLambda :: FilePath -> String -> Either ParseError UncheckedLambda
parseLambda = parse lambda

-- | Parse an integer in Futhark syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseInt :: FilePath -> String -> Either ParseError Value
parseInt = parse intValue

-- | Parse an real number in Futhark syntax from the given 'String', using
-- the 'FilePath' as the source name for error messages.
parseReal :: FilePath -> String -> Either ParseError Value
parseReal = parse realValue

-- | Parse a boolean Futhark syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseBool :: FilePath -> String -> Either ParseError Value
parseBool = parse boolValue

-- | Parse a character in Futhark syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseChar :: FilePath -> String -> Either ParseError Value
parseChar = parse charValue

-- | Parse a string in Futhark syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseString :: FilePath -> String -> Either ParseError Value
parseString = parse stringValue

-- | Parse a tuple in Futhark syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseTuple :: FilePath -> String -> Either ParseError Value
parseTuple = parse tupleValue

-- | Parse an array in Futhark syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseArray :: FilePath -> String -> Either ParseError Value
parseArray = parse arrayValue

-- | Parse any Futhark value from the given 'String', using the 'FilePath'
-- as the source name for error messages.
parseValue :: FilePath -> String -> Either ParseError Value
parseValue = parse anyValue

-- | Parse several Futhark values (separated by anything) from the given
-- 'String', using the 'FilePath' as the source name for error
-- messages.
parseValues :: FilePath -> String -> Either ParseError [Value]
parseValues = parse anyValues
