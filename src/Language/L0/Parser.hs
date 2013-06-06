-- | Interface to the L0 parser.
module Language.L0.Parser
  ( parseL0
  , parseInt
  , parseReal
  , parseBool
  , parseChar
  , parseString
  , parseArray
  , parseTuple
  , parseValue

  , ParseError
  )
  where

import Control.Monad
import Control.Monad.Error

import Language.L0.Syntax
import Language.L0.Attributes
import Language.L0.Parser.Parser
import Language.L0.Parser.Lexer

data ParseError = ParseError String

instance Show ParseError where
  show (ParseError s) = s

instance Error ParseError where
  noMsg = ParseError "Unspecifed parse error"
  strMsg = ParseError

canFail :: (b -> Either String a) -> b -> Either ParseError a
canFail f = either (Left . ParseError) Right . f

-- | Parse an entire L0 program from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseL0 :: FilePath -> String -> Either ParseError UncheckedProg
parseL0 file = canFail $ prog <=< alexScanTokens file

-- | Parse an integer in L0 syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseInt :: FilePath -> String -> Either ParseError Value
parseInt file = canFail $ intValue <=< alexScanTokens file

-- | Parse an real number in L0 syntax from the given 'String', using
-- the 'FilePath' as the source name for error messages.
parseReal :: FilePath -> String -> Either ParseError Value
parseReal file = canFail $ realValue <=< alexScanTokens file

-- | Parse a boolean L0 syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseBool :: FilePath -> String -> Either ParseError Value
parseBool file = canFail $ boolValue <=< alexScanTokens file

-- | Parse a character in L0 syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseChar :: FilePath -> String -> Either ParseError Value
parseChar file = canFail $ charValue <=< alexScanTokens file

-- | Parse a string in L0 syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseString :: FilePath -> String -> Either ParseError Value
parseString file = canFail $ stringValue <=< alexScanTokens file

-- | Parse a tuple in L0 syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseTuple :: FilePath -> String -> Either ParseError Value
parseTuple file = canFail $ tupleValue <=< alexScanTokens file

-- | Parse an array in L0 syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseArray :: FilePath -> String -> Either ParseError Value
parseArray file = canFail $ arrayValue <=< alexScanTokens file

-- | Parse any L0 value from the given 'String', using the 'FilePath'
-- as the source name for error messages.
parseValue :: FilePath -> String -> Either ParseError Value
parseValue file s = msum $ map (\f -> f file s)
                    [parseInt,
                     parseReal,
                     parseBool,
                     parseChar,
                     parseString,
                     parseArray,
                     parseTuple]
