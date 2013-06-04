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
  )
  where

import Control.Monad

import Language.L0.Syntax
import Language.L0.Parser.Parser
import Language.L0.Parser.Lexer

-- | Parse an entire L0 program from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseL0 :: FilePath -> String -> Either String (Prog (Maybe Type))
parseL0 file = prog <=< alexScanTokens file

-- | Parse an integer in L0 syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseInt :: FilePath -> String -> Either String Value
parseInt file = intValue <=< alexScanTokens file

-- | Parse an real number in L0 syntax from the given 'String', using
-- the 'FilePath' as the source name for error messages.
parseReal :: FilePath -> String -> Either String Value
parseReal file = realValue <=< alexScanTokens file

-- | Parse a boolean L0 syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseBool :: FilePath -> String -> Either String Value
parseBool file = boolValue <=< alexScanTokens file

-- | Parse a character in L0 syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseChar :: FilePath -> String -> Either String Value
parseChar file = charValue <=< alexScanTokens file

-- | Parse a string in L0 syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseString :: FilePath -> String -> Either String Value
parseString file = stringValue <=< alexScanTokens file

-- | Parse a tuple in L0 syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseTuple :: FilePath -> String -> Either String Value
parseTuple file = tupleValue <=< alexScanTokens file

-- | Parse an array in L0 syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseArray :: FilePath -> String -> Either String Value
parseArray file = arrayValue <=< alexScanTokens file

-- | Parse any L0 value from the given 'String', using the 'FilePath'
-- as the source name for error messages.
parseValue :: FilePath -> String -> Either String Value
parseValue file s = msum $ map (\f -> f file s)
                    [parseInt,
                     parseReal,
                     parseBool,
                     parseChar,
                     parseString,
                     parseArray,
                     parseTuple]
