module L0.Parser
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

import L0.AbSyn
import L0.Parser.Parser
import L0.Parser.Lexer

parseL0 :: FilePath -> String -> Either String (Prog (Maybe Type))
parseL0 file = prog <=< alexScanTokens file

parseInt :: FilePath -> String -> Either String Value
parseInt file = intValue <=< alexScanTokens file

parseReal :: FilePath -> String -> Either String Value
parseReal file = realValue <=< alexScanTokens file

parseBool :: FilePath -> String -> Either String Value
parseBool file = boolValue <=< alexScanTokens file

parseChar :: FilePath -> String -> Either String Value
parseChar file = charValue <=< alexScanTokens file

parseString :: FilePath -> String -> Either String Value
parseString file = stringValue <=< alexScanTokens file

parseTuple :: FilePath -> String -> Either String Value
parseTuple file = tupleValue <=< alexScanTokens file

parseArray :: FilePath -> String -> Either String Value
parseArray file = arrayValue <=< alexScanTokens file

parseValue :: FilePath -> String -> Either String Value
parseValue file s = msum $ map (\f -> f file s)
                    [parseInt,
                     parseReal,
                     parseBool,
                     parseChar,
                     parseString,
                     parseArray,
                     parseTuple]
