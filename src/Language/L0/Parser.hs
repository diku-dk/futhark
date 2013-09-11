-- | Interface to the L0 parser.
module Language.L0.Parser
  ( parseL0
  , parseExp
  , parsePattern
  , parseType

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

-- | A parse error.  Use 'show' to get a human-readable description.
data ParseError = ParseError String

instance Show ParseError where
  show (ParseError s) = s

instance Error ParseError where
  noMsg = ParseError "Unspecifed parse error"
  strMsg = ParseError

canFail :: (b -> Either String a) -> b -> Either ParseError a
canFail f = either (Left . ParseError) Right . f

parse :: ([L Token] -> Either String a)
      -> FilePath -> String -> Either ParseError a
parse f file = canFail $ f <=< alexScanTokens file

-- | Parse an entire L0 program from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseL0 :: FilePath -> String -> Either ParseError UncheckedProg
parseL0 = parse prog

-- | Parse an L0 expression from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseExp :: FilePath -> String -> Either ParseError UncheckedExp
parseExp = parse expression

-- | Parse an L0 pattern from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parsePattern :: FilePath -> String -> Either ParseError UncheckedTupIdent
parsePattern = parse pattern

-- | Parse an L0 type from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseType :: FilePath -> String -> Either ParseError UncheckedType
parseType = parse l0type

-- | Parse an integer in L0 syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseInt :: FilePath -> String -> Either ParseError Value
parseInt = parse intValue

-- | Parse an real number in L0 syntax from the given 'String', using
-- the 'FilePath' as the source name for error messages.
parseReal :: FilePath -> String -> Either ParseError Value
parseReal = parse realValue

-- | Parse a boolean L0 syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseBool :: FilePath -> String -> Either ParseError Value
parseBool = parse boolValue

-- | Parse a character in L0 syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseChar :: FilePath -> String -> Either ParseError Value
parseChar = parse charValue

-- | Parse a string in L0 syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseString :: FilePath -> String -> Either ParseError Value
parseString = parse stringValue

-- | Parse a tuple in L0 syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseTuple :: FilePath -> String -> Either ParseError Value
parseTuple = parse tupleValue

-- | Parse an array in L0 syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseArray :: FilePath -> String -> Either ParseError Value
parseArray = parse arrayValue

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
