-- | Interface to the Futhark parser.
module Language.Futhark.Parser
  ( parseFuthark,
    parseExp,
    parseModExp,
    parseType,
    parseValue,
    parseValues,
    parseDecOrExpIncrM,
    ParseError,
  )
where

import qualified Data.Text as T
import Language.Futhark.Parser.Parser
import Language.Futhark.Prop
import Language.Futhark.Syntax

-- | Parse an entire Futhark program from the given 'T.Text', using
-- the 'FilePath' as the source name for error messages.
parseFuthark ::
  FilePath ->
  T.Text ->
  Either ParseError UncheckedProg
parseFuthark = parse prog

-- | Parse an Futhark expression from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseExp ::
  FilePath ->
  T.Text ->
  Either ParseError UncheckedExp
parseExp = parse expression

-- | Parse a Futhark module expression from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseModExp ::
  FilePath ->
  T.Text ->
  Either ParseError (ModExpBase NoInfo Name)
parseModExp = parse modExpression

-- | Parse an Futhark type from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseType ::
  FilePath ->
  T.Text ->
  Either ParseError UncheckedTypeExp
parseType = parse futharkType

-- | Parse any Futhark value from the given 'String', using the 'FilePath'
-- as the source name for error messages.
parseValue ::
  FilePath ->
  T.Text ->
  Either ParseError Value
parseValue = parse anyValue

-- | Parse several Futhark values (separated by anything) from the given
-- 'String', using the 'FilePath' as the source name for error
-- messages.
parseValues ::
  FilePath ->
  T.Text ->
  Either ParseError [Value]
parseValues = parse anyValues
