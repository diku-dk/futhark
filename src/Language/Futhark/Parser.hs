-- | Interface to the Futhark parser.
module Language.Futhark.Parser
  ( parseFuthark,
    parseFutharkWithComments,
    parseExp,
    parseModExp,
    parseType,
    parseDecOrExp,
    SyntaxError (..),
    Comment (..),
  )
where

import Data.Text qualified as T
import Language.Futhark.Parser.Parser
import Language.Futhark.Prop
import Language.Futhark.Syntax

-- | Parse an entire Futhark program from the given 'T.Text', using
-- the 'FilePath' as the source name for error messages.
parseFuthark ::
  FilePath ->
  T.Text ->
  Either SyntaxError UncheckedProg
parseFuthark = parse prog

-- | Parse an entire Futhark program from the given 'T.Text', using
-- the 'FilePath' as the source name for error messages.  Also returns
-- the comments encountered.
parseFutharkWithComments ::
  FilePath ->
  T.Text ->
  Either SyntaxError (UncheckedProg, [Comment])
parseFutharkWithComments = parseWithComments prog

-- | Parse an Futhark expression from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseExp ::
  FilePath ->
  T.Text ->
  Either SyntaxError UncheckedExp
parseExp = parse expression

-- | Parse a Futhark module expression from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseModExp ::
  FilePath ->
  T.Text ->
  Either SyntaxError (ModExpBase NoInfo Name)
parseModExp = parse modExpression

-- | Parse an Futhark type from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseType ::
  FilePath ->
  T.Text ->
  Either SyntaxError UncheckedTypeExp
parseType = parse futharkType

-- | Parse either an expression or a declaration; favouring
-- declarations in case of ambiguity.
parseDecOrExp ::
  FilePath ->
  T.Text ->
  Either SyntaxError (Either UncheckedDec UncheckedExp)
parseDecOrExp file input =
  case parse declaration file input of
    Left {} -> Right <$> parseExp file input
    Right d -> Right $ Left d
