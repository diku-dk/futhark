-- | Interface to the Futhark parser.
module Language.Futhark.Parser
  ( parseFuthark,
    parseFutharkWithComments,
    parseExp,
    parseExpAt,
    parseModExp,
    parseType,
    parseDecOrExp,
    SyntaxError (..),
    Comment (..),
  )
where

import Data.Text qualified as T
import Futhark.Util.Loc (Pos (..))
import Language.Futhark.Parser.Parser
import Language.Futhark.Prop
import Language.Futhark.Syntax

initialPos :: FilePath -> Pos
initialPos fname = Pos fname 1 1 0

-- | Parse an entire Futhark program from the given 'T.Text', using
-- the 'FilePath' as the source name for error messages.
parseFuthark ::
  FilePath ->
  T.Text ->
  Either SyntaxError UncheckedProg
parseFuthark fname = parse prog (initialPos fname)

-- | Parse an entire Futhark program from the given 'T.Text', using
-- the 'FilePath' as the source name for error messages.  Also returns
-- the comments encountered.
parseFutharkWithComments ::
  FilePath ->
  T.Text ->
  Either SyntaxError (UncheckedProg, [Comment])
parseFutharkWithComments fname = parseWithComments prog (initialPos fname)

-- | Parse an Futhark expression from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseExp ::
  FilePath ->
  T.Text ->
  Either SyntaxError UncheckedExp
parseExp fname = parse expression (initialPos fname)

-- | As 'parseExp', but the expression is assumed to start at the
-- given position, rather than at the beginning of a file.  This is
-- useful when the expression is a fragment of a larger file, as the
-- source locations in the result will then refer to that file.  The
-- position also provides the source name for error messages.
parseExpAt ::
  Pos ->
  T.Text ->
  Either SyntaxError UncheckedExp
parseExpAt = parse expression

-- | Parse a Futhark module expression from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseModExp ::
  FilePath ->
  T.Text ->
  Either SyntaxError (ModExpBase NoInfo Name)
parseModExp fname = parse modExpression (initialPos fname)

-- | Parse an Futhark type from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseType ::
  FilePath ->
  T.Text ->
  Either SyntaxError UncheckedTypeExp
parseType fname = parse futharkType (initialPos fname)

-- | Parse either an expression or a declaration; favouring declarations in case
-- of ambiguity.
parseDecOrExp ::
  FilePath ->
  T.Text ->
  Either SyntaxError (Either UncheckedDec UncheckedExp)
parseDecOrExp fname input =
  case parse declaration (initialPos fname) input of
    Left {} -> Right <$> parseExp fname input
    Right d -> Right $ Left d
