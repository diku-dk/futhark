{-# LANGUAGE OverloadedStrings #-}

-- | Interface to the Futhark parser.
module Language.Futhark.Parser
  ( parseFuthark,
    parseExp,
    parseModExp,
    parseType,
    parseValue,
    parseValues,
    parseDecOrExpIncrM,
    SyntaxError (..),
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
  Either SyntaxError UncheckedProg
parseFuthark = parse prog

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

-- | Parse any Futhark value from the given 'String', using the 'FilePath'
-- as the source name for error messages.
parseValue ::
  FilePath ->
  T.Text ->
  Either SyntaxError Value
parseValue = parse anyValue

-- | Parse several Futhark values (separated by anything) from the given
-- 'String', using the 'FilePath' as the source name for error
-- messages.
parseValues ::
  FilePath ->
  T.Text ->
  Either SyntaxError [Value]
parseValues = parse anyValues

-- | Parse an Futhark expression incrementally from monadic actions, using the
-- 'FilePath' as the source name for error messages.
parseExpIncrM ::
  Monad m =>
  m T.Text ->
  FilePath ->
  T.Text ->
  m (Either SyntaxError UncheckedExp)
parseExpIncrM fetch file program =
  getLinesFromM fetch $ parseInMonad expression file program

-- | Parse either an expression or a declaration incrementally;
-- favouring declarations in case of ambiguity.
parseDecOrExpIncrM ::
  Monad m =>
  m T.Text ->
  FilePath ->
  T.Text ->
  m (Either SyntaxError (Either UncheckedDec UncheckedExp))
parseDecOrExpIncrM fetch file input =
  case parseInMonad declaration file input of
    Value Left {} -> fmap Right <$> parseExpIncrM fetch file input
    Value (Right d) -> pure $ Right $ Left d
    GetLine _ -> do
      l <- fetch
      parseDecOrExpIncrM fetch file $ input <> "\n" <> l
