-- | Interface to the Futhark parser.
module Language.Futhark.Parser
  ( parseFuthark
  , parseExp
  , parsePattern
  , parseType
  , parseLambda

  , parseInt
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
  , RealConfiguration (..)
  )
  where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.State
import Control.Monad.Except
import qualified Data.HashMap.Lazy as HM
import Data.Monoid

import Language.Futhark.Syntax
import Language.Futhark.Attributes hiding (arrayValue)
import Language.Futhark.Parser.Parser
import Language.Futhark.Parser.Lexer
import Language.Futhark.Parser.RealConfiguration

-- | A parse error.  Use 'show' to get a human-readable description.
data ParseError = ParseError String

instance Show ParseError where
  show (ParseError s) = s

parseInMonad :: ParserMonad a -> RealConfiguration -> FilePath -> String
             -> ReadLineMonad (Either ParseError a)
parseInMonad p rconf file program =
  liftM (either (Left . ParseError) Right) $ either (return . Left)
  (evalStateT (runReaderT (runExceptT p) env))
  (alexScanTokens file program)
  where env = case rconf of RealAsFloat32 ->
                              ParserEnv file Float32 toFloat32 float32funs
                            RealAsFloat64 ->
                              ParserEnv file Float64 (FloatValue . Float64Value) float64funs
        toFloat32 x =
          let (m,n) = decodeFloat x
          in FloatValue $ Float32Value $ encodeFloat m n
        float32funs = HM.map (<>nameFromString "32") funs
        float64funs = HM.map (<>nameFromString "64") funs
        funs = HM.fromList $ zip funnames funnames
        funnames = map nameFromString ["toFloat", "trunc", "sqrt", "log", "exp"]

parseIncrementalIO :: ParserMonad a -> RealConfiguration -> FilePath -> String
                   -> IO (Either ParseError a)
parseIncrementalIO p rconf file program =
  getLinesFromIO $ parseInMonad p rconf file program

parseIncremental :: ParserMonad a -> RealConfiguration -> FilePath -> String
                 -> Either ParseError a
parseIncremental p rconf file program =
  either (Left . ParseError) id
  $ getLinesFromStrings (lines program)
  $ parseInMonad p rconf file ""

parse :: ParserMonad a -> RealConfiguration -> FilePath -> String
      -> Either ParseError a
parse p rconf file program =
  either (Left . ParseError) id
  $ getNoLines $ parseInMonad p rconf file program

-- | Parse an Futhark expression greedily from the given 'String', only parsing
-- enough lines to get a correct expression, using the 'FilePath' as the source
-- name for error messages.
parseExpIncr :: RealConfiguration -> FilePath -> String
             -> Either ParseError UncheckedExp
parseExpIncr = parseIncremental expression

-- | Parse an Futhark expression incrementally from IO 'getLine' calls, using the
-- 'FilePath' as the source name for error messages.
parseExpIncrIO :: RealConfiguration -> FilePath -> String
               -> IO (Either ParseError UncheckedExp)
parseExpIncrIO = parseIncrementalIO expression

-- | Parse an entire Futhark program from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseFuthark :: RealConfiguration -> FilePath -> String
             -> Either ParseError UncheckedProg
parseFuthark = parse prog

-- | Parse an Futhark expression from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseExp :: RealConfiguration -> FilePath -> String
         -> Either ParseError UncheckedExp
parseExp = parse expression

-- | Parse an Futhark pattern from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parsePattern :: RealConfiguration -> FilePath -> String
             -> Either ParseError UncheckedTupIdent
parsePattern = parse tupId

-- | Parse an Futhark type from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseType :: RealConfiguration -> FilePath -> String
          -> Either ParseError UncheckedType
parseType = parse futharktype

-- | Parse an Futhark anonymous function from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseLambda :: RealConfiguration -> FilePath -> String
            -> Either ParseError UncheckedLambda
parseLambda = parse lambda

-- | Parse an integer in Futhark syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseInt :: RealConfiguration -> FilePath -> String
         -> Either ParseError Value
parseInt = parse intValue

-- | Parse a boolean Futhark syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseBool :: RealConfiguration -> FilePath -> String
          -> Either ParseError Value
parseBool = parse boolValue

-- | Parse a character in Futhark syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseChar :: RealConfiguration -> FilePath -> String
          -> Either ParseError Value
parseChar = parse charValue

-- | Parse a string in Futhark syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseString :: RealConfiguration -> FilePath -> String
            -> Either ParseError Value
parseString = parse stringValue

-- | Parse a tuple in Futhark syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseTuple :: RealConfiguration -> FilePath -> String
           -> Either ParseError Value
parseTuple = parse tupleValue

-- | Parse an array in Futhark syntax from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseArray :: RealConfiguration -> FilePath -> String
           -> Either ParseError Value
parseArray = parse arrayValue

-- | Parse any Futhark value from the given 'String', using the 'FilePath'
-- as the source name for error messages.
parseValue :: RealConfiguration -> FilePath -> String
           -> Either ParseError Value
parseValue = parse anyValue

-- | Parse several Futhark values (separated by anything) from the given
-- 'String', using the 'FilePath' as the source name for error
-- messages.
parseValues :: RealConfiguration -> FilePath -> String
            -> Either ParseError [Value]
parseValues = parse anyValues
