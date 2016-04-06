-- | Interface to the Futhark parser.
module Language.Futhark.Parser
  ( parseFuthark
  , parseExp
  , parseType
  , parseLambda

  , parseValue
  , parseValues

  , parseExpIncr
  , parseExpIncrIO

  , ParseError (..)
  )
  where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Except
import Data.Maybe (mapMaybe)
import Data.List (intersect)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath (takeDirectory, (</>), (<.>))

import Prelude

import Language.Futhark.Syntax
import Language.Futhark.Attributes hiding (arrayValue)
import Language.Futhark.Parser.Parser
import Language.Futhark.Parser.Lexer

-- | A parse error.  Use 'show' to get a human-readable description.
data ParseError = ParseError String

instance Show ParseError where
  show (ParseError s) = s

parseInMonad :: ParserMonad a -> FilePath -> T.Text
             -> ReadLineMonad (Either ParseError a)
parseInMonad p file program =
  either (Left . ParseError) Right <$> either (return . Left)
  (evalStateT (evalStateT (runExceptT p) env))
  (scanTokens file program)
  where env = newParserEnv file Int32 Float64

parseIncrementalIO :: ParserMonad a -> FilePath -> T.Text
                   -> IO (Either ParseError a)
parseIncrementalIO p file program =
  getLinesFromIO $ parseInMonad p file program

parseIncremental :: ParserMonad a -> FilePath -> T.Text
                 -> Either ParseError a
parseIncremental p file program =
  either (Left . ParseError) id
  $ getLinesFromTexts (T.lines program)
  $ parseInMonad p file mempty

parse :: ParserMonad a -> FilePath -> T.Text
      -> Either ParseError a
parse p file program =
  either (Left . ParseError) id
  $ getNoLines $ parseInMonad p file program

-- | Parse an Futhark expression greedily from the given 'String', only parsing
-- enough lines to get a correct expression, using the 'FilePath' as the source
-- name for error messages.
parseExpIncr :: FilePath -> T.Text
             -> Either ParseError UncheckedExp
parseExpIncr = parseIncremental expression

-- | Parse an Futhark expression incrementally from IO 'getLine' calls, using the
-- 'FilePath' as the source name for error messages.
parseExpIncrIO :: FilePath -> T.Text
               -> IO (Either ParseError UncheckedExp)
parseExpIncrIO = parseIncrementalIO expression

-- | Parse an entire Futhark program from the given 'String', using
-- the 'FilePath' as the source name for error messages and the
-- relative path to use for includes, and parsing and reacting to all
-- headers.
parseFuthark :: FilePath -> T.Text
             -> IO (Either ParseError UncheckedProg)
parseFuthark fp0 s0 = parseWithPrevIncludes [fp0] (fp0, s0)
  where parseWithPrevIncludes :: [FilePath] -> (FilePath, T.Text)
                              -> IO (Either ParseError UncheckedProg)
        parseWithPrevIncludes prevIncludes (fp, s) =
          case parse prog fp s of
            Left e -> return $ Left e
            Right p ->
              let newIncludes = mapMaybe headerInclude $ progWHHeaders p
                  intersection = prevIncludes `intersect` newIncludes
              in if not (null intersection)
                 then return $ Left $ ParseError
                      ("Include cycle with " ++ show intersection ++ ".")
                 else let p' = Prog $ progWHFunctions p
                      in if null newIncludes
                         then return $ Right p'
                         else includeIncludes prevIncludes newIncludes p'

        includeIncludes :: [FilePath] -> [FilePath] -> UncheckedProg
                           -> IO (Either ParseError UncheckedProg)
        includeIncludes prevIncludes newIncludes endProg = do
          let allIncludes = prevIncludes ++ newIncludes
          ss <- liftIO $ mapM T.readFile newIncludes
          parses <- liftIO $ mapM (parseWithPrevIncludes allIncludes)
            (zip newIncludes ss)
          return $ foldr mergePrograms (Right endProg) parses

        mergePrograms :: Either ParseError UncheckedProg
                      -> Either ParseError UncheckedProg
                      -> Either ParseError UncheckedProg
        mergePrograms a b = case (a, b) of
          (Right (Prog fs), Right (Prog gs)) -> Right (Prog (fs ++ gs))
          (Left err, _) -> Left err
          (_, Left err) -> Left err

        headerInclude :: ProgHeader -> Maybe String
        headerInclude (Include name) = Just $ search_dir </> name <.> "fut"

        search_dir = takeDirectory fp0

-- | Parse an Futhark expression from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseExp :: FilePath -> T.Text
         -> Either ParseError UncheckedExp
parseExp = parse expression

-- | Parse an Futhark type from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseType :: FilePath -> T.Text
          -> Either ParseError UncheckedType
parseType = parse futharktype

-- | Parse an Futhark anonymous function from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseLambda :: FilePath -> T.Text
            -> Either ParseError UncheckedLambda
parseLambda = parse lambda

-- | Parse any Futhark value from the given 'String', using the 'FilePath'
-- as the source name for error messages.
parseValue :: FilePath -> T.Text
           -> Either ParseError Value
parseValue = parse anyValue

-- | Parse several Futhark values (separated by anything) from the given
-- 'String', using the 'FilePath' as the source name for error
-- messages.
parseValues :: FilePath -> T.Text
            -> Either ParseError [Value]
parseValues = parse anyValues
