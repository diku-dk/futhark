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
import Data.List (intersect, (\\))
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

parseInMonad :: ParserMonad a -> FilePath -> String
             -> ReadLineMonad (Either ParseError a)
parseInMonad p file program =
  either (Left . ParseError) Right <$> either (return . Left)
  (evalStateT (evalStateT (runExceptT p) env))
  (alexScanTokens file program)
  where env = newParserEnv file Int32 Float64

parseIncrementalIO :: ParserMonad a -> FilePath -> String
                   -> IO (Either ParseError a)
parseIncrementalIO p file program =
  getLinesFromIO $ parseInMonad p file program

parseIncremental :: ParserMonad a -> FilePath -> String
                 -> Either ParseError a
parseIncremental p file program =
  either (Left . ParseError) id
  $ getLinesFromStrings (lines program)
  $ parseInMonad p file ""

parse :: ParserMonad a -> FilePath -> String
      -> Either ParseError a
parse p file program =
  either (Left . ParseError) id
  $ getNoLines $ parseInMonad p file program

-- | Parse an Futhark expression greedily from the given 'String', only parsing
-- enough lines to get a correct expression, using the 'FilePath' as the source
-- name for error messages.
parseExpIncr :: FilePath -> String
             -> Either ParseError UncheckedExp
parseExpIncr = parseIncremental expression

-- | Parse an Futhark expression incrementally from IO 'getLine' calls, using the
-- 'FilePath' as the source name for error messages.
parseExpIncrIO :: FilePath -> String
               -> IO (Either ParseError UncheckedExp)
parseExpIncrIO = parseIncrementalIO expression

-- Needed @parseFuthark@, since it might read files.  Kept as simple as
-- possible and without external dependencies.
newtype ErrorIO e t = ErrorIO { evalErrorIO :: IO (Either e t) }

instance Monad (ErrorIO e) where
  m >>= g = ErrorIO $ do
    eith <- evalErrorIO m
    case eith of
      Left e -> return $ Left e
      Right t -> evalErrorIO $ g t

  return x = ErrorIO $ return $ Right x

bad :: e -> ErrorIO e t
bad e = ErrorIO $ return $ Left e

liftEither :: Either e t -> ErrorIO e t
liftEither eith = ErrorIO $ return eith

instance MonadIO (ErrorIO e) where
  liftIO io = ErrorIO (Right <$> io)

instance Functor (ErrorIO e) where
  fmap = liftM

instance Applicative (ErrorIO e) where
  (<*>) = ap
  pure = return

-- | Parse an entire Futhark program from the given 'String', using
-- the 'FilePath' as the source name for error messages and the
-- relative path to use for includes, and parsing and reacting to all
-- headers.
parseFuthark :: FilePath -> String
                -> IO (Either ParseError UncheckedProg)
parseFuthark fp0 s0 =
  (snd <$>) <$> (evalErrorIO $ parseWithIncludes [fp0] [fp0] (fp0, s0))
  where parseWithIncludes :: [FilePath] -> [FilePath] -> (FilePath, String)
                             -> ErrorIO ParseError ([FilePath], UncheckedProg)
        parseWithIncludes alreadyIncluded includeSources (fp, s) = do
          p <- liftEither $ parse prog fp s
          let newIncludes = mapMaybe headerInclude $ progWHHeaders p
              intersectionSources = includeSources `intersect` newIncludes

          when (not $ null intersectionSources) $ bad
            $ ParseError ("Include cycle with " ++ show intersectionSources ++ ".")

          let newIncludes' = newIncludes \\ alreadyIncluded
              alreadyIncluded' = fp : alreadyIncluded
              includeSources' = fp : includeSources
              p' = Prog $ progWHFunctions p
          if null newIncludes'
            then return (alreadyIncluded', p')
            else includeIncludes alreadyIncluded' includeSources' newIncludes' p'

        includeIncludes :: [FilePath] -> [FilePath] -> [FilePath] -> UncheckedProg
                          -> ErrorIO ParseError ([FilePath], UncheckedProg)
        includeIncludes alreadyIncluded includeSources newIncludes baseProg = do
          foldM (\(already, p) new -> do
                    (already', p1) <- includeInclude already includeSources new
                    return (already', mergePrograms p p1))
            (alreadyIncluded, baseProg) newIncludes

        includeInclude :: [FilePath] -> [FilePath] -> FilePath
                          -> ErrorIO ParseError ([FilePath], UncheckedProg)
        includeInclude alreadyIncluded includeSources newInclude = do
          s <- liftIO $ readFile newInclude
          parseWithIncludes alreadyIncluded includeSources (newInclude, s)

        mergePrograms :: UncheckedProg -> UncheckedProg -> UncheckedProg
        mergePrograms (Prog fs) (Prog gs) = Prog (fs ++ gs)

        headerInclude :: ProgHeader -> Maybe String
        headerInclude (Include strings) =
          Just $ (foldl (</>) search_dir strings) <.> "fut"

        search_dir = takeDirectory fp0

-- | Parse an Futhark expression from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseExp :: FilePath -> String
         -> Either ParseError UncheckedExp
parseExp = parse expression

-- | Parse an Futhark type from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseType :: FilePath -> String
          -> Either ParseError UncheckedType
parseType = parse futharktype

-- | Parse an Futhark anonymous function from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseLambda :: FilePath -> String
            -> Either ParseError UncheckedLambda
parseLambda = parse lambda

-- | Parse any Futhark value from the given 'String', using the 'FilePath'
-- as the source name for error messages.
parseValue :: FilePath -> String
           -> Either ParseError Value
parseValue = parse anyValue

-- | Parse several Futhark values (separated by anything) from the given
-- 'String', using the 'FilePath' as the source name for error
-- messages.
parseValues :: FilePath -> String
            -> Either ParseError [Value]
parseValues = parse anyValues
