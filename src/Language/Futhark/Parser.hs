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
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath (takeDirectory, (</>), (<.>))

import Prelude

import Language.Futhark.Syntax
import Language.Futhark.Attributes
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

-- Needed by @parseFuthark@, since that function might read files.  Kept as
-- simple as possible and without external dependencies.
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
--
-- Fails on cyclical includes.  Ignores repeat non-cyclical includes.
parseFuthark :: FilePath -> T.Text
                -> IO (Either ParseError UncheckedProg)
parseFuthark fp0 s0 =
  (snd <$>) <$> evalErrorIO (parseWithIncludes [fp0] [fp0] (fp0, s0))
  where parseWithIncludes :: [FilePath] -> [FilePath] -> (FilePath, T.Text)
                          -> ErrorIO ParseError ([FilePath], UncheckedProg)
        parseWithIncludes alreadyIncluded includeSources (fp, s) = do
          p <- liftEither $ parse prog fp s
          let newIncludes = mapMaybe headerInclude $ progWHHeaders p
              intersectionSources = includeSources `intersect` newIncludes

          unless (null intersectionSources) $ bad
            $ ParseError ("Include cycle with " ++ show intersectionSources ++ ".")

          let newIncludes' = newIncludes \\ alreadyIncluded
              alreadyIncluded' = fp : newIncludes' ++ alreadyIncluded
              includeSources' = fp : includeSources
              p' = Prog $ progWHDecs p

          if null newIncludes'
            then return (alreadyIncluded', p')
            else includeIncludes alreadyIncluded' includeSources' newIncludes' p'

        includeIncludes :: [FilePath] -> [FilePath] -> [FilePath] -> UncheckedProg
                          -> ErrorIO ParseError ([FilePath], UncheckedProg)
        includeIncludes alreadyIncluded includeSources newIncludes baseProg =
          foldM (\(already, p) new -> do
                    (already', p1) <- includeInclude already includeSources new
                    return (already', mergePrograms p1 p))
            (alreadyIncluded, baseProg) $ reverse newIncludes

        includeInclude :: [FilePath] -> [FilePath] -> FilePath
                          -> ErrorIO ParseError ([FilePath], UncheckedProg)
        includeInclude alreadyIncluded includeSources newInclude = do
          t <- liftIO $ T.readFile newInclude
          parseWithIncludes alreadyIncluded includeSources (newInclude, t)

        mergePrograms :: UncheckedProg -> UncheckedProg -> UncheckedProg
        mergePrograms (Prog defs) (Prog defs') = Prog (defs ++ defs')

        headerInclude :: ProgHeader -> Maybe String
        headerInclude (Include strings) =
          Just $ foldl (</>) search_dir strings <.> "fut"

        search_dir = takeDirectory fp0

-- | Parse an Futhark expression from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseExp :: FilePath -> T.Text
         -> Either ParseError UncheckedExp
parseExp = parse expression

-- | Parse an Futhark type from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseType :: FilePath -> T.Text
          -> Either ParseError UncheckedUserType
parseType = parse futharkType

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
