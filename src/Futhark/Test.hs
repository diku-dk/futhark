-- | Facilities for reading Futhark test programs.  A Futhark test
-- program is an ordinary Futhark program where an initial comment
-- block specifies input- and output-sets.
module Futhark.Test
       ( testSpecFromFile
       , testSpecsFromPaths
       , valuesFromText
       , getValues
       , getValuesText
       , compareValues
       , Mismatch

       , ProgramTest (..)
       , StructureTest (..)
       , StructurePipeline (..)
       , TestAction (..)
       , ExpectedError (..)
       , TestRun (..)
       , RunMode (..)
       , ExpectedResult (..)
       , Values (..)
       , Value
       )
       where

import Control.Category ((>>>))
import Control.Applicative
import Control.Monad hiding (forM_)
import Control.Monad.IO.Class
import qualified Data.HashMap.Lazy as HM
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.List hiding (foldl')
import Data.Foldable (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory.Tree (readDirectoryWith, flattenDir,
                              DirTree(File), AnchoredDirTree(..),
                              FileName)
import System.IO
import System.FilePath

import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.Text
import Text.Parsec.Error
import Text.Regex.TDFA

import Prelude

import Futhark.Representation.SOACS (SOACS)
import Futhark.Representation.Kernels (Kernels)
import Futhark.Analysis.Metrics
import Futhark.Pipeline
import Futhark.Pass.Simplify
import Futhark.Pass.ExtractKernels
import Futhark.Passes
import Futhark.Util.Pretty (prettyText)
import Futhark.Test.Values

-- | Description of a test to be carried out on a Futhark program.
-- The Futhark program is stored separately.
data ProgramTest =
  ProgramTest { testDescription ::
                   T.Text
              , testTags ::
                   [T.Text]
              , testAction ::
                   TestAction
              , testExpectedStructure ::
                   Maybe StructureTest
              }
  deriving (Show)

-- | How to test a program.
data TestAction
  = CompileTimeFailure ExpectedError
  | RunCases [TestRun]
  deriving (Show)

-- | The error expected for a negative test.
data ExpectedError = AnyError
                   | ThisError T.Text Regex

instance Show ExpectedError where
  show AnyError = "AnyError"
  show (ThisError r _) = "ThisError " ++ show r

-- | How a program can be transformed.
data StructurePipeline = KernelsPipeline (Pipeline SOACS Kernels)
                       | SOACSPipeline (Pipeline SOACS SOACS)

-- | A structure test specifies a compilation pipeline, as well as
-- metrics for the program coming out the other end.
data StructureTest = StructureTest StructurePipeline AstMetrics

instance Show StructureTest where
  show (StructureTest _ metrics) =
    "StructureTest <config> " ++ show metrics

-- | The conditions under which a test case is to be run.
data RunMode
  = CompiledOnly -- ^ Cannot run with interpreter.
  | InterpretedOnly -- ^ Only run with interpreter.
  | NoTravis -- ^ Requires a lot of memory, do not run in Travis.
  | NoBench -- ^ When benchmarking, don't run this.
  | InterpretedAndCompiled -- ^ Can be interpreted or compiled.
  deriving (Eq, Show)

-- | A condition for execution, input, and expected result.
data TestRun = TestRun
               { runMode :: RunMode
               , runInput :: Values
               , runExpectedResult :: ExpectedResult Values
               , runDescription :: String
               }
             deriving (Show)

-- | Several Values - either literally, or by reference to a file.
data Values = Values [Value]
            | InFile FilePath
            deriving (Show)

-- | How a test case is expected to terminate.
data ExpectedResult values
  = Succeeds (Maybe values) -- ^ Execution suceeds, with or without
                            -- expected result values.
  | RunTimeFailure ExpectedError -- ^ Execution fails with this error.
  deriving (Show)

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

lexstr :: String -> Parser ()
lexstr = void . try . lexeme . string

braces :: Parser a -> Parser a
braces p = lexstr "{" *> p <* lexstr "}"

parseNatural :: Parser Int
parseNatural = lexeme $ foldl' (\acc x -> acc * 10 + x) 0 .
               map num <$> some digit
  where num c = ord c - ord '0'

parseDescription :: Parser T.Text
parseDescription = lexeme $ T.pack <$> (anyChar `manyTill` parseDescriptionSeparator)

parseDescriptionSeparator :: Parser ()
parseDescriptionSeparator = try (string descriptionSeparator >> void newline) <|> eof

descriptionSeparator :: String
descriptionSeparator = "=="

parseTags :: Parser [T.Text]
parseTags = lexstr "tags" *> braces (many parseTag) <|> pure []
  where parseTag = T.pack <$> lexeme (many1 $ satisfy constituent)
        constituent c = not (isSpace c) && c /= '}'

parseAction :: Parser TestAction
parseAction = CompileTimeFailure <$> (lexstr "error:" *> parseExpectedError) <|>
              RunCases <$> parseRunCases

parseRunMode :: Parser RunMode
parseRunMode = (lexstr "compiled" *> pure CompiledOnly) <|>
               (lexstr "nobench" *> pure NoBench) <|>
               (lexstr "notravis" *> pure NoTravis) <|>
               pure InterpretedAndCompiled

parseRunCases :: Parser [TestRun]
parseRunCases = parseRunCases' (0::Int)
  where parseRunCases' i = (:) <$> parseRunCase i <*> (parseRunCases' (i+1) <|> pure [])
        parseRunCase i = do
          runmode <- parseRunMode
          input <- parseInput
          expr <- parseExpectedResult
          return $ TestRun runmode input expr $ desc i input
        desc i (InFile path) = path
        desc i Values{}      = "#" ++ show i


parseExpectedResult :: Parser (ExpectedResult Values)
parseExpectedResult =
  (Succeeds . Just <$> (lexstr "output" *> parseValues)) <|>
  (RunTimeFailure <$> (lexstr "error:" *> parseExpectedError)) <|>
  pure (Succeeds Nothing)

parseExpectedError :: Parser ExpectedError
parseExpectedError = lexeme $ do
  s <- restOfLine
  if T.all isSpace s
    then return AnyError
         -- blankCompOpt creates a regular expression that treats
         -- newlines like ordinary characters, which is what we want.
    else ThisError s <$> makeRegexOptsM blankCompOpt defaultExecOpt (T.unpack s)

parseInput :: Parser Values
parseInput = lexstr "input" *> parseValues

parseValues :: Parser Values
parseValues = do s <- parseBlock
                 case valuesFromText "input" s of
                   Left err -> fail $ show err
                   Right vs -> return $ Values vs
              <|> lexstr "@" *> lexeme (InFile . T.unpack <$> restOfLine)

parseBlock :: Parser T.Text
parseBlock = lexeme $ braces (T.pack <$> parseBlockBody 0)

parseBlockBody :: Int -> Parser String
parseBlockBody n = do
  c <- lookAhead anyChar
  case (c,n) of
    ('}', 0) -> return mempty
    ('}', _) -> (:) <$> anyChar <*> parseBlockBody (n-1)
    ('{', _) -> (:) <$> anyChar <*> parseBlockBody (n+1)
    _        -> (:) <$> anyChar <*> parseBlockBody n

restOfLine :: Parser T.Text
restOfLine = T.pack <$> (anyChar `manyTill` (void newline <|> eof))

parseExpectedStructure :: Parser StructureTest
parseExpectedStructure =
  lexstr "structure" *>
  (StructureTest <$> optimisePipeline <*> parseMetrics)

optimisePipeline :: Parser StructurePipeline
optimisePipeline = lexstr "distributed" *> pure distributePipelineConfig <|>
                   pure defaultPipelineConfig
  where defaultPipelineConfig =
          SOACSPipeline standardPipeline
        distributePipelineConfig =
          KernelsPipeline $
          standardPipeline >>>
          onePass extractKernels >>>
          onePass simplifyKernels

parseMetrics :: Parser AstMetrics
parseMetrics = braces $ fmap HM.fromList $ many $
               (,) <$> (T.pack <$> lexeme (many1 (satisfy constituent))) <*> parseNatural
  where constituent c = isAlpha c || c == '/'

testSpec :: Parser ProgramTest
testSpec =
  ProgramTest <$> parseDescription <*> parseTags <*> parseAction <*> optional parseExpectedStructure

readTestSpec :: SourceName -> T.Text -> Either ParseError ProgramTest
readTestSpec = parse $ testSpec <* eof

commentPrefix :: T.Text
commentPrefix = T.pack "--"

fixPosition :: ParseError -> ParseError
fixPosition err =
  let newpos = incSourceColumn (errorPos err) $ T.length commentPrefix
  in setErrorPos newpos err

-- | Read the test specification from the given Futhark program.
-- Note: will call 'error' on parse errors.
testSpecFromFile :: FilePath -> IO ProgramTest
testSpecFromFile path = do
  s <- T.unlines .
       map (T.drop 2) .
       takeWhile (commentPrefix `T.isPrefixOf`) .
       T.lines <$>
       T.readFile path
  case readTestSpec path s of
    Left err -> error $ show $ fixPosition err
    Right v  -> return v

-- | Read test specifications from the given path, which can be a file
-- or directory containing @.fut@ files and further directories.
-- Calls 'error' on parse errors.
testSpecsFromPath :: FilePath -> IO [(FilePath, ProgramTest)]
testSpecsFromPath path = do
  programs <- testPrograms path
  zip programs <$> mapM testSpecFromFile programs

-- | Read test specifications from the given paths, which can be a files
-- or directories containing @.fut@ files and further directories.
-- Calls 'error' on parse errors.
testSpecsFromPaths :: [FilePath] -> IO [(FilePath, ProgramTest)]
testSpecsFromPaths = fmap concat . mapM testSpecsFromPath

testPrograms :: FilePath -> IO [FileName]
testPrograms dir = filter isFut <$> directoryContents dir
  where isFut = (==".fut") . takeExtension

directoryContents :: FilePath -> IO [FileName]
directoryContents dir = do
  _ :/ tree <- readDirectoryWith return dir
  return $ mapMaybe isFile $ flattenDir tree
  where isFile (File _ path) = Just path
        isFile _             = Nothing

-- | Try to parse a several values from a text.  The 'SourceName'
-- parameter is used for error messages.
valuesFromText :: SourceName -> T.Text -> Either String [Value]
valuesFromText srcname = maybe (Left srcname) Right . readValues

-- | Get the actual core Futhark values corresponding to a 'Values'
-- specification.  The 'FilePath' is the directory which file paths
-- are read relative to.
getValues :: MonadIO m => FilePath -> Values -> m [Value]
getValues _ (Values vs) =
  return vs
getValues dir (InFile file) = do
  s <- liftIO $ T.readFile file'
  case valuesFromText file' s of
    Left e   -> fail $ show e
    Right vs -> return vs
  where file' = dir </> file

-- | Extract a textual representation of some 'Values'.  In the IO
-- monad because this might involve reading from a file.  There is no
-- guarantee that the resulting text yields a readable value.
getValuesText :: MonadIO m => FilePath -> Values -> m T.Text
getValuesText _ (Values vs) =
  return $ T.unlines $ map prettyText vs
getValuesText dir (InFile file) =
  liftIO $ T.readFile file'
  where file' = dir </> file
