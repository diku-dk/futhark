{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Definition and parsing of a test specification.
module Futhark.Test.Spec
  ( testSpecFromProgram,
    testSpecFromProgramOrDie,
    testSpecsFromPaths,
    testSpecsFromPathsOrDie,
    ProgramTest (..),
    StructureTest (..),
    StructurePipeline (..),
    WarningTest (..),
    TestAction (..),
    ExpectedError (..),
    InputOutputs (..),
    TestRun (..),
    ExpectedResult (..),
    Success (..),
    Values (..),
    GenValue (..),
    genValueType,
  )
where

import Control.Applicative
import Control.Exception (catch)
import Control.Monad
import Data.Char
import Data.Functor
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Futhark.Analysis.Metrics.Type
import Futhark.Data.Parser
import qualified Futhark.Data.Parser as V
import qualified Futhark.Script as Script
import qualified Futhark.Test.Values as V
import Futhark.Util (directoryContents)
import Futhark.Util.Pretty (prettyOneLine)
import System.Exit
import System.FilePath
import System.IO.Error
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char
import Text.Regex.TDFA
import Prelude

-- | Description of a test to be carried out on a Futhark program.
-- The Futhark program is stored separately.
data ProgramTest = ProgramTest
  { testDescription ::
      T.Text,
    testTags ::
      [T.Text],
    testAction ::
      TestAction
  }
  deriving (Show)

-- | How to test a program.
data TestAction
  = CompileTimeFailure ExpectedError
  | RunCases [InputOutputs] [StructureTest] [WarningTest]
  deriving (Show)

-- | Input and output pairs for some entry point(s).
data InputOutputs = InputOutputs
  { iosEntryPoint :: T.Text,
    iosTestRuns :: [TestRun]
  }
  deriving (Show)

-- | The error expected for a negative test.
data ExpectedError
  = AnyError
  | ThisError T.Text Regex

instance Show ExpectedError where
  show AnyError = "AnyError"
  show (ThisError r _) = "ThisError " ++ show r

-- | How a program can be transformed.
data StructurePipeline
  = KernelsPipeline
  | SOACSPipeline
  | SequentialCpuPipeline
  | GpuPipeline
  | NoPipeline
  deriving (Show)

-- | A structure test specifies a compilation pipeline, as well as
-- metrics for the program coming out the other end.
data StructureTest = StructureTest StructurePipeline AstMetrics
  deriving (Show)

-- | A warning test requires that a warning matching the regular
-- expression is produced.  The program must also compile succesfully.
data WarningTest = ExpectedWarning T.Text Regex

instance Show WarningTest where
  show (ExpectedWarning r _) = "ExpectedWarning " ++ T.unpack r

-- | A condition for execution, input, and expected result.
data TestRun = TestRun
  { runTags :: [String],
    runInput :: Values,
    runExpectedResult :: ExpectedResult Success,
    runIndex :: Int,
    runDescription :: String
  }
  deriving (Show)

-- | Several values - either literally, or by reference to a file, or
-- to be generated on demand.  All paths are relative to test program.
data Values
  = Values [V.Value]
  | InFile FilePath
  | GenValues [GenValue]
  | ScriptValues Script.Exp
  | ScriptFile FilePath
  deriving (Show)

data GenValue
  = -- | Generate a value of the given rank and primitive
    -- type.  Scalars are considered 0-ary arrays.
    GenValue V.ValueType
  | -- | A fixed non-randomised primitive value.
    GenPrim V.Value
  deriving (Show)

-- | A prettyprinted representation of type of value produced by a
-- 'GenValue'.
genValueType :: GenValue -> String
genValueType (GenValue (V.ValueType ds t)) =
  concatMap (\d -> "[" ++ show d ++ "]") ds ++ T.unpack (V.primTypeText t)
genValueType (GenPrim v) =
  T.unpack $ V.valueText v

-- | How a test case is expected to terminate.
data ExpectedResult values
  = -- | Execution suceeds, with or without
    -- expected result values.
    Succeeds (Maybe values)
  | -- | Execution fails with this error.
    RunTimeFailure ExpectedError
  deriving (Show)

-- | The result expected from a succesful execution.
data Success
  = -- | These values are expected.
    SuccessValues Values
  | -- | Compute expected values from executing a known-good
    -- reference implementation.
    SuccessGenerateValues
  deriving (Show)

type Parser = Parsec Void T.Text

postlexeme :: Parser ()
postlexeme = void $ hspace *> optional (try $ eol *> "--" *> postlexeme)

lexeme :: Parser a -> Parser a
lexeme p = p <* postlexeme

-- Like 'lexeme', but does not consume trailing linebreaks.
lexeme' :: Parser a -> Parser a
lexeme' p = p <* hspace

lexstr :: T.Text -> Parser ()
lexstr = void . try . lexeme . string

-- Like 'lexstr', but does not consume trailing linebreaks.
lexstr' :: T.Text -> Parser ()
lexstr' = void . try . lexeme' . string

braces :: Parser a -> Parser a
braces p = lexstr "{" *> p <* lexstr "}"

parseNatural :: Parser Int
parseNatural =
  lexeme $ foldl' addDigit 0 . map num <$> some digitChar
  where
    addDigit acc x = acc * 10 + x
    num c = ord c - ord '0'

restOfLine :: Parser T.Text
restOfLine = do
  l <- restOfLine_
  if T.null l then void eol else void eol <|> eof
  pure l

restOfLine_ :: Parser T.Text
restOfLine_ = takeWhileP Nothing (/= '\n')

parseDescription :: Parser T.Text
parseDescription =
  T.unlines <$> pDescLine `manyTill` pDescriptionSeparator
  where
    pDescLine = "--" *> restOfLine
    pDescriptionSeparator = void $ "-- ==" *> postlexeme

parseTags :: Parser [T.Text]
parseTags = lexeme' "tags" *> braces (many parseTag) <|> pure []
  where
    parseTag = T.pack <$> lexeme (some $ satisfy tagConstituent)

tagConstituent :: Char -> Bool
tagConstituent c = isAlphaNum c || c == '_' || c == '-'

parseAction :: Parser TestAction
parseAction =
  choice
    [ CompileTimeFailure <$> (lexstr' "error:" *> parseExpectedError),
      RunCases <$> parseInputOutputs
        <*> many parseExpectedStructure
        <*> many parseWarning
    ]

parseInputOutputs :: Parser [InputOutputs]
parseInputOutputs = do
  entrys <- parseEntryPoints
  cases <- parseRunCases
  pure $
    if null cases
      then []
      else map (`InputOutputs` cases) entrys

parseEntryPoints :: Parser [T.Text]
parseEntryPoints =
  (lexeme' "entry:" *> many entry <* postlexeme) <|> pure ["main"]
  where
    constituent c = not (isSpace c) && c /= '}'
    entry = lexeme' $ T.pack <$> some (satisfy constituent)

parseRunTags :: Parser [String]
parseRunTags = many . try . lexeme' $ do
  s <- some $ satisfy tagConstituent
  guard $ s `notElem` ["input", "structure", "warning"]
  pure s

parseRunCases :: Parser [TestRun]
parseRunCases = parseRunCases' (0 :: Int)
  where
    parseRunCases' i =
      (:) <$> parseRunCase i <*> parseRunCases' (i + 1)
        <|> pure []
    parseRunCase i = do
      tags <- parseRunTags
      lexstr "input"
      input <-
        if "random" `elem` tags
          then parseRandomValues
          else
            if "script" `elem` tags
              then parseScriptValues
              else parseValues
      expr <- parseExpectedResult
      pure $ TestRun tags input expr i $ desc i input

    -- If the file is gzipped, we strip the 'gz' extension from
    -- the dataset name.  This makes it more convenient to rename
    -- from 'foo.in' to 'foo.in.gz', as the reported dataset name
    -- does not change (which would make comparisons to historical
    -- data harder).
    desc _ (InFile path)
      | takeExtension path == ".gz" = dropExtension path
      | otherwise = path
    desc i (Values vs) =
      -- Turn linebreaks into space.
      "#" ++ show i ++ " (\"" ++ unwords (lines vs') ++ "\")"
      where
        vs' = case unwords $ map (T.unpack . V.valueText) vs of
          s
            | length s > 50 -> take 50 s ++ "..."
            | otherwise -> s
    desc _ (GenValues gens) =
      unwords $ map genValueType gens
    desc _ (ScriptValues e) =
      prettyOneLine e
    desc _ (ScriptFile path) =
      path

parseExpectedResult :: Parser (ExpectedResult Success)
parseExpectedResult =
  choice
    [ lexstr "auto" *> lexstr "output" $> Succeeds (Just SuccessGenerateValues),
      Succeeds . Just . SuccessValues <$> (lexstr "output" *> parseValues),
      RunTimeFailure <$> (lexstr "error:" *> parseExpectedError),
      pure (Succeeds Nothing)
    ]

parseExpectedError :: Parser ExpectedError
parseExpectedError = lexeme $ do
  s <- T.strip <$> restOfLine_ <* postlexeme
  if T.null s
    then pure AnyError
    else -- blankCompOpt creates a regular expression that treats
    -- newlines like ordinary characters, which is what we want.
      ThisError s <$> makeRegexOptsM blankCompOpt defaultExecOpt (T.unpack s)

parseScriptValues :: Parser Values
parseScriptValues =
  choice
    [ ScriptValues <$> braces (Script.parseExp postlexeme),
      ScriptFile . T.unpack <$> (lexstr "@" *> lexeme nextWord)
    ]
  where
    nextWord = takeWhileP Nothing $ not . isSpace

parseRandomValues :: Parser Values
parseRandomValues = GenValues <$> braces (many parseGenValue)

parseGenValue :: Parser GenValue
parseGenValue =
  choice
    [ GenValue <$> lexeme parseType,
      GenPrim <$> lexeme V.parsePrimValue
    ]

parseValues :: Parser Values
parseValues =
  choice
    [ Values <$> braces (many $ parseValue postlexeme),
      InFile . T.unpack <$> (lexstr "@" *> lexeme nextWord)
    ]
  where
    nextWord = takeWhileP Nothing $ not . isSpace

parseWarning :: Parser WarningTest
parseWarning = lexstr "warning:" >> parseExpectedWarning
  where
    parseExpectedWarning = lexeme $ do
      s <- T.strip <$> restOfLine_
      ExpectedWarning s <$> makeRegexOptsM blankCompOpt defaultExecOpt (T.unpack s)

parseExpectedStructure :: Parser StructureTest
parseExpectedStructure =
  lexstr "structure" *> (StructureTest <$> optimisePipeline <*> parseMetrics)

optimisePipeline :: Parser StructurePipeline
optimisePipeline =
  lexstr "distributed" $> KernelsPipeline
    <|> lexstr "gpu" $> GpuPipeline
    <|> lexstr "cpu" $> SequentialCpuPipeline
    <|> lexstr "internalised" $> NoPipeline
    <|> pure SOACSPipeline

parseMetrics :: Parser AstMetrics
parseMetrics =
  braces . fmap (AstMetrics . M.fromList) . many $
    (,) <$> (T.pack <$> lexeme (some (satisfy constituent))) <*> parseNatural
  where
    constituent c = isAlpha c || c == '/'

testSpec :: Parser ProgramTest
testSpec =
  ProgramTest <$> parseDescription <*> parseTags <*> parseAction

couldNotRead :: IOError -> IO (Either String a)
couldNotRead = pure . Left . show

pProgramTest :: Parser ProgramTest
pProgramTest = do
  void $ many pNonTestLine
  maybe_spec <- optional testSpec <* pEndOfTestBlock <* many pNonTestLine
  case maybe_spec of
    Just spec
      | RunCases old_cases structures warnings <- testAction spec -> do
        cases <- many $ pInputOutputs <* many pNonTestLine
        pure spec {testAction = RunCases (old_cases ++ concat cases) structures warnings}
      | otherwise ->
        many pNonTestLine *> notFollowedBy "-- ==" *> pure spec
          <?> "no more test blocks, since first test block specifies type error."
    Nothing ->
      eof $> noTest
  where
    noTest =
      ProgramTest mempty mempty (RunCases mempty mempty mempty)

    pEndOfTestBlock =
      (void eol <|> eof) *> notFollowedBy "--"
    pNonTestLine =
      void $ notFollowedBy "-- ==" *> restOfLine
    pInputOutputs =
      parseDescription *> parseInputOutputs <* pEndOfTestBlock

-- | Read the test specification from the given Futhark program.
testSpecFromProgram :: FilePath -> IO (Either String ProgramTest)
testSpecFromProgram path =
  ( either (Left . errorBundlePretty) Right . parse pProgramTest path
      <$> T.readFile path
  )
    `catch` couldNotRead

-- | Like 'testSpecFromProgram', but exits the process on error.
testSpecFromProgramOrDie :: FilePath -> IO ProgramTest
testSpecFromProgramOrDie prog = do
  spec_or_err <- testSpecFromProgram prog
  case spec_or_err of
    Left err -> do
      putStrLn err
      exitFailure
    Right spec -> pure spec

-- | Read test specifications from the given path, which can be a file
-- or directory containing @.fut@ files and further directories.
testSpecsFromPath :: FilePath -> IO (Either String [(FilePath, ProgramTest)])
testSpecsFromPath path = do
  programs_or_err <- (Right <$> testPrograms path) `catch` couldNotRead
  case programs_or_err of
    Left err -> pure $ Left err
    Right programs -> do
      specs_or_errs <- mapM testSpecFromProgram programs
      pure $ zip programs <$> sequence specs_or_errs

-- | Read test specifications from the given paths, which can be a
-- files or directories containing @.fut@ files and further
-- directories.
testSpecsFromPaths ::
  [FilePath] ->
  IO (Either String [(FilePath, ProgramTest)])
testSpecsFromPaths = fmap (fmap concat . sequence) . mapM testSpecsFromPath

-- | Like 'testSpecsFromPaths', but kills the process on errors.
testSpecsFromPathsOrDie ::
  [FilePath] ->
  IO [(FilePath, ProgramTest)]
testSpecsFromPathsOrDie dirs = do
  specs_or_err <- testSpecsFromPaths dirs
  case specs_or_err of
    Left err -> do
      putStrLn err
      exitFailure
    Right specs -> pure specs

testPrograms :: FilePath -> IO [FilePath]
testPrograms dir = filter isFut <$> directoryContents dir
  where
    isFut = (== ".fut") . takeExtension
