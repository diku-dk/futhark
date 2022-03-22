{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Definition and parsing of a test specification.
module Futhark.Test.Spec
  ( testSpecFromProgram,
    testSpecFromProgramOrDie,
    testSpecsFromPaths,
    testSpecsFromPathsOrDie,
    testSpecFromFile,
    testSpecFromFileOrDie,
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
  = GpuPipeline
  | SOACSPipeline
  | SeqMemPipeline
  | GpuMemPipeline
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

-- | How to generate a single random value.
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

lexeme :: Parser () -> Parser a -> Parser a
lexeme sep p = p <* sep

-- Like 'lexeme', but does not consume trailing linebreaks.
lexeme' :: Parser a -> Parser a
lexeme' p = p <* hspace

-- Like 'lexstr', but does not consume trailing linebreaks.
lexstr' :: T.Text -> Parser ()
lexstr' = void . try . lexeme' . string

inBraces :: Parser () -> Parser a -> Parser a
inBraces sep = between (lexeme sep "{") (lexeme sep "}")

parseNatural :: Parser () -> Parser Int
parseNatural sep =
  lexeme sep $ foldl' addDigit 0 . map num <$> some digitChar
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

parseDescription :: Parser () -> Parser T.Text
parseDescription sep =
  T.unlines <$> pDescLine `manyTill` pDescriptionSeparator
  where
    pDescLine = restOfLine <* sep
    pDescriptionSeparator = void $ "==" *> sep

parseTags :: Parser () -> Parser [T.Text]
parseTags sep = lexeme' "tags" *> inBraces sep (many parseTag) <|> pure []
  where
    parseTag = T.pack <$> lexeme sep (some $ satisfy tagConstituent)

tagConstituent :: Char -> Bool
tagConstituent c = isAlphaNum c || c == '_' || c == '-'

parseAction :: Parser () -> Parser TestAction
parseAction sep =
  choice
    [ CompileTimeFailure <$> (lexstr' "error:" *> parseExpectedError sep),
      RunCases <$> parseInputOutputs sep
        <*> many (parseExpectedStructure sep)
        <*> many (parseWarning sep)
    ]

parseInputOutputs :: Parser () -> Parser [InputOutputs]
parseInputOutputs sep = do
  entrys <- parseEntryPoints sep
  cases <- parseRunCases sep
  pure $
    if null cases
      then []
      else map (`InputOutputs` cases) entrys

parseEntryPoints :: Parser () -> Parser [T.Text]
parseEntryPoints sep =
  (lexeme' "entry:" *> many entry <* sep) <|> pure ["main"]
  where
    constituent c = not (isSpace c) && c /= '}'
    entry = lexeme' $ T.pack <$> some (satisfy constituent)

parseRunTags :: Parser [String]
parseRunTags = many . try . lexeme' $ do
  s <- some $ satisfy tagConstituent
  guard $ s `notElem` ["input", "structure", "warning"]
  pure s

parseRunCases :: Parser () -> Parser [TestRun]
parseRunCases sep = parseRunCases' (0 :: Int)
  where
    parseRunCases' i =
      (:) <$> parseRunCase i <*> parseRunCases' (i + 1)
        <|> pure []
    parseRunCase i = do
      tags <- parseRunTags
      void $ lexeme sep "input"
      input <-
        if "random" `elem` tags
          then parseRandomValues sep
          else
            if "script" `elem` tags
              then parseScriptValues sep
              else parseValues sep
      expr <- parseExpectedResult sep
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

parseExpectedResult :: Parser () -> Parser (ExpectedResult Success)
parseExpectedResult sep =
  choice
    [ lexeme sep "auto" *> lexeme sep "output" $> Succeeds (Just SuccessGenerateValues),
      Succeeds . Just . SuccessValues <$> (lexeme sep "output" *> parseValues sep),
      RunTimeFailure <$> (lexeme sep "error:" *> parseExpectedError sep),
      pure (Succeeds Nothing)
    ]

parseExpectedError :: Parser () -> Parser ExpectedError
parseExpectedError sep = lexeme sep $ do
  s <- T.strip <$> restOfLine_ <* sep
  if T.null s
    then pure AnyError
    else -- blankCompOpt creates a regular expression that treats
    -- newlines like ordinary characters, which is what we want.
      ThisError s <$> makeRegexOptsM blankCompOpt defaultExecOpt (T.unpack s)

parseScriptValues :: Parser () -> Parser Values
parseScriptValues sep =
  choice
    [ ScriptValues <$> inBraces sep (Script.parseExp sep),
      ScriptFile . T.unpack <$> (lexeme sep "@" *> lexeme sep nextWord)
    ]
  where
    nextWord = takeWhileP Nothing $ not . isSpace

parseRandomValues :: Parser () -> Parser Values
parseRandomValues sep = GenValues <$> inBraces sep (many (parseGenValue sep))

parseGenValue :: Parser () -> Parser GenValue
parseGenValue sep =
  choice
    [ GenValue <$> lexeme sep parseType,
      GenPrim <$> lexeme sep V.parsePrimValue
    ]

parseValues :: Parser () -> Parser Values
parseValues sep =
  choice
    [ Values <$> inBraces sep (many $ parseValue sep),
      InFile . T.unpack <$> (lexeme sep "@" *> lexeme sep nextWord)
    ]
  where
    nextWord = takeWhileP Nothing $ not . isSpace

parseWarning :: Parser () -> Parser WarningTest
parseWarning sep = lexeme sep "warning:" >> parseExpectedWarning
  where
    parseExpectedWarning = lexeme sep $ do
      s <- T.strip <$> restOfLine_
      ExpectedWarning s <$> makeRegexOptsM blankCompOpt defaultExecOpt (T.unpack s)

parseExpectedStructure :: Parser () -> Parser StructureTest
parseExpectedStructure sep =
  lexeme sep "structure" *> (StructureTest <$> optimisePipeline sep <*> parseMetrics sep)

optimisePipeline :: Parser () -> Parser StructurePipeline
optimisePipeline sep =
  choice
    [ lexeme sep "gpu-mem" $> GpuMemPipeline,
      lexeme sep "gpu" $> GpuPipeline,
      lexeme sep "seq-mem" $> SeqMemPipeline,
      lexeme sep "internalised" $> NoPipeline,
      pure SOACSPipeline
    ]

parseMetrics :: Parser () -> Parser AstMetrics
parseMetrics sep =
  inBraces sep . fmap (AstMetrics . M.fromList) . many $
    (,) <$> (T.pack <$> lexeme sep (some (satisfy constituent))) <*> parseNatural sep
  where
    constituent c = isAlpha c || c == '/'

testSpec :: Parser () -> Parser ProgramTest
testSpec sep =
  ProgramTest <$> parseDescription sep <*> parseTags sep <*> parseAction sep

couldNotRead :: IOError -> IO (Either String a)
couldNotRead = pure . Left . show

pProgramTest :: Parser ProgramTest
pProgramTest = do
  void $ many pNonTestLine
  maybe_spec <-
    optional ("--" *> sep *> testSpec sep) <* pEndOfTestBlock <* many pNonTestLine
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
    sep = void $ hspace *> optional (try $ eol *> "--" *> sep)

    noTest =
      ProgramTest mempty mempty (RunCases mempty mempty mempty)

    pEndOfTestBlock =
      (void eol <|> eof) *> notFollowedBy "--"
    pNonTestLine =
      void $ notFollowedBy "-- ==" *> restOfLine
    pInputOutputs =
      "--" *> sep *> parseDescription sep *> parseInputOutputs sep <* pEndOfTestBlock

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

testPrograms :: FilePath -> IO [FilePath]
testPrograms dir = filter isFut <$> directoryContents dir
  where
    isFut = (== ".fut") . takeExtension

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

-- | Read a test specification from a file.  Expects only a single
-- block, and no comment prefixes.
testSpecFromFile :: FilePath -> IO (Either String ProgramTest)
testSpecFromFile path =
  ( either (Left . errorBundlePretty) Right . parse (testSpec space) path
      <$> T.readFile path
  )
    `catch` couldNotRead

-- | Like 'testSpecFromFile', but kills the process on errors.
testSpecFromFileOrDie :: FilePath -> IO ProgramTest
testSpecFromFileOrDie dirs = do
  spec_or_err <- testSpecFromFile dirs
  case spec_or_err of
    Left err -> do
      putStrLn err
      exitFailure
    Right spec -> pure spec
