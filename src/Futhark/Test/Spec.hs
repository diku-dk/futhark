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
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void
import Futhark.Analysis.Metrics.Type
import Futhark.Data.Parser
import Futhark.Data.Parser qualified as V
import Futhark.Script qualified as Script
import Futhark.Test.Values qualified as V
import Futhark.Util (directoryContents, nubOrd, showText)
import Futhark.Util.Pretty (prettyTextOneLine)
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (charLiteral)
import Text.Regex.TDFA
import Prelude

-- | Description of a test to be carried out on a Futhark program.
-- The Futhark program is stored separately.
data ProgramTest = ProgramTest
  { testDescription :: T.Text,
    testTags :: [T.Text],
    testAction :: TestAction
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
  | MCPipeline
  | SOACSPipeline
  | SeqMemPipeline
  | GpuMemPipeline
  | MCMemPipeline
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
  { runTags :: [T.Text],
    runInput :: Values,
    runExpectedResult :: ExpectedResult Success,
    runIndex :: Int,
    runDescription :: T.Text
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
genValueType :: GenValue -> T.Text
genValueType (GenValue (V.ValueType ds t)) =
  foldMap (\d -> "[" <> showText d <> "]") ds <> V.primTypeText t
genValueType (GenPrim v) =
  V.valueText v

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
  lexeme sep $ L.foldl' addDigit 0 . map num <$> some digitChar
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
      RunCases
        <$> parseInputOutputs sep
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

parseRunTags :: Parser [T.Text]
parseRunTags = many . try . lexeme' $ do
  s <- some $ satisfy tagConstituent
  guard $ s `notElem` ["input", "structure", "warning"]
  pure $ T.pack s

parseStringLiteral :: Parser () -> Parser T.Text
parseStringLiteral sep =
  lexeme sep . fmap T.pack $ char '"' >> manyTill charLiteral (char '"')

parseRunCases :: Parser () -> Parser [TestRun]
parseRunCases sep = parseRunCases' (0 :: Int)
  where
    parseRunCases' i =
      (:) <$> parseRunCase i <*> parseRunCases' (i + 1)
        <|> pure []
    parseRunCase i = do
      name <- optional $ parseStringLiteral sep
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
      pure $ TestRun tags input expr i $ fromMaybe (desc i input) name

    -- If the file is gzipped, we strip the 'gz' extension from
    -- the dataset name.  This makes it more convenient to rename
    -- from 'foo.in' to 'foo.in.gz', as the reported dataset name
    -- does not change (which would make comparisons to historical
    -- data harder).
    desc _ (InFile path)
      | takeExtension path == ".gz" = T.pack $ dropExtension path
      | otherwise = T.pack path
    desc i (Values vs) =
      -- Turn linebreaks into space.
      "#" <> showText i <> " (\"" <> T.unwords (T.lines vs') <> "\")"
      where
        vs' = case T.unwords $ map V.valueText vs of
          s
            | T.length s > 50 -> T.take 50 s <> "..."
            | otherwise -> s
    desc _ (GenValues gens) =
      T.unwords $ map genValueType gens
    desc _ (ScriptValues e) =
      prettyTextOneLine e
    desc _ (ScriptFile path) =
      T.pack path

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
      lexeme sep "mc-mem" $> MCMemPipeline,
      lexeme sep "mc" $> MCPipeline,
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
          many pNonTestLine
            *> notFollowedBy "-- =="
            *> pure spec
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

validate :: FilePath -> ProgramTest -> Either String ProgramTest
validate path pt = do
  case testAction pt of
    CompileTimeFailure {} -> pure pt
    RunCases ios _ _ -> do
      mapM_ (noDups . map runDescription . iosTestRuns) ios
      Right pt
  where
    noDups xs =
      let xs' = nubOrd xs
       in -- Works because \\ only removes first instance.
          case xs L.\\ xs' of
            [] -> Right ()
            x : _ -> Left $ path <> ": multiple datasets with name " <> show (T.unpack x)

-- | Read the test specification from the given Futhark program.
testSpecFromProgram :: FilePath -> IO (Either String ProgramTest)
testSpecFromProgram path =
  ( either (Left . errorBundlePretty) (validate path) . parse pProgramTest path
      <$> T.readFile path
  )
    `catch` couldNotRead

-- | Like 'testSpecFromProgram', but exits the process on error.
testSpecFromProgramOrDie :: FilePath -> IO ProgramTest
testSpecFromProgramOrDie prog = do
  spec_or_err <- testSpecFromProgram prog
  case spec_or_err of
    Left err -> do
      hPutStrLn stderr err
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
      hPutStrLn stderr err
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
      hPutStrLn stderr err
      exitFailure
    Right spec -> pure spec
