{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Facilities for reading Futhark test programs.  A Futhark test
-- program is an ordinary Futhark program where an initial comment
-- block specifies input- and output-sets.
module Futhark.Test
  ( testSpecFromFile,
    testSpecFromFileOrDie,
    testSpecsFromPaths,
    testSpecsFromPathsOrDie,
    valuesFromByteString,
    FutharkExe (..),
    getValues,
    getValuesBS,
    valuesAsVars,
    V.compareValues,
    checkResult,
    testRunReferenceOutput,
    getExpectedResult,
    compileProgram,
    runProgram,
    readResults,
    ensureReferenceOutput,
    determineTuning,
    binaryName,
    futharkServerCfg,
    V.Mismatch,
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
    V.Value,
    V.valueText,
  )
where

import Codec.Compression.GZip
import Codec.Compression.Zlib.Internal (DecompressError)
import Control.Applicative
import Control.Exception (catch)
import qualified Control.Exception.Base as E
import Control.Monad
import Control.Monad.Except
import qualified Data.Binary as Bin
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Functor
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Void
import Futhark.Analysis.Metrics.Type
import Futhark.Data.Parser
import qualified Futhark.Data.Parser as V
import qualified Futhark.Script as Script
import Futhark.Server
import Futhark.Server.Values
import qualified Futhark.Test.Values as V
import Futhark.Util (directoryContents, isEnvVarAtLeast, pmapIO)
import Futhark.Util.Pretty (prettyOneLine, prettyText, prettyTextOneLine)
import System.Directory
import System.Exit
import System.FilePath
import System.IO (IOMode (..), hClose, hFileSize, withFile)
import System.IO.Error
import System.IO.Temp
import System.Process.ByteString (readProcessWithExitCode)
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
  lexeme $
    foldl' (\acc x -> acc * 10 + x) 0
      . map num
      <$> some digitChar
  where
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
  CompileTimeFailure <$> (lexstr' "error:" *> parseExpectedError)
    <|> ( RunCases <$> parseInputOutputs
            <*> many parseExpectedStructure
            <*> many parseWarning
        )

parseInputOutputs :: Parser [InputOutputs]
parseInputOutputs = do
  entrys <- parseEntryPoints
  cases <- parseRunCases
  return $
    if null cases
      then []
      else map (`InputOutputs` cases) entrys

parseEntryPoints :: Parser [T.Text]
parseEntryPoints =
  (lexeme' "entry:" *> many entry <* postlexeme)
    <|> pure ["main"]
  where
    constituent c = not (isSpace c) && c /= '}'
    entry = lexeme' $ T.pack <$> some (satisfy constituent)

parseRunTags :: Parser [String]
parseRunTags = many . try . lexeme' $ do
  s <- some $ satisfy tagConstituent
  guard $ s `notElem` ["input", "structure", "warning"]
  return s

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
      return $ TestRun tags input expr i $ desc i input

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
  (lexstr "auto" *> lexstr "output" $> Succeeds (Just SuccessGenerateValues))
    <|> (Succeeds . Just . SuccessValues <$> (lexstr "output" *> parseValues))
    <|> (RunTimeFailure <$> (lexstr "error:" *> parseExpectedError))
    <|> pure (Succeeds Nothing)

parseExpectedError :: Parser ExpectedError
parseExpectedError = lexeme $ do
  s <- T.strip <$> restOfLine_ <* postlexeme
  if T.null s
    then return AnyError
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
  lexstr "structure"
    *> (StructureTest <$> optimisePipeline <*> parseMetrics)

optimisePipeline :: Parser StructurePipeline
optimisePipeline =
  lexstr "distributed" $> KernelsPipeline
    <|> lexstr "gpu" $> GpuPipeline
    <|> lexstr "cpu" $> SequentialCpuPipeline
    <|> lexstr "internalised" $> NoPipeline
    <|> pure SOACSPipeline

parseMetrics :: Parser AstMetrics
parseMetrics =
  braces $
    fmap (AstMetrics . M.fromList) $
      many $
        (,) <$> (T.pack <$> lexeme (some (satisfy constituent))) <*> parseNatural
  where
    constituent c = isAlpha c || c == '/'

testSpec :: Parser ProgramTest
testSpec =
  ProgramTest <$> parseDescription <*> parseTags <*> parseAction

couldNotRead :: IOError -> IO (Either String a)
couldNotRead = return . Left . show

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
testSpecFromFile :: FilePath -> IO (Either String ProgramTest)
testSpecFromFile path =
  ( either (Left . errorBundlePretty) Right . parse pProgramTest path
      <$> T.readFile path
  )
    `catch` couldNotRead

-- | Like 'testSpecFromFile', but kills the process on error.
testSpecFromFileOrDie :: FilePath -> IO ProgramTest
testSpecFromFileOrDie prog = do
  spec_or_err <- testSpecFromFile prog
  case spec_or_err of
    Left err -> do
      putStrLn err
      exitFailure
    Right spec -> return spec

-- | Read test specifications from the given path, which can be a file
-- or directory containing @.fut@ files and further directories.
testSpecsFromPath :: FilePath -> IO (Either String [(FilePath, ProgramTest)])
testSpecsFromPath path = do
  programs_or_err <- (Right <$> testPrograms path) `catch` couldNotRead
  case programs_or_err of
    Left err -> return $ Left err
    Right programs -> do
      specs_or_errs <- mapM testSpecFromFile programs
      return $ zip programs <$> sequence specs_or_errs

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
    Right specs -> return specs

testPrograms :: FilePath -> IO [FilePath]
testPrograms dir = filter isFut <$> directoryContents dir
  where
    isFut = (== ".fut") . takeExtension

-- | Try to parse a several values from a byte string.  The 'String'
-- parameter is used for error messages.
valuesFromByteString :: String -> BS.ByteString -> Either String [V.Value]
valuesFromByteString srcname =
  maybe (Left $ "Cannot parse values from '" ++ srcname ++ "'") Right . V.readValues

-- | The @futhark@ executable we are using.  This is merely a wrapper
-- around the underlying file path, because we will be using a lot of
-- different file paths here, and it is easy to mix them up.
newtype FutharkExe = FutharkExe FilePath
  deriving (Eq, Ord, Show)

-- | Get the actual core Futhark values corresponding to a 'Values'
-- specification.  The first 'FilePath' is the path of the @futhark@
-- executable, and the second is the directory which file paths are
-- read relative to.
getValues :: (MonadFail m, MonadIO m) => FutharkExe -> FilePath -> Values -> m [V.Value]
getValues _ _ (Values vs) =
  return vs
getValues futhark dir v = do
  s <- getValuesBS futhark dir v
  case valuesFromByteString file s of
    Left e -> fail e
    Right vs -> return vs
  where
    file = case v of
      Values {} -> "<values>"
      InFile f -> f
      GenValues {} -> "<randomly generated>"
      ScriptValues {} -> "<FutharkScript expression>"
      ScriptFile f -> f

readAndDecompress :: FilePath -> IO (Either DecompressError BS.ByteString)
readAndDecompress file = E.try $ do
  s <- BS.readFile file
  E.evaluate $ decompress s

-- | Extract a pretty representation of some 'Values'.  In the IO
-- monad because this might involve reading from a file.  There is no
-- guarantee that the resulting byte string yields a readable value.
getValuesBS :: (MonadFail m, MonadIO m) => FutharkExe -> FilePath -> Values -> m BS.ByteString
getValuesBS _ _ (Values vs) =
  return $ BS.fromStrict $ T.encodeUtf8 $ T.unlines $ map V.valueText vs
getValuesBS _ dir (InFile file) =
  case takeExtension file of
    ".gz" -> liftIO $ do
      s <- readAndDecompress file'
      case s of
        Left e -> fail $ show file ++ ": " ++ show e
        Right s' -> return s'
    _ -> liftIO $ BS.readFile file'
  where
    file' = dir </> file
getValuesBS futhark dir (GenValues gens) =
  mconcat <$> mapM (getGenBS futhark dir) gens
getValuesBS _ _ (ScriptValues e) =
  fail $ "Cannot get values from FutharkScript expression: " <> prettyOneLine e
getValuesBS _ _ (ScriptFile f) =
  fail $ "Cannot get values from FutharkScript file: " <> f

valueAsVar ::
  (MonadError T.Text m, MonadIO m) =>
  Server ->
  VarName ->
  V.Value ->
  m ()
valueAsVar server v val =
  cmdMaybe $ putValue server v val

-- Frees the expression on error.
scriptValueAsVars ::
  (MonadError T.Text m, MonadIO m) =>
  Server ->
  [(VarName, TypeName)] ->
  Script.ExpValue ->
  m ()
scriptValueAsVars server names_and_types val
  | vals <- V.unCompound val,
    length names_and_types == length vals,
    Just loads <- zipWithM f names_and_types vals =
    sequence_ loads
  where
    f (v, t0) (V.ValueAtom (Script.SValue t1 sval))
      | t0 == t1 =
        Just $ case sval of
          Script.VVar oldname ->
            cmdMaybe $ cmdRename server oldname v
          Script.VVal sval' ->
            valueAsVar server v sval'
    f _ _ = Nothing
scriptValueAsVars server names_and_types val = do
  cmdMaybe $ cmdFree server $ S.toList $ Script.serverVarsInValue val
  throwError $
    "Expected value of type: "
      <> prettyTextOneLine (V.mkCompound (map (V.ValueAtom . snd) names_and_types))
      <> "\nBut got value of type:  "
      <> prettyTextOneLine (fmap Script.scriptValueType val)
      <> notes
  where
    notes = mconcat $ mapMaybe note names_and_types
    note (_, t)
      | "(" `T.isPrefixOf` t =
        Just $
          "\nNote: expected type " <> prettyText t <> " is an opaque tuple that cannot be constructed\n"
            <> "in FutharkScript.  Consider using type annotations to give it a proper name."
      | "{" `T.isPrefixOf` t =
        Just $
          "\nNote: expected type " <> prettyText t <> " is an opaque record that cannot be constructed\n"
            <> "in FutharkScript.  Consider using type annotations to give it a proper name."
      | otherwise =
        Nothing

-- | Make the provided 'Values' available as server-side variables.
-- This may involve arbitrary server-side computation.  Error
-- detection... dubious.
valuesAsVars ::
  (MonadError T.Text m, MonadIO m) =>
  Server ->
  [(VarName, TypeName)] ->
  FutharkExe ->
  FilePath ->
  Values ->
  m ()
valuesAsVars server names_and_types _ dir (InFile file)
  | takeExtension file == ".gz" = do
    s <- liftIO $ readAndDecompress $ dir </> file
    case s of
      Left e ->
        throwError $ T.pack $ show file <> ": " <> show e
      Right s' ->
        cmdMaybe . withSystemTempFile "futhark-input" $ \tmpf tmpf_h -> do
          BS.hPutStr tmpf_h s'
          hClose tmpf_h
          cmdRestore server tmpf names_and_types
  | otherwise =
    cmdMaybe $ cmdRestore server (dir </> file) names_and_types
valuesAsVars server names_and_types futhark dir (GenValues gens) = do
  unless (length gens == length names_and_types) $
    throwError "Mismatch between number of expected and generated values."
  gen_fs <- mapM (getGenFile futhark dir) gens
  forM_ (zip gen_fs names_and_types) $ \(file, (v, t)) ->
    cmdMaybe $ cmdRestore server (dir </> file) [(v, t)]
valuesAsVars server names_and_types _ _ (Values vs) = do
  unless (length vs == length names_and_types) $
    throwError "Mismatch between number of expected and provided values."
  cmdMaybe . withSystemTempFile "futhark-input" $ \tmpf tmpf_h -> do
    mapM_ (BS.hPutStr tmpf_h . Bin.encode) vs
    hClose tmpf_h
    cmdRestore server tmpf names_and_types
valuesAsVars server names_and_types _ _ (ScriptValues e) =
  Script.withScriptServer' server $ \server' -> do
    e_v <- Script.evalExp noBuiltin server' e
    scriptValueAsVars server names_and_types e_v
  where
    noBuiltin f _ = do
      throwError $ "Unknown builtin procedure: " <> f
valuesAsVars server names_and_types futhark dir (ScriptFile f) = do
  e <-
    either (throwError . T.pack . errorBundlePretty) pure
      . parse (Script.parseExp space) f
      =<< liftIO (T.readFile (dir </> f))
  valuesAsVars server names_and_types futhark dir (ScriptValues e)

-- | There is a risk of race conditions when multiple programs have
-- identical 'GenValues'.  In such cases, multiple threads in 'futhark
-- test' might attempt to create the same file (or read from it, while
-- something else is constructing it).  This leads to a mess.  To
-- avoid this, we create a temporary file, and only when it is
-- complete do we move it into place.  It would be better if we could
-- use file locking, but that does not work on some file systems.  The
-- approach here seems robust enough for now, but certainly it could
-- be made even better.  The race condition that remains should mostly
-- result in duplicate work, not crashes or data corruption.
getGenFile :: MonadIO m => FutharkExe -> FilePath -> GenValue -> m FilePath
getGenFile futhark dir gen = do
  liftIO $ createDirectoryIfMissing True $ dir </> "data"
  exists_and_proper_size <-
    liftIO $
      withFile (dir </> file) ReadMode (fmap (== genFileSize gen) . hFileSize)
        `catch` \ex ->
          if isDoesNotExistError ex
            then return False
            else E.throw ex
  unless exists_and_proper_size $
    liftIO $ do
      s <- genValues futhark [gen]
      withTempFile (dir </> "data") (genFileName gen) $ \tmpfile h -> do
        hClose h -- We will be writing and reading this ourselves.
        SBS.writeFile tmpfile s
        renameFile tmpfile $ dir </> file
  pure file
  where
    file = "data" </> genFileName gen

getGenBS :: MonadIO m => FutharkExe -> FilePath -> GenValue -> m BS.ByteString
getGenBS futhark dir gen = liftIO . BS.readFile . (dir </>) =<< getGenFile futhark dir gen

genValues :: FutharkExe -> [GenValue] -> IO SBS.ByteString
genValues (FutharkExe futhark) gens = do
  (code, stdout, stderr) <- readProcessWithExitCode futhark ("dataset" : args) mempty
  case code of
    ExitSuccess ->
      return stdout
    ExitFailure e ->
      fail $
        "'futhark dataset' failed with exit code " ++ show e ++ " and stderr:\n"
          ++ map (chr . fromIntegral) (SBS.unpack stderr)
  where
    args = "-b" : concatMap argForGen gens
    argForGen g = ["-g", genValueType g]

genFileName :: GenValue -> FilePath
genFileName gen = genValueType gen ++ ".in"

-- | Compute the expected size of the file.  We use this to check
-- whether an existing file is broken/truncated.
genFileSize :: GenValue -> Integer
genFileSize = genSize
  where
    header_size = 1 + 1 + 1 + 4 -- 'b' <version> <num_dims> <type>
    genSize (GenValue (V.ValueType ds t)) =
      toInteger $
        header_size + length ds * 8
          + product ds * V.primTypeBytes t
    genSize (GenPrim v) =
      toInteger $ header_size + product (V.valueShape v) * V.primTypeBytes (V.valueElemType v)

-- | When/if generating a reference output file for this run, what
-- should it be called?  Includes the "data/" folder.
testRunReferenceOutput :: FilePath -> T.Text -> TestRun -> FilePath
testRunReferenceOutput prog entry tr =
  "data"
    </> takeBaseName prog
    <> ":"
    <> T.unpack entry
    <> "-"
    <> map clean (runDescription tr)
    <.> "out"
  where
    clean '/' = '_' -- Would this ever happen?
    clean ' ' = '_'
    clean c = c

-- | Get the values corresponding to an expected result, if any.
getExpectedResult ::
  (MonadFail m, MonadIO m) =>
  FutharkExe ->
  FilePath ->
  T.Text ->
  TestRun ->
  m (ExpectedResult [V.Value])
getExpectedResult futhark prog entry tr =
  case runExpectedResult tr of
    (Succeeds (Just (SuccessValues vals))) ->
      Succeeds . Just <$> getValues futhark (takeDirectory prog) vals
    Succeeds (Just SuccessGenerateValues) ->
      getExpectedResult
        futhark
        prog
        entry
        tr
          { runExpectedResult =
              Succeeds $
                Just $
                  SuccessValues $
                    InFile $
                      testRunReferenceOutput prog entry tr
          }
    Succeeds Nothing ->
      return $ Succeeds Nothing
    RunTimeFailure err ->
      return $ RunTimeFailure err

-- | The name we use for compiled programs.
binaryName :: FilePath -> FilePath
binaryName = dropExtension

-- | @compileProgram extra_options futhark backend program@ compiles
-- @program@ with the command @futhark backend extra-options...@, and
-- returns stdout and stderr of the compiler.  Throws an IO exception
-- containing stderr if compilation fails.
compileProgram ::
  (MonadIO m, MonadError [T.Text] m) =>
  [String] ->
  FutharkExe ->
  String ->
  FilePath ->
  m (SBS.ByteString, SBS.ByteString)
compileProgram extra_options (FutharkExe futhark) backend program = do
  (futcode, stdout, stderr) <- liftIO $ readProcessWithExitCode futhark (backend : options) ""
  case futcode of
    ExitFailure 127 -> throwError [progNotFound $ T.pack futhark]
    ExitFailure _ -> throwError [T.decodeUtf8 stderr]
    ExitSuccess -> return ()
  return (stdout, stderr)
  where
    binOutputf = binaryName program
    options = [program, "-o", binOutputf] ++ extra_options
    progNotFound s = s <> ": command not found"

-- | @runProgram futhark runner extra_options prog entry input@ runs the
-- Futhark program @prog@ (which must have the @.fut@ suffix),
-- executing the @entry@ entry point and providing @input@ on stdin.
-- The program must have been compiled in advance with
-- 'compileProgram'.  If @runner@ is non-null, then it is used as
-- "interpreter" for the compiled program (e.g. @python@ when using
-- the Python backends).  The @extra_options@ are passed to the
-- program.
runProgram ::
  FutharkExe ->
  FilePath ->
  [String] ->
  String ->
  T.Text ->
  Values ->
  IO (ExitCode, SBS.ByteString, SBS.ByteString)
runProgram futhark runner extra_options prog entry input = do
  let progbin = binaryName prog
      dir = takeDirectory prog
      binpath = "." </> progbin
      entry_options = ["-e", T.unpack entry]

      (to_run, to_run_args)
        | null runner = (binpath, entry_options ++ extra_options)
        | otherwise = (runner, binpath : entry_options ++ extra_options)

  input' <- getValuesBS futhark dir input
  liftIO $ readProcessWithExitCode to_run to_run_args $ BS.toStrict input'

-- | Read the given variables from a running server.
readResults ::
  (MonadIO m, MonadError T.Text m) =>
  Server ->
  [VarName] ->
  m [V.Value]
readResults server =
  mapM (either throwError pure <=< liftIO . getValue server)

-- | Ensure that any reference output files exist, or create them (by
-- compiling the program with the reference compiler and running it on
-- the input) if necessary.
ensureReferenceOutput ::
  (MonadIO m, MonadError [T.Text] m) =>
  Maybe Int ->
  FutharkExe ->
  String ->
  FilePath ->
  [InputOutputs] ->
  m ()
ensureReferenceOutput concurrency futhark compiler prog ios = do
  missing <- filterM isReferenceMissing $ concatMap entryAndRuns ios

  unless (null missing) $ do
    void $ compileProgram [] futhark compiler prog

    res <- liftIO $
      flip (pmapIO concurrency) missing $ \(entry, tr) -> do
        (code, stdout, stderr) <- runProgram futhark "" ["-b"] prog entry $ runInput tr
        case code of
          ExitFailure e ->
            return $
              Left
                [ T.pack $
                    "Reference dataset generation failed with exit code "
                      ++ show e
                      ++ " and stderr:\n"
                      ++ map (chr . fromIntegral) (SBS.unpack stderr)
                ]
          ExitSuccess -> do
            let f = file (entry, tr)
            liftIO $ createDirectoryIfMissing True $ takeDirectory f
            SBS.writeFile f stdout
            return $ Right ()

    case sequence_ res of
      Left err -> throwError err
      Right () -> return ()
  where
    file (entry, tr) =
      takeDirectory prog </> testRunReferenceOutput prog entry tr

    entryAndRuns (InputOutputs entry rts) = map (entry,) rts

    isReferenceMissing (entry, tr)
      | Succeeds (Just SuccessGenerateValues) <- runExpectedResult tr =
        liftIO . fmap not . doesFileExist . file $ (entry, tr)
      | otherwise =
        return False

-- | Determine the --tuning options to pass to the program.  The first
-- argument is the extension of the tuning file, or 'Nothing' if none
-- should be used.
determineTuning :: MonadIO m => Maybe FilePath -> FilePath -> m ([String], String)
determineTuning Nothing _ = return ([], mempty)
determineTuning (Just ext) program = do
  exists <- liftIO $ doesFileExist (program <.> ext)
  if exists
    then
      return
        ( ["--tuning", program <.> ext],
          " (using " <> takeFileName (program <.> ext) <> ")"
        )
    else return ([], mempty)

-- | Check that the result is as expected, and write files and throw
-- an error if not.
checkResult ::
  (MonadError T.Text m, MonadIO m) =>
  FilePath ->
  [V.Value] ->
  [V.Value] ->
  m ()
checkResult program expected_vs actual_vs =
  case V.compareSeveralValues (V.Tolerance 0.002) actual_vs expected_vs of
    mismatch : mismatches -> do
      let actualf = program <.> "actual"
          expectedf = program <.> "expected"
      liftIO $ BS.writeFile actualf $ mconcat $ map Bin.encode actual_vs
      liftIO $ BS.writeFile expectedf $ mconcat $ map Bin.encode expected_vs
      throwError $
        T.pack actualf <> " and " <> T.pack expectedf
          <> " do not match:\n"
          <> T.pack (show mismatch)
          <> if null mismatches
            then mempty
            else "\n...and " <> prettyText (length mismatches) <> " other mismatches."
    [] ->
      return ()

-- | Create a Futhark server configuration suitable for use when
-- testing/benchmarking Futhark programs.
futharkServerCfg :: FilePath -> [String] -> ServerCfg
futharkServerCfg prog opts =
  (newServerCfg prog opts)
    { cfgDebug = isEnvVarAtLeast "FUTHARK_COMPILER_DEBUGGING" 1
    }
