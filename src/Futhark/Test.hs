{-# LANGUAGE OverloadedStrings #-}
-- | Facilities for reading Futhark test programs.  A Futhark test
-- program is an ordinary Futhark program where an initial comment
-- block specifies input- and output-sets.
module Futhark.Test
       ( testSpecFromFile
       , testSpecsFromPaths
       , valuesFromByteString
       , getValues
       , getValuesBS
       , compareValues
       , Mismatch

       , ProgramTest (..)
       , StructureTest (..)
       , StructurePipeline (..)
       , WarningTest (..)
       , TestAction (..)
       , ExpectedError (..)
       , InputOutputs (..)
       , TestRun (..)
       , ExpectedResult (..)
       , Values (..)
       , Value
       )
       where

import Control.Applicative
import qualified Data.ByteString.Lazy as BS
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import Data.Char
import Data.Functor
import Data.Maybe
import Data.Foldable (foldl')
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Void
import System.FilePath
import Codec.Compression.GZip
import Codec.Compression.Zlib.Internal (DecompressError)
import qualified Control.Exception.Base as E

import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char
import Text.Regex.TDFA

import Prelude

import Futhark.Analysis.Metrics
import Futhark.Util.Pretty (pretty, prettyText)
import Futhark.Test.Values
import Futhark.Util (directoryContents)

-- | Description of a test to be carried out on a Futhark program.
-- The Futhark program is stored separately.
data ProgramTest =
  ProgramTest { testDescription ::
                   T.Text
              , testTags ::
                   [T.Text]
              , testAction ::
                   TestAction
              }
  deriving (Show)

-- | How to test a program.
data TestAction
  = CompileTimeFailure ExpectedError
  | RunCases [InputOutputs] [StructureTest] [WarningTest]
  deriving (Show)

-- | Input and output pairs for some entry point(s).
data InputOutputs = InputOutputs { iosEntryPoint :: T.Text
                                 , iosTestRuns :: [TestRun] }
  deriving (Show)

-- | The error expected for a negative test.
data ExpectedError = AnyError
                   | ThisError T.Text Regex

instance Show ExpectedError where
  show AnyError = "AnyError"
  show (ThisError r _) = "ThisError " ++ show r

-- | How a program can be transformed.
data StructurePipeline = KernelsPipeline
                       | SOACSPipeline
                       | SequentialCpuPipeline
                       | GpuPipeline
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
               { runTags :: [String]
               , runInput :: Values
               , runExpectedResult :: ExpectedResult Values
               , runIndex :: Int
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

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme p = p <* space

-- | Like 'lexeme', but does not consume trailing linebreaks.
lexeme' :: Parser a -> Parser a
lexeme' p = p <* many (oneOf (" \t" :: String))

lexstr :: T.Text -> Parser ()
lexstr = void . try . lexeme . string

braces :: Parser a -> Parser a
braces p = lexstr "{" *> p <* lexstr "}"

parseNatural :: Parser Int
parseNatural = lexeme $ foldl' (\acc x -> acc * 10 + x) 0 .
               map num <$> some digitChar
  where num c = ord c - ord '0'

parseDescription :: Parser T.Text
parseDescription = lexeme $ T.pack <$> (anyChar `manyTill` parseDescriptionSeparator)

parseDescriptionSeparator :: Parser ()
parseDescriptionSeparator = try (string descriptionSeparator >>
                                 void (satisfy isSpace `manyTill` newline)) <|> eof

descriptionSeparator :: T.Text
descriptionSeparator = "=="

parseTags :: Parser [T.Text]
parseTags = lexstr "tags" *> braces (many parseTag) <|> pure []
  where parseTag = T.pack <$> lexeme (some $ satisfy constituent)
        constituent c = not (isSpace c) && c /= '}'

parseAction :: Parser TestAction
parseAction = CompileTimeFailure <$> (lexstr "error:" *> parseExpectedError) <|>
              (RunCases <$> parseInputOutputs <*>
               many parseExpectedStructure <*> many parseWarning)

parseInputOutputs :: Parser [InputOutputs]
parseInputOutputs = do
  entrys <- parseEntryPoints
  cases <- parseRunCases
  return $ map (`InputOutputs` cases) entrys

parseEntryPoints :: Parser [T.Text]
parseEntryPoints = (lexstr "entry:" *> many entry <* space) <|> pure ["main"]
  where constituent c = not (isSpace c) && c /= '}'
        entry = lexeme' $ T.pack <$> some (satisfy constituent)

parseRunTags :: Parser [String]
parseRunTags = many parseTag
  where parseTag = try $ lexeme $ do s <- some $ satisfy isAlphaNum
                                     guard $ s `notElem` ["input", "structure", "warning"]
                                     return s

parseRunCases :: Parser [TestRun]
parseRunCases = parseRunCases' (0::Int)
  where parseRunCases' i = (:) <$> parseRunCase i <*> parseRunCases' (i+1)
                           <|> pure []
        parseRunCase i = do
          tags <- parseRunTags
          input <- parseInput
          expr <- parseExpectedResult
          return $ TestRun tags input expr i $ desc i input

        -- If the file is gzipped, we strip the 'gz' extension from
        -- the dataset name.  This makes it more convenient to rename
        -- from 'foo.in' to 'foo.in.gz', as the reported dataset name
        -- does not change (which would make comparisons to historical
        -- data harder).
        desc _ (InFile path)
          | takeExtension path == ".gz" = dropExtension path
          | otherwise                   = path
        desc i (Values vs) =
          -- Turn linebreaks into space.
          "#" ++ show i ++ " (\"" ++ unwords (lines vs') ++ "\")"
          where vs' = case unwords (map pretty vs) of
                        s | length s > 50 -> take 50 s ++ "..."
                          | otherwise     -> s


parseExpectedResult :: Parser (ExpectedResult Values)
parseExpectedResult =
  (Succeeds . Just <$> (lexstr "output" *> parseValues)) <|>
  (RunTimeFailure <$> (lexstr "error:" *> parseExpectedError)) <|>
  pure (Succeeds Nothing)

parseExpectedError :: Parser ExpectedError
parseExpectedError = lexeme $ do
  s <- T.strip <$> restOfLine
  if T.null s
    then return AnyError
         -- blankCompOpt creates a regular expression that treats
         -- newlines like ordinary characters, which is what we want.
    else ThisError s <$> makeRegexOptsM blankCompOpt defaultExecOpt (T.unpack s)

parseInput :: Parser Values
parseInput = lexstr "input" *> parseValues

parseValues :: Parser Values
parseValues = do s <- parseBlock
                 case valuesFromByteString "input" $ BS.fromStrict $ T.encodeUtf8 s of
                   Left err -> fail err
                   Right vs -> return $ Values vs
              <|> lexstr "@" *> lexeme (InFile . T.unpack <$> nextWord)

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

nextWord :: Parser T.Text
nextWord = T.pack <$> (anyChar `manyTill` satisfy isSpace)

parseWarning :: Parser WarningTest
parseWarning = lexstr "warning:" >> parseExpectedWarning
  where parseExpectedWarning = lexeme $ do
          s <- T.strip <$> restOfLine
          ExpectedWarning s <$> makeRegexOptsM blankCompOpt defaultExecOpt (T.unpack s)

parseExpectedStructure :: Parser StructureTest
parseExpectedStructure =
  lexstr "structure" *>
  (StructureTest <$> optimisePipeline <*> parseMetrics)

optimisePipeline :: Parser StructurePipeline
optimisePipeline = lexstr "distributed" $> KernelsPipeline <|>
                   lexstr "gpu" $> GpuPipeline <|>
                   lexstr "cpu" $> SequentialCpuPipeline <|>
                   pure SOACSPipeline

parseMetrics :: Parser AstMetrics
parseMetrics = braces $ fmap (AstMetrics . M.fromList) $ many $
               (,) <$> (T.pack <$> lexeme (some (satisfy constituent))) <*> parseNatural
  where constituent c = isAlpha c || c == '/'

testSpec :: Parser ProgramTest
testSpec =
  ProgramTest <$> parseDescription <*> parseTags <*> parseAction

readTestSpec :: String -> T.Text -> Either (ParseError Char Void) ProgramTest
readTestSpec = parse $ testSpec <* eof

readInputOutputs :: String -> T.Text -> Either (ParseError Char Void) [InputOutputs]
readInputOutputs = parse $ parseDescription *> space *> parseInputOutputs <* eof

commentPrefix :: T.Text
commentPrefix = T.pack "--"

fixPosition :: Int -> ParseError Char Void -> ParseError Char Void
fixPosition lineno err =
  case err of TrivialError poss x y ->
                TrivialError (fmap fixup poss) x y
              FancyError poss x ->
                FancyError (fmap fixup poss) x
  where fixup pos =
          pos { sourceLine = sourceLine pos <> mkPos lineno
              , sourceColumn = sourceColumn pos <> mkPos (T.length commentPrefix)
              }

-- | Read the test specification from the given Futhark program.
-- Note: will call 'error' on parse errors.
testSpecFromFile :: FilePath -> IO ProgramTest
testSpecFromFile path = do
  blocks <- testBlocks <$> T.readFile path
  let (first_spec_line, first_spec, rest_specs) =
        case blocks of []       -> (0, mempty, [])
                       (n,s):ss -> (n, s, ss)
  case readTestSpec path first_spec of
    Left err -> error $ parseErrorPretty $ fixPosition (1+first_spec_line) err
    Right v  -> foldM moreCases v rest_specs

  where moreCases test (lineno, cases) =
          case readInputOutputs path cases of
            Left err     -> error $ parseErrorPretty $ fixPosition lineno err
            Right cases' ->
              case testAction test of
                RunCases old_cases structures warnings ->
                  return test { testAction = RunCases (old_cases ++ cases') structures warnings }
                _ -> fail "Secondary test block provided, but primary test block specifies compilation error."

testBlocks :: T.Text -> [(Int, T.Text)]
testBlocks = mapMaybe isTestBlock . commentBlocks
  where isTestBlock (n,block)
          | any ((" " <> descriptionSeparator) `T.isPrefixOf`) block =
              Just (n, T.unlines block)
          | otherwise =
              Nothing

commentBlocks :: T.Text -> [(Int, [T.Text])]
commentBlocks = commentBlocks' . zip [0..] . T.lines
  where isComment = (commentPrefix `T.isPrefixOf`)
        commentBlocks' ls =
          let ls' = dropWhile (not . isComment . snd) ls
          in case ls' of
            [] -> []
            (n,_) : _ ->
              let (block, ls'') = span (isComment . snd) ls'
                  block' = map (T.drop 2 . snd) block
              in (n, block') : commentBlocks' ls''

-- | Read test specifications from the given path, which can be a file
-- or directory containing @.fut@ files and further directories.
-- Calls 'error' on parse errors, or if the given path name does not
-- name a file that exists.
testSpecsFromPath :: FilePath -> IO [(FilePath, ProgramTest)]
testSpecsFromPath path = do
  programs <- testPrograms path
  zip programs <$> mapM testSpecFromFile programs

-- | Read test specifications from the given paths, which can be a
-- files or directories containing @.fut@ files and further
-- directories.  Calls 'error' on parse errors, or if any of the
-- immediately passed path names do not name a file that exists.
testSpecsFromPaths :: [FilePath] -> IO [(FilePath, ProgramTest)]
testSpecsFromPaths = fmap concat . mapM testSpecsFromPath

testPrograms :: FilePath -> IO [FilePath]
testPrograms dir = filter isFut <$> directoryContents dir
  where isFut = (==".fut") . takeExtension

-- | Try to parse a several values from a byte string.  The 'String'
-- parameter is used for error messages.
valuesFromByteString :: String -> BS.ByteString -> Either String [Value]
valuesFromByteString srcname =
  maybe (Left $ "Cannot parse values from " ++ srcname) Right . readValues

-- | Get the actual core Futhark values corresponding to a 'Values'
-- specification.  The 'FilePath' is the directory which file paths
-- are read relative to.
getValues :: MonadIO m => FilePath -> Values -> m [Value]
getValues _ (Values vs) =
  return vs
getValues dir (InFile file) = do
  s <- getValuesBS dir (InFile file)
  case valuesFromByteString file' s of
    Left e   -> fail $ show e
    Right vs -> return vs
  where file' = dir </> file

-- | Extract a pretty representation of some 'Values'.  In the IO
-- monad because this might involve reading from a file.  There is no
-- guarantee that the resulting byte string yields a readable value.
getValuesBS :: MonadIO m => FilePath -> Values -> m BS.ByteString
getValuesBS _ (Values vs) =
  return $ BS.fromStrict $ T.encodeUtf8 $ T.unlines $ map prettyText vs
getValuesBS dir (InFile file) =
  case takeExtension file of
   ".gz" -> liftIO $ do
     s <- E.try readAndDecompress
     case s of
       Left e   -> fail $ show file ++ ": " ++ show (e :: DecompressError)
       Right s' -> return s'

   _  -> liftIO $ BS.readFile file'
  where file' = dir </> file
        readAndDecompress = do s <- BS.readFile file'
                               E.evaluate $ decompress s
