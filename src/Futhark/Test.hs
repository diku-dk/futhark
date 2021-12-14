{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Facilities for reading Futhark test programs.  A Futhark test
-- program is an ordinary Futhark program where an initial comment
-- block specifies input- and output-sets.
module Futhark.Test
  ( module Futhark.Test.Spec,
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
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Futhark.Script as Script
import Futhark.Server
import Futhark.Server.Values
import Futhark.Test.Spec
import qualified Futhark.Test.Values as V
import Futhark.Util (isEnvVarAtLeast, pmapIO)
import Futhark.Util.Pretty (prettyOneLine, prettyText, prettyTextOneLine)
import System.Directory
import System.Exit
import System.FilePath
import System.IO (IOMode (..), hClose, hFileSize, withFile)
import System.IO.Error
import System.IO.Temp
import System.Process.ByteString (readProcessWithExitCode)
import Prelude

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
  pure vs
getValues futhark dir v = do
  s <- getValuesBS futhark dir v
  case valuesFromByteString file s of
    Left e -> fail e
    Right vs -> pure vs
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
  pure $ BS.fromStrict $ T.encodeUtf8 $ T.unlines $ map V.valueText vs
getValuesBS _ dir (InFile file) =
  case takeExtension file of
    ".gz" -> liftIO $ do
      s <- readAndDecompress file'
      case s of
        Left e -> fail $ show file ++ ": " ++ show e
        Right s' -> pure s'
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
  let types = map snd names_and_types
      vs_types = map (V.valueTypeTextNoDims . V.valueType) vs
  unless (types == vs_types) . throwError . T.unlines $
    [ "Expected input of types: " <> prettyTextOneLine types,
      "Provided input of types: " <> prettyTextOneLine vs_types
    ]
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
    either throwError pure . Script.parseExpFromText f
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
            then pure False
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
      pure stdout
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
      getExpectedResult futhark prog entry tr'
      where
        tr' =
          tr
            { runExpectedResult =
                Succeeds . Just . SuccessValues . InFile $
                  testRunReferenceOutput prog entry tr
            }
    Succeeds Nothing ->
      pure $ Succeeds Nothing
    RunTimeFailure err ->
      pure $ RunTimeFailure err

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
    ExitSuccess -> pure ()
  pure (stdout, stderr)
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
            pure $
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
            pure $ Right ()

    case sequence_ res of
      Left err -> throwError err
      Right () -> pure ()
  where
    file (entry, tr) =
      takeDirectory prog </> testRunReferenceOutput prog entry tr

    entryAndRuns (InputOutputs entry rts) = map (entry,) rts

    isReferenceMissing (entry, tr)
      | Succeeds (Just SuccessGenerateValues) <- runExpectedResult tr =
        liftIO $
          ((<) <$> getModificationTime (file (entry, tr)) <*> getModificationTime prog)
            `catch` (\e -> if isDoesNotExistError e then pure True else E.throw e)
      | otherwise =
        pure False

-- | Determine the --tuning options to pass to the program.  The first
-- argument is the extension of the tuning file, or 'Nothing' if none
-- should be used.
determineTuning :: MonadIO m => Maybe FilePath -> FilePath -> m ([String], String)
determineTuning Nothing _ = pure ([], mempty)
determineTuning (Just ext) program = do
  exists <- liftIO $ doesFileExist (program <.> ext)
  if exists
    then
      pure
        ( ["--tuning", program <.> ext],
          " (using " <> takeFileName (program <.> ext) <> ")"
        )
    else pure ([], mempty)

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
      pure ()

-- | Create a Futhark server configuration suitable for use when
-- testing/benchmarking Futhark programs.
futharkServerCfg :: FilePath -> [String] -> ServerCfg
futharkServerCfg prog opts =
  (newServerCfg prog opts)
    { cfgDebug = isEnvVarAtLeast "FUTHARK_COMPILER_DEBUGGING" 1
    }
