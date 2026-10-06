-- | Facilities for reading Futhark test programs.  A Futhark test
-- program is an ordinary Futhark program where an initial comment
-- block specifies input- and output-sets.
module Futhark.Test
  ( module Futhark.Test.Property,
    module Futhark.Test.Spec,
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
    readResults,
    ensureReferenceOutput,
    determineTuning,
    determineCache,
    binaryName,
    futharkServerCfg,
    V.Mismatch,
    V.Value,
    V.valueText,
  )
where

import Codec.Compression.GZip
import Control.Applicative
import Control.Exception (catch)
import Control.Exception.Base qualified as E
import Control.Monad
import Control.Monad.Except (ExceptT (..), MonadError (..), liftEither, runExceptT, withExceptT)
import Control.Monad.Free.Church (F, runF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (first)
import Data.Binary qualified as Bin
import Data.ByteString qualified as SBS
import Data.ByteString.Lazy qualified as BS
import Data.Char
import Data.Either (fromRight)
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Futhark.Compiler (readProgramFilesExceptKnown)
import Futhark.Error (prettyCompilerError)
import Futhark.Eval (externaliseLast, interpretImports, runFFI)
import Futhark.FreshNames (VNameSource)
import Futhark.Server
import Futhark.Server.Values
import Futhark.Test.Compile
import Futhark.Test.Property
import Futhark.Test.Spec
import Futhark.Test.Values qualified as V
import Futhark.Util (ensureCacheDirectory, nubOrd, pmapIO, showText)
import Futhark.Util.Pretty (docText, prettyText, prettyTextOneLine)
import Language.Futhark
  ( DecBase (..),
    EntryParam (..),
    EntryPoint (..),
    EntryType (..),
    Exp,
    Info (..),
    Name,
    ProgBase (..),
    StructType,
    UncheckedExp,
    ValBindBase (..),
    baseName,
    isTupleRecord,
    nameToText,
    noSizes,
    typeOf,
  )
import Language.Futhark.Core (nameFromText)
import Language.Futhark.Interpreter qualified as I
import Language.Futhark.Interpreter.FFI.Push qualified as FFI
import Language.Futhark.Interpreter.FFI.ServerM qualified as FFI
import Language.Futhark.Interpreter.Values qualified as IV
import Language.Futhark.Parser (SyntaxError (..), parseExp)
import Language.Futhark.Semantic (Env, FileModule (..), Imports)
import Language.Futhark.Tuple (areTupleFields)
import Language.Futhark.TypeChecker (checkExp, prettyTypeError)
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

-- | Get the actual core Futhark values corresponding to a 'Values'
-- specification.  The first 'FilePath' is the path of the @futhark@
-- executable, and the second is the directory which file paths are
-- read relative to.
getValues :: (MonadFail m, MonadIO m) => FutharkExe -> FilePath -> Values -> m [V.Value]
getValues _ _ (Values vs) = pure vs
getValues futhark dir v = do
  s <- getValuesBS futhark dir v
  case valuesFromByteString (fileName v) s of
    Left e -> fail e
    Right vs -> pure vs
  where
    fileName Values {} = "<values>"
    fileName GenValues {} = "<randomly generated>"
    fileName ScriptValues {} = "<script expression>"
    fileName (InFile f) = f
    fileName (ScriptFile f) = f

readAndDecompress :: FilePath -> IO (Either DecompressError BS.ByteString)
readAndDecompress file = E.try $ do
  s <- BS.readFile file
  E.evaluate $ decompress s

-- | Extract a text representation of some 'Values'.  In the IO monad
-- because this might involve reading from a file.  There is no
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
  fail $
    "Cannot get values from script expression: "
      <> T.unpack (prettyTextOneLine e)
getValuesBS _ _ (ScriptFile f) =
  fail $ "Cannot get values from script file: " <> f

-- | Run an interpreter action that produces test input. Calls to entry points
-- are dispatched to the server, files are read relative to the given directory,
-- and traces and breakpoints are ignored.
runScript :: FFI.Server -> FilePath -> F I.ExtOp a -> IO (Either I.InterpreterError a)
runScript server dir m = runF m (pure . Right) intOp
  where
    intOp (I.ExtOpError err) = pure $ Left err
    intOp (I.ExtOpTrace _ _ c) = c
    intOp (I.ExtOpBreak _ _ _ c) = c
    intOp (I.ExtOpFFI sm c) = either (pure . Left) c =<< runFFI (Just server) sm
    intOp (I.ExtOpIO op c) =
      either (pure . Left . I.InterpreterError) c =<< I.doIOOp (I.ioRelativeTo dir op)

-- | The entry points of the program (the last import).
programEntryPoints :: Imports -> M.Map Name EntryPoint
programEntryPoints imports =
  M.fromList
    [ (baseName $ valBindName vb, ep)
    | ValDec vb <- progDecs $ fileProg $ snd $ last imports,
      Just (Info ep) <- [valBindEntryPoint vb]
    ]

-- | Read, type check and interpret the program, with its entry points run on
-- the server. Produces what is needed to type check and evaluate script
-- expressions in the context of the program, as well as the parameter types of
-- the given entry point.
scriptContext ::
  FFI.Server ->
  FilePath ->
  Name ->
  ExceptT T.Text IO (VNameSource, Env, I.Ctx, [StructType])
scriptContext server prog entry = do
  (_, imports, src) <-
    withExceptT (docText . prettyCompilerError) $
      readProgramFilesExceptKnown [] mempty [prog]
  (scope, ctx) <-
    withExceptT docText . interpretImports (runScript server $ takeDirectory prog) $
      externaliseLast imports
  ep <-
    maybe (throwError $ "Unknown entry point: " <> nameToText entry) pure $
      M.lookup entry $
        programEntryPoints imports
  pure (src, scope, ctx, map (entryType . entryParamType) $ entryParams ep)

-- | Split the result of a script expression into the inputs of an entry point
-- with these parameters: a tuple with an element for each, unless there is only
-- one.
splitInputs :: [p] -> (a -> Maybe [a]) -> a -> Maybe [a]
splitInputs [_] _ x = Just [x]
splitInputs _ untuple x = untuple x

-- | Type check a script expression, which must provide the inputs of an entry
-- point with these parameter types.
checkScriptExp :: VNameSource -> Env -> [StructType] -> UncheckedExp -> Either T.Text Exp
checkScriptExp src scope param_ts e =
  case checkExp [] src scope e of
    (_, Left terr) ->
      Left $ docText $ prettyTypeError terr
    (_, Right (_ : _, fexp)) ->
      Left $ "Ambiguous type of expression: " <> prettyText (typeOf fexp)
    (_, Right ([], fexp))
      | (map noSizes <$> splitInputs param_ts isTupleRecord t) == Just (map noSizes param_ts) ->
          Right fexp
      | otherwise ->
          Left . T.unlines $
            [ "Expected input of types: " <> T.unwords (map (prettyTextOneLine . noSizes) param_ts),
              "Provided input of type: " <> prettyTextOneLine (noSizes t)
            ]
      where
        t = typeOf fexp

-- | Evaluate a script expression with the interpreter, and make the result
-- available as server-side variables for the inputs of the given entry point.
-- If the entry point has more than one parameter, the value must be a tuple
-- with an element for each. The expression is evaluated in the context of the
-- program, with its entry points run on the server. Returns the variable of
-- each input, taken from the given names: one per input, except that inputs
-- provided with the same server-side value share a variable, named after the
-- first of them.
scriptValuesAsVars ::
  (MonadError T.Text m, MonadIO m) =>
  Server ->
  EntryName ->
  [VarName] ->
  FilePath ->
  UncheckedExp ->
  m [VarName]
scriptValuesAsVars server entry names prog e = do
  ffi_server <- liftIO $ FFI.newServer server
  let entry' = nameFromText entry
      onServer = fmap (first T.pack) . FFI.runServerM ffi_server
  r <- liftIO . runExceptT $ do
    (src, scope, ctx, param_ts) <- scriptContext ffi_server prog entry'
    fexp <- liftEither $ checkScriptExp src scope param_ts e
    v <-
      withExceptT (docText . I.prettyInterpreterError) . ExceptT $
        runScript ffi_server (takeDirectory prog) (I.interpretExp ctx fexp)
    -- The type check ensures that the value can be split.
    ExceptT . onServer . FFI.putArgs entry' . fromMaybe [] $
      splitInputs param_ts IV.fromTuple v
  -- Anything not adopted as an input is garbage, now that the interpreter is
  -- done - and if something failed, that is everything.
  released <- liftIO . onServer . FFI.release $ zip (fromRight [] r) names
  liftEither $ r *> released

-- | Make the provided 'Values' available as server-side variables, for use
-- as the inputs of the given entry point, and return the variable of each
-- input. These are the given names, except that several inputs may share a
-- variable (see 'scriptValuesAsVars').  This may involve arbitrary
-- server-side computation.  Error detection... dubious.  The 'FilePath' is
-- the program, relative to which other file paths are read.
valuesAsVars ::
  (MonadError T.Text m, MonadIO m) =>
  Server ->
  EntryName ->
  [(VarName, TypeName)] ->
  FutharkExe ->
  FilePath ->
  Values ->
  m [VarName]
valuesAsVars server entry names_and_types futhark prog =
  valuesAsVars' server entry names_and_types futhark prog (takeDirectory prog)

valuesAsVars' ::
  (MonadError T.Text m, MonadIO m) =>
  Server ->
  EntryName ->
  [(VarName, TypeName)] ->
  FutharkExe ->
  FilePath ->
  FilePath ->
  Values ->
  m [VarName]
valuesAsVars' server _ names_and_types _ _ dir (InFile file)
  | takeExtension file == ".gz" = do
      s <- liftIO $ readAndDecompress $ dir </> file
      case s of
        Left e ->
          throwError $ showText file <> ": " <> showText e
        Right s' ->
          cmdMaybe . withSystemTempFile "futhark-input" $ \tmpf tmpf_h -> do
            BS.hPutStr tmpf_h s'
            hClose tmpf_h
            cmdRestore server tmpf names_and_types
      pure $ map fst names_and_types
  | otherwise = do
      cmdMaybe $ cmdRestore server (dir </> file) names_and_types
      pure $ map fst names_and_types
valuesAsVars' server _ names_and_types futhark _ dir (GenValues gens) = do
  unless (length gens == length names_and_types) . throwError . T.unlines $
    [ "Expected "
        <> showText (length names_and_types)
        <> " input values of types",
      "  " <> T.unwords (map snd names_and_types),
      "Provided "
        <> showText (length gens)
        <> " input values of types",
      "  " <> T.unwords (map genValueType gens)
    ]
  gen_fs <- mapM (getGenFile futhark dir) gens
  forM_ (zip gen_fs names_and_types) $ \(file, (v, t)) ->
    cmdMaybe $ cmdRestore server (dir </> file) [(v, t)]
  pure $ map fst names_and_types
valuesAsVars' server _ names_and_types _ _ _ (Values vs) = do
  let types = map snd names_and_types
      vs_types = map (V.valueTypeTextNoDims . V.valueType) vs
  unless (types == vs_types) . throwError . T.unlines $
    [ "Expected input of types: " <> T.unwords (map prettyTextOneLine types),
      "Provided input of types: " <> T.unwords (map prettyTextOneLine vs_types)
    ]
  cmdMaybe . withSystemTempFile "futhark-input" $ \tmpf tmpf_h -> do
    mapM_ (BS.hPutStr tmpf_h . Bin.encode) vs
    hClose tmpf_h
    cmdRestore server tmpf names_and_types
  pure $ map fst names_and_types
valuesAsVars' server entry names_and_types _ prog _ (ScriptValues e) =
  scriptValuesAsVars server entry (map fst names_and_types) prog e
valuesAsVars' server entry names_and_types _ prog dir (ScriptFile f) = do
  let f' = dir </> f
  e <-
    either (\(SyntaxError _ err) -> throwError err) pure . parseExp f'
      =<< liftIO (T.readFile f')
  scriptValuesAsVars server entry (map fst names_and_types) prog e

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
getGenFile :: (MonadIO m) => FutharkExe -> FilePath -> GenValue -> m FilePath
getGenFile futhark dir gen = do
  liftIO $ ensureCacheDirectory $ dir </> "data"
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

getGenBS :: (MonadIO m) => FutharkExe -> FilePath -> GenValue -> m BS.ByteString
getGenBS futhark dir gen = liftIO . BS.readFile . (dir </>) =<< getGenFile futhark dir gen

genValues :: FutharkExe -> [GenValue] -> IO SBS.ByteString
genValues (FutharkExe futhark) gens = do
  (code, stdout, stderr) <-
    readProcessWithExitCode futhark ("dataset" : map T.unpack args) mempty
  case code of
    ExitSuccess ->
      pure stdout
    ExitFailure e ->
      fail $
        "'futhark dataset' failed with exit code "
          ++ show e
          ++ " and stderr:\n"
          ++ map (chr . fromIntegral) (SBS.unpack stderr)
  where
    args = "-b" : concatMap argForGen gens
    argForGen g = ["-g", genValueType g]

genFileName :: GenValue -> FilePath
genFileName gen = T.unpack (genValueType gen) <> ".in"

-- | Compute the expected size of the file.  We use this to check
-- whether an existing file is broken/truncated.
genFileSize :: GenValue -> Integer
genFileSize = genSize
  where
    header_size = 1 + 1 + 1 + 4 -- 'b' <version> <num_dims> <type>
    genSize (GenValue (V.ValueType ds t)) =
      toInteger $
        header_size
          + length ds * 8
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
      <> map clean (T.unpack (runDescription tr))
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

getValueM :: (MonadIO m, MonadError T.Text m) => Server -> VarName -> m V.Value
getValueM server = either throwError pure <=< liftIO . getValue server

-- Retrieve components of tuple.
getTupleElems ::
  (MonadIO m, MonadError T.Text m) =>
  Server ->
  VarName ->
  Int ->
  m [V.Value]
getTupleElems server v k = do
  -- We construct intermediate variables for the elements that we free at the
  -- end. However, they are leaked if we have a failure along the way, and
  -- getValueM may fail. This is not a big problem in practice we hope, as a
  -- failing test results in the server being shut down soon after.
  let is = [0 .. k - 1]
      elem_vs = [v <> "_elem" <> showText i | i <- is]
  forM_ (zip is elem_vs) $ \(i, elem_v) ->
    cmdMaybe $ cmdProject server elem_v v (showText i)
  mapM (getValueM server) elem_vs <* cmdMaybe (cmdFree server elem_vs)

isServerTuple ::
  (MonadIO m) =>
  Server ->
  TypeName ->
  m (Maybe [TypeName])
isServerTuple server v_t = do
  x <- liftIO $ cmdFields server v_t
  case x of
    Right fields -> do
      let onField f = (nameFromText $ fieldName f, fieldType f)
      case areTupleFields $ M.fromList $ map onField fields of
        Just ts -> pure $ Just ts
        Nothing -> pure Nothing
    Left _ -> pure Nothing

-- | Read the given variable from a running server. As a special case, if the
-- result is a tuple, we unpack it and return the elements individually.
readResults ::
  (MonadIO m, MonadError T.Text m) =>
  Server ->
  (VarName, TypeName) ->
  m [V.Value]
readResults server (v, v_t) = do
  maybe_elems <- isServerTuple server v_t
  case maybe_elems of
    Just ts ->
      getTupleElems server v $ length ts
    Nothing ->
      pure <$> getValueM server v

-- | Call an entry point. Returns server variable storing the result.
callEntry ::
  (MonadIO m, MonadError T.Text m) =>
  FutharkExe ->
  Server ->
  FilePath ->
  EntryName ->
  Values ->
  m VarName
callEntry futhark server prog entry input = do
  input_types <- cmdEither $ cmdInputs server entry
  let out = "out"
      ins = ["in" <> showText i | i <- [0 .. length input_types - 1]]
      ins_and_types = zip ins (map inputType input_types)
  ins' <- valuesAsVars server entry ins_and_types futhark prog input
  _ <- cmdEither $ cmdCall server entry out ins'
  cmdMaybe $ cmdFree server $ nubOrd ins'
  pure out

-- | Ensure that any reference output files exist, or create them (by
-- compiling the program with the reference compiler and running it on
-- the input) if necessary.
ensureReferenceOutput ::
  (MonadIO m, MonadError T.Text m) =>
  Maybe Int ->
  FutharkExe ->
  String ->
  FilePath ->
  [InputOutputs] ->
  m ()
ensureReferenceOutput concurrency futhark compiler prog ios = do
  missing <- filterM isReferenceMissing $ concatMap entryAndRuns ios

  unless (null missing) $ do
    void $ compileProgram ["--server"] futhark compiler prog

    res <- liftIO . flip (pmapIO concurrency) missing $ \(entry, tr) ->
      withServer server_cfg $ \server -> runExceptT $ do
        out <- callEntry futhark server prog entry $ runInput tr
        let f = file entry tr
        liftIO $ ensureCacheDirectory $ takeDirectory f
        cmdMaybe $ cmdStore server f [out]
        cmdMaybe $ cmdFree server [out]
    either throwError (const (pure ())) (sequence_ res)
  where
    server_cfg = futharkServerCfg ("." </> dropExtension prog) []

    file entry tr =
      takeDirectory prog </> testRunReferenceOutput prog entry tr

    entryAndRuns (InputOutputs entry rts) = map (entry,) rts

    isReferenceMissing (entry, tr)
      | Succeeds (Just SuccessGenerateValues) <- runExpectedResult tr =
          liftIO $
            ((<) <$> getModificationTime (file entry tr) <*> getModificationTime prog)
              `catch` (\e -> if isDoesNotExistError e then pure True else E.throw e)
      | otherwise =
          pure False

-- | Determine the @--tuning@ options to pass to the program.  The first
-- argument is the extension of the tuning file, or 'Nothing' if none
-- should be used.
determineTuning :: (MonadIO m) => Maybe FilePath -> FilePath -> m ([String], String)
determineTuning Nothing _ = pure ([], mempty)
determineTuning (Just ext) program = do
  exists <- liftIO $ doesFileExist (program <.> ext)
  if exists
    then
      pure
        ( ["--tuning", program <.> ext],
          " (using " <> takeFileName (program <.> ext) <> ")"
        )
    else pure ([], " (no tuning file)")

-- | Determine the @--cache-file@ options to pass to the program.  The
-- first argument is the extension of the cache file, or 'Nothing' if
-- none should be used.
determineCache :: Maybe FilePath -> FilePath -> [String]
determineCache Nothing _ = []
determineCache (Just ext) program = ["--cache-file", program <.> ext]

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
        T.pack actualf
          <> " and "
          <> T.pack expectedf
          <> " do not match:\n"
          <> showText mismatch
          <> if null mismatches
            then mempty
            else "\n...and " <> prettyText (length mismatches) <> " other mismatches."
    [] ->
      pure ()
