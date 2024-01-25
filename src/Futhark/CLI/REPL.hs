{-# LANGUAGE QuasiQuotes #-}

-- | @futhark repl@
module Futhark.CLI.REPL (main) where

import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Free.Church
import Control.Monad.State
import Data.Char
import Data.List (intersperse)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Version
import Futhark.Compiler
import Futhark.Format (parseFormatString)
import Futhark.MonadFreshNames
import Futhark.Util (fancyTerminal, showText)
import Futhark.Util.Options
import Futhark.Util.Pretty (AnsiStyle, Color (..), Doc, align, annotate, bgColorDull, bold, brackets, color, docText, docTextForHandle, hardline, italicized, oneLine, pretty, putDoc, putDocLn, unAnnotate, (<+>))
import Futhark.Version
import Language.Futhark
import Language.Futhark.Interpreter qualified as I
import Language.Futhark.Parser
import Language.Futhark.Semantic qualified as T
import Language.Futhark.TypeChecker qualified as T
import NeatInterpolation (text)
import System.Console.Haskeline qualified as Haskeline
import System.Directory
import System.IO (stdout)
import Text.Read (readMaybe)

banner :: Doc AnsiStyle
banner =
  mconcat . map ((<> hardline) . decorate . pretty) $
    [ "┃╱╱ ┃╲    ┃   ┃╲  ┃╲   ╱" :: T.Text,
      "┃╱  ┃ ╲   ┃╲  ┃╲  ┃╱  ╱ ",
      "┃   ┃  ╲  ┃╱  ┃   ┃╲  ╲ ",
      "┃   ┃   ╲ ┃   ┃   ┃ ╲  ╲"
    ]
  where
    decorate = annotate (bgColorDull Red <> bold <> color White)

-- | Run @futhark repl@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "options... [program.fut]" run
  where
    run [] _ = Just $ repl Nothing
    run [prog] _ = Just $ repl $ Just prog
    run _ _ = Nothing

data StopReason = EOF | Stop | Exit | Load FilePath | Interrupt

repl :: Maybe FilePath -> IO ()
repl maybe_prog = do
  when fancyTerminal $ do
    putDoc banner
    putStrLn $ "Version " ++ showVersion version ++ "."
    putStrLn "Copyright (C) DIKU, University of Copenhagen, released under the ISC license."
    putStrLn ""
    putDoc $ "Run" <+> annotate bold ":help" <+> "for a list of commands."
    putStrLn ""

  let toploop s = do
        (stop, s') <-
          Haskeline.handleInterrupt (pure (Left Interrupt, s))
            . Haskeline.withInterrupt
            $ runStateT (runExceptT $ runFutharkiM $ forever readEvalPrint) s

        case stop of
          Left Stop -> finish s'
          Left EOF -> finish s'
          Left Exit -> finish s'
          Left Interrupt -> do
            liftIO $ T.putStrLn "Interrupted"
            toploop s' {futharkiCount = futharkiCount s' + 1}
          Left (Load file) -> do
            liftIO $ T.putStrLn $ "Loading " <> T.pack file
            maybe_new_state <-
              liftIO $ newFutharkiState (futharkiCount s) (futharkiProg s) $ Just file
            case maybe_new_state of
              Right new_state -> toploop new_state
              Left err -> do
                liftIO $ putDocLn err
                toploop s'
          Right _ -> pure ()

      finish s = do
        quit <- if fancyTerminal then confirmQuit else pure True
        if quit then pure () else toploop s

  maybe_init_state <- liftIO $ newFutharkiState 0 noLoadedProg maybe_prog
  s <- case maybe_init_state of
    Left prog_err -> do
      noprog_init_state <- liftIO $ newFutharkiState 0 noLoadedProg Nothing
      case noprog_init_state of
        Left err ->
          error $ "Failed to initialise interpreter state: " <> T.unpack (docText err)
        Right s -> do
          liftIO $ putDoc prog_err
          pure s {futharkiLoaded = maybe_prog}
    Right s ->
      pure s
  Haskeline.runInputT Haskeline.defaultSettings $ toploop s

  putStrLn "Leaving 'futhark repl'."

confirmQuit :: Haskeline.InputT IO Bool
confirmQuit = do
  c <- Haskeline.getInputChar "Quit REPL? (y/n) "
  case c of
    Nothing -> pure True -- EOF
    Just 'y' -> pure True
    Just 'n' -> pure False
    _ -> confirmQuit

-- | Representation of breaking at a breakpoint, to allow for
-- navigating through the stack frames and such.
data Breaking = Breaking
  { breakingStack :: NE.NonEmpty I.StackFrame,
    -- | Index of the current breakpoint (with
    -- 0 being the outermost).
    breakingAt :: Int
  }

data FutharkiState = FutharkiState
  { futharkiProg :: LoadedProg,
    futharkiCount :: Int,
    futharkiEnv :: (T.Env, I.Ctx),
    -- | Are we currently stopped at a breakpoint?
    futharkiBreaking :: Maybe Breaking,
    -- | Skip breakpoints at these locations.
    futharkiSkipBreaks :: [Loc],
    futharkiBreakOnNaN :: Bool,
    -- | The currently loaded file.
    futharkiLoaded :: Maybe FilePath
  }

extendEnvs :: LoadedProg -> (T.Env, I.Ctx) -> [ImportName] -> (T.Env, I.Ctx)
extendEnvs prog (tenv, ictx) opens = (tenv', ictx')
  where
    tenv' = T.envWithImports t_imports tenv
    ictx' = I.ctxWithImports i_envs ictx
    t_imports = filter ((`elem` opens) . fst) $ lpImports prog
    i_envs = map snd $ filter ((`elem` opens) . fst) $ M.toList $ I.ctxImports ictx

newFutharkiState :: Int -> LoadedProg -> Maybe FilePath -> IO (Either (Doc AnsiStyle) FutharkiState)
newFutharkiState count prev_prog maybe_file = runExceptT $ do
  let files = maybeToList maybe_file
  -- Put code through the type checker.
  prog <-
    badOnLeft prettyProgErrors
      =<< liftIO (reloadProg prev_prog files M.empty)
  liftIO $ putDoc $ prettyWarnings $ lpWarnings prog
  -- Then into the interpreter.
  ictx <-
    foldM
      (\ctx -> badOnLeft (pretty . show) <=< runInterpreterNoBreak . I.interpretImport ctx)
      I.initialCtx
      $ map (fmap fileProg) (lpImports prog)

  let (tenv, ienv) =
        let (iname, fm) = last $ lpImports prog
         in ( fileScope fm,
              ictx {I.ctxEnv = I.ctxImports ictx M.! iname}
            )

  pure
    FutharkiState
      { futharkiProg = prog,
        futharkiCount = count,
        futharkiEnv = (tenv, ienv),
        futharkiBreaking = Nothing,
        futharkiSkipBreaks = mempty,
        futharkiBreakOnNaN = False,
        futharkiLoaded = maybe_file
      }
  where
    badOnLeft :: (err -> err') -> Either err a -> ExceptT err' IO a
    badOnLeft _ (Right x) = pure x
    badOnLeft p (Left err) = throwError $ p err

getPrompt :: FutharkiM String
getPrompt = do
  i <- gets futharkiCount
  fmap T.unpack $ liftIO $ docTextForHandle stdout $ annotate bold $ brackets (pretty i) <> "> "

-- The ExceptT part is more of a continuation, really.
newtype FutharkiM a = FutharkiM {runFutharkiM :: ExceptT StopReason (StateT FutharkiState (Haskeline.InputT IO)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState FutharkiState,
      MonadIO,
      MonadError StopReason
    )

readEvalPrint :: FutharkiM ()
readEvalPrint = do
  prompt <- getPrompt
  line <- inputLine prompt
  breaking <- gets futharkiBreaking
  case T.uncons line of
    Nothing
      | isJust breaking -> throwError Stop
      | otherwise -> pure ()
    Just (':', command) -> do
      let (cmdname, rest) = T.break isSpace command
          arg = T.dropWhileEnd isSpace $ T.dropWhile isSpace rest
      case filter ((cmdname `T.isPrefixOf`) . fst) commands of
        [] -> liftIO $ T.putStrLn $ "Unknown command '" <> cmdname <> "'"
        [(_, (cmdf, _))] -> cmdf arg
        matches ->
          liftIO . T.putStrLn $
            "Ambiguous command; could be one of "
              <> mconcat (intersperse ", " (map fst matches))
    _ -> do
      -- Read a declaration or expression.
      case parseDecOrExp prompt line of
        Left (SyntaxError _ err) -> liftIO $ T.putStrLn err
        Right (Left d) -> onDec d
        Right (Right e) -> do
          valOrErr <- onExp e
          case valOrErr of
            Left err -> liftIO $ putDocLn err
            Right val -> liftIO $ putDocLn $ I.prettyValue val
  modify $ \s -> s {futharkiCount = futharkiCount s + 1}
  where
    inputLine prompt = do
      inp <- FutharkiM $ lift $ lift $ Haskeline.getInputLine prompt
      case inp of
        Just s -> pure $ T.pack s
        Nothing -> throwError EOF

getIt :: FutharkiM (Imports, VNameSource, T.Env, I.Ctx)
getIt = do
  imports <- gets $ lpImports . futharkiProg
  src <- gets $ lpNameSource . futharkiProg
  (tenv, ienv) <- gets futharkiEnv
  pure (imports, src, tenv, ienv)

onDec :: UncheckedDec -> FutharkiM ()
onDec d = do
  old_imports <- gets $ lpImports . futharkiProg
  cur_import <- gets $ T.mkInitialImport . fromMaybe "." . futharkiLoaded
  let mkImport = T.mkImportFrom cur_import
      files = map (T.includeToFilePath . mkImport . fst) $ decImports d

  cur_prog <- gets futharkiProg
  imp_r <- liftIO $ extendProg cur_prog files M.empty
  case imp_r of
    Left e -> liftIO $ putDoc $ prettyProgErrors e
    Right prog -> do
      env <- gets futharkiEnv
      let (tenv, ienv) =
            extendEnvs prog env $ map (T.mkInitialImport . fst) $ decImports d
          imports = lpImports prog
          src = lpNameSource prog
      case T.checkDec imports src tenv cur_import d of
        (_, Left e) -> liftIO $ putDoc $ T.prettyTypeErrorNoLoc e
        (_, Right (tenv', d', src')) -> do
          let new_imports =
                filter ((`notElem` map fst old_imports) . fst) imports
          int_r <- runInterpreter $ do
            let onImport ienv' (s, imp) =
                  I.interpretImport ienv' (s, T.fileProg imp)
            ienv' <- foldM onImport ienv new_imports
            I.interpretDec ienv' d'
          case int_r of
            Left err -> liftIO $ print err
            Right ienv' -> modify $ \s ->
              s
                { futharkiEnv = (tenv', ienv'),
                  futharkiProg = prog {lpNameSource = src'}
                }

onExp :: UncheckedExp -> FutharkiM (Either (Doc AnsiStyle) I.Value)
onExp e = do
  (imports, src, tenv, ienv) <- getIt
  case T.checkExp imports src tenv e of
    (_, Left err) -> pure $ Left $ T.prettyTypeErrorNoLoc err
    (_, Right (tparams, e'))
      | null tparams -> do
          r <- runInterpreter $ I.interpretExp ienv e'
          case r of
            Left err -> pure $ Left $ pretty $ showText err
            Right v -> pure $ Right v
      | otherwise ->
          pure $
            Left $
              ("Inferred type of expression: " <> align (pretty (typeOf e')))
                <> hardline
                <> pretty
                  ( "The following types are ambiguous: "
                      <> T.intercalate
                        ", "
                        (map (nameToText . toName . typeParamName) tparams)
                  )
                <> hardline

prettyBreaking :: Breaking -> T.Text
prettyBreaking b =
  prettyStacktrace (breakingAt b) $ map locText $ NE.toList $ breakingStack b

-- Are we currently willing to break for this reason?  Among othe
-- things, we do not want recursive breakpoints.  It could work fine
-- technically, but is probably too confusing to be useful.
breakForReason :: FutharkiState -> I.StackFrame -> I.BreakReason -> Bool
breakForReason s _ I.BreakNaN
  | not $ futharkiBreakOnNaN s = False
breakForReason s top _ =
  isNothing (futharkiBreaking s)
    && locOf top `notElem` futharkiSkipBreaks s

runInterpreter :: F I.ExtOp a -> FutharkiM (Either I.InterpreterError a)
runInterpreter m = runF m (pure . Right) intOp
  where
    intOp (I.ExtOpError err) =
      pure $ Left err
    intOp (I.ExtOpTrace w v c) = do
      liftIO $ putDocLn $ pretty w <> ":" <+> unAnnotate v
      c
    intOp (I.ExtOpBreak w why callstack c) = do
      s <- get

      let why' = case why of
            I.BreakPoint -> "Breakpoint"
            I.BreakNaN -> "NaN produced"
          top = NE.head callstack
          ctx = I.stackFrameCtx top
          tenv = I.typeCheckerEnv $ I.ctxEnv ctx
          breaking = Breaking callstack 0

      -- Are we supposed to respect this breakpoint?
      when (breakForReason s top why) $ do
        liftIO $ T.putStrLn $ why' <> " at " <> locText w
        liftIO $ T.putStrLn $ prettyBreaking breaking
        liftIO $ T.putStrLn "<Enter> to continue."

        -- Note the cleverness to preserve the Haskeline session (for
        -- line history and such).
        (stop, s') <-
          FutharkiM . lift . lift $
            runStateT
              (runExceptT $ runFutharkiM $ forever readEvalPrint)
              s
                { futharkiEnv = (tenv, ctx),
                  futharkiCount = futharkiCount s + 1,
                  futharkiBreaking = Just breaking
                }

        case stop of
          Left (Load file) -> throwError $ Load file
          _ -> do
            liftIO $ putStrLn "Continuing..."
            put
              s
                { futharkiCount =
                    futharkiCount s',
                  futharkiSkipBreaks =
                    futharkiSkipBreaks s' <> futharkiSkipBreaks s,
                  futharkiBreakOnNaN =
                    futharkiBreakOnNaN s'
                }

      c

runInterpreterNoBreak :: (MonadIO m) => F I.ExtOp a -> m (Either I.InterpreterError a)
runInterpreterNoBreak m = runF m (pure . Right) intOp
  where
    intOp (I.ExtOpError err) = pure $ Left err
    intOp (I.ExtOpTrace w v c) = do
      liftIO $ putDocLn $ pretty w <> ":" <+> align (unAnnotate v)
      c
    intOp (I.ExtOpBreak _ I.BreakNaN _ c) = c
    intOp (I.ExtOpBreak w _ _ c) = do
      liftIO $
        T.putStrLn $
          locText w <> ": " <> "ignoring breakpoint when computating constant."
      c

type Command = T.Text -> FutharkiM ()

loadCommand :: Command
loadCommand file = do
  loaded <- gets futharkiLoaded
  case (T.null file, loaded) of
    (True, Just loaded') -> throwError $ Load loaded'
    (True, Nothing) -> liftIO $ T.putStrLn "No file specified and no file previously loaded."
    (False, _) -> throwError $ Load $ T.unpack file

genTypeCommand ::
  (String -> T.Text -> Either SyntaxError a) ->
  (Imports -> VNameSource -> T.Env -> a -> (Warnings, Either T.TypeError b)) ->
  (b -> Doc AnsiStyle) ->
  Command
genTypeCommand f g h e = do
  prompt <- getPrompt
  case f prompt e of
    Left (SyntaxError _ err) -> liftIO $ T.putStrLn err
    Right e' -> do
      (imports, src, tenv, _) <- getIt
      case snd $ g imports src tenv e' of
        Left err -> liftIO $ putDoc $ T.prettyTypeErrorNoLoc err
        Right x -> liftIO $ putDocLn $ h x

typeCommand :: Command
typeCommand = genTypeCommand parseExp T.checkExp $ \(ps, e) ->
  oneLine (pretty (typeOf e))
    <> if not (null ps)
      then
        annotate italicized $
          "\n\nPolymorphic in"
            <+> mconcat (intersperse " " $ map pretty ps)
            <> "."
      else mempty

mtypeCommand :: Command
mtypeCommand = genTypeCommand parseModExp T.checkModExp $ pretty . fst

formatCommand :: Command
formatCommand input = do
  case parseFormatString input of
    Left err -> liftIO $ T.putStrLn err
    Right parts -> do
      prompt <- getPrompt
      case mapM (traverse $ parseExp prompt) parts of
        Left (SyntaxError _ err) ->
          liftIO $ T.putStr err
        Right parts' -> do
          parts'' <- mapM sequenceA <$> mapM (traverse onExp) parts'
          case parts'' of
            Left err -> liftIO $ putDoc err
            Right parts''' ->
              liftIO . T.putStrLn . mconcat $
                map (either id (docText . I.prettyValue)) parts'''

unbreakCommand :: Command
unbreakCommand _ = do
  top <- gets $ fmap (NE.head . breakingStack) . futharkiBreaking
  case top of
    Nothing -> liftIO $ putStrLn "Not currently stopped at a breakpoint."
    Just top' -> do
      modify $ \s -> s {futharkiSkipBreaks = locOf top' : futharkiSkipBreaks s}
      throwError Stop

nanbreakCommand :: Command
nanbreakCommand _ = do
  modify $ \s -> s {futharkiBreakOnNaN = not $ futharkiBreakOnNaN s}
  b <- gets futharkiBreakOnNaN
  liftIO $
    putStrLn $
      if b
        then "Now treating NaNs as breakpoints."
        else "No longer treating NaNs as breakpoints."

frameCommand :: Command
frameCommand which = do
  maybe_stack <- gets $ fmap breakingStack . futharkiBreaking
  case (maybe_stack, readMaybe $ T.unpack which) of
    (Just stack, Just i)
      | frame : _ <- NE.drop i stack -> do
          let breaking = Breaking stack i
              ctx = I.stackFrameCtx frame
              tenv = I.typeCheckerEnv $ I.ctxEnv ctx
          modify $ \s ->
            s
              { futharkiEnv = (tenv, ctx),
                futharkiBreaking = Just breaking
              }
          liftIO $ T.putStrLn $ prettyBreaking breaking
    (Just _, _) ->
      liftIO $ putStrLn $ "Invalid stack index: " ++ T.unpack which
    (Nothing, _) ->
      liftIO $ putStrLn "Not stopped at a breakpoint."

pwdCommand :: Command
pwdCommand _ = liftIO $ putStrLn =<< getCurrentDirectory

cdCommand :: Command
cdCommand dir
  | T.null dir = liftIO $ putStrLn "Usage: ':cd <dir>'."
  | otherwise =
      liftIO $
        setCurrentDirectory (T.unpack dir)
          `catch` \(err :: IOException) -> print err

helpCommand :: Command
helpCommand _ = liftIO $
  forM_ commands $ \(cmd, (_, desc)) -> do
    putDoc $ annotate bold $ ":" <> pretty cmd <> hardline
    T.putStrLn $ T.replicate (1 + T.length cmd) "─"
    T.putStr desc
    T.putStrLn ""
    T.putStrLn ""

quitCommand :: Command
quitCommand _ = throwError Exit

commands :: [(T.Text, (Command, T.Text))]
commands =
  [ ( "load",
      ( loadCommand,
        [text|
Load a Futhark source file.  Usage:

  > :load foo.fut

If the loading succeeds, any expressions entered subsequently can use the
declarations in the source file.

Only one source file can be loaded at a time.  Using the :load command a
second time will replace the previously loaded file.  It will also replace
any declarations entered at the REPL.

|]
      )
    ),
    ( "format",
      ( formatCommand,
        [text|
Use format strings to print arbitrary futhark expressions. Usage:

  > :format The value of foo: {foo}. The value of 2+2={2+2}
|]
      )
    ),
    ( "type",
      ( typeCommand,
        [text|
Show the type of an expression, which must fit on a single line.
|]
      )
    ),
    ( "mtype",
      ( mtypeCommand,
        [text|
Show the type of a module expression, which must fit on a single line.
|]
      )
    ),
    ( "unbreak",
      ( unbreakCommand,
        [text|
Skip all future occurrences of the current breakpoint.
|]
      )
    ),
    ( "nanbreak",
      ( nanbreakCommand,
        [text|
Toggle treating operators that produce new NaNs as breakpoints.  We consider a NaN
to be "new" if none of the arguments to the operator in question is a NaN.
|]
      )
    ),
    ( "frame",
      ( frameCommand,
        [text|
While at a break point, jump to another stack frame, whose variables can then
be inspected.  Resuming from the breakpoint will jump back to the innermost
stack frame.
|]
      )
    ),
    ( "pwd",
      ( pwdCommand,
        [text|
Print the current working directory.
|]
      )
    ),
    ( "cd",
      ( cdCommand,
        [text|
Change the current working directory.
|]
      )
    ),
    ( "help",
      ( helpCommand,
        [text|
Print a list of commands and a description of their behaviour.
|]
      )
    ),
    ( "quit",
      ( quitCommand,
        [text|
Exit REPL.
|]
      )
    )
  ]
