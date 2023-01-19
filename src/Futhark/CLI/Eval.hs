module Futhark.CLI.Eval (main) where

import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Free.Church
import Data.Maybe
import Data.Text qualified as T
import Futhark.Compiler
import Futhark.MonadFreshNames

import Futhark.Pipeline
import Futhark.Util.Options

import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.Interpreter qualified as I
import Language.Futhark.Parser
import Language.Futhark.Semantic qualified as T
import Language.Futhark.TypeChecker qualified as T
import System.Exit
import System.FilePath
import System.IO
import Prelude

main :: String -> [String] -> IO ()
main = mainWithOptions interpreterConfig options "options... <exprs...>" run
  where
    run [] _ = Nothing
    run exprs config = Just $ runExprs exprs config

runExprs :: [String] -> InterpreterConfig -> IO ()
runExprs exprs cfg = do
  let InterpreterConfig _ file = cfg
  maybe_new_state <- newFutharkiState cfg file
  (src, env, ctx) <- case maybe_new_state of
    Left _ -> do
      hPutStrLn stderr $ fromJust file <> ": file not found."
      exitWith $ ExitFailure 2
    Right s -> pure s
  runExprs' exprs src env ctx

runExprs' :: [String] -> VNameSource -> T.Env -> I.Ctx -> IO ()
runExprs' [] _ _ _ = putStrLn ""
runExprs' (x : xs) src env ctx = do
  runExpr src env ctx x
  runExprs' xs src env ctx

-- Use parseExp, checkExp, then interpretExp.
runExpr :: VNameSource -> T.Env -> I.Ctx -> String -> IO ()
runExpr src env ctx str = do
  uexp <- case parseExp "" (T.pack str) of
    Left _ -> error $ "Syntax Error:" <> str
    Right e -> pure e
  fexp <- case T.checkExp [] src env uexp of
    (_, Left _) -> error $ "Type Error:" <> str
    (_, Right (_, e)) -> pure e
  let ext = I.interpretExp ctx fexp
  pval <- runInterpreter' ext
  val <- case pval of
    Left err -> do
      putDoc $ I.prettyInterpreterError err
      exitWith $ ExitFailure 0
    Right x -> pure x
  putDoc $ I.prettyValue val <> hardline

data InterpreterConfig = InterpreterConfig
  { interpreterPrintWarnings :: Bool,
    interpreterFile :: Maybe String
  }

interpreterConfig :: InterpreterConfig
interpreterConfig = InterpreterConfig True Nothing

options :: [FunOptDescr InterpreterConfig]
options =
  [ Option
      "f"
      ["file"]
      ( ReqArg
          ( \entry -> Right $ \config ->
              config {interpreterFile = Just entry}
          )
          "NAME"
      )
      "The file to load before evaluating expressions.",
    Option
      "w"
      ["no-warnings"]
      (NoArg $ Right $ \config -> config {interpreterPrintWarnings = False})
      "Do not print warnings."
  ]

newFutharkiState ::
  InterpreterConfig ->
  Maybe FilePath ->
  IO (Either (Doc AnsiStyle) (VNameSource, T.Env, I.Ctx))
newFutharkiState cfg maybe_file = runExceptT $ do
  (ws, imports, src) <-
    badOnLeft prettyCompilerError
      =<< liftIO
        ( runExceptT (readProgramFiles [] $ maybeToList maybe_file)
            `catch` \(err :: IOException) ->
              pure (externalErrorS (show err))
        )
  when (interpreterPrintWarnings cfg) $
    liftIO $
      hPutDoc stderr $
        prettyWarnings ws

  let imp = T.mkInitialImport "."
  ienv1 <-
    foldM (\ctx -> badOnLeft I.prettyInterpreterError <=< runInterpreter' . I.interpretImport ctx) I.initialCtx $
      map (fmap fileProg) imports
  (tenv1, d1, src') <-
    badOnLeft T.prettyTypeError . snd $
      T.checkDec imports src T.initialEnv imp $
        mkOpen "/prelude/prelude"
  ienv2 <- badOnLeft I.prettyInterpreterError =<< runInterpreter' (I.interpretDec ienv1 d1)
  pure (src', tenv1, ienv2)
  where
    badOnLeft :: (err -> err') -> Either err a -> ExceptT err' IO a
    badOnLeft _ (Right x) = pure x
    badOnLeft p (Left err) = throwError $ p err

runInterpreter' :: MonadIO m => F I.ExtOp a -> m (Either I.InterpreterError a)
runInterpreter' m = runF m (pure . Right) intOp
  where
    intOp (I.ExtOpError err) = pure $ Left err
    intOp (I.ExtOpTrace w v c) = do
      liftIO $ putDocLn $ pretty w <> ":" <+> align (unAnnotate v)
      c
    intOp (I.ExtOpBreak _ _ _ c) = c

mkOpen :: FilePath -> UncheckedDec
mkOpen f = OpenDec (ModImport f NoInfo mempty) mempty
