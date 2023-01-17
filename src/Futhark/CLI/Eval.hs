module Futhark.CLI.Eval (main) where
  
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Free.Church

import Data.Maybe
import Data.Text qualified as T

import Futhark.Compiler
import Futhark.MonadFreshNames
-- import Futhark.Util (toPOSIX)
import Futhark.Util.Options

import Language.Futhark
import Language.Futhark.Interpreter qualified as I
import Language.Futhark.Parser
import Language.Futhark.Semantic qualified as T
import Language.Futhark.TypeChecker qualified as T

import System.FilePath

import Futhark.Pipeline

import System.IO
import Prelude
-- import Language.Futhark.Interpreter (interpretExp, ExtOp)


import Futhark.Util.Pretty
import Data.List (foldl')

main :: String -> [String] -> IO ()
-- main _ _ = putStrLn "Hello World"
main = mainWithOptions interpreterConfig options "options... <exprs...>" run
  where
    run [] _ = Nothing
    run exprs config = Just $ runExprs exprs config

runExprs :: [String] -> InterpreterConfig -> IO ()
runExprs exprs cfg = do
  -- putStrLn "Running expressions"
  let InterpreterConfig _ file = cfg
  -- putStrLn "File has come up."
  maybe_new_state <- newFutharkiState cfg file
  -- putStrLn "New state made"
  (src, env, ctx) <- case maybe_new_state of
    Left _ -> error "Unable to load file."
    Right s -> pure s
  -- putStrLn "Starting on the expressions"
  foldl' (\_ b -> runExpr src env ctx b) (putStr "") exprs
  putStrLn ""
-- runExprs _ _ = putStrLn "Running Exprs"



-- Use parseExp, checkExp, then interpretExp.
runExpr :: VNameSource -> T.Env -> I.Ctx -> String -> IO ()
runExpr src env ctx str = do
  uexp <- case parseExp "" (T.pack str) of 
    Left _ -> error "Syntax Error"
    Right e -> pure e
  let (_, expdat) = T.checkExp [] src env uexp
  fexp <- case expdat of
    Left _ -> error "Type Error"
    Right (_,e) -> pure e
  let ext = I.interpretExp ctx fexp
  pval <- runInterpreter' ext
  val <- case pval of
    Left _ -> error ""
    Right x -> pure x
  putDoc $ I.prettyValue val <> hardline

data InterpreterConfig = InterpreterConfig {
  interpreterPrintWarnings :: Bool,
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
  files <- case maybe_file of
    Just f -> pure [f]
    Nothing -> pure []
  (ws, imports, src) <-
    badOnLeft prettyCompilerError
      =<< liftIO
        ( runExceptT (readProgramFiles [] files)
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
  -- (tenv2, d2, src'') <-
  --   badOnLeft T.prettyTypeError . snd $
  --     T.checkDec imports src' tenv1 imp $
  --       mkOpen $
  --         toPOSIX $
  --           dropExtension file
  ienv2 <- badOnLeft I.prettyInterpreterError =<< runInterpreter' (I.interpretDec ienv1 d1)
  -- ienv3 <- badOnLeft I.prettyInterpreterError =<< runInterpreter' (I.interpretDec ienv2 d2)
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
