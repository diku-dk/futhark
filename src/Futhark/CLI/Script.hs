-- | @futhark script@
module Futhark.CLI.Script (main) where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Binary qualified as Bin
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Char (chr)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Futhark.CLI.Literate
  ( Options (..),
    initialOptions,
    prepareServer,
    scriptCommandLineOptions,
  )
import Futhark.Script
import Futhark.Test.Values (Compound (..), getValue, valueType)
import Futhark.Util.Options
import Futhark.Util.Pretty (prettyText)
import System.Exit
import System.IO

commandLineOptions :: [FunOptDescr Options]
commandLineOptions =
  scriptCommandLineOptions
    ++ [ Option
           "D"
           ["debug"]
           ( NoArg $ Right $ \config ->
               config
                 { scriptExtraOptions = "-D" : scriptExtraOptions config,
                   scriptVerbose = scriptVerbose config + 1
                 }
           )
           "Enable debugging.",
         Option
           "L"
           ["log"]
           ( NoArg $ Right $ \config ->
               config
                 { scriptExtraOptions = "-L" : scriptExtraOptions config,
                   scriptVerbose = scriptVerbose config + 1
                 }
           )
           "Enable logging.",
         Option
           "b"
           ["binary"]
           (NoArg $ Right $ \config -> config {scriptBinary = True})
           "Produce binary output.",
         Option
           "f"
           ["file"]
           ( ReqArg
               (\f -> Right $ \config -> config {scriptExps = scriptExps config ++ [Left f]})
               "FILE"
           )
           "Run FutharkScript from this file.",
         Option
           "e"
           ["expression"]
           ( ReqArg
               (\s -> Right $ \config -> config {scriptExps = scriptExps config ++ [Right (T.pack s)]})
               "EXP"
           )
           "Run this expression."
       ]

parseScriptFile :: FilePath -> IO Exp
parseScriptFile f = do
  s <- T.readFile f
  case parseExpFromText f s of
    Left e -> do
      T.hPutStrLn stderr e
      exitFailure
    Right e -> pure e

getExp :: Either FilePath T.Text -> IO Exp
getExp (Left f) = parseScriptFile f
getExp (Right s) = case parseExpFromText "command line option" s of
  Left e -> do
    T.hPutStrLn stderr e
    exitFailure
  Right e -> pure e

-- A few extra procedures that are not handled by scriptBuiltin.
extScriptBuiltin :: (MonadError T.Text m, MonadIO m) => EvalBuiltin m
extScriptBuiltin "store" [ValueAtom fv, ValueAtom vv]
  | Just path <- getValue fv = do
      let path' = map (chr . fromIntegral) (path :: [Bin.Word8])
      liftIO $ BS.writeFile path' $ Bin.encode vv
      pure $ ValueTuple []
extScriptBuiltin "store" vs =
  throwError $
    "$store does not accept arguments of types: "
      <> T.intercalate ", " (map (prettyText . fmap valueType) vs)
extScriptBuiltin f vs =
  scriptBuiltin "." f vs

-- | Run @futhark script@.
main :: String -> [String] -> IO ()
main = mainWithOptions initialOptions commandLineOptions "PROGRAM [EXP]" $ \args opts ->
  case args of
    [prog, script] -> Just $ main' prog opts $ scriptExps opts ++ [Right $ T.pack script]
    [prog] -> Just $ main' prog opts $ scriptExps opts
    _ -> Nothing
  where
    main' prog opts scripts = do
      scripts' <- mapM getExp scripts
      prepareServer prog opts $ \s -> do
        r <-
          runExceptT $ do
            vs <- mapM (evalExp extScriptBuiltin s) scripts'
            case reverse vs of
              [] -> pure Nothing
              v : _ -> Just <$> getExpValue s v
        case r of
          Left e -> do
            T.hPutStrLn stderr e
            exitFailure
          Right Nothing ->
            pure ()
          Right (Just v) ->
            if scriptBinary opts
              then case v of
                ValueAtom v' -> BS.putStr $ Bin.encode v'
                _ ->
                  T.hPutStrLn
                    stderr
                    "Result value cannot be represented in binary format."
              else T.putStrLn $ prettyText v
