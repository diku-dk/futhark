{-# LANGUAGE OverloadedStrings #-}
module Futhark.Compiler
       (
         runPipelineOnProgram
       , runCompilerOnProgram
       , runPipelineOnSource
       , interpretAction'
       , FutharkConfig (..)
       , newFutharkConfig
       , RealConfiguration (..)
       , dumpError
       )
where

import Control.Applicative
import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import System.Exit (exitWith, ExitCode(..))
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Prelude

import Language.Futhark.Parser
import Futhark.Internalise
import Futhark.Pipeline
import Futhark.Actions

import qualified Language.Futhark as E
import qualified Futhark.Representation.External.TypeChecker as E
import qualified Futhark.Representation.External.Renamer as E

import Futhark.MonadFreshNames
import Futhark.Representation.AST
import qualified Futhark.Representation.SOACS as I
import qualified Futhark.TypeCheck as I
import Futhark.Util.Log

data FutharkConfig = FutharkConfig {
    futharkVerbose :: Maybe (Maybe FilePath)
  , futharkRealConfiguration :: RealConfiguration
}

newFutharkConfig :: FutharkConfig
newFutharkConfig = FutharkConfig { futharkVerbose = Nothing
                                 , futharkRealConfiguration = RealAsFloat64
                                 }

dumpError :: FutharkConfig -> CompileError -> IO ()
dumpError config err = do
  T.hPutStrLn stderr $ errorDesc err
  case (errorData err, futharkVerbose config) of
    (s, Just outfile) ->
      maybe (T.hPutStr stderr) T.writeFile outfile $ s <> "\n"
    _ -> return ()

runCompilerOnProgram :: FutharkConfig
                     -> Pipeline I.SOACS lore
                     -> Action lore
                     -> FilePath
                     -> IO ()
runCompilerOnProgram config pipeline action file = do
  (res, msgs) <- runFutharkM compile
  when (isJust $ futharkVerbose config) $
    liftIO $ T.hPutStrLn stderr $ toText msgs
  case res of
    Left err -> liftIO $ do
      dumpError config err
      exitWith $ ExitFailure 2
    Right () ->
      return ()
  where compile = do
          source <- liftIO $ readFile file
          prog <- runPipelineOnSource config pipeline file source
          when (isJust $ futharkVerbose config) $
            liftIO $ hPutStrLn stderr $ "Running " ++ actionDescription action ++ "."
          actionProcedure action prog

runPipelineOnProgram :: FutharkConfig
                     -> Pipeline I.SOACS tolore
                     -> FilePath
                     -> FutharkM (Prog tolore)
runPipelineOnProgram config pipeline file = do
  source <- liftIO $ readFile file
  runPipelineOnSource config pipeline file source

runPipelineOnSource :: FutharkConfig
                    -> Pipeline I.SOACS tolore
                    -> FilePath
                    -> String
                    -> FutharkM (Prog tolore)
runPipelineOnSource config pipeline filename srccode = do
  parsed_prog <- parseSourceProgram (futharkRealConfiguration config) filename srccode
  (tagged_ext_prog, namesrc) <- E.tagProg <$> typeCheckSourceProgram parsed_prog
  putNameSource namesrc
  res <- internaliseProg tagged_ext_prog
  case res of
    Left err ->
      compileErrorS "During internalisation:" err
    Right int_prog -> do
      typeCheckInternalProgram int_prog
      runPasses pipeline pipeline_config int_prog
  where pipeline_config =
          PipelineConfig { pipelineVerbose = isJust $ futharkVerbose config
                         , pipelineValidate = True
                         }

parseSourceProgram :: RealConfiguration -> FilePath -> String
                   -> FutharkM E.UncheckedProg
parseSourceProgram rconf filename file_contents =
  case parseFuthark rconf filename file_contents of
    Left err   -> compileError (T.pack $ show err) ()
    Right prog -> return prog

typeCheckSourceProgram :: E.UncheckedProg
                       -> FutharkM (E.ProgBase E.CompTypeBase I.Name)
typeCheckSourceProgram prog =
  case E.checkProg prog of
    Left err    -> compileError (T.pack $ show err) ()
    Right prog' -> return prog'

typeCheckInternalProgram :: I.Prog -> FutharkM ()
typeCheckInternalProgram prog =
  case I.checkProg prog of
    Left err -> compileError (T.pack $ "After internalisation:\n" ++ show err) prog
    Right () -> return ()

interpretAction' :: RealConfiguration -> Action I.SOACS
interpretAction' rconf =
  interpretAction parseValues'
  where parseValues' :: FilePath -> String -> Either ParseError [I.Value]
        parseValues' path s =
          fmap concat $ mapM internalise =<< parseValues rconf path s
        internalise v =
          maybe (Left $ ParseError $ "Invalid input value: " ++ I.pretty v) Right $
          internaliseValue v
