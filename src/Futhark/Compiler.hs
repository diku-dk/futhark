{-# LANGUAGE OverloadedStrings #-}
module Futhark.Compiler
       (
         runPipelineOnProgram
       , runCompilerOnProgram
       , interpretAction'
       , FutharkConfig (..)
       , newFutharkConfig
       , RealConfiguration (..)
       , dumpError
       )
where

import Data.Monoid
import Control.Monad
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

import qualified Futhark.Representation.External as E
import qualified Futhark.Representation.External.TypeChecker as E
import qualified Futhark.Representation.External.Renamer as E

import Futhark.MonadFreshNames
import Futhark.Representation.AST
import qualified Futhark.Representation.SOACS as I
import qualified Futhark.TypeCheck as I
import Futhark.Util.Log

data FutharkConfig = FutharkConfig {
    futharkVerbose :: Maybe (Maybe FilePath)
  , futharkBoundsCheck :: Bool
  , futharkRealConfiguration :: RealConfiguration
}

newFutharkConfig :: FutharkConfig
newFutharkConfig = FutharkConfig { futharkVerbose = Nothing
                                 , futharkBoundsCheck = True
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
  (res, msgs) <- runPipelineOnProgram config pipeline file
  T.hPutStrLn stderr $ toText msgs
  case res of
    Left err -> do
      dumpError config err
      exitWith $ ExitFailure 2
    Right prog -> do
      when (isJust $ futharkVerbose config) $
        hPutStrLn stderr $ "Running " ++ actionDescription action ++ "."
      (action_res, action_msgs) <- runFutharkM $ actionProcedure action prog
      T.hPutStrLn stderr $ toText action_msgs
      case action_res of
        Left err -> do
          dumpError config err
          exitWith $ ExitFailure 2
        Right () -> return ()

runPipelineOnProgram :: FutharkConfig
                     -> Pipeline I.SOACS tolore
                     -> FilePath
                     -> IO (Either CompileError (Prog tolore), Log)
runPipelineOnProgram config pipeline file = do
  contents <- readFile file
  runPipelineOnSource config pipeline file contents

runPipelineOnSource :: FutharkConfig
                    -> Pipeline I.SOACS tolore
                    -> FilePath
                    -> String
                    -> IO (Either CompileError (Prog tolore), Log)
runPipelineOnSource config pipeline filename srccode = do
  res <- runFutharkM futharkc'
  case res of (Left err, msgs)  -> return (Left err, msgs)
              (Right prog, msgs) -> return (Right prog, msgs)
  where futharkc' = do
          parsed_prog <- parseSourceProgram (futharkRealConfiguration config) filename srccode
          tagged_ext_prog <- E.tagProg <$> typeCheckSourceProgram parsed_prog
          putNameSource $ E.newNameSourceForProg tagged_ext_prog
          res <- internaliseProg (futharkBoundsCheck config) tagged_ext_prog
          case res of
            Left err ->
              compileErrorS "During internalisation:" err
            Right int_prog -> do
              typeCheckInternalProgram int_prog
              runPasses pipeline pipeline_config int_prog
        pipeline_config =
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
          liftM concat $ mapM internalise =<< parseValues rconf path s
        internalise v =
          maybe (Left $ ParseError $ "Invalid input value: " ++ I.pretty v) Right $
          internaliseValue v
