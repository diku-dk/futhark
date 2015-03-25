module Futhark.Compiler
       (
         runCompilerOnProgram
       , runPipelineOnProgram
       , interpretAction'
       , newFutharkConfig
       )
where

import Control.Monad
import System.Exit (exitWith, ExitCode(..))
import System.IO

import Language.Futhark.Parser
import Futhark.Internalise
import Futhark.Pipeline
import Futhark.Actions

import qualified Futhark.Representation.External as E
import qualified Futhark.Representation.External.TypeChecker as E
import qualified Futhark.Representation.External.Renamer as E

import qualified Futhark.Representation.Basic as I
import qualified Futhark.TypeCheck as I

newFutharkConfig :: FutharkConfig
newFutharkConfig = FutharkConfig { futharkpipeline = []
                                 , futharkaction = printAction
                                 , futharkcheckAliases = True
                                 , futharkverbose = Nothing
                                 , futharkboundsCheck = True
                                 }

runCompilerOnProgram :: FutharkConfig -> FilePath -> IO ()
runCompilerOnProgram config file = do
  (msgs, res) <- runPipelineOnProgram config file
  hPutStr stderr msgs
  case res of
    Left err -> do
      hPutStrLn stderr $ errorDesc err
      case (errorState err, futharkverbose config) of
        (Just s, Just outfile) ->
          maybe (hPutStr stderr) writeFile outfile $
            I.pretty s ++ "\n"
        _ -> return ()
      exitWith $ ExitFailure 2
    Right s -> do
      let action = futharkaction config
      when (verbose config) $
        hPutStrLn stderr $ "Running " ++ actionDescription action ++ "."
      applyAction action s

runPipelineOnProgram :: FutharkConfig -> FilePath
                     -> IO (String, Either CompileError PipelineState)
runPipelineOnProgram config file = do
  contents <- readFile file
  runPipelineOnSource config file contents

runPipelineOnSource :: FutharkConfig -> FilePath -> String
                    -> IO (String, Either CompileError PipelineState)
runPipelineOnSource config filename srccode = do
  res <- runFutharkM futharkc'
  case res of (Left err, msgs)  -> return (msgs, Left err)
              (Right prog, msgs) -> return (msgs, Right prog)
  where futharkc' = do
          parsed_prog <- parseSourceProgram filename srccode
          ext_prog    <- typeCheckSourceProgram config parsed_prog
          case internaliseProg (futharkboundsCheck config) $ E.tagProg ext_prog of
            Left err ->
              compileError ("During internalisation:\n" ++ err) Nothing
            Right int_prog -> do
              typeCheckInternalProgram config int_prog
              runPasses config $ Basic int_prog

typeCheck :: (prog -> Either err prog')
          -> (prog -> Either err prog')
          -> FutharkConfig
          -> prog -> Either err prog'
typeCheck checkProg checkProgNoUniqueness config
  | futharkcheckAliases config = checkProg
  | otherwise                  = checkProgNoUniqueness

parseSourceProgram :: FilePath -> String
                   -> FutharkM E.UncheckedProg
parseSourceProgram filename file_contents =
  case parseFuthark filename file_contents of
    Left err   -> compileError (show err) Nothing
    Right prog -> return prog

typeCheckSourceProgram :: FutharkConfig
                       -> E.UncheckedProg
                       -> FutharkM (E.ProgBase E.CompTypeBase I.Name)
typeCheckSourceProgram config prog =
  case typeCheck E.checkProg E.checkProgNoUniqueness config prog of
    Left err    -> compileError (show err) Nothing
    Right prog' -> return prog'

typeCheckInternalProgram :: FutharkConfig -> I.Prog -> FutharkM ()
typeCheckInternalProgram config prog =
  case typeCheck I.checkProg I.checkProgNoUniqueness config prog of
    Left err -> compileError ("After internalisation:\n" ++ show err) $
                Just $ Basic prog
    Right () -> return ()

interpretAction' :: Action
interpretAction' = interpretAction parseValues'
  where parseValues' :: FilePath -> String -> Either ParseError [I.Value]
        parseValues' path s =
          liftM concat $ mapM internalise =<< parseValues path s
        internalise v =
          maybe (Left $ ParseError $ "Invalid input value: " ++ I.pretty v) Right $
          internaliseValue v
