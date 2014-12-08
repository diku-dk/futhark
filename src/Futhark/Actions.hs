module Futhark.Actions
  ( printAction
  , externaliseAction
  , interpretAction
  , impCodeGenAction
  , seqCodegenAction
  , flowGraphAction
  )
where

import Control.Monad
import Data.Array (elems)
import Data.List
import System.Exit (exitWith, ExitCode(..))
import System.IO
import Text.Printf

import Futhark.Externalise
import Futhark.Pipeline
import Futhark.Analysis.Alias
import Futhark.Representation.AST hiding (Basic)
import Futhark.Representation.AST.Lore (Lore)
import Futhark.Interpreter
import qualified Futhark.SOACFlowGraph as FG
import qualified Futhark.CodeGen.ImpGen as ImpGen
import qualified Futhark.CodeGen.Backends.SequentialC as SequentialC

printAction :: Action
printAction = polyAction "prettyprinter" act
  where act (ExplicitMemory prog) = pp prog
        act (Basic prog) = pp prog
        pp :: PrettyLore lore => Prog lore -> IO ()
        pp = putStrLn . pretty . aliasAnalysis

externaliseAction :: Action
externaliseAction = basicAction "externalise" $
                    putStrLn . pretty . externaliseProg

interpretAction :: Show error => (FilePath -> String -> Either error [Value])
                -> Action
interpretAction = basicAction "interpreter" . interpret

seqCodegenAction :: Action
seqCodegenAction = explicitMemoryAction "sequential code generator" $
                   either error putStrLn . SequentialC.compileProg

impCodeGenAction :: Action
impCodeGenAction = explicitMemoryAction "imperative code generator" $
                   either error (putStrLn . pretty) . ImpGen.compileProgSimply

flowGraphAction :: Action
flowGraphAction = basicAction "SOAC flow graph" $
                  putStrLn . FG.makeFlowGraphString

interpret :: (Show error, Lore lore) =>
             (FilePath -> String -> Either error [Value])
          -> Prog lore -> IO ()
interpret parseValues prog =
  case funDecByName defaultEntryPoint prog of
    Nothing -> do hPutStrLn stderr "Interpreter error: no main function."
                  exitWith $ ExitFailure 2
    Just _ -> do
      parseres <- liftM (parseValues "<stdin>") getContents
      args <- case parseres of Left e -> do hPutStrLn stderr $ "Read error: " ++ show e
                                            exitWith $ ExitFailure 2
                               Right vs -> return vs
      let (res, trace) = runFun defaultEntryPoint args prog
      forM_ trace $ \(loc, what) ->
        hPutStrLn stderr $ locStr loc ++ ": " ++ what
      case res of
        Left err -> do hPutStrLn stderr $ "Interpreter error:\n" ++ show err
                       exitWith $ ExitFailure 2
        Right val  -> putStrLn $ ppOutput val
  where ppOutput vs = intercalate "\n" $ map ppOutput' vs
        ppOutput' val | Just s <- arrayString val = s
        ppOutput' (BasicVal (RealVal x)) = printf "%.6f" x
        ppOutput' (BasicVal (IntVal x))  = show x
        ppOutput' (BasicVal (CharVal c)) = show c
        ppOutput' (BasicVal (LogVal b))  = show b
        ppOutput' (BasicVal Checked) = "Checked"
        ppOutput' (ArrayVal a t)
          | [] <- elems a = "empty(" ++ pretty t ++ ")"
          | otherwise     = "[" ++ intercalate ", " (map ppOutput' $ elems a) ++ "]"
