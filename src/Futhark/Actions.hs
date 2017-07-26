{-# LANGUAGE FlexibleContexts #-}
module Futhark.Actions
  ( printAction
  , interpretAction
  , impCodeGenAction
  , kernelImpCodeGenAction
  , seqCodeGenAction
  , rangeAction
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit (exitWith, ExitCode(..))
import System.IO

import Prelude

import Futhark.Pipeline
import Futhark.Analysis.Alias
import Futhark.Analysis.Range
import Futhark.Representation.AST
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.SOACS (SOACS)
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import Futhark.Interpreter
import qualified Futhark.CodeGen.ImpGen.Sequential as ImpGenSequential
import qualified Futhark.CodeGen.ImpGen.Kernels as ImpGenKernels
import qualified Futhark.CodeGen.Backends.SequentialC as SequentialC
import Futhark.Representation.AST.Attributes.Ranges (CanBeRanged)
import Futhark.Util.Pretty (text, ppr, prettyDoc, prettyText, brackets, (<>))

printAction :: (Attributes lore, CanBeAliased (Op lore)) => Action lore
printAction =
  Action { actionName = "Prettyprint"
         , actionDescription = "Prettyprint the resulting internal representation on standard output."
         , actionProcedure = liftIO . putStrLn . pretty . aliasAnalysis
         }

interpretAction :: Show error =>
                   (FilePath -> T.Text -> Either error [Value])
                -> Name
                -> Action SOACS
interpretAction parser entry =
  Action { actionName = "Interpret"
         , actionDescription = "Run the program via an interpreter."
         , actionProcedure = liftIO . interpret parser entry
         }

rangeAction :: (Attributes lore, CanBeRanged (Op lore)) => Action lore
rangeAction =
    Action { actionName = "Range analysis"
           , actionDescription = "Print the program with range annotations added."
           , actionProcedure = liftIO . putStrLn . pretty . rangeAnalysis
           }

seqCodeGenAction :: Action ExplicitMemory
seqCodeGenAction =
  Action { actionName = "Compile sequentially"
         , actionDescription = "Translate program into sequential C and write it on standard output."
         , actionProcedure = \prog ->
                               either (`internalError` prettyText prog) (liftIO . putStrLn) =<<
                               SequentialC.compileProg prog
         }


impCodeGenAction :: Action ExplicitMemory
impCodeGenAction =
  Action { actionName = "Compile imperative"
         , actionDescription = "Translate program into imperative IL and write it on standard output."
         , actionProcedure = \prog ->
                               either (`internalError` prettyText prog) (liftIO . putStrLn . pretty) =<<
                               ImpGenSequential.compileProg prog
         }

kernelImpCodeGenAction :: Action ExplicitMemory
kernelImpCodeGenAction =
  Action { actionName = "Compile imperative kernels"
         , actionDescription = "Translate program into imperative IL with kernels and write it on standard output."
         , actionProcedure = \prog ->
                               either (`internalError` prettyText prog) (liftIO . putStrLn . pretty) =<<
                               ImpGenKernels.compileProg prog
         }

interpret :: Show error =>
             (FilePath -> T.Text -> Either error [Value])
          -> Name -> Prog SOACS -> IO ()
interpret parseValues entry prog =
  case funDefByName entry prog of
    Nothing -> do hPutStrLn stderr "Interpreter error: no main function."
                  exitWith $ ExitFailure 2
    Just fundef -> do
      parseres <- fmap (parseValues "<stdin>") T.getContents
      args <- case parseres of Left e -> do hPutStrLn stderr $ "Read error: " ++ show e
                                            exitWith $ ExitFailure 2
                               Right vs -> return vs
      case runFunWithShapes entry args prog of
        Left err  -> do hPutStrLn stderr $ "Interpreter error:\n" ++ show err
                        exitWith $ ExitFailure 2
        Right val -> putStrLn $ ppOutput val $
                     fromMaybe (repeat TypeDirect) $ snd <$> funDefEntryPoint fundef
  where ppOutput vs epts = intercalate "\n" $ zipWith prettyRetVal epts vs
        prettyRetVal ept v = prettyDoc 80 $ ppArray (prettyType ept) (prettyPrim ept) v
        prettyPrim TypeUnsigned (IntValue (Int8Value v))  =
          text $ show (fromIntegral v :: Word8) ++ "u8"
        prettyPrim TypeUnsigned (IntValue (Int16Value v)) =
          text $ show (fromIntegral v :: Word16) ++ "u16"
        prettyPrim TypeUnsigned (IntValue (Int32Value v)) =
          text $ show (fromIntegral v :: Word32) ++ "u32"
        prettyPrim TypeUnsigned (IntValue (Int64Value v)) =
          text $ show (fromIntegral v :: Word64) ++ "u64"
        prettyPrim _ v =
          ppr v
        prettyType TypeUnsigned (Prim (IntType Int8))  =
          text "u8"
        prettyType TypeUnsigned (Prim (IntType Int16)) =
          text "u16"
        prettyType TypeUnsigned (Prim (IntType Int32))  =
          text "u32"
        prettyType TypeUnsigned (Prim (IntType Int64)) =
          text "u64"
        prettyType ept (Array et (Rank n) u) =
          ppr u <> mconcat (replicate n $ brackets mempty) <> prettyType ept (Prim et)
        prettyType _ t =
          ppr t
