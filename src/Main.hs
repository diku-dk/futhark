-- | L0 Compiler Driver
module Main (main) where

import Control.Monad
import Control.Monad.Writer.Strict (runWriter)
import Control.Monad.Error
import Data.Array
import Data.List
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))
import System.IO

import Text.Printf

import Language.L0.Core
import Language.L0.Parser
import L0C.Internalise
import L0C.Pipeline

import qualified L0C.ExternalRep.TypeChecker as E
import qualified L0C.ExternalRep.Renamer as E

import qualified L0C.InternalRep as I
import qualified L0C.InternalRep.TypeChecker as I
import qualified L0C.InternalRep.Renamer as I

import L0C.Interpreter
import L0C.EnablingOpts.EnablingOptDriver
import L0C.HOTrans.HOTransDriver
import qualified L0C.FirstOrderTransform as FOT
import qualified L0C.Rebinder as RB
import qualified L0C.IndexInliner as II
import qualified L0C.SOACFlowGraph as FG
import L0C.Untrace
import qualified L0C.Backends.SequentialC as SequentialC
-- import qualified L0C.Backends.Bohrium as Bohrium

newL0Config :: L0Config
newL0Config = L0Config {
                l0pipeline = []
              , l0action = printAction
              , l0checkAliases = True
              , l0verbose = Nothing
              }

type L0Option = OptDescr (L0Config -> L0Config)

commandLineOptions :: [L0Option]
commandLineOptions =
  [ Option "V" ["verbose"]
    (OptArg (\file opts -> opts { l0verbose = Just file }) "FILE")
    "Print verbose output on standard error; wrong program to FILE."
  , Option [] ["inhibit-uniqueness-checking"]
    (NoArg $ \opts -> opts { l0checkAliases = False })
    "Don't check that uniqueness constraints are being upheld."
  , Option [] ["compile-sequential"]
    (NoArg $ \opts -> opts { l0action = seqCodegenAction })
    "Translate program into sequential C and write it on standard output."
{-
  , Option [] ["compile-bohrium"]
    (NoArg $ \opts -> opts { l0action = bohriumCodegenAction })
    "Translate program into C using Bohrium and write it on standard output."
-}
  , Option [] ["generate-flow-graph"]
    (NoArg $ \opts -> opts { l0action = flowGraphAction })
    "Print the SOAC flow graph of the final program."
  , Option "p" ["print"]
    (NoArg $ \opts -> opts { l0action = printAction })
    "Prettyprint the program on standard output (default action)."
  , Option "i" ["interpret"]
    (NoArg $ \opts -> opts { l0action = interpretAction })
    "Run the program via an interpreter."
  , renameOpt "r" ["rename"]
  , hoistOpt "o" ["hoist"]
  , hoistAggrOpt "O" ["hoist-aggressively"]
  , uttransformOpt "u" ["untrace"]
  , fotransformOpt "f" ["first-order-transform"]
  , eotransformOpt "e" ["enabling-optimisations"]
  , iitransformOpt []  ["inline-map-indexes"]
  , hotransformOpt "h" ["higher-order-optimizations"]
  , Option "s" ["standard"]
    (NoArg $ \opts -> opts { l0pipeline = standardPipeline ++ l0pipeline opts })
    "Use the recommended optimised pipeline."
  ]

printAction :: Action
printAction = ("prettyprinter", putStrLn . I.prettyPrint)

interpretAction :: Action
interpretAction = ("interpreter", interpret)

seqCodegenAction :: Action
seqCodegenAction = ("sequential code generator", putStrLn . SequentialC.compileProg)
{-
bohriumCodegenAction :: Action
bohriumCodegenAction = ("Bohrium code generator", putStrLn . Bohrium.compileProg)
-}

flowGraphAction :: Action
flowGraphAction = ("SOAC flow graph", putStrLn . FG.makeFlowGraphString)

interpret :: I.Prog -> IO ()
interpret prog =
  case I.funDecByName I.defaultEntryPoint prog of
    Nothing -> do hPutStrLn stderr "Interpreter error: no main function."
                  exitWith $ ExitFailure 2
    Just (_,_,fparams,_,_) -> do
      args <- forM fparams $ \_ -> do
                line <- getLine
                case parseValue "<stdin>" line of
                  Left e -> do hPutStrLn stderr $ "Read error: " ++ show e
                               exitWith $ ExitFailure 2
                  Right v -> return v
      let (res, trace) = runFun I.defaultEntryPoint (concatMap internaliseValue args) prog
      forM_ trace $ \(loc, what) ->
        hPutStrLn stderr $ locStr loc ++ ": " ++ what
      case res of
        Left err -> do hPutStrLn stderr $ "Interpreter error:\n" ++ show err
                       exitWith $ ExitFailure 2
        Right val  -> putStrLn $ ppOutput val
  where ppOutput [v] = ppOutput' v
        ppOutput vs = "{" ++ intercalate ", " (map ppOutput' vs) ++ "}"
        ppOutput' val | Just s <- I.arrayString val = s
        ppOutput' (I.BasicVal (I.RealVal x)) = printf "%.6f" x
        ppOutput' (I.BasicVal (I.IntVal x))  = show x
        ppOutput' (I.BasicVal (I.CharVal c)) = show c
        ppOutput' (I.BasicVal (I.LogVal b))  = show b
        ppOutput' (I.BasicVal I.Checked) = "Checked"
        ppOutput' (I.ArrayVal a t)
          | [] <- elems a = "empty(" ++ I.ppType t ++ ")"
          | otherwise     = "[" ++ intercalate ", " (map ppOutput' $ elems a) ++ "]"

rename :: Pass
rename = Pass { passName = "renamer"
              , passOp = return . I.renameProg
              }

hoist :: Pass
hoist = Pass { passName = "rebinder"
             , passOp = return . RB.transformProg
             }

hoistAggr :: Pass
hoistAggr = Pass { passName = "rebinder (aggressive)"
                 , passOp = return . RB.transformProgAggr
                 }

fotransform :: Pass
fotransform = Pass { passName = "first-order transform"
                   , passOp = return . FOT.transformProg
                   }

uttransform :: Pass
uttransform = Pass { passName = "debugging annotation removal"
                   , passOp = return . untraceProg
                   }

eotransform :: Pass
eotransform = Pass { passName = "enabling optimations"
                   , passOp = liftPass enablingOpts
                   }

iitransform :: Pass
iitransform = Pass { passName = "inlining map indexing"
                   , passOp = return . II.transformProg
                   }

hotransform :: Pass
hotransform = Pass { passName = "higher-order optimisations"
                   , passOp = liftPass highOrdTransf
                   }

standardPipeline :: [Pass]
standardPipeline =
  [ uttransform, eotransform, iitransform, rename, hoist
  , eotransform, hotransform, eotransform , hoistAggr
  , eotransform ]

passoption :: String -> Pass -> String -> [String] -> L0Option
passoption desc pass short long =
  Option short long
  (NoArg $ \opts -> opts { l0pipeline = pass : l0pipeline opts })
  desc

renameOpt :: String -> [String] -> L0Option
renameOpt =
  passoption "Rename all non-function identifiers to be unique." rename

hoistOpt :: String -> [String] -> L0Option
hoistOpt =
  passoption "Rebinder - hoisting, CSE, dependency graph compression." hoist

hoistAggrOpt :: String -> [String] -> L0Option
hoistAggrOpt =
  passoption "Rebinder - hoisting, CSE, dependency graph compression (aggressively)."
  hoistAggr

fotransformOpt :: String -> [String] -> L0Option
fotransformOpt =
  passoption "Transform all second-order array combinators to for-loops."
  fotransform

uttransformOpt :: String -> [String] -> L0Option
uttransformOpt =
  passoption "Remove debugging annotations from program." uttransform

eotransformOpt :: String -> [String] -> L0Option
eotransformOpt =
  passoption "Perform simple enabling optimisations."
  eotransform

iitransformOpt :: String -> [String] -> L0Option
iitransformOpt =
  passoption "Inline indexing into maps."
  iitransform

hotransformOpt :: String -> [String] -> L0Option
hotransformOpt =
  passoption "Perform higher-order optimisation, i.e., fusion."
  hotransform

-- | Entry point.  Non-interactive, except when reading interpreter
-- input from standard input.
main :: IO ()
main = do args <- getArgs
          case getOpt RequireOrder commandLineOptions args of
            (opts, [file], []) -> compiler (foldl (.) id opts newL0Config) file
            (_, _, errs)       -> usage errs

usage :: [String] -> IO ()
usage errs = do
  prog <- getProgName
  mapM_ (hPutStr stderr) errs
  hPutStr stderr "\n"
  hPutStr stderr $ usageInfo (prog ++ " [options] <file>") commandLineOptions
  exitWith $ ExitFailure 1

compiler :: L0Config -> FilePath -> IO ()
compiler config file = do
  contents <- readFile file
  let (msgs, res) = l0c config file contents
  hPutStr stderr msgs
  case res of
    Left err -> do
      hPutStrLn stderr $ errorDesc err
      case (errorProg err, l0verbose config) of
        (Just prog, Just outfile) ->
          maybe (hPutStr stderr) writeFile outfile $
            I.prettyPrint prog ++ "\n"
        _ -> return ()
      exitWith $ ExitFailure 2
    Right prog -> do
      let (actiondesc, action) = l0action config
      when (verbose config) $
        hPutStrLn stderr $ "Running " ++ actiondesc ++ "."
      action prog

typeCheck :: (prog -> Either err prog')
          -> (prog -> Either err prog')
          -> L0Config
          -> prog -> Either err prog'
typeCheck checkProg checkProgNoUniqueness config
  | l0checkAliases config = checkProg
  | otherwise             = checkProgNoUniqueness

l0c :: L0Config -> FilePath -> String -> (String, Either CompileError I.Prog)
l0c config filename srccode =
  case runWriter (runErrorT l0c') of
    (Left err, msgs) -> (msgs, Left err)
    (Right prog, msgs) -> (msgs, Right prog)
  where l0c' = do
          parsed_prog <- canFail "" Nothing $ parseL0 filename srccode
          ext_prog    <- canFail "" Nothing $
                         typeCheck E.checkProg E.checkProgNoUniqueness config
                         parsed_prog
          let int_prog = internaliseProg $ E.tagProg ext_prog
          int_prog_checked <- canFail "After internalisation:\n" (Just int_prog) $
                              typeCheck I.checkProg I.checkProgNoUniqueness config
                              int_prog
          runPasses config int_prog_checked

canFail :: Show err => String -> Maybe I.Prog -> Either err a -> L0CM a
canFail d p (Left err) = compileError (d ++ show err) p
canFail _ _ (Right v)  = return v

liftPass :: Show err => (I.Prog -> Either err a) -> I.Prog -> L0CM a
liftPass f p = canFail "" (Just p) (f p)

{-
module Main where

import Control.Applicative
import L0C.ExternalRep as E
import Language.L0.Parser
import qualified L0C.Internalise

import qualified L0C.ExternalRep as E
import qualified L0C.ExternalRep.TypeChecker as E
import qualified L0C.ExternalRep.Renamer as E

import qualified L0C.InternalRep as I
import qualified L0C.InternalRep.TypeChecker as I

import System.Environment

test :: FilePath -> IO ()
test file = do Right exprog <- parseL0 file <$> readFile file
               case E.checkProg $ E.tagProg exprog of
                 Left err -> error $ show err
                 Right exprog' -> do
                   let inprog = L0C.Internalise.transformProg exprog'
                   putStrLn $ I.prettyPrint inprog
                   case I.checkProg inprog of
                     Left err -> error $ show err
                     Right _  -> putStrLn "It's all good"

main :: IO ()
main = do [f] <- getArgs
          test f
-}
