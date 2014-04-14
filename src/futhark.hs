-- | Futhark Compiler Driver
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

import Language.Futhark.Core
import Language.Futhark.Parser
import Futhark.Internalise
import Futhark.Externalise
import Futhark.Pipeline
import Futhark.Passes

import qualified Futhark.ExternalRep as E
import qualified Futhark.ExternalRep.TypeChecker as E
import qualified Futhark.ExternalRep.Renamer as E

import qualified Futhark.InternalRep as I
import qualified Futhark.InternalRep.TypeChecker as I

import Futhark.Interpreter
import qualified Futhark.SOACFlowGraph as FG
import qualified Futhark.Backends.SequentialC as SequentialC
import qualified Futhark.Backends.Bohrium as Bohrium

newFutharkonfig :: Futharkonfig
newFutharkonfig = Futharkonfig {
                futharkpipeline = []
              , futharkaction = printAction
              , futharkcheckAliases = True
              , futharkverbose = Nothing
              , futharkboundsCheck = True
              }

type FutharkOption = OptDescr (Futharkonfig -> Futharkonfig)

passoption :: String -> Pass -> String -> [String] -> FutharkOption
passoption desc pass short long =
  Option short long
  (NoArg $ \opts -> opts { futharkpipeline = pass : futharkpipeline opts })
  desc

commandLineOptions :: [FutharkOption]
commandLineOptions =
  [ Option "V" ["verbose"]
    (OptArg (\file opts -> opts { futharkverbose = Just file }) "FILE")
    "Print verbose output on standard error; wrong program to FILE."
  , Option [] ["inhibit-uniqueness-checking"]
    (NoArg $ \opts -> opts { futharkcheckAliases = False })
    "Don't check that uniqueness constraints are being upheld."
  , Option [] ["compile-sequential"]
    (NoArg $ \opts -> opts { futharkaction = seqCodegenAction })
    "Translate program into sequential C and write it on standard output."
  , Option [] ["compile-bohrium"]
    (NoArg $ \opts -> opts { futharkaction = bohriumCodegenAction })
    "Translate program into C using Bohrium and write it on standard output."
  , Option [] ["generate-flow-graph"]
    (NoArg $ \opts -> opts { futharkaction = flowGraphAction })
    "Print the SOAC flow graph of the final program."
  , Option "p" ["print"]
    (NoArg $ \opts -> opts { futharkaction = printAction })
    "Prettyprint the resulting internal representation on standard output (default action)."
  , Option "i" ["interpret"]
    (NoArg $ \opts -> opts { futharkaction = interpretAction })
    "Run the program via an interpreter."
  , Option [] ["externalise"]
    (NoArg $ \opts -> opts { futharkaction = externaliseAction})
    "Prettyprint the resulting external representation on standard output."
  , Option [] ["no-bounds-checking"]
    (NoArg $ \opts -> opts { futharkboundsCheck = False })
    "Do not perform bounds checking in the generated program."
  , passoption "Remove debugging annotations from program." uttransform
    "u" ["untrace"]
  , passoption "Transform all second-order array combinators to for-loops." fotransform
    "f" ["first-order-transform"]
  , passoption "Perform simple enabling optimisations." eotransform
                 "e" ["enabling-optimisations"]
  , passoption "Inline indexing into maps." iitransform
    []  ["inline-map-indexes"]
  , passoption "Perform higher-order optimisation, i.e., fusion." hotransform
    "h" ["higher-order-optimizations"]
  , passoption "Aggressively inline and remove dead functions." inlinetransform
    [] ["inline-functions"]
  , passoption "Split certificates from main computation" splitasserttransform [] ["split-assertions"]
  , Option "s" ["standard"]
    (NoArg $ \opts -> opts { futharkpipeline = standardPipeline ++ futharkpipeline opts })
    "Use the recommended optimised pipeline."
  ]

printAction :: Action
printAction = ("prettyprinter", putStrLn . I.prettyPrint)

externaliseAction :: Action
externaliseAction = ("externalise", putStrLn . E.prettyPrint . externaliseProg)

interpretAction :: Action
interpretAction = ("interpreter", interpret)

seqCodegenAction :: Action
seqCodegenAction = ("sequential code generator", putStrLn . SequentialC.compileProg)

bohriumCodegenAction :: Action
bohriumCodegenAction = ("Bohrium code generator", putStrLn . Bohrium.compileProg)

flowGraphAction :: Action
flowGraphAction = ("SOAC flow graph", putStrLn . FG.makeFlowGraphString)

interpret :: I.Prog -> IO ()
interpret prog =
  case I.funDecByName I.defaultEntryPoint prog of
    Nothing -> do hPutStrLn stderr "Interpreter error: no main function."
                  exitWith $ ExitFailure 2
    Just _ -> do
      parseres <- liftM (parseValues "<stdin>") getContents
      args <- case parseres of Left e -> do hPutStrLn stderr $ "Read error: " ++ show e
                                            exitWith $ ExitFailure 2
                               Right vs -> return vs
      let (res, trace) = runFun I.defaultEntryPoint (internaliseParamValues args) prog
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

standardPipeline :: [Pass]
standardPipeline =
  [ uttransform, inlinetransform
  , eotransform, iitransform
  , hotransform, eotransform
  ]

-- | Entry point.  Non-interactive, except when reading interpreter
-- input from standard input.
main :: IO ()
main = do args <- getArgs
          case getOpt RequireOrder commandLineOptions args of
            (opts, [file], []) -> compiler (foldl (.) id opts newFutharkonfig) file
            (_, _, errs)       -> usage errs

usage :: [String] -> IO ()
usage errs = do
  prog <- getProgName
  mapM_ (hPutStr stderr) errs
  hPutStr stderr "\n"
  hPutStr stderr $ usageInfo (prog ++ " [options] <file>") commandLineOptions
  exitWith $ ExitFailure 1

compiler :: Futharkonfig -> FilePath -> IO ()
compiler config file = do
  contents <- readFile file
  let (msgs, res) = futharkc config file contents
  hPutStr stderr msgs
  case res of
    Left err -> do
      hPutStrLn stderr $ errorDesc err
      case (errorProg err, futharkverbose config) of
        (Just prog, Just outfile) ->
          maybe (hPutStr stderr) writeFile outfile $
            I.prettyPrint prog ++ "\n"
        _ -> return ()
      exitWith $ ExitFailure 2
    Right prog -> do
      let (actiondesc, action) = futharkaction config
      when (verbose config) $
        hPutStrLn stderr $ "Running " ++ actiondesc ++ "."
      action prog

typeCheck :: (prog -> Either err prog')
          -> (prog -> Either err prog')
          -> Futharkonfig
          -> prog -> Either err prog'
typeCheck checkProg checkProgNoUniqueness config
  | futharkcheckAliases config = checkProg
  | otherwise             = checkProgNoUniqueness

futharkc :: Futharkonfig -> FilePath -> String -> (String, Either CompileError I.Prog)
futharkc config filename srccode =
  case runWriter (runErrorT futharkc') of
    (Left err, msgs) -> (msgs, Left err)
    (Right prog, msgs) -> (msgs, Right prog)
  where futharkc' = do
          parsed_prog <- canFail "" Nothing $ parseFuthark filename srccode
          ext_prog    <- canFail "" Nothing $
                         typeCheck E.checkProg E.checkProgNoUniqueness config
                         parsed_prog
          let int_prog = internaliseProg (futharkboundsCheck config) $ E.tagProg ext_prog
          int_prog_checked <- canFail "After internalisation:\n" (Just int_prog) $
                              typeCheck I.checkProg I.checkProgNoUniqueness config
                              int_prog
          runPasses config int_prog_checked
