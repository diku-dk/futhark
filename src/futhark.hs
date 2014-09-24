-- | Futhark Compiler Driver
module Main (main) where

import Control.Monad
import Control.Monad.Writer.Strict (runWriter)
import Control.Monad.Error
import Data.Array
import Data.List
import Data.Version
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import System.IO

import Text.Printf

import Language.Futhark.Core
import Language.Futhark.Parser
import Futhark.Internalise
import Futhark.Externalise
import Futhark.Pipeline
import Futhark.Passes
import Futhark.Analysis.Alias

import qualified Futhark.Representation.External as E
import qualified Futhark.Representation.External.TypeChecker as E
import qualified Futhark.Representation.External.Renamer as E

import qualified Futhark.Representation.Basic as I
import qualified Futhark.TypeCheck as I

import Futhark.Interpreter
import qualified Futhark.SOACFlowGraph as FG
import qualified Futhark.CodeGen.Backends.SequentialC as SequentialC
import qualified Futhark.CodeGen.Backends.Bohrium as Bohrium
import Futhark.Version

newFutharkonfig :: Futharkonfig
newFutharkonfig = Futharkonfig {
                futharkpipeline = []
              , futharkaction = printAction
              , futharkcheckAliases = True
              , futharkverbose = Nothing
              , futharkboundsCheck = True
              }

type FutharkOption = OptDescr (Either (IO ()) (Futharkonfig -> Futharkonfig))

passoption :: String -> Pass -> String -> [String] -> FutharkOption
passoption desc pass short long =
  Option short long
  (NoArg $ Right $ \opts -> opts { futharkpipeline = pass : futharkpipeline opts })
  desc

commandLineOptions :: [FutharkOption]
commandLineOptions =
  [ Option "v" ["version"]
    (NoArg $ Left $ do putStrLn $ "Futhark " ++ showVersion version
                       putStrLn "(C) HIPERFIT research centre"
                       putStrLn "Department of Computer Science, University of Copenhagen (DIKU)"
                       exitSuccess)
    "Print version information and exit."
  , Option "V" ["verbose"]
    (OptArg (\file -> Right $ \opts -> opts { futharkverbose = Just file }) "FILE")
    "Print verbose output on standard error; wrong program to FILE."
  , Option [] ["inhibit-uniqueness-checking"]
    (NoArg $ Right $ \opts -> opts { futharkcheckAliases = False })
    "Don't check that uniqueness constraints are being upheld."
  , Option [] ["compile-sequential"]
    (NoArg $ Right $ \opts -> opts { futharkaction = seqCodegenAction })
    "Translate program into sequential C and write it on standard output."
  , Option [] ["compile-bohrium"]
    (NoArg $ Right $ \opts -> opts { futharkaction = bohriumCodegenAction })
    "Translate program into C using Bohrium and write it on standard output."
  , Option [] ["generate-flow-graph"]
    (NoArg $ Right $ \opts -> opts { futharkaction = flowGraphAction })
    "Print the SOAC flow graph of the final program."
  , Option "p" ["print"]
    (NoArg $ Right $ \opts -> opts { futharkaction = printAction })
    "Prettyprint the resulting internal representation on standard output (default action)."
  , Option "i" ["interpret"]
    (NoArg $ Right $ \opts -> opts { futharkaction = interpretAction })
    "Run the program via an interpreter."
  , Option [] ["externalise"]
    (NoArg $ Right $ \opts -> opts { futharkaction = externaliseAction})
    "Prettyprint the resulting external representation on standard output."
  , Option [] ["no-bounds-checking"]
    (NoArg $ Right $ \opts -> opts { futharkboundsCheck = False })
    "Do not perform bounds checking in the generated program."
  , passoption "Remove debugging annotations from program." uttransform
    "u" ["untrace"]
  , passoption "Transform all second-order array combinators to for-loops." fotransform
    "f" ["first-order-transform"]
  , passoption "Perform simple enabling optimisations." eotransform
    "e" ["enabling-optimisations"]
  , passoption "Perform higher-order optimisation, i.e., fusion." hotransform
    "h" ["higher-order-optimizations"]
  , passoption "Aggressively inline and remove dead functions." inlinetransform
    [] ["inline-functions"]
  , passoption "Remove dead functions." removeDeadFunctions
    [] ["remove-dead-functions"]
  , passoption "Optimise predicates" optimisePredicates
    [] ["optimise-predicates"]
  , passoption "Optimise shape computation" optimiseShapes
    [] ["optimise-shapes"]
  , Option "s" ["standard"]
    (NoArg $ Right $ \opts -> opts { futharkpipeline = standardPipeline ++ futharkpipeline opts })
    "Use the recommended optimised pipeline."
  ]

printAction :: Action
printAction = ("prettyprinter", putStrLn . I.prettyPrint . aliasAnalysis)

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
  [ uttransform
  , eotransform
  , inlinetransform
  , eotransform
  , hotransform
  , eotransform
  , removeDeadFunctions
  ]

-- | Entry point.  Non-interactive, except when reading interpreter
-- input from standard input.
main :: IO ()
main = do args <- getArgs
          case getOpt' RequireOrder commandLineOptions args of
            (opts, nonopts, [], []) ->
              case applyOpts opts of
                Right conf | [file] <- nonopts -> compiler conf file
                           | otherwise         -> invalid nonopts [] []
                Left m     -> m
            (_, nonopts, unrecs, errs) -> invalid nonopts unrecs errs

  where applyOpts :: [Either (IO ()) (Futharkonfig -> Futharkonfig)]
                  -> Either (IO ()) Futharkonfig
        applyOpts opts = do fs <- sequence opts
                            return $ foldl (.) id fs newFutharkonfig

        invalid nonopts unrecs errs = do usage <- usageStr commandLineOptions
                                         badOptions usage nonopts errs unrecs


usageStr :: [OptDescr a] -> IO String
usageStr opts = do
  prog <- getProgName
  let header = "Help for " ++ prog ++ " (Futhark " ++ showVersion version ++ ")"
  return $ usageInfo header opts

badOptions :: String -> [String] -> [String] -> [String] -> IO ()
badOptions usage nonopts errs unrecs = do
  mapM_ (errput . ("Junk argument: " ++)) nonopts
  mapM_ (errput . ("Unrecognised argument: " ++)) unrecs
  hPutStr stderr $ concat errs ++ usage
  exitWith $ ExitFailure 1

-- | Short-hand for 'liftIO . hPutStrLn stderr'
errput :: MonadIO m => String -> m ()
errput = liftIO . hPutStrLn stderr

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
            I.prettyPrint (aliasAnalysis prog) ++ "\n"
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
  | otherwise                  = checkProgNoUniqueness

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
          int_prog_checked <- canFail "After internalisation:\n" (Just int_prog)
                              (typeCheck I.checkProg I.checkProgNoUniqueness config
                              int_prog >> return int_prog)
          runPasses config int_prog_checked
