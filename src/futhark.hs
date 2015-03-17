-- | Futhark Compiler Driver
module Main (main) where

import Control.Monad
import Control.Monad.Except
import Data.Version
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import System.IO

import Language.Futhark.Parser
import Futhark.Internalise
import Futhark.Pipeline
import Futhark.Passes
import Futhark.Actions

import qualified Futhark.Representation.External as E
import qualified Futhark.Representation.External.TypeChecker as E
import qualified Futhark.Representation.External.Renamer as E

import qualified Futhark.Representation.Basic as I
import qualified Futhark.TypeCheck as I

import Futhark.Version

newFutharkConfig :: FutharkConfig
newFutharkConfig = FutharkConfig { futharkpipeline = []
                                 , futharkaction = printAction
                                 , futharkcheckAliases = True
                                 , futharkverbose = Nothing
                                 , futharkboundsCheck = True
                                 }

type FutharkOption = OptDescr (Either (IO ()) (FutharkConfig -> FutharkConfig))

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
  , Option [] ["generate-flow-graph"]
    (NoArg $ Right $ \opts -> opts { futharkaction = flowGraphAction })
    "Print the SOAC flow graph of the final program."
  , Option [] ["compile-imperative"]
    (NoArg $ Right $ \opts -> opts { futharkaction = impCodeGenAction })
    "Translate program into the imperative IL and write it on standard output."
  , Option "p" ["print"]
    (NoArg $ Right $ \opts -> opts { futharkaction = printAction })
    "Prettyprint the resulting internal representation on standard output (default action)."
  , Option "i" ["interpret"]
    (NoArg $ Right $ \opts -> opts { futharkaction = interpretAction' })
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
  , passoption "Transform program to explicit memory representation" explicitMemory
    "a" ["explicit-allocations"]
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
  , passoption "Lower in-place updates" inPlaceLowering
    [] ["in-place-lowering"]
  , passoption "Common subexpression elimination" commonSubexpressionElimination
    [] ["cse"]
  , passoption "Flattening" flattening
    [] ["flattening"]
  , Option "s" ["standard"]
    (NoArg $ Right $ \opts -> opts { futharkpipeline = standardPipeline ++ futharkpipeline opts })
    "Use the recommended optimised pipeline."
  ]

interpretAction' :: Action
interpretAction' = interpretAction parseValues'
  where parseValues' :: FilePath -> String -> Either ParseError [I.Value]
        parseValues' path s =
          liftM (concatMap internaliseValue) $ parseValues path s

standardPipeline :: [Pass]
standardPipeline =
  [ uttransform
  , eotransform
  , inlinetransform
  , commonSubexpressionElimination
  , eotransform
  , hotransform
  , commonSubexpressionElimination
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

  where applyOpts :: [Either (IO ()) (FutharkConfig -> FutharkConfig)]
                  -> Either (IO ()) FutharkConfig
        applyOpts opts = do fs <- sequence opts
                            return $ foldl (.) id fs newFutharkConfig

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

compiler :: FutharkConfig -> FilePath -> IO ()
compiler config file = do
  contents <- readFile file
  (msgs, res) <- futharkc config file contents
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

typeCheck :: (prog -> Either err prog')
          -> (prog -> Either err prog')
          -> FutharkConfig
          -> prog -> Either err prog'
typeCheck checkProg checkProgNoUniqueness config
  | futharkcheckAliases config = checkProg
  | otherwise                  = checkProgNoUniqueness

futharkc :: FutharkConfig -> FilePath -> String
         -> IO (String, Either CompileError PipelineState)
futharkc config filename srccode = do
  res <- runFutharkM futharkc'
  case res of (Left err, msgs)  -> return (msgs, Left err)
              (Right prog, msgs) -> return (msgs, Right prog)
  where futharkc' = do
          parsed_prog <- parseSourceProgram filename srccode
          ext_prog    <- typeCheckSourceProgram config parsed_prog
          let int_prog = internaliseProg (futharkboundsCheck config) $ E.tagProg ext_prog
          typeCheckInternalProgram config int_prog
          runPasses config $ Basic int_prog

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
