-- | L0 Compiler Driver
module Main (main) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer.Strict (Writer, runWriter, tell)
import Control.Monad.Error
import Data.Array
import Data.Maybe (isJust)
import Data.List
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))
import System.IO

import Text.Printf

import Language.L0.Parser
import L0C.L0
import L0C.TypeChecker
import L0C.Renamer
import L0C.Interpreter
import L0C.EnablingOpts.EnablingOptDriver
import L0C.HOTrans.HOTransDriver
import qualified L0C.FirstOrderTransform as FOT
import qualified L0C.TupleTransform as TT
import qualified L0C.FullNormalization as FN
import qualified L0C.Rebinder as RB
import qualified L0C.IndexInliner as II
import L0C.Untrace
import qualified L0C.Backends.SequentialC as SequentialC
import qualified L0C.Backends.Bohrium as Bohrium

data CompileError = CompileError {
    errorDesc :: String
  , errorProg :: Maybe Prog
  }

instance Error CompileError where
  strMsg s = CompileError s Nothing

type L0CM = ErrorT CompileError (Writer String)

data Pass = Pass {
    passName :: String
  , passOp :: Prog -> L0CM Prog
  }

type Action = (String, Prog -> IO ())

data L0Config = L0Config {
    l0pipeline :: [Pass]
  , l0action :: Action
  , l0checkAliases :: Bool
  , l0verbose :: Maybe (Maybe FilePath)
}

newL0Config :: L0Config
newL0Config = L0Config {
                l0pipeline = []
              , l0action = printAction
              , l0checkAliases = True
              , l0verbose = Nothing
              }

verbose :: L0Config -> Bool
verbose = isJust . l0verbose

compileError :: String -> Maybe Prog -> L0CM a
compileError s p = throwError $ CompileError s p

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
  , Option [] ["compile-bohrium"]
    (NoArg $ \opts -> opts { l0action = bohriumCodegenAction })
    "Translate program into C using Bohrium and write it on standard output."
  , Option "p" ["print"]
    (NoArg $ \opts -> opts { l0action = printAction })
    "Prettyprint the program on standard output (default action)."
  , Option "i" ["interpret"]
    (NoArg $ \opts -> opts { l0action = interpretAction })
    "Run the program via an interpreter."
  , renameOpt "r" ["rename"]
  , hoistOpt "o" ["hoist"]
  , hoistAggrOpt "O" ["hoist-aggressively"]
  , normalizeOpt "n" ["normalize"]
  , uttransformOpt "u" ["untrace"]
  , fotransformOpt "f" ["first-order-transform"]
  , tatransformOpt "t" ["tuple-of-arrays-transform"]
  , eotransformOpt "e" ["enabling-optimisations"]
  , iitransformOpt []  ["inline-map-indexes"]
  , hotransformOpt "h" ["higher-order-optimizations"]
  , Option "s" ["standard"]
    (NoArg $ \opts -> opts { l0pipeline = standardPipeline ++ l0pipeline opts })
    "Use the recommended optimised pipeline."
  ]

printAction :: Action
printAction = ("prettyprinter", putStrLn . prettyPrint)

interpretAction :: Action
interpretAction = ("interpreter", interpret)

seqCodegenAction :: Action
seqCodegenAction = ("sequential code generator", putStrLn . SequentialC.compileProg)

bohriumCodegenAction :: Action
bohriumCodegenAction = ("Bohrium code generator", putStrLn . Bohrium.compileProg)

interpret :: Prog -> IO ()
interpret prog =
  case funDecByName defaultEntryPoint prog of
    Nothing -> do hPutStrLn stderr "Interpreter error: no main function."
                  exitWith $ ExitFailure 2
    Just (_,_,fparams,_,_) -> do
      args <- forM fparams $ \_ -> do
                line <- getLine
                case parseValue "<stdin>" line of
                  Left e -> do hPutStrLn stderr $ "Read error: " ++ show e
                               exitWith $ ExitFailure 2
                  Right v -> return v
      let (res, trace) = runFun defaultEntryPoint args prog
      forM_ trace $ \(loc, what) ->
        hPutStrLn stderr $ locStr loc ++ ": " ++ what
      case res of
        Left err -> do hPutStrLn stderr $ "Interpreter error:\n" ++ show err
                       exitWith $ ExitFailure 2
        Right val  -> putStrLn $ ppOutput val
  where ppOutput val | Just s <- arrayString val = s
        ppOutput (RealVal x) = printf "%.6f" x
        ppOutput (IntVal x)  = show x
        ppOutput (CharVal c) = show c
        ppOutput (LogVal b)  = show b
        ppOutput Checked = "Checked"
        ppOutput (TupVal vs) = "{" ++ intercalate ", " (map ppOutput vs) ++ "}"
        ppOutput (ArrayVal a t)
          | [] <- elems a = "empty(" ++ ppType t ++ ")"
          | otherwise     = "[" ++ intercalate ", " (map ppOutput $ elems a) ++ "]"

rename :: Pass
rename = Pass { passName = "renamer"
              , passOp = return . renameProg
              }

hoist :: Pass
hoist = Pass { passName = "rebinder"
             , passOp = return . RB.transformProg
             }

hoistAggr :: Pass
hoistAggr = Pass { passName = "rebinder (aggressive)"
                 , passOp = return . RB.transformProgAggr
                 }

normalize :: Pass
normalize = Pass { passName = "full normalizer"
                 , passOp = return . FN.normalizeProg
                 }

fotransform :: Pass
fotransform = Pass { passName = "first-order transform"
                   , passOp = return . FOT.transformProg
                   }

tatransform :: Pass
tatransform = Pass { passName = "tuple-transform"
                   , passOp = return . TT.transformProg
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
  [ uttransform, tatransform, eotransform, iitransform, rename,
    normalize, hoist, eotransform, hotransform, eotransform, normalize,
    hoistAggr, eotransform ]

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

normalizeOpt :: String -> [String] -> L0Option
normalizeOpt =
  passoption "Full normalization" normalize

fotransformOpt :: String -> [String] -> L0Option
fotransformOpt =
  passoption "Transform all second-order array combinators to for-loops."
  fotransform

tatransformOpt :: String -> [String] -> L0Option
tatransformOpt =
  passoption "Transform most uses of tuples away" tatransform

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
            prettyPrint prog ++ "\n"
        _ -> return ()
      exitWith $ ExitFailure 2
    Right prog -> do
      let (actiondesc, action) = l0action config
      when (verbose config) $
        hPutStrLn stderr $ "Running " ++ actiondesc ++ "."
      action prog

typeCheck :: (TypeBox ty, VarName vn) =>
             L0Config -> ProgBase ty vn
          -> Either (TypeError vn) (ProgBase CompTypeBase vn)
typeCheck config
  | l0checkAliases config = checkProg
  | otherwise             = checkProgNoUniqueness

l0c :: L0Config -> FilePath -> String -> (String, Either CompileError Prog)
l0c config filename srccode =
  case runWriter (runErrorT l0c') of
    (Left err, msgs) -> (msgs, Left err)
    (Right prog, msgs) -> (msgs, Right prog)
  where l0c' = canFail Nothing (parseL0 filename srccode) >>=
               canFail Nothing . typeCheck config >>=
               pipeline . tagProg
        pipeline = foldl comb return $ l0pipeline config
        comb prev pass prog = do
          prog' <- prev prog
          when (verbose config) $ tell $ "Running " ++ passName pass ++ ".\n"
          res <- lift $ runErrorT $ passOp pass prog'
          case res of
            Left err ->
              compileError ("Error during pass '" ++ passName pass ++ "':\n" ++ errorDesc err)
                           (Just prog')
            Right prog'' ->
              case typeCheck config prog'' of
                Left err ->
                  compileError ("Type error after pass '" ++ passName pass ++
                                "':\n" ++ show err)
                               (Just prog'')
                Right prog''' -> return prog'''

canFail :: Show err => Maybe Prog -> Either err a -> L0CM a
canFail p (Left err) = compileError (show err) p
canFail _ (Right v)  = return v

liftPass :: Show err => (Prog -> Either err a) -> Prog -> L0CM a
liftPass f p = canFail (Just p) (f p)
