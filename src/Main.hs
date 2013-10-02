-- | L0 Compiler Driver
module Main (main) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer.Strict (Writer, runWriter, tell)
import Control.Monad.Error
import Data.Maybe (isJust)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))
import System.IO

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
import qualified L0C.EnablingOpts.Hoisting as LHO
import L0C.Untrace
import L0C.CCodeGen

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

data L0Config = L0Config {
    l0pipeline :: [Pass]
  , l0action :: Prog -> IO ()
  , l0checkAliases :: Bool
  , l0verbose :: Maybe (Maybe FilePath)
}

newL0Config :: L0Config
newL0Config = L0Config {
                l0pipeline = []
              , l0action = putStrLn . prettyPrint
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
  , Option "c" ["compile"]
    (NoArg $ \opts -> opts { l0action = putStrLn . compileProg })
    "Translate program into C and write it on standard output."
  , Option "p" ["print"]
    (NoArg $ \opts -> opts { l0action = putStrLn . prettyPrint })
    "Prettyprint the program on standard output (default action)."
  , Option "i" ["interpret"]
    (NoArg $ \opts -> opts { l0action = interpret })
    "Run the program via an interpreter."
  , rename "r" ["rename"]
  , hoist "o" ["hoist"]
  , normalize "n" ["normalize"]
  , uttransform "u" ["untrace"]
  , fotransform "f" ["first-order-transform"]
  , tatransform "t" ["tuple-of-arrays-transform"]
  , eotransform "e" ["enabling-optimisations"]
  , hotransform "h" ["higher-order-optimizations"]
  ]

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
        Right val  -> putStrLn $ "Result of evaluation: " ++ ppValue val

passoption :: String -> Pass -> String -> [String] -> L0Option
passoption desc pass short long =
  Option short long
  (NoArg $ \opts -> opts { l0pipeline = pass : l0pipeline opts })
  desc

rename :: String -> [String] -> L0Option
rename =
  passoption "Rename all non-function identifiers to be unique."
  Pass { passName = "renamer"
       , passOp = return . renameProg
       }

hoist :: String -> [String] -> L0Option
hoist =
  passoption "Let-hoisting"
  Pass { passName = "let-hoister"
       , passOp = return . LHO.transformProg
       }

normalize :: String -> [String] -> L0Option
normalize =
  passoption "Full normalization"
  Pass { passName = "full normalizer"
       , passOp = return . FN.normalizeProg
       }

fotransform :: String -> [String] -> L0Option
fotransform =
  passoption "Transform all second-order array combinators to for-loops."
  Pass { passName = "first-order transform"
       , passOp = return . FOT.transformProg
       }

tatransform :: String -> [String] -> L0Option
tatransform =
  passoption "Transform most uses of tuples away"
  Pass { passName = "tuple-transform"
       , passOp = return . TT.transformProg
       }

uttransform :: String -> [String] -> L0Option
uttransform =
  passoption "Remove debugging annotations from program."
  Pass { passName = "debugging annotation removal"
       , passOp = return . untraceProg
       }


eotransform :: String -> [String] -> L0Option
eotransform =
  passoption "Perform simple enabling optimisations."
  Pass { passName = "enabling optimations"
       , passOp = liftPass enablingOpts
       }

hotransform :: String -> [String] -> L0Option
hotransform =
  passoption "Perform higher-order optimisation, i.e., fusion."
  Pass { passName = "higher-order optimisations"
       , passOp = liftPass highOrdTransf
       }

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
          maybe (hPutStrLn stderr) writeFile outfile $
            prettyPrint prog
        _ -> return ()
      exitWith $ ExitFailure 2
    Right prog -> l0action config prog

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
