-- | L0 Compiler Driver
module Main (main) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer.Strict (Writer, runWriter, tell)
import Control.Monad.Error
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))
import System.IO

import L0.AbSyn
import L0.Parser (parseL0)
import L0.TypeChecker
import L0.Renamer
import L0.Interpreter
import L0.EnablingOpts.EnablingOptDriver
import qualified L0.FirstOrderTransform as FOT
import qualified L0.TupleArrayTransform as TAT
import L0.CCodeGen

type L0CM = ErrorT String (Writer String)

data Pass = Pass {
    passName :: String
  , passOp :: Prog Type -> L0CM (Prog Type)
  }

data L0Config = L0Config {
    l0pipeline :: [Pass]
  , l0action :: Prog Type -> IO ()
  , l0verbose :: Bool
}

newL0Config :: L0Config
newL0Config = L0Config {
                l0pipeline = []
              , l0action = putStrLn . prettyPrint
              , l0verbose = False
              }

type L0Option = OptDescr (L0Config -> L0Config)

commandLineOptions :: [L0Option]
commandLineOptions =
  [ Option "V" ["verbose"]
    (NoArg $ \opts -> opts { l0verbose = True })
    "Display verbose output on standard error."
  , Option "c" ["compile"]
    (NoArg $ \opts -> opts { l0action = putStrLn . compileProg })
    "Translate program into C and write it on standard output."
  , Option "p" ["print"]
    (NoArg $ \opts -> opts { l0action = putStrLn . prettyPrint })
    "Prettyprint the program on standard output (default action)."
  , Option "i" ["interpret"]
    (NoArg $ \opts -> opts { l0action = interpret })
    "Run the program via an interpreter."
  , tracepass [] ["trace"]
  , rename "r" ["rename"]
  , fotransform "f" ["first-order-transform"]
  , tatransform "t" ["tuple-of-arrays-transform"]
  , eotransform "e" ["enabling-optimisations"]
  ]

interpret :: Prog Type -> IO ()
interpret prog = do
  res <- runProgIO prog
  case res of
    Left err -> do hPutStrLn stderr $ "Interpreter error:\n" ++ show err
                   exitWith $ ExitFailure 2
    Right _  -> return ()

tracepass :: String -> [String] -> L0Option
tracepass short long =
  Option short long
         (OptArg (\arg opts ->
            opts { l0pipeline = pass arg : l0pipeline opts })
         "DESC")
  "Print pipeline argument to standard error."
  where pass arg =
          Pass { passName = case arg of
                              Just arg' -> "trace " ++ arg'
                              Nothing   -> "trace"
               , passOp = \prog -> tell (prettyPrint prog) >> return prog
               }

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

fotransform :: String -> [String] -> L0Option
fotransform =
  passoption "Transform all second-order array combinators to for-loops."
  Pass { passName = "first-order transform"
       , passOp = return . FOT.transformProg
       }

tatransform :: String -> [String] -> L0Option
tatransform =
  passoption "Transform arrays of tuples to tuples of arrays."
  Pass { passName = "tuple-of-arrays transform"
       , passOp = return . TAT.transformProg
       }

eotransform :: String -> [String] -> L0Option
eotransform =
  passoption "Perform simple enabling optimisations."
  Pass { passName = "enabling optimations"
       , passOp = either (throwError . show) return . enablingOpts
       }

main :: IO ()
main = do args <- getArgs
          case getOpt RequireOrder commandLineOptions args of
            (opts, [file], []) -> do
              contents <- readFile file
              let config = foldl (.) id opts newL0Config
                  (msgs, res) = l0c config file contents
              hPutStr stderr msgs
              case res of
                Left err -> do hPutStrLn stderr err
                               exitWith $ ExitFailure 2
                Right prog -> l0action config prog
            (_, _, errs) -> do
              prog <- getProgName
              mapM_ (hPutStr stderr) errs
              hPutStr stderr "\n"
              hPutStr stderr $ usageInfo (prog ++ " [options] <file>") commandLineOptions
              exitWith $ ExitFailure 1

l0c :: L0Config -> FilePath -> String -> (String, Either String (Prog Type))
l0c config filename srccode =
  case runWriter (runErrorT l0c') of
    (Left err, msgs) -> (msgs, Left err)
    (Right prog, msgs) -> (msgs, Right prog)
  where l0c' :: L0CM (Prog Type)
        l0c' = canFail (parseL0 filename srccode) >>=
               canFail . checkProg >>=
               pipeline
        pipeline = foldl comb return $ l0pipeline config
        comb prev pass prog = do
          prog' <- prev prog
          when (l0verbose config) $ tell $ "Running " ++ passName pass ++ ".\n"
          res <- lift $ runErrorT $ passOp pass prog'
          case res of
            Left err ->
              throwError $ "Error during pass '" ++ passName pass ++ "':" ++ err
            Right prog'' ->
              case checkProg prog'' of
                Left err ->
                  throwError $ "Type error after pass '" ++
                  passName pass ++ "':\n" ++ show err ++
                  if l0verbose config
                  then "\nErroneous program is:\n" ++ prettyPrint prog''
                  else ""
                Right prog''' -> return prog'''

canFail :: Show err => Either err a -> L0CM a
canFail (Left err) = throwError $ show err
canFail (Right v)  = return v
