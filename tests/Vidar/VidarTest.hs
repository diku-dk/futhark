module VidarTests where

import qualified Vidar as V
import qualified Vidar.Match as V
import qualified Vidar.Parser as V

import Vidarify

import Data.List (isInfixOf, isPrefixOf, intercalate)

import System.IO
import System.Exit (exitWith, exitSuccess, ExitCode(..))

import Control.Monad
import Control.Monad.Writer.Strict (runWriter)
import Control.Monad.Except

import Language.Futhark.Parser
import Language.Futhark.Core
import Futhark.Pipeline
import Futhark.Passes
import Futhark.Analysis.Alias
import Futhark.Internalise
import Futhark.Actions (printAction)

import qualified Futhark.Representation.Basic as I
import qualified Futhark.Representation.AST.Syntax as S
import qualified Futhark.TypeCheck as I

import qualified Futhark.Representation.External as E
import qualified Futhark.Representation.External.TypeChecker as E
import qualified Futhark.Representation.External.Renamer as E

foo :: IO I.Prog
foo = do
    (Basic p) <- compile f
    return p
  where f = "../testprogrammer/no-assert.fut"

run :: FilePath -> IO ()
run f = do
    (Basic p) <- compile f
    vidarCode <- extractVidar f
    case V.parse vidarCode of
        Left _  -> putStrLn "Could not parse Vidar code"
        Right v -> putStrLn $ show $ V.match (V.SubBlock $ V.StrictBlock v) (vidarify p)

extractVidar :: FilePath -> IO String
extractVidar f = do
    s <- readFile f
    return $ intercalate "\n" $ getVidarLines s

getVidarLines :: String -> [String]
getVidarLines s = map (dropWhile (\x -> x == '/' || x == ' '))
                $ drop 1
                $ dropWhile (not . ("BEGIN_VIDAR" `isInfixOf`))
                $ takeWhile (not . ("END_VIDAR" `isInfixOf`))
                $ filter ("//" `isPrefixOf`)
                $ lines s

newFutharkConfig :: FutharkConfig
newFutharkConfig = FutharkConfig {
                futharkpipeline = standardPipeline
              , futharkaction = printAction
              , futharkcheckAliases = True
              , futharkverbose = Nothing
              , futharkboundsCheck = True
              }

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

compile :: FilePath -> IO PipelineState
compile file = do
  contents <- readFile file
  let (msgs, res) = futharkc file contents
  hPutStr stderr msgs
  case res of
    Left err -> do
      hPutStrLn stderr $ errorDesc err
      case (errorState err, futharkverbose newFutharkConfig) of
        (Just prog, Just outfile) ->
          maybe (hPutStr stderr) writeFile outfile $
            I.pretty prog ++ "\n"
        _ -> return ()
      exitWith $ ExitFailure 2
    Right prog -> return prog

typeCheck :: (prog -> Either err prog')
          -> (prog -> Either err prog')
          -> FutharkConfig
          -> prog -> Either err prog'
typeCheck checkProg checkProgNoUniqueness config
  | futharkcheckAliases config = checkProg
  | otherwise                  = checkProgNoUniqueness


futharkc :: FilePath -> String -> (String, Either CompileError PipelineState)
futharkc filename srccode =
  case runWriter (runExceptT futharkc') of
    (Left err, msgs) -> (msgs, Left err)
    (Right prog, msgs) -> (msgs, Right prog)
  where futharkc' = do
          parsed_prog <- canFail "" Nothing $ parseFuthark filename srccode
          ext_prog    <- canFail "" Nothing $
                         typeCheck E.checkProg E.checkProgNoUniqueness newFutharkConfig
                         parsed_prog
          let int_prog = internaliseProg (futharkboundsCheck newFutharkConfig) $ E.tagProg ext_prog
          _ <- canFail "After internalisation:\n" (Just $ Basic int_prog)
               (typeCheck I.checkProg I.checkProgNoUniqueness newFutharkConfig int_prog)
          runPasses newFutharkConfig $ Basic int_prog
