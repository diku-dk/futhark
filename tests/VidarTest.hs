module VidarTests where

import qualified Vidar as V
--import FutharkFuseThing

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
  where f = "../testprogrammer/fuse-across-reshape1.fut"

main :: IO ()
main = do
    p <- foo
    putStrLn $ V.ppVidar $ vidarify p

vidarify :: I.Prog -> [V.Element I.Exp]
vidarify (I.Prog decs) = map vidarifyDec decs

vidarifyDec :: I.FunDec -> V.Element I.Exp
vidarifyDec (I.FunDec n _ params body) =
  V.Block (V.ExactName $ nameToString n)
    $ V.StrictBlock [V.SubBlock $ V.StrictBlock $ vidarifyParams params  -- parameters
                    ,V.SubBlock $ V.StrictBlock $ vidarifyFuncBody body] -- body

vidarifyParams :: [S.FParam I.Basic] -> [V.Element I.Exp]
vidarifyParams _ = []

vidarifyFuncBody :: I.Body -> [V.Element I.Exp]
vidarifyFuncBody (I.Body _ bs res) =
    map vidarifyBinding bs ++ [vidarifyRes res]

vidarifyBinding :: I.Binding -> V.Element I.Exp
vidarifyBinding (I.Let p _ exp) = V.Binding (vidarifyPattern p) (vidarifyExp exp)

vidarifyExp :: I.Exp -> V.Element I.Exp
vidarifyExp (I.PrimOp p) = vidarifyPrimOp p
vidarifyExp e = V.Expr e

vidarifyPrimOp :: I.PrimOp -> V.Element I.Exp
vidarifyPrimOp _ = V.Anything

vidarifyPattern :: I.Pattern -> V.Name
vidarifyPattern p = V.AnyName

vidarifyRes :: I.Result -> V.Element I.Exp
vidarifyRes res = V.Anything

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
