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
  where f = "../testprogrammer/no-assert.fut"

main :: IO ()
main = do
    p <- foo
    putStrLn $ V.ppVidar $ vidarify p

showName :: VName -> String
showName n = baseString n ++ show (baseTag n)

vidarify :: I.Prog -> [V.Element]
vidarify (I.Prog decs) = map vidarifyDec decs

vidarifyDec :: I.FunDec -> V.Element
vidarifyDec (I.FunDec n _retType params body) =
  V.Block (V.ExactName $ nameToString n)
    $ V.StrictBlock [V.SubBlock $ V.StrictBlock $ vidarifyParams params  -- parameters
                    ,V.SubBlock $ V.StrictBlock $ vidarifyFuncBody body] -- body

vidarifyParams :: [S.FParam I.Basic] -> [V.Element]
vidarifyParams = map vidarifyParam

vidarifyParam :: S.FParam I.Basic -> V.Element
vidarifyParam (S.FParam (S.Ident n _idType) _lore) = V.Name $ V.ExactName $ showName n
--vidarifyParam _ = V.Anything

vidarifyFuncBody :: I.Body -> [V.Element]
vidarifyFuncBody (I.Body _lore bs res) =
    map vidarifyBinding bs ++ [vidarifyRes res]

vidarifyBinding :: I.Binding -> V.Element
vidarifyBinding (I.Let p _lore exp) = V.Binding (vidarifyPattern p) (vidarifyExp exp)

vidarifyExp :: I.Exp -> V.Element
vidarifyExp (I.PrimOp p) = vidarifyPrimOp p
vidarifyExp e = V.Anything

vidarifyPrimOp :: I.PrimOp -> V.Element
vidarifyPrimOp (S.SubExp subexp) = vidarifySubExp subexp
vidarifyPrimOp (S.ArrayLit subexps _type) =
    V.Block (V.ExactName "array") $ V.StrictBlock $
        map vidarifySubExp subexps
vidarifyPrimOp (S.Assert subexp _loc) =
    V.Block (V.ExactName "assert") $ V.StrictBlock [
        vidarifySubExp subexp
    ]
vidarifyPrimOp _ = V.Anything

vidarifyPattern :: I.Pattern -> V.Name
vidarifyPattern (S.Pattern [b]) = vidarifyPatElem b
vidarifyPattern p = V.AnyName

vidarifyPatElem :: I.PatElem -> V.Name
vidarifyPatElem (S.PatElem (S.Ident n _idType) _bindage _lore) = V.ExactName $ showName n

vidarifyRes :: I.Result -> V.Element
vidarifyRes (S.Result subexps) = V.SubBlock $ V.StrictBlock $ map vidarifySubExp subexps

vidarifySubExp :: I.SubExp -> V.Element
vidarifySubExp (S.Constant bv)             = vidarifyBasicVal bv
vidarifySubExp (S.Var (S.Ident n _idType)) = V.Name $ V.ExactName $ showName n

vidarifyBasicVal :: BasicValue -> V.Element
vidarifyBasicVal (IntVal x) = V.Name $ V.ExactName $ show x
vidarifyBasicVal _ = V.Anything

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
