-- | This module exports functionality for generating a call graph of
-- an Futhark program.
module Futhark.Analysis.CallGraph
  ( CallGraph
  , buildCallGraph
  )
  where

import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.List (foldl')

import Futhark.Representation.SOACS

type FunctionTable = M.Map Name (FunDef SOACS)

buildFunctionTable :: Prog SOACS -> FunctionTable
buildFunctionTable = foldl expand M.empty . progFuns
  where expand ftab f = M.insert (funDefName f) f ftab

-- | The call graph is just a mapping from a function name, i.e., the
-- caller, to a set of the names of functions called *directly* (not
-- transitively!) by the function.
type CallGraph = M.Map Name (M.Map Name ConstFun)

-- | @buildCallGraph prog@ build the program's call graph.
buildCallGraph :: Prog SOACS -> CallGraph
buildCallGraph prog = foldl' (buildCGfun ftable) M.empty entry_points
  where entry_points = map funDefName $ filter (isJust . funDefEntryPoint) $
                       progFuns prog
        ftable = buildFunctionTable prog

-- | @buildCallGraph ftable cg fname@ updates @cg@ with the
-- contributions of function @fname@.
buildCGfun :: FunctionTable -> CallGraph -> Name -> CallGraph
buildCGfun ftable cg fname  =
  -- Check if function is a non-builtin that we have not already
  -- processed.
  case M.lookup fname ftable of
    Just f | Nothing <- M.lookup fname cg -> do
               let callees = buildCGbody $ funDefBody f
                   cg' = M.insert fname callees cg
               -- recursively build the callees
               foldl' (buildCGfun ftable) cg' $ M.keys callees
    _ -> cg

buildCGbody :: Body -> M.Map Name ConstFun
buildCGbody = mconcat . map (buildCGexp . stmExp) . stmsToList . bodyStms

buildCGexp :: Exp -> M.Map Name ConstFun
buildCGexp (Apply fname _ _ (constf, _, _, _)) = M.singleton fname constf
buildCGexp (Op op) = execWriter $ mapSOACM folder op
  where folder = identitySOACMapper {
          mapOnSOACLambda = \lam -> do tell $ buildCGbody $ lambdaBody lam
                                       return lam
          }
buildCGexp e = execWriter $ mapExpM folder e
  where folder = identityMapper {
          mapOnBody = \_ body -> do tell $ buildCGbody body
                                    return body
          }
