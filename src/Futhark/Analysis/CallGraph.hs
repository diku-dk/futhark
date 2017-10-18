-- | This module exports functionality for generating a call graph of
-- an Futhark program.
module Futhark.Analysis.CallGraph
  ( CallGraph
  , buildCallGraph
  )
  where

import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (isJust)

import Futhark.Representation.SOACS

type FunctionTable = M.Map Name FunDef

buildFunctionTable :: Prog -> FunctionTable
buildFunctionTable =
  foldl expand M.empty . progFunctions
  where expand ftab f =
          M.insert (funDefName f) f ftab

-- | The symbol table for functions
newtype CGEnv = CGEnv { envFtable  :: FunctionTable }

type CGM = Reader CGEnv

-- | Building the call grah runs in this monad.  There is no
-- mutable state.
runCGM :: CGM a -> CGEnv -> a
runCGM = runReader

-- | The call graph is just a mapping from a function name, i.e., the
-- caller, to a list of the names of functions called by the function.
-- The order of this list is not significant.
type CallGraph = M.Map Name (S.Set Name)

-- | @buildCallGraph prog@ build the program's Call Graph. The representation
-- is a hashtable that maps function names to a list of callee names.
buildCallGraph :: Prog -> CallGraph
buildCallGraph prog = do
  let ftable = buildFunctionTable prog
  runCGM (foldM buildCGfun M.empty entry_points) $ CGEnv ftable
  where entry_points = map funDefName $ filter (isJust . funDefEntryPoint) $ progFunctions prog

-- | @buildCallGraph cg f@ updates Call Graph @cg@ with the contributions of function
-- @fname@, and recursively, with the contributions of the callees of @fname@.
buildCGfun :: CallGraph -> Name -> CGM CallGraph
buildCGfun cg fname  = do
  bnd <- asks $ M.lookup fname . envFtable
  case bnd of
    Nothing -> return cg -- Must be builtin or similar.
    Just f ->
      case M.lookup fname cg of
        Just _  -> return cg
        Nothing -> do let callees = buildCGbody mempty $ funDefBody f
                      let cg' = M.insert fname callees cg

                      -- recursively build the callees
                      foldM buildCGfun cg' callees

buildCGbody :: S.Set Name -> Body -> S.Set Name
buildCGbody callees = foldl (\x -> buildCGexp x . stmExp) callees . bodyStms

buildCGexp :: S.Set Name -> Exp -> S.Set Name
buildCGexp callees (Apply fname _ _)
  | fname `elem` callees = callees
  | otherwise            = S.insert fname callees
buildCGexp callees (Op op) =
  case op of Map _ lam _ ->
               buildCGbody callees $ lambdaBody lam
             Reduce _ _ lam _ ->
               buildCGbody callees $ lambdaBody lam
             Scan _ lam _ ->
               buildCGbody callees $ lambdaBody lam
             Redomap _ _ lam0 lam1 _ _ ->
               buildCGbody (buildCGbody callees $ lambdaBody lam0) (lambdaBody lam1)
             Scanomap _ lam0 lam1 _ _ ->
               buildCGbody (buildCGbody callees $ lambdaBody lam0) (lambdaBody lam1)
             Stream _ (Parallel _ _ lam0 _) lam _ ->
               buildCGbody (buildCGbody callees $ lambdaBody lam0) (extLambdaBody lam)
             Stream _ _ lam _ ->
               buildCGbody callees (extLambdaBody lam)
             Scatter {} ->
               callees
buildCGexp callees e =
  foldExp folder callees e
  where folder =
          identityFolder { foldOnBody = \x body -> return $ buildCGbody x body
                         }
