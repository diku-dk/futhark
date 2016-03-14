-- | This module exports functionality for generating a call graph of
-- an Futhark program.
module Futhark.Analysis.CallGraph
  ( CallGraph
  , buildCallGraph
  , FunctionTable
  , buildFunctionTable
  )
  where

import Control.Monad.Reader

import qualified Data.HashMap.Lazy as HM

import Futhark.Representation.SOACS

type FunctionTable = HM.HashMap Name FunDef

buildFunctionTable :: Prog -> FunctionTable
buildFunctionTable =
  foldl expand HM.empty . progFunctions
  where expand ftab f =
          HM.insert (funDefName f) f ftab

-- | The symbol table for functions
data CGEnv = CGEnv { envFtable  :: FunctionTable }

type CGM = Reader CGEnv

-- | Building the call grah runs in this monad.  There is no
-- mutable state.
runCGM :: CGM a -> CGEnv -> a
runCGM = runReader

-- | The call graph is just a mapping from a function name, i.e., the
-- caller, to a list of the names of functions called by the function.
-- The order of this list is not significant.
type CallGraph = HM.HashMap Name [Name]

-- | @buildCallGraph prog@ build the program's Call Graph. The representation
-- is a hashtable that maps function names to a list of callee names.
buildCallGraph :: Prog -> CallGraph
buildCallGraph prog = do
  let ftable = buildFunctionTable prog
  runCGM (foldM buildCGfun HM.empty entry_points) $ CGEnv ftable
  where entry_points = map funDefName $ filter funDefEntryPoint $ progFunctions prog

-- | @buildCallGraph cg f@ updates Call Graph @cg@ with the contributions of function
-- @fname@, and recursively, with the contributions of the callees of @fname@.
buildCGfun :: CallGraph -> Name -> CGM CallGraph
buildCGfun cg fname  = do
  bnd <- asks $ HM.lookup fname . envFtable
  case bnd of
    Nothing -> return cg -- Must be builtin or similar.
    Just f ->
      case HM.lookup fname cg of
        Just _  -> return cg
        Nothing -> do let callees = buildCGbody [] $ funDefBody f
                      let cg' = HM.insert fname callees cg

                      -- recursively build the callees
                      foldM buildCGfun cg' callees

buildCGbody :: [Name] -> Body -> [Name]
buildCGbody callees = foldl (\x -> buildCGexp x . bindingExp) callees . bodyBindings

buildCGexp :: [Name] -> Exp -> [Name]
buildCGexp callees (Apply fname _ _)
  | fname `elem` callees = callees
  | otherwise            = fname:callees
buildCGexp callees (Op op) =
  case op of Map _ _ lam _ ->
               buildCGbody callees $ lambdaBody lam
             Reduce _ _ _ lam _ ->
               buildCGbody callees $ lambdaBody lam
             Scan _ _ lam _ ->
               buildCGbody callees $ lambdaBody lam
             Redomap _ _ _ lam0 lam1 _ _ ->
               buildCGbody (buildCGbody callees $ lambdaBody lam0) (lambdaBody lam1)
             Scanomap _ _ lam0 lam1 _ _ ->
               buildCGbody (buildCGbody callees $ lambdaBody lam0) (lambdaBody lam1)
             Stream _ _ (RedLike _ _ lam0 _) lam _ ->
               buildCGbody (buildCGbody callees $ lambdaBody lam0) (extLambdaBody lam)
             Stream _ _ _ lam _ ->
               buildCGbody callees (extLambdaBody lam)
             Write {} ->
               callees
buildCGexp callees e =
  foldExp folder callees e
  where folder =
          identityFolder { foldOnBody = \x body -> return $ buildCGbody x body
                         }
