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

import Data.List
import qualified Data.HashMap.Lazy as HM

import Futhark.InternalRep
import Futhark.Optimise.Errors

type FunctionTable = HM.HashMap Name FunDec

buildFunctionTable :: Prog -> Either Error FunctionTable
buildFunctionTable prog =
  foldM expand HM.empty (progFunctions prog)
  where
    expand ftab f@(name,_,_,_,pos)
      | Just (_,_,_,_,pos2) <- HM.lookup name ftab =
        Left $ DupDefinitionError name pos pos2
      | otherwise = Right $ HM.insert name f ftab

-- | The symbol table for functions
data CGEnv = CGEnv { envFtable  :: FunctionTable }

type CGM = ReaderT CGEnv (Either Error)

-- | Building the call grah runs in this monad.  There is no
-- mutable state.
runCGM :: CGM a -> CGEnv -> Either Error a
runCGM = runReaderT

badCGM :: Error -> CGM a
badCGM = lift . Left

-- | The call graph is just a mapping from a function name, i.e., the
-- caller, to a three-element tuple: The first element is a list that
-- contains the (unique) function names that may be called directly
-- from the current function, i.e., ``apply'' callees. The second
-- element is a list that contains the (unique) function names that
-- may be called via SOACs.
type CallGraph = HM.HashMap Name ([Name],[Name])

-- | @buildCallGraph prog@ build the program's Call Graph. The representation
-- is a hashtable that maps function names to a list of callee names.
buildCallGraph :: Prog -> Either Error CallGraph
buildCallGraph prog = do
  ftable <- buildFunctionTable prog
  runCGM (buildCGfun HM.empty defaultEntryPoint) $ CGEnv ftable

-- | @buildCallGraph cg fname@ updates Call Graph @cg@ with the contributions of function
-- @fname@, and recursively, with the contributions of the callees of @fname@.
-- In particular, @buildCGfun HM.empty defaultEntryPoint@ should construct the Call Graph
-- of the whole program.
buildCGfun :: CallGraph -> Name -> CGM CallGraph
buildCGfun cg fname  = do
  bnd <- asks $ HM.lookup fname . envFtable
  case bnd of
    Nothing -> badCGM $ FunctionNotInFtab fname
    Just (caller,_,_,body,pos) ->
      if caller == fname
      then
        case HM.lookup caller cg of
          Just _  -> return cg
          Nothing -> do let callees@(fs, soacs) = buildCGbody ([],[]) body

                        let cg' = HM.insert caller callees cg

                        -- recursively build the callees
                        let fs_soacs = fs `union` soacs
                        foldM buildCGfun cg' fs_soacs

      else  badCGM $ TypeError pos  (" in buildCGfun lookup for fundec of " ++
                                     nameToString fname ++ " resulted in " ++
                                     nameToString caller)
  where

buildCGbody :: ([Name],[Name]) -> Body -> ([Name],[Name])
buildCGbody = foldBody build
  where build = identityFolder {
                  foldOnBody = \x -> return . buildCGbody x
                , foldOnExp  = \x -> return . buildCGexp  x
                }

buildCGexp :: ([Name],[Name]) -> Exp -> ([Name],[Name])

buildCGexp callees@(fs, soacfs) (Apply fname _ _ _)  =
    if isBuiltInFunction fname || elem fname fs || nameToString fname == "trace"
    then callees
    else (fname:fs, soacfs)

buildCGexp callees e =
    foldlPattern buildCGexp addLamFun callees e

addLamFun :: ([Name],[Name]) -> Lambda -> ([Name],[Name])
addLamFun callees _ = callees
