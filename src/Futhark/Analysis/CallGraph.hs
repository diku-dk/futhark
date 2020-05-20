{-# LANGUAGE OverloadedStrings #-}
-- | This module exports functionality for generating a call graph of
-- an Futhark program.
module Futhark.Analysis.CallGraph
  ( CallGraph
  , buildCallGraph
  , isFunInCallGraph
  , calls
  , calledByConsts
  , allCalledBy
  , findNoninlined
  )
  where

import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.List (foldl')

import Futhark.IR.SOACS

type FunctionTable = M.Map Name (FunDef SOACS)

buildFunctionTable :: Prog SOACS -> FunctionTable
buildFunctionTable = foldl expand M.empty . progFuns
  where expand ftab f = M.insert (funDefName f) f ftab

type FunGraph = M.Map Name (S.Set Name)

-- | The call graph is a mapping from a function name, i.e., the
-- caller, to a set of the names of functions called *directly* (not
-- transitively!) by the function.
--
-- We keep track separately of the functions called by constants.
data CallGraph = CallGraph { calledByFuns :: M.Map Name (S.Set Name)
                           , calledInConsts :: S.Set Name
                           }

-- | Is the given function known to the call graph?
isFunInCallGraph :: Name -> CallGraph -> Bool
isFunInCallGraph f = M.member f . calledByFuns

-- | Does the first function call the second?
calls :: Name -> Name -> CallGraph -> Bool
calls caller callee =
  maybe False (S.member callee) . M.lookup caller . calledByFuns

-- | Is the function called in any of the constants?
calledByConsts :: Name -> CallGraph -> Bool
calledByConsts f = S.member f . calledInConsts

-- | All functions called by this function.
allCalledBy :: Name -> CallGraph -> S.Set Name
allCalledBy f = fromMaybe mempty . M.lookup f . calledByFuns

-- | @buildCallGraph prog@ build the program's call graph.
buildCallGraph :: Prog SOACS -> CallGraph
buildCallGraph prog =
  CallGraph fg $ buildFGStms $ progConsts prog
  where fg = foldl' (buildFGfun ftable) M.empty entry_points

        entry_points = map funDefName $ progFuns prog
        ftable = buildFunctionTable prog

-- | @buildCallGraph ftable fg fname@ updates @fg@ with the
-- contributions of function @fname@.
buildFGfun :: FunctionTable -> FunGraph -> Name -> FunGraph
buildFGfun ftable fg fname  =
  -- Check if function is a non-builtin that we have not already
  -- processed.
  case M.lookup fname ftable of
    Just f | Nothing <- M.lookup fname fg -> do
               let callees = buildFGBody $ funDefBody f
                   fg' = M.insert fname callees fg
               -- recursively build the callees
               foldl' (buildFGfun ftable) fg' callees
    _ -> fg

buildFGStms :: Stms SOACS -> S.Set Name
buildFGStms = mconcat . map (buildFGexp . stmExp) . stmsToList

buildFGBody :: Body -> S.Set Name
buildFGBody = buildFGStms . bodyStms

buildFGexp :: Exp -> S.Set Name
buildFGexp (Apply fname _ _ _) = S.singleton fname
buildFGexp (Op op) = execWriter $ mapSOACM folder op
  where folder = identitySOACMapper {
          mapOnSOACLambda = \lam -> do tell $ buildFGBody $ lambdaBody lam
                                       return lam
          }
buildFGexp e = execWriter $ mapExpM folder e
  where folder = identityMapper {
          mapOnBody = \_ body -> do tell $ buildFGBody body
                                    return body
          }

-- | The set of all functions that are called noinline somewhere.
findNoninlined :: Prog SOACS -> S.Set Name
findNoninlined prog =
  foldMap onStm $ foldMap (bodyStms . funDefBody) (progFuns prog) <> progConsts prog
  where onStm :: Stm -> S.Set Name
        onStm (Let _ aux (Apply fname _ _ _))
          | "noinline" `inAttrs` stmAuxAttrs aux =
              S.singleton fname
        onStm (Let _ _ e) = execWriter $ mapExpM folder e
          where folder =
                  identityMapper
                  { mapOnBody = \_ body -> do tell $ foldMap onStm $ bodyStms body
                                              return body
                  , mapOnOp = mapSOACM
                              identitySOACMapper
                              { mapOnSOACLambda = \lam -> do
                                  tell $ foldMap onStm $ bodyStms $ lambdaBody lam
                                  return lam
                              }
                  }
