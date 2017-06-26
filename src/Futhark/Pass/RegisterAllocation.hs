{-# LANGUAGE TypeFamilies #-}
-- | Use "register allocation" on memory blocks.  Do a pass over the bodies and
-- their live intervals to reduce memory usage.
--
-- FIXME: Find a better name for this module!
--
-- Enable by setting the environment variable REGISTER_ALLOCATION=1.
module Futhark.Pass.RegisterAllocation
  ( runThroughAllocations
  ) where

import System.IO.Unsafe (unsafePerformIO) -- Just for debugging!

import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe (isJust)
import qualified Data.Map.Strict as M

import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Pass
import Futhark.Representation.AST
import qualified Futhark.Representation.ExplicitMemory as ExpMem

import Futhark.Pass.RegisterAllocation.Traversal (regAllocFunDef, RegAllocResult)

import Futhark.Util (unixEnvironment)
usesDebugging :: Bool
usesDebugging = isJust $ lookup "FUTHARK_DEBUG" unixEnvironment

runThroughAllocations :: Pass ExpMem.ExplicitMemory ExpMem.ExplicitMemory
runThroughAllocations = simplePass
                    "use register allocation methods on memory blocks"
                    "Transform program to reuse non-interfering memory blocks"
                    transformProg


transformProg :: MonadFreshNames m
              => Prog ExpMem.ExplicitMemory
              -> m (Prog ExpMem.ExplicitMemory)
transformProg prog = do
  prog1 <- intraproceduralTransformation transformFunDef prog

  let debug = unsafePerformIO $ when usesDebugging $ putStrLn $ pretty prog1

  debug `seq` return prog1


transformFunDef :: MonadFreshNames m
                => FunDef ExpMem.ExplicitMemory
                -> m (FunDef ExpMem.ExplicitMemory)
transformFunDef fundef = do
  let allocs = regAllocFunDef fundef
      body' = runReader (transformBody $ funDefBody fundef) allocs
  return fundef { funDefBody = body' }

type TransformM = Reader RegAllocResult

transformBody :: Body ExpMem.ExplicitMemory -> TransformM (Body ExpMem.ExplicitMemory)
transformBody (Body () bnds res) = do
  bnds' <- mapM transformStm bnds
  return $ Body () bnds' res

transformStm :: Stm ExpMem.ExplicitMemory -> TransformM (Stm ExpMem.ExplicitMemory)
transformStm (Let (Pattern patctxelems patvalelems) () e) = do
  e' <- mapExpM transform e
  patvalelems' <- mapM transformPatValElemT patvalelems

  let pat' = Pattern patctxelems patvalelems'

  return $ Let pat' () e'

  where transform = identityMapper { mapOnBody = const transformBody
                                   }
transformPatValElemT :: PatElemT (LetAttr ExpMem.ExplicitMemory)
                     -> TransformM (PatElemT (LetAttr ExpMem.ExplicitMemory))
transformPatValElemT (PatElem x bindage
                      membound@(ExpMem.ArrayMem pt shape u _xmem xixfun)) = do
  mapping <- M.lookup x <$> ask
  let membound' = case mapping of
        Nothing -> membound
        Just xmem' -> ExpMem.ArrayMem pt shape u xmem' xixfun
  return $ PatElem x bindage membound'
transformPatValElemT pe = return pe
