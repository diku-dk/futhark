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
import Data.Maybe (isJust)

import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Pass
import Futhark.Representation.AST
import qualified Futhark.Representation.ExplicitMemory as ExpMem

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
transformFunDef = return
