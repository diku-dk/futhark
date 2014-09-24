-- | This module exports a generally useful expression compiler.
module Futhark.CodeGen.FirstOrderSOACS
  (firstOrderSOACS)
where

import Control.Monad

import qualified Futhark.FirstOrderTransform as FOT
import Futhark.Representation.AST.Attributes.Patterns (patternIdents)
import Futhark.Tools

import qualified Futhark.CodeGen.ImpGen as ImpGen

-- | A generic expression compiler that turns SOACs into do-loops,
-- since ImpGen itself cannot handle them.
firstOrderSOACS :: ImpGen.ExpCompiler a
firstOrderSOACS targets e
  | FOT.transformable e =
    liftM ImpGen.CompileBindings $ do
      (e',bnds) <- runBinder'' $ FOT.transformExp e
      return $ bnds ++ [mkLet (patternIdents targets) e']
  | otherwise           =
    return $ ImpGen.CompileExp e
