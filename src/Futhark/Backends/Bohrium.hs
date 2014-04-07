-- | Bohrium code generator.
--
-- Interesting stuff does in "Futhark.Backends.BohriumCodeGen".
--

module Futhark.Backends.Bohrium (compileProg) where

import Control.Monad

import Data.Loc

import Futhark.InternalRep
import qualified Futhark.FirstOrderTransform as FOT
import Futhark.Backends.BohriumCodeGen
import Futhark.Tools

import qualified Futhark.Backends.GenericC as GenericC

compileProg :: Prog -> String
compileProg = addHeader . GenericC.compileProg expCompiler
  where expCompiler target e = do
          res <- compileSOACtoBohrium target e
          case res of Nothing   -> liftM GenericC.CompileBody $ runBinder $ do
                                     es <- letTupExp "soac" =<< FOT.transformExp e
                                     return $ resultBody [] (map Var es) $ srclocOf e
                      Just res' -> return $ GenericC.CCode res'
        addHeader = ("#include <bh_c.h>\n"++)
