-- | Bohrium code generator.
--
-- Interesting stuff does in "L0C.Backends.BohriumCodeGen".
--

module L0C.Backends.Bohrium (compileProg) where

import Control.Monad

import Data.Loc

import L0C.InternalRep
import qualified L0C.FirstOrderTransform as FOT
import L0C.Backends.BohriumCodeGen
import L0C.Tools

import qualified L0C.Backends.GenericC as GenericC

compileProg :: Prog -> String
compileProg = addHeader . GenericC.compileProg expCompiler
  where expCompiler target e = do
          res <- compileSOACtoBohrium target e
          case res of Nothing   -> liftM GenericC.CompileBody $ runBinder $ do
                                     es <- letTupExp "soac" =<< FOT.transformExp e
                                     return $ resultBody [] (map Var es) $ srclocOf e
                      Just res' -> return $ GenericC.CCode res'
        addHeader = ("#include <bh_c.h>\n"++)
