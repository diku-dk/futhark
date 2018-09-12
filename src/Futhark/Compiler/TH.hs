-- | Wrappers around "Futhark.Compiler" to provide functionality
-- useful for Template Haskell.
module Futhark.Compiler.TH
  (embedBasis)
where

import Control.Monad.Except (runExceptT, (<=<))
import Language.Futhark.TH ()
import Language.Haskell.TH.Syntax (Exp, Q, runIO, lift, qAddDependentFile)
import System.FilePath

import Futhark.Compiler.Program
import Language.Futhark.Semantic (mkInitialImport)

readBasis :: [String] -> Q Basis
readBasis roots = do
  res <- runIO $ runExceptT $ readImports emptyBasis $ map mkInitialImport roots
  case res of
    Right (_, imps, src) -> do
      mapM_ (qAddDependentFile . (<.> "fut") . fst) imps
      return $ Basis imps src roots
    Left err             -> error $ show err

-- | At compile-time, produce an 'Exp' corresponding to a 'Basis'.
embedBasis :: [String] -> Q Exp
embedBasis = lift <=< readBasis
