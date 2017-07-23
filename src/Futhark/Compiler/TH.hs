-- | Wrappers around "Futhark.Compiler" to provide functionality
-- useful for Template Haskell.
module Futhark.Compiler.TH
  (embedBasis)
where

import Control.Monad.Except (runExceptT)
import Language.Futhark.TH ()
import Language.Haskell.TH.Syntax (Exp, Q, runIO, lift, qAddDependentFile)
import System.FilePath

import Futhark.Compiler
import Futhark.Util (directoryContents)

futFiles :: FilePath -> IO [FilePath]
futFiles dir = filter isFut <$> directoryContents dir
  where isFut = (==".fut") . takeExtension

readBasis :: FilePath -> String -> Q Basis
readBasis fpath entry = do
  files <- runIO $ futFiles fpath

  -- In many cases, the 'fpath' may be only a single file, which
  -- imports others.  We will assume that all .fut files in the
  -- containing directory may influence for dependency information.
  -- Even if we get this wrong, it only means we'll do a few
  -- unnecessary recompiles.
  all_files <- runIO $ futFiles $ takeDirectory fpath
  mapM_ qAddDependentFile all_files

  res <- runIO $ runExceptT $ readProgram emptyBasis files
  case res of
    Right (_, _, imps, src) ->
      return $ Basis imps src [entry]
    Left err -> error $ show err

-- | At compile-time, produce an 'Exp' corresponding to a 'Basis'.
-- The 'FilePath' must refer to a @.fut@ file.
embedBasis :: FilePath -> String -> Q Exp
embedBasis fpath entry =
  lift =<< readBasis fpath entry
