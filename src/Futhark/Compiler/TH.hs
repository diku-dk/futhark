-- | Wrappers around "Futhark.Compiler" to provide functionality
-- useful for Template Haskell.
module Futhark.Compiler.TH
  (embedBasis)
where

import Control.Monad.Except (runExceptT)
import Language.Futhark.TH ()
import Language.Haskell.TH.Syntax (Exp, Q, runIO, lift, qAddDependentFile)
import System.FilePath

import Futhark.Compiler.Program
import Futhark.Util (directoryContents)

futFiles :: FilePath -> IO [FilePath]
futFiles dir = filter isFut <$> directoryContents dir
  where isFut = (==".fut") . takeExtension

readBasis :: ImportPaths -> FilePath -> String -> Q Basis
readBasis search_path fpath entry = do
  files <- runIO $ futFiles fpath

  -- In many cases, the 'fpath' may be only a single file, which
  -- imports others.  We will assume that all .fut files in the
  -- containing directory may influence for dependency information.
  -- Even if we get this wrong, it only means we'll do a few
  -- unnecessary recompiles.
  all_files <- runIO $ futFiles $ takeDirectory fpath
  mapM_ qAddDependentFile all_files

  res <- runIO $ runExceptT $ readLibrary False emptyBasis search_path files
  case res of
    Right (_, imps, src) ->
      return $ Basis imps src [entry]
    Left err -> error $ show err

-- | At compile-time, produce an 'Exp' corresponding to a 'Basis'.
-- The 'FilePath' must refer to a @.fut@ file.
embedBasis :: ImportPaths -> FilePath -> String -> Q Exp
embedBasis search_path fpath entry =
  lift =<< readBasis search_path fpath entry
