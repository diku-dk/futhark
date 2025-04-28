-- | @futhark deps@
module Futhark.CLI.Deps (main) where

import Futhark.Compiler
import Futhark.Util.Loc
import Futhark.Util.Options
import Language.Futhark.Deps
import Language.Futhark.Syntax
import Text.Read (readMaybe)

containingModule :: Imports -> Pos -> Maybe FileModule
containingModule imports (Pos file _ _ _) =
  snd <$> find ((== file') . fst) imports
  where
    file' = mkInitialImport $ fst $ Posix.splitExtension file

-- | Run @futhark deps@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> do
      (_, imports, _) <- readProgramOrDie
      prog <- fileProg <$> containingModule imports pos
      Just $ putStrLn printDeps file 
    _ -> Nothing
