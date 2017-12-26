{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.Compiler.Program
       ( readProgram
       , readLibrary
       , E.Imports
       , E.FileModule(..)

       , ImportPaths
       , importPath
       , Basis(..)
       , emptyBasis
       )
where

import Data.Monoid
import Data.Loc
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Extra (firstJustM)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
import System.FilePath
import qualified System.FilePath.Posix as Posix
import System.IO.Error
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.TH.Syntax (Lift)
import qualified Data.Semigroup as Sem

import Futhark.Error
import Futhark.FreshNames
import Language.Futhark.Parser
import qualified Language.Futhark as E
import qualified Language.Futhark.TypeChecker as E
import Language.Futhark.Futlib
import Language.Futhark.TH ()

-- | A little monad for reading and type-checking a Futhark program.
type CompilerM m = ReaderT [FilePath] (StateT ReaderState m)

data ReaderState = ReaderState { alreadyImported :: E.Imports
                               , nameSource :: VNameSource
                               , warnings :: E.Warnings
                               }

-- | Pre-typechecked imports, including a starting point for the name source.
data Basis = Basis { basisImports :: E.Imports
                   , basisNameSource :: VNameSource
                   , basisRoots :: [String]
                     -- ^ Files that should be implicitly opened.
                   }
           deriving (Lift)

emptyBasis :: Basis
emptyBasis = Basis { basisImports = mempty
                   , basisNameSource = src
                   , basisRoots = mempty
                   }
  where src = newNameSource $ succ $ maximum $ map E.baseTag $ M.keys E.intrinsics

newtype ImportPaths = ImportPaths [FilePath]
  deriving (Sem.Semigroup, Monoid)

importPath :: FilePath -> ImportPaths
importPath = ImportPaths . pure

-- | Absolute reference to a Futhark code file.  Does not include the
-- @.fut@ extension.  The 'FilePath' must be absolute.
data FutharkInclude = FutharkInclude Posix.FilePath SrcLoc
  deriving (Eq, Show)

instance Located FutharkInclude where
  locOf (FutharkInclude _ loc) = locOf loc

mkInitialInclude :: String -> FutharkInclude
mkInitialInclude s = FutharkInclude ("/" Posix.</> toPOSIX s) noLoc
  where
    toPOSIX :: FilePath -> Posix.FilePath
    toPOSIX = Posix.joinPath . splitDirectories

mkInclude :: FutharkInclude -> String -> SrcLoc -> FutharkInclude
mkInclude (FutharkInclude includer _) includee =
  FutharkInclude (takeDirectory includer Posix.</> includee)

includeToFilePath :: FilePath -> FutharkInclude -> FilePath
includeToFilePath dir (FutharkInclude fp _) =
  dir </> fromPOSIX (Posix.makeRelative "/" fp) <.> "fut"
  where
    -- | Some bad operating systems do not use forward slash as
    -- directory separator - this is where we convert Futhark includes
    -- (which always use forward slash) to native paths.
    fromPOSIX :: Posix.FilePath -> FilePath
    fromPOSIX = joinPath . Posix.splitDirectories

includeToString :: FutharkInclude -> String
includeToString (FutharkInclude s _) = s

includePath :: FutharkInclude -> String
includePath (FutharkInclude s _) = Posix.takeDirectory s

readImport :: (MonadError CompilerError m, MonadIO m) =>
              Bool -> ImportPaths -> [FutharkInclude] -> FutharkInclude -> CompilerM m ()
readImport permit_recursion search_path steps include
  | include `elem` steps =
      throwError $ ExternalError $ T.pack $
      "Import cycle: " ++ intercalate " -> "
      (map includeToString $ reverse $ include:steps)
  | otherwise = do
      already_done <- gets $ isJust . lookup (includeToString include) . alreadyImported

      unless already_done $
        uncurry (handleFile permit_recursion search_path steps include) =<<
        readImportFile search_path include

handleFile :: (MonadIO m, MonadError CompilerError m) =>
              Bool
           -> ImportPaths
           -> [FutharkInclude]
           -> FutharkInclude
           -> T.Text
           -> FilePath
           -> CompilerM m ()
handleFile permit_recursion search_path steps include file_contents file_name = do
  prog <- case parseFuthark file_name file_contents of
    Left err -> externalErrorS $ show err
    Right prog -> return prog

  mapM_ (readImport permit_recursion search_path steps' . uncurry (mkInclude include)) $
    E.progImports prog

  -- It is important to not read these before the above calls to
  -- readImport.
  imports <- gets alreadyImported
  src <- gets nameSource
  prelude <- ask

  case E.checkProg permit_recursion imports src (includePath include) $
       prependPrelude prelude prog of
    Left err ->
      externalError $ T.pack $ show err
    Right (m, ws, src') ->
      modify $ \s ->
        s { alreadyImported = (includeToString include,m) : imports
          , nameSource      = src'
          , warnings        = warnings s <> ws
          }
  where steps' = include:steps

readImportFile :: (MonadError CompilerError m, MonadIO m) =>
                  ImportPaths -> FutharkInclude -> m (T.Text, FilePath)
readImportFile (ImportPaths dirs) include = do
  -- First we try to find a file of the given name in the search path,
  -- then we look at the builtin library if we have to.  For the
  -- builtins, we don't use the search path.
  r <- liftIO $ firstJustM lookInDir dirs
  case (r, lookup abs_filepath futlib) of
    (Just (Right (filepath,s)), _) -> return (s, filepath)
    (Just (Left e), _)  -> externalErrorS e
    (Nothing, Just t)   -> return (t, includeToFilePath "[builtin]" include)
    (Nothing, Nothing)  -> externalErrorS not_found
   where abs_filepath = includeToFilePath "/" include

         lookInDir dir = do
           let filepath = includeToFilePath dir include
           (Just . Right . (filepath,) <$> T.readFile filepath) `catch` couldNotRead filepath

         couldNotRead filepath e
           | isDoesNotExistError e =
               return Nothing
           | otherwise             =
               return $ Just $ Left $
               "Could not read " ++ filepath ++ ": " ++ show e

         not_found =
           "Could not find import '" ++ includeToString include ++ "' at " ++
           E.locStr (srclocOf include) ++ " in path '" ++ show dirs ++ "'."

-- | Read and type-check a Futhark program, including all imports.
readProgram :: (MonadError CompilerError m, MonadIO m) =>
               Bool -> Basis -> ImportPaths -> FilePath
            -> m (E.Warnings,
                  E.Imports,
                  VNameSource)
readProgram permit_recursion basis search_path fp =
  runCompilerM basis $ do
    fs <- liftIO $ T.readFile fp

    handleFile permit_recursion (importPath fp_dir <> search_path)
      [] (mkInitialInclude fp_name) fs fp
  where (fp_dir, fp_file) = splitFileName fp
        (fp_name, _) = splitExtension fp_file

-- | Read and type-check a Futhark library (multiple files, relative
-- to the same search path), including all imports.
readLibrary :: (MonadError CompilerError m, MonadIO m) =>
               Bool -> Basis -> ImportPaths -> [FilePath]
            -> m (E.Warnings,
                  E.Imports,
                  VNameSource)
readLibrary permit_recursion basis search_path fps =
  runCompilerM basis (mapM onFile fps)
  where onFile fp =  do

          fs <- liftIO $ T.readFile fp

          handleFile permit_recursion search_path [] (mkInitialInclude fp_name) fs fp
            where (fp_name, _) = splitExtension fp

runCompilerM :: Monad m =>
                Basis -> CompilerM m a
             -> m (E.Warnings, [(String, E.FileModule)], VNameSource)
runCompilerM (Basis imports src roots) m = do
  let s = ReaderState (reverse imports) src mempty
  s' <- execStateT (runReaderT m roots) s
  return (warnings s',
          reverse $ alreadyImported s',
          nameSource s')

prependPrelude :: [FilePath] -> E.UncheckedProg -> E.UncheckedProg
prependPrelude prelude (E.Prog doc ds) =
  E.Prog doc $ map mkImport prelude ++ ds
  where mkImport fp =
          E.LocalDec (E.OpenDec (E.ModImport fp E.NoInfo noLoc) [] E.NoInfo noLoc) noLoc
