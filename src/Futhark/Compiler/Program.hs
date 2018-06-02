{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TupleSections #-}
module Futhark.Compiler.Program
       ( readProgram
       , readLibrary
       , Imports
       , FileModule(..)

       , Basis(..)
       , emptyBasis
       )
where

import Data.Semigroup ((<>))
import Data.Loc
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
import System.FilePath
import System.IO.Error
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.TH.Syntax (Lift)

import Futhark.Error
import Futhark.FreshNames
import Language.Futhark.Parser
import qualified Language.Futhark as E
import qualified Language.Futhark.TypeChecker as E
import Language.Futhark.Semantic
import Language.Futhark.Futlib
import Language.Futhark.TH ()

-- | A little monad for reading and type-checking a Futhark program.
type CompilerM m = ReaderT [FilePath] (StateT ReaderState m)

data ReaderState = ReaderState { alreadyImported :: Imports
                               , nameSource :: VNameSource
                               , warnings :: E.Warnings
                               }

-- | Pre-typechecked imports, including a starting point for the name source.
data Basis = Basis { basisImports :: Imports
                   , basisNameSource :: VNameSource
                   , basisRoots :: [String]
                     -- ^ Files that should be implicitly opened.
                   }
           deriving (Lift)

-- | A basis that contains no imports, and has a properly initialised
-- name source.
emptyBasis :: Basis
emptyBasis = Basis { basisImports = mempty
                   , basisNameSource = src
                   , basisRoots = mempty
                   }
  where src = newNameSource $ succ $ maximum $ map E.baseTag $ M.keys E.intrinsics

readImport :: (MonadError CompilerError m, MonadIO m) =>
              [ImportName] -> ImportName -> CompilerM m ()
readImport steps include
  | include `elem` steps =
      throwError $ ExternalError $ T.pack $
      "Import cycle: " ++ intercalate " -> "
      (map includeToString $ reverse $ include:steps)
  | otherwise = do
      already_done <- gets $ isJust . lookup (includeToString include) . alreadyImported

      unless already_done $
        uncurry (handleFile steps include) =<< readImportFile include

handleFile :: (MonadIO m, MonadError CompilerError m) =>
              [ImportName]
           -> ImportName
           -> T.Text
           -> FilePath
           -> CompilerM m ()
handleFile steps include file_contents file_name = do
  prog <- case parseFuthark file_name file_contents of
    Left err -> externalErrorS $ show err
    Right prog -> return prog

  mapM_ (readImport steps' . uncurry (mkImportFrom include)) $
    E.progImports prog

  -- It is important to not read these before the above calls to
  -- readImport.
  imports <- gets alreadyImported
  src <- gets nameSource
  prelude <- ask

  case E.checkProg imports src include $ prependPrelude prelude prog of
    Left err ->
      externalError $ T.pack $ show err
    Right (m, ws, src') ->
      modify $ \s ->
        s { alreadyImported = (includeToString include,m) : imports
          , nameSource      = src'
          , warnings        = warnings s <> ws
          }
  where steps' = include:steps

readFileSafely :: String -> IO (Maybe (Either String (String, T.Text)))
readFileSafely filepath =
  (Just . Right . (filepath,) <$> T.readFile filepath) `catch` couldNotRead
  where couldNotRead e
          | isDoesNotExistError e =
              return Nothing
          | otherwise             =
              return $ Just $ Left $ show e

readImportFile :: (MonadError CompilerError m, MonadIO m) =>
                  ImportName -> m (T.Text, FilePath)
readImportFile include = do
  -- First we try to find a file of the given name in the search path,
  -- then we look at the builtin library if we have to.  For the
  -- builtins, we don't use the search path.
  r <- liftIO $ readFileSafely $ includeToFilePath include
  case (r, lookup abs_filepath futlib) of
    (Just (Right (filepath,s)), _) -> return (s, filepath)
    (Just (Left e), _)  -> externalErrorS e
    (Nothing, Just t)   -> return (t, "[builtin]" </> includeToFilePath include)
    (Nothing, Nothing)  -> externalErrorS not_found
   where abs_filepath = includeToFilePath include

         not_found =
           "Error at " ++ E.locStr (srclocOf include) ++
           ": could not find import '" ++ includeToString include ++ "'."

-- | Read and type-check a Futhark program, including all imports.
readProgram :: (MonadError CompilerError m, MonadIO m) =>
               Basis -> FilePath
            -> m (E.Warnings,
                  Imports,
                  VNameSource)
readProgram basis fp = readLibrary basis [fp]

-- | Read and type-check a Futhark library (multiple files, relative
-- to the same search path), including all imports.
readLibrary :: (MonadError CompilerError m, MonadIO m) =>
               Basis -> [FilePath]
            -> m (E.Warnings,
                  Imports,
                  VNameSource)
readLibrary basis fps =
  runCompilerM basis (mapM onFile fps)
  where onFile fp =  do
          r <- liftIO $ readFileSafely fp
          case r of
            Just (Right (_, fs)) ->
              handleFile [] (mkInitialImport fp_name) fs fp
            Just (Left e) -> externalError $ T.pack e
            Nothing -> externalErrorS $ fp ++ ": file not found."
            where (fp_name, _) = splitExtension fp

runCompilerM :: Monad m =>
                Basis -> CompilerM m a
             -> m (E.Warnings, [(String, FileModule)], VNameSource)
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
