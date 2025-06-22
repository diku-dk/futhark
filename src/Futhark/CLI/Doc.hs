{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | @futhark doc@
module Futhark.CLI.Doc (main) where

import Control.Monad
import Control.Monad.State
import Data.FileEmbed
import Data.List (nubBy)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as LT
import Futhark.Compiler (Imports, dumpError, fileProg, newFutharkConfig, readProgramFiles)
import Futhark.Doc.Generator
import Futhark.Pipeline (FutharkM, Verbosity (..), runFutharkM)
import Futhark.Util (directoryContents)
import Futhark.Util.Options
import Futhark.Util.Pretty (hPutDoc)
import Language.Futhark.Semantic (mkInitialImport)
import Language.Futhark.Syntax (DocComment (..), progDoc)
import Language.Futhark.Warnings (prettyWarnings)
import System.Directory (createDirectoryIfMissing)
import System.Exit
import System.FilePath
import System.IO
import Text.Blaze.Html.Renderer.Text

cssFile :: T.Text
cssFile = $(embedStringFile "rts/futhark-doc/style.css")

data DocConfig = DocConfig
  { docOutput :: Maybe FilePath,
    docVerbose :: Bool
  }

initialDocConfig :: DocConfig
initialDocConfig =
  DocConfig
    { docOutput = Nothing,
      docVerbose = False
    }

printDecs :: DocConfig -> FilePath -> [FilePath] -> Imports -> IO ()
printDecs cfg dir files imports = do
  let direct_imports =
        map (mkInitialImport . normalise . dropExtension) files
      (file_htmls, warnings) = do
        renderFiles direct_imports $
          filter (not . ignored) imports
  hPutDoc stderr $ prettyWarnings warnings
  mapM_ (write . fmap (LT.toStrict . renderHtml)) file_htmls
  write ("style.css", cssFile)
  where
    write :: (FilePath, T.Text) -> IO ()
    write (name, content) = do
      let file = dir </> makeRelative "/" name
      when (docVerbose cfg) $ hPutStrLn stderr $ "Writing " <> file
      createDirectoryIfMissing True $ takeDirectory file
      T.writeFile file content

    -- Some files are not worth documenting; typically because
    -- they contain tests.  The current crude mechanism is to
    -- recognise them by a file comment containing "ignore".
    ignored (_, fm) =
      case progDoc (fileProg fm) of
        Just (DocComment s _) -> T.strip s == "ignore"
        _ -> False

type DocOption = OptDescr (Either (IO ()) (DocConfig -> DocConfig))

commandLineOptions :: [DocOption]
commandLineOptions =
  [ Option
      "o"
      ["output-directory"]
      ( ReqArg
          (\dirname -> Right $ \config -> config {docOutput = Just dirname})
          "DIR"
      )
      "Directory in which to put generated documentation.",
    Option
      "v"
      ["verbose"]
      (NoArg $ Right $ \config -> config {docVerbose = True})
      "Print status messages on stderr."
  ]

futFiles :: FilePath -> IO [FilePath]
futFiles dir = filter isFut <$> directoryContents dir
  where
    isFut = (== ".fut") . takeExtension

-- | Run @futhark doc@.
main :: String -> [String] -> IO ()
main = mainWithOptions initialDocConfig commandLineOptions "options... -o outdir programs..." f
  where
    f [dir] config = Just $ do
      res <- runFutharkM (m config dir) Verbose
      case res of
        Left err -> liftIO $ do
          dumpError newFutharkConfig err
          exitWith $ ExitFailure 2
        Right () ->
          pure ()
    f _ _ = Nothing

    m :: DocConfig -> FilePath -> FutharkM ()
    m config dir =
      case docOutput config of
        Nothing -> liftIO $ do
          hPutStrLn stderr "Must specify output directory with -o."
          exitWith $ ExitFailure 1
        Just outdir -> do
          files <- liftIO $ futFiles dir
          when (docVerbose config) $
            liftIO $ do
              mapM_ (hPutStrLn stderr . ("Found source file " <>)) files
              hPutStrLn stderr "Reading files..."
          (_w, imports, _vns) <- readProgramFiles [] files
          liftIO $ printDecs config outdir files $ nubBy sameImport imports

    sameImport (x, _) (y, _) = x == y
