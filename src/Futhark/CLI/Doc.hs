{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Futhark.CLI.Doc (main) where

import Control.Monad.State
import Data.FileEmbed
import Data.List
import System.FilePath
import System.Directory (createDirectoryIfMissing)
import System.Console.GetOpt
import System.IO
import System.Exit
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Text.Blaze.Html.Renderer.Text

import Futhark.Doc.Generator
import Futhark.Compiler (readLibrary, dumpError, newFutharkConfig, Imports, fileProg)
import Futhark.Pipeline (runFutharkM, FutharkM, Verbosity(..))
import Language.Futhark.Syntax (progDoc, DocComment(..))
import Futhark.Util.Options
import Futhark.Util (directoryContents, trim)

main :: String -> [String] -> IO ()
main = mainWithOptions initialDocConfig commandLineOptions "options... -o outdir programs..." f
  where f [dir] config = Just $ do
          res <- runFutharkM (m config dir) Verbose
          case res of
            Left err -> liftIO $ do
              dumpError newFutharkConfig err
              exitWith $ ExitFailure 2
            Right () ->
              return ()
        f _ _ = Nothing

        m :: DocConfig -> FilePath -> FutharkM ()
        m config dir =
          case docOutput config of
            Nothing -> liftIO $ do
              hPutStrLn stderr "Must specify output directory with -o."
              exitWith $ ExitFailure 1
            Just outdir -> do
              files <- liftIO $ futFiles dir
              when (docVerbose config) $ liftIO $ do
                mapM_ (hPutStrLn stderr . ("Found source file "<>)) files
                hPutStrLn stderr "Reading files..."
              (_w, imports, _vns) <- readLibrary files
              liftIO $ printDecs config outdir files $ nubBy sameImport imports

        sameImport (x, _) (y, _) = x == y

futFiles :: FilePath -> IO [FilePath]
futFiles dir = filter isFut <$> directoryContents dir
  where isFut = (==".fut") . takeExtension

printDecs :: DocConfig -> FilePath -> [FilePath] -> Imports -> IO ()
printDecs cfg dir files imports = do
  let direct_imports = map (normalise . dropExtension) files
      (file_htmls, _warnings) = renderFiles direct_imports $
                                filter (not . ignored) imports
  mapM_ (write . fmap renderHtml) file_htmls
  write ("style.css", cssFile)

  where write :: (String, T.Text) -> IO ()
        write (name, content) = do let file = dir </> makeRelative "/" name
                                   when (docVerbose cfg) $
                                     hPutStrLn stderr $ "Writing " <> file
                                   createDirectoryIfMissing True $ takeDirectory file
                                   T.writeFile file content

        -- Some files are not worth documenting; typically because
        -- they contain tests.  The current crude mechanism is to
        -- recognise them by a file comment containing "ignore".
        ignored (_, fm) =
          case progDoc (fileProg fm) of
            Just (DocComment s _) -> trim s == "ignore"
            _                     -> False

cssFile :: T.Text
cssFile = $(embedStringFile "rts/futhark-doc/style.css")

data DocConfig = DocConfig { docOutput :: Maybe FilePath
                           , docVerbose :: Bool
                           }

initialDocConfig :: DocConfig
initialDocConfig = DocConfig { docOutput = Nothing
                             , docVerbose = False
                             }

type DocOption = OptDescr (Either (IO ()) (DocConfig -> DocConfig))

commandLineOptions :: [DocOption]
commandLineOptions = [ Option "o" ["output-directory"]
                       (ReqArg (\dirname -> Right $ \config -> config { docOutput = Just dirname })
                       "DIR")
                       "Directory in which to put generated documentation."
                     , Option "v" ["verbose"]
                       (NoArg $ Right $ \config -> config { docVerbose = True })
                       "Print status messages on stderr."
                     ]
