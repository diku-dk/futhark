{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Data.FileEmbed
import Data.List
import Data.Semigroup ((<>))
import System.FilePath ((</>), (<.>), takeDirectory, takeExtension, makeRelative)
import System.Directory (createDirectoryIfMissing)
import System.Console.GetOpt
import System.IO
import System.Exit
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Text.Blaze.Html.Renderer.Text

import Futhark.Doc.Generator
import Futhark.Compiler (readLibrary, dumpError, newFutharkConfig, Imports)
import Futhark.Pipeline (runFutharkM, FutharkM, Verbosity(..))
import Futhark.Util.Options
import Futhark.Util (directoryContents)
import Language.Futhark.Futlib.Prelude

main :: IO ()
main = mainWithOptions initialDocConfig commandLineOptions f
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
              (_w, imports, _vns) <-
                readLibrary preludeBasis mempty files
              liftIO $ printDecs config outdir $ nubBy sameImport imports

        sameImport (x, _) (y, _) = x == y

futFiles :: FilePath -> IO [FilePath]
futFiles dir = filter isFut <$> directoryContents dir
  where isFut = (==".fut") . takeExtension

printDecs :: DocConfig -> FilePath -> Imports -> IO ()
printDecs cfg dir imports = do
  let to_write = map (fmap renderHtml) $ renderFiles imports
  mapM_ write to_write

  write ("index", renderHtml $ indexPage to_write)
  write' ("style.css", cssFile)

  where write (name, content) = write' (name <.> "html", content)

        write' :: (String, T.Text) -> IO ()
        write' (name, content) = do let file = dir </> makeRelative "/" name
                                    when (docVerbose cfg) $
                                      hPutStrLn stderr $ "Writing " <> file
                                    createDirectoryIfMissing True $ takeDirectory file
                                    T.writeFile file content

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
