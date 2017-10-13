{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Control.Monad.Reader
import Data.FileEmbed
import Data.List
import Data.Monoid
import qualified Data.Map as M
import System.FilePath ((<.>), takeDirectory, takeExtension)
import System.Directory (createDirectoryIfMissing)
import System.Console.GetOpt
import System.IO
import System.Exit

import Text.Blaze.Html5 (docTypeHtml, toHtml, h1)
import Text.Blaze.Html.Renderer.String

import Language.Futhark.TypeChecker (Imports, FileModule(..))
import Language.Futhark.TypeChecker.Monad
import Language.Futhark
import Futhark.Doc.Generator
import Futhark.Compiler (readLibrary, dumpError, newFutharkConfig)
import Futhark.Pipeline (runFutharkM, FutharkM)
import Futhark.Util.Options
import Futhark.Util (directoryContents)
import Language.Futhark.Futlib.Prelude

main :: IO ()
main = mainWithOptions initialDocConfig commandLineOptions f
  where f [dir] config = Just $ do
          res <- runFutharkM (m config dir) True
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
              (Prog prog, _w, imports, _vns) <-
                readLibrary False preludeBasis mempty files
              liftIO $ printDecs config outdir (nubBy sameImport imports) prog

        sameImport (x, _) (y, _) = x == y

futFiles :: FilePath -> IO [FilePath]
futFiles dir = filter isFut <$> directoryContents dir
  where isFut = (==".fut") . takeExtension

type DocEnv = M.Map (Namespace,VName) String

printDecs :: DocConfig -> FilePath -> Imports -> [Dec] -> IO ()
printDecs cfg dir imports decs = do
  let to_write = evalState (mapM (f $ render decs) imports) mempty
  mapM_ write to_write

  write ("index", renderHtml $ indexPage to_write)
  write' ("style.css", cssFile)

  where f g x = (fst x,) <$> g x
        write (name, content) = write' (name <.> "html", content)
        write' (name, content) = do let file = dir ++ "/" ++ name
                                    createDirectoryIfMissing True $ takeDirectory file
                                    when (docVerbose cfg) $
                                      hPutStrLn stderr $ "Writing " <> file
                                    writeFile file content

render :: [Dec] -> (String, FileModule) -> State DocEnv String
render decs (name,fm) = runReaderT m (name,fm)
  where putName = h1 (toHtml name)
        m = renderHtml . docTypeHtml . (putName <>) <$> renderFile decs

cssFile :: String
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
