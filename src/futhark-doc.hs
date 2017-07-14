{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Control.Monad.Reader
import Data.FileEmbed
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
import Futhark.Compiler (readProgram, dumpError, newFutharkConfig)
import Futhark.Pipeline (runFutharkM, FutharkM)
import Futhark.Util.Options
import Futhark.Util (directoryContents)

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
              (Prog prog, _w, imports, _vns) <- readProgram files
              liftIO $ printDecs outdir imports prog

futFiles :: FilePath -> IO [FilePath]
futFiles dir = filter isFut <$> directoryContents dir
  where isFut = (==".fut") . takeExtension

type DocEnv = M.Map (Namespace,VName) String

printDecs :: FilePath -> Imports -> [Dec] -> IO ()
printDecs dir imports decs = do
  let to_write = evalState (mapM (f $ render decs) imports) mempty
  mapM_ write to_write

  write ("index", renderHtml $ indexPage to_write)
  write' ("style.css", cssFile)

  where f g x = (fst x,) <$> g x
        write (name, content) = write' (name <.> "html", content)
        write' (name, content) = do let file = dir ++ "/" ++ name
                                    createDirectoryIfMissing True $ takeDirectory file
                                    writeFile file content

render :: [Dec] -> (String, FileModule) -> State DocEnv String
render decs (name,fm) = runReaderT m (name,fm)
  where putName = h1 (toHtml name)
        m = renderHtml . docTypeHtml . (putName <>) <$> renderFile decs

cssFile :: String
cssFile = $(embedStringFile "rts/futhark-doc/style.css")

newtype DocConfig = DocConfig {
  docOutput :: Maybe FilePath
  }

initialDocConfig :: DocConfig
initialDocConfig = DocConfig { docOutput = Nothing }

type DocOption = OptDescr (Either (IO ()) (DocConfig -> DocConfig))

commandLineOptions :: [DocOption]
commandLineOptions = [ Option "o" ["output-directory"]
                       (ReqArg (\dirname -> Right $ \config -> config { docOutput = Just dirname })
                       "DIR")
                       "Directory in which to put generated documentation."
                     ]
