{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Control.Monad.Reader
import Data.Monoid
import qualified Data.Map as M
import System.FilePath ((-<.>), takeDirectory)
import System.Directory (createDirectoryIfMissing)
import System.Console.GetOpt
import System.IO
import System.Exit

import Text.Blaze.Html5 (docTypeHtml, toHtml, h1)
import Text.Blaze.Html.Renderer.String

import Language.Futhark.TypeChecker (Imports, FileModule(..))
import Language.Futhark.TypeChecker.Monad
import Language.Futhark
import Futhark.Util.Options
import Futhark.Doc.Generator
import Futhark.Compiler (readProgram)
import Futhark.Pipeline (runFutharkM, FutharkM)

main :: IO ()
main = mainWithOptions initialDocConfig commandLineOptions f
  where f [file] config = Just $ void (runFutharkM (m config file) True)
        f _ _ = Nothing
        m :: DocConfig -> FilePath -> Futhark.Pipeline.FutharkM ()
        m config file =
          case docOutput config of
            Nothing -> liftIO $ do
              hPutStrLn stderr "Must specify output directory with -o."
              exitWith $ ExitFailure 1
            Just dir -> do
              (Prog prog, _w, imports, _vns) <- readProgram file
              liftIO $ printDecs dir imports prog

type DocEnv = M.Map (Namespace,VName) String

printDecs :: FilePath -> Imports -> [Dec] -> IO ()
printDecs dir imports decs = mapM_ write . run $ mapM (f $ render decs) (init (M.toList imports))
  where run s = evalState s M.empty
        f g x = (fst x,) <$> g x
        write (name, content) = do let file = dir ++ "/" ++ name -<.> "html"
                                   createDirectoryIfMissing True $ takeDirectory file
                                   writeFile file content

render :: [Dec] -> (String, FileModule) -> State DocEnv String
render decs (name,fm) = runReaderT m (name,fm)
  where putName = h1 (toHtml name)
        m = renderHtml . docTypeHtml . (putName <>) <$> renderDecs decs

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
