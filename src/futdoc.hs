{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Control.Monad.Reader
import Data.Monoid
import qualified Data.Map as M
import System.FilePath ((-<.>), takeDirectory)
import System.Directory (createDirectoryIfMissing)

import Futhark.Compiler (readProgram)
import Futhark.Util.Options
import Futhark.Pipeline (runFutharkM, FutharkM)
import Language.Futhark.TypeChecker (Imports, FileModule(..))
import Language.Futhark.TypeChecker.Monad
import Language.Futhark

import Text.Blaze.Html5 (docTypeHtml, toHtml, h1)
import Text.Blaze.Html.Renderer.String

import Documentation.Generator

main :: IO ()
main = mainWithOptions () [] f
  where f [file] _ = Just $ (runFutharkM (m file)  True >> return ())
        f _ _ = Nothing
        m ::FilePath -> Futhark.Pipeline.FutharkM ()
        m file = do
          (Prog prog, _w, imports, _vns) <- readProgram file
          liftIO (printDecs imports prog)

type DocEnv = M.Map (Namespace,VName) (String)

printDecs :: Imports -> [Dec] -> IO ()
printDecs imports decs = mapM_ write . run $ mapM (f $ render decs) (init (M.toList imports))
  where run s = evalState s M.empty
        f g x = (fst x,) <$> g x
        write (name, content) = do let file = "doc/" ++ name -<.> "html"
                                   createDirectoryIfMissing True (takeDirectory file)
                                   writeFile file content

render :: [Dec] -> (String, FileModule) -> State DocEnv String
render decs (name,fm) = runReaderT m (name,fm)
  where putName = h1 (toHtml name)
        m = renderHtml . docTypeHtml . (putName <>) <$> renderDecs decs

