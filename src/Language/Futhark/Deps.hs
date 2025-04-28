-- | Finds instances of irregular nested data-parallelism (hopefully)
module Language.Futhark.Deps
  ( printDeps
  )
where

import Control.Monad
import Control.Monad.State
import Data.List (find)
import Data.Map qualified as M
import Futhark.Util.Loc (Loc (..), Pos (..))
import Language.Futhark
import Language.Futhark.Semantic
import Language.Futhark.Traversals
import System.FilePath.Posix qualified as Posix

printDeps :: String -> String
printDeps file = "Hey, you!"
