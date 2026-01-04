module Futhark.Util.Html (relativise, headHtml) where

import Data.String (fromString)
import System.FilePath (makeRelative, splitPath)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

relativise :: FilePath -> FilePath -> FilePath
relativise dest src =
  concat (replicate (length (splitPath src) - 1) "../") ++ makeRelative "/" dest

headHtml :: FilePath -> String -> H.Html
headHtml current titleText =
  H.head $
    H.meta
      ! A.charset "utf-8"
      <> H.title (fromString titleText)
      <> H.link
        ! A.href (fromString $ relativise "style.css" current)
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
