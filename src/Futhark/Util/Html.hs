{-# LANGUAGE TemplateHaskell #-}

module Futhark.Util.Html
  ( relativise,
    headHtmlWithCss,
    headHtml,
    cssFile,
  )
where

import Data.FileEmbed (embedStringFile)
import Data.String (fromString)
import Data.Text qualified as T
import System.FilePath (makeRelative, splitPath)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

cssFile :: T.Text
cssFile = $(embedStringFile "rts/style.css")

relativise :: FilePath -> FilePath -> FilePath
relativise dest src =
  concat (replicate (length (splitPath src) - 1) "../") ++ makeRelative "/" dest

headHtmlWithCss :: String -> String -> H.Html
headHtmlWithCss cssPath titleText =
  H.head $
    H.meta
      ! A.charset "utf-8"
      <> H.title (fromString titleText)
      <> H.link
        ! A.href (fromString cssPath)
        ! A.rel "stylesheet"
        ! A.type_ "text/css"

headHtml :: FilePath -> String -> H.Html
headHtml current = headHtmlWithCss (relativise "style.css" current)
