-- | Basic table building for prettier futhark-test output.
module Futhark.Util.Table
     ( buildTable
     , mkEntry
     , Entry
     ) where

import Data.List
import System.Console.ANSI

data RowTemplate = RowTemplate [Int] Int deriving (Show)

-- | A table entry. Consists of the content as well a list of
-- SGR commands to color/stylelize the entry.
type Entry = (String, [SGR])

-- | Makes a table entry with the default SGR mode.
mkEntry :: String -> (String, [SGR])
mkEntry s = (s, [])

color :: [SGR] -> String -> String
color sgr s = setSGRCode sgr ++ s ++ setSGRCode [Reset]

buildRowTemplate :: [[Entry]] -> Int -> RowTemplate

buildRowTemplate rows = RowTemplate widths
  where widths = map (maximum . map (length . fst)) . transpose $ rows

buildRow :: RowTemplate -> [Entry] -> String
buildRow (RowTemplate widths pad) entries = cells ++ "\n"
  where bar   = "\x2502"
        cells = concatMap buildCell (zip entries widths) ++ bar
        buildCell ((entry, sgr), width) =
          let padding = width - length entry + pad
          in  bar ++ " " ++ color sgr entry ++ replicate padding ' '

buildSep :: Char -> Char -> Char -> RowTemplate -> String
buildSep lCorner rCorner sep (RowTemplate widths pad) =
  corners . concatMap cellFloor $ widths
  where cellFloor width = replicate (width + pad + 1) '\x2500' ++ [sep]
        corners [] = ""
        corners s  = [lCorner] ++ init s ++ [rCorner]

-- | Builds a table from a list of entries and a padding amount that
-- determines padding from the right side of the widest entry in each column.
buildTable :: [[Entry]] -> Int -> String
buildTable rows pad = buildTop template ++ sepRows ++ buildBottom template
  where sepRows       = intercalate (buildFloor template) builtRows
        builtRows     = map (buildRow template) rows
        template      = buildRowTemplate rows pad
        buildTop rt   = buildSep '\x250C' '\x2510' '\x252C' rt ++ "\n"
        buildFloor rt = buildSep '\x251C' '\x2524' '\x253C' rt ++ "\n"
        buildBottom   = buildSep '\x2514' '\x2518' '\x2534'
