-- | Basic table building for prettier futhark-test output.
module Futhark.Util.Table
  ( hPutTable,
    mkEntry,
    Entry,
    AnsiStyle,
    Color (..),
    color,
  )
where

import Data.List (intersperse, transpose)
import Futhark.Util (maxinum)
import Futhark.Util.Pretty hiding (sep, width)
import System.IO (Handle)

data RowTemplate = RowTemplate [Int] Int deriving (Show)

-- | A table entry. Consists of the content as well as how it should
-- be styled..
data Entry = Entry {entryText :: String, _entryStyle :: AnsiStyle}

-- | Makes a table entry.
mkEntry :: String -> AnsiStyle -> Entry
mkEntry = Entry

buildRowTemplate :: [[Entry]] -> Int -> RowTemplate
buildRowTemplate rows = RowTemplate widths
  where
    widths = map (maxinum . map (length . entryText)) . transpose $ rows

buildRow :: RowTemplate -> [Entry] -> Doc AnsiStyle
buildRow (RowTemplate widths pad) entries = cells <> hardline
  where
    bar = "\x2502"
    cells = mconcat (zipWith buildCell entries widths) <> bar
    buildCell (Entry entry sgr) width =
      let padding = width - length entry + pad
       in bar <> " " <> annotate sgr (pretty entry) <> mconcat (replicate padding " ")

buildSep :: Char -> Char -> Char -> RowTemplate -> Doc AnsiStyle
buildSep lCorner rCorner sep (RowTemplate widths pad) =
  corners . concatMap cellFloor $ widths
  where
    cellFloor width = replicate (width + pad + 1) '\x2500' <> [sep]
    corners [] = ""
    corners s = pretty lCorner <> pretty (init s) <> pretty rCorner

-- | Produce a table from a list of entries and a padding amount that
-- determines padding from the right side of the widest entry in each column.
hPutTable :: Handle -> [[Entry]] -> Int -> IO ()
hPutTable h rows pad = hPutDoc h $ buildTop template <> sepRows <> buildBottom template <> hardline
  where
    sepRows = mconcat $ intersperse (buildFloor template) builtRows
    builtRows = map (buildRow template) rows
    template = buildRowTemplate rows pad
    buildTop rt = buildSep '\x250C' '\x2510' '\x252C' rt <> hardline
    buildFloor rt = buildSep '\x251C' '\x2524' '\x253C' rt <> hardline
    buildBottom = buildSep '\x2514' '\x2518' '\x2534'
