{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A re-export of the prettyprinting library, along with some
-- convenience functions.
module Futhark.Util.Pretty
  ( -- * Rendering to texts
    prettyTuple,
    prettyTupleLines,
    prettyString,
    prettyStringOneLine,
    prettyText,
    prettyTextOneLine,
    docText,
    docTextForHandle,
    docString,

    -- * Rendering to terminal
    putDoc,
    hPutDoc,
    putDocLn,
    hPutDocLn,

    -- * Building blocks
    module Prettyprinter,
    module Prettyprinter.Symbols.Ascii,
    module Prettyprinter.Render.Terminal,
    apply,
    oneLine,
    annot,
    nestedBlock,
    textwrap,
    shorten,
    commastack,
    commasep,
    semistack,
    stack,
    parensIf,
    ppTuple',
    ppTupleLines',

    -- * Operators
    (</>),
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Numeric.Half
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bgColor, bgColorDull, bold, color, colorDull, italicized, underlined)
import Prettyprinter.Render.Terminal qualified
import Prettyprinter.Render.Text qualified
import Prettyprinter.Symbols.Ascii
import System.IO (Handle, hIsTerminalDevice, hPutStrLn, stdout)

-- | Print a doc with styling to the given file; stripping colors if
-- the file does not seem to support such things.
hPutDoc :: Handle -> Doc AnsiStyle -> IO ()
hPutDoc h d = do
  colours <- hIsTerminalDevice h
  if colours
    then Prettyprinter.Render.Terminal.renderIO h (layouter d)
    else Prettyprinter.Render.Text.hPutDoc h d
  where
    layouter =
      layoutSmart defaultLayoutOptions {layoutPageWidth = Unbounded}

-- | Like 'hPutDoc', but with a final newline.
hPutDocLn :: Handle -> Doc AnsiStyle -> IO ()
hPutDocLn h d = do
  hPutDoc h d
  hPutStrLn h ""

-- | Like 'hPutDoc', but to stdout.
putDoc :: Doc AnsiStyle -> IO ()
putDoc = hPutDoc stdout

-- | Like 'putDoc', but with a final newline.
putDocLn :: Doc AnsiStyle -> IO ()
putDocLn d = do
  putDoc d
  putStrLn ""

-- | Produce text suitable for printing on the given handle.  This
-- mostly means stripping any control characters if the handle is not
-- a terminal.
docTextForHandle :: Handle -> Doc AnsiStyle -> IO T.Text
docTextForHandle h d = do
  colours <- hIsTerminalDevice h
  let sds = layoutSmart defaultLayoutOptions d
  pure $
    if colours
      then Prettyprinter.Render.Terminal.renderStrict sds
      else Prettyprinter.Render.Text.renderStrict sds

-- | Prettyprint a value to a 'String', appropriately wrapped.
prettyString :: (Pretty a) => a -> String
prettyString = T.unpack . prettyText

-- | Prettyprint a value to a 'String' on a single line.
prettyStringOneLine :: (Pretty a) => a -> String
prettyStringOneLine = T.unpack . prettyTextOneLine

-- | Prettyprint a value to a 'Text', appropriately wrapped.
prettyText :: (Pretty a) => a -> Text
prettyText = docText . pretty

-- | Convert a 'Doc' to text.  This ignores any annotations (i.e. it
-- will be non-coloured output).
docText :: Doc a -> T.Text
docText = Prettyprinter.Render.Text.renderStrict . layouter
  where
    layouter =
      layoutSmart defaultLayoutOptions {layoutPageWidth = Unbounded}

-- | Convert a 'Doc' to a 'String', through 'docText'. Intended for
-- debugging.
docString :: Doc a -> String
docString = T.unpack . docText

-- | Prettyprint a value to a 'Text' on a single line.
prettyTextOneLine :: (Pretty a) => a -> Text
prettyTextOneLine = Prettyprinter.Render.Text.renderStrict . layoutSmart oneLineLayout . group . pretty
  where
    oneLineLayout = defaultLayoutOptions {layoutPageWidth = Unbounded}

ppTuple' :: [Doc a] -> Doc a
ppTuple' ets = braces $ commasep $ map align ets

ppTupleLines' :: [Doc a] -> Doc a
ppTupleLines' ets = braces $ commastack $ map align ets

-- | Prettyprint a list enclosed in curly braces.
prettyTuple :: (Pretty a) => [a] -> Text
prettyTuple = docText . ppTuple' . map pretty

-- | Like 'prettyTuple', but put a linebreak after every element.
prettyTupleLines :: (Pretty a) => [a] -> Text
prettyTupleLines = docText . ppTupleLines' . map pretty

-- | The document @'apply' ds@ separates @ds@ with commas and encloses them with
-- parentheses.
apply :: [Doc a] -> Doc a
apply = parens . align . commasep . map align

-- | Make sure that the given document is printed on just a single line.
oneLine :: Doc a -> Doc a
oneLine = group

-- | Splits the string into words and permits line breaks between all
-- of them.
textwrap :: T.Text -> Doc a
textwrap = fillSep . map pretty . T.words

-- | Stack and prepend a list of 'Doc's to another 'Doc', separated by
-- a linebreak.  If the list is empty, the second 'Doc' will be
-- returned without a preceding linebreak.
annot :: [Doc a] -> Doc a -> Doc a
annot [] s = s
annot l s = vsep (l ++ [s])

-- | Surround the given document with enclosers and add linebreaks and
-- indents.
nestedBlock :: Doc a -> Doc a -> Doc a -> Doc a
nestedBlock pre post body = vsep [pre, indent 2 body, post]

-- | Prettyprint on a single line up to at most some appropriate
-- number of characters, with trailing ... if necessary.  Used for
-- error messages.
shorten :: Doc a -> Doc b
shorten a
  | T.length s > 70 = pretty (T.take 70 s) <> "..."
  | otherwise = pretty s
  where
    s = Prettyprinter.Render.Text.renderStrict $ layoutCompact a

-- | Like 'commasep', but a newline after every comma.
commastack :: [Doc a] -> Doc a
commastack = align . vsep . punctuate comma

-- | Separate with semicolons and newlines.
semistack :: [Doc a] -> Doc a
semistack = align . vsep . punctuate semi

-- | Separate with commas.
commasep :: [Doc a] -> Doc a
commasep = hsep . punctuate comma

-- | Separate with linebreaks.
stack :: [Doc a] -> Doc a
stack = align . mconcat . punctuate line

-- | The document @'parensIf' p d@ encloses the document @d@ in parenthesis if
-- @p@ is @True@, and otherwise yields just @d@.
parensIf :: Bool -> Doc a -> Doc a
parensIf True doc = parens doc
parensIf False doc = doc

instance Pretty Half where
  pretty = viaShow

(</>) :: Doc a -> Doc a -> Doc a
a </> b = a <> line <> b
