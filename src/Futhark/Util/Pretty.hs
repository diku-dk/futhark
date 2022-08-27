{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A re-export of the prettyprinting library, along with some
-- convenience functions.
module Futhark.Util.Pretty
  ( -- * Rendering to texts
    prettyTuple,
    prettyTupleLines,
    prettyString,
    prettyText,
    prettyTextOneLine,
    docText,

    -- * Rendering to terminal
    putDoc,
    hPutDoc,

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
    semisep,
    stack,
    parensIf,
    ppTuple',

    -- * Operators
    (</>),
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Numeric.Half
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bgColor, bold, color)
import qualified Prettyprinter.Render.Terminal
import qualified Prettyprinter.Render.Text
import Prettyprinter.Symbols.Ascii
import System.IO (Handle, hIsTerminalDevice, stdout)

-- | Like 'hPutDoc', but to stdout.
putDoc :: Doc AnsiStyle -> IO ()
putDoc = hPutDoc stdout

-- | Print a doc with styling to the given file; stripping colors if
-- the file does not seem to support such things.
hPutDoc :: Handle -> Doc AnsiStyle -> IO ()
hPutDoc h d = do
  colours <- hIsTerminalDevice h
  if colours
    then Prettyprinter.Render.Terminal.hPutDoc h d
    else Prettyprinter.Render.Text.hPutDoc h d

-- | Prettyprint a value to a 'String', appropriately wrapped.
prettyString :: Pretty a => a -> String
prettyString = T.unpack . prettyText

-- | Prettyprint a value to a 'Text', appropriately wrapped.
prettyText :: Pretty a => a -> Text
prettyText = docText . pretty

-- | Convert a 'Doc' to text.  Thsi ignores any annotations (i.e. it
-- will be non-coloured output).
docText :: Doc a -> T.Text
docText = Prettyprinter.Render.Text.renderStrict . layoutSmart defaultLayoutOptions

-- | Prettyprint a value to a 'Text' on a single line.
prettyTextOneLine :: Pretty a => a -> Text
prettyTextOneLine = Prettyprinter.Render.Text.renderStrict . layoutSmart oneLineLayout . group . pretty
  where
    oneLineLayout = defaultLayoutOptions {layoutPageWidth = Unbounded}

ppTuple' :: [Doc a] -> Doc a
ppTuple' ets = braces $ commasep $ map align ets

-- | Prettyprint a list enclosed in curly braces.
prettyTuple :: Pretty a => [a] -> Text
prettyTuple = docText . ppTuple' . map pretty

-- | Like 'prettyTuple', but put a linebreak after every element.
prettyTupleLines :: Pretty a => [a] -> Text
prettyTupleLines = docText . ppTupleLines'
  where
    ppTupleLines' = braces . hsep . punctuate comma . map (align . pretty)

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

-- | Separate with commas.
commasep :: [Doc a] -> Doc a
commasep = hsep . punctuate comma

-- | Separate with semicolons.
semisep :: [Doc a] -> Doc a
semisep = hsep . punctuate semi

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
