{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A re-export of the prettyprinting library, along with some convenience functions.
module Futhark.Util.Pretty
  ( module Text.PrettyPrint.Mainland,
    module Text.PrettyPrint.Mainland.Class,
    prettyDoc,
    prettyTuple,
    prettyTupleLines,
    prettyString,
    prettyText,
    prettyTextOneLine,
    prettyOneLine,
    apply,
    oneLine,
    annot,
    nestedBlock,
    textwrap,
    shorten,
    commastack,
  )
where

import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Numeric.Half
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

-- | Prettyprint a value to a 'String', wrapped to 80 characters.
prettyString :: Pretty a => a -> String
prettyString = pretty 80 . ppr

-- | Prettyprint a value to a 'Text', wrapped to 80 characters.
prettyText :: Pretty a => a -> Text
prettyText = LT.toStrict . prettyLazyText 80 . ppr

-- | Prettyprint a value to a 'Text' without any width restriction.
prettyTextOneLine :: Pretty a => a -> Text
prettyTextOneLine = LT.toStrict . prettyLazyText 80 . oneLine . ppr

-- | Prettyprint a value without any width restriction.
prettyOneLine :: Pretty a => a -> String
prettyOneLine = ($ "") . displayS . renderCompact . oneLine . ppr

-- | Re-export of 'pretty'.
prettyDoc :: Int -> Doc -> String
prettyDoc = pretty

ppTuple' :: Pretty a => [a] -> Doc
ppTuple' ets = braces $ commasep $ map (align . ppr) ets

-- | Prettyprint a list enclosed in curly braces.
prettyTuple :: Pretty a => [a] -> String
prettyTuple = pretty 80 . ppTuple'

-- | Like 'prettyTuple', but put a linebreak after every element.
prettyTupleLines :: Pretty a => [a] -> String
prettyTupleLines = pretty 80 . ppTupleLines'
  where
    ppTupleLines' ets = braces $ stack $ punctuate comma $ map (align . ppr) ets

-- | The document @'apply' ds@ separates @ds@ with commas and encloses them with
-- parentheses.
apply :: [Doc] -> Doc
apply = parens . commasep . map align

-- | Make sure that the given document is printed on just a single line.
oneLine :: Doc -> Doc
oneLine s = text $ displayS (renderCompact s) ""

-- | Like 'text', but splits the string into words and permits line breaks between all of them.
textwrap :: String -> Doc
textwrap = folddoc (<+/>) . map text . words

-- | Stack and prepend a list of 'Doc's to another 'Doc', separated by
-- a linebreak.  If the list is empty, the second 'Doc' will be
-- returned without a preceding linebreak.
annot :: [Doc] -> Doc -> Doc
annot [] s = s
annot l s = stack l </> s

-- | Surround the given document with enclosers and add linebreaks and
-- indents.
nestedBlock :: String -> String -> Doc -> Doc
nestedBlock pre post body =
  text pre
    </> indent 2 body
    </> text post

-- | Prettyprint on a single line up to at most some appropriate
-- number of characters, with trailing ... if necessary.  Used for
-- error messages.
shorten :: Pretty a => a -> Doc
shorten a
  | length s > 70 = text (take 70 s) <> text "..."
  | otherwise = text s
  where
    s = prettyString a

-- | Like 'commasep', but a newline after every comma.
commastack :: [Doc] -> Doc
commastack = align . stack . punctuate comma

instance Pretty Half where
  ppr = text . show
