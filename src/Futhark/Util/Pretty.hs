{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A re-export of the prettyprinting library, along with some convenience functions.
module Futhark.Util.Pretty
  ( module Prettyprinter,
    prettyDoc,
    prettyTuple,
    prettyText,
    prettyOneLine,
    apply,
    oneLine,
    annot,
    nestedBlock,
    textwrap,
    shorten,
  )
where

import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Prettyprinter
import qualified Prettyprinter as PP

-- | Prettyprint a value, wrapped to 80 characters.
-- pretty :: Pretty a => a -> String
-- pretty = show . PP.pretty

-- | Prettyprint a value to a 'Text', wrapped to 80 characters.
prettyText :: Pretty a => a -> Text
prettyText = LT.toStrict . LT.pack . show . PP.pretty

-- | Prettyprint a value without any width restriction.
prettyOneLine :: Pretty a => a -> String
prettyOneLine = show . oneLine . PP.pretty

-- | Re-export of 'PP.pretty'.
prettyDoc :: Show ann => Int -> Doc ann -> String
prettyDoc n = show . layoutPretty (LayoutOptions {layoutPageWidth = AvailablePerLine n 1.0})

ppTuple' :: Pretty a => [a] -> Doc ann
ppTuple' = encloseSep lbrace rbrace comma . fmap PP.pretty

-- | Prettyprint a list enclosed in curly braces.
prettyTuple :: Pretty a => [a] -> String
prettyTuple = show . ppTuple'

-- | The document @'apply' ds@ separates @ds@ with commas and encloses them with
-- parentheses.
apply :: [Doc ann] -> Doc ann
apply = encloseSep lparen rparen comma

-- | Make sure that the given document is printed on just a single line.
oneLine :: PP.Doc ann -> PP.Doc ann
oneLine = group -- This will not work if the line is too long to fit...

-- | Like 'text', but splits the string into words and permits line breaks between all of them.
textwrap :: String -> Doc ann
textwrap = fillSep . fmap PP.pretty . words

-- | Stack and prepend a list of 'Doc's to another 'Doc', separated by
-- a linebreak.  If the list is empty, the second 'Doc' will be
-- returned without a preceding linebreak.
annot :: [Doc ann] -> Doc ann -> Doc ann
annot [] s = s
annot l s = vsep l <> line <> s

-- | Surround the given document with enclosers and add linebreaks and
-- indents.
nestedBlock :: String -> String -> Doc ann -> Doc ann
nestedBlock pre post body =
  vsep
    [ PP.pretty pre,
      body,
      PP.pretty post
    ]

-- | Prettyprint on a single line up to at most some appropriate
-- number of characters, with trailing ... if necessary.  Used for
-- error messages.
shorten :: Pretty a => a -> Doc ann
shorten a
  | length s > 70 = PP.pretty (take 70 s) <> PP.pretty "..."
  | otherwise = PP.pretty s
  where
    s = show $ PP.pretty a
