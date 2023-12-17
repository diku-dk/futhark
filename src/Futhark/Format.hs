-- | Parsing of format strings.
module Futhark.Format (parseFormatString) where

import Data.Bifunctor
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec

pFormatString :: Parsec Void T.Text [Either T.Text T.Text]
pFormatString =
  many (choice [Left <$> pLiteral, Right <$> pInterpolation]) <* eof
  where
    pInterpolation = "{" *> takeWhileP Nothing (`notElem` braces) <* "}"
    pLiteral = takeWhile1P Nothing (`notElem` braces)
    braces = "{}" :: String

-- | The Lefts are pure text; the Rights are the contents of
-- interpolations.
parseFormatString :: T.Text -> Either T.Text [Either T.Text T.Text]
parseFormatString =
  first (T.pack . errorBundlePretty) . runParser pFormatString ""
