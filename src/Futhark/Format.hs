-- | Parsing of format strings.
module Futhark.Format (parseFormatString) where

import Data.Text qualified as T

parseFormatString :: T.Text -> Either T.Text ([T.Text], [T.Text])
parseFormatString printStr
  | not balanced =
      Left
        "Invalid format string, possibly due to mismatched braces."
  | otherwise = Right (strs, exps)
  where
    firstSplit = T.split (== '{') printStr
    splits = map (T.split (== '}')) $ tail firstSplit
    balanced =
      (T.count "{" printStr == T.count "}" printStr)
        && all (\x -> length x == 2) splits
    strs = head firstSplit : map last splits
    exps = map head splits
