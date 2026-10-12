-- | A Safe Haskell-trusted re-export of the @srcloc@ package.
module Futhark.Util.Loc (module Data.Loc, contains) where

import Data.Loc

-- | Does the location of the given thing contain the given position?
-- The end of a 'Loc' is exclusive: it is the position just after the
-- last character.
contains :: (Located a) => a -> Pos -> Bool
contains a pos =
  case locOf a of
    Loc start end -> pos >= start && pos < end
    NoLoc -> False
