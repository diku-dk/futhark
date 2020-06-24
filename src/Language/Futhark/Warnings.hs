{-# LANGUAGE Safe #-}
-- | A very simple representation of collections of warnings.
-- Warnings have a position (so they can be ordered), and their
-- 'Show'-instance produces a human-readable string.
module Language.Futhark.Warnings
  ( Warnings
  , singleWarning
  , singleWarning'
  ) where

import Data.Monoid
import Data.List (sortOn, intercalate)

import Prelude

import Language.Futhark.Core (locStr, prettyStacktrace)
import Futhark.Util.Console (inRed)
import Futhark.Util.Loc

-- | The warnings produced by the compiler.  The 'Show' instance
-- produces a human-readable description.
newtype Warnings = Warnings [(SrcLoc, [SrcLoc], String)] deriving (Eq)

instance Semigroup Warnings where
  Warnings ws1 <> Warnings ws2 = Warnings $ ws1 <> ws2

instance Monoid Warnings where
  mempty = Warnings mempty

instance Show Warnings where
  show (Warnings []) = ""
  show (Warnings ws) =
    intercalate "\n\n" ws' ++ "\n"
    where ws' = map showWarning $ sortOn (rep . wloc) ws
          wloc (x, _, _) = locOf x
          rep NoLoc = ("", 0)
          rep (Loc p _) = (posFile p, posCoff p)
          showWarning (loc, [], w) =
            inRed ("Warning at " ++ locStr loc ++ ":") ++ "\n" ++
            intercalate "\n" (map ("  "<>) $ lines w)
          showWarning (loc, locs, w) =
            inRed ("Warning at\n" ++
                   prettyStacktrace 0 (map locStr (loc:locs))) ++
            intercalate "\n" (map ("  "<>) $ lines w)

-- | A single warning at the given location.
singleWarning :: SrcLoc -> String -> Warnings
singleWarning loc = singleWarning' loc []

-- | A single warning at the given location, but also with a stack
-- trace (sort of) to the location.
singleWarning' :: SrcLoc -> [SrcLoc] -> String -> Warnings
singleWarning' loc locs problem = Warnings [(loc, locs, problem)]
