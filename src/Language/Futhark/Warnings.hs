{-# LANGUAGE Safe #-}

-- | A very simple representation of collections of warnings.
-- Warnings have a position (so they can be ordered), and their
-- 'Show'-instance produces a human-readable string.
module Language.Futhark.Warnings
  ( Warnings,
    anyWarnings,
    singleWarning,
    singleWarning',
  )
where

import Data.List (sortOn)
import Data.Monoid
import Futhark.Util.Console (inRed)
import Futhark.Util.Loc
import Futhark.Util.Pretty
import Language.Futhark.Core (locStr, prettyStacktrace)
import Prelude

-- | The warnings produced by the compiler.  The 'Show' instance
-- produces a human-readable description.
newtype Warnings ann = Warnings [(SrcLoc, [SrcLoc], Doc ann)]

instance Semigroup (Warnings ann) where
  Warnings ws1 <> Warnings ws2 = Warnings $ ws1 <> ws2

instance Monoid (Warnings ann) where
  mempty = Warnings mempty

instance Pretty (Warnings ann) where
  pretty (Warnings []) = mempty
  pretty (Warnings ws) =
    unAnnotate $ vsep $ punctuate line $ map onWarning $ sortOn (rep . wloc) ws
    where
      wloc (x, _, _) = locOf x
      rep NoLoc = ("", 0)
      rep (Loc p _) = (posFile p, posCoff p)
      onWarning :: (SrcLoc, [SrcLoc], Doc ann) -> Doc ann
      onWarning (loc, [], w) =
        (pretty (inRed ("Warning at " ++ locStr loc ++ ":")))
          <> line
          <> indent 2 w
      onWarning (loc, locs, w) =
        ( pretty $
            inRed
              ( "Warning at\n"
                  ++ prettyStacktrace 0 (map locStr (loc : locs))
              )
        )
          <> line
          <> indent 2 w

-- | True if there are any warnings in the set.
anyWarnings :: Warnings ann -> Bool
anyWarnings (Warnings ws) = not $ null ws

-- | A single warning at the given location.
singleWarning :: SrcLoc -> Doc ann -> Warnings ann
singleWarning loc = singleWarning' loc []

-- | A single warning at the given location, but also with a stack
-- trace (sort of) to the location.
singleWarning' :: SrcLoc -> [SrcLoc] -> Doc ann -> Warnings ann
singleWarning' loc locs problem = Warnings [(loc, locs, problem)]
