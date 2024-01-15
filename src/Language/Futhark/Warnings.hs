-- | A very simple representation of collections of warnings.
-- Warnings have a position (so they can be ordered), and their
-- 'Pretty'-instance produces a human-readable string.
module Language.Futhark.Warnings
  ( Warnings,
    anyWarnings,
    singleWarning,
    singleWarning',
    listWarnings,
    prettyWarnings,
  )
where

import Data.List (sortOn)
import Data.Monoid
import Futhark.Util.Loc
import Futhark.Util.Pretty
import Language.Futhark.Core (locText, prettyStacktrace)
import Prelude

-- | The warnings produced by the compiler.  The 'Show' instance
-- produces a human-readable description.
newtype Warnings = Warnings [(SrcLoc, [SrcLoc], Doc ())]

instance Semigroup Warnings where
  Warnings ws1 <> Warnings ws2 = Warnings $ ws1 <> ws2

instance Monoid Warnings where
  mempty = Warnings mempty

-- | Prettyprint warnings, making use of colours and such.
prettyWarnings :: Warnings -> Doc AnsiStyle
prettyWarnings (Warnings []) = mempty
prettyWarnings (Warnings ws) =
  stack $ map ((<> hardline) . onWarning) $ sortOn (rep . wloc) ws
  where
    wloc (x, _, _) = locOf x
    rep NoLoc = ("", 0)
    rep (Loc p _) = (posFile p, posCoff p)
    onWarning (loc, [], w) =
      annotate
        (color Yellow)
        ("Warning at" <+> pretty (locText loc) <> ":")
        </> indent 2 (unAnnotate w)
    onWarning (loc, locs, w) =
      annotate
        (color Yellow)
        ("Warning at" </> pretty (prettyStacktrace 0 (map locText (loc : locs))))
        </> indent 2 (unAnnotate w)

-- | True if there are any warnings in the set.
anyWarnings :: Warnings -> Bool
anyWarnings (Warnings ws) = not $ null ws

-- | A single warning at the given location.
singleWarning :: SrcLoc -> Doc () -> Warnings
singleWarning loc = singleWarning' loc []

-- | A single warning at the given location, but also with a stack
-- trace (sort of) to the location.
singleWarning' :: SrcLoc -> [SrcLoc] -> Doc () -> Warnings
singleWarning' loc locs problem = Warnings [(loc, locs, problem)]

-- | Exports Warnings into a list of (location, problem).
listWarnings :: Warnings -> [(SrcLoc, Doc ())]
listWarnings (Warnings ws) = map (\(loc, _, doc) -> (loc, doc)) ws
