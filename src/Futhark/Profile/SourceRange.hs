module Futhark.Profile.SourceRange (SourceRange (..), startLineCol, endLineCol, filter123, overlapsWith, mergeSemigroup, parse) where

import Control.Arrow ((&&&))
import Control.Monad (void, when)
import Data.Bifunctor (first)
import Data.Loc (Pos (Pos), posCol, posFile, posLine)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

-- | I chose this representation over `Loc` from `srcLoc` because it guarantees the presence of a range.
-- Loc is essentially a 'Maybe (Pos, Pos)', because of the 'NoLoc' constructor.
-- I cannot even imagine dealing with cross-file ranges anyway.

-- The end of the range is exclusive
data SourceRange = SourceRange
  { startPos :: !Pos,
    -- | invariant: at least a big as start.line
    endLine :: !Int,
    -- | invariant: at least as big as start.col, unless the range spans multiple lines
    endColumn :: !Int
  }
  deriving (Show, Eq, Ord)

-- | Extract start line and column
startLineCol :: SourceRange -> (Int, Int)
startLineCol (SourceRange pos _ _) = (posLine pos, posCol pos)

-- | Extract end line and column
endLineCol :: SourceRange -> (Int, Int)
endLineCol (SourceRange _ line col) = (line, col)

overlapsWith :: SourceRange -> SourceRange -> Bool
overlapsWith a b =
  -- since the end is exclusive, I need to use a different operator
  let rangeOverlaps (sa, ea) (sb, eb) = sa < eb && sb < ea
      startA = startLineCol a
      endA = endLineCol a
      startB = startLineCol b
      endB = endLineCol b
   in rangeOverlaps (startA, endA) (startB, endB)

-- >>> isEmpty $ SourceRange {sourceRangeStartPos = Pos "Futhark.fut" 4 40 (-1), sourceRangeEndLine = 4, sourceRangeEndColumn = 40}
-- True

isEmpty :: SourceRange -> Bool
isEmpty (SourceRange (Pos _ startLine startCol _) endLine endCol) =
  startLine == endLine && startCol == endCol

-- | Parse a source range, respect the invariants noted in the definition
-- and print the MegaParsec errorbundle into a Text.
--
-- >>> parse "example.fut:1:1-5"
-- Right (SourceRange {start = Pos "example.fut" 1 1 (-1), endLine = 1, endColumn = 5})
--
-- >>> parse "directory/example.fut:15:12-17:1"
-- Right (SourceRange {start = Pos "directory/example.fut" 15 12 (-1), endLine = 17, endColumn = 1})
parse :: T.Text -> Either T.Text SourceRange
parse text = first textErrorBundle $ P.parse pSourceRange fname text
  where
    fname = ""
    textErrorBundle = T.pack . P.errorBundlePretty

    lineRangeInvariantMessage =
      "End of Line Range is not bigger than or equal to Start of Line Range."
    columnRangeInvariantMessage =
      "End of Column Range is not bigger than or equal to Start of Column Range"

    pSourceRange :: P.Parsec Void T.Text SourceRange
    pSourceRange = do
      fileName <- L.charLiteral `P.manyTill` P.single ':' -- separator
      startLine <- L.decimal
      void $ P.single ':' -- separator
      startCol <- L.decimal

      void $ P.single '-' -- range begin
      rangeEnd1 <- L.decimal
      -- we can't know yet whether this is going to be a line or column position

      (lineRangeEnd, columnRangeEnd) <-
        P.choice
          [ do
              endCol <- P.single ':' *> L.decimal
              pure (rangeEnd1, endCol),
            pure (startLine, rangeEnd1)
          ]

      let lineRangeInvalid = startLine > lineRangeEnd
      when lineRangeInvalid $ fail lineRangeInvariantMessage

      let columnRangeInvalid =
            startLine == lineRangeEnd && startCol > columnRangeEnd
      when columnRangeInvalid $ fail columnRangeInvariantMessage

      pure $
        SourceRange
          { startPos = Pos fileName startLine startCol (-1),
            endLine = lineRangeEnd,
            endColumn = columnRangeEnd
          }

-- | Assumes that the ranges overlap
mergeSemigroup ::
  (Semigroup s) =>
  (SourceRange, s) ->
  (SourceRange, s) ->
  OneTwoThree (SourceRange, s)
mergeSemigroup a@(rangeA, auxA) b@(rangeB, auxB) =
  let orderedBy f x y = if f x < f y then (x, y) else (y, x)
      (startsEarlier, startsLater) = orderedBy (startLineCol . fst) a b

      startsLaterStart = startLineCol . fst $ startsLater
      fname = posFile . startPos $ rangeA

      (endsEarlier, endsLater) =
        orderedBy
          ((endLine &&& endColumn) . fst)
          a
          b

      firstRange =
        (fst startsEarlier)
          { endLine = fst startsLaterStart,
            endColumn = snd startsLaterStart
          }
      secondRange =
        (fst startsLater)
          { endLine = endLine . fst $ endsEarlier,
            endColumn = endColumn . fst $ endsEarlier
          }

      thirdRange =
        let startLine = endLine secondRange
            startCol = endColumn secondRange
         in (fst endsLater)
              { startPos = Pos fname startLine startCol (-1)
              }

      rawRanges =
        Three
          (firstRange, snd startsEarlier)
          (secondRange, auxA <> auxB)
          (thirdRange, snd endsLater)
   in case filter123 (not . isEmpty . fst) rawRanges of
        Nothing ->
          error . unwords $
            [ "Impossible! `mergeRanges` produced no range at all, input ranges:",
              show rangeA,
              show rangeB
            ]
        Just merged -> merged

data OneTwoThree a = One a | Two a a | Three a a a
  deriving (Show, Functor, Foldable)

filter123 :: (a -> Bool) -> OneTwoThree a -> Maybe (OneTwoThree a)
filter123 p self@(One x) = if p x then Just self else Nothing
filter123 p self@(Two x y) = case (p x, p y) of
  (True, True) -> Just self
  (True, False) -> Just (One x)
  (False, True) -> Just (One y)
  (False, False) -> Nothing
filter123 p self@(Three x y z) = case (p x, p y, p z) of
  (False, False, False) -> Nothing
  (False, False, True) -> Just (One z)
  (False, True, False) -> Just (One y)
  (False, True, True) -> Just (Two y z)
  (True, False, False) -> Just (One x)
  (True, False, True) -> Just (Two x z)
  (True, True, False) -> Just (Two x y)
  (True, True, True) -> Just self
