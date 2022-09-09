-- | Facilities for generating and otherwise handling pretty-based progress bars.
module Futhark.Util.ProgressBar
  ( progressBar,
    ProgressBar (..),
    progressSpinner,
  )
where

import Data.Text qualified as T

-- | Information about a progress bar to render.  The "progress space"
-- spans from 0 and up to the `progressBarBound`, but can be
-- visualised in any number of steps.
data ProgressBar = ProgressBar
  { -- | Number of steps in the visualisation.
    progressBarSteps :: Int,
    -- | The logical upper bound.
    progressBarBound :: Double,
    -- | The current position in the progress bar, relative to the
    -- upper bound.
    progressBarElapsed :: Double
  }

-- | Render the progress bar.
progressBar :: ProgressBar -> T.Text
progressBar (ProgressBar steps bound elapsed) =
  "|" <> T.pack (map cell [1 .. steps]) <> "| "
  where
    step_size :: Double
    step_size = bound / fromIntegral steps
    chars = " ▏▎▍▍▌▋▊▉█"
    num_chars = T.length chars
    char i
      | i >= 0 && i < num_chars = T.index chars i
      | otherwise = ' '

    cell :: Int -> Char
    cell i
      | i' * step_size <= elapsed = char 9
      | otherwise =
          char (floor (((elapsed - (i' - 1) * step_size) * fromIntegral num_chars) / step_size))
      where
        i' = fromIntegral i

-- | Render a spinner - a kind of progress bar where there is no upper
-- bound because we don't know how long it'll take.  You certainly
-- know these from THE INTERNET.  The non-negative integer is how many
-- "steps" have been taken.  The spinner looks best if this is
-- incremented by one for every call.
progressSpinner :: Int -> T.Text
progressSpinner spin_idx =
  T.singleton $ T.index spin_load (spin_idx `rem` n)
  where
    spin_load = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"
    n = T.length spin_load
