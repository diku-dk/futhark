-- | Progress bar
module Futhark.Util.ProgressBar
     ( descString
     , mkProgressPrompt) where

import Data.IORef
import System.IO
import Data.Maybe
import Futhark.Util (fancyTerminal, maybeNth)

descString :: String -> Int -> String
descString desc pad_to = desc ++ ": " ++ replicate (pad_to - length desc) ' '

progressBar :: Int -> Int -> Int -> String
progressBar cur bound steps =
  "[" ++ map cell [1..steps] ++ "] " ++ show cur ++ "/" ++ show bound
  where step_size = bound `div` steps
        chars = " ▏▎▍▍▌▋▊▉█"
        char i = fromMaybe ' ' $ maybeNth (i::Int) chars

        cell :: Int -> Char
        cell i
          | i * step_size <= cur = char 9
          | otherwise = char (((cur - (i-1) * step_size) * length chars)
                              `div` step_size)

mkProgressPrompt :: Int -> Int -> String -> IO (Maybe Int -> IO ())
mkProgressPrompt runs pad_to dataset_desc
  | fancyTerminal = do
      count <- newIORef (0::Int)
      return $ \us -> do
        putStr "\r" -- Go to start of line.
        i <- readIORef count
        let i' = if isJust us then i+1 else i
        writeIORef count i'
        putStr $ descString dataset_desc pad_to ++ progressBar i' runs 10
        putStr " " -- Just to move the cursor away from the progress bar.
        hFlush stdout

  | otherwise = do
      putStr $ descString dataset_desc pad_to
      hFlush stdout
      return $ const $ return ()
