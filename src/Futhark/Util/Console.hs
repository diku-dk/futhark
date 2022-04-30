-- | Some utility functions for working with pretty console output.
module Futhark.Util.Console
  ( color,
    inRed,
    inYellow,
    inBold,
  )
where

import System.Console.ANSI

-- | Surround the given string with the given start/end colour codes.
color :: [SGR] -> String -> String
color sgr s = setSGRCode sgr ++ s ++ setSGRCode [Reset]

-- | Make the string red.
inRed :: String -> String
inRed s = setSGRCode [SetColor Foreground Vivid Red] ++ s ++ setSGRCode [Reset]

-- | Make the string yellow.
inYellow :: String -> String
inYellow s = setSGRCode [SetColor Foreground Vivid Yellow] ++ s ++ setSGRCode [Reset]

-- | Make the string bold.
inBold :: String -> String
inBold s = setSGRCode [SetConsoleIntensity BoldIntensity] ++ s ++ setSGRCode [Reset]
