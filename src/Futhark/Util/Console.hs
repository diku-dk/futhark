-- | Some utility functions for working with pretty console output.
module Futhark.Util.Console
       ( color
       , inRed
       , inGreen
       , inBold
       )
       where

import System.Console.ANSI

color :: [SGR] -> String -> String
color sgr s = setSGRCode sgr ++ s ++ setSGRCode [Reset]

inRed :: String -> String
inRed s = setSGRCode [SetColor Foreground Vivid Red] ++ s ++ setSGRCode [Reset]

inGreen :: String -> String
inGreen s = setSGRCode [SetColor Foreground Vivid Red] ++ s ++ setSGRCode [Reset]

inBold :: String -> String
inBold s = setSGRCode [SetConsoleIntensity BoldIntensity] ++ s ++ setSGRCode [Reset]
