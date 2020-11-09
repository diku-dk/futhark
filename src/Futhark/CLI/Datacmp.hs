{-# LANGUAGE OverloadedStrings #-}

-- | @futhark datacmp@
module Futhark.CLI.Datacmp (main) where

import qualified Data.ByteString.Lazy.Char8 as BS
import Futhark.Test.Values
import Futhark.Util.Options
import System.Directory
import System.Exit

-- | Run @futhark datacmp@
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "<file> <file>" f
  where
    f [file_a, file_b] () = Just $ do
      aExists <- doesFileExist file_a
      bExists <- doesFileExist file_b
      case (aExists, bExists) of
        (False, _) ->
          putStrLn $ "No such file : " ++ file_a
        (_, False) ->
          putStrLn $ "No such file : " ++ file_b
        (True, True) -> do
          vs_a_maybe <- readValues <$> BS.readFile file_a
          vs_b_maybe <- readValues <$> BS.readFile file_b
          case (vs_a_maybe, vs_b_maybe) of
            (Nothing, _) ->
              error $ "Error reading values from " ++ file_a
            (_, Nothing) ->
              error $ "Error reading values from " ++ file_b
            (Just vs_a, Just vs_b) ->
              case compareValues vs_a vs_b of
                [] -> return ()
                es -> do
                  mapM_ print es
                  exitWith $ ExitFailure 2
    f _ _ =
      Nothing
