{-# LANGUAGE OverloadedStrings #-}

-- | @futhark datacmp@
module Futhark.CLI.Datacmp (main) where

import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as BS
import Futhark.Test.Values
import Futhark.Util.Options
import System.Exit
import System.IO.Error

readFileSafely :: String -> IO (Maybe (Either String BS.ByteString))
readFileSafely filepath =
  (Just . Right <$> BS.readFile filepath) `catch` couldNotRead
  where
    couldNotRead e
      | isDoesNotExistError e =
        return Nothing
      | otherwise =
        return $ Just $ Left $ show e

-- | Run @futhark datacmp@
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "<file> <file>" f
  where
    f [file_a, file_b] () = Just $ do
      file_contents_a_maybe <- readFileSafely file_a
      file_contents_b_maybe <- readFileSafely file_b
      case (file_contents_a_maybe, file_contents_b_maybe) of
        (Nothing, _) ->
          putStrLn $ "No such file : " ++ file_a
        (_, Nothing) ->
          putStrLn $ "No such file : " ++ file_b
        (Just (Left err_msg), _) ->
          error $ err_msg ++ " when reading " ++ file_a
        (_, Just (Left err_msg)) ->
          error $ err_msg ++ " when reading " ++ file_b
        (Just (Right contents_a), Just (Right contents_b)) -> do
          let vs_a_maybe = readValues contents_a
          let vs_b_maybe = readValues contents_b
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
