-- | @futhark datacmp@
module Futhark.CLI.Datacmp (main) where

import Control.Exception
import Data.ByteString.Lazy.Char8 qualified as BS
import Futhark.Data.Compare
import Futhark.Data.Reader
import Futhark.Util.Options
import System.Exit
import System.IO

readFileSafely :: String -> IO (Either String BS.ByteString)
readFileSafely filepath =
  (Right <$> BS.readFile filepath) `catch` couldNotRead
  where
    couldNotRead e = pure $ Left $ show (e :: IOError)

-- | Run @futhark datacmp@
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "<file> <file>" f
  where
    f [file_a, file_b] () = Just $ do
      file_contents_a_maybe <- readFileSafely file_a
      file_contents_b_maybe <- readFileSafely file_b
      case (file_contents_a_maybe, file_contents_b_maybe) of
        (Left err_msg, _) -> do
          hPutStrLn stderr err_msg
          exitFailure
        (_, Left err_msg) -> do
          hPutStrLn stderr err_msg
          exitFailure
        (Right contents_a, Right contents_b) -> do
          let vs_a_maybe = readValues contents_a
          let vs_b_maybe = readValues contents_b
          case (vs_a_maybe, vs_b_maybe) of
            (Nothing, _) -> do
              hPutStrLn stderr $ "Error reading values from " ++ file_a
              exitFailure
            (_, Nothing) -> do
              hPutStrLn stderr $ "Error reading values from " ++ file_b
              exitFailure
            (Just vs_a, Just vs_b) ->
              case compareSeveralValues (Tolerance 0.002) vs_a vs_b of
                [] -> pure ()
                es -> do
                  mapM_ print es
                  exitWith $ ExitFailure 2
    f _ _ =
      Nothing
