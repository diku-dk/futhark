-- | @futhark query@
module Futhark.CLI.Query (main) where

import Futhark.Compiler
import Futhark.Util.Loc
import Futhark.Util.Options
import Language.Futhark.Query
import Language.Futhark.Syntax
import Text.Read (readMaybe)

-- | Run @futhark query@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "program line col" $ \args () ->
  case args of
    [file, line, col] -> do
      line' <- readMaybe line
      col' <- readMaybe col
      Just $ do
        (_, imports, _) <- readProgramOrDie file
        -- The 'offset' part of the Pos is not used and can be arbitrary.
        case atPos imports $ Pos file line' col' 0 of
          Nothing -> putStrLn "No information available."
          Just (AtName qn def loc) -> do
            putStrLn $ "Name: " ++ prettyString qn
            putStrLn $ "Position: " ++ locStr (srclocOf loc)
            case def of
              Nothing -> pure ()
              Just (BoundTerm t defloc) -> do
                putStrLn $ "Type: " ++ prettyString t
                putStrLn $ "Definition: " ++ locStr (srclocOf defloc)
              Just (BoundType defloc) ->
                putStrLn $ "Definition: " ++ locStr (srclocOf defloc)
              Just (BoundModule defloc) ->
                putStrLn $ "Definition: " ++ locStr (srclocOf defloc)
              Just (BoundModuleType defloc) ->
                putStrLn $ "Definition: " ++ locStr (srclocOf defloc)
    _ -> Nothing
