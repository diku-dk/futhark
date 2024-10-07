-- | Various small subcommands that are too simple to deserve their own file.
module Futhark.CLI.Misc
  ( mainImports,
    mainHash,
    mainDataget,
    mainCheckSyntax,
    mainThanks,
    mainTokens,
  )
where

import Control.Monad
import Control.Monad.State
import Data.ByteString.Lazy qualified as BS
import Data.Function (on)
import Data.List (nubBy)
import Data.Loc (L (..), startPos)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Futhark.Compiler
import Futhark.Test
import Futhark.Util (hashText, interactWithFileSafely)
import Futhark.Util.Options
import Futhark.Util.Pretty (prettyTextOneLine)
import Language.Futhark.Parser.Lexer (scanTokensText)
import Language.Futhark.Prop (isBuiltin)
import Language.Futhark.Semantic (includeToString)
import System.Environment (getExecutablePath)
import System.Exit
import System.FilePath
import System.IO
import System.Random
import Language.Futhark.Parser
  ( Comment (..),
    SyntaxError (..),
    parseFutharkWithComments,
  )

-- | @futhark imports@
mainImports :: String -> [String] -> IO ()
mainImports = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ do
      (_, prog_imports, _) <- readProgramOrDie file
      liftIO . putStr . unlines . map (++ ".fut") . filter (not . isBuiltin) $
        map (includeToString . fst) prog_imports
    _ -> Nothing

-- | @futhark hash@
mainHash :: String -> [String] -> IO ()
mainHash = mainWithOptions () [] "program" $ \args () ->
  case args of
    [] -> Just $ onInput . parse "<stdin>" =<< T.getContents
    [file] | not $ isBuiltin file -> Just $ onInput . parse file =<< T.readFile file
    _any -> Nothing
  where
    parse a b =
      case parseFutharkWithComments a b of
        Left err -> error $ T.unpack $ syntaxErrorMsg err
        Right (prog, _cs) -> prog
    onInput prog = do
      -- The 'map snd' is an attempt to get rid of the file names so
      -- they won't affect the hashing.
      liftIO $ T.putStrLn $ hashText $ prettyTextOneLine prog

-- | @futhark dataget@
mainDataget :: String -> [String] -> IO ()
mainDataget = mainWithOptions () [] "program dataset" $ \args () ->
  case args of
    [file, dataset] -> Just $ dataget file $ T.pack dataset
    _ -> Nothing
  where
    dataget prog dataset = do
      let dir = takeDirectory prog

      runs <- testSpecRuns <$> testSpecFromProgramOrDie prog

      let exact = filter ((dataset ==) . runDescription) runs
          infixes = filter ((dataset `T.isInfixOf`) . runDescription) runs

      futhark <- FutharkExe <$> getExecutablePath

      case nubBy ((==) `on` runDescription) $
        if null exact then infixes else exact of
        [x] -> BS.putStr =<< getValuesBS futhark dir (runInput x)
        [] -> do
          T.hPutStr stderr $ "No dataset '" <> dataset <> "'.\n"
          T.hPutStr stderr "Available datasets:\n"
          mapM_ (T.hPutStrLn stderr . ("  " <>) . runDescription) runs
          exitFailure
        runs' -> do
          T.hPutStr stderr $ "Dataset '" <> dataset <> "' ambiguous:\n"
          mapM_ (T.hPutStrLn stderr . ("  " <>) . runDescription) runs'
          exitFailure

    testSpecRuns = testActionRuns . testAction
    testActionRuns CompileTimeFailure {} = []
    testActionRuns (RunCases ios _ _) = concatMap iosTestRuns ios

-- | @futhark check-syntax@
mainCheckSyntax :: String -> [String] -> IO ()
mainCheckSyntax = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ void $ readUntypedProgramOrDie file
    _ -> Nothing

-- | @futhark thanks@
mainThanks :: String -> [String] -> IO ()
mainThanks = mainWithOptions () [] "" $ \args () ->
  case args of
    [] -> Just $ do
      i <- randomRIO (0, n - 1)
      putStrLn $ responses !! i
    _ -> Nothing
  where
    n = length responses
    responses =
      [ "You're welcome!",
        "Tell all your friends about Futhark!",
        "Likewise!",
        "And thank you in return for trying the language!",
        "It's our pleasure!",
        "Have fun with Futhark!"
      ]

-- | @futhark tokens@
mainTokens :: String -> [String] -> IO ()
mainTokens = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ do
      res <- interactWithFileSafely (scanTokensText (startPos file) <$> T.readFile file)
      case res of
        Nothing -> do
          hPutStrLn stderr $ file <> ": file not found."
          exitWith $ ExitFailure 2
        Just (Left e) -> do
          hPrint stderr e
          exitWith $ ExitFailure 2
        Just (Right (Left e)) -> do
          hPrint stderr e
          exitWith $ ExitFailure 2
        Just (Right (Right tokens)) ->
          mapM_ printToken tokens
    _ -> Nothing
  where
    printToken (L _ token) =
      print token
