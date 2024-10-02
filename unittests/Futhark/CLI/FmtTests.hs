{-# LANGUAGE TemplateHaskell #-}

module Futhark.CLI.FmtTests (tests) where

import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.FileEmbed
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Futhark.CLI.Fmt qualified as Fmt
import Futhark.CLI.Fmt.Printer qualified as Printer
import Futhark.CLI.Misc(mainTokens)
import Language.Futhark
import Language.Futhark.Parser
import System.FilePath.Posix (isExtensionOf)
import Test.Tasty
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import System.IO.Silently(capture_)
import System.Directory(createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath.Posix(takeDirectory)

programs :: [(FilePath, BS.ByteString)]
programs = filter ((".fut" `isExtensionOf`) . fst) $(embedDir "tests")

tests :: TestTree
tests =
  testGroup "Futhark.CLI.Fmt" [fmtParseTests] --, fmtTokenTests]

-- | Formats and compiles a file
fmtParseTest :: (FilePath, BS.ByteString) -> TestTree
fmtParseTest (file, bs) =
  testCase ("formatting and compiling: " ++ file) $
    case result of
      Left err ->
        assertFailure $ "Could not parse: " <> T.unpack err
      Right _ -> pure ()
  where
    succeeded f g t =
      case (f t, g t) of
        (Left (SyntaxError _ err0), Left (SyntaxError _ err1)) | err0 == err1 -> Right ()
        (Right _, Right _) -> Right ()
        (Left (SyntaxError loc err), _) -> Left $ locText loc <> ": " <> prettyText err
        (_, Left (SyntaxError loc err)) -> Left $ locText loc <> ": " <> prettyText err

    result = do
      t <- first (const "Error: Can not parse file as UTF-8.") $ T.decodeUtf8' bs
      succeeded (Printer.fmtText file) (parseFutharkWithComments file) t

-- | Checks that the AST resulting from parsing a file is the same before and after
--   formatting
fmtTokenTest :: (FilePath, BS.ByteString) -> TestTree
--fmtTokenTest = undefined
fmtTokenTest (file, _) =
  -- Problem: how to compare abstract syntax trees? 
  -- Answer: either by using tokens or somehow finding a way to compile 
  testCase ("Comparing Tokens of " ++ file) test 
  where test = do
          bfTokens <- capture_ $ mainTokens "program" ["tests/"++file]   
          fmtContent <- capture_ $ Fmt.main "program" ["tests/"++file]
          createAndWriteFile ("tests/fmtTemp/" ++ file) fmtContent 
          fmtTokens <- capture_ $ mainTokens "Program" ["tests/fmtTemp/" ++ file]
          removeComments fmtTokens @?= removeComments bfTokens
          removeFmtDirectory

        removeComments tokens = unlines $ map unwords $ filter nonComment $ map words $ lines tokens
        nonComment lst = "COMMENT" `notElem` lst

fmtParseTests :: TestTree
fmtParseTests = testGroup "format and parse tests" $ map fmtParseTest programs

fmtTokenTests :: TestTree
fmtTokenTests =
  testGroup "Compare tokens before and after formatter" $ map fmtTokenTest programs

createAndWriteFile :: FilePath -> String -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path content

removeFmtDirectory :: IO ()
removeFmtDirectory = removeDirectoryRecursive "tests/fmtTemp"
