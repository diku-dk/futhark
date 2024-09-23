{-# LANGUAGE TemplateHaskell #-}

module Futhark.CLI.FmtTests (tests) where

import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.FileEmbed
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Futhark.CLI.Fmt (fmtText)
import Language.Futhark
import Language.Futhark.Parser
import System.FilePath.Posix (isExtensionOf)
import Test.Tasty
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

programs :: [(FilePath, BS.ByteString)]
programs = filter ((".fut" `isExtensionOf`) . fst) $(embedDir "tests")

tests :: TestTree
tests =
  testGroup "Futhark.CLI.Fmt" [fmtParseTests] -- , fmtIdempotenceTests]

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
      succeeded (fmtText file) (parseFutharkWithComments file) t

-- | Checks that the AST resulting from parsing a file is the same before and after
--   formatting
fmtIdempotenceTest :: (FilePath, BS.ByteString) -> TestTree
fmtIdempotenceTest = undefined

-- fmtIdempotenceTest (file, bs) =
--   -- Problem: how to compare abstract syntax trees? Also parsing does not give AST but CST
--   testCase ("Comparing AST of " ++ file) $ beforeAST @?= fmtAST
--   where beforeAST = do
--           t <- first (const "Error: Can not parse file as UTF-8.") $ T.decodeUtf8' bs
--           first (\(SyntaxError loc err) -> locText loc <> ": " <> prettyText err) $
--             parseFuthark file t
--         fmtAST = fmtParse file bs

fmtParseTests :: TestTree
fmtParseTests = testGroup "format and parse tests" $ map fmtParseTest programs

fmtIdempotenceTests :: TestTree
fmtIdempotenceTests =
  testGroup "Idempotence of formatter" $
    map fmtIdempotenceTest programs
