{-# LANGUAGE TemplateHaskell #-}
module Futhark.CLI.FmtTests (tests) where

import Data.Bifunctor
import Test.Tasty
import Test.Tasty.HUnit(testCase, assertFailure, (@?=))
import Futhark.CLI.Fmt(fmtText)
import Language.Futhark
import Language.Futhark.Parser
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.FileEmbed
import System.FilePath.Posix (isExtensionOf)

programs :: [(FilePath, BS.ByteString)]
programs = filter ((".fut" `isExtensionOf`) . fst) $(embedDir "tests")

tests :: TestTree
tests =
  testGroup "Futhark.CLI.Fmt" [fmtParseTests, fmtIdempotenceTests]

fmtParse :: FilePath -> BS.ByteString -> Either T.Text UncheckedProg
fmtParse file bs = do 
      t <- first (const "Error: Can not parse file as UTF-8.") $ T.decodeUtf8' bs 
      fmt <- fmtText file t
      first (\(SyntaxError loc err) -> locText loc <> ": " <> prettyText err) $ 
        parseFuthark file fmt

-- | Formats and compiles a file   
fmtParseTest :: (FilePath, BS.ByteString) -> TestTree
fmtParseTest (file, bs) = 
    testCase ("formatting and compiling: " ++ file) $ 
      case fmtParse file bs of 
        Left err -> 
          assertFailure $ "Could not parse: " <> T.unpack err
        Right _ -> pure ()


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
fmtIdempotenceTests = testGroup "Idempotence of formatter" $
  map fmtIdempotenceTest programs