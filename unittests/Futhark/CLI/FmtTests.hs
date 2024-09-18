{-# LANGUAGE TemplateHaskell #-}
module Futhark.CLI.FmtTests (tests) where

import Data.Bifunctor
import Test.Tasty
import Test.Tasty.HUnit
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
  testGroup "Futhark.CLI.Fmt" [fmtParseTests]

-- | Formats and compiles a file   
fmtParseTest :: (FilePath, BS.ByteString) -> TestTree
fmtParseTest (file, bs) = 
    testCase ("formatting and compiling: " ++ file) $ 
      case result of 
        Left err -> 
          assertFailure $ "Could not parse: " <> T.unpack err
        Right _ -> pure ()
  where
    result = do 
      t <- first (const "Error: Can not parse file as UTF-8.") $ T.decodeUtf8' bs 
      fmt <- fmtText file t
      first (\(SyntaxError loc err) -> locText loc <> ": " <> prettyText err)
        $ parseFutharkWithComments file fmt

fmtParseTests :: TestTree 
fmtParseTests = testGroup "format and parse tests" $ map fmtParseTest programs