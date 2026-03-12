{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Futhark.LspTests (tests) where

import Colog.Core (LogAction (LogAction))
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Data.Text (Text)
import Futhark.CLI.LSP (serverDefinition)
import Language.LSP.Protocol.Types
  ( Hover (Hover),
    LanguageKind (LanguageKind_Custom),
    MarkupContent (..),
    MarkupKind (MarkupKind_PlainText),
    Position (..),
    Range (..),
    TextDocumentIdentifier,
    type (|?) (InL),
  )
import Language.LSP.Server (runServerWithHandles)
import Language.LSP.Test
  ( Session,
    createDoc,
    defaultConfig,
    fullLatestClientCaps,
    getHover,
    runSessionWithHandles,
  )
import System.Process (createPipe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Futhark.LspTests"
    [ testHoverInformation
    ]

runServerSessionPair :: Session a -> IO a
runServerSessionPair session = do
  (serverIn, clientOut) <- createPipe
  (clientIn, serverOut) <- createPipe
  futharkServer <- serverDefinition
  let ignoreLog :: (Applicative m) => LogAction m msg
      ignoreLog = LogAction . const $ pure ()

  _serverTid <-
    forkIO . void $
      runServerWithHandles ignoreLog ignoreLog serverIn serverOut futharkServer

  runSessionWithHandles clientOut clientIn defaultConfig fullLatestClientCaps "." session


createMainDoc :: Text -> Session TextDocumentIdentifier
createMainDoc = createDoc "main.fut" futharkLanguage
  where
    futharkLanguage :: LanguageKind
    futharkLanguage = LanguageKind_Custom "Futhark"

testHoverInformation :: TestTree
testHoverInformation = testCase "Hover" $
  runServerSessionPair $ do
    docIdent <- createMainDoc "def main = let xyz = 42i32 in xyz"
    hoverInfo <- getHover docIdent $ Position {_character = 31, _line = 0}
    case hoverInfo of
      Nothing -> liftIO $ assertFailure "Did not receive any hover information"
      Just (Hover content range) -> do
        liftIO $ range @?= Just expectedRange
        liftIO $ content @?= InL expectedMarkup
  where
    expectedMarkup =
      MarkupContent
        { _value = "i32",
          _kind = MarkupKind_PlainText
        }
    expectedRange =
      Range
        { _start = Position {_line = 0, _character = 30},
          _end = Position {_line = 0, _character = 34}
        }
