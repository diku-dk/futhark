{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MultilineStrings #-}

module Futhark.LspTests (tests) where

import Colog.Core (LogAction (LogAction))
import Control.Concurrent (forkIO)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Data.Text (Text)
import Futhark.CLI.LSP (serverDefinition)
import Language.LSP.Protocol.Lens (uri)
import Language.LSP.Protocol.Types
  ( Definition (Definition),
    Hover (Hover),
    LanguageKind (LanguageKind_Custom),
    Location (..),
    MarkupContent (..),
    MarkupKind (MarkupKind_PlainText),
    Position (..),
    Range (..),
    TextDocumentIdentifier,
    type (|?) (..),
  )
import Language.LSP.Server (runServerWithHandles)
import Language.LSP.Test
  ( Session,
    createDoc,
    defaultConfig,
    fullLatestClientCaps,
    getDefinitions,
    getHover,
    runSessionWithHandles,
  )
import System.Process (createPipe)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import System.IO (hClose)
import Data.Foldable (for_)

tests :: TestTree
tests =
  testGroup
    "Futhark.LspTests"
    [ testHoverInformation,
      testDefinition
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

  result <- runSessionWithHandles clientOut clientIn defaultConfig fullLatestClientCaps "." session

  for_ [serverIn, clientOut, clientIn, serverOut] hClose

  pure result


createMainDoc :: Text -> Session TextDocumentIdentifier
createMainDoc = createDoc "main.fut" futharkLanguage
  where
    futharkLanguage :: LanguageKind
    futharkLanguage = LanguageKind_Custom "Futhark"

serverTestCase :: TestName -> Session () -> TestTree
serverTestCase testName = testCase testName . runServerSessionPair

testHoverInformation :: TestTree
testHoverInformation = serverTestCase "Hover" $ do
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

testDefinition :: TestTree
testDefinition = serverTestCase "Go To Definition" $
  do
    docIdent <- createMainDoc mainDocContents
    definition <- getDefinitions docIdent fooBodyPosition

    let expectedDefinition =
          Definition $
            InL $
              Location
                { _uri = docIdent ^. uri,
                  _range =
                    Range
                      { _end = Position 0 15,
                        _start = Position 0 0
                      }
                }

    liftIO $ definition @?= InL expectedDefinition
  where
    mainDocContents =
      """
      def foo = 0i32
      def bar = foo
      """

    fooBodyPosition :: Position
    fooBodyPosition =
      Position
        { _line = 1,
          _character = 12
        }
