{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultilineStrings #-}

module Futhark.LspTests (tests, main) where

import Colog.Core (LogAction (LogAction))
import Control.Concurrent (forkIO)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Text (Text)
import Futhark.CLI.LSP (serverDefinition)
import Futhark.Fmt.Printer (fmtToText)
import Language.Futhark.Parser.Monad (SyntaxError (..))
import Language.LSP.Protocol.Lens (uri)
import Language.LSP.Protocol.Types
  ( CodeLens (CodeLens),
    Definition (Definition),
    FormattingOptions (FormattingOptions),
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
    documentContents,
    executeCommand,
    formatDoc,
    fullLatestClientCaps,
    getAndResolveCodeLenses,
    getDefinitions,
    getHover,
    runSessionWithHandles, message,
  )
import System.IO (hClose)
import System.Process (createPipe)
import Test.Tasty (TestName, TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Language.LSP.Protocol.Message (SMethod(SMethod_WorkspaceApplyEdit))

tests :: TestTree
tests =
  testGroup
    "Futhark.LspTests"
    [ testHoverInformation,
      testDefinition,
      testFormatting,
      testEvaluationComment
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
    docIdent <- createMainDoc mainContents
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
    mainContents =
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

testFormatting :: TestTree
testFormatting = serverTestCase "Formatting" $
  do
    docIdent <- createMainDoc mainContents
    formatDoc docIdent _formattingOptions
    lspFormattedDoc <- documentContents docIdent
    formattedDoc <- case fmtToText "main.fut" mainContents of
      Left (SyntaxError loc msg) ->
        liftIO . assertFailure $
          "Formatting failed: " <> show loc <> ".\n" <> show msg
      Right d -> pure d

    liftIO $ lspFormattedDoc @?= formattedDoc
  where
    -- these are ignored by the formatter anyway
    _formattingOptions = FormattingOptions 0 False Nothing Nothing Nothing
    mainContents =
      """
      -- this is where all the lines start
        def main = 
          0i32
      """

main :: IO ()
main = defaultMain testEvaluationComment

testEvaluationComment :: TestTree
testEvaluationComment = serverTestCase "Evaluation Comment" $
  do
    docIdent <- createMainDoc mainContents
    lensCommand <-
      getAndResolveCodeLenses docIdent >>= \case
        [CodeLens _ (Just command) _] -> pure command
        bad -> liftIO $ assertFailure $ "Unexpected Code Lenses: " <> show bad

    executeCommand lensCommand

    -- this is important, it crashes with an IOError otherwise
    _workspaceEdit <- message SMethod_WorkspaceApplyEdit

    newContents <- documentContents docIdent
    liftIO $ newContents @?= expectedContents
  where
    expectedContents =
      mainContents
        <> "-- 47\n"
    mainContents =
      """
      def x = 42i32
      -- >>> x + 5
      """
