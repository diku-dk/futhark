{-# LANGUAGE ExplicitNamespaces #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Futhark.LspTests (tests) where

import Colog.Core (LogAction (LogAction))
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Futhark.CLI.LSP (serverDefinition)
import Language.LSP.Protocol.Types
  ( Hover (Hover),
    LanguageKind (LanguageKind_Custom),
    Position (..),
    Range (..),
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

testHoverInformation :: TestTree
testHoverInformation = testCase "Hovering over i32 variable provides type hint" $
  runServerSessionPair $ do
    docIdent <- createDoc "main.fut" futharkLanguage "def main = let xyz = 42i32 in xyz"
    hoverInfo <- getHover docIdent $ Position {_character = 31, _line = 0}
    case hoverInfo of
      Nothing -> liftIO $ assertFailure "Did not receive any hover information"
      Just (Hover content range) -> do
        liftIO $ range @?= Just expectedRange
        liftIO $ print content
  where
    futharkLanguage = LanguageKind_Custom "Futhark"
    expectedRange =
      Range
        { _start = Position {_line = 0, _character = 30},
          _end = Position {_line = 0, _character = 34}
        }
