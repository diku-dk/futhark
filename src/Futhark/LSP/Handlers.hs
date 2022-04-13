{-# LANGUAGE OverloadedStrings #-}

module Futhark.LSP.Handlers (handlers) where

import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson.Types (Value (Array, String))
import qualified Data.Vector as V
import Futhark.LSP.Compile (tryReCompile, tryTakeStateFromMVar)
import Futhark.LSP.State (State (..))
import Futhark.LSP.Tool (getHoverInfoFromState)
import Futhark.Util (debug)
import Language.LSP.Server (Handlers, LspM, notificationHandler, requestHandler)
import Language.LSP.Types
import Language.LSP.Types.Lens (HasUri (uri))

handlers :: MVar State -> Handlers (LspM ())
handlers state_mvar =
  mconcat
    [ onInitializeHandler,
      onHoverHandler state_mvar,
      onDocumentOpenHandler state_mvar,
      onDocumentCloseHandler,
      onDocumentSaveHandler state_mvar,
      onDocumentChangeHandler state_mvar,
      onDocumentFocusHandler state_mvar
    ]

onInitializeHandler :: Handlers (LspM ())
onInitializeHandler = notificationHandler SInitialized $ \_msg -> debug "Initialized"

onHoverHandler :: MVar State -> Handlers (LspM ())
onHoverHandler state_mvar = requestHandler STextDocumentHover $ \req responder -> do
  debug "Got hover request"
  let RequestMessage _ _ _ (HoverParams doc pos _workDone) = req
      Position l c = pos
      range = Range pos pos
      file_path = uriToFilePath $ doc ^. uri
  state <- tryTakeStateFromMVar state_mvar file_path
  result <- liftIO $ getHoverInfoFromState state file_path (fromEnum l + 1) (fromEnum c + 1)
  case result of
    Just msg -> do
      let ms = HoverContents $ MarkupContent MkMarkdown msg
          rsp = Hover ms (Just range)
      responder (Right $ Just rsp)
    Nothing -> responder (Right Nothing)

onDocumentFocusHandler :: MVar State -> Handlers (LspM ())
onDocumentFocusHandler state_mvar = notificationHandler (SCustomMethod "custom/onFocusTextDocument") $ \msg -> do
  debug "Got custom request: onFocusTextDocument"
  let NotificationMessage _ _ (Array vector_param) = msg
      String focused_uri = V.head vector_param -- only one parameter passed from the client
  debug $ show focused_uri
  tryReCompile state_mvar (uriToFilePath (Uri focused_uri))

onDocumentSaveHandler :: MVar State -> Handlers (LspM ())
onDocumentSaveHandler state_mvar = notificationHandler STextDocumentDidSave $ \msg -> do
  let NotificationMessage _ _ (DidSaveTextDocumentParams doc _text) = msg
      file_path = uriToFilePath $ doc ^. uri
  debug $ "Saved document: " ++ show doc
  tryReCompile state_mvar file_path

onDocumentChangeHandler :: MVar State -> Handlers (LspM ())
onDocumentChangeHandler state_mvar = notificationHandler STextDocumentDidChange $ \msg -> do
  let NotificationMessage _ _ (DidChangeTextDocumentParams doc _content) = msg
      file_path = uriToFilePath $ doc ^. uri
  tryReCompile state_mvar file_path

onDocumentOpenHandler :: MVar State -> Handlers (LspM ())
onDocumentOpenHandler state_mvar = notificationHandler STextDocumentDidOpen $ \msg -> do
  let NotificationMessage _ _ (DidOpenTextDocumentParams doc) = msg
      file_path = uriToFilePath $ doc ^. uri
  debug $ "Opened document: " ++ show (doc ^. uri)
  tryReCompile state_mvar file_path

onDocumentCloseHandler :: Handlers (LspM ())
onDocumentCloseHandler = notificationHandler STextDocumentDidClose $ \_msg -> debug "Closed document"
