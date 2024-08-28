{-# LANGUAGE DataKinds #-}

-- | The handlers exposed by the language server.
module Futhark.LSP.Handlers (handlers) where

import Colog.Core (logStringStderr, (<&))
import Control.Lens ((^.))
import Data.Aeson.Types (Value (Array, String))
import Data.IORef
import Data.Proxy (Proxy (..))
import Data.Vector qualified as V
import Futhark.LSP.Compile (tryReCompile, tryTakeStateFromIORef)
import Futhark.LSP.State (State (..))
import Futhark.LSP.Tool (findDefinitionRange, getHoverInfoFromState)
import Language.LSP.Protocol.Lens (HasUri (uri))
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server (Handlers, LspM, notificationHandler, requestHandler)

onInitializeHandler :: Handlers (LspM ())
onInitializeHandler = notificationHandler SMethod_Initialized $ \_msg ->
  logStringStderr <& "Initialized"

onHoverHandler :: IORef State -> Handlers (LspM ())
onHoverHandler state_mvar =
  requestHandler SMethod_TextDocumentHover $ \req responder -> do
    let TRequestMessage _ _ _ (HoverParams doc pos _workDone) = req
        Position l c = pos
        file_path = uriToFilePath $ doc ^. uri
    logStringStderr <& ("Got hover request: " <> show (file_path, pos))
    state <- tryTakeStateFromIORef state_mvar file_path
    responder $ Right $ maybe (InR Null) InL $ getHoverInfoFromState state file_path (fromEnum l + 1) (fromEnum c + 1)

onDocumentFocusHandler :: IORef State -> Handlers (LspM ())
onDocumentFocusHandler state_mvar =
  notificationHandler (SMethod_CustomMethod (Proxy @"custom/onFocusTextDocument")) $ \msg -> do
    logStringStderr <& "Got custom request: onFocusTextDocument"
    let TNotificationMessage _ _ (Array vector_param) = msg
        String focused_uri = V.head vector_param -- only one parameter passed from the client
    tryReCompile state_mvar (uriToFilePath (Uri focused_uri))

goToDefinitionHandler :: IORef State -> Handlers (LspM ())
goToDefinitionHandler state_mvar =
  requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
    let TRequestMessage _ _ _ (DefinitionParams doc pos _workDone _partial) = req
        Position l c = pos
        file_path = uriToFilePath $ doc ^. uri
    logStringStderr <& ("Got goto definition: " <> show (file_path, pos))
    state <- tryTakeStateFromIORef state_mvar file_path
    case findDefinitionRange state file_path (fromEnum l + 1) (fromEnum c + 1) of
      Nothing -> responder $ Right $ InR $ InR Null
      Just loc -> responder $ Right $ InL $ Definition $ InL loc

onDocumentSaveHandler :: IORef State -> Handlers (LspM ())
onDocumentSaveHandler state_mvar =
  notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
    let TNotificationMessage _ _ (DidSaveTextDocumentParams doc _text) = msg
        file_path = uriToFilePath $ doc ^. uri
    logStringStderr <& ("Saved document: " ++ show doc)
    tryReCompile state_mvar file_path

onDocumentChangeHandler :: IORef State -> Handlers (LspM ())
onDocumentChangeHandler state_mvar =
  notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
    let TNotificationMessage _ _ (DidChangeTextDocumentParams doc _content) = msg
        file_path = uriToFilePath $ doc ^. uri
    tryReCompile state_mvar file_path

-- Some clients (Eglot) sends open/close events whether we want them
-- or not, so we better be prepared to ignore them.
onDocumentOpenHandler :: Handlers (LspM ())
onDocumentOpenHandler = notificationHandler SMethod_TextDocumentDidOpen $ \_ -> pure ()

onDocumentCloseHandler :: Handlers (LspM ())
onDocumentCloseHandler = notificationHandler SMethod_TextDocumentDidClose $ \_msg -> pure ()

-- Sent by Eglot when first connecting - not sure when else it might
-- be sent.
onWorkspaceDidChangeConfiguration :: IORef State -> Handlers (LspM ())
onWorkspaceDidChangeConfiguration _state_mvar =
  notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_ ->
    logStringStderr <& "WorkspaceDidChangeConfiguration"

-- | Given an 'IORef' tracking the state, produce a set of handlers.
-- When we want to add more features to the language server, this is
-- the thing to change.
handlers :: IORef State -> ClientCapabilities -> Handlers (LspM ())
handlers state_mvar _ =
  mconcat
    [ onInitializeHandler,
      onDocumentOpenHandler,
      onDocumentCloseHandler,
      onDocumentSaveHandler state_mvar,
      onDocumentChangeHandler state_mvar,
      onDocumentFocusHandler state_mvar,
      goToDefinitionHandler state_mvar,
      onHoverHandler state_mvar,
      onWorkspaceDidChangeConfiguration state_mvar
    ]
