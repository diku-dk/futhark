{-# LANGUAGE OverloadedStrings #-}

-- | The handlers exposed by the language server.
module Futhark.LSP.Handlers (handlers) where

import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.))
import Data.Aeson.Types (Value (Array, String))
import qualified Data.Vector as V
import Futhark.LSP.Compile (tryReCompile, tryTakeStateFromMVar)
import Futhark.LSP.State (State (..))
import Futhark.LSP.Tool (findDefinitionRange, getHoverInfoFromState)
import Futhark.Util (debug)
import Language.LSP.Server (Handlers, LspM, notificationHandler, requestHandler)
import Language.LSP.Types
import Language.LSP.Types.Lens (HasUri (uri))

-- | Given an 'MVar' tracking the state, produce a set of handlers.
-- When we want to add more features to the language server, this is
-- the thing to change.
handlers :: MVar State -> Handlers (LspM ())
handlers state_mvar =
  mconcat
    [ onInitializeHandler,
      onHoverHandler state_mvar,
      onDocumentOpenHandler state_mvar,
      onDocumentCloseHandler,
      onDocumentSaveHandler state_mvar,
      onDocumentChangeHandler state_mvar,
      onDocumentFocusHandler state_mvar,
      goToDefinitionHandler state_mvar,
      onDocumentChangeHandler state_mvar
    ]

onInitializeHandler :: Handlers (LspM ())
onInitializeHandler = notificationHandler SInitialized $ \_msg -> debug "Initialized"

onHoverHandler :: MVar State -> Handlers (LspM ())
onHoverHandler state_mvar = requestHandler STextDocumentHover $ \req responder -> do
  debug "Got hover request"
  let RequestMessage _ _ _ (HoverParams doc pos _workDone) = req
      Position l c = pos
      file_path = uriToFilePath $ doc ^. uri
  state <- tryTakeStateFromMVar state_mvar file_path
  responder $ Right $ getHoverInfoFromState state file_path (fromEnum l + 1) (fromEnum c + 1)

onDocumentFocusHandler :: MVar State -> Handlers (LspM ())
onDocumentFocusHandler state_mvar = notificationHandler (SCustomMethod "custom/onFocusTextDocument") $ \msg -> do
  debug "Got custom request: onFocusTextDocument"
  let NotificationMessage _ _ (Array vector_param) = msg
      String focused_uri = V.head vector_param -- only one parameter passed from the client
  debug $ show focused_uri
  tryReCompile state_mvar (uriToFilePath (Uri focused_uri))

goToDefinitionHandler :: MVar State -> Handlers (LspM ())
goToDefinitionHandler state_mvar = requestHandler STextDocumentDefinition $ \req responder -> do
  debug "Got goto definition request"
  let RequestMessage _ _ _ (DefinitionParams doc pos _workDone _partial) = req
      Position l c = pos
      file_path = uriToFilePath $ doc ^. uri
  state <- tryTakeStateFromMVar state_mvar file_path
  case findDefinitionRange state file_path (fromEnum l + 1) (fromEnum c + 1) of
    Nothing -> responder $ Right $ InR $ InL $ List []
    Just loc -> do
      debug $ show loc
      responder $ Right $ InL loc

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
