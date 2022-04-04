{-# LANGUAGE OverloadedStrings #-}

module Futhark.LSP.Handlers (handlers) where

import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.))
import Futhark.LSP.Compile (tryReCompile, tryTakeStateFromMVar)
import Futhark.LSP.State (State (..))
import Futhark.LSP.Tool (findDefinitionRange, getHoverInfoFromState)
import Futhark.Util (debug)
import Futhark.Util.Pretty (pretty)
import Language.LSP.Server
  ( Handlers,
    LspM,
    getVersionedTextDoc,
    notificationHandler,
    requestHandler,
  )
import Language.LSP.Types
import Language.LSP.Types.Lens (HasUri (uri), HasVersion (version))

handlers :: MVar State -> Handlers (LspM ())
handlers state_mvar =
  mconcat
    [ onInitializeHandler,
      onHoverHandler state_mvar,
      onDocumentOpenHandler state_mvar,
      onDocumentCloseHandler,
      onDocumentSaveHandler state_mvar,
      goToDefinitionHandler state_mvar
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
  case getHoverInfoFromState state file_path (fromEnum l + 1) (fromEnum c + 1) of
    Just msg -> do
      let ms = HoverContents $ MarkupContent MkMarkdown msg
          rsp = Hover ms (Just range)
      responder (Right $ Just rsp)
    Nothing -> responder (Right Nothing)

goToDefinitionHandler :: MVar State -> Handlers (LspM ())
goToDefinitionHandler state_mvar = requestHandler STextDocumentDefinition $ \req responder -> do
  debug "Got goto definition request"
  let RequestMessage _ _ _ (DefinitionParams doc pos _workDone _partial) = req
      Position l c = pos
      file_path = uriToFilePath $ doc ^. uri
  state <- tryTakeStateFromMVar state_mvar file_path
  let maybe_range = findDefinitionRange state file_path (fromEnum l + 1) (fromEnum c + 1)
  case maybe_range of
    Nothing -> responder $ Right $ InR $ InL $ List []
    Just range -> responder $ Right $ InL $ Location (doc ^. uri) range

onDocumentSaveHandler :: MVar State -> Handlers (LspM ())
onDocumentSaveHandler state_mvar = notificationHandler STextDocumentDidSave $ \msg -> do
  let NotificationMessage _ _ (DidSaveTextDocumentParams doc _text) = msg
      file_path = uriToFilePath $ doc ^. uri
  versioned_doc <- getVersionedTextDoc doc
  debug $ "Saved document: " ++ show versioned_doc
  tryReCompile state_mvar file_path (versioned_doc ^. version)

onDocumentOpenHandler :: MVar State -> Handlers (LspM ())
onDocumentOpenHandler state_mvar = notificationHandler STextDocumentDidOpen $ \msg -> do
  let NotificationMessage _ _ (DidOpenTextDocumentParams doc) = msg
      file_path = uriToFilePath $ doc ^. uri
  debug $ "Opened document: " ++ pretty file_path
  tryReCompile state_mvar file_path (Just $ doc ^. version)

onDocumentCloseHandler :: Handlers (LspM ())
onDocumentCloseHandler = notificationHandler STextDocumentDidClose $ \_msg -> debug "Closed document"
