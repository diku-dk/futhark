{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}

-- | The handlers exposed by the language server.
module Futhark.LSP.Handlers (handlers) where

import Colog.Core (logStringStderr, (<&))
import Control.Lens ((^.))
import Control.Monad.Except (MonadError (throwError), liftEither)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson.Types (Value (Array, String))
import Data.Bifunctor (bimap, first)
import Data.Function ((&))
import Data.IORef (IORef)
import Data.Proxy (Proxy (..))
import Data.Text.Mixed.Rope qualified as R
import Data.Vector qualified as V
import Futhark.Fmt.Printer (fmtToText)
import Futhark.LSP.Compile (tryReCompile, tryTakeStateFromIORef)
import Futhark.LSP.EvalLenses (evalLensesFor)
import Futhark.LSP.State (State (..))
import Futhark.LSP.Tool (findDefinitionRange, getHoverInfoFromState)
import Futhark.Util (showText)
import Futhark.Util.Pretty (prettyText)
import Language.Futhark.Core (locText)
import Language.Futhark.Parser.Monad (SyntaxError (SyntaxError))
import Language.LSP.Protocol.Lens (HasTextDocument (textDocument), HasUri (uri), params)
import Language.LSP.Protocol.Message
  ( SMethod
      ( SMethod_CustomMethod,
        SMethod_Initialized,
        SMethod_TextDocumentCodeLens,
        SMethod_TextDocumentDefinition,
        SMethod_TextDocumentDidChange,
        SMethod_TextDocumentDidClose,
        SMethod_TextDocumentDidOpen,
        SMethod_TextDocumentDidSave,
        SMethod_TextDocumentFormatting,
        SMethod_TextDocumentHover,
        SMethod_WorkspaceCodeLensRefresh,
        SMethod_WorkspaceDidChangeConfiguration
      ),
    TNotificationMessage (TNotificationMessage),
    TRequestMessage (TRequestMessage),
    TResponseError (TResponseError, _code, _message, _xdata),
  )
import Language.LSP.Protocol.Types
  ( ClientCapabilities,
    CodeLens,
    Definition (Definition),
    DefinitionParams (DefinitionParams),
    DidChangeTextDocumentParams (DidChangeTextDocumentParams),
    DidOpenTextDocumentParams (..),
    DidSaveTextDocumentParams (DidSaveTextDocumentParams),
    DocumentFormattingParams (DocumentFormattingParams),
    ErrorCodes (ErrorCodes_InvalidParams, ErrorCodes_InvalidRequest, ErrorCodes_ParseError),
    HoverParams (HoverParams),
    Null (..),
    Position (Position, _character, _line),
    Range (Range, _end, _start),
    TextEdit (TextEdit, _newText, _range),
    Uri (Uri),
    toNormalizedUri,
    uriToFilePath,
    type (|?) (..),
  )
import Language.LSP.Server (Handlers, LspM, getVirtualFile, notificationHandler, requestHandler, sendRequest)
import Language.LSP.VFS (file_text)

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
onDocumentOpenHandler = notificationHandler SMethod_TextDocumentDidOpen $ \message -> do
  -- discards the lsp-id and the result
  lspId <- sendRequest SMethod_WorkspaceCodeLensRefresh Nothing $ \r -> do
    logStringStderr <& ("CodeLens Refresh got result: " ++ show r)

  logStringStderr <& ("Sent CodeLens Refresh with id: " ++ show lspId)

  pure ()

onDocumentCloseHandler :: Handlers (LspM ())
onDocumentCloseHandler = notificationHandler SMethod_TextDocumentDidClose $ \_msg -> pure ()

-- Sent by Eglot when first connecting - not sure when else it might
-- be sent.
onWorkspaceDidChangeConfiguration :: IORef State -> Handlers (LspM ())
onWorkspaceDidChangeConfiguration _state_mvar =
  notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_ ->
    logStringStderr <& "WorkspaceDidChangeConfiguration"

onDocumentFormattingHandler :: Handlers (LspM ())
onDocumentFormattingHandler =
  requestHandler SMethod_TextDocumentFormatting $ \message report ->
    let TRequestMessage _ _ _ formattingParams = message
        DocumentFormattingParams _progressToken textDoc _opts = formattingParams
        fileUri = textDoc ^. uri
     in do
          logStringStderr <& ("Formatting: " ++ show (textDoc ^. uri))
          result <- runExceptT $ do
            virtualFile <- getVirtualFile' fileUri
            let fileText = R.toText $ virtualFile ^. file_text
            formattedText <- fmtToText' (show fileUri) fileText
            pure $
              if formattedText == fileText
                then InR Null
                else InL [fullTextEdit formattedText]

          logStringStderr <& show result
          report result
  where
    fullTextEdit newText =
      TextEdit
        { _newText = newText,
          _range =
            Range
              { _start =
                  Position
                    { _line = 0,
                      _character = 0
                    },
                _end =
                  Position
                    { -- defaults back to real lines, as documented in @lsp-types@
                      _line = maxBound,
                      _character = maxBound
                    }
              }
        }

    fmtToText' fname ftext =
      fmtToText fname ftext
        & first syntaxErrorResponse
        & liftEither

    getVirtualFile' fileUri = do
      maybeFile <- lift $ getVirtualFile (toNormalizedUri fileUri)
      case maybeFile of
        Nothing -> throwError $ noSuchDocumentResponse fileUri
        Just f -> pure f

    syntaxErrorResponse (SyntaxError loc msg) =
      TResponseError
        { _code = InR ErrorCodes_ParseError,
          _message = "Syntax Error at " <> locText loc <> ":\n" <> prettyText msg,
          _xdata = Nothing
        }

    noSuchDocumentResponse fileUri =
      TResponseError
        { _xdata = Nothing,
          _message = "Failed to retrieve document at " <> showText fileUri,
          _code = InR ErrorCodes_InvalidParams
        }

onDocumentCodeLenses :: Handlers (LspM ())
onDocumentCodeLenses =
  requestHandler SMethod_TextDocumentCodeLens $ \request respond ->
    let textDocUri = request ^. (params . textDocument . uri)
     in do
          logStringStderr <& ("textDocument/CodeLens for" ++ show textDocUri)
          eitherLenses <- evalLensesFor textDocUri
          respond $ bimap failure success eitherLenses
  where
    success :: [CodeLens] -> [CodeLens] |? Null
    success = InL

    failure message =
      TResponseError
        { _xdata = Nothing,
          _message = message,
          _code = InR ErrorCodes_InvalidRequest
        }

-- | Given an 'IORef' tracking the state, produce a set of handlers.
-- When we want to add more features to the language server, this is
-- the thing to change.
handlers :: IORef State -> ClientCapabilities -> Handlers (LspM ())
handlers state_mvar _ =
  mconcat
    [ onInitializeHandler,
      onDocumentOpenHandler,
      onDocumentCloseHandler,
      onDocumentCodeLenses,
      onDocumentFormattingHandler,
      onDocumentSaveHandler state_mvar,
      onDocumentChangeHandler state_mvar,
      onDocumentFocusHandler state_mvar,
      goToDefinitionHandler state_mvar,
      onHoverHandler state_mvar,
      onWorkspaceDidChangeConfiguration state_mvar
    ]
