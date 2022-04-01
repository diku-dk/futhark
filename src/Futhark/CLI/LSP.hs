{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Futhark.CLI.LSP (main) where

import Control.Concurrent.MVar (newMVar)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Futhark.LSP.Handlers (handlers)
import Futhark.LSP.State (emptyState)
import Futhark.Util (debug)
import Language.LSP.Server
  ( Options (textDocumentSync),
    ServerDefinition
      ( ServerDefinition,
        defaultConfig,
        doInitialize,
        interpretHandler,
        onConfigurationChange,
        options,
        staticHandlers
      ),
    defaultOptions,
    runLspT,
    runServer,
    setupLogger,
    type (<~>) (Iso),
  )
import Language.LSP.Types
  ( SaveOptions (SaveOptions),
    TextDocumentSyncKind (TdSyncIncremental),
    TextDocumentSyncOptions (..),
    type (|?) (InR),
  )
import System.Log.Logger (Priority (DEBUG))

main :: String -> [String] -> IO ()
main _prog _args = do
  state_mvar <- newMVar emptyState
  debug "Init with emptyState"
  setupLogger Nothing ["futhark"] DEBUG
  _ <-
    runServer $
      ServerDefinition
        { onConfigurationChange = const $ const $ Right (),
          defaultConfig = (),
          doInitialize = \env _req -> do pure $ Right env,
          staticHandlers = handlers state_mvar,
          interpretHandler = \env -> Iso (runLspT env) liftIO,
          options =
            defaultOptions
              { textDocumentSync = Just syncOptions
              }
        }
  pure ()

syncOptions :: TextDocumentSyncOptions
syncOptions =
  TextDocumentSyncOptions
    { _openClose = Just True,
      _change = Just TdSyncIncremental,
      _willSave = Just False,
      _willSaveWaitUntil = Just False,
      _save = Just $ InR $ SaveOptions $ Just False
    }
