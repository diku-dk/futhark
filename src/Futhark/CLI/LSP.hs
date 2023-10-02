{-# LANGUAGE ExplicitNamespaces #-}

-- | @futhark lsp@
module Futhark.CLI.LSP (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (newIORef)
import Futhark.LSP.Handlers (handlers)
import Futhark.LSP.State (emptyState)
import Language.LSP.Protocol.Types
  ( SaveOptions (SaveOptions),
    TextDocumentSyncKind (TextDocumentSyncKind_Incremental),
    TextDocumentSyncOptions (..),
    type (|?) (InR),
  )
import Language.LSP.Server

-- | Run @futhark lsp@
main :: String -> [String] -> IO ()
main _prog _args = do
  state_mvar <- newIORef emptyState
  _ <-
    runServer $
      ServerDefinition
        { onConfigChange = const $ pure (),
          configSection = "Futhark",
          parseConfig = const . const $ Right (),
          defaultConfig = (),
          doInitialize = \env _req -> pure $ Right env,
          staticHandlers = handlers state_mvar,
          interpretHandler = \env -> Iso (runLspT env) liftIO,
          options =
            defaultOptions
              { optTextDocumentSync = Just syncOptions
              }
        }
  pure ()

syncOptions :: TextDocumentSyncOptions
syncOptions =
  TextDocumentSyncOptions
    { _openClose = Just True,
      _change = Just TextDocumentSyncKind_Incremental,
      _willSave = Just False,
      _willSaveWaitUntil = Just False,
      _save = Just $ InR $ SaveOptions $ Just False
    }
