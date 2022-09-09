{-# LANGUAGE ExplicitNamespaces #-}

-- | @futhark lsp@
module Futhark.CLI.LSP (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (newIORef)
import Futhark.LSP.Handlers (handlers)
import Futhark.LSP.State (emptyState)
import Language.LSP.Server
import Language.LSP.Types
  ( SaveOptions (SaveOptions),
    TextDocumentSyncKind (TdSyncIncremental),
    TextDocumentSyncOptions (..),
    type (|?) (InR),
  )

-- | Run @futhark lsp@
main :: String -> [String] -> IO ()
main _prog _args = do
  state_mvar <- newIORef emptyState
  _ <-
    runServer $
      ServerDefinition
        { onConfigurationChange = const $ const $ Right (),
          defaultConfig = (),
          doInitialize = \env _req -> pure $ Right env,
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
