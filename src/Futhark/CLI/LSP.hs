{-# LANGUAGE ExplicitNamespaces #-}

-- | @futhark lsp@
module Futhark.CLI.LSP (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (newIORef)
import Futhark.LSP.CommandType (CommandType)
import Futhark.LSP.Handlers (handlers)
import Futhark.LSP.State (emptyState)
import Futhark.Util (showText)
import Language.LSP.Protocol.Types
  ( SaveOptions (SaveOptions),
    TextDocumentSyncKind (TextDocumentSyncKind_Incremental),
    TextDocumentSyncOptions (..),
    type (|?) (InR),
  )
import Language.LSP.Server
  ( Options (optExecuteCommandCommands, optTextDocumentSync),
    ServerDefinition
      ( ServerDefinition,
        configSection,
        defaultConfig,
        doInitialize,
        interpretHandler,
        onConfigChange,
        options,
        parseConfig,
        staticHandlers
      ),
    defaultOptions,
    runLspT,
    runServer,
    type (<~>) (Iso),
  )
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr)

-- | Run @futhark lsp@
main :: String -> [String] -> IO ()
main _prog _args = do
  state_mvar <- newIORef emptyState
  hSetBuffering stderr LineBuffering
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
              { optTextDocumentSync = Just syncOptions,
                optExecuteCommandCommands =
                  Just $
                    map showText [minBound :: CommandType .. maxBound]
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
