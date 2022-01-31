{-# LANGUAGE LambdaCase #-}
-- Based on https://github.com/haskell/lsp#example-language-servers
--
-- Another good inspiration:
-- https://github.com/byorgey/swarm/blob/ceaa949455749077107648f73d79e32771685619/src/Swarm/Language/LSP.hs
{-# LANGUAGE OverloadedStrings #-}

module Futhark.CLI.LSP (main) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Futhark.Util.Options
import Language.LSP.Server
import Language.LSP.Types
import System.Exit

-- The Futhark-specific configuration data.
type Config = FilePath

handlers :: Handlers (LspM Config)
handlers =
  mconcat
    [ notificationHandler SInitialized $ \_not -> do
        let params =
              ShowMessageRequestParams
                MtInfo
                "Turn on code lenses?"
                (Just [MessageActionItem "Turn on", MessageActionItem "Don't"])
        _ <- sendRequest SWindowShowMessageRequest params $ \case
          Right (Just (MessageActionItem "Turn on")) -> do
            let regOpts = CodeLensRegistrationOptions Nothing Nothing (Just False)

            _ <- registerCapability STextDocumentCodeLens regOpts $ \_req responder -> do
              let cmd = Command "Say hello" "lsp-hello-command" Nothing
                  rsp = List [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]
              responder (Right rsp)
            pure ()
          Right _ ->
            sendNotification SWindowShowMessage (ShowMessageParams MtInfo "Not turning on code lenses")
          Left err ->
            sendNotification SWindowShowMessage (ShowMessageParams MtError $ "Something went wrong!\n" <> T.pack (show err))
        pure (),
      requestHandler STextDocumentHover $ \req responder -> do
        let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
            Position _l _c' = pos
            rsp = Hover ms (Just range)
            ms = HoverContents $ markedUpContent "lsp-demo-simple-server" "Hello world"
            range = Range pos pos
        responder (Right $ Just rsp)
    ]

main :: String -> [String] -> IO ()
main = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] ->
      Just $
        onExit <=< runServer $
          ServerDefinition
            { onConfigurationChange = const $ pure $ Right file,
              doInitialize = \env _req -> pure $ Right env,
              staticHandlers = handlers,
              interpretHandler = \env -> Iso (runLspT env) liftIO,
              options = defaultOptions,
              defaultConfig = file
            }
    _ -> Nothing
  where
    onExit 0 = exitSuccess
    onExit x = exitWith $ ExitFailure x
