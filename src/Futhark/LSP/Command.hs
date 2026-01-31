{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Futhark.LSP.Command (execute) where

import Control.Monad.Except (ExceptT, throwError)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Futhark.LSP.CommandType (CommandType (CodeLens))
import Futhark.LSP.Lenses qualified as Lenses
import Language.LSP.Protocol.Types (ErrorCodes (ErrorCodes_InvalidRequest), LSPErrorCodes, type (|?) (..))
import Language.LSP.Server (LspT)
import Text.Read (readMaybe)

execute :: Text -> Maybe [Aeson.Value] -> ExceptT (Text, LSPErrorCodes |? ErrorCodes) (LspT () IO) ()
execute name params = case readMaybe $ T.unpack name of
  Just command -> case command of
    CodeLens -> Lenses.execute params
  Nothing ->
    throwError
      ( "Unknown command name: " <> name,
        InR ErrorCodes_InvalidRequest
      )
