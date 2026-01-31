{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Futhark.LSP.Lenses (evalLensesFor, resolveCodeLens) where

import Control.Lens ((^.))
import Control.Monad.Except (Except, MonadError (throwError), runExceptT)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON (parseJSON))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Mixed.Rope qualified as R
import Language.LSP.Protocol.Types (CodeLens (..), Command (..), Position (..), Range (..), UInt, Uri, toNormalizedUri)
import Language.LSP.Server (LspT, getVirtualFile)
import Language.LSP.VFS (file_text)

newtype LensPayload
  = EvalLensPayload EvalLensData

instance Aeson.ToJSON LensPayload where
  toJSON :: LensPayload -> Aeson.Value
  toJSON = \case
    EvalLensPayload payload ->
      Aeson.Object . KeyMap.fromList $
        [ ("type", "EvalLens"),
          ("payload", Aeson.toJSON payload)
        ]

instance Aeson.FromJSON LensPayload where
  parseJSON :: Aeson.Value -> Aeson.Parser LensPayload
  parseJSON = Aeson.withObject "LensPayload" $ \object -> do
    (typ :: Text) <- Aeson.parseField object "type"
    case typ of
      "EvalLens" -> EvalLensPayload <$> Aeson.parseField object "payload"
      _ -> fail $ "Unexpected LensPayload type: " ++ show typ

data EvalLensData = EvalLensData
  { cldtextDocument :: Uri,
    cldLine :: UInt,
    cldComment :: Text
  }

instance Aeson.ToJSON EvalLensData where
  toJSON :: EvalLensData -> Aeson.Value
  toJSON (EvalLensData textDoc line comment) = Aeson.toJSON (textDoc, line, comment)

instance Aeson.FromJSON EvalLensData where
  parseJSON :: Aeson.Value -> Aeson.Parser EvalLensData
  parseJSON o = do
    (textDoc, line, comment) <- parseJSON o
    pure $ EvalLensData textDoc line comment

evalLensesFor :: Uri -> LspT () IO (Either Text [CodeLens])
evalLensesFor file_uri = runExceptT $ do
  vfile <-
    lift (getVirtualFile $ toNormalizedUri file_uri) >>= \case
      Nothing -> throwError . T.pack $ "Could not find file: " ++ show file_uri
      Just vfile -> pure vfile

  let commentLines =
        vfile ^. file_text
          & R.lines
          & zip [0 :: UInt ..]
          & mapMaybe filterComment

  pure $
    commentLines
      & map evalLens
  where
    filterComment (i, line) = (i,) <$> T.stripPrefix "-- >>>" line
    evalLens (i, text) =
      CodeLens
        { _command = Nothing,
          _data_ =
            Just $
              Aeson.toJSON
                EvalLensData
                  { cldtextDocument = file_uri,
                    cldLine = i,
                    cldComment = text
                  },
          _range =
            Range
              { _start = Position {_line = i, _character = 0},
                _end = Position {_line = i, _character = maxBound}
              }
        }

resolveCodeLens :: CodeLens -> Except Text CodeLens
resolveCodeLens (CodeLens _ (Just _) _) =
  throwError "CodeLens is already resolved"
resolveCodeLens (CodeLens _ _ Nothing) =
  throwError "CodeLens doesn't have data attached"
resolveCodeLens (CodeLens range Nothing (Just payload)) =
  pure $
    CodeLens
      { _range = range,
        _data_ = Nothing,
        _command =
          Just
            Command
              { _arguments = Just [payload],
                _title = "Evaluate",
                _command = "CodeLens"
              }
      }
