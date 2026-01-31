{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Futhark.LSP.Lenses (evalLensesFor, resolveCodeLens, execute) where

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Except (Except, ExceptT, MonadError (throwError), runExceptT)
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
import Futhark.LSP.CommandType qualified as CommandType
import Futhark.Util (showText)
import Language.LSP.Protocol.Types (CodeLens (..), Command (..), ErrorCodes (ErrorCodes_InvalidParams), LSPErrorCodes, Position (..), Range (..), UInt, Uri, toNormalizedUri, type (|?) (..))
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
            Just . Aeson.toJSON . EvalLensPayload $
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
  case Aeson.fromJSON payload :: Aeson.Result LensPayload of
    Aeson.Error msg -> throwError $ T.pack msg
    Aeson.Success _ ->
      pure $
        CodeLens
          { _range = range,
            _data_ = Nothing,
            _command =
              Just
                Command
                  { _arguments = Just [payload],
                    _title = "Evaluate",
                    _command = showText CommandType.CodeLens
                  }
          }

type Execute a = ExceptT (Text, LSPErrorCodes |? ErrorCodes) (LspT () IO) a

execute :: Maybe [Aeson.Value] -> Execute ()
execute = \case
  Just [argument] -> case Aeson.fromJSON argument :: Aeson.Result LensPayload of
    Aeson.Success payload -> executeLens payload
    Aeson.Error msg ->
      throwError
        ( "Failed to decode CodeLens command argument: " <> T.pack msg,
          InR ErrorCodes_InvalidParams
        )
  bad ->
    throwError
      ( "Expected exactly one argument for the CodeLens Command, got: "
          <> showText bad,
        InR ErrorCodes_InvalidParams
      )

executeLens :: LensPayload -> Execute ()
executeLens = \case
  EvalLensPayload payloadData -> executeEvalLens payloadData

executeEvalLens :: EvalLensData -> Execute ()
executeEvalLens (EvalLensData docUri line expectedExpression) = do
  -- retrieve the file
  file <-
    lift (getVirtualFile $ toNormalizedUri docUri) >>= \case
      Nothing ->
        throwError
          ( "Could not find document specified in Command: " <> showText docUri,
            InR ErrorCodes_InvalidParams
          )
      Just file -> pure file

  -- check the line
  let lineText = R.toText . R.getLine (fromIntegral line) $ file ^. file_text
  expressionText <- case T.stripPrefix "-- >>>" lineText of
    Nothing ->
      throwError
        ( "Specified line does not contain an evaluation comment",
          InR ErrorCodes_InvalidParams
        )
    Just expression -> pure expression

  -- is the lens too old?
  -- TODO: Does this really need to be checked?
  when (expressionText /= expectedExpression) $
    throwError
      ( "Specified line has changed since the code lens was resolved",
        InR ErrorCodes_InvalidParams
      )

  undefined
