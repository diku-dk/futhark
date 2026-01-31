{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Futhark.LSP.EvalLenses (evalLensesFor) where

import Control.Lens ((^.))
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON (parseJSON))
import Data.Aeson qualified as A
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Mixed.Rope qualified as R
import Futhark.Util (showText)
import Language.LSP.Protocol.Types (CodeLens (..), Command (..), Position (..), Range (..), UInt, Uri, toNormalizedUri)
import Language.LSP.Server (LspT, getVirtualFile)
import Language.LSP.VFS (file_text)
import Text.Read (readMaybe)

data CodeLensType
  = EvalLens
  deriving (Show, Read, Enum, Bounded)

instance Aeson.ToJSON CodeLensType where
  toJSON = A.String . showText

instance Aeson.FromJSON CodeLensType where
  parseJSON = \case
    A.String t -> case readMaybe . T.unpack $ t of
      Nothing -> fail $ "Unknown CodeLensType: " ++ T.unpack t
      Just x -> pure x
    _ -> fail "Failed to decode CodeLensType, expected String"

data EvalLensData = EvalLensData
  { cldtextDocument :: Uri,
    cldLine :: UInt,
    cldComment :: Text
  }

instance Aeson.ToJSON EvalLensData where
  toJSON (EvalLensData textDoc line comment) = A.toJSON (textDoc, line, comment)

instance Aeson.FromJSON EvalLensData where
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
        { _command =
            Just $
              Command
                { _arguments =
                    Just . L.singleton $
                      A.toJSON
                        EvalLensData
                          { cldtextDocument = file_uri,
                            cldLine = i,
                            cldComment = text
                          },
                  _title = "Evaluate",
                  _command = "CodeLens/" <> showText EvalLens
                },
          _data_ = Nothing,
          _range =
            Range
              { _start = Position {_line = i, _character = 0},
                _end = Position {_line = i, _character = maxBound}
              }
        }
