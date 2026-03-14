{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Futhark.LSP.InlayHint (getInlayHints, resolveInlayHint) where

import Data.Aeson (FromJSON, Result (..), ToJSON, Value, fromJSON, toJSON)
import Data.Function ((&))
import Data.Loc (Loc (Loc), Pos (Pos))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Futhark.Compiler.Program (lpImports)
import Futhark.LSP.PositionMapping (toStalePos)
import Futhark.LSP.State (State (stateProgram), getStaleMapping)
import Futhark.Util.Loc (Located (locOf), contains)
import Futhark.Util.Pretty (prettyText)
import Language.Futhark.Query (BoundTo (BoundTerm), allBindings)
import Language.LSP.Protocol.Types (InlayHint (..), Position (..), Range (Range), UInt, type (|?) (InL), InlayHintKind (InlayHintKind_Type))

newtype InlayTypeHintPayload = InlayTypeHintPayload (UInt, UInt, Text)
  deriving newtype (ToJSON, FromJSON)

resolveInlayHint :: Value -> Either Text InlayHint
resolveInlayHint v = case fromJSON v of
  Error msg -> Left $ T.pack msg
  Success (InlayTypeHintPayload (l, c, hint)) ->
    Right $ InlayHint
      { _data_ = Nothing,
        _paddingRight = Nothing,
        _paddingLeft = Nothing,
        _tooltip = Nothing,
        _textEdits = Nothing,
        _kind = Just InlayHintKind_Type,
        _label = InL hint,
        _position = Position l c
      }

getInlayHints :: Range -> State -> FilePath -> [InlayHint]
getInlayHints (Range (Position l1 c1) (Position l2 c2)) state filepath = fromMaybe [] $ do
  imports <- lpImports <$> stateProgram state
  let mapping = getStaleMapping state filepath
  posStart <- toStalePos mapping $ mkPos l1 c1
  posEnd <- toStalePos mapping $ mkPos l2 c2

  pure $
    allBindings imports
      & Map.filter (boundToInRange (Loc posStart posEnd))
      & Map.elems
      & mapMaybe inferredTerms
      & map inlayHint
  where
    inlayHint (Pos _ line col _, typ) =
      InlayHint
        { _textEdits = Nothing,
          _paddingRight = Nothing,
          _paddingLeft = Nothing,
          _tooltip = Nothing,
          _position =
            Position
              { _line = fromIntegral line,
                _character = fromIntegral col
              },
          _data_ =
            Just
              . toJSON
              . InlayTypeHintPayload
              $ (fromIntegral line, fromIntegral col, prettyText typ),
          _label = InL $ prettyText typ,
          _kind = Nothing
        }
    inferredTerms = \case
      BoundTerm inferredType Nothing (Loc _ e) -> Just (e, inferredType)
      _ -> Nothing
    boundToInRange loc bound = loc `contains` start
      where
        Loc start _ = locOf bound
    mkPos l c = Pos filepath (fromIntegral l) (fromIntegral c) (-1)
