{-# LANGUAGE ExplicitNamespaces #-}

module Futhark.LSP.InlayHint (getInlayHints) where

import Data.Function ((&))
import Data.Loc (Pos (Pos))
import Data.Text (Text)
import Futhark.LSP.State (State)
import Futhark.LSP.Tool (bindingsInRange)
import Futhark.LSP.TypeAscription (TypeAscription (..), missingAscriptions)
import Futhark.Util.Pretty (prettyText)
import Language.LSP.Protocol.Types
  ( InlayHint (..),
    InlayHintKind (InlayHintKind_Type),
    Position (..),
    Range,
    type (|?) (InL),
  )

getInlayHints :: Range -> State -> FilePath -> [InlayHint]
getInlayHints range state filepath =
  maybe
    []
    (map snd)
    (bindingsInRange range state filepath)
    & concatMap missingAscriptions
    & concatMap inlayHint
  where
    inlayHint :: TypeAscription -> [InlayHint]
    inlayHint (TypeAscLet typName pos) =
      [bareHint (": " <> prettyText typName) pos]
    inlayHint (TypeAscParam s tname pos) =
      [startHint s, bareHint (": " <> prettyText tname <> ")") pos]
    inlayHint (TypeAscReturn typName pos) =
      [bareHint (" : " <> prettyText typName) pos]
    inlayHint (TypeAscType typName pos) =
      [bareHint (" " <> prettyText typName) pos]

    startHint :: Pos -> InlayHint
    startHint (Pos _ l c _) =
      InlayHint
        { _position =
            Position (fromIntegral l - 1) (fromIntegral c - 1),
          _label = InL "(",
          _kind = Just InlayHintKind_Type,
          _textEdits = Nothing,
          _tooltip = Nothing,
          _paddingLeft = Nothing,
          _paddingRight = Nothing,
          _data_ = Nothing
        }

    bareHint :: Text -> Pos -> InlayHint
    bareHint tname (Pos _ l c _) =
      InlayHint
        { _position =
            Position (fromIntegral l - 1) (fromIntegral c - 1),
          _label = InL tname,
          _kind = Just InlayHintKind_Type,
          _textEdits = Nothing,
          _tooltip = Nothing,
          _paddingLeft = Nothing,
          _paddingRight = Nothing,
          _data_ = Nothing
        }
