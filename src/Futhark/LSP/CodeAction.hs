{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE QuasiQuotes #-}

module Futhark.LSP.CodeAction (getCodeActions) where

import Data.Function ((&))
import Data.Loc (Pos (Pos))
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Futhark.LSP.State (State)
import Futhark.LSP.Tool (bindingsInRange)
import Futhark.LSP.TypeAscription (TypeAscription (TypeAscLet, TypeAscParam, TypeAscReturn, TypeAscType), missingAscriptions)
import Futhark.Util.Pretty (prettyText)
import Language.Futhark.Core (VName, baseText)
import Language.LSP.Protocol.Types (CodeAction (..), CodeActionKind (CodeActionKind_Custom), Command, Position (Position), Range (Range), TextEdit (..), Uri, WorkspaceEdit (..), type (|?) (InR))
import NeatInterpolation qualified as NI

getCodeActions :: Uri -> Range -> State -> FilePath -> [Command |? CodeAction]
getCodeActions file_uri range state filepath =
  maybe
    []
    (concatMap $ \(n, b) -> (n,) <$> missingAscriptions b)
    (bindingsInRange range state filepath)
    & mapMaybe (uncurry codeActions)
    & map InR
  where
    codeActions :: VName -> TypeAscription -> Maybe CodeAction
    codeActions name (TypeAscLet typ (Pos _ line column _)) =
      Just $
        mkAction
          [NI.text|Insert variable type ascription: `$nameText$typeText`|]
          [(typeText, line, column)]
      where
        typeText = prettyText typ
        nameText = baseText name
    codeActions name (TypeAscParam openPos typ pos) =
      Just $
        mkAction
          [NI.text|Insert parameter type ascription: `($nameText: $typeText)`|]
          [ let Pos _ line column _ = openPos
             in ("(", line, column),
            let Pos _ line column _ = pos
             in (": " <> typeText <> ")", line, column)
          ]
      where
        typeText = prettyText typ
        nameText = baseText name
    codeActions name (TypeAscReturn typ (Pos _ line column _)) =
      Just $
        mkAction
          [NI.text|Insert return type of `$nameText`: `$typeText`|]
          [(typeText, line, column)]
      where
        typeText = prettyText typ
        nameText = baseText name
    codeActions _ (TypeAscType _ _) = Nothing

    mkAction title edits =
      CodeAction
        { _isPreferred = Nothing,
          _disabled = Nothing,
          _diagnostics = Nothing,
          _edit =
            Just $
              WorkspaceEdit
                { _documentChanges = Nothing,
                  _changeAnnotations = Nothing,
                  _changes =
                    Just $
                      M.singleton
                        file_uri
                        (map mkEdit edits)
                },
          _title = title,
          _command = Nothing,
          _data_ = Nothing,
          _kind = Just $ CodeActionKind_Custom "TypeAscription"
        }

    mkEdit (text, line, column) =
      TextEdit
        { _range =
            let toLsp = fromIntegral . pred
                insertPos = Position (toLsp line) (toLsp column)
             in Range insertPos insertPos,
          _newText = text
        }
