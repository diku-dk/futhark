{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Futhark.LSP.CodeAction (getCodeActions) where

import Data.Function ((&))
import Data.Loc (Pos (Pos))
import Data.Map qualified as M
import Futhark.LSP.State (State)
import Futhark.LSP.Tool (bindingsInRange)
import Futhark.LSP.TypeAscription (TypeAscription (TypeAscBare, TypeAscParens), missingAscriptions)
import Language.LSP.Protocol.Types (CodeAction (..), CodeActionKind (CodeActionKind_Custom), Command, Position (Position), Range (Range), TextEdit (..), Uri, WorkspaceEdit (..), type (|?) (InR))

getCodeActions :: Uri -> Range -> State -> FilePath -> [Command |? CodeAction]
getCodeActions file_uri range state filepath =
  maybe
    []
    (concatMap missingAscriptions)
    (bindingsInRange range state filepath)
    & concatMap codeActions
    & map InR
  where
    codeActions :: TypeAscription -> [CodeAction]
    codeActions (TypeAscBare text (Pos _ line column _)) =
      pure $ mkAction [(text, line, column)]
    codeActions (TypeAscParens openPos text pos) =
      pure $
        mkAction
          [ let Pos _ line column _ = openPos in ("(", line, column),
            let Pos _ line column _ = pos in (text, line, column)
          ]

    mkAction edits =
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
          _title = "Insert type ascription",
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
