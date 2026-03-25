{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Futhark.LSP.CodeAction (getCodeActions) where

import Data.Function ((&))
import Data.Loc (Pos (Pos))
import Data.Map qualified as M
import Futhark.LSP.State (State)
import Futhark.LSP.Tool (bindingsInRange)
import Futhark.LSP.TypeAscription (TypeAscription (TypeAscLet, TypeAscParam, TypeAscReturn), missingAscriptions)
import Language.Futhark.Core (VName, baseText)
import Language.LSP.Protocol.Types (CodeAction (..), CodeActionKind (CodeActionKind_Custom), Command, Position (Position), Range (Range), TextEdit (..), Uri, WorkspaceEdit (..), type (|?) (InR))

getCodeActions :: Uri -> Range -> State -> FilePath -> [Command |? CodeAction]
getCodeActions file_uri range state filepath =
  maybe
    []
    (concatMap $ \(n, b) -> (n,) <$> missingAscriptions b)
    (bindingsInRange range state filepath)
    & map (InR . uncurry codeActions)
  where
    codeActions :: VName -> TypeAscription -> CodeAction
    codeActions name (TypeAscLet text (Pos _ line column _)) =
      mkAction
        ("Insert variable type ascription for " <> baseText name)
        [(text, line, column)]
    codeActions name (TypeAscParam openPos text pos) =
      mkAction
        ("Insert parameter type ascription for " <> baseText name)
        [ let Pos _ line column _ = openPos in ("(", line, column),
          let Pos _ line column _ = pos in (": " <> text <> ")", line, column)
        ]
    codeActions name (TypeAscReturn text (Pos _ line column _)) =
      mkAction
        ("Insert return type ascription for " <> baseText name)
        [(text, line, column)]

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
