{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Futhark.LSP.CodeAction (getCodeActions) where

import Data.Function ((&))
import Data.Loc (Pos (Pos))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Futhark.LSP.State (State)
import Futhark.LSP.Tool (bindingsInRange)
import Futhark.LSP.TypeAscription (TypeAscription (TypeAscBare), missingAscriptions)
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
      pure $
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
                          [ TextEdit
                              { _range =
                                  let toLsp = fromIntegral . pred
                                      insertPos = Position (toLsp line) (toLsp column)
                                   in Range insertPos insertPos,
                                _newText = text
                              }
                          ]
                  },
            _title = "Insert type ascription",
            _command = Nothing,
            _data_ = Nothing,
            _kind = Just $ CodeActionKind_Custom "TypeAscription"
          }
    codeActions _ = mempty
