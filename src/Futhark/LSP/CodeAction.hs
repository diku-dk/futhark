{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Futhark.LSP.CodeAction (getCodeActions) where

import Data.Function ((&))
import Data.Loc (Pos (Pos), locOf)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Futhark.LSP.State (State, getStaleMapping)
import Futhark.LSP.Tool (bindingsInState, filterByLspRange)
import Futhark.LSP.TypeAscription (TypeAscription (TypeAscLet, TypeAscParam, TypeAscReturn, TypeAscType), missingAscriptions, missingTypeParameters)
import Futhark.Util.Pretty (prettyText)
import Language.Futhark.Core (VName, baseText)
import Language.Futhark.Prop (typeVars)
import Language.Futhark.Syntax (RetTypeBase (RetType), TypeParamBase)
import Language.LSP.Protocol.Types (CodeAction (..), CodeActionKind (CodeActionKind_Custom), Command, Position (Position), Range (Range), TextEdit (..), Uri, WorkspaceEdit (..), type (|?) (InR))
import NeatInterpolation qualified as NI

getCodeActions :: Uri -> Range -> State -> FilePath -> [Command |? CodeAction]
getCodeActions file_uri range state filepath = fromMaybe [] $ do
  allBindings <- bindingsInState state
  let inferredTypes = foldMap missingTypeParameters allBindings
  let mapping = getStaleMapping state filepath
  bindings <-
    filterByLspRange mapping range filepath (locOf . snd) $
      M.assocs allBindings

  pure $
    concatMap
      ( mapMaybe (uncurry $ codeActions inferredTypes)
          . (\(n, b) -> (n,) <$> missingAscriptions b)
      )
      bindings
      & map InR
  where
    codeActions ::
      M.Map VName (Maybe Pos, TypeParamBase VName) ->
      VName ->
      TypeAscription ->
      Maybe CodeAction
    codeActions types name (TypeAscLet typ (Pos _ line column _)) =
      Just
        $ mkAction
          [NI.text|Insert type: `$nameText$typeText`|]
        $ (typeText, line, column) : typeVarEdits types (typeVars typ)
      where
        typeText = ": " <> prettyText typ
        nameText = baseText name
    codeActions types name (TypeAscParam openPos typ pos) =
      Just
        $ mkAction
          [NI.text|Insert type: `($nameText: $typeText)`|]
        $ posEdit openPos "("
          : posEdit pos (": " <> typeText <> ")")
          : typeVarEdits types (typeVars typ)
      where
        posEdit (Pos _ line column _) t = (t, line, column)
        typeText = prettyText typ
        nameText = baseText name
    codeActions types name (TypeAscReturn typ (Pos _ line column _)) =
      Just
        $ mkAction
          [NI.text|Insert return type: `$nameText$typeText`|]
        $ (typeText, line, column) : typeVarEdits types retTypeVars
      where
        typeText = " : " <> prettyText typ
        nameText = baseText name
        retTypeVars =
          S.fromList dims `S.union` typeVars innerType
          where
            RetType dims innerType = typ
    codeActions _ _ (TypeAscType _ _) = Nothing

    -- Text edit information for referenced types that are not yet
    -- syntactically present
    typeVarEdits ::
      M.Map VName (Maybe Pos, TypeParamBase VName) ->
      S.Set VName ->
      [(Text, Int, Int)]
    typeVarEdits availableTypes requiredTypes =
      S.toList requiredTypes
        & mapMaybe (availableTypes M.!?)
        & mapMaybe typeTriple
      where
        typeTriple :: (Maybe Pos, TypeParamBase VName) -> Maybe (Text, Int, Int)
        typeTriple (pos, typ) = do
          Pos _ line col _ <- pos
          pure (" " <> prettyText typ, line, col)

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
          _newText = normalizeNames text
        }
      where
        -- replace subscript characters
        normalizeNames = T.map $ \case
          '\8320' -> '0'
          '\8321' -> '1'
          '\8322' -> '2'
          '\8323' -> '3'
          '\8324' -> '4'
          '\8325' -> '5'
          '\8326' -> '6'
          '\8327' -> '7'
          '\8328' -> '8'
          '\8329' -> '9'
          x -> x
