{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Futhark.LSP.InlayHint (getInlayHints, resolveInlayHint) where

import Data.Aeson (FromJSON, Result (..), ToJSON, Value, fromJSON, toJSON)
import Data.Function ((&))
import Data.Loc (Loc (Loc), Pos (Pos))
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Futhark.Compiler.Program (lpImports)
import Futhark.LSP.PositionMapping (toStalePos)
import Futhark.LSP.State (State (stateProgram), getStaleMapping)
import Futhark.Util.Loc (Located (locOf), contains)
import Futhark.Util.Pretty (prettyText)
import Language.Futhark.Query (BoundTo (BoundTerm), allBindings)
import Language.LSP.Protocol.Types (InlayHint (..), InlayHintKind (InlayHintKind_Type), Position (..), Range (Range), UInt, type (|?) (InL))

newtype InlayTypeHintPayload
  = -- start pos | end pos, type text
    InlayTypeHintPayload (Either (UInt, UInt) (UInt, UInt, Text))
  deriving newtype (ToJSON, FromJSON)

resolveInlayHint :: Value -> Either Text InlayHint
resolveInlayHint v = case fromJSON v of
  Error msg -> Left $ T.pack msg
  Success (InlayTypeHintPayload (Left (l, c))) ->
    Right $
      InlayHint
        { _data_ = Just v,
          _paddingRight = Nothing,
          _paddingLeft = Nothing,
          _tooltip = Nothing,
          _textEdits = Nothing,
          _kind = Just InlayHintKind_Type,
          _label = InL "(",
          _position = Position l c
        }
  Success (InlayTypeHintPayload (Right (l, c, hint))) ->
    Right $
      InlayHint
        { _data_ = Just v,
          _paddingRight = Nothing,
          _paddingLeft = Nothing,
          _tooltip = Nothing,
          _textEdits = Nothing,
          _kind = Just InlayHintKind_Type,
          _label = InL $ ": " <> hint <> ")",
          _position = Position l c
        }

getInlayHints :: Range -> State -> FilePath -> [InlayHint]
getInlayHints (Range (Position l1 c1) (Position l2 c2)) state filepath = fromMaybe [] $ do
  imports <- lpImports <$> stateProgram state -- crash occurs here
  let mapping = getStaleMapping state filepath
  posStart <- toStalePos mapping $ mkPos l1 c1
  posEnd <- toStalePos mapping $ mkPos l2 c2

  pure $
    allBindings imports
      & M.filter (boundToInRange (Loc posStart posEnd))
      & M.elems
      & mapMaybe inferredTerms
      & concatMap inlayHint
  where
    inlayHint (Pos _ startLine startCol _, Pos _ endLine endCol _, typ) =
      let lspEndLine = fromIntegral endLine - 1
          -- the end is exclusive
          lspEndCol = fromIntegral endCol - 1
          lspStartLine = fromIntegral startLine - 1
          -- it needs to start before the identifier
          lspStartCol = fromIntegral startCol - 1
       in [ InlayHint
              { _textEdits = Nothing,
                _paddingRight = Nothing,
                _paddingLeft = Nothing,
                _tooltip = Nothing,
                _position =
                  Position
                    { -- translate line for LSP
                      _line = lspEndLine,
                      -- end is exclusive
                      _character = lspEndCol
                    },
                _data_ =
                  Just
                    . toJSON
                    . InlayTypeHintPayload
                    . Right
                    $ (lspEndLine, lspEndCol, prettyText typ),
                _label = InL $ ": " <> prettyText typ <> ")",
                _kind = Just InlayHintKind_Type
              },
            InlayHint
              { _position =
                  Position
                    { _character = lspStartCol,
                      _line = lspStartLine
                    },
                _label = InL "(",
                _kind = Just InlayHintKind_Type,
                _textEdits = Nothing,
                _tooltip = Nothing,
                _paddingLeft = Nothing,
                _paddingRight = Nothing,
                _data_ =
                  Just
                    . toJSON
                    . InlayTypeHintPayload
                    . Left
                    $ (lspStartLine, lspStartCol)
              }
          ]
    inferredTerms = \case
      BoundTerm inferredType Nothing (Loc s e) -> Just (s, e, inferredType)
      _ -> Nothing
    boundToInRange loc bound = loc `contains` start
      where
        Loc start _ = locOf bound
    mkPos :: UInt -> UInt -> Pos
    mkPos l c = Pos filepath (1 + fromIntegral l) (1 + fromIntegral c) (-1)

-- increment by one: Pos counts from one onwards, LSP starts at zero
