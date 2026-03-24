{-# LANGUAGE ExplicitNamespaces #-}

module Futhark.LSP.InlayHint (getInlayHints) where

import Data.Function ((&))
import Data.Loc (Loc (Loc), Pos (Pos))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Futhark.Compiler.Program (lpImports)
import Futhark.LSP.PositionMapping (toStalePos)
import Futhark.LSP.State (State (stateProgram), getStaleMapping)
import Futhark.LSP.TypeAscription (TypeAscription (..), missingAscriptions)
import Futhark.Util.Loc (Loc (NoLoc), Located (locOf), contains)
import Language.Futhark.Query
  ( allBindings,
  )
import Language.LSP.Protocol.Types
  ( InlayHint (..),
    InlayHintKind (InlayHintKind_Type),
    Position (..),
    Range (Range),
    UInt,
    type (|?) (InL),
  )

getInlayHints :: Range -> State -> FilePath -> [InlayHint]
getInlayHints range state filepath = fromMaybe [] $ do
  let (Range (Position l1 c1) (Position l2 c2)) = range
  imports <- lpImports <$> stateProgram state
  let mapping = getStaleMapping state filepath
  posStart <- toStalePos mapping $ mkPos l1 c1
  posEnd <- toStalePos mapping $ mkPos l2 c2

  pure $
    allBindings imports
      & M.elems
      & filter (boundToInRange (Loc posStart posEnd))
      & concatMap missingAscriptions
      & concatMap inlayHint
  where
    inlayHint :: TypeAscription -> [InlayHint]
    inlayHint (TypeAscBare typName pos) =
      [bareHint typName pos]
    inlayHint (TypeAscParens s tname pos) =
      [startHint s, bareHint (": " <> tname <> ")") pos]

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

    boundToInRange locRange bound = case locOf bound of
      NoLoc -> False
      Loc start _ -> locRange `contains` start
    -- increment by one: Pos counts from one onwards, LSP starts at zero
    mkPos :: UInt -> UInt -> Pos
    mkPos l c = Pos filepath (1 + fromIntegral l) (1 + fromIntegral c) (-1)
