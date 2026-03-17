{-# LANGUAGE ExplicitNamespaces #-}

module Futhark.LSP.InlayHint (getInlayHints) where

import Data.Function ((&))
import Data.Loc (Loc (Loc), Pos (Pos))
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Futhark.Compiler.Program (lpImports)
import Futhark.LSP.PositionMapping (toStalePos)
import Futhark.LSP.State (State (stateProgram), getStaleMapping)
import Futhark.Util.Loc (Located (locOf), contains)
import Futhark.Util.Pretty (prettyText)
import Language.Futhark.Query
  ( BoundTo (BoundTerm),
    TermBindSrc (..),
    TermBinding (..),
    allBindings,
  )
import Language.LSP.Protocol.Types
  ( InlayHint (..),
    InlayHintKind (InlayHintKind_Type),
    Position (..),
    Range (Range),
    UInt,
    type (|?) (InL),
  )

data TypeInlayHintInfo
  = -- | type hint at the position
    TypeHintBare Text Pos
  | -- | type hint with parens: opening pos, hint, closing pos
    TypeHintParens Pos Text Pos

getInlayHints :: Range -> State -> FilePath -> [InlayHint]
getInlayHints range state filepath = fromMaybe [] $ do
  let (Range (Position l1 c1) (Position l2 c2)) = range
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
    inlayHint :: TypeInlayHintInfo -> [InlayHint]
    inlayHint (TypeHintBare typName pos) =
      [endHint typName pos]
    inlayHint (TypeHintParens s tname pos) =
      [startHint s, endHint (tname <> ")") pos]

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

    endHint :: Text -> Pos -> InlayHint
    endHint tname (Pos _ l c _) =
      InlayHint
        { _position =
            Position (fromIntegral l - 1) (fromIntegral c - 1),
          _label = InL $ ": " <> tname,
          _kind = Just InlayHintKind_Type,
          _textEdits = Nothing,
          _tooltip = Nothing,
          _paddingLeft = Nothing,
          _paddingRight = Nothing,
          _data_ = Nothing
        }

    inferredTerms :: BoundTo -> Maybe TypeInlayHintInfo
    inferredTerms (BoundTerm term (Loc termStart termEnd)) =
      case term of
        TermSize -> Nothing
        TermFun _ _ (Just _) _ -> Nothing
        TermVar _ _ (Just _) -> Nothing
        TermFun _ _ _ Nothing -> Nothing
        TermFun _ rtyp Nothing (Just rtyploc) ->
          Just $
            TypeHintBare (prettyText rtyp) rtyploc
        TermVar src inferredType Nothing ->
          ( case src of
              TermBindId -> const Nothing
              TermBindLet -> Just . uncurry TypeHintBare
              TermBindPat -> Just . uncurry (TypeHintParens termStart)
          )
            (prettyText inferredType, termEnd)
    inferredTerms _ = Nothing

    boundToInRange loc bound = loc `contains` start
      where
        Loc start _ = locOf bound
    -- increment by one: Pos counts from one onwards, LSP starts at zero
    mkPos :: UInt -> UInt -> Pos
    mkPos l c = Pos filepath (1 + fromIntegral l) (1 + fromIntegral c) (-1)
