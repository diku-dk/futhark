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
import Futhark.Util.Loc (Located (locOf), contains, Loc (NoLoc))
import Futhark.Util.Pretty (prettyText)
import Language.Futhark.Query
  ( BoundTo (BoundTerm),
    TermBindSrc (..),
    TermBinding (..),
    TermFunData (..),
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
      & M.elems
      & filter (boundToInRange (Loc posStart posEnd))
      & concatMap inferredTerms
      & concatMap inlayHint
  where
    inlayHint :: TypeInlayHintInfo -> [InlayHint]
    inlayHint (TypeHintBare typName pos) =
      [bareHint typName pos]
    inlayHint (TypeHintParens s tname pos) =
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

    inferredTerms :: BoundTo -> [TypeInlayHintInfo]
    inferredTerms (BoundTerm term (Loc termStart termEnd)) =
      case term of
        TermSize -> []
        TermVar _ _ (Just _) -> []
        TermVar src inferredType Nothing ->
          case src of
              TermBindId -> []
              TermBindLet -> 
                [TypeHintBare (": " <> prettyText inferredType) termEnd]
              TermBindPat -> 
                [TypeHintParens termStart (prettyText inferredType) termEnd]
        TermFun tfData ->
          -- ordering is relevant, see the lsp documentation for inlay hints
          let
            isSynthesized typeParam = case locOf typeParam of
              NoLoc -> True
              _ -> False
            inferredTypeParams = filter isSynthesized (termFunTypeParams tfData)
            paramInfo p = case termFunNameEnd tfData of
              Nothing -> id
              Just pos -> (TypeHintBare (prettyText p) pos :)
          in foldr (\ p f hs -> paramInfo p $ f hs) id inferredTypeParams
          $ let retTypeText = prettyText $ termFunRetType tfData
           in case (termFunAscription tfData, termFunArgEnd tfData, termFunNameEnd tfData) of
                (Just _, _, _) -> []
                (Nothing, Just pos, _) -> [TypeHintBare (": " <> retTypeText) pos]
                (Nothing, _, Just pos) -> [TypeHintBare (": " <> retTypeText) pos]
                _ -> []
    inferredTerms _ = []

    boundToInRange loc bound = loc `contains` start
      where
        Loc start _ = locOf bound
    -- increment by one: Pos counts from one onwards, LSP starts at zero
    mkPos :: UInt -> UInt -> Pos
    mkPos l c = Pos filepath (1 + fromIntegral l) (1 + fromIntegral c) (-1)
