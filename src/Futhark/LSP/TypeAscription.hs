module Futhark.LSP.TypeAscription (TypeAscription (..), missingAscriptions) where

import Data.Loc (Loc (..), Pos, locOf)
import Data.Text (Text)
import Futhark.Util.Pretty (prettyText)
import Language.Futhark.Query (BoundTo (..), TermBindSrc (..), TermBinding (..), TermFunData (..))

data TypeAscription
  = -- | type hint at the position
    TypeAscBare Text Pos
  | -- | type hint with parens: opening pos, hint, closing pos
    TypeAscParens Pos Text Pos

missingAscriptions :: BoundTo -> [TypeAscription]
missingAscriptions (BoundTerm term (Loc termStart termEnd)) =
  case term of
    TermSize -> []
    TermVar _ _ (Just _) -> []
    TermVar src inferredType Nothing ->
      let bareHints = [TypeAscBare (": " <> prettyText inferredType) termEnd]
       in case src of
            TermBindId -> []
            TermBindLet -> bareHints
            TermBindPat ->
              [TypeAscParens termStart (prettyText inferredType) termEnd]
            TermBindNested -> bareHints
    TermFun tfData ->
      -- ordering is relevant, see the lsp documentation for inlay hints
      let isSynthesized typeParam = case locOf typeParam of
            NoLoc -> True
            _ -> False
          inferredTypeParams = filter isSynthesized (termFunTypeParams tfData)
          paramInfo p = case termFunNameEnd tfData of
            Nothing -> id
            Just pos -> (TypeAscBare (" " <> prettyText p) pos :)
       in foldr (\p f hs -> paramInfo p $ f hs) id inferredTypeParams $
            let retTypeText = prettyText $ termFunRetType tfData
             in case (termFunAscription tfData, termFunArgEnd tfData, termFunNameEnd tfData) of
                  (Just _, _, _) -> []
                  (Nothing, Just pos, _) -> [TypeAscBare (": " <> retTypeText) pos]
                  (Nothing, _, Just pos) -> [TypeAscBare (": " <> retTypeText) pos]
                  _ -> []
missingAscriptions _ = []
