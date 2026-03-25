module Futhark.LSP.TypeAscription
  ( TypeAscription (..),
    missingAscriptions,
    missingTypeParameters,
  )
where

import Control.Arrow ((&&&))
import Data.Function ((&))
import Data.Loc (Loc (..), Pos, locOf)
import Data.Map qualified as M
import Language.Futhark.Query
  ( BoundTo (..),
    TermBindSrc (..),
    TermBinding (..),
    TermFunData (..),
  )
import Language.Futhark.Syntax
  ( ResRetType,
    StructType,
    TypeParamBase,
    VName,
    typeParamName,
  )

data TypeAscription
  = -- | type hint for a let-binding or nested parameter binding
    TypeAscLet StructType Pos
  | -- | type hint for a parameter
    TypeAscParam Pos StructType Pos
  | -- | type hint for a return type
    TypeAscReturn ResRetType Pos
  | -- | type hint for inferred type parameters
    TypeAscType (TypeParamBase VName) Pos

-- techincally, this name is not entirely correct since it covers type
-- parameters as well
missingAscriptions :: BoundTo -> [TypeAscription]
missingAscriptions (BoundTerm term (Loc termStart termEnd)) =
  case term of
    TermSize -> []
    TermVar _ _ (Just _) -> []
    TermVar src inferredType Nothing ->
      let bareHints = [TypeAscLet inferredType termEnd]
       in case src of
            TermBindId -> []
            TermBindLet -> bareHints
            TermBindPat ->
              [TypeAscParam termStart inferredType termEnd]
            TermBindNested -> bareHints
    TermFun tfData ->
      -- ordering is relevant, see the lsp documentation for inlay hints
      let isSynthesized typeParam = case locOf typeParam of
            NoLoc -> True
            _ -> False
          inferredTypeParams = filter isSynthesized (termFunTypeParams tfData)
          paramInfo p = case termFunNameEnd tfData of
            Nothing -> id
            Just pos -> (TypeAscType p pos :)
       in foldr (\p f hs -> paramInfo p $ f hs) id inferredTypeParams $
            let retType = termFunRetType tfData
             in case (termFunAscription tfData, termFunArgEnd tfData, termFunNameEnd tfData) of
                  (Just _, _, _) -> []
                  (Nothing, Just pos, _) -> [TypeAscReturn retType pos]
                  (Nothing, _, Just pos) -> [TypeAscReturn retType pos]
                  _ -> []
missingAscriptions _ = []

missingTypeParameters :: BoundTo -> M.Map VName (Maybe Pos, TypeParamBase VName)
missingTypeParameters (BoundTerm (TermFun tfData) _) =
  M.fromList inferredTypeParams
  where
    inferredTypeParams =
      termFunTypeParams tfData
        & filter keepNoLoc
        & map (typeParamName &&& (termFunNameEnd tfData,))
    keepNoLoc located = case locOf located of
      NoLoc -> True
      _ -> False
missingTypeParameters _ = M.empty
