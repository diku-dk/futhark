{-# OPTIONS_GHC -Wno-orphans #-}
module Futhark.Analysis.Properties.Property
  ( Property (..),
    askMonotonic,
    askDisjoint,
    hasMon,
    hasDisjoint,
  )
where

import Control.Monad (unless)
import Data.Set qualified as S
import Futhark.Analysis.Properties.AlgebraPC.Symbol (MonDir, Symbol)
import Futhark.Analysis.Properties.Util
import Futhark.SoP.Monad (MonadSoP, askPropertyWith)
import Futhark.SoP.SoP (SoP)
import Futhark.Util.Pretty
import Language.Futhark (VName)

data Property
  = Boolean
  | -- These predicates are pairwise disjoint and collectively exhaustive.
    Disjoint (S.Set VName)
  | Monotonic MonDir
  | -- The restriction of f to the preimage of [a,b] is injective.
    InjectiveRCD (SoP Symbol, SoP Symbol)
  | -- The restriction of f to the preimage of [a,b] is bijective.
    -- [c,d] (subset of [a,b]) is the image of this restricted f.
    BijectiveRCD (SoP Symbol, SoP Symbol) (SoP Symbol, SoP Symbol)
  | FiltPartInv (VName -> SoP Symbol) [(VName -> SoP Symbol, SoP Symbol)]
  deriving (Eq, Ord)

instance Eq (VName -> SoP Symbol) where
  f == g = f dummyVName == g dummyVName

instance Ord (VName -> SoP Symbol) where
  f `compare` g = f dummyVName `compare` g dummyVName

instance Pretty Property where
  pretty Boolean = "Boolean"
  pretty (Disjoint s) =
    "Disjoint" <+> parens (commasep $ map prettyName $ S.toList s)
  pretty (Monotonic dir) = "Mono" <> parens (viaShow dir)
  pretty (InjectiveRCD (a, b)) =
    "InjectiveRCD" <+> parens (pretty a <> comma <+> pretty b)
  pretty (BijectiveRCD (a, b) (c, d)) =
    "BijectiveRCD" <+> parens (pretty a <> comma <+> pretty b) <+> parens (pretty c <> comma <+> pretty d)
  pretty (FiltPartInv pf pps) =
    "FiltPartInv" <+> parens (prettyFun pf "i") <+> ppr
    where
      ppr = brackets $ commasep $ map (\(pp, s) -> parens (prettyFun pp "i" <> comma <+> pretty s)) pps

-- Querying properties.
--------------------------------------------------------------------------------
askMonotonic :: (MonadSoP u e Property m) => u -> m (Maybe MonDir)
askMonotonic sym = askPropertyWith sym hasMon

askDisjoint :: (MonadSoP u e Property m) => u -> m (Maybe (S.Set VName))
askDisjoint sym = askPropertyWith sym hasDisjoint

hasMon :: S.Set Property -> Maybe MonDir
hasMon props
  | S.null props = Nothing
  | Monotonic dir : rest <- filter f (S.toList props) = do
      unless (null rest) $ error "hasMon multiple Monotonic"
      Just dir
  where
    f (Monotonic _) = True
    f _ = False
hasMon _ = Nothing

hasDisjoint :: S.Set Property -> Maybe (S.Set VName)
hasDisjoint props
  | S.null props = Nothing
  | Disjoint nms : rest <- filter f (S.toList props) = do
      unless (null rest) $ error "hasDisjoint multiple Disjoint"
      Just nms
  where
    f (Disjoint {}) = True
    f _ = False
hasDisjoint _ = Nothing
