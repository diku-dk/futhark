{-# OPTIONS_GHC -Wno-orphans #-}

module Futhark.Analysis.Properties.Property
  ( Property (..),
    Predicate (..),
    MonDir (..),
    askMonotonic,
    askDisjoint,
    hasMon,
    hasDisjoint,
    getFiltPart,
    getFiltPartInv,
    askFiltPartInv,
    askFiltPart,
    nameAffectedBy,
  )
where

import Control.Monad (unless)
import Data.Set qualified as S
import Futhark.Analysis.Properties.Util
import Futhark.SoP.Monad (MonadSoP, askPropertyWith)
import Futhark.SoP.SoP (SoP)
import Futhark.Util.Pretty
import Language.Futhark (VName)

data Property u
  = Boolean
  | -- These predicates are pairwise disjoint and collectively exhaustive.
    Disjoint (S.Set VName)
  | Monotonic VName MonDir
  | -- The restriction of f to the preimage of [a,b] is injective.
    InjectiveRCD VName (SoP u, SoP u)
  | -- The restriction of f to the preimage of [a,b] is bijective.
    -- [c,d] (subset of [a,b]) is the image of this restricted f.
    BijectiveRCD VName (SoP u, SoP u) (SoP u, SoP u)
  | FiltPartInv VName (Predicate u) [(Predicate u, SoP u)]
  | FiltPart VName VName (Predicate u) [(Predicate u, SoP u)]
  deriving (Eq, Ord, Show)

data Predicate u = Predicate VName u
  deriving (Eq, Ord, Show)

data MonDir = Inc | IncS | Dec | DecS
  deriving (Show, Eq, Ord)

instance (Pretty u) => Pretty (Property u) where
  pretty Boolean = "Boolean"
  pretty (Disjoint s) =
    "Disjoint" <+> parens (commasep $ map prettyName $ S.toList s)
  pretty (Monotonic x dir) = "Mono" <+> prettyName x <+> viaShow dir
  pretty (InjectiveRCD x rcd) =
    "InjectiveRCD" <+> prettyName x <+> parens (pretty rcd)
  pretty (BijectiveRCD x rcd img) =
    "BijectiveRCD" <+> prettyName x <+> parens (pretty rcd) <+> parens (pretty img)
  pretty (FiltPartInv x pf pps) =
    "FiltPartInv" <+> prettyName x <+> parens (pretty pf) <+> ppr pps
  pretty (FiltPart x y pf pps) =
    "FiltPart" <+> prettyName x <+> prettyName y <+> parens (pretty pf) <+> ppr pps

ppr :: (Pretty a1, Pretty a2) => [(a1, a2)] -> Doc ann
ppr pps =
  brackets . commasep $
    map (\(pp, s) -> parens (pretty pp <> comma <+> pretty s)) pps

instance (Pretty u) => Pretty (Predicate u) where
  pretty (Predicate vn e) = "λ" <> prettyName vn <> dot <+> pretty e

{-
               Querying properties.
-}
askMonotonic :: (MonadSoP u e (Property u) m) => u -> m (Maybe MonDir)
askMonotonic sym = askPropertyWith sym hasMon

askDisjoint :: (MonadSoP u e (Property u) m) => u -> m (Maybe (S.Set VName))
askDisjoint sym = askPropertyWith sym hasDisjoint

hasMon :: S.Set (Property u) -> Maybe MonDir
hasMon props
  | S.null props = Nothing
  | Monotonic _ dir : rest <- filter f (S.toList props) = do
      unless (null rest) $ error "hasMon multiple Monotonic"
      Just dir
  where
    f (Monotonic _ _) = True
    f _ = False
hasMon _ = Nothing

hasDisjoint :: S.Set (Property u) -> Maybe (S.Set VName)
hasDisjoint props
  | S.null props = Nothing
  | Disjoint nms : rest <- filter f (S.toList props) = do
      unless (null rest) $ error "hasDisjoint multiple Disjoint"
      Just nms
  where
    f (Disjoint {}) = True
    f _ = False
hasDisjoint _ = Nothing

askFiltPartInv :: (MonadSoP u e (Property u) m) => u -> m (Maybe (Property u))
askFiltPartInv = (`askPropertyWith` getFiltPartInv)

askFiltPart :: (MonadSoP u e (Property u) m) => u -> m (Maybe (Property u))
askFiltPart = (`askPropertyWith` getFiltPart)

getFiltPartInv :: S.Set (Property u) -> Maybe (Property u)
getFiltPartInv props
  | fp@(FiltPartInv {}) : rest <- filter f (S.toList props) = do
      unless (null rest) $ error "getFiltPartInv multiple FiltPartInv"
      Just fp
  | otherwise = Nothing
  where
    f (FiltPartInv {}) = True
    f _ = False

getFiltPart :: S.Set (Property u) -> Maybe (Property u)
getFiltPart = undefined

nameAffectedBy :: Property u -> VName
nameAffectedBy (Monotonic x _) = x
nameAffectedBy (InjectiveRCD x _) = x
nameAffectedBy (BijectiveRCD x _ _) = x
nameAffectedBy (FiltPartInv x _ _) = x
nameAffectedBy (FiltPart y _x _ _) = y
nameAffectedBy _ = undefined
