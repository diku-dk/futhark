module Futhark.Analysis.Proofs.AlgebraPC.Symbol
  ( IdxSym (..),
    Symbol (..),
    MonDir (..),
    Property (..),
    hasPow,
    hasSum,
    hasIdx,
    hasMdf,
    hasIdxOrSum,
    hasMon,
    getVName,
  )
where

import Control.Monad.RWS.Strict hiding (Sum)
import Data.Set qualified as S
import Futhark.Analysis.Proofs.Util (prettyName)
import Futhark.MonadFreshNames
import Futhark.SoP.Expression
import Futhark.SoP.Monad (AlgEnv (..), MonadSoP (..), Nameable (mkName))
import Futhark.SoP.SoP (SoP)
import Futhark.Util.Pretty (Pretty, brackets, enclose, parens, pretty, (<+>))
import Language.Futhark (VName)
import Language.Futhark qualified as E

data IdxSym
  = -- | one regular name
    One VName
  | -- | represents an OR of index-fun-predicates;
    --   implicit assumption: set's cardinal >= 2.
    POR (S.Set VName)
  deriving (Show, Eq, Ord)

data Symbol
  = Var VName
  | Idx IdxSym (SoP Symbol)
  | -- | `Mdf dir A i1 i2` means `A[i1] - A[i2]` where
    -- `A` is known to be monotonic with direction `dir`
    Mdf MonDir VName (SoP Symbol) (SoP Symbol)
  | Sum IdxSym (SoP Symbol) (SoP Symbol)
  | -- | assumes positive base (>1) and exponents (>= 0);
    --   should be verified before construction
    Pow (Integer, SoP Symbol)
  deriving (Show, Eq, Ord)

instance Pretty IdxSym where
  pretty (One x) = prettyName x
  pretty (POR xs) =
    iversonbrackets (mkPOR (S.toList xs))
    where
      iversonbrackets = enclose "⟦" "⟧"
      mkPOR [] = error "Illegal!"
      mkPOR [x] = pretty x
      mkPOR (x : y : lst) = pretty x <+> "||" <+> mkPOR (y : lst)

instance Pretty Symbol where
  pretty symbol = case symbol of
    (Var x) -> prettyName x
    (Idx x i) -> (pretty x) <> brackets (pretty i)
    (Mdf _ x i1 i2) ->
      parens $
        ((prettyName x) <> (brackets (pretty i1)))
          <+> "-"
          <+> ((prettyName x) <> (brackets (pretty i2)))
    (Sum x lb ub) ->
      "∑"
        <> pretty x
        <> brackets (pretty lb <+> ":" <+> pretty ub)
    (Pow (b, s)) -> parens $ prettyOp "^" b s
    where
      prettyOp s x y = pretty x <+> s <+> pretty y

-- This is required by MonadSoP.
instance Nameable Symbol where
  mkName (VNameSource i) = (Var $ E.VName "x" i, VNameSource $ i + 1)

data MonDir = Inc | IncS | Dec | DecS
  deriving (Show, Eq, Ord)

data Property
  = Monotonic MonDir
  | Injective
  | Indicator
  deriving (Show, Eq, Ord)

---------------------------------
--- Simple accessor functions ---
---------------------------------

hasPow :: Symbol -> Bool
hasPow (Pow _) = True
hasPow _ = False

hasSum :: Symbol -> Bool
hasSum (Sum {}) = True
hasSum _ = False

hasIdx :: Symbol -> Bool
hasIdx (Idx {}) = True
hasIdx _ = False

hasMdf :: Symbol -> Bool
hasMdf (Mdf {}) = True
hasMdf _ = False

hasIdxOrSum :: Symbol -> Bool
hasIdxOrSum x = hasIdx x || hasMdf x || hasSum x

hasMon :: S.Set Property -> Maybe MonDir
hasMon props
  | S.null props = Nothing
  | Monotonic dir : _ <- filter f (S.toList props) =
      Just dir
  where
    f (Monotonic _) = True
    f _ = False
hasMon _ = Nothing

getVName :: Symbol -> VName
getVName (Var vn) = vn
getVName x = error ("getVName: non-Var symbol " <> show x)
