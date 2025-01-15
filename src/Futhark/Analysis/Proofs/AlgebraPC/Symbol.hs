module Futhark.Analysis.Proofs.AlgebraPC.Symbol
  ( IdxSym (..),
    Symbol (..),
    MonDir (..),
    Property (..),
    hasPow,
    hasSum,
    hasIdx,
    hasMdf,
    hasDisjoint,
    hasIdxOrSum,
    hasMon,
    askMonotonic,
    askDisjoint,
    getVName,
    fv,
  )
where

import Data.Set qualified as S
import Futhark.Analysis.Proofs.Util (prettyName)
import Futhark.MonadFreshNames
import Futhark.SoP.Monad (Nameable (mkName), MonadSoP, askPropertyWith, RangeRelated (rangeRelatedTo))
import Futhark.SoP.SoP (SoP, sopToLists, Free(..))
import Futhark.Util.Pretty (Pretty, brackets, commasep, enclose, parens, pretty, viaShow, (<+>))
import Language.Futhark (VName, nameFromString)
import Language.Futhark qualified as E
import Control.Monad (unless)
-- import Futhark.Util.Pretty

data IdxSym
  = -- | one regular name
    One VName
  | -- | represents an OR of index-fun-predicates;
    --   implicit assumption: set's cardinal >= 1.
    POR (S.Set VName)
  deriving (Show, Eq, Ord)

data Symbol
  = Var VName
  | Idx IdxSym (SoP Symbol)
  | -- | `Mdf dir A i1 i2` means `A[i1] - A[i2]` where
    -- `A` is known to be monotonic with direction `dir`
    Mdf MonDir VName (SoP Symbol) (SoP Symbol)
    -- | Sum(x[lb : ub])
    --   Question: should it assume as pre-condition that
    --     ub -lb >= -1, i.e., `x[i : i-1]` is a legal empty
    --     slice but `x[i : i-2]` is *not*.
  | Sum IdxSym (SoP Symbol) (SoP Symbol)
  | -- | assumes positive base (>1) and exponents (>= 0);
    --   should be verified before construction
    Pow (Integer, SoP Symbol)
  deriving (Show, Eq, Ord)

instance Free Symbol Symbol where
  free (Var _) = S.empty
  free (Idx _ sop) = free sop
  free (Mdf _ _ s1 s2) = free s1 <> free s2
  free (Sum _ s1 s2) = free s1 <> free s2
  free (Pow (_, sop))= free sop

instance RangeRelated Symbol where
  rangeRelatedTo = leadingNames
    where
      leadingNames (Var vn) = S.singleton (Var vn)
      leadingNames (Pow _ ) = S.empty
      leadingNames (Idx ix _ ) = leadingIdxNames ix
      leadingNames (Mdf _ vn _ _ ) = S.singleton (Var vn)
      leadingNames (Sum ix _ _ ) = leadingIdxNames ix

      leadingIdxNames (One vn ) = S.singleton (Var vn)
      leadingIdxNames (POR vns) = S.map Var vns

instance Pretty IdxSym where
  pretty (One x) = prettyName x
  pretty (POR xs) =
    iversonbrackets (mkPOR (S.toList xs))
    where
      iversonbrackets = enclose "⟦" "⟧"
      mkPOR [] = error "Illegal!"
      mkPOR [x] = prettyName x
      mkPOR (x : y : lst) = prettyName x <+> "||" <+> mkPOR (y : lst)

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

instance Nameable Symbol where
  mkName (VNameSource i) = (Var $ E.VName (name i) i, VNameSource $ i + 1)
    where
      name j = nameFromString [cycle symbols !! j, 'ª']
      symbols = ['a'  .. 'd'] <> ['p' .. 'r'] <> ['u' .. 'z']

-- Returns the set of free variable names in a SoP Symbol.
fv :: SoP Symbol -> S.Set VName
fv sop = S.unions [fvSymbol t | (ts, _) <- sopToLists sop, t <- ts]
  where
    fvIdxSym (One vn) = S.singleton vn
    fvIdxSym (POR vns) = vns

    fvSymbol (Var vn) = S.singleton vn
    fvSymbol (Idx xs i) = fvIdxSym xs <> fv i
    fvSymbol (Mdf _ vn i1 i2) = S.singleton vn <> fv i1 <> fv i2
    fvSymbol (Sum xs lb ub) = fvIdxSym xs <> fv lb <> fv ub
    fvSymbol (Pow (_, x)) = fv x

data MonDir = Inc | IncS | Dec | DecS
  deriving (Show, Eq, Ord)

data Property
  = Monotonic MonDir
  | Injective
  | Boolean
  | -- These predicates are pairwise disjoint and collectively exhaustive.
    Disjoint (S.Set VName)
  deriving (Show, Eq, Ord)

instance Pretty Property where
  pretty (Disjoint s) =
    "Disjoint" <+> parens (commasep $ map prettyName $ S.toList s)
  pretty p = viaShow p

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

-- tab_props <- getProperties
-- case hasMon (fromMaybe S.empty $ M.lookup (Var anm) tab_props) of

askMonotonic :: MonadSoP u e Property m => u -> m (Maybe MonDir)
askMonotonic sym = askPropertyWith sym hasMon

askDisjoint :: MonadSoP u e Property m => u -> m (Maybe (S.Set VName))
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
    f (Disjoint{}) = True
    f _ = False
hasDisjoint _ = Nothing

getVName :: Symbol -> VName
getVName (Var vn) = vn
getVName x = error ("getVName: non-Var symbol " <> show x)
