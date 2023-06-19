-- | The sum-of-products representation and related operations.
module Futhark.SoP.SoP
  ( Term (..),
    SoP (..),
    Range (..),
    mapSoP,
    filterSoP,
    sym2SoP,
    int2SoP,
    scaleSoP,
    zeroSoP,
    negSoP,
    addSoPs,
    (.+.),
    subSoPs,
    (.-.),
    mulSoPs,
    (.*.),
    divSoPs,
    (./.),
    divSoPInt,
    signumSoP,
    factorSoP,
    numTerms,
    justSym,
    justConstant,
    justAffine,
    justSingleTerm,
    deleteTerm,
    insertTerm,
    Free (..),
    Substitute (..),
    substituteOne,
    sopToList,
    sopToLists,
    termToList,
  )
where

import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MS
import Data.Set (Set)
import Data.Set qualified as S
import Futhark.Analysis.PrimExp.Convert
import Futhark.SoP.Util
import Futhark.Util.Pretty
import Language.Futhark.Core
import Language.Futhark.Prop

-- | A 'Term' is a product of symbols.
newtype Term u = Term {getTerm :: MultiSet u}
  deriving (Eq, Ord, Monoid, Semigroup, Foldable)

-- | A sum-of-products is a constant value added to a sum of terms,
--   which are (by construction)
--
--   1. Lexicographically sorted.
--   2. Contain no duplicated terms, i.e., @2*x*y + 3*x*y@ is
--   illegal.
data SoP u = SoP {getTerms :: Map (Term u) Integer}
  deriving (Eq, Ord)

-- | A symbol @sym@ with range @'Range' lbs k ubs@ means @max{lbs} <=
--   k*sym <= min{ubs}@.  'lbs' and 'ubs' are (potentially empty) sets
--   of 'SoP's.
data Range u = Range
  { lowerBound :: Set (SoP u),
    rangeMult :: Integer,
    upperBound :: Set (SoP u)
  }
  deriving (Eq, Ord)

-- | Should probably make this smarter
instance Ord u => Semigroup (Range u) where
  Range lb1 k1 ub1 <> Range lb2 k2 ub2 =
    Range
      (S.map (int2SoP m1 .*.) lb1 <> S.map (int2SoP m2 .*.) lb2)
      (lcm k1 k2)
      (S.map (int2SoP m1 .*.) ub1 <> S.map (int2SoP m2 .*.) ub2)
    where
      m1 = lcm k1 k2 `div` k1
      m2 = lcm k1 k2 `div` k2

instance Ord u => Monoid (Range u) where
  mempty = Range mempty 1 mempty

instance Pretty u => Pretty (Term u) where
  pretty (Term t) =
    mconcat $ punctuate "*" $ map pretty $ MS.toList t

instance Pretty u => Pretty (SoP u) where
  pretty (SoP ts)
    | M.null ts = "0"
    | otherwise =
        mconcat $
          punctuate " + " $
            map (uncurry pTerm) $
              M.toList ts
    where
      pTerm term n
        | isConstTerm term = pretty n
        | n == 1 = pretty term
        | otherwise = pretty n <> "*" <> pretty term

instance Pretty a => Pretty (Set a) where
  pretty as = "{" <> mconcat (punctuate ", " (map pretty $ S.toList as)) <> "}"

instance Pretty u => Pretty (Range u) where
  pretty (Range lb k ub) =
    pretty_lb <> pretty k <> pretty_ub
    where
      pretty_lb =
        -- \| S.null lb = mempty
        "max" <> pretty lb <+> "<= "
      pretty_ub =
        -- \| S.null ub = mempty
        " <=" <+> "min" <> pretty ub

instance {-# OVERLAPS #-} Pretty u => Pretty (u, Range u) where
  pretty (sym, Range lb k ub) =
    pretty_lb <> psym <> pretty_ub
    where
      psym
        | k == 1 = pretty sym
        | otherwise = pretty k <> "*" <> pretty sym
      pretty_lb =
        -- \| S.null lb = mempty
        "max" <> pretty lb <+> "<= "
      pretty_ub =
        -- \| S.null ub = mempty
        " <=" <+> "min" <> pretty ub

instance Pretty u => Show (Term u) where
  show = prettyString

instance Pretty u => Show (SoP u) where
  show = prettyString

instance Pretty u => Show (Range u) where
  show = prettyString

--------------------------------------------------------------------------------
-- Term operations
--------------------------------------------------------------------------------

-- | Is the term a constant?
isConstTerm :: Term u -> Bool
isConstTerm (Term t) = MS.null t

-- | Converts anything list-like into a term.
toTerm :: (Foldable t, Ord u) => t u -> Term u
toTerm = Term . toMS

termToList :: Term u -> [u]
termToList (Term t) = MS.toList t

-- | Is 'x' a factor of 'y'?
isFactorOf :: Ord u => Term u -> Term u -> Bool
isFactorOf (Term x) (Term y) = x `MS.isSubsetOf` y

-- | Divides 'x' by 'y'.
divTerm :: Ord u => Term u -> Term u -> Maybe (Term u)
divTerm xt@(Term x) yt@(Term y)
  | yt `isFactorOf` xt = Just $ Term $ x MS.\\ y
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Basic operations
--------------------------------------------------------------------------------

-- | Pads a SoP with a 0 constant, if it doesn't have one. I.e.,
--   transforms sop into sop + 0. Useful for pattern matching the constant
--   term of SoPs.
padWithZero :: Ord u => SoP u -> SoP u
padWithZero sop@(SoP ts) =
  case ts M.!? mempty of
    Nothing ->
      SoP $ M.insert mempty 0 ts
    Just {} -> sop

-- | Filters the terms of an 'SoP'.
filterSoP :: (Term u -> Integer -> Bool) -> SoP u -> SoP u
filterSoP p (SoP ts) = SoP $ M.filterWithKey p ts

-- | Normalizes a SoP. Here, that just means removing any keys of the
--   form @0 * term@. (i.e., superfluous zeros).
normalize :: Ord u => SoP u -> SoP u
normalize sop
  | Just {} <- justConstant sop = sop
  | otherwise = filterSoP (\_ n -> n /= 0) sop

mapSoP :: (Integer -> Integer) -> SoP u -> SoP u
mapSoP f (SoP ts) = SoP $ fmap f ts

sopToList :: SoP u -> [(Term u, Integer)]
sopToList (SoP ts) = M.toList ts

sopToLists :: Ord u => SoP u -> [([u], Integer)]
sopToLists (SoP ts) = M.toList $ M.mapKeys termToList ts

sopFromList :: (Foldable t, Ord u, Ord (t u)) => [(t u, Integer)] -> SoP u
sopFromList = SoP . M.mapKeys toTerm . M.fromList

-- | An 'SoP' composed of a single term.
term2SoP :: (Foldable t, Ord u) => t u -> Integer -> SoP u
term2SoP t n = SoP $ M.singleton (toTerm t) n

-- | An 'SoP' composed of a single symbol.
sym2SoP :: Ord u => u -> SoP u
sym2SoP sym = term2SoP (MS.singleton sym) 1

-- | An 'SoP' composed of a single constant.
int2SoP :: Ord u => Integer -> SoP u
int2SoP = term2SoP MS.empty

-- | Deletes a term from an 'SoP'. Warning: ignores the multiplicity
--   of the term---__not__ the same as subtraction!
deleteTerm :: (Foldable t, Ord u) => t u -> SoP u -> SoP u
deleteTerm t (SoP ts) = SoP $ M.delete (toTerm t) ts

-- | Inserts a term into an 'SoP'. Warning: ignores the multiplicity
--   of the term---__not__ the same as addition!
insertTerm :: (Foldable t, Ord u) => t u -> SoP u -> SoP u
insertTerm t (SoP ts) = SoP $ M.insert (toTerm t) 1 ts

--------------------------------------------------------------------------------
-- SoP arithmetic
--------------------------------------------------------------------------------

zeroSoP :: Ord u => SoP u
zeroSoP = SoP $ M.singleton (toTerm MS.empty) 0

scaleSoP :: Integer -> SoP u -> SoP u
scaleSoP k = mapSoP (* k)

negSoP :: SoP u -> SoP u
negSoP = scaleSoP (-1)

addSoPs :: Ord u => SoP u -> SoP u -> SoP u
addSoPs (SoP xs) (SoP ys) = normalize $ SoP $ M.unionWith (+) xs ys

(.+.) :: Ord u => SoP u -> SoP u -> SoP u
(.+.) = addSoPs

infixl 6 .+.

subSoPs :: Ord u => SoP u -> SoP u -> SoP u
subSoPs x y = x .+. negSoP y

(.-.) :: Ord u => SoP u -> SoP u -> SoP u
(.-.) = subSoPs

infixl 6 .-.

mulSoPs :: Ord u => SoP u -> SoP u -> SoP u
mulSoPs (SoP xs) (SoP ys) = normalize $ SoP $ M.fromListWith (+) $ do
  (x_term, x_n) <- M.toList xs
  (y_term, y_n) <- M.toList ys
  pure (x_term <> y_term, x_n * y_n)

(.*.) :: Ord u => SoP u -> SoP u -> SoP u
(.*.) = mulSoPs

infixl 7 .*.

-- | @'factorSoP' term sop = (a, b)@ where @sop = a*term + b@.
factorSoP :: (Foldable t, Ord u) => t u -> SoP u -> (SoP u, SoP u)
factorSoP fact sop = (sopFromList as, sopFromList bs)
  where
    fact' = toTerm fact
    as = mapMaybe (\(t, n) -> (,n) <$> t `divTerm` fact') $ sopToList sop
    bs = filter (not . (fact' `isFactorOf`) . fst) $ sopToList sop

-- | Division of 'SoP's. Handles the following cases:
--
--   1. @(qv + qv_1 * t_1 + ... + qv_n*t_n) / q@ results in
--      @'Just' (v + v_1*t_1 + ... + v_n*t_n)@
--
--   2. @(0 + v_1 * t_1 * t_q + ... + v_n * t_n * t_q) / t_q@
--      results in @'Just' (0 + v_1 * t_1 + ... + v_n * t_n)@.
--
--   Otherwise results in 'Nothing'. A possible generalization would
--   be to perform symbolically division with reminder, i.e., the
--   result would be two sum-of-products representing the quotient and
--   the reminder.
divSoPs :: Ord u => SoP u -> SoP u -> Maybe (SoP u)
divSoPs (SoP x) (SoP q_sop)
  | [q] <- M.toList q_sop = SoP . M.fromList <$> mapM (`divSoPTerm` q) (M.toList x)
  | otherwise = Nothing
  where
    divideVal v qv
      | v `mod` qv == 0 = Just $ v `div` qv
      | otherwise = Nothing
    divSoPTerm (term, v) (qterm, qv) =
      (,) <$> term `divTerm` qterm <*> v `divideVal` qv

(./.) :: Ord u => SoP u -> SoP u -> Maybe (SoP u)
(./.) = divSoPs

infixl 7 ./.

-- | Integer division of 'SoP's. Both 'SoP's must be constants.
divSoPInt :: Ord u => SoP u -> SoP u -> Maybe (SoP u)
divSoPInt x y =
  int2SoP <$> (div <$> justConstant x <*> justConstant y)

-- | Sign of a constant 'SoP'.
signumSoP :: Ord u => SoP u -> Maybe (SoP u)
signumSoP = fmap (int2SoP . signum) . justConstant

--------------------------------------------------------------------------------
-- SoP queries
--------------------------------------------------------------------------------

-- | How many terms does the 'SoP' have?
numTerms :: Ord u => SoP u -> Int
numTerms = length . getTerms . normalize

-- | Is the 'SoP' just a constant?
justConstant :: Ord u => SoP u -> Maybe Integer
justConstant sop
  | [(term, n)] <- sopToList $ padWithZero sop,
    isConstTerm term =
      Just n
  | otherwise = Nothing

-- | Is the 'SoP' just a single symbol?
justSym :: Ord u => SoP u -> Maybe u
justSym sop
  | [([x], 1)] <- sopToLists $ normalize sop = Just x
  | otherwise = Nothing

-- | Is the 'SoP' of the form a*x + b?
justAffine :: Ord u => SoP u -> Maybe (Integer, u, Integer)
justAffine sop
  | [([], a), ([x], m)] <- sopToLists $ padWithZero sop = Just (m, x, a)
  | otherwise = Nothing

-- | Is the 'SoP' a single term?
justSingleTerm :: Ord u => SoP u -> Maybe (Term u, Integer)
justSingleTerm sop
  | [t] <- sopToList $ normalize sop = Just t
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Free symbols in SoPs
--------------------------------------------------------------------------------

class Free u a where
  free :: a -> Set u

instance (Ord u, Free u a) => Free u (Set a) where
  free = foldMap free

instance Ord u => Free u (SoP u) where
  free = foldMap (MS.toSet . getTerm) . M.keys . getTerms

instance Ord u => Free u (Range u) where
  free r = free (lowerBound r) <> free (upperBound r)

--------------------------------------------------------------------------------
-- Substitutions in SoPs
--------------------------------------------------------------------------------

class Substitute a b c where
  substitute :: Map a b -> c -> c

substituteOne :: Substitute a b c => (a, b) -> c -> c
substituteOne (a, b) = substitute (M.singleton a b)

instance (Ord c, Substitute a b c) => Substitute a b (Set c) where
  substitute subst = S.map (substitute subst)

instance (Substitute a b c) => Substitute a b (Map k c) where
  substitute subst = fmap (substitute subst)

instance Ord u => Substitute u (SoP u) (SoP u) where
  substitute subst =
    SoP
      . M.unionsWith (+)
      . map
        ( \(term, n) ->
            getTerms $
              foldr (mulSoPs . lookupSubst) (int2SoP n) (termToList term)
        )
      . sopToList
    where
      lookupSubst u =
        case subst M.!? u of
          Nothing -> sym2SoP u
          Just sop -> sop

instance Ord u => Substitute u (SoP u) (Range u) where
  substitute subst (Range lb k ub) =
    Range (substitute subst lb) k (substitute subst ub)
