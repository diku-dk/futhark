module Futhark.Solve.LP
  ( LP (..),
    LPE (..),
    convert,
    normalize,
    var,
    constant,
    cval,
    bin,
    or,
    oneIsZero,
    (~+~),
    (~-~),
    (~*~),
    (!),
    neg,
    linearProgToLP,
    linearProgToLPE,
    LSum (..),
    LinearProg (..),
    OptType (..),
    Constraint (..),
    (==),
    (<=),
    (>=),
    rowEchelonLPE,
  )
where

import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Vector.Unboxed (Unbox, Vector)
import Data.Vector.Unboxed qualified as V
import Debug.Trace
import Futhark.Solve.Matrix (Matrix (..))
import Futhark.Solve.Matrix qualified as M
import Prelude hiding (or, (<=), (==), (>=))
import Prelude qualified

-- | A linear program. 'LP c a d' represents the program
--
-- > maximize c^T * a
-- > subject to a * x <= d
-- >            x >= 0
--
-- The matrix 'a' is assumed to have linearly-independent rows.
data LP a = LP
  { lpc :: Vector a,
    lpA :: Matrix a,
    lpd :: Vector a
  }
  deriving (Eq, Show)

-- | Equational form of a linear program. 'LPE c a d' represents the
-- program
--
-- > maximize c^T * a
-- > subject to a * x = d
-- >            x >= 0
--
-- The matrix 'a' is assumed to have linearly-independent rows.
data LPE a = LPE
  { pc :: Vector a,
    pA :: Matrix a,
    pd :: Vector a
  }
  deriving (Eq, Show)

rowEchelonLPE :: (Show a, Unbox a, Fractional a, Ord a) => LPE a -> LPE a
rowEchelonLPE (LPE c a d) =
  LPE c (M.sliceCols (V.generate (ncols a) id) ad) (M.getCol (ncols a) ad)
  where
    ad =
      M.filterRows (V.any (Prelude./= 0)) $
        (M.rowEchelon $ a M.<|> M.fromColVector d)

-- | Converts an 'LP' into an equivalent 'LPE' by introducing slack
-- variables.
convert :: (Show a, Num a, Unbox a) => LP a -> LPE a
convert (LP c a d) = LPE c' a' d
  where
    a' = a M.<|> M.diagonal (V.replicate (M.nrows a) 1)
    c' = c V.++ V.replicate (M.nrows a) 0

-- | Linear sum of variables.
newtype LSum v a = LSum {lsum :: (Map (Maybe v) a)}
  deriving (Eq)

instance (Show v, Show a) => Show (LSum v a) where
  show (LSum m) =
    L.intercalate
      " + "
      $ map
        ( \(k, a) ->
            case k of
              Nothing -> show a
              Just k' -> show a <> "*" <> show k'
        )
      $ Map.toList m

instance Functor (LSum v) where
  fmap f (LSum m) = LSum $ fmap f m

-- | Type of constraint
data CType = Equal | LessEq
  deriving (Eq)

instance Show CType where
  show (Equal) = "="
  show (LessEq) = "<="

-- | A constraint for a linear program.
data Constraint v a
  = Constraint CType (LSum v a) (LSum v a)
  deriving (Eq)

instance (Show a, Show v) => Show (Constraint v a) where
  show (Constraint t l r) =
    show l <> " " <> show t <> " " <> show r

data OptType = Maximize | Minimize
  deriving (Show, Eq)

-- | A linear program.
data LinearProg v a = LinearProg
  { optType :: OptType,
    objective :: LSum v a,
    constraints :: [Constraint v a]
  }
  deriving (Eq)

instance (Show v, Show a) => Show (LinearProg v a) where
  show (LinearProg opt obj cs) =
    unlines $
      [ show opt,
        show obj,
        "subject to:"
      ]
        ++ map show cs

bigM :: (Num a) => a
bigM = 10 ^ 3

oneIsZero :: (Eq a, Num a, Ord v) => v -> v -> v -> v -> [Constraint v a]
oneIsZero b1 b2 x1 x2 =
  mkC b1 x1
    <> mkC b2 x2
    <> [(var b1 ~+~ var b2) <= constant 1]
  where
    mkC b x =
      [ var x <= bigM ~*~ var b
      ]

or :: (Eq a, Num a, Ord v) => v -> v -> Constraint v a -> Constraint v a -> [Constraint v a]
or b1 b2 c1 c2 =
  mkC b1 c1
    <> mkC b2 c2
    <> [var b1 ~+~ var b2 <= constant 1]
  where
    mkC b (Constraint Equal l r) =
      [ l <= r ~+~ bigM ~*~ (constant 1 ~-~ var b),
        l >= r ~-~ bigM ~*~ (constant 1 ~-~ var b)
      ]
    mkC b (Constraint LessEq l r) =
      [ l <= r ~+~ bigM ~*~ (constant 1 ~-~ var b)
      ]

bin :: (Num a, Ord v) => v -> Constraint v a
bin v = Constraint LessEq (var v) (constant 1)

(==) :: (Num a, Ord v) => LSum v a -> LSum v a -> Constraint v a
l == r = Constraint Equal l r

infix 4 ==

(<=) :: (Num a, Ord v) => LSum v a -> LSum v a -> Constraint v a
l <= r = Constraint LessEq l r

infix 4 <=

(>=) :: (Num a, Ord v) => LSum v a -> LSum v a -> Constraint v a
l >= r = Constraint LessEq (neg l) (neg r)

infix 4 >=

normalize :: (Eq a, Num a) => LSum v a -> LSum v a
normalize = LSum . Map.filter (/= 0) . lsum

var :: (Num a) => v -> LSum v a
var v = LSum $ Map.singleton (Just v) (fromInteger 1)

constant :: a -> LSum v a
constant = LSum . Map.singleton Nothing

cval :: (Num a, Ord v) => LSum v a -> a
cval = (! Nothing)

(~+~) :: (Eq a, Num a, Ord v) => LSum v a -> LSum v a -> LSum v a
(LSum x) ~+~ (LSum y) = normalize $ LSum $ Map.unionWith (+) x y

infixl 6 ~+~

(~-~) :: (Eq a, Num a, Ord v) => LSum v a -> LSum v a -> LSum v a
x ~-~ y = x ~+~ (neg y)

infixl 6 ~-~

(~*~) :: (Eq a, Num a, Ord v) => a -> LSum v a -> LSum v a
a ~*~ s = normalize $ fmap (a *) s

infixl 7 ~*~

(!) :: (Num a, Ord v) => LSum v a -> Maybe v -> a
(LSum m) ! v =
  case m Map.!? v of
    Nothing -> 0
    Just a -> a

neg :: (Num a, Ord v) => LSum v a -> LSum v a
neg (LSum x) = LSum $ fmap negate x

-- | Converts a linear program given with a list of constraints
-- into the standard form.
linearProgToLP ::
  forall v a.
  (Unbox a, Num a, Ord v, Eq a) =>
  LinearProg v a ->
  (LP a, Map Int v)
linearProgToLP (LinearProg otype obj cs) =
  (LP c a d, idxMap)
  where
    cs' = foldMap (convertEqCType . splitConstraint) cs
    idxMap =
      Map.fromList $
        zip [0 ..] $
          catMaybes $
            Map.keys $
              mconcat $
                map (lsum . fst) cs'
    mkRow s = V.generate (Map.size idxMap) $ \i -> s ! Just (idxMap Map.! i)
    c = mkRow $ convertObj otype obj
    a = M.fromVectors $ map (mkRow . fst) cs'
    d = V.fromList $ map snd cs'

    splitConstraint :: Constraint v a -> (CType, LSum v a, a)
    splitConstraint (Constraint ctype l r) =
      let c = negate $ cval (l ~-~ r)
       in (ctype, l ~-~ r ~-~ constant c, c)

    convertEqCType :: (CType, LSum v a, a) -> [(LSum v a, a)]
    convertEqCType (Equal, s, a) = [(s, a), (neg s, negate a)]
    convertEqCType (LessEq, s, a) = [(s, a)]

    convertObj :: OptType -> LSum v a -> LSum v a
    convertObj Maximize s = s
    convertObj Minimize s = neg s

-- | Converts a linear program given with a list of constraints
-- into the equational form. Assumes no <= constraints.
linearProgToLPE ::
  forall v a.
  (Unbox a, Num a, Ord v, Eq a) =>
  LinearProg v a ->
  (LPE a, Map Int v)
linearProgToLPE (LinearProg otype obj cs) =
  (LPE c a d, idxMap)
  where
    cs' = map (checkOnlyEqType . splitConstraint) cs
    idxMap =
      Map.fromList $
        zip [0 ..] $
          catMaybes $
            Map.keys $
              mconcat $
                map (lsum . fst) cs'
    mkRow s = V.generate (Map.size idxMap) $ \i -> s ! Just (idxMap Map.! i)
    c = mkRow $ convertObj otype obj
    a = M.fromVectors $ map (mkRow . fst) cs'
    d = V.fromList $ map snd cs'

    splitConstraint :: Constraint v a -> (CType, LSum v a, a)
    splitConstraint (Constraint ctype l r) =
      let c = negate $ cval (l ~-~ r)
       in (ctype, l ~-~ r ~-~ constant c, c)

    checkOnlyEqType :: (CType, LSum v a, a) -> (LSum v a, a)
    checkOnlyEqType (Equal, s, a) = (s, a)
    checkOnlyEqType (ctype, _, _) = error $ show ctype

    convertObj :: OptType -> LSum v a -> LSum v a
    convertObj Maximize s = s
    convertObj Minimize s = neg s

test1 :: LPE Double
test1 =
  LPE
    { pc = V.fromList [5.5, 2.1],
      pA =
        M.fromLists
          [ [-1, 1],
            [8, 2]
          ],
      pd = V.fromList [2, 17]
    }
