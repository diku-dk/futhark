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
    min,
    max,
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
    Vars (..),
    CType (..),
    (~==~),
    (~<=~),
    (~>=~),
    rowEchelonLPE,
    isConstant,
  )
where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Vector.Unboxed (Unbox, Vector)
import Data.Vector.Unboxed qualified as V
import Futhark.Solve.Matrix (Matrix (..))
import Futhark.Solve.Matrix qualified as Matrix
import Futhark.Util.Pretty
import Language.Futhark.Pretty
import Prelude hiding (max, min, or)

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

rowEchelonLPE :: (Unbox a, Fractional a, Ord a) => LPE a -> LPE a
rowEchelonLPE (LPE c a d) =
  LPE c (Matrix.sliceCols (V.generate (ncols a) id) ad) (Matrix.getCol (ncols a) ad)
  where
    ad =
      Matrix.filterRows
        (V.any (Prelude./= 0))
        (Matrix.rowEchelon $ a Matrix.<|> Matrix.fromColVector d)

-- | Converts an 'LP' into an equivalent 'LPE' by introducing slack
-- variables.
convert :: (Num a, Unbox a) => LP a -> LPE a
convert (LP c a d) = LPE c' a' d
  where
    a' = a Matrix.<|> Matrix.diagonal (V.replicate (Matrix.nrows a) 1)
    c' = c V.++ V.replicate (Matrix.nrows a) 0

-- | Linear sum of variables.
newtype LSum v a = LSum {lsum :: Map (Maybe v) a}
  deriving (Show, Eq)

instance (IsName v, Pretty a, Eq a, Num a) => Pretty (LSum v a) where
  pretty (LSum m) =
    concatWith (surround " + ")
      $ map
        ( \(k, a) ->
            case k of
              Nothing -> pretty a
              Just k' -> (if a == 1 then mempty else pretty a <> "*") <> prettyName k'
        )
      $ M.toList m

isConstant :: (Ord v) => LSum v a -> Bool
isConstant (LSum m) = M.keysSet m `S.isSubsetOf` S.singleton Nothing

instance Functor (LSum v) where
  fmap f (LSum m) = LSum $ fmap f m

class Vars a v where
  vars :: a -> Set v

instance (Ord v) => Vars (LSum v a) v where
  vars = S.fromList . catMaybes . M.keys . lsum

-- | Type of constraint
data CType = Equal | LessEq
  deriving (Show, Eq)

instance Pretty CType where
  pretty Equal = "=="
  pretty LessEq = "<="

-- | A constraint for a linear program.
data Constraint v a
  = Constraint CType (LSum v a) (LSum v a)
  deriving (Show, Eq)

instance (IsName v, Pretty a, Eq a, Num a) => Pretty (Constraint v a) where
  pretty (Constraint t l r) =
    pretty l <+> pretty t <+> pretty r

instance (Ord v) => Vars (Constraint v a) v where
  vars (Constraint _ l r) = vars l <> vars r

data OptType = Maximize | Minimize
  deriving (Show, Eq)

instance Pretty OptType where
  pretty Maximize = "maximize"
  pretty Minimize = "minimize"

-- | A linear program.
data LinearProg v a = LinearProg
  { optType :: OptType,
    objective :: LSum v a,
    constraints :: [Constraint v a]
  }
  deriving (Show, Eq)

instance (IsName v, Pretty a, Eq a, Num a) => Pretty (LinearProg v a) where
  pretty (LinearProg opt obj cs) =
    vcat
      [ pretty opt,
        indent 2 $ pretty obj,
        "subject to",
        indent 2 $ vcat $ map pretty cs
      ]

instance (Ord v) => Vars (LinearProg v a) v where
  vars lp =
    vars (objective lp)
      <> foldMap vars (constraints lp)

bigM :: (Num a) => a
bigM = 2 ^ (10 :: Int)

-- max{x, y} = z
max :: (Num a, Ord v) => v -> LSum v a -> LSum v a -> LSum v a -> [Constraint v a]
max b x y z =
  [ z ~>=~ x,
    z ~>=~ y,
    z ~<=~ x ~+~ bigM ~*~ var b,
    z ~<=~ y ~+~ bigM ~*~ (constant 1 ~-~ var b)
  ]

-- min{x, y} = z
min :: (Num a, Ord v) => v -> v -> v -> v -> [Constraint v a]
min b x y z =
  [ var z ~<=~ var x,
    var z ~<=~ var y,
    var z ~>=~ var x ~-~ bigM ~*~ (constant 1 ~-~ var b),
    var z ~>=~ var y ~-~ bigM ~*~ var b
  ]

oneIsZero :: (Num a, Ord v) => (v, v) -> (v, v) -> [Constraint v a]
oneIsZero (b1, x1) (b2, x2) =
  mkC b1 x1
    <> mkC b2 x2
    <> [(var b1 ~+~ var b2) ~<=~ constant 1]
  where
    mkC b x =
      [ var x ~<=~ bigM ~*~ var b
      ]

or :: (Num a, Ord v) => v -> v -> Constraint v a -> Constraint v a -> [Constraint v a]
or b1 b2 c1 c2 =
  mkC b1 c1
    <> mkC b2 c2
    <> [var b1 ~+~ var b2 ~<=~ constant 1]
  where
    mkC b (Constraint Equal l r) =
      [ l ~<=~ r ~+~ bigM ~*~ (constant 1 ~-~ var b),
        l ~>=~ r ~-~ bigM ~*~ (constant 1 ~-~ var b)
      ]
    mkC b (Constraint LessEq l r) =
      [ l ~<=~ r ~+~ bigM ~*~ (constant 1 ~-~ var b)
      ]

bin :: (Num a) => v -> Constraint v a
bin v = Constraint LessEq (var v) (constant 1)

(~==~) :: LSum v a -> LSum v a -> Constraint v a
l ~==~ r = Constraint Equal l r

infix 4 ~==~

(~<=~) :: LSum v a -> LSum v a -> Constraint v a
l ~<=~ r = Constraint LessEq l r

infix 4 ~<=~

(~>=~) :: (Num a) => LSum v a -> LSum v a -> Constraint v a
l ~>=~ r = Constraint LessEq (neg l) (neg r)

infix 4 ~>=~

normalize :: (Eq a, Num a) => LSum v a -> LSum v a
normalize = LSum . M.filter (/= 0) . lsum

var :: (Num a) => v -> LSum v a
var v = LSum $ M.singleton (Just v) 1

constant :: a -> LSum v a
constant = LSum . M.singleton Nothing

cval :: (Num a, Ord v) => LSum v a -> a
cval = (! Nothing)

(~+~) :: (Ord v, Num a) => LSum v a -> LSum v a -> LSum v a
(LSum x) ~+~ (LSum y) = LSum $ M.unionWith (+) x y

infixl 6 ~+~

(~-~) :: (Ord v, Num a) => LSum v a -> LSum v a -> LSum v a
x ~-~ y = x ~+~ neg y

infixl 6 ~-~

(~*~) :: (Num a) => a -> LSum v a -> LSum v a
a ~*~ s = fmap (a *) s

infixl 7 ~*~

(!) :: (Num a, Ord v) => LSum v a -> Maybe v -> a
(LSum m) ! v = fromMaybe 0 (m M.!? v)

neg :: (Num a) => LSum v a -> LSum v a
neg (LSum x) = LSum $ fmap negate x

-- | Converts a linear program given with a list of constraints
-- into the standard form.
linearProgToLP ::
  forall v a.
  (Unbox a, Num a, Ord v) =>
  LinearProg v a ->
  (LP a, Map Int v)
linearProgToLP (LinearProg otype obj cs) =
  let c = mkRow $ convertObj otype obj
      a = Matrix.fromVectors $ map (mkRow . fst) cs'
      d = V.fromList $ map snd cs'
   in (LP c a d, idxMap)
  where
    cs' = foldMap (convertEqCType . splitConstraint) cs
    idxMap =
      M.fromList $
        zip [0 ..] $
          catMaybes $
            M.keys $
              mconcat $
                map (lsum . fst) cs'
    mkRow s = V.generate (M.size idxMap) $ \i -> s ! Just (idxMap M.! i)

    convertEqCType :: (CType, LSum v a, a) -> [(LSum v a, a)]
    convertEqCType (Equal, s, a) = [(s, a), (neg s, negate a)]
    convertEqCType (LessEq, s, a) = [(s, a)]

    splitConstraint :: Constraint v a -> (CType, LSum v a, a)
    splitConstraint (Constraint ctype l r) =
      let c = negate $ cval (l ~-~ r)
       in (ctype, l ~-~ r ~-~ constant c, c)

    convertObj :: OptType -> LSum v a -> LSum v a
    convertObj Maximize s = s
    convertObj Minimize s = neg s

-- | Converts a linear program given with a list of constraints
-- into the equational form. Assumes no <= constraints.
linearProgToLPE ::
  forall v a.
  (Unbox a, Num a, Ord v) =>
  LinearProg v a ->
  (LPE a, Map Int v)
linearProgToLPE (LinearProg otype obj cs) =
  let c = mkRow $ convertObj otype obj
      a = Matrix.fromVectors $ map (mkRow . fst) cs'
      d = V.fromList $ map snd cs'
   in (LPE c a d, idxMap)
  where
    cs' = map (checkOnlyEqType . splitConstraint) cs
    idxMap =
      M.fromList $
        zip [0 ..] $
          catMaybes $
            M.keys $
              mconcat $
                map (lsum . fst) cs'
    mkRow s = V.generate (M.size idxMap) $ \i -> s ! Just (idxMap M.! i)

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
