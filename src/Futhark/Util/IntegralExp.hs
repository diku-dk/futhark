-- | It is occasionally useful to define generic functions that can
-- not only compute their result as an integer, but also as a symbolic
-- expression in the form of an AST.
--
-- There are some Haskell hacks for this - it is for example not hard
-- to define an instance of 'Num' that constructs an AST.  However,
-- this falls down for some other interesting classes, like
-- 'Integral', which requires both the problematic method
-- 'fromInteger', and also that the type is an instance of 'Enum'.
--
-- We can always just define hobbled instances that call 'error' for
-- those methods that are impractical, but this is ugly.
--
-- Hence, this module defines similes to standard Haskell numeric
-- typeclasses that have been modified to make generic functions
-- slightly easier to write.
module Futhark.Util.IntegralExp
  ( IntegralExp (..),
    Wrapped (..),
  )
where

import Data.Int
import Prelude

-- | A twist on the 'Integral' type class that is more friendly to
-- symbolic representations.
class (Num e) => IntegralExp e where
  quot :: e -> e -> e
  rem :: e -> e -> e
  div :: e -> e -> e
  mod :: e -> e -> e
  sgn :: e -> Maybe Int
  pow :: e -> e -> e

  -- | Like 'Futhark.Util.IntegralExp.div', but rounds towards
  -- positive infinity.
  divUp :: e -> e -> e
  divUp x y =
    (x + y - 1) `Futhark.Util.IntegralExp.div` y

  nextMul :: e -> e -> e
  nextMul x y = x `divUp` y * y

-- | This wrapper allows you to use a type that is an instance of the
-- true class whenever the simile class is required.
newtype Wrapped a = Wrapped {wrappedValue :: a}
  deriving (Eq, Ord, Show)

instance (Enum a) => Enum (Wrapped a) where
  toEnum a = Wrapped $ toEnum a
  fromEnum (Wrapped a) = fromEnum a

liftOp ::
  (a -> a) ->
  Wrapped a ->
  Wrapped a
liftOp op (Wrapped x) = Wrapped $ op x

liftOp2 ::
  (a -> a -> a) ->
  Wrapped a ->
  Wrapped a ->
  Wrapped a
liftOp2 op (Wrapped x) (Wrapped y) = Wrapped $ x `op` y

instance (Num a) => Num (Wrapped a) where
  (+) = liftOp2 (Prelude.+)
  (-) = liftOp2 (Prelude.-)
  (*) = liftOp2 (Prelude.*)
  abs = liftOp Prelude.abs
  signum = liftOp Prelude.signum
  fromInteger = Wrapped . Prelude.fromInteger
  negate = liftOp Prelude.negate

instance (Integral a) => IntegralExp (Wrapped a) where
  quot = liftOp2 Prelude.quot
  rem = liftOp2 Prelude.rem
  div = liftOp2 Prelude.div
  mod = liftOp2 Prelude.mod
  sgn = Just . fromIntegral . signum . toInteger . wrappedValue
  pow = liftOp2 (Prelude.^)
