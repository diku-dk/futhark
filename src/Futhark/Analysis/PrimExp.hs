-- | A primitive expression is an expression where the non-leaves are
-- primitive operators.  Our representation does not guarantee that
-- the expression is type-correct.
module Futhark.Analysis.PrimExp
  ( PrimExp (..)
  , evalPrimExp
  , primExpType
  , coerceIntPrimExp

  , module Futhark.Representation.Primitive
  ) where

import           Control.Applicative
import           Data.Foldable
import           Data.Traversable

import           Prelude

import           Futhark.Representation.AST.Attributes.Names
import           Futhark.Representation.Primitive
import           Futhark.Util.IntegralExp
import           Futhark.Util.Pretty

-- | A primitive expression parametrised over the representation of free variables.
data PrimExp v = LeafExp v PrimType
               | ValueExp PrimValue
               | BinOpExp BinOp (PrimExp v) (PrimExp v)
               | CmpOpExp CmpOp (PrimExp v) (PrimExp v)
               | UnOpExp UnOp (PrimExp v)
               | ConvOpExp ConvOp (PrimExp v)
               deriving (Ord, Show)

-- The Eq instance upcoerces all integer constants to their largest
-- type before comparing for equality.  This is technically not a good
-- idea, but solves annoying problems related to the Num instance
-- always producing Int64s.
instance Eq v => Eq (PrimExp v) where
  LeafExp x xt == LeafExp y yt = x == y && xt == yt
  ValueExp (IntValue x) == ValueExp (IntValue y) =
    intToInt64 x == intToInt64 y
  BinOpExp xop x1 x2 == BinOpExp yop y1 y2 =
    xop == yop && x1 == y1 && x2 == y2
  CmpOpExp xop x1 x2 == CmpOpExp yop y1 y2 =
    xop == yop && x1 == y1 && x2 == y2
  UnOpExp xop x == UnOpExp yop y =
    xop == yop && x == y
  ConvOpExp xop x == ConvOpExp yop y =
    xop == yop && x == y
  _ == _ = False

instance Functor PrimExp where
  fmap = fmapDefault

instance Foldable PrimExp where
  foldMap = foldMapDefault

instance Traversable PrimExp where
  traverse f (LeafExp v t) =
    LeafExp <$> f v <*> pure t
  traverse _ (ValueExp v) =
    pure $ ValueExp v
  traverse f (BinOpExp op x y) =
    BinOpExp op <$> traverse f x <*> traverse f y
  traverse f (CmpOpExp op x y) =
    CmpOpExp op <$> traverse f x <*> traverse f y
  traverse f (ConvOpExp op x) =
    ConvOpExp op <$> traverse f x
  traverse f (UnOpExp op x) =
    UnOpExp op <$> traverse f x

instance FreeIn v => FreeIn (PrimExp v) where
  freeIn = foldMap freeIn

-- The Num instance performs a little bit of magic: whenever an
-- expression and a constant is combined with a binary operator, the
-- type of the constant may be changed to be the type of the
-- expression, if they are not already the same.  This permits us to
-- write e.g. @x * 4@, where @x@ is an arbitrary PrimExp, and have the
-- @4@ converted to the proper primitive type.  We also support
-- converting integers to floating point values, but not the other way
-- around.  All numeric instances assume unsigned integers for such
-- conversions.
--
-- We also perform simple constant folding, in particular to reduce
-- expressions to constants so that the above works.  However, it is
-- still a bit of a hack.
instance Pretty v => Num (PrimExp v) where
  x + y | zeroIshExp x = y
        | zeroIshExp y = x
        | IntType t <- primExpType x,
          Just z <- constFold (doBinOp $ Add t) x y = z
        | FloatType t <- primExpType x,
          Just z <- constFold (doBinOp $ FAdd t) x y = z
        | Just z <- msum [asIntOp Add x y, asFloatOp FAdd x y] = z
        | otherwise = numBad "+" (x,y)

  x - y | zeroIshExp y = x
        | IntType t <- primExpType x,
          Just z <- constFold (doBinOp $ Sub t) x y = z
        | FloatType t <- primExpType x,
          Just z <- constFold (doBinOp $ FSub t) x y = z
        | Just z <- msum [asIntOp Sub x y, asFloatOp FSub x y] = z
        | otherwise = numBad "-" (x,y)

  x * y | zeroIshExp x = x
        | zeroIshExp y = y
        | oneIshExp x = y
        | oneIshExp y = x
        | IntType t <- primExpType x,
          Just z <- constFold (doBinOp $ Mul t) x y = z
        | FloatType t <- primExpType x,
          Just z <- constFold (doBinOp $ FMul t) x y = z
        | Just z <- msum [asIntOp Mul x y, asFloatOp FMul x y] = z
        | otherwise = numBad "*" (x,y)

  abs x | IntType t <- primExpType x = UnOpExp (Abs t) x
        | FloatType t <- primExpType x = UnOpExp (FAbs t) x
        | otherwise = numBad "abs" x

  signum x | IntType t <- primExpType x = UnOpExp (SSignum t) x
           | otherwise = numBad "signum" x

  fromInteger = ValueExp . IntValue . Int64Value . fromInteger

instance Pretty v => IntegralExp (PrimExp v) where
  x `div` y | oneIshExp y = x
            | Just z <- msum [asIntOp SDiv x y, asFloatOp FDiv x y] = z
            | otherwise = numBad "div" (x,y)

  x `mod` y | Just z <- msum [asIntOp SMod x y] = z
            | otherwise = numBad "mod" (x,y)

  x `quot` y | oneIshExp y = x
             | Just z <- msum [asIntOp SQuot x y] = z
             | otherwise = numBad "quot" (x,y)

  x `rem` y | Just z <- msum [asIntOp SRem x y] = z
            | otherwise = numBad "rem" (x,y)


asIntOp :: (IntType -> BinOp) -> PrimExp v -> PrimExp v -> Maybe (PrimExp v)
asIntOp f x y
  | IntType t <- primExpType x,
    Just y' <- asIntExp t y = Just $ BinOpExp (f t) x y'
  | IntType t <- primExpType y,
    Just x' <- asIntExp t x = Just $ BinOpExp (f t) x' y
  | otherwise = Nothing

asIntExp :: IntType -> PrimExp v -> Maybe (PrimExp v)
asIntExp t e
  | primExpType e == IntType t = Just e
asIntExp t (ValueExp (IntValue v)) =
  Just $ ValueExp $ IntValue $ doSExt v t
asIntExp _ _ =
  Nothing

asFloatOp :: (FloatType -> BinOp) -> PrimExp v -> PrimExp v -> Maybe (PrimExp v)
asFloatOp f x y
  | FloatType t <- primExpType x,
    Just y' <- asFloatExp t y = Just $ BinOpExp (f t) x y'
  | FloatType t <- primExpType y,
    Just x' <- asFloatExp t x = Just $ BinOpExp (f t) x' y
  | otherwise = Nothing

asFloatExp :: FloatType -> PrimExp v -> Maybe (PrimExp v)
asFloatExp t e
  | primExpType e == FloatType t = Just e
asFloatExp t (ValueExp (FloatValue v)) =
  Just $ ValueExp $ FloatValue $ doFPConv v t
asFloatExp t (ValueExp (IntValue v)) =
  Just $ ValueExp $ FloatValue $ doSIToFP v t
asFloatExp _ _ =
  Nothing

constFold :: (PrimValue -> PrimValue -> Maybe PrimValue)
            -> PrimExp v -> PrimExp v
            -> Maybe (PrimExp v)
constFold f x y = do x' <- valueExp x
                     y' <- valueExp y
                     ValueExp <$> f x' y'

numBad :: Pretty a => String -> a -> b
numBad s x =
  error $ "Invalid argument to PrimExp method " ++ s ++ ": " ++ pretty x

-- | Evaluate a 'PrimExp' in the given monad.  Invokes 'fail' on type
-- errors.
evalPrimExp :: (Pretty v, Monad m) => (v -> m PrimValue) -> PrimExp v -> m PrimValue
evalPrimExp f (LeafExp v _) = f v
evalPrimExp _ (ValueExp v) = return v
evalPrimExp f (BinOpExp op x y) = do
  x' <- evalPrimExp f x
  y' <- evalPrimExp f y
  maybe (evalBad op (x,y)) return $ doBinOp op x' y'
evalPrimExp f (CmpOpExp op x y) = do
  x' <- evalPrimExp f x
  y' <- evalPrimExp f y
  maybe (evalBad op (x,y)) (return . BoolValue) $ doCmpOp op x' y'
evalPrimExp f (UnOpExp op x) = do
  x' <- evalPrimExp f x
  maybe (evalBad op x) return $ doUnOp op x'
evalPrimExp f (ConvOpExp op x) = do
  x' <- evalPrimExp f x
  maybe (evalBad op x) return $ doConvOp op x'

evalBad :: (Pretty a, Pretty b, Monad m) => a -> b -> m c
evalBad op arg = fail $ "evalPrimExp: Type error when applying " ++
                 pretty op ++ " to " ++ pretty arg

-- | The type of values returned by a 'PrimExp'.  This function
-- returning does not imply that the 'PrimExp' is type-correct.
primExpType :: PrimExp v -> PrimType
primExpType (LeafExp _ t)     = t
primExpType (ValueExp v)      = primValueType v
primExpType (BinOpExp op _ _) = binOpType op
primExpType CmpOpExp{}        = Bool
primExpType (UnOpExp op _)    = unOpType op
primExpType (ConvOpExp op _)  = snd $ convOpType op

-- | Is the expression a constant zero of some sort?
zeroIshExp :: PrimExp v -> Bool
zeroIshExp (ValueExp v) = zeroIsh v
zeroIshExp _            = False

-- | Is the expression a constant one of some sort?
oneIshExp :: PrimExp v -> Bool
oneIshExp (ValueExp v) = oneIsh v
oneIshExp _            = False

-- | Is the expression a constant value?
valueExp :: PrimExp v -> Maybe PrimValue
valueExp (ValueExp v) = Just v
valueExp _            = Nothing

-- | If the given 'PrimExp' is a constant of the wrong integer type,
-- coerce it to the given integer type.  This is a workaround for an
-- issue in the 'Num' instance.
coerceIntPrimExp :: IntType -> PrimExp v -> PrimExp v
coerceIntPrimExp t (ValueExp (IntValue v)) = ValueExp $ IntValue $ doSExt v t
coerceIntPrimExp _ e                       = e

-- Prettyprinting instances

instance Pretty v => Pretty (PrimExp v) where
  ppr (LeafExp v _)     = ppr v
  ppr (ValueExp v)      = ppr v
  ppr (BinOpExp op x y) = ppr op <+> parens (ppr x) <+> parens (ppr y)
  ppr (CmpOpExp op x y) = ppr op <+> parens (ppr x) <+> parens (ppr y)
  ppr (ConvOpExp op x)  = ppr op <+> parens (ppr x)
  ppr (UnOpExp op x)    = ppr op <+> parens (ppr x)
