{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Futhark.Analysis.AlgSimplifyTests
  ( tests,
  )
where

import Control.Monad
import Data.Function ((&))
import Data.List (subsequences)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Futhark.Analysis.AlgSimplify
import Futhark.Analysis.PrimExp
import Futhark.IR.Syntax.Core
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "AlgSimplifyTests"
    [ testProperty "simplifySum does not change evaluation result" $ \s -> eval s == eval (simplifySum s),
      testProperty "simplifySum is idempotent" $ \s -> simplifySum' s == (simplifySum' s >>= simplifySum'),
      testProperty "simplifySum doesn't change exp evalutation result" $
        \(TestableExp e) ->
          evalPrimExp (\_ -> Nothing) e
            == evalPrimExp (\_ -> Nothing) (untyped $ simplify mempty (TPrimExp e))
    ]

eval :: Sum -> Int64
eval (Sum _ terms) =
  sum $ map evalTerm terms

evalTerm :: Term -> Int64
evalTerm (Product _ []) = 0
evalTerm (Product _ atoms) =
  product $ map evalAtom atoms
evalTerm (Negated t) = (0 -) $ evalTerm t

evalAtom :: Atom -> Int64
evalAtom (Exp (ValueExp (IntValue (Int64Value i)))) = i
evalAtom (NestedSum s) = eval s
evalAtom _ = undefined

instance Arbitrary Sum where
  arbitrary = fmap (Sum OverflowUndef) arbitrary
  shrink (Sum o terms) = map (Sum o) $ concatMap shrink $ subsequences terms

instance Arbitrary Term where
  arbitrary = do
    n <- getSize
    if n <= 1
      then fmap (Product OverflowUndef) arbitrary
      else
        oneof
          [ scale (`div` 2) $ fmap (Product OverflowUndef) arbitrary,
            scale (`div` 2) $ fmap Negated arbitrary
          ]
  shrink (Negated t) = map Negated (shrink t) ++ shrink t
  shrink (Product o atoms) = map (Product o) $ concatMap shrink $ subsequences atoms

instance Arbitrary Atom where
  arbitrary = do
    n <- getSize
    if n <= 1
      then fmap (Exp . ValueExp . IntValue . Int64Value) arbitrary
      else
        oneof
          [ scale (`div` 2) $ fmap (Exp . ValueExp . IntValue . Int64Value) arbitrary,
            scale (`div` 2) $ fmap NestedSum arbitrary
          ]
  shrink (Exp e) = []
  shrink (NestedSum s) = map NestedSum $ shrink s

add :: PrimExp VName -> PrimExp VName -> PrimExp VName
add = BinOpExp (Add Int64 OverflowUndef)

sub :: PrimExp VName -> PrimExp VName -> PrimExp VName
sub = BinOpExp (Sub Int64 OverflowUndef)

mul :: PrimExp VName -> PrimExp VName -> PrimExp VName
mul = BinOpExp (Mul Int64 OverflowUndef)

neg :: PrimExp VName -> PrimExp VName
neg = BinOpExp (Sub Int64 OverflowUndef) (val 0)

l :: Int -> PrimExp VName
l i = LeafExp (VName (nameFromString $ show i) i) (IntType Int64)

val :: Int64 -> PrimExp VName
val = ValueExp . IntValue . Int64Value

generateExp :: Gen (PrimExp VName)
generateExp = do
  n <- getSize
  if n <= 1
    then val <$> arbitrary
    else
      oneof
        [ scale (`div` 2) $ generateBinOp add,
          scale (`div` 2) $ generateBinOp sub,
          scale (`div` 2) $ generateBinOp mul,
          scale (`div` 2) generateNeg,
          val <$> arbitrary
        ]

generateBinOp :: (PrimExp VName -> PrimExp VName -> PrimExp VName) -> Gen (PrimExp VName)
generateBinOp op = do
  t1 <- generateExp
  op t1 <$> generateExp

generateNeg :: Gen (PrimExp VName)
generateNeg =
  do neg <$> generateExp

newtype TestableExp = TestableExp (PrimExp VName)
  deriving (Show)

instance Arbitrary TestableExp where
  arbitrary = TestableExp <$> generateExp
