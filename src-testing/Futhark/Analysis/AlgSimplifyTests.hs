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
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Futhark.Analysis.AlgSimplify hiding (add, sub)
import Futhark.Analysis.PrimExp
import Futhark.IR.Syntax.Core
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "AlgSimplifyTests"
    [ testProperty "simplify is idempotent" $ \(TestableExp e) -> simplify e == simplify (simplify e),
      testProperty "simplify doesn't change exp evalutation result" $
        \(TestableExp e) ->
          evalPrimExp (\_ -> Nothing) e
            == evalPrimExp (\_ -> Nothing) (simplify e)
    ]

eval :: TestableExp -> Int64
eval (TestableExp e) = evalExp e

evalExp :: PrimExp VName -> Int64
evalExp (ValueExp (IntValue (Int64Value i))) = i
evalExp (BinOpExp (Add Int64 OverflowUndef) e1 e2) = evalExp e1 + evalExp e2
evalExp (BinOpExp (Sub Int64 OverflowUndef) e1 e2) = evalExp e1 - evalExp e2
evalExp (BinOpExp (Mul Int64 OverflowUndef) e1 e2) = evalExp e1 * evalExp e2
evalExp _ = undefined

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
