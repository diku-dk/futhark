-- | Translate SOACs into Bohrium operations.  Quite unfinished.
--
-- The idea: For every SOAC expression we encounter, check whether it
-- can be directly translated to a Bohrium primitive.  If it can't,
-- "Futhark.CodeGen.Backends.BohriumBackend" will translate it into a
-- sequential loop for us, but that's obviously not something we want
-- to happen.  Hence, another (currently unwritten) compiler pass
-- should do aggressive loop fission and other transformations in
-- order to make the program fit patterns recognised by this module.
--
-- For example, the SOAC @map(fn int (int x) => x+2,a)@ corresponds
-- nicely to the Bohrium function
-- @bh_multi_array_int32_add_scalar_rhs@.  And @map(fn int (int x, int
-- y) => x+y,zip(a,b))@ is @bh_multi_array_int32_add@.  This module
-- should eventually recognise all such simple patterns.
--
-- Current state: Simple unary and binary mappings across integer
-- arrays can be translated, nothing else.  Also significantly, arrays
-- are copied to Bohrium space before every operation, and back when
-- it's done.  This is massively wasteful.
module Futhark.CodeGen.Backends.BohriumOp
  ( compileSOACtoBohrium
  , BohriumOp (..)
  , BohriumBinOp (..)
  , BohriumUnOp (..)
  ) where

import Control.Monad

import Data.List

import Futhark.InternalRep

import qualified Futhark.Analysis.HORepresentation.SOAC as SOAC
import Futhark.Analysis.HORepresentation.SOACNest (SOACNest(..))
import qualified Futhark.Analysis.HORepresentation.SOACNest as Nest

import qualified Futhark.CodeGen.ImpCode as Imp
import qualified Futhark.CodeGen.ImpGen as ImpGen

data BohriumOp = UnOpMap BohriumUnOp (VName, Imp.Type)
               | BinOpMap BohriumBinOp (VName, Imp.Type) (VName, Imp.Type)

data BohriumUnOp = BohrIntInc Int

data BohriumBinOp = BohrIntSum
                  | BohrIntSub
                  | BohrIntMult
                  | BohrIntDiv
                  | BohrIntMod
                  | BohrIntXor
                  | BohrIntOr
                  | BohrIntAnd
                  | BohrIntEqual
                  | BohrIntLess
                  | BohrIntLeq

compileSOACtoBohrium :: Exp -> Maybe BohriumOp
compileSOACtoBohrium e
  | Right nest <- Nest.fromExp e =
      msum $ map ($nest) [ compileMap
                         , compileReduce
                         , compileMapWithReduce
                         , compileMapWithScan
                         ]
  | otherwise = Nothing

input :: Ident -> (VName, Imp.Type)
input v = (identName v, ImpGen.compileType $ identType v)

compileMap :: SOACNest -> Maybe BohriumOp

compileMap (SOACNest [SOAC.Input ts (SOAC.Var inp)] (Nest.Map _ (Nest.Fun l) _))
  | SOAC.nullTransforms ts,
    all (basicType . identType) $ lambdaParams l,
    Just op <- compileLambda l unOp =
      Just $ UnOpMap op $ input inp

compileMap (SOACNest [SOAC.Input ts1 (SOAC.Var inp1),
                      SOAC.Input ts2 (SOAC.Var inp2)]
            (Nest.Map _ (Nest.Fun l) _))
  | SOAC.nullTransforms ts1,
    SOAC.nullTransforms ts2,
    all (basicType . identType) $ lambdaParams l,
    Just op <- compileLambda l binOp =
      Just $ BinOpMap op (input inp1) (input inp2)

compileMap _ = Nothing

compileReduce :: SOACNest -> Maybe BohriumOp
compileReduce (SOACNest _ (Nest.Reduce _ (Nest.Fun _) _ _)) =
  Nothing
compileReduce _ = Nothing

compileMapWithReduce :: SOACNest -> Maybe BohriumOp
compileMapWithReduce (SOACNest _ (Nest.Reduce _ (Nest.NewNest _ (Nest.Map {})) _ _)) =
  Nothing
compileMapWithReduce _ = Nothing

compileMapWithScan :: SOACNest -> Maybe BohriumOp
compileMapWithScan (SOACNest _ (Nest.Scan _ (Nest.NewNest _ (Nest.Map {})) _ _)) =
  Nothing
compileMapWithScan _ = Nothing

unOp :: [Param] -> Exp -> Maybe BohriumUnOp
unOp ps (BinOp Plus (Constant (BasicVal (IntVal x)) _) (Var p1) _ _)
  | [toParam p1] == ps = Just $ BohrIntInc x
unOp ps (BinOp Plus (Var p1) (Constant (BasicVal (IntVal x)) _) _ _)
  | [toParam p1] == ps = Just $ BohrIntInc x
unOp _ _ = Nothing

binOp :: [Param] -> Exp -> Maybe BohriumBinOp
binOp ps (BinOp op (Var p1) (Var p2) _ _)
  | [toParam p1, toParam p2] `matches` ps = op'
  where op' = liftM snd $ find ((==op) . fst)
              [(Plus,BohrIntSum),
               (Minus, BohrIntSub),
               (Times, BohrIntMult),
               (Divide, BohrIntDiv),
               (Mod, BohrIntMod),
               (Band, BohrIntAnd),
               (Xor, BohrIntXor),
               (Bor, BohrIntOr),
               (Equal, BohrIntEqual),
               (Less, BohrIntLess),
               (Leq, BohrIntLeq)]

binOp _ _ = Nothing

compileLambda :: Lambda -> ([Param] -> Exp -> Maybe a) -> Maybe a
compileLambda l f =
  case lambdaBody l of
    Body [Let [k1] op] (Result _ [Var k2] _)
      | k1 == k2 -> f (lambdaParams l) op
    _ -> Nothing

matches :: Ord a => [a] -> [a] -> Bool
matches xs ys = sort xs == sort ys
