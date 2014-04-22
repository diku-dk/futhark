{-# LANGUAGE QuasiQuotes #-}
-- | Translate SOACs into calls to the Bohrium C API.  Quite unfinished.
--
-- The idea: For every SOAC expression we encounter, check whether it
-- can be directly translated to a Bohrium primitive.  If it can't,
-- the "Futhark.Backends.BohriumBackend" will translate it into a
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
module Futhark.CodeGen.Backends.BohriumCodeGen (compileSOACtoBohrium) where

import Control.Monad

import Data.List

import Futhark.InternalRep
import Futhark.MonadFreshNames
import Futhark.Backends.SimpleRepresentation
import Futhark.Backends.GenericC

import qualified Futhark.HORepresentation.SOAC as SOAC
import Futhark.HORepresentation.SOACNest (SOACNest(..))
import qualified Futhark.HORepresentation.SOACNest as Nest

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C

new :: String -> CompilerM String
new = liftM textual  . newVName

alt :: CompilerM (Maybe a) -> CompilerM (Maybe a) -> CompilerM (Maybe a)
alt x y = do x' <- x
             case x' of
               Just _  -> return x'
               Nothing -> y

compileSOACtoBohrium :: C.Exp -> Exp -> CompilerM (Maybe [C.BlockItem])
compileSOACtoBohrium target e
  | Right nest <- Nest.fromExp e =
      let try f = f target nest
      in try compileMap `alt`
         try compileReduce `alt`
         try compileMapWithReduce `alt`
         try compileMapWithScan
  | otherwise = return Nothing

compileInput :: C.Exp -> C.Exp -> SubExp -> CompilerM [C.BlockItem]

compileInput place shape e = do
  (arr, e') <- compileExpNewVar $ subExp e
  stride <- new "stride"
  let t = subExpType e
      d = arrayRank t
      strideStm 0 = [C.cexp|1|]
      strideStm i = [C.cexp|$exp:shape[$int:i-1] * $id:stride[$int:d-$int:i]|]
      shapeStms = concat [ [[C.cstm|$exp:shape[$int:i] = $exp:se;|],
                            [C.cstm|$id:stride[$int:d-$int:i-1] = $exp:(strideStm i);|]]
                           | (i, se) <- zip [(0::Int)..] $ arrayShapeExp (varExp arr) t ]
  return $ stm [C.cstm|{
                     typename int64_t $id:stride[$int:d];
                     $items:e'
                     $stms:shapeStms
                     $exp:place = bh_multi_array_int32_new_from_view
                                  (bh_multi_array_int32_create_base
                                   ($id:arr.data, $exp:(arraySizeExp (varExp arr) t)),
                                   $int:d, 0, $exp:shape, $id:stride);
                   }|]

compileMap :: C.Exp -> SOACNest -> CompilerM (Maybe [C.BlockItem])
compileMap target (SOACNest [SOAC.Input ts (SOAC.Var inp)] (Nest.Map _ (Nest.Fun l) _))
  | SOAC.nullTransforms ts,
    all (basicType . identType) $ lambdaParams l,
    Just op <- compileLambda l unOp = do
      inputName <- new "map_input"
      outputName <- new "output"
      inp' <- compileInput (varExp inputName) [C.cexp|$exp:target.elem_0.dims|] $ Var inp
      return $ Just $ stm [C.cstm|{
                                typename bh_multi_array_int32_p $id:inputName;
                                typename bh_multi_array_int32_p $id:outputName;
                                $items:inp';
                                $stm:(doUnaryOperation op outputName inputName);
                                bh_multi_array_int32_sync($id:outputName);
                                $exp:target.elem_0.data =
                                  bh_multi_array_int32_get_base_data
                                    (bh_multi_array_int32_get_base($id:outputName));
                              }|]
compileMap target (SOACNest [SOAC.Input ts1 (SOAC.Var inp1),
                             SOAC.Input ts2 (SOAC.Var inp2)]
                   (Nest.Map _ (Nest.Fun l) _))
  | SOAC.nullTransforms ts1,
    SOAC.nullTransforms ts2,
    all (basicType . identType) $ lambdaParams l,
    Just op <- compileLambda l binOp = do
      inputName1 <- new "map_input_x"
      inputName2 <- new "map_input_y"
      outputName <- new "output"
      inp1' <- compileInput (varExp inputName1) [C.cexp|$exp:target.elem_0.dims|] $ Var inp1
      inp2' <- compileInput (varExp inputName2) [C.cexp|$exp:target.elem_0.dims|] $ Var inp2
      return $ Just $ stm [C.cstm|{
                                typename bh_multi_array_int32_p $id:inputName1;
                                typename bh_multi_array_int32_p $id:inputName2;
                                typename bh_multi_array_int32_p $id:outputName;
                                $items:inp1'; $items:inp2';
                                $stm:(doBinaryOperation op outputName inputName1 inputName2);
                                bh_multi_array_int32_sync($id:outputName);
                                $exp:target.elem_0.data =
                                  bh_multi_array_int32_get_base_data
                                    (bh_multi_array_int32_get_base($id:outputName));
                              }|]
compileMap _ _ = return Nothing

compileReduce :: C.Exp -> SOACNest -> CompilerM (Maybe [C.BlockItem])
compileReduce _ (SOACNest _ (Nest.Reduce _ (Nest.Fun _) _ _)) =
  return Nothing
compileReduce _ _ = return Nothing

compileMapWithReduce :: C.Exp -> SOACNest -> CompilerM (Maybe [C.BlockItem])
compileMapWithReduce _ (SOACNest _ (Nest.Reduce _ (Nest.NewNest _ (Nest.Map {})) _ _)) =
  return Nothing
compileMapWithReduce _ _ = return Nothing

compileMapWithScan :: C.Exp -> SOACNest -> CompilerM (Maybe [C.BlockItem])
compileMapWithScan _ (SOACNest _ (Nest.Scan _ (Nest.NewNest _ (Nest.Map {})) _ _)) =
  return Nothing
compileMapWithScan _ _ = return Nothing

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

doUnaryOperation :: BohriumUnOp -> String -> String -> C.Stm
doUnaryOperation (BohrIntInc x) outputName inputName =
  [C.cstm|$id:outputName = bh_multi_array_int32_add_scalar_rhs($id:inputName, $int:x);|]

doBinaryOperation :: BohriumBinOp -> String -> String -> String -> C.Stm
doBinaryOperation op outputName inputName1 inputName2 =
  [C.cstm|$id:outputName = $id:opfun($id:inputName1, $id:inputName2);|]
    where opfun = case op of
                    BohrIntSum   -> "bh_multi_array_int32_add"
                    BohrIntSub   -> "bh_multi_array_int32_subtract"
                    BohrIntDiv   -> "bh_multi_array_int32_divide"
                    BohrIntMod   -> "bh_multi_array_int32_modulo"
                    BohrIntXor   -> "bh_multi_array_int32_bitwise_xor"
                    BohrIntOr    -> "bh_multi_array_int32_bitwise_or"
                    BohrIntAnd   -> "bh_multi_array_int32_bitwise_and"
                    BohrIntMult  -> "bh_multi_array_int32_multiply"
                    BohrIntEqual -> "bh_multi_array_int32_equal_to"
                    BohrIntLess  -> "bh_multi_array_int32_less_than"
                    BohrIntLeq   -> "bh_multi_array_int32_less_than_or_equal_to"

matches :: Ord a => [a] -> [a] -> Bool
matches xs ys = sort xs == sort ys
