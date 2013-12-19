{-# LANGUAGE FlexibleContexts, QuasiQuotes #-}
module L0C.Backends.BohriumCodeGen (compileSOACtoBohrium) where

import Control.Applicative
import Control.Monad

import Data.List

import L0C.L0
import L0C.MonadFreshNames
import L0C.Backends.SimpleRepresentation
import L0C.Backends.GenericC

import L0C.HORepresentation.SOAC (SOAC)
import qualified L0C.HORepresentation.SOAC as SOAC
import L0C.HORepresentation.SOACNest (SOACNest(..))
import qualified L0C.HORepresentation.SOACNest as Nest

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

compileInput :: C.Exp -> C.Exp -> SOAC.Input -> CompilerM [C.BlockItem]

compileInput place shape (SOAC.Iota ne) = do
  nv <- new "iota_n"
  ne' <- compileExp (varExp nv) ne
  return $ stm [C.cstm|{
                     typename int64_t $id:nv;
                     $items:ne'
                     $exp:shape[0] = $id:nv;
                     $exp:place = bh_multi_array_int32_new_range(0, $id:nv-1, 0);
                   }|]

compileInput place shape inp = do
  let e = SOAC.inputToExp inp
  (arr, e') <- compileExpNewVar  e
  stride <- new "stride"
  let t = typeOf e
      d = arrayDims t
      strideStm 0 = [C.cexp|1|]
      strideStm i = [C.cexp|$exp:shape[$int:i-1] * $id:stride[$int:i-1]|]
      shapeStms = concat [ [[C.cstm|$exp:shape[$int:i] = $exp:se;|],
                            [C.cstm|$id:stride[$int:i] = $exp:(strideStm i);|]]
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
compileMap target (SOACNest [inp] (Nest.Map2 _ (Nest.Lambda l) _ _))
  | all (basicType . identType) $ tupleLambdaParams l,
    Just op <- compileLambda l unOp = do
      inputName <- new "map_input"
      outputName <- new "output"
      inp' <- compileInput (varExp inputName) [C.cexp|$exp:target.elem_0.dims|] inp
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
compileMap target (SOACNest [inp1, inp2] (Nest.Map2 _ (Nest.Lambda l) _ _))
  | all (basicType . identType) $ tupleLambdaParams l,
    Just op <- compileLambda l binOp = do
      inputName1 <- new "map_input_x"
      inputName2 <- new "map_input_y"
      outputName <- new "output"
      inp1' <- compileInput (varExp inputName1) [C.cexp|$exp:target.elem_0.dims|] inp1
      inp2' <- compileInput (varExp inputName2) [C.cexp|$exp:target.elem_0.dims|] inp2
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
compileReduce _ (SOACNest _ (Nest.Reduce2 _ (Nest.Lambda _) _ _ _)) =
  return Nothing
compileReduce _ _ = return Nothing

compileMapWithReduce :: C.Exp -> SOACNest -> CompilerM (Maybe [C.BlockItem])
compileMapWithReduce _ (SOACNest _ (Nest.Reduce2 _ (Nest.NewNest _ (Nest.Map2 {})) _ _ _)) =
  return Nothing
compileMapWithReduce _ _ = return Nothing

compileMapWithScan :: C.Exp -> SOACNest -> CompilerM (Maybe [C.BlockItem])
compileMapWithScan _ (SOACNest _ (Nest.Scan2 _ (Nest.NewNest _ (Nest.Map2 {})) _ _ _)) =
  return Nothing
compileMapWithScan _ _ = return Nothing

data BohriumUnOp = BohrIntInc Int

data BohriumBinOp = BohrIntSum

unOp :: [Parameter] -> Exp -> Maybe BohriumUnOp
unOp ps (BinOp Plus (Literal (IntVal x) _) (Var p1) _ _)
  | [toParam p1] `matches` ps = Just $ BohrIntInc x
unOp ps (BinOp Plus (Var p1) (Literal (IntVal x) _) _ _)
  | [toParam p1] `matches` ps = Just $ BohrIntInc x
unOp _ _ = Nothing

binOp :: [Parameter] -> Exp -> Maybe BohriumBinOp
binOp ps (BinOp Plus (Var p1) (Var p2) _ _)
  | [toParam p1, toParam p2] `matches` ps = Just BohrIntSum
binOp _ _ = Nothing

compileLambda :: TupleLambda -> ([Parameter] -> Exp -> Maybe a) -> Maybe a
compileLambda l f =
  case tupleLambdaBody l of
    LetPat (Id k1) op (TupLit [Var k2] _) _
      | k1 == k2 -> f (tupleLambdaParams l) op
    _ -> Nothing

doUnaryOperation :: BohriumUnOp -> String -> String -> C.Stm
doUnaryOperation (BohrIntInc x) outputName inputName =
  [C.cstm|$id:outputName = bh_multi_array_int32_add_scalar_rhs($id:inputName, $int:x);|]

doBinaryOperation :: BohriumBinOp -> String -> String -> String -> C.Stm
doBinaryOperation BohrIntSum outputName inputName1 inputName2 =
  [C.cstm|$id:outputName = bh_multi_array_int32_add($id:inputName1, $id:inputName2);|]

matches :: Ord a => [a] -> [a] -> Bool
matches xs ys = sort xs == sort ys
