{-# LANGUAGE QuasiQuotes #-}
module Futhark.CodeGen.Backends.BohriumCodeGen
  ( bohriumCompiler
  ) where

import Control.Monad

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C

import Futhark.MonadFreshNames

import Futhark.CodeGen.Backends.GenericC
import qualified Futhark.CodeGen.Backends.CUtils as C
import Futhark.CodeGen.ImpCode
import Futhark.CodeGen.Backends.BohriumOp

new :: MonadFreshNames m => String -> m String
new = liftM textual  . newVName

bohriumCompiler :: OpCompiler (C.Exp, BohriumOp)
bohriumCompiler (target, UnOpMap op input) = do
  inputName <- new "map_input"
  outputName <- new "output"
  inp' <- compileInput (C.var inputName) [C.cexp|$exp:target.elem_0.dims|] input
  stm [C.cstm|{
            typename bh_multi_array_int32_p $id:inputName;
            typename bh_multi_array_int32_p $id:outputName;
            $stm:inp';
            $stm:(doUnaryOperation op outputName inputName);
            bh_multi_array_int32_sync($id:outputName);
            $exp:target.elem_0.data =
              bh_multi_array_int32_get_base_data
                (bh_multi_array_int32_get_base($id:outputName));
          }|]
  return Done

bohriumCompiler (target, BinOpMap op input1 input2) = do
  inputName1 <- new "map_input_x"
  inputName2 <- new "map_input_y"
  outputName <- new "output"
  inp1' <- compileInput (C.var inputName1) [C.cexp|$exp:target.elem_0.dims|] input1
  inp2' <- compileInput (C.var inputName2) [C.cexp|$exp:target.elem_0.dims|] input2
  stm [C.cstm|{
            typename bh_multi_array_int32_p $id:inputName1;
            typename bh_multi_array_int32_p $id:inputName2;
            typename bh_multi_array_int32_p $id:outputName;
            $stm:inp1'; $stm:inp2';
            $stm:(doBinaryOperation op outputName inputName1 inputName2);
            bh_multi_array_int32_sync($id:outputName);
            $exp:target.elem_0.data =
              bh_multi_array_int32_get_base_data
                (bh_multi_array_int32_get_base($id:outputName));
          }|]
  return Done

compileInput :: MonadFreshNames m =>
                C.Exp -> C.Exp -> (VName, Type) -> m C.Stm
compileInput place shape (inp,t) = do
  stride <- new "stride"
  let inp' = textual inp
      d = typeRank t
      strideStm 0 = [C.cexp|1|]
      strideStm i = [C.cexp|$exp:shape[$int:i-1] * $id:stride[$int:d-$int:i]|]
      shapeStms = concat [ [[C.cstm|$exp:shape[$int:i] = $exp:se;|],
                            [C.cstm|$id:stride[$int:d-$int:i-1] = $exp:(strideStm i);|]]
                           | (i, se) <- zip [(0::Int)..] $ typeShape t ]
  return [C.cstm|{
               typename int64_t $id:stride[$int:d];
               $stms:shapeStms
               $exp:place = bh_multi_array_int32_new_from_view
                            (bh_multi_array_int32_create_base
                             ($id:inp'.data, $exp:(C.product $ typeShape t)),
                             $int:d, 0, $exp:shape, $id:stride);
             }|]

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
