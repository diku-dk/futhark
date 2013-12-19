{-# LANGUAGE FlexibleContexts, QuasiQuotes #-}
module L0C.Backends.BohriumCodeGen (compileExp) where

import Control.Applicative
import Control.Monad

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

compileExp :: C.Exp -> Exp -> CompilerM (Maybe [C.BlockItem])
compileExp target e
  | Right nest <- Nest.fromExp e =
      let try f = f target nest
      in try compileMap `alt`
         try compileReduce `alt`
         try compileMapWithReduce `alt`
         try compileMapWithScan
  | otherwise = return Nothing

compileInput :: C.Exp -> C.Exp -> SOAC.Input -> CompilerM [C.BlockItem]
compileInput place shape (SOAC.Var idd) = do
  arr <- lookupVar $ identName idd
  stride <- new "stride"
  let t = identType idd
      d = arrayDims t
      strideStm 0 = [C.cexp|1|]
      strideStm i = [C.cexp|$exp:shape[$int:i-1] * $id:stride[$int:i-1]|]
      shapeStms = concat [ [[C.cstm|$exp:shape[$int:i] = $exp:e;|],
                            [C.cstm|$id:stride[$int:i] = $exp:(strideStm i);|]]
                           | (i, e) <- zip [(0::Int)..] $ arrayShapeExp arr t ]
  return $ stm [C.cstm|{
                     typename int64_t $id:stride[$int:d];
                     $stms:shapeStms
                     $exp:place = bh_multi_array_int32_new_from_view
                                  (bh_multi_array_int32_create_base($exp:arr.data, $exp:(arraySizeExp arr t)),
                                   $int:d, 0, $exp:shape, $id:stride);
                   }|]
compileInput _ _ _ = fail "Can only handle Var inputs"

data BohriumOp = BohrIntInc Int

compileLambda :: TupleLambda -> Maybe BohriumOp
compileLambda l =
  case tupleLambdaBody l of
    LetPat (Id k1) op (TupLit [Var k2] _) _
      | Just op' <- knownOp op, k1 == k2 -> Just op'
      | otherwise                        -> Nothing
    _ -> Nothing
  where knownOp (BinOp Plus (Literal (IntVal x) _) (Var p1) _ _)
          | [toParam p1] == tupleLambdaParams l = Just $ BohrIntInc x
        knownOp (BinOp Plus (Var p1) (Literal (IntVal x) _) _ _)
          | [toParam p1] == tupleLambdaParams l = Just $ BohrIntInc x
        knownOp _ = Nothing


doOperation :: BohriumOp -> String -> String -> C.Stm
doOperation (BohrIntInc x) outputName inputName =
  [C.cstm|$id:outputName = bh_multi_array_int32_add_scalar_rhs($id:inputName, $int:x);|]

compileMap :: C.Exp -> SOACNest -> CompilerM (Maybe [C.BlockItem])
compileMap target (SOACNest [inp] (Nest.Map2 _ (Nest.Lambda l) _ _))
  | all (basicType . identType) $ tupleLambdaParams l,
    Just op <- compileLambda l = do
      inputName <- new "map_input"
      outputName <- new "output"
      inp' <- compileInput (varExp inputName) [C.cexp|$exp:target.elem_0.dims|] inp
      return $ Just $ stm [C.cstm|{
                                typename bh_multi_array_int32_p $id:inputName;
                                typename bh_multi_array_int32_p $id:outputName;
                                $items:inp';
                                $stm:(doOperation op outputName inputName);
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
