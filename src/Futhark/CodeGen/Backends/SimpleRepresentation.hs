{-# LANGUAGE QuasiQuotes #-}
-- | Simple C runtime representation.
module Futhark.CodeGen.Backends.SimpleRepresentation
  ( sameRepresentation
  , sameRepresentation'
  , tupleField
  , tupleFieldExp
  , funName
  , allocArray
  , arraySliceCopyStm
  , indexArrayExp
  , indexArrayElemStms
  )
    where

import Data.List

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C

import qualified Futhark.CodeGen.Backends.CUtils as C
import Futhark.CodeGen.ImpCode

-- | True if both types map to the same runtime representation.  This
-- is the case if they are identical modulo uniqueness.
sameRepresentation :: [Type] -> [Type] -> Bool
sameRepresentation ets1 ets2
  | length ets1 == length ets2 =
    and $ zipWith sameRepresentation' ets1 ets2
  | otherwise = False

sameRepresentation' :: Type -> Type -> Bool
sameRepresentation' (Value (Type et1 shape1)) (Value (Type et2 shape2)) =
    length shape1 == length shape2 && et1 == et2
sameRepresentation' (Mem _) (Mem _) = True
sameRepresentation' _ _ = False

-- | @tupleField i@ is the name of field number @i@ in a tuple.
tupleField :: Int -> String
tupleField i = "elem_" ++ show i

-- | @tupleFieldExp e i@ is the expression for accesing field @i@ of
-- tuple @e@.  If @e@ is an lvalue, so will the resulting expression
-- be.
tupleFieldExp :: C.Exp -> Int -> C.Exp
tupleFieldExp e i = [C.cexp|$exp:e.$id:(tupleField i)|]

-- | @funName f@ is the name of the C function corresponding to
-- the Futhark function @f@.
funName :: Name -> String
funName = ("futhark_"++) . nameToString

-- | The size is in elements.
allocArray :: C.Exp -> [C.Exp] -> C.Type -> C.Stm
allocArray place shape basetype =
  [C.cstm|{$stms:shapeassign
           $exp:place.data = calloc($exp:sizeexp, sizeof($ty:basetype));}|]
  where sizeexp = C.product shape
        shapeassign = zipWith assign shape [0..]
        assign :: C.Exp -> Int -> C.Stm
        assign n i = [C.cstm|$exp:place.shape[$int:i] = $exp:n;|]

-- | @arraySliceCopyStm to from fromshape t slice@ is a @memcpy()@ statement copying
-- a slice of the array @from@ to the memory pointed at by @to@.
arraySliceCopyStm :: C.Exp -> C.Exp -> [C.Exp] -> Int -> C.Stm
arraySliceCopyStm to from fromshape slice =
  [C.cstm|memcpy($exp:to,
                 $exp:from,
                 $exp:(C.product $ drop slice fromshape)*sizeof(*$exp:from));|]

-- | Generate an expression indexing the given array with the given
-- indices.  No bounds checking is done.
indexArrayExp :: C.Exp -> [C.Exp] -> [C.Exp] -> C.Exp
indexArrayExp place shape indexes =
  let sizes = map C.product $ tails $ drop 1 shape
      mult x y = [C.cexp|$exp:x * $exp:y|]
      index = C.sum $ zipWith mult sizes indexes
  in [C.cexp|$exp:place.data[$exp:index]|]

-- | @indexArrayElemStms place from t idxs@ produces a statement that
-- stores the value at @from[idxs]@ in @place@.  In contrast to
-- 'indexArrayExp', this function takes care of creating proper size
-- information if the result is an array itself.
indexArrayElemStms :: C.Exp -> C.Exp -> [C.Exp] -> [C.Exp] -> [C.Stm]
indexArrayElemStms place from fromshape idxs =
  case drop (length idxs) fromshape of
    [] -> [[C.cstm|$exp:place = $exp:index;|]]
    dims ->
      let dimstms = [ [C.cstm|$exp:place.shape[$int:i] = $exp:dim;|] |
                      (i, dim) <- zip [(0::Int)..] dims ]
      in dimstms++[[C.cstm|$exp:place.data = &$exp:index;|]]
  where index = indexArrayExp from fromshape idxs
