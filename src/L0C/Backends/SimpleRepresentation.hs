{-# LANGUAGE QuasiQuotes #-}
-- | Simple C runtime representation.
module L0C.Backends.SimpleRepresentation
  ( varExp
  , sameRepresentation
  , tupleField
  , tupleFieldExp
  , funName
  , allocArray
  , arraySliceCopyStm
  , arraySizeExp
  , arraySliceSizeExp
  , arrayShapeExp
  , indexArrayExp
  , indexArrayElemStms
  , boundsCheckStm
  , stm
  , stms
  )
    where

import L0C.L0

import Data.List

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C

-- | Turn a name into a C expression consisting of just that name.
varExp :: String -> C.Exp
varExp k = [C.cexp|$id:k|]

-- | True if both types map to the same runtime representation.  This
-- is the case if they are identical modulo uniqueness.
sameRepresentation :: Eq (als VName) => GenType als -> GenType als -> Bool
sameRepresentation (Elem (Tuple ets1)) (Elem (Tuple ets2))
  | length ets1 == length ets2 =
    and $ zipWith sameRepresentation ets1 ets2
sameRepresentation (Array et1 ds1 _ _) (Array et2 ds2 _ _) =
  length ds1 == length ds2 && sameRepresentation (Elem et1) (Elem et2)
sameRepresentation t1 t2 = t1 == t2

-- | @tupleField i@ is the name of field number @i@ in a tuple.
tupleField :: Int -> String
tupleField i = "elem_" ++ show i

-- | @tupleFieldExp e i@ is the expression for accesing field @i@ of
-- tuple @e@.  If @e@ is an lvalue, so will the resulting expression
-- be.
tupleFieldExp :: C.Exp -> Int -> C.Exp
tupleFieldExp e i = [C.cexp|$exp:e.$id:(tupleField i)|]

-- | @funName f@ is the name of the C function corresponding to
-- the L0 function @f@.
funName :: Name -> String
funName = ("l0_"++) . nameToString

-- | The size is in elements.
allocArray :: C.Exp -> [C.Exp] -> C.Type -> C.Stm
allocArray place shape basetype =
  [C.cstm|{$stms:shapeassign
           $exp:place.data = calloc($exp:sizeexp, sizeof($ty:basetype));}|]
  where sizeexp = foldl mult [C.cexp|1|] shape
        mult x y = [C.cexp|$exp:x * $exp:y|]
        shapeassign = zipWith assign shape [0..]
        assign :: C.Exp -> Int -> C.Stm
        assign n i = [C.cstm|$exp:place.shape[$int:i] = $exp:n;|]

-- | @arraySliceCopyStm to from t slice@ is a @memcpy()@ statement copying
-- a slice of the array @from@ to the memory pointed at by @to@.
arraySliceCopyStm :: C.Exp -> C.Exp -> Type -> Int -> C.Stm
arraySliceCopyStm to from t slice =
  [C.cstm|memcpy($exp:to,
                 $exp:from.data,
                 $exp:(arraySliceSizeExp from t slice)*sizeof($exp:from.data[0]));|]

-- | The size of the array of the given L0 type, as an integer.
arraySizeExp :: C.Exp -> Type -> C.Exp
arraySizeExp place t = arraySliceSizeExp place t 0

-- | Return an expression giving the array slice size in elements.
-- The integer argument is the number of dimensions sliced off.
arraySliceSizeExp :: C.Exp -> Type -> Int -> C.Exp
arraySliceSizeExp place t slice =
  foldl comb [C.cexp|1|] [slice..arrayRank t-1]
  where comb y i = [C.cexp|$exp:place.shape[$int:i] * $exp:y|]

-- | Return an list of expressions giving the array shape in elements.
arrayShapeExp :: C.Exp -> GenType als -> [C.Exp]
arrayShapeExp place t =
  map comb [0..arrayRank t-1]
  where comb i = [C.cexp|$exp:place.shape[$int:i]|]

-- | Generate an expression indexing the given array with the given
-- indices.  No bounds checking is done.
indexArrayExp :: C.Exp -> GenType als -> [C.Exp] -> C.Exp
indexArrayExp place t indexes =
  let sizes = map (foldl mult [C.cexp|1|]) $ tails $ map field [1..arrayRank t - 1]
      field :: Int -> C.Exp
      field i = [C.cexp|$exp:place.shape[$int:i]|]
      mult x y = [C.cexp|$exp:x * $exp:y|]
      add x y = [C.cexp|$exp:x + $exp:y|]
      index = foldl add [C.cexp|0|] $ zipWith mult sizes indexes
  in [C.cexp|$exp:place.data[$exp:index]|]

-- | @indexArrayElemStms place from t idxs@ produces a statement that
-- stores the value at @from[idxs]@ in @place@.  In contrast to
-- 'indexArrayExp', this function takes care of creating proper size
-- information if the result is an array itself.
indexArrayElemStms :: C.Exp -> C.Exp -> GenType als -> [C.Exp] -> [C.Stm]
indexArrayElemStms place from t idxs =
  case drop (length idxs) $ arrayShapeExp from t of
    [] -> [[C.cstm|$exp:place = $exp:index;|]]
    dims ->
      let dimstms = [ [C.cstm|$exp:place.shape[$int:i] = $exp:dim;|] |
                      (i, dim) <- zip [(0::Int)..] dims ]
      in dimstms++[[C.cstm|$exp:place.data = &$exp:index;|]]
  where index = indexArrayExp from t idxs

boundsCheckStm :: C.Exp -> [C.Exp] -> [C.Stm]
boundsCheckStm place idxs = zipWith check idxs [0..]
  where check :: C.Exp -> Int -> C.Stm
        check var i = [C.cstm|if ($exp:var < 0 || $exp:var >= $exp:place.shape[$int:i]) {
                            error(1, "Array index out of bounds.\n");
                          }|]

stm :: C.Stm -> [C.BlockItem]
stm (C.Block items _) = items
stm (C.Default s _) = stm s
stm s = [[C.citem|$stm:s|]]

stms :: [C.Stm] -> [C.BlockItem]
stms = map $ \s -> [C.citem|$stm:s|]
