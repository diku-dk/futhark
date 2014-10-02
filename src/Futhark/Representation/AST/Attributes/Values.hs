module Futhark.Representation.AST.Attributes.Values
       (
         valueType
       , valueShape
       , valueSize
       , arrayVal
       , IsValue (..)
       , intconst

         -- * Rearranging
       , permuteShape
       , permuteArray
       , permuteInverse
       , permuteReach
       , permuteCompose
       , transposeIndex

         -- * Rotation
       , rotateArray

         -- * Miscellaneous
       , arrayString
       )
       where

import Data.Array
import Data.Loc
import Data.List
import Data.Ord

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes.Types
import Futhark.Representation.AST.Attributes.Constants

-- | Return the type of the given value.
valueType :: Value -> Type
valueType (BasicVal v) =
  Basic $ basicValueType v
valueType v@(ArrayVal _ (Basic et)) =
  Array et (Shape [n]) Nonunique
  where n = constant (valueSize v) noLoc
valueType v@(ArrayVal _ (Array et _ _)) =
  Array et (Shape $ map (`intconst` noLoc) $ valueShape v) Nonunique
valueType (ArrayVal _ (Mem {})) =
  error "valueType Mem"

-- | Return the size of the first dimension of an array, or zero for
-- non-arrays.
valueSize :: Value -> Int
valueSize t = case valueShape t of
                []  -> 0
                n:_ -> n

-- | Return a list of the sizes of an array (the shape, in other
-- terms).  For non-arrays, this is the empty list.  A two-dimensional
-- array with five rows and three columns would return the list @[5,
-- 3]@.  If an array has @n@ dimensions, the result is always a list
-- of @n@ elements.
valueShape :: Value -> [Int]
valueShape (ArrayVal arr rt)
  | v:_ <- elems arr = snd (bounds arr) + 1 : valueShape v
  | otherwise = replicate (1 + arrayRank rt) 0
valueShape _ = []

-- | Given an N-dimensional array, return a one-dimensional array
-- with the same elements.
flattenArray :: Value -> Value
flattenArray (ArrayVal arr et) =
  arrayVal (concatMap flatten $ elems arr) et
    where flatten (ArrayVal arr' _) = concatMap flatten $ elems arr'
          flatten v = [v]
flattenArray v = v

-- | Construct an array value containing the given elements.
arrayVal :: ArrayShape shape => [Value] -> TypeBase shape -> Value
arrayVal vs t = ArrayVal (listArray (0, length vs-1) vs) $
                t `setArrayShape` Rank n
  where n = shapeRank $ arrayShape t

-- | Calculate the given permutation of the list.  It is an error if
-- the permutation goes out of bounds.
permuteShape :: [Int] -> [a] -> [a]
permuteShape perm l = map (l!!) perm

permuteArray :: [Int] -> Value -> Value
permuteArray perm v =
  case flattenArray v of
    ArrayVal inarr _ ->
      let newshape = move oldshape
          idx is shape = sum (zipWith (*) is (map product $ drop 1 (tails shape)))
          f rt is (m:ms) =
            arrayVal [ f (stripArray 1 rt) (is ++ [i]) ms | i <- [0..m-1] ] rt
          f _ is [] = inarr ! idx (move is) oldshape
      in f (rowType $ valueType v) [] newshape
    _ -> v
  where oldshape = valueShape v
        move = permuteShape perm

-- | Produce the inverse permutation.
permuteInverse :: [Int] -> [Int]
permuteInverse perm = map snd $ sortBy (comparing fst) $ zip perm [0..]

-- | Return the first dimension not affected by the permutation.  For
-- example, the permutation @[1,0,2]@ would return @2@.
permuteReach :: [Int] -> Int
permuteReach perm = case dropWhile (uncurry (/=)) $ zip (tails perm) (tails [0..n-1]) of
                      []          -> n + 1
                      (perm',_):_ -> n - length perm'
  where n = length perm

-- | Compose two permutations, with the second given permutation being
-- applied first.
permuteCompose :: [Int] -> [Int] -> [Int]
permuteCompose = permuteShape

-- | If @l@ is an index into the array @a@, then @transposeIndex k n
-- l@ is an index to the same element in the array @transposeArray k n
-- a@.
transposeIndex :: Int -> Int -> [a] -> [a]
transposeIndex k n l
  | k + n >= length l =
    let n' = ((k + n) `mod` length l)-k
    in transposeIndex k n' l
  | n < 0,
    (pre,needle:end) <- splitAt k l,
    (beg,mid) <- splitAt (length pre+n) pre =
    beg ++ [needle] ++ mid ++ end
  | (beg,needle:post) <- splitAt k l,
    (mid,end) <- splitAt n post =
    beg ++ mid ++ [needle] ++ end
  | otherwise = l

-- | Rotate the elements of an array as per the Futhark 'rotate' command.
-- If the value is not an array, this is a no-op.
rotateArray :: Int -> Value -> Value
rotateArray n (ArrayVal a t) =
  arrayVal rotatedElems t
  where arrelems = elems a
        nelems = length arrelems
        rotatedElems
          | n > 0     = drop (nelems - n) arrelems ++ take (nelems - n) arrelems
          | otherwise = drop (-n) arrelems ++ take (-n) arrelems
rotateArray _ v = v

-- | If the given value is a nonempty array containing only
-- characters, return the corresponding 'String', otherwise return
-- 'Nothing'.
arrayString :: Value -> Maybe String
arrayString (ArrayVal arr _)
  | c:cs <- elems arr = mapM asChar $ c:cs
  where asChar (BasicVal (CharVal c)) = Just c
        asChar _                        = Nothing
arrayString _ = Nothing
