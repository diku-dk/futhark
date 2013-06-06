{-# LANGUAGE FlexibleInstances #-}
module Language.L0.Attributes
  ( locStr
  , subtypeOf
  , similarTo
  , uniqueness
  , unique
  , TypeBox(..)
  , Typed(..)
  , arrayDims
  , arrayShape
  , arraySize
  , peelArray
  , elemType
  , rowType
  , basicType
  , arrayType
  , arrayOf
  , stripArray
  , blankValue
  , arrayVal
  , emptyArray
  , arrayString
  , flattenArray
  , expToValue
  , funDecByName

  -- * Type aliases
  , UncheckedIdent
  , UncheckedExp
  , UncheckedLambda
  , UncheckedTupIdent
  , UncheckedFunDec
  , UncheckedProg
  )
  where

import Data.Array
import Data.List
import Data.Loc
import Data.Monoid

import Language.L0.Syntax

locStr :: SrcLoc -> String
locStr (SrcLoc NoLoc) = "unknown location"
locStr (SrcLoc (Loc (Pos file line1 col1 _) (Pos _ line2 col2 _))) =
  -- Assume that both positions are in the same file (what would the
  -- alternative mean?)
  file ++ ":" ++ show line1 ++ ":" ++ show col1
       ++ "-" ++ show line2 ++ ":" ++ show col2

-- | Return the dimensionality of a type.  For non-arrays, this is
-- zero.  For a one-dimensional array it is one, for a two-dimensional
-- it is two, and so forth.
arrayDims :: Type -> Int
arrayDims (Array _ ds _) = length ds
arrayDims _              = 0

-- | @x `subuniqueOf` y@ is true if @x@ is not less unique than @y@.
subuniqueOf :: Uniqueness -> Uniqueness -> Bool
subuniqueOf Nonunique Unique = False
subuniqueOf _ _ = True

-- | @x \`subtypeOf\` y@ is true if @x@ is a subtype of @y@ (or equal to
-- @y@), meaning @x@ is valid whenever @y@ is.
subtypeOf :: Type -> Type -> Bool
subtypeOf (Array t1 dims1 u1) (Array t2 dims2 u2) =
  u1 `subuniqueOf` u2 && Elem t1 `subtypeOf` Elem t2 && dims1 == dims2
subtypeOf (Elem (Tuple ts1)) (Elem (Tuple ts2)) =
  and $ zipWith subtypeOf ts1 ts2
subtypeOf t1 t2 = t1 == t2

-- | @x \`similarTo\` y@ is true if @x@ and @y@ are the same type,
-- ignoring uniqueness.
similarTo :: Type -> Type -> Bool
similarTo t1 t2 = t1 `subtypeOf` t2 || t2 `subtypeOf` t1

-- | Return the uniqueness of a type.
uniqueness :: Typed a => a -> Uniqueness
uniqueness = uniqueness' . typeOf
  where uniqueness' (Array _ _ u) = u
        uniqueness' _ = Nonunique

-- | @unique t@ is 'True' if the type of the argument is unique.
unique :: Typed a => a -> Bool
unique = (==Unique) . uniqueness

-- | A type box provides a way to box a type, and possibly retrieve
-- one.
class (Eq ty, Ord ty, Show ty) => TypeBox ty where
  unboxType :: ty -> Maybe Type
  boxType :: Type -> ty

instance TypeBox () where
  unboxType = const Nothing
  boxType = const ()

instance TypeBox Type where
  unboxType = Just
  boxType = id

-- | A typed value is one from which we can retrieve a type.
class Typed v where
  typeOf :: v -> Type

instance Typed Type where
  typeOf = id

-- | @peelArray n t@ returns the type resulting from peeling the first
-- @n@ array dimensions from @t@.  Returns @Nothing@ if @t@ has less
-- than @n@ dimensions.
peelArray :: Int -> Type -> Maybe Type
peelArray 0 t = Just t
peelArray 1 (Array et [_] _) = Just $ Elem et
peelArray n (Array et (_:ds) u) =
  peelArray (n-1) $ Array et ds u
peelArray _ _ = Nothing

-- | Returns the bottommost type of an array.  For @[[int]]@, this
-- would be @int@.
elemType :: Type -> ElemType
elemType (Array t _ _) = t
elemType (Elem t) = t

-- | Return the immediate row-type of an array.  For @[[int]]@, this
-- would be @[int]@.
rowType :: Type -> Type
rowType (Array et (_:_:dims) u) = Array et dims u
rowType (Array et _ _) = Elem et
rowType (Elem et) = Elem et

-- | A type is a basic type if it is not an array and any component
-- types are basic types.
basicType :: Type -> Bool
basicType (Array {}) = False
basicType (Elem (Tuple ts)) = all basicType ts
basicType _ = True

uniqueOrBasic :: Typed t => t -> Bool
uniqueOrBasic x = basicType (typeOf x) || unique x

-- | @array n t@ is the type of @n@-dimensional arrays having @t@ as
-- the base type.  If @t@ is itself an m-dimensional array, the result
-- is an @n+m@-dimensional array with the same base type as @t@
arrayType :: Int -> Type -> Uniqueness -> Type
arrayType 0 t _ = t
arrayType n t u = arrayOf t ds u
  where ds = replicate n Nothing

arrayOf :: Type -> ArraySize -> Uniqueness -> Type
arrayOf (Array et size1 _) size2 u =
  Array et (size2 ++ size1) u
arrayOf (Elem et) size u = Array et size u

-- | @stripArray n t@ removes the @n@ outermost layers of the array.
-- Essentially, it is the type of indexing an array of type @t@ with
-- @n@ indexes.
stripArray :: Int -> Type -> Type
stripArray n (Array et ds u)
  | n < length ds = Array et (drop n ds) u
  | otherwise     = Elem et
stripArray _ t = t

withUniqueness :: Type -> Uniqueness -> Type
withUniqueness (Array et dims _) u = Array et dims u
withUniqueness (Elem (Tuple ets)) u =
  Elem $ Tuple $ map (`withUniqueness` u) ets
withUniqueness t _ = t

-- | A "blank" value of the given type - this is zero, or whatever is
-- close to it.  Don't depend on this value, but use it for creating
-- arrays to be populated by do-loops.
blankValue :: Type -> Value
blankValue (Elem Int) = IntVal 0
blankValue (Elem Real) = RealVal 0.0
blankValue (Elem Bool) = LogVal False
blankValue (Elem Char) = CharVal '\0'
blankValue (Elem (Tuple ts)) = TupVal (map blankValue ts)
blankValue (Array et (_:ds) u) = arrayVal [] rt
  where rt = Array et ds u
blankValue (Array et _ _) = arrayVal [] $ Elem et

instance Typed Value where
  typeOf (IntVal _) = Elem Int
  typeOf (RealVal _) = Elem Real
  typeOf (LogVal _) = Elem Bool
  typeOf (CharVal _) = Elem Char
  typeOf (TupVal vs) = Elem $ Tuple (map typeOf vs)
  typeOf (ArrayVal _ (Elem et)) =
    Array et [Nothing] Nonunique
  typeOf (ArrayVal _ (Array et ds _)) =
    Array et (Nothing:ds) Nonunique

-- | Return a list of the sizes of an array (the shape, in other
-- terms).  For non-arrays, this is the empty list.  A two-dimensional
-- array with five rows and three columns would return the list @[5,
-- 3]@.
arrayShape :: Value -> [Int]
arrayShape (ArrayVal arr _)
  | v:_ <- elems arr = snd (bounds arr) + 1 : arrayShape v
arrayShape _ = []

-- | Return the size of the first dimension of an array, or zero for
-- non-arrays.
arraySize :: Value -> Int
arraySize t = case arrayShape t of
                []  -> 0
                n:_ -> n

-- | Construct an array value containing the given elements.
arrayVal :: [Value] -> Type -> Value
arrayVal vs = ArrayVal $ listArray (0, length vs-1) vs

emptyArray :: Type -> Value
emptyArray = arrayVal []

-- | If the given value is a nonempty array containing only
-- characters, return the corresponding 'String', otherwise return
-- 'Nothing'.
arrayString :: Value -> Maybe String
arrayString (ArrayVal arr _)
  | c:cs <- elems arr = mapM asChar $ c:cs
  where asChar (CharVal c) = Just c
        asChar _ = Nothing
arrayString _ = Nothing

-- | Given an N-dimensional array, return a one-dimensional array
-- with the same elements.
flattenArray :: Value -> Value
flattenArray (ArrayVal arr et) =
  arrayVal (concatMap flatten $ elems arr) et
    where flatten (ArrayVal arr' _) = concatMap flatten $ elems arr'
          flatten v = [v]
flattenArray v = v

instance Typed (IdentBase Type vn) where
  typeOf = identType

instance Typed (ExpBase Type vn) where
  typeOf (Literal val _) = typeOf val
  typeOf (TupLit es _) = Elem $ Tuple $ map typeOf es
  typeOf (ArrayLit es t _) = arrayType 1 t $ mconcat $ map uniqueness es
  typeOf (BinOp _ _ _ t _) = t
  typeOf (And {}) = Elem Bool
  typeOf (Or {}) = Elem Bool
  typeOf (Not _ _) = Elem Bool
  typeOf (Negate _ t _) = t
  typeOf (If _ _ _ t _) = t
  typeOf (Var ident) = identType ident
  typeOf (Apply _ _ t _) = t
  typeOf (LetPat _ _ body _) = typeOf body
  typeOf (LetWith _ _ _ _ body _) = typeOf body
  typeOf (Index _ _ t _) = t
  typeOf (Iota _ _) = arrayType 1 (Elem Int) Unique
  typeOf (Size _ _) = Elem Int
  typeOf (Replicate _ e _) = arrayType 1 (typeOf e) u
    where u | uniqueOrBasic e = Unique
            | otherwise = Nonunique
  typeOf (Reshape shape e _) = build (length shape) (elemType $ typeOf e)
    where build 0 t = Elem t
          build n t = Array t (replicate n Nothing) Nonunique
  typeOf (Transpose e _) = typeOf e
  typeOf (Map f _ _ _) = arrayType 1 (typeOf f) u
    where u | uniqueOrBasic (typeOf f) = Unique
            | otherwise = Nonunique
  typeOf (Reduce fun _ _ _ _) = typeOf fun
  typeOf (Zip es _) = arrayType 1 (Elem $ Tuple $ map snd es) Nonunique
  typeOf (Unzip _ ts _) = Elem $ Tuple $ map (flip (arrayType 1) Unique) ts
  typeOf (Scan fun e arr _ _) =
    arrayType 1 (typeOf fun) (uniqueness fun <> uniqueness e <> uniqueness arr)
  typeOf (Filter _ arr _ _) = typeOf arr
  typeOf (Mapall fun e _) = arrayType (arrayDims $ typeOf e) (typeOf fun) Nonunique
  typeOf (Redomap redfun _ _ _ _ _) = typeOf redfun
  typeOf (Split _ _ t _) = Elem $ Tuple [arrayType 1 t Nonunique, arrayType 1 t Nonunique]
  typeOf (Concat x y _) = typeOf x `withUniqueness` u
    where u = uniqueness (typeOf x) <> uniqueness (typeOf y)
  typeOf (Copy e _) = typeOf e `withUniqueness` Unique
  typeOf (DoLoop _ _ _ _ _ body _) = typeOf body
--- Begin SOAC2: (Cosmin) ---
  typeOf (Map2 f _ _ _) =
    let ftp = typeOf f in
    case ftp of
        Elem (Tuple tps) -> Elem $ Tuple $ map (\x -> arrayType 1 x (uniqueProp x)) tps
        _ -> arrayType 1 ftp (uniqueProp ftp)
  --typeOf (Map2 _ _ _ (Elem (Tuple tps)) _) = Elem $ Tuple $ map (\x -> arrayType 1 x Unique) tps
  --typeOf (Map2 _ _ _ tp _) = arrayType 1 tp Unique
  typeOf (Reduce2 fun _ _ _ _) = typeOf fun
  typeOf (Scan2 _ _ _ (Elem (Tuple tps)) _) = Elem $ Tuple $ map (\x -> arrayType 1 x Unique) tps
  typeOf (Scan2 _ _ _ tp _) = arrayType 1 tp Unique
  typeOf (Filter2 _ arrs _) =
    case map typeOf arrs of
      [t] -> t
      tps -> Elem $ Tuple tps
  typeOf (Redomap2 redfun _ _ _ _ _) = typeOf redfun
  typeOf (Mapall2 fun es _) =
      let inpdim= case map typeOf es of
                    et:etps -> foldl min
                               (arrayDims et)
                               (map arrayDims etps)
                    _       -> 0
          fnrtp = typeOf fun
      in case fnrtp of
          Elem (Tuple tps) -> Elem $ Tuple $ map (\x -> arrayType inpdim x Unique) tps
          _ -> arrayType inpdim fnrtp Unique

uniqueProp :: Typed t => t -> Uniqueness
uniqueProp tp = if uniqueOrBasic tp then Unique else Nonunique
--- End SOAC2: (Cosmin) ---

-- | If possible, convert an expression to a value.  This is not a
-- true constant propagator, but a quick way to convert array/tuple
-- literal expressions into literal values instead.
expToValue :: ExpBase Type vn -> Maybe Value
expToValue (Literal val _) = Just val
expToValue (TupLit es _) = do es' <- mapM expToValue es
                              Just $ TupVal es'
expToValue (ArrayLit es t _) = do es' <- mapM expToValue es
                                  Just $ arrayVal es' t
expToValue _ = Nothing

instance Typed (LambdaBase Type vn) where
  typeOf (AnonymFun _ _ t _) = t
  typeOf (CurryFun _ _ t _) = t

-- | Find the function of the given name in the L0 program.
funDecByName :: Name -> ProgBase ty vn -> Maybe (FunDecBase ty vn)
funDecByName fname = find (\(fname',_,_,_,_) -> fname == fname') . progFunctions

type UncheckedIdent = IdentBase () Name

type UncheckedExp = ExpBase () Name

type UncheckedLambda = LambdaBase () Name

type UncheckedTupIdent = TupIdentBase () Name

type UncheckedFunDec = FunDecBase () Name

type UncheckedProg = ProgBase () Name
