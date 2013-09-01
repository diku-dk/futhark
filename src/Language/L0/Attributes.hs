{-# LANGUAGE FlexibleInstances #-}
-- | This module provides various simple ways to query and manipulate
-- fundamental L0 terms, such as types and values.  The intent is to
-- keep "L0.Language.Syntax" simple, and put whatever embellishments
-- we need here.
module Language.L0.Attributes
  ( locStr
  , TypeBox(..)
  , funDecByName

  -- * Parameter handling
  , toParam
  , fromParam

  -- * Queries on expressions
  , expToValue
  , mapTails
  , typeOf

  -- * Queries on types
  , basicType
  , uniqueness
  , unique
  , aliases
  , diet
  , dietingAs
  , subtypeOf
  , similarTo
  , returnType
  , lambdaType
  , lambdaReturnType

  -- * Operations on types
  , stripArray
  , peelArray
  , arrayOf
  , arrayType
  , elemType
  , rowType
  , toDecl
  , toElemDecl
  , fromDecl
  , fromElemDecl
  , setAliases
  , setElemAliases
  , addAliases
  , addElemAliases
  , setUniqueness
  , unifyUniqueness

  -- ** Removing and adding names
  --
  -- $names
  , addNames
  , addElemNames
  , removeNames
  , removeElemNames

  -- * Queries on values
  , arrayDims
  , arrayShape
  , arraySize
  , arrayString
  , valueType

  -- * Operations on values
  , blankValue
  , arrayVal
  , emptyArray
  , flattenArray
  , transposeArray
  , transposeIndex

  -- * Type aliases

  -- | Values of these types are produces by the parser.  They use
  -- unadorned names and have no type information, apart from that
  -- which is syntactically required.
  , NoInfo(..)
  , UncheckedType
  , UncheckedIdent
  , UncheckedExp
  , UncheckedLambda
  , UncheckedTupIdent
  , UncheckedFunDec
  , UncheckedProg
  )
  where

import Control.Applicative

import Data.Array
import Data.List
import Data.Loc
import Data.Monoid
import qualified Data.Set as S

import Language.L0.Syntax

-- | A human-readable location string, of the form
-- @filename:lineno:columnno@.
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
arrayDims :: TypeBase vn as -> Int
arrayDims (Array _ ds _ _) = length ds
arrayDims _                = 0

-- | @x `subuniqueOf` y@ is true if @x@ is not less unique than @y@.
subuniqueOf :: Uniqueness -> Uniqueness -> Bool
subuniqueOf Nonunique Unique = False
subuniqueOf _ _ = True

-- | @x \`subtypeOf\` y@ is true if @x@ is a subtype of @y@ (or equal to
-- @y@), meaning @x@ is valid whenever @y@ is.
subtypeOf :: TypeBase vn as1 -> TypeBase vn as2 -> Bool
subtypeOf (Array t1 dims1 u1 _) (Array t2 dims2 u2 _) =
  u1 `subuniqueOf` u2
       && Elem t1 `subtypeOf` Elem t2
       && length dims1 == length dims2
subtypeOf (Elem (Tuple ts1)) (Elem (Tuple ts2)) =
  and $ zipWith subtypeOf ts1 ts2
subtypeOf (Elem Int) (Elem Int) = True
subtypeOf (Elem Char) (Elem Char) = True
subtypeOf (Elem Real) (Elem Real) = True
subtypeOf (Elem Bool) (Elem Bool) = True
subtypeOf _ _ = False

-- | @x \`similarTo\` y@ is true if @x@ and @y@ are the same type,
-- ignoring uniqueness.
similarTo :: TypeBase vn as -> TypeBase vn as -> Bool
similarTo t1 t2 = t1 `subtypeOf` t2 || t2 `subtypeOf` t1

-- | Return the uniqueness of a type.
uniqueness :: TypeBase vn as -> Uniqueness
uniqueness (Array _ _ u _) = u
uniqueness _ = Nonunique

-- | @unique t@ is 'True' if the type of the argument is unique.
unique :: TypeBase vn as -> Bool
unique = (==Unique) . uniqueness

-- | Return the set of all variables mentioned in the aliasing of a
-- type.
aliases :: Monoid (as vn) => TypeBase as vn -> as vn
aliases (Array _ _ _ als) = als
aliases (Elem (Tuple et)) = mconcat $ map aliases et
aliases (Elem _)          = mempty

-- | @diet t@ returns a description of how a function parameter of
-- type @t@ might consume its argument.
diet :: TypeBase as vn -> Diet
diet (Elem (Tuple ets)) = TupleDiet $ map diet ets
diet (Elem _) = Observe
diet (Array _ _ Unique _) = Consume
diet (Array _ _ Nonunique _) = Observe

-- | @t `dietingAs` d@ modifies the uniqueness attributes of @t@ to
-- reflect how it is consumed according to @d@ - if it is consumed, it
-- becomes 'Unique'.  Tuples are handled intelligently.
dietingAs :: TypeBase as vn -> Diet -> TypeBase as vn
Elem (Tuple ets) `dietingAs` TupleDiet ds =
  Elem $ Tuple $ zipWith dietingAs ets ds
t `dietingAs` Consume =
  t `setUniqueness` Unique
t `dietingAs` _ =
  t `setUniqueness` Nonunique

-- | @t `maskAliases` d@ removes aliases (sets them to 'mempty') from
-- the parts of @t@ that are denoted as 'Consumed' by the 'Diet' @d@.
maskAliases :: Monoid (as vn) => TypeBase as vn -> Diet -> TypeBase as vn
maskAliases t Consume = t `setAliases` mempty
maskAliases t Observe = t
maskAliases (Elem (Tuple ets)) (TupleDiet ds) =
  Elem $ Tuple $ zipWith maskAliases ets ds
maskAliases _ _ = error "Invalid arguments passed to maskAliases."

-- | Remove aliasing information from a type.
toDecl :: TypeBase as vn -> DeclTypeBase vn
toDecl (Array et sz u _) = Array (toElemDecl et) sz u NoInfo
toDecl (Elem et) = Elem $ toElemDecl et

-- | Remove aliasing information from an element type.
toElemDecl :: ElemTypeBase as vn -> ElemTypeBase NoInfo vn
toElemDecl (Tuple ts) = Tuple $ map toDecl ts
toElemDecl t          = t `setElemAliases` NoInfo

-- | Replace no aliasing with an empty alias set.
fromDecl :: DeclTypeBase vn -> TypeBase Names vn
fromDecl (Array et sz u _) = Array et sz u S.empty
fromDecl (Elem et) = Elem $ fromElemDecl et

-- | Replace no aliasing with an empty alias set.
fromElemDecl :: ElemTypeBase NoInfo vn -> ElemTypeBase Names vn
fromElemDecl (Tuple ts) = Tuple $ map fromDecl ts
fromElemDecl t          = t `setElemAliases` S.empty

-- | A type box provides a way to box a 'CompTypeBase', and possibly
-- retrieve one, if the box is not empty.  This can be used to write
-- function on L0 terms that are polymorphic in the type annotations,
-- yet can still inspect types if they are present.
class TypeBox ty where
  -- | Try to retrieve a type from the type box.
  unboxType :: ty vn -> Maybe (CompTypeBase vn)
  -- | Put a type in the box.
  boxType :: CompTypeBase vn -> ty vn
  -- | Apply a mapping action to the type contained in the box.
  mapType :: Applicative f =>
             (CompTypeBase vn -> f (CompTypeBase vn')) -> ty vn -> f (ty vn')

instance TypeBox NoInfo where
  unboxType = const Nothing
  boxType = const NoInfo
  mapType = const . const (pure NoInfo)

instance TypeBox CompTypeBase where
  unboxType = Just
  boxType = id
  mapType = ($)

instance TypeBox DeclTypeBase where
  unboxType = Just . fromDecl
  boxType = toDecl
  mapType f x = toDecl <$> f (fromDecl x)

-- | @peelArray n t@ returns the type resulting from peeling the first
-- @n@ array dimensions from @t@.  Returns @Nothing@ if @t@ has less
-- than @n@ dimensions.
peelArray :: Int -> TypeBase as vn -> Maybe (TypeBase as vn)
peelArray 0 t = Just t
peelArray 1 (Array et [_] _ als) = Just $ Elem et `setAliases` als
peelArray n (Array et (_:ds) u als) =
  peelArray (n-1) $ Array et ds u als
peelArray _ _ = Nothing

-- | Returns the bottommost type of an array.  For @[[int]]@, this
-- would be @int@.
elemType :: TypeBase vn as -> ElemTypeBase vn as
elemType (Array t _ _ als) = t `setElemAliases` als
elemType (Elem t) = t

-- | Return the immediate row-type of an array.  For @[[int]]@, this
-- would be @[int]@.
rowType :: TypeBase vn as -> TypeBase vn as
rowType (Array et (_:_:dims) u als) = Array et dims u als
rowType (Array et _ _ als) = Elem et `setAliases` als
rowType (Elem et) = Elem et

-- | A type is a basic type if it is not an array and any component
-- types are basic types.
basicType :: TypeBase vn as -> Bool
basicType (Array {}) = False
basicType (Elem (Tuple ts)) = all basicType ts
basicType _ = True

uniqueOrBasic :: TypeBase vn as -> Bool
uniqueOrBasic x = basicType x || unique x

-- $names
--
-- The element type annotation of 'ArrayVal' values is a 'TypeBase'
-- with '()' for variable names.  This means that when we construct
-- 'ArrayVal's based on a type with a more common name representation,
-- we need to remove the names and replace them with '()'s.  Since the
-- only place names appear in types is in the array size annotations,
-- and those can always be replaced with 'Nothing', we can simply put
-- that in, instead of the original expression (if any).

-- | Remove names from a type - this involves removing all size
-- annotations from arrays, as well as all aliasing.
removeNames :: TypeBase as vn -> TypeBase NoInfo ()
removeNames (Array et sizes u _) =
  Array (removeElemNames et) (map (const Nothing) sizes) u NoInfo
removeNames (Elem et) = Elem $ removeElemNames et

-- | Remove names from an element type, as in 'removeNames'.
removeElemNames :: ElemTypeBase as vn -> ElemTypeBase NoInfo ()
removeElemNames (Tuple ets) = Tuple $ map removeNames ets
removeElemNames Int  = Int
removeElemNames Bool = Bool
removeElemNames Char = Char
removeElemNames Real = Real

-- | Add names to a type - this replaces array sizes with 'Nothing',
-- although they probably are already, if you're using this.
addNames :: TypeBase NoInfo () -> TypeBase NoInfo vn
addNames (Array et sizes u _) =
  Array (addElemNames et) (map (const Nothing) sizes) u NoInfo
addNames (Elem et) = Elem $ addElemNames et

-- | Add names to an element type - this replaces array sizes with
-- 'Nothing', although they probably are already, if you're using
-- this.
addElemNames :: ElemTypeBase NoInfo () -> ElemTypeBase NoInfo vn
addElemNames (Tuple ets) = Tuple $ map addNames ets
addElemNames Int  = Int
addElemNames Bool = Bool
addElemNames Char = Char
addElemNames Real = Real

-- | @arrayOf t s u@ constructs an array type.  The convenience
-- compared to using the 'Array' constructor directly is that @t@ can
-- itself be an array.  If @t@ is an @n@-dimensional array, and @s@ is
-- a list of length @n@, the resulting type is of an @n+m@ dimensions.
-- The uniqueness of the new array will be @u@, no matter the
-- uniqueness of @t@.
arrayOf :: Monoid (as vn) =>
           TypeBase as vn -> ArraySize vn -> Uniqueness -> TypeBase as vn
arrayOf (Array et size1 _ als) size2 u =
  Array et (size2 ++ size1) u als
arrayOf (Elem et) size u =
  Array (et `setElemAliases` NoInfo) size u $ aliases $ Elem et

-- | @array n t@ is the type of @n@-dimensional arrays having @t@ as
-- the base type.  If @t@ is itself an m-dimensional array, the result
-- is an @n+m@-dimensional array with the same base type as @t@.  If
-- you need to specify size information for the array, use 'arrayOf'
-- instead.
arrayType :: Monoid (as vn) =>
             Int -> TypeBase as vn -> Uniqueness -> TypeBase as vn
arrayType 0 t _ = t
arrayType n t u = arrayOf t ds u
  where ds = replicate n Nothing

-- | @stripArray n t@ removes the @n@ outermost layers of the array.
-- Essentially, it is the type of indexing an array of type @t@ with
-- @n@ indexes.
stripArray :: Int -> TypeBase vn as -> TypeBase vn as
stripArray n (Array et ds u als)
  | n < length ds = Array et (drop n ds) u als
  | otherwise     = Elem et `setAliases` als
stripArray _ t = t

-- | Set the uniqueness attribute of a type.  If the type is a tuple,
-- the uniqueness of its components will be modified.
setUniqueness :: TypeBase as vn -> Uniqueness -> TypeBase as vn
setUniqueness (Array et dims _ als) u = Array et dims u als
setUniqueness (Elem (Tuple ets)) u =
  Elem $ Tuple $ map (`setUniqueness` u) ets
setUniqueness t _ = t

-- | @t \`setAliases\` als@ returns @t@, but with @als@ substituted for
-- any already present aliasing.
setAliases :: TypeBase asf vn -> ast vn -> TypeBase ast vn
setAliases t = addAliases t . const

-- | @t \`setElemAliases\` als@ returns @t@, but with @als@ substituted for
-- any already present aliasing.
setElemAliases :: ElemTypeBase asf vn -> ast vn -> ElemTypeBase ast vn
setElemAliases t = addElemAliases t . const

-- | @t \`addAliases\` f@ returns @t@, but with any already present
-- aliasing replaced by @f@ applied to that aliasing.
addAliases :: TypeBase asf vn -> (asf vn -> ast vn) -> TypeBase ast vn
addAliases (Array et dims u als) f = Array et dims u $ f als
addAliases (Elem et) f = Elem $ et `addElemAliases` f

-- | @t \`addAliases\` f@ returns @t@, but with any already present
-- aliasing replaced by @f@ applied to that aliasing.
addElemAliases :: ElemTypeBase asf vn -> (asf vn -> ast vn) -> ElemTypeBase ast vn
addElemAliases (Tuple ets) f = Tuple $ map (`addAliases` f) ets
addElemAliases Int  _ = Int
addElemAliases Real _ = Real
addElemAliases Bool _ = Bool
addElemAliases Char _ = Char

-- | Unify the uniqueness attributes and aliasing information of two
-- types.  The two types must otherwise be identical.  The resulting
-- alias set will be the 'mappend' of the two input types aliasing sets,
-- and the uniqueness will be 'Unique' only if both of the input types
-- are unique.
unifyUniqueness :: Monoid (as vn) =>
                   TypeBase as vn -> TypeBase as vn -> TypeBase as vn
unifyUniqueness (Array et dims u1 als1) (Array _ _ u2 als2) =
  Array et dims (u1 <> u2) (als1 <> als2)
unifyUniqueness (Elem (Tuple ets1)) (Elem (Tuple ets2)) =
  Elem $ Tuple $ zipWith unifyUniqueness ets1 ets2
unifyUniqueness t1 _ = t1

-- | A "blank" value of the given type - this is zero, or whatever is
-- close to it.  Don't depend on this value, but use it for creating
-- arrays to be populated by do-loops.
blankValue :: TypeBase as vn -> Value
blankValue (Elem Int) = IntVal 0
blankValue (Elem Real) = RealVal 0.0
blankValue (Elem Bool) = LogVal False
blankValue (Elem Char) = CharVal '\0'
blankValue (Elem (Tuple ts)) = TupVal (map blankValue ts)
blankValue (Array et [_] _ _) = arrayVal [] $ Elem et
blankValue (Array et (_:ds) u as) = arrayVal [] rt
  where rt = Array et ds u as
blankValue (Array et _ _ _) = arrayVal [] $ Elem et

-- | The type of an L0 value.
valueType :: Value -> DeclTypeBase vn
valueType (IntVal _) = Elem Int
valueType (RealVal _) = Elem Real
valueType (LogVal _) = Elem Bool
valueType (CharVal _) = Elem Char
valueType (TupVal vs) = Elem $ Tuple (map valueType vs)
valueType (ArrayVal _ (Elem et)) =
  Array (addElemNames et) [Nothing] Nonunique NoInfo
valueType (ArrayVal _ (Array et ds _ _)) =
  Array (addElemNames et) (Nothing:replicate (length ds) Nothing) Nonunique NoInfo

-- | Return a list of the sizes of an array (the shape, in other
-- terms).  For non-arrays, this is the empty list.  A two-dimensional
-- array with five rows and three columns would return the list @[5,
-- 3]@.  If an array has @n@ dimensions, the result is always a list
-- of @n@ elements.
arrayShape :: Value -> [Int]
arrayShape (ArrayVal arr rt)
  | v:_ <- elems arr = snd (bounds arr) + 1 : arrayShape v
  | otherwise = replicate (1 + arrayDims rt) 0
arrayShape _ = []

-- | Return the size of the first dimension of an array, or zero for
-- non-arrays.
arraySize :: Value -> Int
arraySize t = case arrayShape t of
                []  -> 0
                n:_ -> n

-- | Construct an array value containing the given elements.
arrayVal :: [Value] -> TypeBase as vn -> Value
arrayVal vs = ArrayVal (listArray (0, length vs-1) vs) . removeNames . toDecl

-- | An empty array with the given row type.
emptyArray :: TypeBase as vn -> Value
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

-- | Array transpose generalised to multiple dimensions.  The result
-- of @transposeArray k n a@ is an array where the element @a[i_1,
-- ..., i_k ,i_{k+1}, ..., i_{k+n}, ..., i_q ]@ is now at index @[i_1
-- ,.., i_{k+1} , ..., i_{k+n} ,i_k, ..., i_q ]@.
--
-- @transposeArray 0 1@ is equivalent to the common transpose.  If the
-- given value is not an array, it is returned unchanged.
transposeArray :: Int -> Int -> Value -> Value
transposeArray k n v =
  case flattenArray v of
    ArrayVal inarr _ ->
      let newshape = move oldshape
          idx is shape = sum (zipWith (*) is (map product $ drop 1 (tails shape)))
          f rt is (m:ms) =
            arrayVal [ f (stripArray 1 rt) (is ++ [i]) ms | i <- [0..m-1] ] rt
          f _ is [] = inarr ! idx (move is) oldshape
      in f (rowType $ valueType v) [] newshape
    _ -> v
  where oldshape = arrayShape v
        move = transposeIndex k n

-- | If @l@ is an index into the array @a@, then @transposeIndex k n
-- l@ is an index to the same element in the array @transposeArray k n
-- a@.
transposeIndex :: Int -> Int -> [a] -> [a]
transposeIndex k n l
  | (pre,needle:post) <- splitAt k l,
    (mid,end) <- splitAt n post = pre ++ mid ++ [needle] ++ end
  | otherwise = l

-- | The type of an L0 term.  The aliasing will refer to itself, if
-- the term is a non-tuple-typed variable.
typeOf :: Ord vn => ExpBase CompTypeBase vn -> CompTypeBase vn
typeOf (Literal val _) = fromDecl $ valueType val
typeOf (TupLit es _) = Elem $ Tuple $ map typeOf es
typeOf (ArrayLit es t _) =
  arrayType 1 t $ mconcat $ map (uniqueness . typeOf) es
typeOf (BinOp _ _ _ t _) = t
typeOf (And {}) = Elem Bool
typeOf (Or {}) = Elem Bool
typeOf (Not _ _) = Elem Bool
typeOf (Negate _ t _) = t
typeOf (If _ _ _ t _) = t
typeOf (Var ident) =
  case identType ident of
    Elem (Tuple ets) -> Elem $ Tuple ets
    t                -> t `addAliases` S.insert (identName ident)
typeOf (Apply _ _ t _) = t
typeOf (LetPat _ _ body _) = typeOf body
typeOf (LetWith _ _ _ _ body _) = typeOf body
typeOf (Index ident _ t _) =
  t `addAliases` S.insert (identName ident)
typeOf (Iota _ _) = arrayType 1 (Elem Int) Unique
typeOf (Size {}) = Elem Int
typeOf (Replicate _ e _) = arrayType 1 (typeOf e) u
  where u | uniqueOrBasic (typeOf e) = Unique
          | otherwise = Nonunique
typeOf (Reshape shape e _) = build (length shape) (elemType $ typeOf e)
  where build 0 t = Elem t
        build n t =
          Array (t `setElemAliases` NoInfo) (replicate n Nothing) Nonunique $
          case typeOf e of Array _ _ _ als -> als
                           _               -> S.empty -- Type error.
typeOf (Transpose k n e _)
  | Array et dims u als <- typeOf e,
    (pre,d:post) <- splitAt k dims,
    (mid,end) <- splitAt n post = Array et (pre++mid++[d]++end) u als
  | otherwise = typeOf e
typeOf (Map f arr _ _) = arrayType 1 et $ uniqueProp et
  where et = lambdaType f [rowType $ typeOf arr]
typeOf (Reduce fun start arr _ _) =
  lambdaType fun [typeOf start, rowType (typeOf arr)]
typeOf (Zip es _) = arrayType 1 (Elem $ Tuple $ map snd es) Nonunique
typeOf (Unzip _ ts _) =
  Elem $ Tuple $ map (\t -> arrayType 1 t $ uniqueProp t) ts
typeOf (Scan fun start arr _ _) =
  arrayType 1 et Unique
    where et = lambdaType fun [typeOf start, rowType $ typeOf arr]
typeOf (Filter _ arr _ _) = typeOf arr
typeOf (Redomap redfun mapfun start arr rt loc) =
  lambdaType redfun [typeOf start, rowType $ typeOf $ Map mapfun arr rt loc]
typeOf (Split _ _ t _) =
  Elem $ Tuple [arrayType 1 t Nonunique, arrayType 1 t Nonunique]
typeOf (Concat x y _) = typeOf x `setUniqueness` u
  where u = uniqueness (typeOf x) <> uniqueness (typeOf y)
typeOf (Copy e _) = typeOf e `setUniqueness` Unique `setAliases` S.empty
typeOf (DoLoop _ _ _ _ _ body _) = typeOf body
typeOf (Map2 f arrs _ _) =
  Elem $ Tuple $ case lambdaType f $ map typeOf arrs of
                   Elem (Tuple tps) ->
                     map (\x -> arrayType 1 x (uniqueProp x)) tps
                   ftp -> [arrayType 1 ftp (uniqueProp ftp)]
typeOf (Reduce2 fun acc arrs _ _) =
  lambdaType fun $ map typeOf acc ++ map typeOf arrs
typeOf (Scan2 _ _ _ ets _) =
  Elem $ Tuple $ map (\x -> arrayType 1 x Unique) ets
typeOf (Filter2 _ arrs _) = Elem $ Tuple $ map typeOf arrs
typeOf (Redomap2 redfun mapfun start arrs rt loc) =
  lambdaType redfun $ map typeOf start ++ case typeOf (Map2 mapfun arrs rt loc) of
                                            Elem (Tuple ts) -> ts
                                            t               -> [t]

uniqueProp :: TypeBase vn as -> Uniqueness
uniqueProp tp = if uniqueOrBasic tp then Unique else Nonunique

-- | If possible, convert an expression to a value.  This is not a
-- true constant propagator, but a quick way to convert array/tuple
-- literal expressions into literal values instead.
expToValue :: ExpBase (TypeBase as) vn -> Maybe Value
expToValue (Literal val _) = Just val
expToValue (TupLit es _) = do es' <- mapM expToValue es
                              Just $ TupVal es'
expToValue (ArrayLit es t _) = do es' <- mapM expToValue es
                                  Just $ arrayVal es' t
expToValue _ = Nothing

-- | The result of applying the arguments of the given types to the
-- given lambda function.
lambdaType :: Ord vn =>
              LambdaBase CompTypeBase vn -> [CompTypeBase vn] -> CompTypeBase vn
lambdaType lam = returnType (lambdaReturnType lam) (lambdaParamDiets lam)


 -- | The result of applying the arguments of the given types to a
-- function with the given return type, consuming its parameters with
-- the given diets .
returnType :: Ord vn => DeclTypeBase vn -> [Diet] -> [CompTypeBase vn] -> CompTypeBase vn
returnType (Array et sz Nonunique NoInfo) ds args = Array et sz Nonunique als
  where als = mconcat $ map aliases $ zipWith maskAliases args ds
returnType (Array et sz Unique NoInfo) _ _ = Array et sz Unique mempty
returnType (Elem (Tuple ets)) ds args =
  Elem $ Tuple $ map (\et -> returnType et ds args) ets
returnType (Elem t) _ _ = Elem t `setAliases` S.empty

-- | The specified return type of a lambda.
lambdaReturnType :: LambdaBase CompTypeBase vn -> DeclTypeBase vn
lambdaReturnType (AnonymFun _ _ t _) = t
lambdaReturnType (CurryFun _ _ t _)  = toDecl t

-- | The parameter 'Diet's of a lambda.
lambdaParamDiets :: LambdaBase ty vn -> [Diet]
lambdaParamDiets (AnonymFun params _ _ _) = map (diet . identType) params
lambdaParamDiets (CurryFun _ args _ _) = map (const Observe) args

-- | Find the function of the given name in the L0 program.
funDecByName :: Name -> ProgBase ty vn -> Maybe (FunDecBase ty vn)
funDecByName fname = find (\(fname',_,_,_,_) -> fname == fname') . progFunctions

-- | Change those subexpressions where evaluation of the expression
-- would stop.  Also change type annotations at branches.
mapTails :: (ExpBase ty vn -> ExpBase ty vn) -> (ty vn -> ty vn)
         -> ExpBase ty vn -> ExpBase ty vn
mapTails f g (LetPat pat e body loc) =
  LetPat pat e (mapTails f g body) loc
mapTails f g (LetWith dest src idxs ve body loc) =
  LetWith dest src idxs ve (mapTails f g body) loc
mapTails f g (DoLoop pat me i bound loopbody body loc) =
  DoLoop pat me i bound loopbody (mapTails f g body) loc
mapTails f g (If c te fe t loc) =
  If c (mapTails f g te) (mapTails f g fe) (g t) loc
mapTails f _ e = f e

-- | Convert an identifier to a 'ParamBase'.
toParam :: IdentBase (TypeBase as) vn -> ParamBase vn
toParam (Ident name t loc) = Ident name (toDecl t) loc

-- | Convert a 'ParamBase' to an identifier.
fromParam :: ParamBase vn -> IdentBase CompTypeBase vn
fromParam (Ident name t loc) = Ident name (fromDecl t) loc

-- | A type with no aliasing information.
type UncheckedType = TypeBase NoInfo Name

-- | An identifier with no type annotations.
type UncheckedIdent = IdentBase NoInfo Name

-- | An expression with no type annotations.
type UncheckedExp = ExpBase NoInfo Name

-- | A lambda with no type annotations.
type UncheckedLambda = LambdaBase NoInfo Name

-- | A pattern with no type annotations.
type UncheckedTupIdent = TupIdentBase NoInfo Name

-- | A function declaration with no type annotations.
type UncheckedFunDec = FunDecBase NoInfo Name

-- | An L0 program with no type annotations.
type UncheckedProg = ProgBase NoInfo Name
