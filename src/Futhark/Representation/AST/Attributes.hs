{-# LANGUAGE TypeFamilies #-}
-- | This module provides various simple ways to query and manipulate
-- fundamental Futhark terms, such as types and values.  The intent is to
-- keep "Futhark.Language.Internal.Syntax" simple, and put whatever
-- embellishments we need here.  This is an internal, desugared
-- representation.
module Futhark.Representation.AST.Attributes
  ( locStr
  , funDecByName
  , progNames

  -- * Parameter handling
  , toParam
  , fromParam

  -- * Operations on bodies
  , bodyResult
  , setBodyBindings
  , setBodyResult
  , mapResult
  , mapResultM
  , insertBinding
  , insertBindings
  , resultBody

  -- * Operations on expressions
  , subExpType
  , bodyType
  , loopResultType
  , loopResult
  , mapType
  , reduceType
  , scanType
  , filterType
  , redomapType
  , typeOf
  , freeInBody
  , freeNamesInBody
  , freeInExp
  , freeNamesInExp
  , consumedInBody
  , consumedInExp
  , safeExp
  , constant
  , intconst

  -- * Queries on lambdas
  , freeInLambda
  , freeNamesInLambda

  -- * Queries on types
  , basicType
  , uniqueness
  , unique
  , uniqueOrBasic
  , aliases
  , diet
  , dietingAs
  , subtypeOf
  , subtypesOf
  , similarTo
  , arrayRank
  , arrayShape
  , arrayDims
  , arraySize
  , arraysSize
  , setArrayShape
  , setArrayDims
  , setOuterSize
  , returnType
  , lambdaType
  , lambdaReturnType
  , hasStaticShape

  -- * Operations on types
  , stripArray
  , peelArray
  , arrayOf
  , elemType
  , rowType
  , basicDecl
  , toDecl
  , toConstType
  , fromConstType
  , setAliases
  , changeAliases
  , setUniqueness
  , unifyUniqueness
  , generaliseResTypes
  , staticShapes
  , instantiateShapes
  , existentialShapes
  , shapeContext
  , valueShapeContext
  , subExpShapeContext

  -- * Queries on values
  , valueShape
  , valueSize
  , arrayString
  , valueType

  -- * Operations on values
  , blankValue
  , arrayVal
  , emptyArray
  , flattenArray
  , IsValue(..)

  -- * Rearranging
  , permuteShape
  , permuteArray
  , permuteInverse
  , permuteReach
  , permuteCompose

  -- * Rotating
  , rotateArray

  -- * Transposition
  , transposeArray
  , transposeIndex
  , transposeInverse
  , transposeDimension

  -- * Reshaping
  , reshapeOuter
  , reshapeInner
  )
  where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.State

import Data.Array
import Data.Ord
import Data.List
import Data.Loc
import Data.Maybe
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM

import Futhark.Representation.AST.Lore (Proper)
import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Traversals

-- | Return the dimensionality of a type.  For non-arrays, this is
-- zero.  For a one-dimensional array it is one, for a two-dimensional
-- it is two, and so forth.
arrayRank :: ArrayShape shape => TypeBase as shape -> Int
arrayRank = shapeRank . arrayShape

-- | Return the shape of a type - for non-arrays, this is the
-- 'mempty'.
arrayShape :: ArrayShape shape => TypeBase as shape -> shape
arrayShape (Array _ ds _ _) = ds
arrayShape _                = mempty

-- | Return the dimensions of a type - for non-arrays, this is the
-- empty list.
arrayDims :: TypeBase als (Shape lore) -> [SubExp lore]
arrayDims = shapeDims . arrayShape

-- | Return the size of the given dimension.  If the dimension does
-- not exist, the zero constant is returned.
arraySize :: Int -> TypeBase als (Shape lore) -> SubExp lore
arraySize i t = case drop i $ arrayDims t of
                  e : _ -> e
                  _     -> intconst 0 noLoc

-- | Return the size of the given dimension in the first element of
-- the given type list.  If the dimension does not exist, or no types
-- are given, the zero constant is returned.
arraysSize :: Int -> [TypeBase als (Shape lore)] -> SubExp lore
arraysSize _ []    = intconst 0 noLoc
arraysSize i (t:_) = arraySize i t

-- | Set the shape of an array.  If the given type is not an
-- array, return the type unchanged.
setArrayShape :: ArrayShape newshape =>
                 TypeBase as oldshape -> newshape -> TypeBase as newshape
setArrayShape (Array et _ u as) ds
  | shapeRank ds == 0 = Basic et
  | otherwise         = Array et ds u as
setArrayShape (Basic t)  _         = Basic t

-- | Set the dimensions of an array.  If the given type is not an
-- array, return the type unchanged.
setArrayDims :: TypeBase as oldshape -> [SubExp lore] -> TypeBase as (Shape lore)
setArrayDims t dims = t `setArrayShape` Shape dims

-- | Replace the size of the outermost dimension of an array.  If the
-- given type is not an array, it is returned unchanged.
setOuterSize :: TypeBase as (Shape lore) -> SubExp lore -> TypeBase as (Shape lore)
setOuterSize t e = case arrayShape t of
                      Shape (_:es) -> t `setArrayShape` Shape (e : es)
                      _            -> t

-- | @x `subuniqueOf` y@ is true if @x@ is not less unique than @y@.
subuniqueOf :: Uniqueness -> Uniqueness -> Bool
subuniqueOf Nonunique Unique = False
subuniqueOf _ _ = True

-- | @x \`subtypeOf\` y@ is true if @x@ is a subtype of @y@ (or equal to
-- @y@), meaning @x@ is valid whenever @y@ is.
subtypeOf :: ArrayShape shape => TypeBase as1 shape -> TypeBase as2 shape -> Bool
subtypeOf (Array t1 shape1 u1 _) (Array t2 shape2 u2 _) =
  u1 `subuniqueOf` u2
       && t1 == t2
       && shapeRank shape1 == shapeRank shape2
subtypeOf (Basic t1) (Basic t2) = t1 == t2
subtypeOf _ _ = False

-- | @xs \`subtypesOf\` ys@ is true if @xs@ is the same size as @ys@,
-- and each element in @xs@ is a subtype of the corresponding element
-- in @ys@..
subtypesOf :: ArrayShape shape => [TypeBase as1 shape] -> [TypeBase as2 shape] -> Bool
subtypesOf xs ys = length xs == length ys &&
                   and (zipWith subtypeOf xs ys)

-- | @x \`similarTo\` y@ is true if @x@ and @y@ are the same type,
-- ignoring uniqueness.
similarTo :: ArrayShape shape => TypeBase as1 shape -> TypeBase as2 shape -> Bool
similarTo t1 t2 = t1 `subtypeOf` t2 || t2 `subtypeOf` t1

-- | Return the uniqueness of a type.
uniqueness :: TypeBase as shape -> Uniqueness
uniqueness (Array _ _ u _) = u
uniqueness _ = Nonunique

-- | @unique t@ is 'True' if the type of the argument is unique.
unique :: TypeBase as shape -> Bool
unique = (==Unique) . uniqueness

-- | Return the set of all variables mentioned in the aliasing of a
-- type.
aliases :: Monoid as => TypeBase as shape -> as
aliases (Array _ _ _ als) = als
aliases (Basic _)          = mempty

-- | @diet t@ returns a description of how a function parameter of
-- type @t@ might consume its argument.
diet :: TypeBase as shape -> Diet
diet (Basic _) = Observe
diet (Array _ _ Unique _) = Consume
diet (Array _ _ Nonunique _) = Observe

-- | @t `dietingAs` d@ modifies the uniqueness attributes of @t@ to
-- reflect how it is consumed according to @d@ - if it is consumed, it
-- becomes 'Unique'.  Tuples are handled intelligently.
dietingAs :: TypeBase as shape -> Diet -> TypeBase as shape
t `dietingAs` Consume =
  t `setUniqueness` Unique
t `dietingAs` _ =
  t `setUniqueness` Nonunique

-- | @t `maskAliases` d@ removes aliases (sets them to 'mempty') from
-- the parts of @t@ that are denoted as 'Consumed' by the 'Diet' @d@.
maskAliases :: Monoid as => TypeBase as shape -> Diet -> TypeBase as shape
maskAliases t Consume = t `setAliases` mempty
maskAliases t Observe = t

-- | Given a basic type, construct a type without aliasing and shape
-- information.  This is sometimes handy for disambiguation when
-- constructing types.
basicDecl :: BasicType -> DeclType
basicDecl = Basic

-- | Remove aliasing and shape information from a type.
toDecl :: ArrayShape shape => TypeBase as shape -> DeclType
toDecl (Array et sz u _) = Array et (Rank $ shapeRank sz) u ()
toDecl (Basic et) = Basic et

-- | Add (empty) aliasing information to a type.
fromConstType :: ConstType lore -> Type lore
fromConstType t = t `setAliases` mempty

-- | Remove aliasing information from a type.
toConstType :: TypeBase als (Shape lore) -> ConstType lore
toConstType t = t `setAliases` ()

-- | @peelArray n t@ returns the type resulting from peeling the first
-- @n@ array dimensions from @t@.  Returns @Nothing@ if @t@ has less
-- than @n@ dimensions.
peelArray :: ArrayShape shape =>
             Int -> TypeBase as shape -> Maybe (TypeBase as shape)
peelArray 0 t = Just t
peelArray n (Array et shape u als)
  | shapeRank shape == n = Just $ Basic et `setAliases` als
  | shapeRank shape >  n = Just $ Array et (stripDims n shape) u als
peelArray _ _ = Nothing

-- | Returns the bottommost type of an array.  For @[[int]]@, this
-- would be @int@.  If the given type is not an array, it is returned.
elemType :: TypeBase as shape -> BasicType
elemType (Array t _ _ _) = t
elemType (Basic t)       = t

-- | Return the immediate row-type of an array.  For @[[int]]@, this
-- would be @[int]@.
rowType :: ArrayShape shape => TypeBase as shape -> TypeBase as shape
rowType = stripArray 1

-- | A type is a basic type if it is not an array and any component
-- types are basic types.
basicType :: TypeBase as shape -> Bool
basicType (Array {}) = False
basicType _ = True

-- | Is the given type either unique (as per 'unique') or basic (as
-- per 'basicType')?
uniqueOrBasic :: TypeBase as shape -> Bool
uniqueOrBasic x = basicType x || unique x

-- | @arrayOf t s u@ constructs an array type.  The convenience
-- compared to using the 'Array' constructor directly is that @t@ can
-- itself be an array.  If @t@ is an @n@-dimensional array, and @s@ is
-- a list of length @n@, the resulting type is of an @n+m@ dimensions.
-- The uniqueness of the new array will be @u@, no matter the
-- uniqueness of @t@.
arrayOf :: (ArrayShape shape, Monoid as) =>
           TypeBase as shape -> shape -> Uniqueness -> TypeBase as shape
arrayOf (Array et size1 _ als) size2 u =
  Array et (size2 <> size1) u als
arrayOf (Basic et) size u =
  Array et size u mempty

-- | @stripArray n t@ removes the @n@ outermost layers of the array.
-- Essentially, it is the type of indexing an array of type @t@ with
-- @n@ indexes.
stripArray :: ArrayShape shape => Int -> TypeBase as shape -> TypeBase as shape
stripArray n (Array et shape u als)
  | n < shapeRank shape = Array et (stripDims n shape) u als
  | otherwise           = Basic et `setAliases` als
stripArray _ t = t

-- | Set the uniqueness attribute of a type.  If the type is a tuple,
-- the uniqueness of its components will be modified.
setUniqueness :: TypeBase as shape -> Uniqueness -> TypeBase as shape
setUniqueness (Array et dims _ als) u = Array et dims u als
setUniqueness t _ = t

-- | @t \`setAliases\` als@ returns @t@, but with @als@ substituted for
-- any already present aliasing.
setAliases :: TypeBase asf shape -> ast -> TypeBase ast shape
setAliases t = changeAliases t . const

-- | @t \`changeAliases\` f@ returns @t@, but with any already present
-- aliasing replaced by @f@ applied to that aliasing.
changeAliases :: TypeBase asf shape -> (asf -> ast) -> TypeBase ast shape
changeAliases (Array et dims u als) f = Array et dims u $ f als
changeAliases (Basic et) _            = Basic et

-- | Unify the uniqueness attributes and aliasing information of two
-- types.  The two types must otherwise be identical.  The resulting
-- alias set will be the 'mappend' of the two input types aliasing sets,
-- and the uniqueness will be 'Unique' only if both of the input types
-- are unique.
unifyUniqueness :: Monoid as => TypeBase as shape -> TypeBase as shape -> TypeBase as shape
unifyUniqueness (Array et dims u1 als1) (Array _ _ u2 als2) =
  Array et dims (u1 <> u2) (als1 <> als2)
unifyUniqueness t1 _ = t1

-- | Compute the most specific generalisation of two types with
-- existentially quantified shapes.
generaliseResTypes :: Proper lore => ResType lore -> ResType lore -> ResType lore
generaliseResTypes rt1 rt2 =
  evalState (zipWithM unifyExtShapes rt1 rt2) (0, HM.empty)
  where unifyExtShapes t1 t2 =
          unifyUniqueness <$>
          (setArrayShape t1 <$> ExtShape <$>
           zipWithM unifyExtDims
           (extShapeDims $ arrayShape t1)
           (extShapeDims $ arrayShape t2)) <*>
          pure t2
        unifyExtDims (Free se1) (Free se2)
          | se1 == se2 = return $ Free se1 -- Arbitrary
          | otherwise  = do (n,m) <- get
                            put (n + 1, m)
                            return $ Ext n
        unifyExtDims (Ext x) (Ext y)
          | x == y = Ext <$> (maybe (new x) return =<<
                              gets (HM.lookup x . snd))
        unifyExtDims (Ext x) _ = Ext <$> new x
        unifyExtDims _ (Ext x) = Ext <$> new x
        new x = do (n,m) <- get
                   put (n + 1, HM.insert x n m)
                   return n

-- | A "blank" value of the given type - this is zero, or whatever is
-- close to it.  Don't depend on this value, but use it for creating
-- arrays to be populated by do-loops.
blankValue :: DeclType -> Value
blankValue (Basic Int) = BasicVal $ IntVal 0
blankValue (Basic Real) = BasicVal $ RealVal 0.0
blankValue (Basic Bool) = BasicVal $ LogVal False
blankValue (Basic Char) = BasicVal $ CharVal '\0'
blankValue (Basic Cert) = BasicVal Checked
blankValue (Array et (Rank 1) _ _)  = arrayVal [] (Basic et :: TypeBase as Rank)
blankValue (Array et (Rank n) u as) = arrayVal [] rt
  where rt = Array et (Shape $ replicate (n-1) $ intconst 0 noLoc) u as

-- | If a Haskell type is an instance of 'IsValue', it means that a
-- value of that type can be converted to a Futhark 'Value'.  This is
-- intended to cut down on boilerplate when writing compiler code -
-- for example, you'll quickly grow tired of writing @Constant
-- (BasicVal $ LogVal True) loc@.
class IsValue a where
  value :: a -> Value

instance IsValue Int where
  value = BasicVal . IntVal

instance IsValue Double where
  value = BasicVal . RealVal

instance IsValue Bool where
  value = BasicVal . LogVal

instance IsValue Char where
  value = BasicVal . CharVal

valueType :: Value -> ConstType lore
valueType (BasicVal v) =
  Basic $ basicValueType v
valueType v@(ArrayVal _ (Basic et)) =
  Array et (Shape [n]) Nonunique ()
  where n = constant (valueSize v) noLoc
valueType v@(ArrayVal _ (Array et _ _ _)) =
  Array et (Shape $ map (`intconst` noLoc) $ valueShape v) Nonunique ()

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

-- | Return the size of the first dimension of an array, or zero for
-- non-arrays.
valueSize :: Value -> Int
valueSize t = case valueShape t of
                []  -> 0
                n:_ -> n

-- | Construct an array value containing the given elements.
arrayVal :: ArrayShape shape => [Value] -> TypeBase as shape -> Value
arrayVal vs t = ArrayVal (listArray (0, length vs-1) vs) $
                t `setAliases` () `setArrayShape` Rank n
  where n = shapeRank $ arrayShape t

-- | An empty array with the given row type.
emptyArray :: TypeBase as (Shape lore) -> Value
emptyArray = arrayVal []

-- | If the given value is a nonempty array containing only
-- characters, return the corresponding 'String', otherwise return
-- 'Nothing'.
arrayString :: Value -> Maybe String
arrayString (ArrayVal arr _)
  | c:cs <- elems arr = mapM asChar $ c:cs
  where asChar (BasicVal (CharVal c)) = Just c
        asChar _                        = Nothing
arrayString _ = Nothing

-- | Given an N-dimensional array, return a one-dimensional array
-- with the same elements.
flattenArray :: Value -> Value
flattenArray (ArrayVal arr et) =
  arrayVal (concatMap flatten $ elems arr) et
    where flatten (ArrayVal arr' _) = concatMap flatten $ elems arr'
          flatten v = [v]
flattenArray v = v

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

-- | Array transpose generalised to multiple dimensions.  The result
-- of @transposeArray k n a@ is an array where the element @a[i_1,
-- ..., i_k ,i_{k+1}, ..., i_{k+n}, ..., i_q ]@ is now at index @[i_1
-- ,.., i_{k+1} , ..., i_{k+n} ,i_k, ..., i_q ]@.
--
-- @transposeArray 0 1@ is equivalent to the common transpose.  If the
-- given value is not an array, it is returned unchanged.
transposeArray :: Int -> Int -> Value -> Value
transposeArray k n v = permuteArray (transposeIndex k n [0..rank-1]) v
  where rank = arrayRank $ valueType v

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

-- | Compute the inverse of a given transposition.  Upholds the
-- following property:
--
-- @uncurry transposeIndex (transposeInverse k n) (transposeIndex k n l) == l@
transposeInverse :: Int -> Int -> (Int,Int)
transposeInverse k n = (k+n,-n)

-- | @transposeDimension k n dim numShape@ gives the new position of
-- dimension @dim@ in a @numShape@-dimensional array after being
-- @n,k@-transposed.
transposeDimension :: Int -> Int -> Int -> Int -> Int
transposeDimension k n dim numDims =
  transposeIndex k n [0..numDims-1] !! dim

shapeExps :: SubExp lore -> [SubExp lore]
shapeExps = shapeDims . arrayShape . subExpType

-- | @reshapeOuter shape n src@ returns a 'Reshape' expression that
-- replaces the outer @n@ dimensions of @src@ with @shape@.
reshapeOuter :: [SubExp lore] -> Int -> SubExp lore -> [SubExp lore]
reshapeOuter shape n src = shape ++ drop n (shapeExps src)

-- | @reshapeInner shape n src@ returns a 'Reshape' expression that
-- replaces the inner @m-n@ dimensions (where @m@ is the rank of
-- @src@) of @src@ with @shape@.
reshapeInner :: [SubExp lore] -> Int -> SubExp lore -> [SubExp lore]
reshapeInner shape n src = take n (shapeExps src) ++ shape

varType :: Ident lore -> Type lore
varType ident = identType ident `changeAliases` HS.insert (identName ident)

subExpType :: SubExp lore -> Type lore
subExpType (Constant val _) = fromConstType $ valueType val
subExpType (Var ident)      = varType ident

bodyType :: Body lore -> ResType lore
bodyType (Body bnds res) =
  evalState (mapM makeBoundShapesFree $
             staticShapes $ map subExpType $ resultSubExps res)
  (0, HM.empty, HM.empty)
  where boundInLet (Let pat _ _) = map identName pat
        bound = HS.fromList $ concatMap boundInLet bnds
        makeBoundShapesFree t = do
          shape <- mapM checkDim $ extShapeDims $ arrayShape t
          return $ t `setArrayShape` ExtShape shape
        checkDim (Free (Var v))
          | identName v `HS.member` bound =
            replaceVar $ identName v
        checkDim (Free se) = return $ Free se
        checkDim (Ext x)   = replaceExt x
        replaceExt x = do
          (n, extmap, varmap) <- get
          case HM.lookup x extmap of
            Nothing -> do put (n+1, HM.insert x (Ext n) extmap, varmap)
                          return $ Ext $ n+1
            Just replacement -> return replacement
        replaceVar name = do
          (n, extmap, varmap) <- get
          case HM.lookup name varmap of
            Nothing -> do put (n+1, extmap, HM.insert name (Ext n) varmap)
                          return $ Ext $ n+1
            Just replacement -> return replacement

loopResultType :: [Type lore] -> [(Ident lore, SubExp lore)] -> ResType lore
loopResultType restypes merge = evalState (mapM inspect restypes) 0
  where bound = map (identName . fst) merge
        inspect t = do
          shape <- mapM inspectShape $ arrayDims t
          return $ t `setArrayShape` ExtShape shape
        inspectShape (Var v)
          | identName v `elem` bound = do
            i <- get
            put $ i + 1
            return $ Ext i
        inspectShape se = return $ Free se

-- | A loop returns not only the values indicated in the result
-- pattern, but also any non-static shapes of arrays.  Thus,
-- @loopResult res merge@ returns @res@ prefixed with with those
-- variables in @merge@ that constitute the shape context.
loopResult :: [Ident lore] -> [Ident lore] -> [Ident lore]
loopResult res merge = resShapes ++ res
  where notInRes (Constant _ _) = Nothing
        notInRes (Var v)
          | v `notElem` res,
            v `elem` merge = Just v
          | otherwise       = Nothing
        resShapes =
          nub $ concatMap (mapMaybe notInRes . arrayDims . identType) res

staticShapes :: [TypeBase als (Shape lore)] -> [TypeBase als (ExtShape lore)]
staticShapes = map staticShapes'
  where staticShapes' (Basic bt) =
          Basic bt
        staticShapes' (Array bt (Shape shape) u als) =
          Array bt (ExtShape $ map Free shape) u als

instantiateShapes :: Monad m => m (SubExp lore) -> [TypeBase als (ExtShape lore)]
                  -> m [TypeBase als (Shape lore)]
instantiateShapes f ts = evalStateT (mapM instantiate ts) HM.empty
  where instantiate t = do
          shape <- mapM instantiate' $ extShapeDims $ arrayShape t
          return $ t `setArrayShape` Shape shape
        instantiate' (Ext x) = do
          m <- get
          case HM.lookup x m of
            Just se -> return se
            Nothing -> do se <- lift f
                          put $ HM.insert x se m
                          return se
        instantiate' (Free se) = return se

existentialShapes :: [TypeBase als1 (ExtShape lore)] -> [TypeBase als2 (Shape lore)]
                  -> [SubExp lore]
existentialShapes t1s t2s  = concat $ zipWith concreteShape' t1s t2s
  where concreteShape' t1 t2 =
          catMaybes $ zipWith concretise
          (extShapeDims $ arrayShape t1)
          (shapeDims $ arrayShape t2)
        concretise (Ext _) se  = Just se
        concretise (Free _) _ = Nothing

shapeContext :: [TypeBase als (ExtShape lore)] -> [[a]] -> [a]
shapeContext ts shapes =
  evalState (concat <$> zipWithM extract ts shapes) HS.empty
  where extract t shape =
          catMaybes <$> zipWithM extract' (extShapeDims $ arrayShape t) shape
        extract' (Ext x) v = do
          seen <- gets $ HS.member x
          return $ if seen
                   then Nothing
                   else Just v
        extract' (Free _) _ = return Nothing

valueShapeContext :: [TypeBase als (ExtShape lore)] -> [Value] -> [Value]
valueShapeContext rettype values =
  map value $ shapeContext rettype $ map valueShape values

subExpShapeContext :: [TypeBase als (ExtShape lore)] -> [SubExp lore] -> [SubExp lore]
subExpShapeContext rettype ses =
  shapeContext rettype $ map (arrayDims . subExpType) ses

hasStaticShape :: ResType lore -> Maybe [Type lore]
hasStaticShape = mapM hasStaticShape'
  where hasStaticShape' (Basic bt) =
          Just $ Basic bt
        hasStaticShape' (Array bt (ExtShape shape) u als) =
          Array bt <$> (Shape <$> mapM isFree shape) <*> pure u <*> pure als
        isFree (Free s) = Just s
        isFree (Ext _)  = Nothing

mapType :: Lambda lore -> [SubExp lore] -> [Type lore]
mapType f arrs = [ arrayOf t (Shape [outersize]) (uniqueness t)
                 | t <- lambdaType f arrts ]
  where outersize = arraysSize 0 arrts
        arrts     = map subExpType arrs

reduceType :: Lambda lore -> [(SubExp lore, SubExp lore)] -> [Type lore]
reduceType fun inputs =
  lambdaType fun $ map subExpType acc ++ map subExpType arrs
  where (acc, arrs) = unzip inputs

scanType :: Lambda lore -> [(SubExp lore, SubExp lore)] -> [Type lore]
scanType _ inputs =
  map ((`setUniqueness` Unique) . subExpType) arrs
  where (_, arrs) = unzip inputs

filterType :: Lambda lore -> [SubExp lore] -> ResType lore
filterType _ =
  map (extOuterDim . subExpType)
  where extOuterDim t =
          t `setArrayShape` ExtShape (extOuterDim' $ arrayShape t)
        extOuterDim' (Shape dims) =
          Ext 0 : map Free (drop 1 dims)

redomapType:: Lambda lore -> Lambda lore -> [SubExp lore] -> [SubExp lore] -> [Type lore]
redomapType outerfun innerfun acc arrs =
  lambdaType outerfun $ lambdaType innerfun (innerres ++ innerres)
  where innerres = lambdaType innerfun
                   (map subExpType acc ++ map (rowType . subExpType) arrs)

-- | The type of an Futhark term.  The aliasing will refer to itself, if
-- the term is a non-tuple-typed variable.
typeOf :: Exp lore -> ResType lore
typeOf (SubExp se) = staticShapes [subExpType se]
typeOf (ArrayLit es t loc) =
  staticShapes [arrayOf t (Shape [n]) $ mconcat $ map (uniqueness . subExpType) es]
  where n = constant (length es) loc
typeOf (BinOp _ _ _ t _) = staticShapes [t]
typeOf (Not _ _) = staticShapes [Basic Bool]
typeOf (Negate e _) = staticShapes [subExpType e]
typeOf (If _ _ _ t _) = t
typeOf (Apply _ _ t _) = t
typeOf (Index _ ident idx _) =
  staticShapes [stripArray (length idx) (varType ident)
              `changeAliases` HS.insert (identName ident)]
typeOf (Update _ src _ _ _) =
  staticShapes [identType src `setAliases` HS.empty]
typeOf (Iota ne _) =
  staticShapes [arrayOf (Basic Int) (Shape [ne]) Nonunique]
typeOf (Replicate ne e _) =
  staticShapes [arrayOf (subExpType e) (Shape [ne]) u]
  where u = uniqueness $ subExpType e
typeOf (Reshape _ [] e _) =
  staticShapes [Basic $ elemType $ subExpType e]
typeOf (Reshape _ shape e _) =
  staticShapes [subExpType e `setArrayShape` Shape shape]
typeOf (Rearrange _ perm e _) =
  staticShapes [subExpType e `setArrayShape` Shape (permuteShape perm shape)]
  where Shape shape = arrayShape $ subExpType e
typeOf (Rotate _ _ e _) = staticShapes [subExpType e]
typeOf (Split _ ne e secsize _) =
  staticShapes [subExpType e `setOuterSize` ne,
              subExpType e `setOuterSize` secsize]
typeOf (Concat _ x y ressize _) =
  staticShapes [subExpType x `setUniqueness` u `setOuterSize` ressize]
  where u = uniqueness (subExpType x) <> uniqueness (subExpType y)
typeOf (Copy e _) =
  staticShapes [subExpType e `setUniqueness` Unique `setAliases` HS.empty]
typeOf (Assert _ _) =
  staticShapes [Basic Cert]
typeOf (Conjoin _ _) =
  staticShapes [Basic Cert]
typeOf (DoLoop res merge _ _ _ _) = loopResultType (map identType res) merge
typeOf (Map _ f arrs _) =
  staticShapes $ mapType f arrs
typeOf (Reduce _ fun inputs _) =
  staticShapes $ reduceType fun inputs
typeOf (Scan _ fun inputs _) =
  staticShapes $ scanType fun inputs
typeOf (Filter _ f arrs _) =
  filterType f arrs
typeOf (Redomap _ outerfun innerfun acc arrs _) =
  staticShapes $ redomapType outerfun innerfun acc arrs

-- | The result of applying the arguments of the given types to the
-- given tuple lambda function.
lambdaType :: Lambda lore -> [Type lore] -> [Type lore]
lambdaType (Lambda params _ ets _) =
  returnType ets ds
  where ds = map (diet . identType) params

-- | The result of applying the arguments of the given types to a
-- function with the given return type, consuming its parameters with
-- the given diets.
returnType :: [TypeBase als shape1] -> [Diet] -> [TypeBase Names shape2]
           -> [TypeBase Names shape1]
returnType rts ds args = map returnType' rts
  where returnType' (Array et sz Nonunique _) =
          Array et sz Nonunique als
          where als = mconcat $ map aliases $ zipWith maskAliases args ds
        returnType' (Array et sz Unique _) =
          Array et sz Unique mempty
        returnType' (Basic t) =
          Basic t `setAliases` HS.empty

-- | Find the function of the given name in the Futhark program.
funDecByName :: Name -> Prog lore -> Maybe (FunDec lore)
funDecByName fname = find (\(fname',_,_,_,_) -> fname == fname') . progFunctions

-- | Return the set of all variable names bound in a program.
progNames :: Prog lore -> Names
progNames = execWriter . mapM funNames . progFunctions
  where names = identityWalker {
                  walkOnBinding = bindingNames
                , walkOnBody = bodyNames
                , walkOnLambda = lambdaNames
                }

        one = tell . HS.singleton . identName
        funNames (_, _, params, body, _) =
          mapM_ one params >> bodyNames body

        bodyNames = mapM_ bindingNames . bodyBindings

        bindingNames (Let pat _ e) =
          mapM_ one pat >> expNames e

        expNames (DoLoop _ pat i _ loopbody _) =
          mapM_ (one . fst) pat >> one i >> bodyNames loopbody
        expNames e = walkExpM names e

        lambdaNames (Lambda params body _ _) =
          mapM_ one params >> bodyNames body

-- | @setBodyBindings bnds body@ sets the bindings of @body@ to @bnds@.
setBodyBindings :: [Binding lore] -> Body lore -> Body lore
setBodyBindings bnds body = body { bodyBindings = bnds }

-- | @setBodyResult result body@ sets the tail end of @body@ (the
-- 'Result' part) to @result@.
setBodyResult :: Body lore -> Body lore -> Body lore
setBodyResult result = mapResult $ const result

-- | Change that subexpression where evaluation of the body would
-- stop.
mapResultM :: (Applicative m, Monad m) =>
              (Result lore -> m (Body lore)) -> Body lore -> m (Body lore)
mapResultM f (Body bnds res) = do
  Body bnds2 res' <- f res
  return $ Body (bnds++bnds2) res'

-- | Add a binding at the outermost level of a 'Body'.
insertBinding :: Binding lore -> Body lore -> Body lore
insertBinding bnd (Body bnds res) = Body (bnd:bnds) res

-- | Add several bindings at the outermost level of a 'Body'.
insertBindings :: [Binding lore] -> Body lore -> Body lore
insertBindings bnds1 (Body bnds2 res) = Body (bnds1++bnds2) res

-- | Conveniently construct a body that contains no bindings.
resultBody :: Certificates lore -> [SubExp lore] -> SrcLoc -> Body lore
resultBody cs ses loc = Body [] $ Result cs ses loc

-- | Change that result where evaluation of the body would stop.  Also
-- change type annotations at branches.  This a non-monadic variant of
-- @mapResultM@.
mapResult :: (Result lore -> Body lore) -> Body lore -> Body lore
mapResult f e = runIdentity $ mapResultM (return . f) e

freeWalker :: Walker lore (Writer (HS.HashSet (Ident lore)))
freeWalker = identityWalker {
               walkOnSubExp = subExpFree
             , walkOnBody = bodyFree
             , walkOnBinding = bindingFree
             , walkOnLambda = lambdaFree
             , walkOnIdent = identFree
             , walkOnCertificates = mapM_ identFree
             , walkOnType = typeFree
             }
  where identFree ident = do
          tell $ HS.singleton ident
          typeFree $ identType ident

        subExpFree (Var ident) = identFree ident
        subExpFree (Constant {})  = return ()

        bodyFree (Body [] (Result cs ses _)) = do
          mapM_ identFree cs
          mapM_ subExpFree ses
        bodyFree (Body (Let pat _ e:bnds) res) = do
          expFree e
          binding (HS.fromList pat) $ do
            mapM_ (typeFree . identType) pat
            bodyFree $ Body bnds res

        bindingFree (Let pat _ e) =
          binding (HS.fromList pat) $ do
            mapM_ (typeFree . identType) pat
            expFree e

        expFree (DoLoop _ merge i boundexp loopbody _) = do
          let (mergepat, mergeexps) = unzip merge
          mapM_ subExpFree mergeexps
          subExpFree boundexp
          binding (i `HS.insert` HS.fromList mergepat) $ do
            mapM_ (typeFree . identType) mergepat
            bodyFree loopbody
        expFree e = walkExpM freeWalker e

        lambdaFree = tell . freeInLambda

        typeFree = mapM_ subExpFree . shapeDims . arrayShape

        binding bound = censor (`HS.difference` bound)

-- | Return the set of identifiers that are free in the given
-- body.
freeInBody :: Body lore -> HS.HashSet (Ident lore)
freeInBody = execWriter . walkOnBody freeWalker

-- | As 'freeInBody', but returns the raw names rather than 'Ident's.
freeNamesInBody :: Body lore -> Names
freeNamesInBody = HS.map identName . freeInBody

-- | Return the set of identifiers that are free in the given
-- expression.
freeInExp :: Exp lore -> HS.HashSet (Ident lore)
freeInExp = execWriter . walkExpM freeWalker

-- | As 'freeInExp', but returns the raw names rather than 'Ident's.
freeNamesInExp :: Exp lore -> Names
freeNamesInExp = HS.map identName . freeInExp

-- | Return the set of variables names consumed by the given
-- body.
consumedInBody :: Body lore -> Names
consumedInBody = execWriter . bodyConsumed
  where unconsume s = censor (`HS.difference` s)

        bodyConsumed (Body [] _) = return ()
        bodyConsumed (Body (Let pat _ e:bnds) res) = do
          tell $ consumedInExp e
          unconsume (HS.fromList $ map identName pat) $
            bodyConsumed $ Body bnds res

-- | Return the set of variable names consumed by the given
-- expression.
consumedInExp :: Exp lore -> Names
consumedInExp (Apply _ args _ _) =
  mconcat $ map (consumeArg . first subExpType) args
  where consumeArg (t, Consume) = aliases t
        consumeArg (_, Observe) = mempty
consumedInExp (Update _ src _ _ _) =
  identName src `HS.insert` aliases (identType src)
consumedInExp (If _ tb fb _ _) = consumedInBody tb <> consumedInBody fb
consumedInExp (DoLoop _ merge _ _ _ _) =
  mconcat $ map (aliases . subExpType . snd) $ filter (unique . identType . fst) merge
consumedInExp _ = mempty

-- | An expression is safe if it is always well-defined (assuming that
-- any required certificates have been checked) in any context.  For
-- example, array indexing is not safe, as the index may be out of
-- bounds.  On the other hand, adding two numbers cannot fail.
safeExp :: Exp lore -> Bool
safeExp e | not $ HS.null $ consumedInExp e = False
safeExp (Index {}) = False
safeExp (Update {}) = False
safeExp (Split {}) = False
safeExp (Assert {}) = False
safeExp (Reshape {}) = False
safeExp (ArrayLit {}) = False
safeExp (Concat {}) = False
safeExp (Apply {}) = False
safeExp (BinOp Divide _ (Constant (BasicVal (IntVal k))  _) _ _) = k /= 0
safeExp (BinOp Divide _ (Constant (BasicVal (RealVal k)) _) _ _) = k /= 0
safeExp (BinOp Divide _ _ _ _) = False
safeExp (BinOp Mod _ (Constant (BasicVal (IntVal k))  _) _ _) = k /= 0
safeExp (BinOp Mod _ (Constant (BasicVal (RealVal k)) _) _ _) = k /= 0
safeExp (BinOp Mod _ _ _ _) = False
safeExp (BinOp Pow _ _ _ _) = False
safeExp _ = True

-- | Create a 'Constant' 'SubExp' containing the given value.
constant :: IsValue v => v -> SrcLoc -> SubExp lore
constant = Constant . value

-- | For reasons of type ambiguity, a specialised 'constant' for integers is defined.
intconst :: Int -> SrcLoc -> SubExp lore
intconst = constant

-- | Return the set of identifiers that are free in the given lambda,
-- including shape annotations in the parameters.
freeInLambda :: Lambda lore -> HS.HashSet (Ident lore)
freeInLambda (Lambda params body rettype _) =
  inRet <> inParams <> inBody
  where inRet = mconcat $ map freeInType rettype
        inParams = mconcat $ map freeInParam params
        freeInParam = freeInType . identType
        inBody = HS.filter ((`notElem` paramnames) . identName) $ freeInBody body
        freeInType = mconcat . map (freeInExp . SubExp) . shapeDims . arrayShape
        paramnames = map identName params

-- | As 'freeInLambda', but returns the raw names rather than
-- 'IdentBase's.
freeNamesInLambda :: Lambda lore -> Names
freeNamesInLambda = HS.map identName . freeInLambda

-- | Convert an identifier to a 'ParamBase'.
toParam :: Ident lore -> Param lore
toParam (Ident name t loc) = Ident name (t `setAliases` ()) loc

-- | Convert a 'ParamBase' to an identifier.
fromParam :: Proper lore => Param lore -> Ident lore
fromParam (Ident name t loc) = Ident name (t `setAliases` mempty) loc
