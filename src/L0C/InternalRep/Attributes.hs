-- | This module provides various simple ways to query and manipulate
-- fundamental L0 terms, such as types and values.  The intent is to
-- keep "L0.Language.Internal.Syntax" simple, and put whatever
-- embellishments we need here.  This is an internal, desugared
-- representation.
module L0C.InternalRep.Attributes
  ( locStr
  , funDecByName
  , progNames

  -- * Parameter handling
  , toParam
  , fromParam

  -- * Queries on expressions
  , mapTail
  , mapTailM
  , subExpType
  , bodyType
  , typeOf
  , freeInBody
  , freeNamesInBody
  , freeInExp
  , freeNamesInExp
  , consumedInBody
  , consumedInExp
  , safeExp

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
  , setArrayShape
  , returnType
  , lambdaType
  , lambdaReturnType

  -- * Operations on types
  , stripArray
  , peelArray
  , arrayOf
  , elemType
  , rowType
  , basicDecl
  , toDecl
  , fromDecl
  , toConstType
  , fromConstType
  , setAliases
  , changeAliases
  , setUniqueness
  , unifyUniqueness

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

  -- * Rearranging
  , permuteShape
  , permuteArray
  , permuteInverse
  , permuteReach
  , permuteCompose

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

import Data.Array
import Data.Ord
import Data.List
import Data.Loc
import qualified Data.HashSet as HS

import L0C.InternalRep.Syntax
import L0C.InternalRep.Traversals

-- | Return the dimensionality of a type.  For non-arrays, this is
-- zero.  For a one-dimensional array it is one, for a two-dimensional
-- it is two, and so forth.
arrayRank :: ArrayShape shape => TypeBase as shape -> Int
arrayRank = shapeRank . arrayShape

-- | Return the dimensions of a type - for non-arrays, this is the
-- empty list.
arrayShape :: ArrayShape shape => TypeBase as shape -> shape
arrayShape (Array _ ds _ _) = ds
arrayShape _                = mempty

-- | Set the dimensions of an array.  If the given type is not an
-- array, return the type unchanged.
setArrayShape :: ArrayShape newshape =>
                 TypeBase as oldshape -> newshape -> TypeBase as newshape
setArrayShape (Array et _ u as) ds = Array et ds u as
setArrayShape (Basic t)  _         = Basic t

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
       && shape1 == shape2
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

-- | Replace no aliasing with an empty alias set.
fromDecl :: DeclType -> ExpType
fromDecl (Array et sz u _) = Array et sz u HS.empty
fromDecl (Basic et) = Basic et

-- | Add (empty) aliasing information to a type.
fromConstType :: ConstType -> Type
fromConstType t = t `setAliases` mempty

-- | Remove aliasing information from a type.
toConstType :: TypeBase als Shape -> ConstType
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
  where rt = Array et (Shape $ replicate (n-1) zero) u as
        zero = Constant (BasicVal $ IntVal 0) noLoc

valueType :: Value -> ConstType
valueType (BasicVal v) =
  Basic $ basicValueType v
valueType v@(ArrayVal _ (Basic et)) =
  Array et (Shape [n]) Nonunique ()
  where n = Constant (BasicVal $ IntVal $ valueSize v) noLoc
valueType v@(ArrayVal _ (Array et _ _ _)) =
  Array et (Shape $ map intconst $ valueShape v) Nonunique ()
  where intconst x = Constant (BasicVal $ IntVal x) noLoc

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
emptyArray :: TypeBase as Shape -> Value
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

shapeExps :: SubExp -> [Exp]
shapeExps src = [Size [] i src loc | i <- [0..arrayRank (subExpType src) - 1] ]
  where loc = srclocOf src

-- | @reshapeOuter shape n src@ returns a 'Reshape' expression that
-- replaces the outer @n@ dimensions of @src@ with @shape@.
reshapeOuter :: [Exp] -> Int -> SubExp -> [Exp]
reshapeOuter shape n src = shape ++ drop n (shapeExps src)

-- | @reshapeInner shape n src@ returns a 'Reshape' expression that
-- replaces the inner @m-n@ dimensions (where @m@ is the rank of
-- @src@) of @src@ with @shape@.
reshapeInner :: [Exp] -> Int -> SubExp -> [Exp]
reshapeInner shape n src = take n (shapeExps src) ++ shape

varType :: Ident -> Type
varType ident = identType ident `changeAliases` HS.insert (identName ident)

subExpType :: SubExp -> Type
subExpType (Constant val _) = fromConstType $ valueType val
subExpType (Var ident)      = varType ident

bodyType :: Body -> [Type]
bodyType (LetPat _ _ body _) = bodyType body
bodyType (LetWith _ _ _ _ _ body _) = bodyType body
bodyType (DoLoop _ _ _ _ body _) = bodyType body
bodyType (Result _ ses _) = map subExpType ses

-- | The type of an L0 term.  The aliasing will refer to itself, if
-- the term is a non-tuple-typed variable.
typeOf :: Exp -> [Type]
typeOf (SubExp e) = [subExpType e]
typeOf (TupLit vs _) = map subExpType vs
typeOf (ArrayLit es t _) =
  [arrayOf t (Shape [n]) $ mconcat $ map (uniqueness . subExpType) es]
  where n = Constant (BasicVal $ IntVal $ length es) noLoc
typeOf (BinOp _ _ _ t _) = [t]
typeOf (Not _ _) = [Basic Bool]
typeOf (Negate e _) = [subExpType e]
typeOf (If _ _ _ t _) = t
typeOf (Apply _ _ t _) = t
typeOf (Index _ ident idx _) =
  [stripArray (length idx) (varType ident)
   `changeAliases` HS.insert (identName ident)]
typeOf (Iota ne _) =
  [arrayOf (Basic Int) (Shape [ne]) Unique]
typeOf (Size {}) = [Basic Int]
typeOf (Replicate ne e _) =
  [arrayOf (subExpType e) (Shape [ne]) u]
  where u | uniqueOrBasic (subExpType e) = Unique
          | otherwise = Nonunique
typeOf (Reshape _ [] e _) =
  [Basic $ elemType $ subExpType e]
typeOf (Reshape _ shape e _) =
  [subExpType e `setArrayShape` Shape shape]
typeOf (Rearrange _ perm e _) =
  [subExpType e `setArrayShape` Shape (permuteShape perm shape)]
  where Shape shape = arrayShape $ subExpType e
typeOf (Split _ _ e _) =
  [subExpType e, subExpType e]
typeOf (Concat _ x y _) =
  [subExpType x `setUniqueness` u]
  where u = uniqueness (subExpType x) <> uniqueness (subExpType y)
typeOf (Copy e _) =
  [subExpType e `setUniqueness` Unique `setAliases` HS.empty]
typeOf (Assert _ _) = [Basic Cert]
typeOf (Conjoin _ _) = [Basic Cert]
typeOf (Map _ f arrs _)
  | arr : _     <- arrs,
    Shape (n:_) <- arrayShape $ subExpType arr =
    map (\x -> arrayOf x (Shape [n]) (uniqueProp x)) $ lambdaType f $ map subExpType arrs
  | otherwise = []
typeOf (Reduce _ fun inputs _) =
  lambdaType fun $ map subExpType acc ++ map subExpType arrs
  where (acc, arrs) = unzip inputs
typeOf (Scan _ _ inputs _) =
  map ((`setUniqueness` Unique) . subExpType) arrs
  where (_, arrs) = unzip inputs
typeOf (Filter _ _ arrs outer_shape _) =
  map (setOuterShape . subExpType) arrs
  where setOuterShape t =
          let Shape dims = arrayShape t
          in t `setArrayShape` Shape (Var outer_shape : drop 1 dims)
typeOf (Redomap _ outerfun innerfun acc arrs _) =
  lambdaType outerfun $ lambdaType innerfun (innerres ++ innerres)
  where innerres = lambdaType innerfun
                   (map subExpType acc ++ map (rowType . subExpType) arrs)

uniqueProp :: TypeBase as shape -> Uniqueness
uniqueProp tp = if uniqueOrBasic tp then Unique else Nonunique

-- | The result of applying the arguments of the given types to the
-- given tuple lambda function.
lambdaType :: Lambda -> [Type] -> [Type]
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

-- | Find the function of the given name in the L0 program.
funDecByName :: Name -> Prog -> Maybe FunDec
funDecByName fname = find (\(fname',_,_,_,_) -> fname == fname') . progFunctions

-- | Return the set of all variable names bound in a program.
progNames :: Prog -> HS.HashSet VName
progNames = execWriter . mapM funNames . progFunctions
  where names = identityWalker {
                  walkOnExp = expNames
                , walkOnBody = bodyNames
                , walkOnLambda = lambdaNames
                }

        one = tell . HS.singleton . identName
        funNames (_, _, params, body, _) =
          mapM_ one params >> bodyNames body

        bodyNames e@(LetWith _ dest _ _ _ _ _) =
          one dest >> walkBodyM names e
        bodyNames e@(LetPat pat _ _ _) =
          mapM_ one pat >> walkBodyM names e
        bodyNames e@(DoLoop pat i _ _ _ _) =
          mapM_ (one . fst) pat >> one i >> walkBodyM names e
        bodyNames e = walkBodyM names e

        expNames = walkExpM names

        lambdaNames (Lambda params body _ _) =
          mapM_ one params >> bodyNames body

-- | Change that subexpression where evaluation of the body would
-- stop.  Also change type annotations at branches.
mapTailM :: (Applicative m, Monad m) => (Certificates -> [SubExp] -> m Body) -> Body -> m Body
mapTailM f (LetPat pat e body loc) =
  LetPat pat e <$> mapTailM f body <*> pure loc
mapTailM f (LetWith cs dest src idxs ve body loc) =
  LetWith cs dest src idxs ve <$> mapTailM f body <*> pure loc
mapTailM f (DoLoop pat i bound loopbody body loc) =
  DoLoop pat i bound loopbody <$> mapTailM f body <*> pure loc
mapTailM f (Result cs ses _) = f cs ses

-- | Change that result where evaluation of the body would stop.  Also
-- change type annotations at branches.  This a non-monadic variant of
-- @mapTailM@.
mapTail :: (Certificates -> [SubExp] -> Body) -> Body -> Body
mapTail f e = runIdentity $ mapTailM (((.).(.)) return f) e

freeWalker :: Walker (Writer (HS.HashSet Ident))
freeWalker = identityWalker {
               walkOnSubExp = subExpFree
             , walkOnBody = bodyFree
             , walkOnExp = expFree
             , walkOnLambda = lambdaFree
             , walkOnIdent = identFree
             , walkOnCertificates = mapM_ identFree
             , walkOnType = typeFree
             }
  where identFree ident =
          tell $ HS.singleton ident

        subExpFree (Var ident) = identFree ident
        subExpFree (Constant {})  = return ()

        bodyFree (LetPat pat e body _) = do
          expFree e
          binding (HS.fromList pat) $ bodyFree body
        bodyFree (LetWith cs dest src idxs ve body _) = do
          mapM_ identFree cs
          identFree src
          mapM_ subExpFree idxs
          subExpFree ve
          binding (HS.singleton dest) $ bodyFree body
        bodyFree (DoLoop pat i boundexp loopbody letbody _) = do
          mapM_ (subExpFree . snd) pat
          subExpFree boundexp
          binding (i `HS.insert` HS.fromList (map fst pat)) $ do
            bodyFree loopbody
            bodyFree letbody
        bodyFree (Result cs ses _) = do
          mapM_ identFree cs
          mapM_ subExpFree ses

        expFree = walkExpM freeWalker

        lambdaFree = tell . freeInLambda

        typeFree = mapM_ subExpFree . shapeDims . arrayShape

        binding bound = censor (`HS.difference` bound)

-- | Return the set of identifiers that are free in the given
-- body.
freeInBody :: Body -> HS.HashSet Ident
freeInBody = execWriter . walkOnBody freeWalker

-- | As 'freeInBody', but returns the raw names rather than 'Ident's.
freeNamesInBody :: Body -> HS.HashSet VName
freeNamesInBody = HS.map identName . freeInBody

-- | Return the set of identifiers that are free in the given
-- expression.
freeInExp :: Exp -> HS.HashSet Ident
freeInExp = execWriter . walkOnExp freeWalker

-- | As 'freeInExp', but returns the raw names rather than 'Ident's.
freeNamesInExp :: Exp -> HS.HashSet VName
freeNamesInExp = HS.map identName . freeInExp

-- | Return the set of variables names consumed by the given
-- body.
consumedInBody :: Body -> HS.HashSet VName
consumedInBody = execWriter . bodyConsumed
  where unconsume s = censor (`HS.difference` s)

        consume ident =
          tell $ identName ident `HS.insert` aliases (identType ident)

        bodyConsumed (LetPat pat e body _) = do
          expConsumed e
          unconsume (HS.fromList $ map identName pat) $ bodyConsumed body
        bodyConsumed (LetWith _ dest src _ _ body _) = do
          consume src
          unconsume (HS.singleton $ identName dest) $ bodyConsumed body
        bodyConsumed (DoLoop pat i _ loopbody letbody _) =
          unconsume (identName i `HS.insert`
                     HS.fromList (map (identName . fst) pat)) $ do
            bodyConsumed loopbody
            bodyConsumed letbody
        bodyConsumed (Result {}) = return ()

        expConsumed = tell . consumedInExp

-- | Return the set of variables names consumed by the given
-- body.
consumedInExp :: Exp -> HS.HashSet VName
consumedInExp (Apply _ args _ _) =
  mconcat $ map (consumeArg . first subExpType) args
  where consumeArg (t, Consume) = aliases t
        consumeArg (_, Observe) = mempty
consumedInExp _ = mempty

-- | An expression is safe if it is always well-defined (assuming that
-- any required certificates have been checked) in any context.  For
-- example, array indexing is not safe, as the index may be out of
-- bounds.  On the other hand, adding two numbers cannot fail.
safeExp :: Exp -> Bool
safeExp (Index {}) = False
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
safeExp _ = False

-- | Return the set of identifiers that are free in the given lambda.
freeInLambda :: Lambda -> HS.HashSet Ident
freeInLambda (Lambda params body _ _) =
  inParams <> inBody
  where inParams = mconcat $ map freeInParam params
        freeInParam = mconcat . map (freeInExp . SubExp) . shapeDims . arrayShape . identType
        inBody = HS.filter ((`notElem` paramnames) . identName) $ freeInBody body
        paramnames = map identName params

-- | As 'freeInLambda', but returns the raw names rather than
-- 'IdentBase's.
freeNamesInLambda :: Lambda -> HS.HashSet VName
freeNamesInLambda = HS.map identName . freeInLambda

-- | Convert an identifier to a 'ParamBase'.
toParam :: Ident -> Param
toParam (Ident name t loc) = Ident name (t `setAliases` ()) loc

-- | Convert a 'ParamBase' to an identifier.
fromParam :: Param -> Ident
fromParam (Ident name t loc) = Ident name (t `setAliases` mempty) loc
