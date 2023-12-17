{-# LANGUAGE TypeFamilies #-}

-- | = Constructing Futhark ASTs
--
-- This module re-exports and defines a bunch of building blocks for
-- constructing fragments of Futhark ASTs.  More importantly, it also
-- contains a basic introduction on how to use them.
--
-- The "Futhark.IR.Syntax" module contains the core
-- AST definition.  One important invariant is that all bound names in
-- a Futhark program must be /globally/ unique.  In principle, you
-- could use the facilities from "Futhark.MonadFreshNames" (or your
-- own bespoke source of unique names) to manually construct
-- expressions, statements, and entire ASTs.  In practice, this would
-- be very tedious.  Instead, we have defined a collection of building
-- blocks (centered around the 'MonadBuilder' type class) that permits
-- a more abstract way of generating code.
--
-- Constructing ASTs with these building blocks requires you to ensure
-- that all free variables are in scope.  See
-- "Futhark.IR.Prop.Scope".
--
-- == 'MonadBuilder'
--
-- A monad that implements 'MonadBuilder' tracks the statements added
-- so far, the current names in scope, and allows you to add
-- additional statements with 'addStm'.  Any monad that implements
-- 'MonadBuilder' also implements the t'Rep' type family, which
-- indicates which rep it works with.  Inside a 'MonadBuilder' we can
-- use 'collectStms' to gather up the 'Stms' added with 'addStm' in
-- some nested computation.
--
-- The 'BuilderT' monad (and its convenient 'Builder' version) provides
-- the simplest implementation of 'MonadBuilder'.
--
-- == Higher-level building blocks
--
-- On top of the raw facilities provided by 'MonadBuilder', we have
-- more convenient facilities.  For example, 'letSubExp' lets us
-- conveniently create a 'Stm' for an 'Exp' that produces a /single/
-- value, and returns the (fresh) name for the resulting variable:
--
-- @
-- z <- letExp "z" $ BasicOp $ BinOp (Add Int32) (Var x) (Var y)
-- @
--
-- == Monadic expression builders
--
-- This module also contains "monadic expression" functions that let
-- us build nested expressions in a "direct" style, rather than using
-- 'letExp' and friends to bind every sub-part first.  See functions
-- such as 'eIf' and 'eBody' for example.  See also
-- "Futhark.Analysis.PrimExp" and the 'ToExp' type class.
--
-- == Examples
--
-- The "Futhark.Transform.FirstOrderTransform" module is a
-- (relatively) simple example of how to use these components.  As are
-- some of the high-level building blocks in this very module.
module Futhark.Construct
  ( -- * Basic building blocks
    module Futhark.Builder,
    letSubExp,
    letExp,
    letTupExp,
    letTupExp',
    letInPlace,

    -- * Monadic expression builders
    eSubExp,
    eParam,
    eMatch',
    eMatch,
    eIf,
    eIf',
    eBinOp,
    eUnOp,
    eCmpOp,
    eConvOp,
    eSignum,
    eCopy,
    eBody,
    eLambda,
    eBlank,
    eAll,
    eAny,
    eDimInBounds,
    eOutOfBounds,
    eIndex,
    eLast,

    -- * Other building blocks
    asIntZ,
    asIntS,
    resultBody,
    resultBodyM,
    insertStmsM,
    buildBody,
    buildBody_,
    mapResult,
    foldBinOp,
    binOpLambda,
    cmpOpLambda,
    mkLambda,
    sliceDim,
    fullSlice,
    fullSliceNum,
    isFullSlice,
    sliceAt,

    -- * Result types
    instantiateShapes,
    instantiateShapes',
    removeExistentials,

    -- * Convenience
    simpleMkLetNames,
    ToExp (..),
    toSubExp,
  )
where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Data.List (foldl', sortOn, transpose)
import Data.Map.Strict qualified as M
import Futhark.Builder
import Futhark.IR
import Futhark.Util (maybeNth)

-- | @letSubExp desc e@ binds the expression @e@, which must produce a
-- single value.  Returns a t'SubExp' corresponding to the resulting
-- value.  For expressions that produce multiple values, see
-- 'letTupExp'.
letSubExp ::
  (MonadBuilder m) =>
  String ->
  Exp (Rep m) ->
  m SubExp
letSubExp _ (BasicOp (SubExp se)) = pure se
letSubExp desc e = Var <$> letExp desc e

-- | Like 'letSubExp', but returns a name rather than a t'SubExp'.
letExp ::
  (MonadBuilder m) =>
  String ->
  Exp (Rep m) ->
  m VName
letExp _ (BasicOp (SubExp (Var v))) =
  pure v
letExp desc e = do
  n <- length <$> expExtType e
  vs <- replicateM n $ newVName desc
  letBindNames vs e
  case vs of
    [v] -> pure v
    _ -> error $ "letExp: tuple-typed expression given:\n" ++ prettyString e

-- | Like 'letExp', but the 'VName' and 'Slice' denote an array that
-- is 'Update'd with the result of the expression.  The name of the
-- updated array is returned.
letInPlace ::
  (MonadBuilder m) =>
  String ->
  VName ->
  Slice SubExp ->
  Exp (Rep m) ->
  m VName
letInPlace desc src slice e = do
  tmp <- letSubExp (desc ++ "_tmp") e
  letExp desc $ BasicOp $ Update Unsafe src slice tmp

-- | Like 'letExp', but the expression may return multiple values.
letTupExp ::
  (MonadBuilder m) =>
  String ->
  Exp (Rep m) ->
  m [VName]
letTupExp _ (BasicOp (SubExp (Var v))) =
  pure [v]
letTupExp name e = do
  e_t <- expExtType e
  names <- replicateM (length e_t) $ newVName name
  letBindNames names e
  pure names

-- | Like 'letTupExp', but returns t'SubExp's instead of 'VName's.
letTupExp' ::
  (MonadBuilder m) =>
  String ->
  Exp (Rep m) ->
  m [SubExp]
letTupExp' _ (BasicOp (SubExp se)) = pure [se]
letTupExp' name ses = map Var <$> letTupExp name ses

-- | Turn a subexpression into a monad expression.  Does not actually
-- lead to any code generation.  This is supposed to be used alongside
-- the other monadic expression functions, such as 'eIf'.
eSubExp ::
  (MonadBuilder m) =>
  SubExp ->
  m (Exp (Rep m))
eSubExp = pure . BasicOp . SubExp

-- | Treat a parameter as a monadic expression.
eParam ::
  (MonadBuilder m) =>
  Param t ->
  m (Exp (Rep m))
eParam = eSubExp . Var . paramName

removeRedundantScrutinees :: [SubExp] -> [Case b] -> ([SubExp], [Case b])
removeRedundantScrutinees ses cases =
  let (ses', vs) =
        unzip $ filter interesting $ zip ses $ transpose (map casePat cases)
   in (ses', zipWith Case (transpose vs) $ map caseBody cases)
  where
    interesting = any (/= Nothing) . snd

-- | As 'eMatch', but an 'MatchSort' can be given.
eMatch' ::
  (MonadBuilder m, BranchType (Rep m) ~ ExtType) =>
  [SubExp] ->
  [Case (m (Body (Rep m)))] ->
  m (Body (Rep m)) ->
  MatchSort ->
  m (Exp (Rep m))
eMatch' ses cases_m defbody_m sort = do
  cases <- mapM (traverse insertStmsM) cases_m
  defbody <- insertStmsM defbody_m
  ts <-
    foldl' generaliseExtTypes
      <$> bodyExtType defbody
      <*> mapM (bodyExtType . caseBody) cases
  cases' <- mapM (traverse $ addContextForBranch ts) cases
  defbody' <- addContextForBranch ts defbody
  let ts' = replicate (length (shapeContext ts)) (Prim int64) ++ ts
      (ses', cases'') = removeRedundantScrutinees ses cases'
  pure $ Match ses' cases'' defbody' $ MatchDec ts' sort
  where
    addContextForBranch ts (Body _ stms val_res) = do
      body_ts <- extendedScope (traverse subExpResType val_res) stmsscope
      let ctx_res =
            map snd $ sortOn fst $ M.toList $ shapeExtMapping ts body_ts
      mkBodyM stms $ subExpsRes ctx_res ++ val_res
      where
        stmsscope = scopeOf stms

-- | Construct a 'Match' expression.  The main convenience here is
-- that the existential context of the return type is automatically
-- deduced, and the necessary elements added to the branches.
eMatch ::
  (MonadBuilder m, BranchType (Rep m) ~ ExtType) =>
  [SubExp] ->
  [Case (m (Body (Rep m)))] ->
  m (Body (Rep m)) ->
  m (Exp (Rep m))
eMatch ses cases_m defbody_m = eMatch' ses cases_m defbody_m MatchNormal

-- | Construct a 'Match' modelling an if-expression from a monadic
-- condition and monadic branches.  'eBody' might be convenient for
-- constructing the branches.
eIf ::
  (MonadBuilder m, BranchType (Rep m) ~ ExtType) =>
  m (Exp (Rep m)) ->
  m (Body (Rep m)) ->
  m (Body (Rep m)) ->
  m (Exp (Rep m))
eIf ce te fe = eIf' ce te fe MatchNormal

-- | As 'eIf', but an 'MatchSort' can be given.
eIf' ::
  (MonadBuilder m, BranchType (Rep m) ~ ExtType) =>
  m (Exp (Rep m)) ->
  m (Body (Rep m)) ->
  m (Body (Rep m)) ->
  MatchSort ->
  m (Exp (Rep m))
eIf' ce te fe if_sort = do
  ce' <- letSubExp "cond" =<< ce
  eMatch' [ce'] [Case [Just $ BoolValue True] te] fe if_sort

-- The type of a body.  Watch out: this only works for the degenerate
-- case where the body does not already return its context.
bodyExtType :: (HasScope rep m, Monad m) => Body rep -> m [ExtType]
bodyExtType (Body _ stms res) =
  existentialiseExtTypes (M.keys stmsscope) . staticShapes
    <$> extendedScope (traverse subExpResType res) stmsscope
  where
    stmsscope = scopeOf stms

-- | Construct a v'BinOp' expression with the given operator.
eBinOp ::
  (MonadBuilder m) =>
  BinOp ->
  m (Exp (Rep m)) ->
  m (Exp (Rep m)) ->
  m (Exp (Rep m))
eBinOp op x y = do
  x' <- letSubExp "x" =<< x
  y' <- letSubExp "y" =<< y
  pure $ BasicOp $ BinOp op x' y'

-- | Construct a v'UnOp' expression with the given operator.
eUnOp ::
  (MonadBuilder m) =>
  UnOp ->
  m (Exp (Rep m)) ->
  m (Exp (Rep m))
eUnOp op x = BasicOp . UnOp op <$> (letSubExp "x" =<< x)

-- | Construct a v'CmpOp' expression with the given comparison.
eCmpOp ::
  (MonadBuilder m) =>
  CmpOp ->
  m (Exp (Rep m)) ->
  m (Exp (Rep m)) ->
  m (Exp (Rep m))
eCmpOp op x y = do
  x' <- letSubExp "x" =<< x
  y' <- letSubExp "y" =<< y
  pure $ BasicOp $ CmpOp op x' y'

-- | Construct a v'ConvOp' expression with the given conversion.
eConvOp ::
  (MonadBuilder m) =>
  ConvOp ->
  m (Exp (Rep m)) ->
  m (Exp (Rep m))
eConvOp op x = do
  x' <- letSubExp "x" =<< x
  pure $ BasicOp $ ConvOp op x'

-- | Construct a 'SSignum' expression.  Fails if the provided
-- expression is not of integer type.
eSignum ::
  (MonadBuilder m) =>
  m (Exp (Rep m)) ->
  m (Exp (Rep m))
eSignum em = do
  e <- em
  e' <- letSubExp "signum_arg" e
  t <- subExpType e'
  case t of
    Prim (IntType int_t) ->
      pure $ BasicOp $ UnOp (SSignum int_t) e'
    _ ->
      error $ "eSignum: operand " ++ prettyString e ++ " has invalid type."

-- | Copy a value.
eCopy ::
  (MonadBuilder m) =>
  m (Exp (Rep m)) ->
  m (Exp (Rep m))
eCopy e = BasicOp . Replicate mempty <$> (letSubExp "copy_arg" =<< e)

-- | Construct a body from expressions.  If multiple expressions are
-- provided, their results will be concatenated in order and returned
-- as the result.
--
-- /Beware/: this will not produce correct code if the type of the
-- body would be existential.  That is, the type of the results being
-- returned should be invariant to the body.
eBody ::
  (MonadBuilder m) =>
  [m (Exp (Rep m))] ->
  m (Body (Rep m))
eBody es = buildBody_ $ do
  es' <- sequence es
  xs <- mapM (letTupExp "x") es'
  pure $ varsRes $ concat xs

-- | Bind each lambda parameter to the result of an expression, then
-- bind the body of the lambda.  The expressions must produce only a
-- single value each.
eLambda ::
  (MonadBuilder m) =>
  Lambda (Rep m) ->
  [m (Exp (Rep m))] ->
  m [SubExpRes]
eLambda lam args = do
  zipWithM_ bindParam (lambdaParams lam) args
  bodyBind $ lambdaBody lam
  where
    bindParam param arg = letBindNames [paramName param] =<< arg

-- | @eInBoundsForDim w i@ produces @0 <= i < w@.
eDimInBounds :: (MonadBuilder m) => m (Exp (Rep m)) -> m (Exp (Rep m)) -> m (Exp (Rep m))
eDimInBounds w i =
  eBinOp
    LogAnd
    (eCmpOp (CmpSle Int64) (eSubExp (intConst Int64 0)) i)
    (eCmpOp (CmpSlt Int64) i w)

-- | Are these indexes out-of-bounds for the array?
eOutOfBounds ::
  (MonadBuilder m) =>
  VName ->
  [m (Exp (Rep m))] ->
  m (Exp (Rep m))
eOutOfBounds arr is = do
  arr_t <- lookupType arr
  let ws = arrayDims arr_t
  is' <- mapM (letSubExp "write_i") =<< sequence is
  let checkDim w i = do
        less_than_zero <-
          letSubExp "less_than_zero" $
            BasicOp $
              CmpOp (CmpSlt Int64) i (constant (0 :: Int64))
        greater_than_size <-
          letSubExp "greater_than_size" $
            BasicOp $
              CmpOp (CmpSle Int64) w i
        letSubExp "outside_bounds_dim" $
          BasicOp $
            BinOp LogOr less_than_zero greater_than_size
  foldBinOp LogOr (constant False) =<< zipWithM checkDim ws is'

-- | The array element at this index.  Returns array unmodified if
-- indexes are null (does not even need to be an array in that case).
eIndex :: (MonadBuilder m) => VName -> [m (Exp (Rep m))] -> m (Exp (Rep m))
eIndex arr [] = eSubExp $ Var arr
eIndex arr is = do
  is' <- mapM (letSubExp "i" =<<) is
  arr_t <- lookupType arr
  pure $ BasicOp $ Index arr $ fullSlice arr_t $ map DimFix is'

-- | The last element of the given array.
eLast :: (MonadBuilder m) => VName -> m (Exp (Rep m))
eLast arr = do
  n <- arraySize 0 <$> lookupType arr
  nm1 <-
    letSubExp "nm1" . BasicOp $
      BinOp (Sub Int64 OverflowUndef) n (intConst Int64 1)
  eIndex arr [eSubExp nm1]

-- | Construct an unspecified value of the given type.
eBlank :: (MonadBuilder m) => Type -> m (Exp (Rep m))
eBlank (Prim t) = pure $ BasicOp $ SubExp $ Constant $ blankPrimValue t
eBlank (Array t shape _) = pure $ BasicOp $ Scratch t $ shapeDims shape
eBlank Acc {} = error "eBlank: cannot create blank accumulator"
eBlank Mem {} = error "eBlank: cannot create blank memory"

-- | Sign-extend to the given integer type.
asIntS :: (MonadBuilder m) => IntType -> SubExp -> m SubExp
asIntS = asInt SExt

-- | Zero-extend to the given integer type.
asIntZ :: (MonadBuilder m) => IntType -> SubExp -> m SubExp
asIntZ = asInt ZExt

asInt ::
  (MonadBuilder m) =>
  (IntType -> IntType -> ConvOp) ->
  IntType ->
  SubExp ->
  m SubExp
asInt ext to_it e = do
  e_t <- subExpType e
  case e_t of
    Prim (IntType from_it)
      | to_it == from_it -> pure e
      | otherwise -> letSubExp s $ BasicOp $ ConvOp (ext from_it to_it) e
    _ -> error "asInt: wrong type"
  where
    s = case e of
      Var v -> baseString v
      _ -> "to_" ++ prettyString to_it

-- | Apply a binary operator to several subexpressions.  A left-fold.
foldBinOp ::
  (MonadBuilder m) =>
  BinOp ->
  SubExp ->
  [SubExp] ->
  m (Exp (Rep m))
foldBinOp _ ne [] =
  pure $ BasicOp $ SubExp ne
foldBinOp bop ne (e : es) =
  eBinOp bop (pure $ BasicOp $ SubExp e) (foldBinOp bop ne es)

-- | True if all operands are true.
eAll :: (MonadBuilder m) => [SubExp] -> m (Exp (Rep m))
eAll [] = pure $ BasicOp $ SubExp $ constant True
eAll [x] = eSubExp x
eAll (x : xs) = foldBinOp LogAnd x xs

-- | True if any operand is true.
eAny :: (MonadBuilder m) => [SubExp] -> m (Exp (Rep m))
eAny [] = pure $ BasicOp $ SubExp $ constant False
eAny [x] = eSubExp x
eAny (x : xs) = foldBinOp LogOr x xs

-- | Create a two-parameter lambda whose body applies the given binary
-- operation to its arguments.  It is assumed that both argument and
-- result types are the same.  (This assumption should be fixed at
-- some point.)
binOpLambda ::
  (MonadBuilder m, Buildable (Rep m)) =>
  BinOp ->
  PrimType ->
  m (Lambda (Rep m))
binOpLambda bop t = binLambda (BinOp bop) t t

-- | As 'binOpLambda', but for t'CmpOp's.
cmpOpLambda ::
  (MonadBuilder m, Buildable (Rep m)) =>
  CmpOp ->
  m (Lambda (Rep m))
cmpOpLambda cop = binLambda (CmpOp cop) (cmpOpType cop) Bool

binLambda ::
  (MonadBuilder m, Buildable (Rep m)) =>
  (SubExp -> SubExp -> BasicOp) ->
  PrimType ->
  PrimType ->
  m (Lambda (Rep m))
binLambda bop arg_t ret_t = do
  x <- newVName "x"
  y <- newVName "y"
  body <-
    buildBody_ . fmap (pure . subExpRes) $
      letSubExp "binlam_res" $
        BasicOp $
          bop (Var x) (Var y)
  pure
    Lambda
      { lambdaParams =
          [ Param mempty x (Prim arg_t),
            Param mempty y (Prim arg_t)
          ],
        lambdaReturnType = [Prim ret_t],
        lambdaBody = body
      }

-- | Easily construct a t'Lambda' within a 'MonadBuilder'.  See also
-- 'runLambdaBuilder'.
mkLambda ::
  (MonadBuilder m) =>
  [LParam (Rep m)] ->
  m Result ->
  m (Lambda (Rep m))
mkLambda params m = do
  (body, ret) <- buildBody . localScope (scopeOfLParams params) $ do
    res <- m
    ret <- mapM subExpResType res
    pure (res, ret)
  pure $ Lambda params ret body

-- | Slice a full dimension of the given size.
sliceDim :: SubExp -> DimIndex SubExp
sliceDim d = DimSlice (constant (0 :: Int64)) d (constant (1 :: Int64))

-- | @fullSlice t slice@ returns @slice@, but with 'DimSlice's of
-- entire dimensions appended to the full dimensionality of @t@.  This
-- function is used to turn incomplete indexing complete, as required
-- by 'Index'.
fullSlice :: Type -> [DimIndex SubExp] -> Slice SubExp
fullSlice t slice =
  Slice $ slice ++ map sliceDim (drop (length slice) $ arrayDims t)

-- | @ sliceAt t n slice@ returns @slice@ but with 'DimSlice's of the
-- outer @n@ dimensions prepended, and as many appended as to make it
-- a full slice.  This is a generalisation of 'fullSlice'.
sliceAt :: Type -> Int -> [DimIndex SubExp] -> Slice SubExp
sliceAt t n slice =
  fullSlice t $ map sliceDim (take n $ arrayDims t) ++ slice

-- | Like 'fullSlice', but the dimensions are simply numeric.
fullSliceNum :: (Num d) => [d] -> [DimIndex d] -> Slice d
fullSliceNum dims slice =
  Slice $ slice ++ map (\d -> DimSlice 0 d 1) (drop (length slice) dims)

-- | Does the slice describe the full size of the array?  The most
-- obvious such slice is one that 'DimSlice's the full span of every
-- dimension, but also one that fixes all unit dimensions.
isFullSlice :: Shape -> Slice SubExp -> Bool
isFullSlice shape slice = and $ zipWith allOfIt (shapeDims shape) (unSlice slice)
  where
    allOfIt (Constant v) DimFix {} = oneIsh v
    allOfIt d (DimSlice _ n _) = d == n
    allOfIt _ _ = False

-- | Conveniently construct a body that contains no bindings.
resultBody :: (Buildable rep) => [SubExp] -> Body rep
resultBody = mkBody mempty . subExpsRes

-- | Conveniently construct a body that contains no bindings - but
-- this time, monadically!
resultBodyM :: (MonadBuilder m) => [SubExp] -> m (Body (Rep m))
resultBodyM = mkBodyM mempty . subExpsRes

-- | Evaluate the action, producing a body, then wrap it in all the
-- bindings it created using 'addStm'.
insertStmsM ::
  (MonadBuilder m) =>
  m (Body (Rep m)) ->
  m (Body (Rep m))
insertStmsM m = do
  (Body _ stms res, otherstms) <- collectStms m
  mkBodyM (otherstms <> stms) res

-- | Evaluate an action that produces a 'Result' and an auxiliary
-- value, then return the body constructed from the 'Result' and any
-- statements added during the action, along the auxiliary value.
buildBody ::
  (MonadBuilder m) =>
  m (Result, a) ->
  m (Body (Rep m), a)
buildBody m = do
  ((res, v), stms) <- collectStms m
  body <- mkBodyM stms res
  pure (body, v)

-- | As 'buildBody', but there is no auxiliary value.
buildBody_ ::
  (MonadBuilder m) =>
  m Result ->
  m (Body (Rep m))
buildBody_ m = fst <$> buildBody ((,()) <$> m)

-- | Change that result where evaluation of the body would stop.  Also
-- change type annotations at branches.
mapResult ::
  (Buildable rep) =>
  (Result -> Body rep) ->
  Body rep ->
  Body rep
mapResult f (Body _ stms res) =
  let Body _ stms2 newres = f res
   in mkBody (stms <> stms2) newres

-- | Instantiate all existential parts dimensions of the given
-- type, using a monadic action to create the necessary t'SubExp's.
-- You should call this function within some monad that allows you to
-- collect the actions performed (say, 'State').
instantiateShapes ::
  (Monad m) =>
  (Int -> m SubExp) ->
  [TypeBase ExtShape u] ->
  m [TypeBase Shape u]
instantiateShapes f ts = evalStateT (mapM instantiate ts) M.empty
  where
    instantiate t = do
      shape <- mapM instantiate' $ shapeDims $ arrayShape t
      pure $ t `setArrayShape` Shape shape
    instantiate' (Ext x) = do
      m <- get
      case M.lookup x m of
        Just se -> pure se
        Nothing -> do
          se <- lift $ f x
          put $ M.insert x se m
          pure se
    instantiate' (Free se) = pure se

-- | Like 'instantiateShapes', but obtains names from the provided
-- list.  If an 'Ext' is out of bounds of this list, the function
-- fails with 'error'.
instantiateShapes' :: [VName] -> [TypeBase ExtShape u] -> [TypeBase Shape u]
instantiateShapes' names ts =
  -- Carefully ensure that the order of idents we produce corresponds
  -- to their existential index.
  runIdentity $ instantiateShapes instantiate ts
  where
    instantiate x =
      case maybeNth x names of
        Nothing -> error $ "instantiateShapes': " ++ prettyString names ++ ", " ++ show x
        Just name -> pure $ Var name

-- | Remove existentials by imposing sizes from another type where
-- needed.
removeExistentials :: ExtType -> Type -> Type
removeExistentials t1 t2 =
  t1
    `setArrayDims` zipWith
      nonExistential
      (shapeDims $ arrayShape t1)
      (arrayDims t2)
  where
    nonExistential (Ext _) dim = dim
    nonExistential (Free dim) _ = dim

-- | Can be used as the definition of 'mkLetNames' for a 'Buildable'
-- instance for simple representations.
simpleMkLetNames ::
  ( ExpDec rep ~ (),
    LetDec rep ~ Type,
    MonadFreshNames m,
    TypedOp (Op rep),
    HasScope rep m
  ) =>
  [VName] ->
  Exp rep ->
  m (Stm rep)
simpleMkLetNames names e = do
  et <- expExtType e
  let ts = instantiateShapes' names et
  pure $ Let (Pat $ zipWith PatElem names ts) (defAux ()) e

-- | Instances of this class can be converted to Futhark expressions
-- within a 'MonadBuilder'.
class ToExp a where
  toExp :: (MonadBuilder m) => a -> m (Exp (Rep m))

instance ToExp SubExp where
  toExp = pure . BasicOp . SubExp

instance ToExp VName where
  toExp = pure . BasicOp . SubExp . Var

-- | A convenient composition of 'letSubExp' and 'toExp'.
toSubExp :: (MonadBuilder m, ToExp a) => String -> a -> m SubExp
toSubExp s e = letSubExp s =<< toExp e
