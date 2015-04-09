-- | High-level representation of SOACs.  When performing
-- SOAC-transformations, operating on normal 'Exp' values is somewhat
-- of a nuisance, as they can represent terms that are not proper
-- SOACs.  In contrast, this module exposes a SOAC representation that
-- does not enable invalid representations (except for type errors).
--
-- Furthermore, while standard normalised Futhark requires that the inputs
-- to a SOAC are variables or constants, the representation in this
-- module also supports various index-space transformations, like
-- @replicate@ or @rearrange@.  This is also very convenient when
-- implementing transformations.
--
-- The names exported by this module conflict with the standard Futhark
-- syntax tree constructors, so you are advised to use a qualified
-- import:
--
-- @
-- import Futhark.Analysis.HORepresentation.SOAC (SOAC)
-- import qualified Futhark.Analysis.HORepresentation.SOAC as SOAC
-- @
module Futhark.Analysis.HORepresentation.SOAC
  (
   -- * SOACs
    SOAC (..)
  , inputs
  , setInputs
  , lambda
  , setLambda
  , certificates
  , typeOf
  -- ** Converting to and from expressions
  , NotSOAC (..)
  , fromExp
  , toExp
  -- * SOAC inputs
  , Input (..)
  , varInput
  , identInput
  , isVarInput
  , addTransform
  , addTransforms
  , InputArray (..)
  , inputArray
  , inputRank
  , inputType
  , inputRowType
  , transformRows
  , transformTypeRows
  , transposeInput
  -- ** Converting to and from expressions
  , inputFromSubExp
  , inputsToSubExps
  -- ** Input transformations
  , ArrayTransforms
  , noTransforms
  , singleTransform
  , nullTransforms
  , (|>)
  , (<|)
  , viewf
  , ViewF(..)
  , viewl
  , ViewL(..)
  , ArrayTransform(..)
  , transformFromExp
  )
  where

import Control.Applicative

import Data.Foldable as Foldable
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as Seq

import Prelude

import qualified Futhark.Representation.AST as Futhark
import Futhark.Representation.AST
  hiding (Map, Reduce, Scan, Redomap,
          Var, Iota, Rearrange, Reshape, Replicate)
import Futhark.Substitute
import Futhark.Tools

-- | A single, simple transformation.  If you want several, don't just
-- create a list, use 'ArrayTransforms' instead.
data ArrayTransform = Rearrange Certificates [Int]
                    -- ^ A permutation of an otherwise valid input.
                    | Reshape Certificates [SubExp]
                    -- ^ A reshaping of an otherwise valid input.
                    | ReshapeOuter Certificates [SubExp]
                    -- ^ A reshaping of the outer dimension.
                    | ReshapeInner Certificates [SubExp]
                    -- ^ A reshaping of everything but the outer dimension.
                    | Replicate SubExp
                    -- ^ Replicate the rows of the array a number of times.
                      deriving (Show, Eq, Ord)

instance Substitute ArrayTransform where
  substituteNames substs (Rearrange cs xs) =
    Rearrange (substituteNames substs cs) xs
  substituteNames substs (Reshape cs ses) =
    Reshape (substituteNames substs cs) (substituteNames substs ses)
  substituteNames substs (ReshapeOuter cs ses) =
    ReshapeOuter (substituteNames substs cs) (substituteNames substs ses)
  substituteNames substs (ReshapeInner cs ses) =
    ReshapeInner (substituteNames substs cs) (substituteNames substs ses)
  substituteNames substs (Replicate se) =
    Replicate $ substituteNames substs se

-- | A sequence of array transformations, heavily inspired by
-- "Data.Seq".  You can decompose it using 'viewF' and 'viewL', and
-- grow it by using '|>' and '<|'.  These correspond closely to the
-- similar operations for sequences, except that appending will try to
-- normalise and simplify the transformation sequence.
--
-- The data type is opaque in order to enforce normalisation
-- invariants.  Basically, when you grow the sequence, the
-- implementation will try to coalesce neighboring permutations, for
-- example by composing permutations and removing identity
-- transformations.
newtype ArrayTransforms = ArrayTransforms (Seq.Seq ArrayTransform)
  deriving (Eq, Ord, Show)

instance Monoid ArrayTransforms where
  mempty = noTransforms
  ts1 `mappend` ts2 =
    case viewf ts2 of
      t :< ts2' -> (ts1 |> t) `mappend` ts2'
      EmptyF    -> ts1

instance Substitute ArrayTransforms where
  substituteNames substs (ArrayTransforms ts) =
    ArrayTransforms $ substituteNames substs <$> ts

-- | The empty transformation list.
noTransforms :: ArrayTransforms
noTransforms = ArrayTransforms Seq.empty

-- | Is it an empty transformation list?
nullTransforms :: ArrayTransforms -> Bool
nullTransforms (ArrayTransforms s) = Seq.null s

-- | A transformation list containing just a single transformation.
singleTransform :: ArrayTransform -> ArrayTransforms
singleTransform = ArrayTransforms . Seq.singleton

-- | Decompose the input-end of the transformation sequence.
viewf :: ArrayTransforms -> ViewF
viewf (ArrayTransforms s) = case Seq.viewl s of
                              t Seq.:< s' -> t :< ArrayTransforms s'
                              Seq.EmptyL  -> EmptyF

-- | A view of the first transformation to be applied.
data ViewF = EmptyF
           | ArrayTransform :< ArrayTransforms

-- | Decompose the output-end of the transformation sequence.
viewl :: ArrayTransforms -> ViewL
viewl (ArrayTransforms s) = case Seq.viewr s of
                              s' Seq.:> t -> ArrayTransforms s' :> t
                              Seq.EmptyR  -> EmptyL

-- | A view of the last transformation to be applied.
data ViewL = EmptyL
           | ArrayTransforms :> ArrayTransform

-- | Add a transform to the end of the transformation list.
(|>) :: ArrayTransforms -> ArrayTransform -> ArrayTransforms
(|>) = flip $ addTransform' extract add $ uncurry (flip (,))
   where extract ts' = case viewl ts' of
                         EmptyL     -> Nothing
                         ts'' :> t' -> Just (t', ts'')
         add t' (ArrayTransforms ts') = ArrayTransforms $ ts' Seq.|> t'

-- | Add a transform at the beginning of the transformation list.
(<|) :: ArrayTransform -> ArrayTransforms -> ArrayTransforms
(<|) = addTransform' extract add id
   where extract ts' = case viewf ts' of
                         EmptyF     -> Nothing
                         t' :< ts'' -> Just (t', ts'')
         add t' (ArrayTransforms ts') = ArrayTransforms $ t' Seq.<| ts'

addTransform' :: (ArrayTransforms -> Maybe (ArrayTransform, ArrayTransforms))
              -> (ArrayTransform -> ArrayTransforms -> ArrayTransforms)
              -> ((ArrayTransform,ArrayTransform) -> (ArrayTransform,ArrayTransform))
              -> ArrayTransform -> ArrayTransforms
              -> ArrayTransforms
addTransform' extract add swap t ts =
  fromMaybe (t `add` ts) $ do
    (t', ts') <- extract ts
    combined <- uncurry combineTransforms $ swap (t', t)
    Just $ if identityTransform combined then ts'
           else addTransform' extract add swap combined ts'

identityTransform :: ArrayTransform -> Bool
identityTransform (Rearrange _ perm) =
  Foldable.and $ zipWith (==) perm [0..]
identityTransform _ = False

combineTransforms :: ArrayTransform -> ArrayTransform -> Maybe ArrayTransform
combineTransforms (Rearrange cs2 perm2) (Rearrange cs1 perm1) =
  Just $ Rearrange (cs1++cs2) $ perm2 `permuteCompose` perm1
combineTransforms _ _ = Nothing

-- | Given an expression, determine whether the expression represents
-- an input transformation of an array variable.  If so, return the
-- variable and the transformation.  Only 'Rearrange' and 'Reshape'
-- are possible to express this way.
transformFromExp :: Exp lore -> Maybe (VName, ArrayTransform)
transformFromExp (PrimOp (Futhark.Rearrange cs perm v)) =
  Just (v, Rearrange cs perm)
transformFromExp (PrimOp (Futhark.Reshape cs shape v)) =
  Just (v, Reshape cs shape)
transformFromExp _ = Nothing

-- | The basic source of data for an array input - either an array
-- variable (possibly indexed) or an @iota@.
data InputArray = Var VName Type
                -- ^ Some array-typed variable in scope.
                | Iota SubExp
                -- ^ @iota(e)@.
                  deriving (Show, Eq, Ord)

instance Substitute InputArray where
  substituteNames substs (Var name t) =
    Var (substituteNames substs name) (substituteNames substs t)
  substituteNames substs (Iota e) =
    Iota $ substituteNames substs e

inputArrayToExp :: InputArray -> Exp lore
inputArrayToExp (Var k _) = PrimOp $ SubExp $ Futhark.Var k
inputArrayToExp (Iota e)  = PrimOp $ Futhark.Iota e

-- | One array input to a SOAC - a SOAC may have multiple inputs, but
-- all are of this form.  Only the array inputs are expressed with
-- this type; other arguments, such as initial accumulator values, are
-- plain expressions.  The transforms are done left-to-right, that is,
-- the first element of the 'ArrayTransform' list is applied first.
data Input = Input ArrayTransforms InputArray
             deriving (Show, Eq, Ord)

instance Substitute Input where
  substituteNames substs (Input ts a) =
    Input (substituteNames substs ts) (substituteNames substs a)

-- | Create a plain array variable input with no transformations.
varInput :: HasTypeEnv f => VName -> f Input
varInput v = withType <$> lookupType v
  where withType t = Input (ArrayTransforms Seq.empty) $ Var v t

-- | Create a plain array variable input with no transformations, from an 'Ident'.
identInput :: Ident -> Input
identInput v = Input (ArrayTransforms Seq.empty) $ Var (identName v) (identType v)

-- | If the given input is a plain variable input, with no transforms,
-- return the variable.
isVarInput :: Input -> Maybe VName
isVarInput (Input ts (Var v _)) | nullTransforms ts = Just v
isVarInput _                                        = Nothing

-- | Add a transformation to the end of the transformation list.
addTransform :: ArrayTransform -> Input -> Input
addTransform t (Input ts ia) =
  Input (ts |> t) ia

-- | Add several transformations to the end of the transformation
-- list.
addTransforms :: ArrayTransforms -> Input -> Input
addTransforms ts (Input ots ia) = Input (ots <> ts) ia

-- | If the given expression represents a normalised SOAC input,
-- return that input.
inputFromSubExp :: HasTypeEnv f => SubExp -> f (Maybe Input)
inputFromSubExp (Futhark.Var v) = Just <$> varInput v
inputFromSubExp _               = pure Nothing

-- | Convert SOAC inputs to the corresponding expressions.
inputsToSubExps :: (MonadBinder m) =>
                   [Input] -> m [VName]
inputsToSubExps = mapM inputToExp'
  where inputToExp' (Input (ArrayTransforms ts) ia) = do
          ia' <- letExp "soac_input" $ inputArrayToExp ia
          foldlM transform ia' ts

        transform ia (Replicate n) =
          letExp "repeat" $ PrimOp $ Futhark.Replicate n (Futhark.Var ia)

        transform ia (Rearrange cs perm) =
          letExp "rearrange" $ PrimOp $ Futhark.Rearrange cs perm ia

        transform ia (Reshape cs shape) =
          letExp "reshape" $ PrimOp $ Futhark.Reshape cs shape ia

        transform ia (ReshapeOuter cs shape) = do
          shape' <- reshapeOuter shape 1 <$> arrayShape <$> lookupType ia
          letExp "reshape_outer" $ PrimOp $ Futhark.Reshape cs shape' ia

        transform ia (ReshapeInner cs shape) = do
          shape' <- reshapeInner shape 1 <$> arrayShape <$> lookupType ia
          letExp "reshape_inner" $ PrimOp $ Futhark.Reshape cs shape' ia

-- | If the input is a (possibly rearranged, reshaped or otherwise
-- transformed) array variable, return that variable.
inputArray :: Input -> Maybe VName
inputArray (Input _ (Var v _)) = Just v
inputArray (Input _ (Iota _))  = Nothing

inputArrayType :: InputArray -> Type
inputArrayType (Var _ t) = t
inputArrayType (Iota e)  = arrayOf (Basic Int) (Shape [e]) Unique

-- | Return the type of an input.
inputType :: Input -> Type
inputType (Input (ArrayTransforms ts) ia) =
  Foldable.foldl transformType (inputArrayType ia) ts
  where transformType t (Replicate n) =
          arrayOf t (Shape [n]) u
          where u | unique t  = Unique
                  | otherwise = Nonunique
        transformType t (Rearrange _ perm) =
          let Shape oldshape = arrayShape t
          in t `setArrayShape` Shape (permuteShape perm oldshape)
        transformType t (Reshape _ shape) =
          t `setArrayShape` Shape shape
        transformType t (ReshapeOuter _ shape) =
          let Shape oldshape = arrayShape t
          in t `setArrayShape` Shape (shape ++ drop 1 oldshape)
        transformType t (ReshapeInner _ shape) =
          let Shape oldshape = arrayShape t
          in t `setArrayShape` Shape (take 1 oldshape ++ shape)

-- | Return the row type of an input.  Just a convenient alias.
inputRowType :: Input -> Type
inputRowType = rowType . inputType

-- | Return the array rank (dimensionality) of an input.  Just a
-- convenient alias.
inputRank :: Input -> Int
inputRank = arrayRank . inputType

-- | Apply the transformations to every row of the input.
transformRows :: ArrayTransforms -> Input -> Input
transformRows (ArrayTransforms ts) =
  flip (Foldable.foldl transformRows') ts
  where transformRows' inp (Rearrange cs perm) =
          addTransform (Rearrange cs (0:map (+1) perm)) inp
        transformRows' inp (Reshape cs shape) =
          addTransform (ReshapeInner cs shape) inp
        transformRows' inp (Replicate n)
          | inputRank inp == 1 =
            Rearrange [] [1,0] `addTransform`
            (Replicate n `addTransform` inp)
          | otherwise =
            Rearrange [] (1:0:[2..inputRank inp]) `addTransform`
            (Replicate n `addTransform`
             (Rearrange [] (1:0:[2..inputRank inp-1]) `addTransform` inp))
        transformRows' inp nts =
          error $ "transformRows: Cannot transform this yet:\n" ++ show nts ++ "\n" ++ show inp

-- | Get the resulting type after transforming the rows.
transformTypeRows :: ArrayTransforms -> Type -> Type
transformTypeRows (ArrayTransforms ts) = flip (Foldable.foldl transform) ts
  where transform t (Rearrange _ perm) =
          t `setArrayShape` Shape (permuteShape (0:map (+1) perm) $ arrayDims t)
        transform t (Reshape _ shape) =
          t `setArrayShape` Shape shape
        transform t (ReshapeOuter _ shape) =
          let outer:oldshape = arrayDims t
          in t `setArrayShape` Shape (outer : shape ++ drop 1 oldshape)
        transform t (ReshapeInner _ shape) =
          let outer:inner:_ = arrayDims t
          in t `setArrayShape` Shape (outer : inner : shape)
        transform t (Replicate n) =
          let outer:shape = arrayDims t
          in t `setArrayShape` Shape (outer : n : shape)

-- | Add to the input a 'Rearrange' transform that performs an @(k,n)@
-- transposition.  The new transform will be at the end of the current
-- transformation list.
transposeInput :: Int -> Int -> Input -> Input
transposeInput k n inp =
  addTransform (Rearrange [] $ transposeIndex k n [0..inputRank inp-1]) inp

-- | A definite representation of a SOAC expression.
data SOAC lore = Map Certificates (Lambda lore) [Input]
               | Reduce  Certificates (Lambda lore) [(SubExp,Input)]
               | Scan Certificates (Lambda lore) [(SubExp,Input)]
               | Redomap Certificates (Lambda lore) (Lambda lore) [SubExp] [Input]
            deriving (Show)

-- | Returns the inputs used in a SOAC.
inputs :: SOAC lore -> [Input]
inputs (Map _     _     arrs) = arrs
inputs (Reduce  _ _     args) = map snd args
inputs (Scan    _ _     args) = map snd args
inputs (Redomap _ _ _ _ arrs) = arrs

-- | Set the inputs to a SOAC.
setInputs :: [Input] -> SOAC lore -> SOAC lore
setInputs arrs (Map cs lam _) =
  Map cs lam arrs
setInputs arrs (Reduce cs lam args) =
  Reduce cs lam (zip (map fst args) arrs)
setInputs arrs (Scan cs lam args) =
  Scan cs lam (zip (map fst args) arrs)
setInputs arrs (Redomap cs lam1 lam ne _) =
  Redomap cs lam1 lam ne arrs

-- | The lambda used in a given SOAC.
lambda :: SOAC lore -> Lambda lore
lambda (Map     _ lam _       ) = lam
lambda (Reduce  _ lam _       ) = lam
lambda (Scan    _ lam _       ) = lam
lambda (Redomap _ _   lam2 _ _) = lam2

-- | Set the lambda used in the SOAC.
setLambda :: Lambda lore -> SOAC lore -> SOAC lore
setLambda lam (Map cs _ arrs) =
  Map cs lam    arrs
setLambda lam (Reduce cs _ args) =
  Reduce cs lam args
setLambda lam (Scan cs _ args) =
  Scan cs lam args
setLambda lam (Redomap cs lam1 _ ne arrs) =
  Redomap cs lam1 lam ne arrs

-- | Returns the certificates used in a SOAC.
certificates :: SOAC lore -> Certificates
certificates (Map     cs _     _) = cs
certificates (Reduce  cs _ _    ) = cs
certificates (Scan    cs _ _    ) = cs
certificates (Redomap cs _ _ _ _) = cs

typeOf :: SOAC lore -> [Type]
typeOf (Map _ lam inps) =
  mapType lam $ map inputType inps
typeOf (Reduce _ lam _) =
  lambdaReturnType lam
typeOf (Scan _ _ input) =
  map (inputType . snd) input
typeOf (Redomap _ _ lam _ _) =
  lambdaReturnType lam

-- | Convert a SOAC to the corresponding expression.
toExp :: (MonadBinder m) =>
         SOAC (Lore m) -> m (Exp (Lore m))
toExp (Map cs l as) =
  LoopOp <$> (Futhark.Map cs l <$> inputsToSubExps as)
toExp (Reduce cs l args) =
  LoopOp <$> (Futhark.Reduce cs l <$> (zip es <$> inputsToSubExps as))
  where (es, as) = unzip args
toExp (Scan cs l args) =
  LoopOp <$> (Futhark.Scan cs l <$> (zip es <$> inputsToSubExps as))
  where (es, as) = unzip args
toExp (Redomap cs l1 l2 es as) =
  LoopOp <$> (Futhark.Redomap cs l1 l2 es <$> inputsToSubExps as)

-- | The reason why some expression cannot be converted to a 'SOAC'
-- value.
data NotSOAC = NotSOAC -- ^ The expression is not a (tuple-)SOAC at all.
             | InvalidArrayInput SubExp -- ^ One of the input arrays has an
                                        -- invalid form, i.e. cannot be
                                        -- converted to an 'Input' value.
               deriving (Show)

-- | Either convert an expression to the normalised SOAC
-- representation, or a reason why the expression does not have the
-- valid form.
fromExp :: HasTypeEnv f =>
           Exp lore -> f (Either NotSOAC (SOAC lore))
fromExp (LoopOp (Futhark.Map cs l as)) =
  Right <$> Map cs l <$> traverse varInput as
fromExp (LoopOp (Futhark.Reduce cs l args)) = do
  let (es,as) = unzip args
  Right <$> Reduce cs l <$> zip es <$> traverse varInput as
fromExp (LoopOp (Futhark.Scan cs l args)) = do
  let (es,as) = unzip args
  Right <$> Scan cs l <$> zip es <$> traverse varInput as
fromExp (LoopOp (Futhark.Redomap cs l1 l2 es as)) =
  Right <$> Redomap cs l1 l2 es <$> traverse varInput as
fromExp _ = pure $ Left NotSOAC
