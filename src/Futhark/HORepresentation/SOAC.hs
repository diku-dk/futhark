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
-- import Futhark.HORepresentation.SOAC (SOAC)
-- import qualified Futhark.HORepresentation.SOAC as SOAC
-- @
module Futhark.HORepresentation.SOAC
  (
   -- * SOACs
    SOAC (..)
  , inputs
  , setInputs
  , lambda
  , setLambda
  , certificates
  -- ** Converting to and from expressions
  , NotSOAC (..)
  , fromExp
  , toExp
  -- * SOAC inputs
  , Input (..)
  , varInput
  , isVarInput
  , addTransform
  , addTransforms
  , InputArray (..)
  , inputArray
  , inputRank
  , inputTypes
  , inputsWithTypes
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

import Prelude hiding (foldl, foldr, and)

import Control.Applicative

import Data.Foldable
import Data.Loc
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.Sequence as Seq

import qualified Futhark.InternalRep as Futhark
import Futhark.InternalRep hiding (Map, Reduce, Scan, Filter, Redomap,
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
  and $ zipWith (==) perm [0..]
identityTransform _ = False

combineTransforms :: ArrayTransform -> ArrayTransform -> Maybe ArrayTransform
combineTransforms (Rearrange cs2 perm2) (Rearrange cs1 perm1) =
  Just $ Rearrange (cs1++cs2) $ perm2 `permuteCompose` perm1
combineTransforms _ _ = Nothing

-- | Given an expression, determine whether the expression represents
-- an input transformation of an array variable.  If so, return the
-- variable and the transformation.  Only 'Rearrange' and 'Reshape'
-- are possible to express this way.
transformFromExp :: Exp -> Maybe (Ident, ArrayTransform)
transformFromExp (Futhark.Rearrange cs perm (Futhark.Var v) _) =
  Just (v, Rearrange cs perm)
transformFromExp (Futhark.Reshape cs shape (Futhark.Var v) _) =
  Just (v, Reshape cs shape)
transformFromExp _ = Nothing

-- | The basic source of data for an array input - either an array
-- variable (possibly indexed) or an @iota@.
data InputArray = Var Ident
                -- ^ Some array-typed variable in scope.
                | Iota SubExp
                -- ^ @iota(e)@.
                  deriving (Show, Eq, Ord)

instance Located InputArray where
  locOf (Var v)  = locOf v
  locOf (Iota e) = locOf e

inputArrayToExp :: InputArray -> Exp
inputArrayToExp (Var k)  = SubExp $ Futhark.Var k
inputArrayToExp (Iota e) = Futhark.Iota e $ srclocOf e

-- | One array input to a SOAC - a SOAC may have multiple inputs, but
-- all are of this form.  Only the array inputs are expressed with
-- this type; other arguments, such as initial accumulator values, are
-- plain expressions.  The transforms are done left-to-right, that is,
-- the first element of the 'ArrayTransform' list is applied first.
data Input = Input ArrayTransforms InputArray
             deriving (Show, Eq, Ord)

instance Located Input where
  locOf (Input _ ia) = locOf ia

instance Substitute Input where
  substituteNames m (Input ts (Var v)) =
    case HM.lookup (identName v) m of
      Just name -> Input ts $ Var v { identName = name }
      Nothing   -> Input ts $ Var v
  substituteNames _ (Input ts ia) =
    Input ts ia

-- | Create a plain array variable input with no transformations.
varInput :: Ident -> Input
varInput = Input (ArrayTransforms Seq.empty) . Var

-- | If the given input is a plain variable input, with no transforms,
-- return the variable.
isVarInput :: Input -> Maybe Ident
isVarInput (Input ts (Var v)) | nullTransforms ts = Just v
isVarInput _                                      = Nothing

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
inputFromSubExp :: SubExp -> Maybe Input
inputFromSubExp (Futhark.Var v) = Just $ Input (ArrayTransforms Seq.empty) $ Var v
inputFromSubExp _          = Nothing

-- | Convert SOAC inputs to the corresponding expressions.
inputsToSubExps :: [Input] -> Binder [SubExp]
inputsToSubExps = mapM inputToExp'
  where inputToExp' (Input (ArrayTransforms ts) ia) = do
          ia' <- letSubExp "soac_input" $ inputArrayToExp ia
          foldlM transform ia' ts

        transform ia (Replicate n) =
          letSubExp "repeat" $ Futhark.Replicate n ia loc
          where loc  = srclocOf ia

        transform ia (Rearrange cs perm) =
          letSubExp "rearrange" $ Futhark.Rearrange cs perm ia $ srclocOf ia

        transform ia (Reshape cs shape) =
          letSubExp "reshape" $ Futhark.Reshape cs shape ia $ srclocOf ia

        transform ia (ReshapeOuter cs shape) =
          let shape' = reshapeOuter shape 1 ia
          in letSubExp "reshape_outer" $ Futhark.Reshape cs shape' ia $ srclocOf ia

        transform ia (ReshapeInner cs shape) =
          let shape' = reshapeInner shape 1 ia
          in letSubExp "reshape_inner" $ Futhark.Reshape cs shape' ia $ srclocOf ia

-- | If the input is a (possibly rearranged, reshaped or otherwise
-- transformed) array variable, return that variable.
inputArray :: Input -> Maybe Ident
inputArray (Input _ (Var v))         = Just v
inputArray (Input _ (Iota _))        = Nothing

inputArrayType :: InputArray -> Type
inputArrayType (Var v)  = identType v
inputArrayType (Iota e) = arrayOf (Basic Int) (Shape [e]) Unique

-- | Return the array rank (dimensionality) of an input.
inputRank :: Input -> Int
inputRank (Input (ArrayTransforms ts) ia) =
  foldl transformType (arrayRank $ inputArrayType ia) ts
  where transformType rank (Replicate _)          = rank + 1
        transformType rank (Rearrange _ _)        = rank
        transformType _    (Reshape _ shape)      = length shape
        transformType rank (ReshapeOuter _ shape) = rank - 1 + length shape
        transformType _    (ReshapeInner _ shape) = 1 + length shape

-- | Return the types of a list of inputs.
inputTypes :: [Input] -> [Type]
inputTypes = map inputType
  where inputType (Input (ArrayTransforms ts) ia) =
          foldl transformType (inputArrayType ia) ts

        transformType t (Replicate n) =
          arrayOf t (Shape [n]) u
          where u | uniqueOrBasic t = Unique
                  | otherwise       = Nonunique
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

-- | Tag each input with its corresponding type.
inputsWithTypes :: [Input] -> [(Type, Input)]
inputsWithTypes l = zip (inputTypes l) l

-- | Apply the transformations to every row of the input.
transformRows :: ArrayTransforms -> Input -> Input
transformRows (ArrayTransforms ts) =
  flip (foldl transformRows') ts
  where transformRows' inp (Rearrange cs perm) =
          addTransform (Rearrange cs (0:map (+1) perm)) inp
        transformRows' inp (Reshape cs shape) =
          addTransform (ReshapeInner cs shape) inp
        transformRows' inp (Replicate n) =
          Replicate n `addTransform`
          (Rearrange [] (1:0:[2..inputRank inp]) `addTransform` inp)
        transformRows' inp nts =
          error $ "transformRows: Cannot transform this yet:\n" ++ show nts ++ "\n" ++ show inp

-- | Get the resulting type after transforming the rows.
transformTypeRows :: ArrayTransforms -> Type -> Type
transformTypeRows (ArrayTransforms ts) = flip (foldl transform) ts
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
data SOAC = Map Certificates Lambda [Input] SrcLoc
          | Reduce  Certificates Lambda [(SubExp,Input)] SrcLoc
          | Scan Certificates Lambda [(SubExp,Input)] SrcLoc
          | Filter Certificates Lambda [Input] SubExp SrcLoc
          | Redomap Certificates Lambda Lambda [SubExp] [Input] SrcLoc
            deriving (Show)

instance Located SOAC where
  locOf (Map _ _ _ loc) = locOf loc
  locOf (Reduce _ _ _ loc) = locOf loc
  locOf (Scan _ _ _ loc) = locOf loc
  locOf (Filter _ _ _ _ loc) = locOf loc
  locOf (Redomap _ _ _ _ _ loc) = locOf loc

-- | Returns the inputs used in a SOAC.
inputs :: SOAC -> [Input]
inputs (Map _     _     arrs   _) = arrs
inputs (Reduce  _ _     args   _) = map snd args
inputs (Scan    _ _     args   _) = map snd args
inputs (Filter  _ _     arrs _ _) = arrs
inputs (Redomap _ _ _ _ arrs   _) = arrs

-- | Set the inputs to a SOAC.
setInputs :: [Input] -> SOAC -> SOAC
setInputs arrs (Map cs lam _ loc) =
  Map cs lam arrs loc
setInputs arrs (Reduce cs lam args loc) =
  Reduce cs lam (zip (map fst args) arrs) loc
setInputs arrs (Scan cs lam args loc) =
  Scan cs lam (zip (map fst args) arrs) loc
setInputs arrs (Filter cs lam _ outer_shape loc) =
  Filter cs lam arrs outer_shape loc
setInputs arrs (Redomap cs lam1 lam ne _ loc) =
  Redomap cs lam1 lam ne arrs loc

-- | The lambda used in a given SOAC.
lambda :: SOAC -> Lambda
lambda (Map     _ lam _        _) = lam
lambda (Reduce  _ lam _        _) = lam
lambda (Scan    _ lam _        _) = lam
lambda (Filter  _ lam _      _ _) = lam
lambda (Redomap _ _   lam2 _ _ _) = lam2

-- | Set the lambda used in the SOAC.
setLambda :: Lambda -> SOAC -> SOAC
setLambda lam (Map cs _ arrs loc) =
  Map cs lam    arrs loc
setLambda lam (Reduce cs _ args loc) =
  Reduce cs lam args loc
setLambda lam (Scan cs _ args loc) =
  Scan cs lam args loc
setLambda lam (Filter cs _ arrs outer_shape loc) =
  Filter cs lam arrs outer_shape loc
setLambda lam (Redomap cs lam1 _ ne arrs loc) =
  Redomap cs lam1 lam ne arrs loc

-- | Returns the certificates used in a SOAC.
certificates :: SOAC -> Certificates
certificates (Map     cs _     _ _) = cs
certificates (Reduce  cs _ _     _) = cs
certificates (Scan    cs _ _     _) = cs
certificates (Filter  cs _   _ _ _) = cs
certificates (Redomap cs _ _ _ _ _) = cs

-- | Convert a SOAC to the corresponding expression.
toExp :: SOAC -> Binder Exp
toExp (Map cs l as loc) =
  Futhark.Map cs l <$> inputsToSubExps as <*> pure loc
toExp (Reduce cs l args loc) =
  Futhark.Reduce cs l <$> (zip es <$> inputsToSubExps as) <*> pure loc
  where (es, as) = unzip args
toExp (Scan cs l args loc) =
  Futhark.Scan cs l <$> (zip es <$> inputsToSubExps as) <*> pure loc
  where (es, as) = unzip args
toExp (Filter cs l as outer_shape loc) =
  Futhark.Filter cs l <$> inputsToSubExps as <*> pure outer_shape <*> pure loc
toExp (Redomap cs l1 l2 es as loc) =
  Futhark.Redomap cs l1 l2 es <$> inputsToSubExps as <*> pure loc

-- | The reason why some expression cannot be converted to a 'SOAC'
-- value.
data NotSOAC = NotSOAC -- ^ The expression is not a (tuple-)SOAC at all.
             | InvalidArrayInput SubExp -- ^ One of the input arrays has an
                                        -- invalid form, i.e. cannot be
                                        -- converted to an 'Input' value.
               deriving (Show)

inputFromSubExp' :: SubExp -> Either NotSOAC Input
inputFromSubExp' e = maybe (Left $ InvalidArrayInput e) Right $ inputFromSubExp e

-- | Either convert an expression to the normalised SOAC
-- representation, or a reason why the expression does not have the
-- valid form.
fromExp :: Exp -> Either NotSOAC SOAC
fromExp (Futhark.Map cs l as loc) = do
  as' <- mapM inputFromSubExp' as
  Right $ Map cs l as' loc
fromExp (Futhark.Reduce cs l args loc) = do
  let (es,as) = unzip args
  as' <- mapM inputFromSubExp' as
  Right $ Reduce cs l (zip es as') loc
fromExp (Futhark.Scan cs l args loc) = do
  let (es,as) = unzip args
  as' <- mapM inputFromSubExp' as
  Right $ Scan cs l (zip es as') loc
fromExp (Futhark.Filter cs l as outer_shape loc) = do
  as' <- mapM inputFromSubExp' as
  Right $ Filter cs l as' outer_shape loc
fromExp (Futhark.Redomap cs l1 l2 es as loc) = do
  as' <- mapM inputFromSubExp' as
  Right $ Redomap cs l1 l2 es as' loc
fromExp _ = Left NotSOAC
