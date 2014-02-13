-- | High-level representation of SOACs.  When performing
-- SOAC-transformations, operating on normal 'Exp' values is somewhat
-- of a nuisance, as they can represent terms that are not proper
-- SOACs.  In contrast, this module exposes a SOAC representation that
-- does not enable invalid representations (except for type errors).
--
-- The names exported by this module conflict with the standard L0
-- syntax tree constructors, so you are advised to use a qualified
-- import:
--
-- @
-- import L0C.HORepresentation.SOAC (SOAC)
-- import qualified L0C.HORepresentation.SOAC as SOAC
-- @
module L0C.HORepresentation.SOAC
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
  , InputTransform(..)
  , Index (..)
  , InputArray (..)
  , inputArray
  , inputType
  , inputTransposes
  , transformRows
  -- ** Converting to and from expressions
  , inputFromExp
  , inputsToExps
  )
  where

import Control.Arrow (first)
import Control.Monad

import Data.List
import Data.Loc
import Data.Maybe
import qualified Data.HashMap.Lazy as HM

import qualified L0C.InternalRep as L0
import L0C.Substitute
import L0C.InternalRep hiding (Map, Reduce, Scan, Filter, Redomap,
                               Var, Iota, Transpose, Reshape, Index)

data InputTransform = Transpose Certificates Int Int
                    -- ^ A transposition of an otherwise valid input (possibly
                    -- another transposition!).
                    | Reshape Certificates [Exp]
                    -- ^ A reshaping of an otherwise valid input.
                    | ReshapeOuter Certificates [Exp]
                    -- ^ A reshaping of the outer dimension.
                    | ReshapeInner Certificates [Exp]
                    -- ^ A reshaping of everything but the outer dimension.
                    | Repeat
                    -- ^ Replicate the input a whole number of times in order
                    -- to match the outer size of the other inputs.  If this is
                    -- the only input, it will be the empty array.
                      deriving (Show, Eq, Ord)

-- | An index used in an 'Input'.
data Index = VarIndex Ident
           | ConstIndex Int
             deriving (Show, Eq, Ord)

-- | The basic source of data for an array input - either an array
-- variable (possibly indexed) or an @iota@.
data InputArray = Var Ident
                -- ^ Some array-typed variable in scope.
                | Iota Exp
                -- ^ @iota(e)@.
                | Index Certificates Ident (Maybe Certificates) [Index]
                  -- ^ @a[i]@.
                  deriving (Show, Eq, Ord)

inputArrayToExp :: InputArray -> Exp
inputArrayToExp (Var k)  = L0.Var k
inputArrayToExp (Iota e) = L0.Iota e $ srclocOf e
inputArrayToExp (Index cs idd idxcs idxs) =
  L0.Index cs idd idxcs (map idx idxs) $ srclocOf idd
  where idx (VarIndex indidd) = L0.Var indidd
        idx (ConstIndex i)    = Constant (IntVal i) $ srclocOf idd

-- | One array input to a SOAC - a SOAC may have multiple inputs, but
-- all are of this form.  Only the array inputs are expressed with
-- this type; other arguments, such as initial accumulator values, are
-- plain expressions.  The transforms are done left-to-right, that is,
-- the first element of the 'InputTransform' list is applied first.
data Input = Input [InputTransform] InputArray
             deriving (Show, Eq, Ord)

instance Located Input where
  locOf (Input _ (Var v))         = locOf v
  locOf (Input _ (Iota e))        = locOf e
  locOf (Input _ (Index _ k _ _)) = locOf k

instance Substitute Input where
  substituteNames m (Input ts (Var v)) =
    case HM.lookup (identName v) m of
      Just name -> Input ts $ Var v { identName = name }
      Nothing   -> Input ts $ Var v
  substituteNames _ (Input ts ia) =
    Input ts ia

-- | Create a plain array variable input with no transformations.
varInput :: Ident -> Input
varInput = Input [] . Var

-- | If the given input is a plain variable input, with no transforms,
-- return the variable.
isVarInput :: Input -> Maybe Ident
isVarInput (Input [] (Var v)) = Just v
isVarInput _                  = Nothing

-- | Add a transformation to the end of the transformation list.
addTransform :: InputTransform -> Input -> Input
addTransform t (Input ts ia) = Input (ts++[t]) ia

-- | Add several transformations to the end of the transformation
-- list.
addTransforms :: [InputTransform] -> Input -> Input
addTransforms ts1 (Input ts2 ia) = Input (ts2++ts1) ia

-- | If the given expression represents a normalised SOAC input,
-- return that input.
inputFromExp :: Exp -> Maybe Input
inputFromExp ie = do (ts, ia) <- examineExp ie
                     return $ Input (reverse ts) ia
  where examineExp (L0.Var k)    = Just ([], Var k)

        examineExp (L0.Iota ne _) = Just ([], Iota ne)

        examineExp (L0.Index cs idd idxcs idxs _) = do
          idxs' <- mapM idx idxs
          Just ([], Index cs idd idxcs idxs')
          where idx (L0.Var indidd)           = Just $ VarIndex indidd
                idx (L0.Constant (IntVal i) _) = Just $ ConstIndex i
                idx _                         = Nothing

        examineExp (L0.Transpose cs k n inp _) = do
          (ts, inp') <- examineExp inp
          Just (Transpose cs k n : ts, inp')

        examineExp (L0.Reshape cs shape inp _) = do
          (ts, inp') <- examineExp inp
          Just (Reshape cs shape : ts, inp')

        examineExp _ = Nothing

-- | Convert SOAC inputs to the corresponding expressions.
inputsToExps :: [Input] -> [Exp]
inputsToExps is =
  map (inputToExp' $ dimSizes is) is
  where inputToExp' sizes (Input ts ia) =
          transform sizes 0 (inputArrayToExp ia) $ reverse ts

        transform _ _ e [] = e

        transform sizes d e (Repeat:ts) =
          L0.Replicate sze' (transform sizes (d+1) e ts) loc
          where sze' = fromMaybe (Constant (IntVal 0) loc) $
                       join $ listToMaybe $ drop d sizes
                loc  = srclocOf e

        transform sizes d e (Transpose cs k n:ts) =
          let e' = transform (transposeIndex k n sizes) d e ts
          in L0.Transpose cs k n e' $ srclocOf e

        transform sizes d e (Reshape cs shape:ts) =
          L0.Reshape cs shape (transform sizes d e ts) $ srclocOf e

        transform sizes d e (ReshapeOuter cs shape:ts) =
          let e' = transform sizes d e ts
          in L0.Reshape cs (reshapeOuter shape 1 e') e $ srclocOf e

        transform sizes d e (ReshapeInner cs shape:ts) =
          let e' = transform sizes d e ts
          in L0.Reshape cs (reshapeInner shape 1 e') e $ srclocOf e

dimSizes :: [Input] -> [Maybe Exp]
dimSizes is =
  map (listToMaybe . catMaybes) $ transpose $ map inspect is
  where inspect (Input ts ia) = foldr inspect' (iaDims ia) ts

        iaDims (Var v) =
          [ Just $ L0.Size [] i (L0.Var v) loc
            | i <- [0..arrayRank t-1] ]
          where loc = srclocOf v
                t   = identType v

        iaDims (Iota e) = [Just e]

        iaDims (Index cs v _ idxs) =
          [ Just $ L0.Size cs i (L0.Var v) loc
            | i <- [0..arrayRank t-length idxs] ]
          where loc = srclocOf v
                t   = identType v

        inspect' (Transpose _ k n) ds =
          transposeIndex k n ds

        inspect' (Reshape _ shape) _ =
          map Just shape

        inspect' (ReshapeOuter _ shape) ds =
          map Just shape ++ drop 1 ds

        inspect' (ReshapeInner _ shape) ds =
          take 1 ds ++ map Just shape

        inspect' (Repeat) ds =
          Nothing : ds

-- | If the input is a (possibly transposed, reshaped or otherwise
-- transformed) array variable, return that variable.
inputArray :: Input -> Maybe Ident
inputArray (Input _ (Var v))         = Just v
inputArray (Input _ (Index _ v _ _)) = Just v
inputArray (Input _ (Iota _))        = Nothing

-- | Return the type of an input.
inputType :: Input -> Type
inputType (Input ts ia) = foldl transformType (typeOf $ inputArrayToExp ia) ts
  where transformType t Repeat = arrayOf t [Nothing] u
          where u | uniqueOrBasic t = Unique
                  | otherwise       = Nonunique
        transformType t (Transpose _ k n) =
          setArrayDims (transposeIndex k n $ arrayDims t) t
        transformType t (Reshape _ shape) =
          setArrayDims (replicate (length shape) Nothing) t
        transformType t (ReshapeOuter _ shape) =
          setArrayDims (replicate (length shape) Nothing ++ drop 1 (arrayDims t)) t
        transformType t (ReshapeInner _ shape) =
          setArrayDims (take 1 (arrayDims t) ++ replicate (length shape) Nothing) t

-- | Strip surrounding transpositions from the input, returning the
-- inner input and a list of @(k,n)@-transposition pairs.
inputTransposes :: Input -> (Input, [(Int,Int)])
inputTransposes (Input ts ia) =
  (Input ts' ia, kns)
  where (kns, ts') = takeTransposes ts
        takeTransposes (Transpose _ k n : rest) =
          first ((k,n):) $ takeTransposes rest
        takeTransposes rest = ([],rest)

transformRows :: [InputTransform] -> Input -> Input
transformRows [] (Input ots ia) =
  Input ots ia
transformRows (Transpose cs k n:nts) inp =
  transformRows nts $ addTransform (Transpose cs (k+1) n) inp
transformRows (Reshape cs shape:nts) inp =
  transformRows nts $ addTransform (ReshapeInner cs shape) inp
transformRows (Repeat:nts) inp =
  transformRows nts $ addTransforms [Repeat, Transpose [] 0 1] inp
transformRows nts inp =
  error $ "transformRows: Cannot transform this yet:\n" ++ show nts ++ "\n" ++ show inp

-- | A definite representation of a SOAC expression.
data SOAC = Map Certificates Lambda [Input] SrcLoc
          | Reduce  Certificates Lambda [(Exp,Input)] SrcLoc
          | Scan Certificates Lambda [(Exp,Input)] SrcLoc
          | Filter Certificates Lambda [Input] SrcLoc
          | Redomap Certificates Lambda Lambda [Exp] [Input] SrcLoc
            deriving (Show)

instance Located SOAC where
  locOf (Map _ _ _ loc) = locOf loc
  locOf (Reduce _ _ _ loc) = locOf loc
  locOf (Scan _ _ _ loc) = locOf loc
  locOf (Filter _ _ _ loc) = locOf loc
  locOf (Redomap _ _ _ _ _ loc) = locOf loc

-- | Returns the inputs used in a SOAC.
inputs :: SOAC -> [Input]
inputs (Map _     _     arrs _) = arrs
inputs (Reduce  _ _     args _) = map snd args
inputs (Scan    _ _     args _) = map snd args
inputs (Filter  _ _     arrs _) = arrs
inputs (Redomap _ _ _ _ arrs _) = arrs

-- | Set the inputs to a SOAC.
setInputs :: [Input] -> SOAC -> SOAC
setInputs arrs (Map cs lam _ loc) =
  Map cs lam arrs loc
setInputs arrs (Reduce cs lam args loc) =
  Reduce cs lam (zip (map fst args) arrs) loc
setInputs arrs (Scan cs lam args loc) =
  Scan cs lam (zip (map fst args) arrs) loc
setInputs arrs (Filter cs lam _ loc) =
  Filter cs lam arrs loc
setInputs arrs (Redomap cs lam1 lam ne _ loc) =
  Redomap cs lam1 lam ne arrs loc

-- | The lambda used in a given SOAC.
lambda :: SOAC -> Lambda
lambda (Map     _ lam _        _) = lam
lambda (Reduce  _ lam _        _) = lam
lambda (Scan    _ lam _        _) = lam
lambda (Filter  _ lam _        _) = lam
lambda (Redomap _ _   lam2 _ _ _) = lam2

-- | Set the lambda used in the SOAC.
setLambda :: Lambda -> SOAC -> SOAC
setLambda lam (Map cs _ arrs loc) =
  Map cs lam    arrs loc
setLambda lam (Reduce cs _ args loc) =
  Reduce cs lam args loc
setLambda lam (Scan cs _ args loc) =
  Scan cs lam args loc
setLambda lam (Filter cs _ arrs loc) =
  Filter cs lam arrs loc
setLambda lam (Redomap cs lam1 _ ne arrs loc) =
  Redomap cs lam1 lam ne arrs loc

-- | Returns the certificates used in a SOAC.
certificates :: SOAC -> Certificates
certificates (Map     cs _     _ _) = cs
certificates (Reduce  cs _ _     _) = cs
certificates (Scan    cs _ _     _) = cs
certificates (Filter  cs _     _ _) = cs
certificates (Redomap cs _ _ _ _ _) = cs

-- | Convert a SOAC to the corresponding expression.
toExp :: SOAC -> Exp
toExp (Map cs l as loc) =
  L0.Map cs l (inputsToExps as) loc
toExp (Reduce cs l args loc) =
  L0.Reduce cs l (zip es $ inputsToExps as) loc
  where (es, as) = unzip args
toExp (Scan cs l args loc) =
  L0.Scan cs l (zip es $ inputsToExps as) loc
  where (es, as) = unzip args
toExp (Filter cs l as loc) =
  L0.Filter cs l (inputsToExps as) loc
toExp (Redomap cs l1 l2 es as loc) =
  L0.Redomap cs l1 l2 es (inputsToExps as) loc

-- | The reason why some expression cannot be converted to a 'SOAC'
-- value.
data NotSOAC = NotSOAC -- ^ The expression is not a (tuple-)SOAC at all.
             | InvalidArrayInput Exp -- ^ One of the input arrays has an
                                     -- invalid form, i.e. cannot be
                                     -- converted to an 'Input' value.
               deriving (Show)

inputFromExp' :: Exp -> Either NotSOAC Input
inputFromExp' e = maybe (Left $ InvalidArrayInput e) Right $ inputFromExp e

-- | Either convert an expression to the normalised SOAC
-- representation, or a reason why the expression does not have the
-- valid form.
fromExp :: Exp -> Either NotSOAC SOAC
fromExp (L0.Map cs l as loc) = do
  as' <- mapM inputFromExp' as
  Right $ Map cs l as' loc
fromExp (L0.Reduce cs l args loc) = do
  let (es,as) = unzip args
  as' <- mapM inputFromExp' as
  Right $ Reduce cs l (zip es as') loc
fromExp (L0.Scan cs l args loc) = do
  let (es,as) = unzip args
  as' <- mapM inputFromExp' as
  Right $ Scan cs l (zip es as') loc
fromExp (L0.Filter cs l as loc) = do
  as' <- mapM inputFromExp' as
  Right $ Filter cs l as' loc
fromExp (L0.Redomap cs l1 l2 es as loc) = do
  as' <- mapM inputFromExp' as
  Right $ Redomap cs l1 l2 es as' loc
fromExp (L0.LetPat pats e (L0.TupLit tupes _) _)
  | Right soac <- fromExp e,
    Just tupvs <- vars tupes,
    tupvs == pats =
      Right soac
fromExp _ = Left NotSOAC

vars :: [Exp] -> Maybe [Ident]
vars = mapM varExp
  where varExp (L0.Var k) = Just k
        varExp _          = Nothing
