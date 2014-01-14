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

import qualified L0C.L0 as L0
import L0C.L0 hiding (MapT, ReduceT, ScanT, FilterT, RedomapT,
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
        idx (ConstIndex i)    = Literal (IntVal i) $ srclocOf idd

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
                idx (L0.Literal (IntVal i) _) = Just $ ConstIndex i
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
  map (inputToExp' $ dimSizes is ++ repeat Nothing) is
  where inputToExp' sizes (Input ts ia) =
          transform sizes 0 (inputArrayToExp ia) $ reverse ts

        transform _ _ e [] = e

        transform sizes d e (Repeat:ts) =
          L0.Replicate sze' (transform sizes (d+1) e ts) loc
          where sze' = fromMaybe (Literal (IntVal 0) loc) $
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
  where inspect (Input ts ia) = foldl inspect' (iaDims ia) ts

        iaDims (Var v) =
          [ Just $ L0.Size [] i (L0.Var v) loc
            | i <- [0..arrayRank t] ]
          where loc = srclocOf v
                t   = identType v

        iaDims (Iota e) = [Just e]

        iaDims (Index cs v _ idxs) =
          [ Just $ L0.Size cs i (L0.Var v) loc
            | i <- [0..arrayRank t-length idxs] ]
          where loc = srclocOf v
                t   = identType v

        inspect' ds (Transpose _ k n) =
          transposeIndex k n ds

        inspect' _ (Reshape _ shape) =
          map Just shape

        inspect' ds (ReshapeOuter _ shape) =
          map Just shape ++ drop 1 ds

        inspect' ds (ReshapeInner _ shape) =
          take 1 ds ++ map Just shape

        inspect' ds (Repeat) =
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
  transformRows nts $ addTransforms [Transpose [] 0 1, Repeat, Transpose [] 0 2] inp
transformRows nts inp =
  error $ "transformRows: Cannot transform this yet:\n" ++ show nts ++ "\n" ++ show inp

-- | A definite representation of a SOAC expression.
data SOAC = MapT Certificates TupleLambda [Input] SrcLoc
          | ReduceT  Certificates TupleLambda [(Exp,Input)] SrcLoc
          | ScanT Certificates TupleLambda [(Exp,Input)] SrcLoc
          | FilterT Certificates TupleLambda [Input] SrcLoc
          | RedomapT Certificates TupleLambda TupleLambda [Exp] [Input] SrcLoc
            deriving (Show)

instance Located SOAC where
  locOf (MapT _ _ _ loc) = locOf loc
  locOf (ReduceT _ _ _ loc) = locOf loc
  locOf (ScanT _ _ _ loc) = locOf loc
  locOf (FilterT _ _ _ loc) = locOf loc
  locOf (RedomapT _ _ _ _ _ loc) = locOf loc

-- | Returns the inputs used in a SOAC.
inputs :: SOAC -> [Input]
inputs (MapT _     _     arrs _) = arrs
inputs (ReduceT  _ _     args _) = map snd args
inputs (ScanT    _ _     args _) = map snd args
inputs (FilterT  _ _     arrs _) = arrs
inputs (RedomapT _ _ _ _ arrs _) = arrs

-- | Set the inputs to a SOAC.
setInputs :: [Input] -> SOAC -> SOAC
setInputs arrs (MapT cs lam _ loc) =
  MapT cs lam arrs loc
setInputs arrs (ReduceT cs lam args loc) =
  ReduceT cs lam (zip (map fst args) arrs) loc
setInputs arrs (ScanT cs lam args loc) =
  ScanT cs lam (zip (map fst args) arrs) loc
setInputs arrs (FilterT cs lam _ loc) =
  FilterT cs lam arrs loc
setInputs arrs (RedomapT cs lam1 lam ne _ loc) =
  RedomapT cs lam1 lam ne arrs loc

-- | The lambda used in a given SOAC.
lambda :: SOAC -> TupleLambda
lambda (MapT     _ lam _        _) = lam
lambda (ReduceT  _ lam _        _) = lam
lambda (ScanT    _ lam _        _) = lam
lambda (FilterT  _ lam _        _) = lam
lambda (RedomapT _ _   lam2 _ _ _) = lam2

-- | Set the lambda used in the SOAC.
setLambda :: TupleLambda -> SOAC -> SOAC
setLambda lam (MapT cs _ arrs loc) =
  MapT cs lam    arrs loc
setLambda lam (ReduceT cs _ args loc) =
  ReduceT cs lam args loc
setLambda lam (ScanT cs _ args loc) =
  ScanT cs lam args loc
setLambda lam (FilterT cs _ arrs loc) =
  FilterT cs lam arrs loc
setLambda lam (RedomapT cs lam1 _ ne arrs loc) =
  RedomapT cs lam1 lam ne arrs loc

-- | Returns the certificates used in a SOAC.
certificates :: SOAC -> Certificates
certificates (MapT     cs _     _ _) = cs
certificates (ReduceT  cs _ _     _) = cs
certificates (ScanT    cs _ _     _) = cs
certificates (FilterT  cs _     _ _) = cs
certificates (RedomapT cs _ _ _ _ _) = cs

-- | Convert a SOAC to the corresponding expression.
toExp :: SOAC -> Exp
toExp (MapT cs l as loc) =
  L0.MapT cs l (inputsToExps as) loc
toExp (ReduceT cs l args loc) =
  L0.ReduceT cs l (zip es $ inputsToExps as) loc
  where (es, as) = unzip args
toExp (ScanT cs l args loc) =
  L0.ScanT cs l (zip es $ inputsToExps as) loc
  where (es, as) = unzip args
toExp (FilterT cs l as loc) =
  L0.FilterT cs l (inputsToExps as) loc
toExp (RedomapT cs l1 l2 es as loc) =
  L0.RedomapT cs l1 l2 es (inputsToExps as) loc

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
fromExp (L0.MapT cs l as loc) = do
  as' <- mapM inputFromExp' as
  Right $ MapT cs l as' loc
fromExp (L0.ReduceT cs l args loc) = do
  let (es,as) = unzip args
  as' <- mapM inputFromExp' as
  Right $ ReduceT cs l (zip es as') loc
fromExp (L0.ScanT cs l args loc) = do
  let (es,as) = unzip args
  as' <- mapM inputFromExp' as
  Right $ ScanT cs l (zip es as') loc
fromExp (L0.FilterT cs l as loc) = do
  as' <- mapM inputFromExp' as
  Right $ FilterT cs l as' loc
fromExp (L0.RedomapT cs l1 l2 es as loc) = do
  as' <- mapM inputFromExp' as
  Right $ RedomapT cs l1 l2 es as' loc
fromExp (L0.LetPat (TupId pats _) e (L0.TupLit tupes _) _)
  | Right soac <- fromExp e,
    Just tupvs <- vars tupes,
    map Id tupvs == pats =
      Right soac
fromExp _ = Left NotSOAC

vars :: [Exp] -> Maybe [Ident]
vars = mapM varExp
  where varExp (L0.Var k) = Just k
        varExp _          = Nothing
