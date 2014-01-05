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
  , Index (..)
  , inputArray
  , inputType
  , inputTransposes
  -- ** Converting to and from expressions
  , inputFromExp
  , inputToExp
  )
  where

import Control.Arrow (second)

import Data.Loc

import qualified L0C.L0 as L0
import L0C.L0 hiding (MapT, ReduceT, ScanT, FilterT, RedomapT,
                      Var, Iota, Transpose, Reshape, Index)

-- | One array input to a SOAC - a SOAC may have multiple inputs, but
-- all are of this form.  Only the array inputs are expressed with
-- this type; other arguments, such as initial accumulator values, are
-- plain expressions.
data Input = Var Ident
           -- ^ Some array-typed variable in scope.
           | Iota Exp
           -- ^ @iota(e)@.
           | Transpose Certificates Int Int Input
           -- ^ A transposition of an otherwise valid input (possibly
           -- another transposition!).
           | Reshape Certificates [Exp] Input
           -- ^ A reshaping of an otherwise valid input.
           | Index Certificates Ident (Maybe Certificates) [Index]
           -- ^ @a[i]@.
             deriving (Show, Eq, Ord)

-- | An index used in an 'Input'.
data Index = VarIndex Ident
           | ConstIndex Int
             deriving (Show, Eq, Ord)

instance Located Input where
  locOf (Var idd)             = locOf idd
  locOf (Iota e)              = locOf e
  locOf (Transpose _ _ _ inp) = locOf inp
  locOf (Reshape _ _ inp)     = locOf inp
  locOf (Index _ k _ _)       = locOf k

-- | If the given expression represents a normalised SOAC input,
-- return that input.
inputFromExp :: Exp -> Maybe Input
inputFromExp (L0.Var k)    = Just $ Var k
inputFromExp (L0.Iota e _) = Just $ Iota e
inputFromExp (L0.Transpose cs k n inp _) = do
  inp' <- inputFromExp inp
  Just $ Transpose cs k n inp'
inputFromExp (L0.Reshape cs shape inp _) = do
  inp' <- inputFromExp inp
  Just $ Reshape cs shape inp'
inputFromExp (L0.Index cs idd idxcs idxs _ _) = do
  idxs' <- mapM idx idxs
  Just $ Index cs idd idxcs idxs'
  where idx (L0.Var indidd)           = Just $ VarIndex indidd
        idx (L0.Literal (IntVal i) _) = Just $ ConstIndex i
        idx _                         = Nothing
inputFromExp _ = Nothing

-- | Convert a SOAC input to the corresponding expression.
inputToExp :: Input -> Exp
inputToExp (Var k)  = L0.Var k
inputToExp (Iota e) = L0.Iota e $ srclocOf e
inputToExp (Transpose cs k n inp) =
  L0.Transpose cs k n (inputToExp inp) $ srclocOf inp
inputToExp (Reshape cs shape inp) =
  L0.Reshape cs shape (inputToExp inp) $ srclocOf inp
inputToExp (Index cs idd idxcs idxs) =
  L0.Index cs idd idxcs (map idx idxs) t $ srclocOf idd
  where t = stripArray (length idxs) $ identType idd
        idx (VarIndex indidd) = L0.Var indidd
        idx (ConstIndex i)    = Literal (IntVal i) $ srclocOf idd

-- | If the input is a (possibly transposed, reshaped or otherwise
-- transformed) array variable, return that variable.
inputArray :: Input -> Maybe Ident
inputArray (Var idd)             = Just idd
inputArray (Iota _)              = Nothing
inputArray (Transpose _ _ _ inp) = inputArray inp
inputArray (Reshape _ _ inp)     = inputArray inp
inputArray (Index {})            = Nothing

-- | Return the type of an input - just another name for @'typeOf'
-- . 'inputToExp'@.
inputType :: Input -> Type
inputType = typeOf . inputToExp

-- | Strip surrounding transpositions from the input, returning the
-- inner input and a list of @(k,n)@-transposition pairs.
inputTransposes :: Input -> (Input, [(Int,Int)])
inputTransposes (Transpose _ k n inp) =
  second ((k,n):) $ inputTransposes inp
inputTransposes inp =
  (inp, [])

-- | A definite representation of a SOAC expression.
data SOAC = MapT Certificates TupleLambda [Input] SrcLoc
          | ReduceT  Certificates TupleLambda [Exp] [Input] SrcLoc
          | ScanT Certificates TupleLambda [Exp] [Input] SrcLoc
          | FilterT Certificates TupleLambda [Input] SrcLoc
          | RedomapT Certificates TupleLambda TupleLambda [Exp] [Input] SrcLoc
            deriving (Show)

instance Located SOAC where
  locOf (MapT _ _ _ loc) = locOf loc
  locOf (ReduceT _ _ _ _ loc) = locOf loc
  locOf (ScanT _ _ _ _ loc) = locOf loc
  locOf (FilterT _ _ _ loc) = locOf loc
  locOf (RedomapT _ _ _ _ _ loc) = locOf loc

-- | Returns the inputs used in a SOAC.
inputs :: SOAC -> [Input]
inputs (MapT _     _     arrs _) = arrs
inputs (ReduceT  _ _ _   arrs _) = arrs
inputs (ScanT    _ _ _   arrs _) = arrs
inputs (FilterT  _ _     arrs _) = arrs
inputs (RedomapT _ _ _ _ arrs _) = arrs

-- | Set the inputs to a SOAC.
setInputs :: [Input] -> SOAC -> SOAC
setInputs arrs (MapT cs lam _ loc) =
  MapT cs lam arrs loc
setInputs arrs (ReduceT cs lam ne _ loc) =
  ReduceT cs lam ne arrs loc
setInputs arrs (ScanT cs lam ne _ loc) =
  ScanT cs lam ne arrs loc
setInputs arrs (FilterT cs lam _ loc) =
  FilterT cs lam arrs loc
setInputs arrs (RedomapT cs lam1 lam ne _ loc) =
  RedomapT cs lam1 lam ne arrs loc

-- | The lambda used in a given SOAC.
lambda :: SOAC -> TupleLambda
lambda (MapT     _ lam _    _    ) = lam
lambda (ReduceT  _ lam _    _ _  ) = lam
lambda (ScanT    _ lam _    _ _  ) = lam
lambda (FilterT  _ lam _    _    ) = lam
lambda (RedomapT _ _   lam2 _ _ _) = lam2

-- | Set the lambda used in the SOAC.
setLambda :: TupleLambda -> SOAC -> SOAC
setLambda lam (MapT     cs         _    arrs loc) =
  MapT     cs          lam    arrs loc
setLambda lam (ReduceT  cs         _ ne arrs loc) =
  ReduceT  cs      lam ne arrs loc
setLambda lam (ScanT    cs         _ ne arrs loc) =
  ScanT  cs      lam ne arrs loc
setLambda lam (FilterT  cs         _    arrs loc) =
  FilterT  cs      lam    arrs      loc
setLambda lam (RedomapT cs lam1    _ ne arrs loc) =
  RedomapT cs lam1 lam ne arrs loc

-- | Returns the certificates used in a SOAC.
certificates :: SOAC -> Certificates
certificates (MapT     cs _     _ _) = cs
certificates (ReduceT  cs _ _   _ _) = cs
certificates (ScanT    cs _ _   _ _) = cs
certificates (FilterT  cs _     _ _) = cs
certificates (RedomapT cs _ _ _ _ _) = cs

-- | Convert a SOAC to the corresponding expression.
toExp :: SOAC -> Exp
toExp (MapT cs l as loc) =
  L0.MapT cs l as' loc
  where as' = map inputToExp as
toExp (ReduceT cs l es as loc) =
  L0.ReduceT cs l es as' loc
  where as' = map inputToExp as
toExp (ScanT cs l es as loc) =
  L0.ScanT cs l es as' loc
  where as' = map inputToExp as
toExp (FilterT cs l as loc) =
  L0.FilterT cs l (map inputToExp as) loc
toExp (RedomapT cs l1 l2 es as loc) =
  L0.RedomapT cs l1 l2 es as' loc
  where as' = map inputToExp as

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
fromExp (L0.ReduceT cs l es as loc) = do
  as' <- mapM inputFromExp' as
  Right $ ReduceT cs l es as' loc
fromExp (L0.ScanT cs l es as loc) = do
  as' <- mapM inputFromExp' as
  Right $ ScanT cs l es as' loc
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
