module L0C.HOTrans.SOAC ( SOAC (..)
                        , Input (..)
                        , Index (..)
                        , inputFromExp
                        , inputToExp
                        , inputs
                        , setInputs
                        , lambda
                        , setLambda
                        , certificates
                        , NotSOAC (..)
                        , fromExp
                        , toExp
                        )
  where

import Data.Loc

import qualified L0C.L0 as L0
import L0C.L0 hiding (Map2, Reduce2, Scan2, Filter2, Redomap2,
                      Var, Iota, Transpose, Index)

data Input = Var Ident
           | Iota Exp
           | Transpose Certificates Int Int Input
           | Index Certificates Ident (Maybe Certificates) [Index]
             deriving (Show, Eq, Ord)

data Index = VarIndex Ident
           | ConstIndex Int
             deriving (Show, Eq, Ord)

instance Located Input where
  locOf (Var idd)             = locOf idd
  locOf (Iota e)              = locOf e
  locOf (Transpose _ _ _ inp) = locOf inp
  locOf (Index _ k _ _)       = locOf k

inputFromExp :: Exp -> Maybe Input
inputFromExp (L0.Var k)    = Just $ Var k
inputFromExp (L0.Iota e _) = Just $ Iota e
inputFromExp (L0.Transpose cs k n inp _) = do
  inp' <- inputFromExp inp
  Just $ Transpose cs k n inp'
inputFromExp (L0.Index cs idd idxcs idxs _ _) = do
  idxs' <- mapM idx idxs
  Just $ Index cs idd idxcs idxs'
  where idx (L0.Var indidd)           = Just $ VarIndex indidd
        idx (L0.Literal (IntVal i) _) = Just $ ConstIndex i
        idx _                         = Nothing
inputFromExp _ = Nothing

inputToExp :: Input -> Exp
inputToExp (Var k)  = L0.Var k
inputToExp (Iota e) = L0.Iota e $ srclocOf e
inputToExp (Transpose cs k n inp) =
  L0.Transpose cs k n (inputToExp inp) $ srclocOf inp
inputToExp (Index cs idd idxcs idxs) =
  L0.Index cs idd idxcs (map idx idxs) t $ srclocOf idd
  where t = stripArray (length idxs) $ identType idd
        idx (VarIndex indidd) = L0.Var indidd
        idx (ConstIndex i)    = Literal (IntVal i) $ srclocOf idd

data SOAC = Map2 Certificates TupleLambda [Input] SrcLoc
          | Reduce2  Certificates TupleLambda [Exp] [Input] SrcLoc
          | Scan2 Certificates TupleLambda [Exp] [Input] SrcLoc
          | Filter2 Certificates TupleLambda [Input] SrcLoc
          | Redomap2 Certificates TupleLambda TupleLambda [Exp] [Input] SrcLoc
            deriving (Show)

instance Located SOAC where
  locOf (Map2 _ _ _ loc) = locOf loc
  locOf (Reduce2 _ _ _ _ loc) = locOf loc
  locOf (Scan2 _ _ _ _ loc) = locOf loc
  locOf (Filter2 _ _ _ loc) = locOf loc
  locOf (Redomap2 _ _ _ _ _ loc) = locOf loc

-- | Returns the input arrays used in a SOAC.
inputs :: SOAC -> [Input]
inputs (Map2 _     _     arrs _) = arrs
inputs (Reduce2  _ _ _   arrs _) = arrs
inputs (Scan2    _ _ _   arrs _) = arrs
inputs (Filter2  _ _     arrs _) = arrs
inputs (Redomap2 _ _ _ _ arrs _) = arrs

setInputs :: [Input] -> SOAC -> SOAC
setInputs arrs (Map2 cs lam _ loc) =
  Map2 cs lam arrs loc
setInputs arrs (Reduce2 cs lam ne _ loc) =
  Reduce2 cs lam ne arrs loc
setInputs arrs (Scan2 cs lam ne _ loc) =
  Scan2 cs lam ne arrs loc
setInputs arrs (Filter2 cs lam _ loc) =
  Filter2 cs lam arrs loc
setInputs arrs (Redomap2 cs lam1 lam ne _ loc) =
  Redomap2 cs lam1 lam ne arrs loc

lambda :: SOAC -> TupleLambda
lambda (Map2     _ lam _    _    ) = lam
lambda (Reduce2  _ lam _    _ _  ) = lam
lambda (Scan2    _ lam _    _ _  ) = lam
lambda (Filter2  _ lam _    _    ) = lam
lambda (Redomap2 _ _   lam2 _ _ _) = lam2

setLambda :: TupleLambda -> SOAC -> SOAC
setLambda lam (Map2     cs         _    arrs loc) =
  Map2     cs          lam    arrs loc
setLambda lam (Reduce2  cs         _ ne arrs loc) =
  Reduce2  cs      lam ne arrs loc
setLambda lam (Scan2    cs         _ ne arrs loc) =
  Scan2  cs      lam ne arrs loc
setLambda lam (Filter2  cs         _    arrs loc) =
  Filter2  cs      lam    arrs      loc
setLambda lam (Redomap2 cs lam1    _ ne arrs loc) =
  Redomap2 cs lam1 lam ne arrs loc

-- | Returns the certificates used in a SOAC.
certificates :: SOAC -> Certificates
certificates (Map2     cs _     _ _) = cs
certificates (Reduce2  cs _ _   _ _) = cs
certificates (Scan2    cs _ _   _ _) = cs
certificates (Filter2  cs _     _ _) = cs
certificates (Redomap2 cs _ _ _ _ _) = cs

rowTypes :: [Exp] -> [Type]
rowTypes = map (rowType . typeOf)

toExp :: SOAC -> Exp
toExp (Map2 cs l as loc) =
  L0.Map2 cs l as' (rowTypes as') loc
  where as' = map inputToExp as
toExp (Reduce2 cs l es as loc) =
  L0.Reduce2 cs l es as' (rowTypes as') loc
  where as' = map inputToExp as
toExp (Scan2 cs l es as loc) =
  L0.Scan2 cs l es as' (rowTypes as') loc
  where as' = map inputToExp as
toExp (Filter2 cs l as loc) =
  L0.Filter2 cs l (map inputToExp as) loc
toExp (Redomap2 cs l1 l2 es as loc) =
  L0.Redomap2 cs l1 l2 es as' (rowTypes as') loc
  where as' = map inputToExp as

-- The reason why some expression cannot be converted to a 'SOAC'
-- value.
data NotSOAC = NotSOAC -- ^ The expression is not a (tuple-)SOAC at all.
             | InvalidArrayInput Exp -- ^ One of the input arrays has an
                                     -- invalid form, i.e. cannot be
                                     -- converted to an 'Input' value.
               deriving (Show)

inputFromExp' :: Exp -> Either NotSOAC Input
inputFromExp' e = maybe (Left $ InvalidArrayInput e) Right $ inputFromExp e

fromExp :: Exp -> Either NotSOAC SOAC
fromExp (L0.Map2 cs l as _ loc) = do
  as' <- mapM inputFromExp' as
  Right $ Map2 cs l as' loc
fromExp (L0.Reduce2 cs l es as _ loc) = do
  as' <- mapM inputFromExp' as
  Right $ Reduce2 cs l es as' loc
fromExp (L0.Scan2 cs l es as _ loc) = do
  as' <- mapM inputFromExp' as
  Right $ Scan2 cs l es as' loc
fromExp (L0.Filter2 cs l as loc) = do
  as' <- mapM inputFromExp' as
  Right $ Filter2 cs l as' loc
fromExp (L0.Redomap2 cs l1 l2 es as _ loc) = do
  as' <- mapM inputFromExp' as
  Right $ Redomap2 cs l1 l2 es as' loc
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
