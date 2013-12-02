module L0C.HOTrans.SOAC ( SOAC(..)
                        , inputs
                        , lambda
                        , setLambda
                        , certificates
                        , fromExp
                        , toExp
                        )
  where

import Data.Loc

import qualified L0C.L0 as L0
import L0C.L0 hiding (Map2, Reduce2, Scan2, Filter2, Redomap2)

data SOAC = Map2 Certificates TupleLambda [Exp] [Type] SrcLoc
          | Reduce2  Certificates TupleLambda [Exp] [Exp] [Type] SrcLoc
          | Scan2 Certificates TupleLambda [Exp] [Exp] [Type] SrcLoc
          | Filter2 Certificates TupleLambda [Exp] SrcLoc
          | Redomap2 Certificates TupleLambda TupleLambda [Exp] [Exp] [Type] SrcLoc

instance Located SOAC where
  locOf (Map2 _ _ _ _ loc) = locOf loc
  locOf (Reduce2 _ _ _ _ _ loc) = locOf loc
  locOf (Scan2 _ _ _ _ _ loc) = locOf loc
  locOf (Filter2 _ _ _ loc) = locOf loc
  locOf (Redomap2 _ _ _ _ _ _ loc) = locOf loc

-- | Returns the input arrays used in a SOAC.
inputs :: SOAC -> [Exp]
inputs (Map2 _     _     arrs _ _) = arrs
inputs (Reduce2  _ _ _   arrs _ _) = arrs
inputs (Scan2    _ _ _   arrs _ _) = arrs
inputs (Filter2  _ _     arrs _  ) = arrs
inputs (Redomap2 _ _ _ _ arrs _ _) = arrs

lambda :: SOAC -> TupleLambda
lambda (Map2     _ lam _    _ _    ) = lam
lambda (Reduce2  _ lam _    _ _ _  ) = lam
lambda (Scan2    _ lam _    _ _ _  ) = lam
lambda (Filter2  _ lam _    _      ) = lam
lambda (Redomap2 _ _   lam2 _ _ _ _) = lam2

setLambda :: TupleLambda -> SOAC -> SOAC
setLambda lam (Map2     cs         _    arrs eltp loc) =
  Map2     cs          lam    arrs eltp loc
setLambda lam (Reduce2  cs         _ ne arrs eltp loc) =
  Reduce2  cs      lam ne arrs eltp loc
setLambda lam (Scan2    cs         _ ne arrs eltp loc) =
  Scan2  cs      lam ne arrs eltp loc
setLambda lam (Filter2  cs         _    arrs      loc) =
  Filter2  cs      lam    arrs      loc
setLambda lam (Redomap2 cs lam1    _ ne arrs eltp loc) =
  Redomap2 cs lam1 lam ne arrs eltp loc

-- | Returns the certificates used in a SOAC.
certificates :: SOAC -> Certificates
certificates (Map2     cs _     _ _ _) = cs
certificates (Reduce2  cs _ _   _ _ _) = cs
certificates (Scan2    cs _ _   _ _ _) = cs
certificates (Filter2  cs _     _ _  ) = cs
certificates (Redomap2 cs _ _ _ _ _ _) = cs

toExp :: SOAC -> Exp
toExp (Map2 cs l as t loc) = L0.Map2 cs l as t loc
toExp (Reduce2 cs l es as ts loc) = L0.Reduce2 cs l es as ts loc
toExp (Scan2 cs l es as ts loc) = L0.Scan2 cs l es as ts loc
toExp (Filter2 cs l es loc) = L0.Filter2 cs l es loc
toExp (Redomap2 cs l1 l2 es as ts loc) = L0.Redomap2 cs l1 l2 es as ts loc

fromExp :: Exp -> Maybe SOAC
fromExp (L0.Map2 cs l as t loc) = Just $ Map2 cs l as t loc
fromExp (L0.Reduce2 cs l es as ts loc) = Just $ Scan2 cs l es as ts loc
fromExp (L0.Scan2 cs l es as ts loc) = Just $ Scan2 cs l es as ts loc
fromExp (L0.Filter2 cs l es loc) = Just $ Filter2 cs l es loc
fromExp (L0.Redomap2 cs l1 l2 es as ts loc) = Just $ Redomap2 cs l1 l2 es as ts loc
fromExp _ = Nothing
