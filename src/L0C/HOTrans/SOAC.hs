module L0C.HOTrans.SOAC ( SOAC(..)
                        , inputs
                        , lambda
                        , setLambda
                        , certificates
                        , fromExp
                        , toExp
                        , LoopNest
                        , optimNest
                        , unNest
                        , toNest
                        , MapN
                        , mapNfromSOAC
                        , mapNtoSOAC
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
fromExp (L0.Reduce2 cs l es as ts loc) = Just $ Reduce2 cs l es as ts loc
fromExp (L0.Scan2 cs l es as ts loc) = Just $ Scan2 cs l es as ts loc
fromExp (L0.Filter2 cs l es loc) = Just $ Filter2 cs l es loc
fromExp (L0.Redomap2 cs l1 l2 es as ts loc) = Just $ Redomap2 cs l1 l2 es as ts loc
fromExp _ = Nothing

data LoopNest = SWIM Certificates Certificates (Exp, Exp) (Ident,Ident) TupleLambda SrcLoc

optimNest :: LoopNest -> Exp
optimNest (SWIM cs1 cs2 (e,a) (_,arrident) ml loc) =
  trans (L0.Map2 cs1 l [trans a] [rowType $ typeOf $ trans a] loc)
    where l = TupleLambda {
                tupleLambdaParams = [toParam arrident]
              , tupleLambdaBody = lb
              , tupleLambdaReturnType = [toDecl $ typeOf lb]
              , tupleLambdaSrcLoc = loc
              }
          lb = L0.Scan2 cs2 ml [e] [Var arrident] [rowType $ identType arrident] loc
          trans x = Transpose (cs1++cs2) 0 1 x loc

unNest :: LoopNest -> SOAC
unNest (SWIM cs1 cs2 (e,a) (accident,arrident) ml loc) =
  Scan2 cs1 sl [e] [a] ts loc
    where sl = TupleLambda {
                 tupleLambdaParams = map toParam [accident,arrident]
               , tupleLambdaBody = slbody
               , tupleLambdaReturnType = [toDecl $ typeOf slbody]
               , tupleLambdaSrcLoc = loc
               }
          slbody = L0.Map2 cs2 ml [Var accident,Var arrident]
                   [rowType $ identType accident,
                    rowType $ identType arrident]
                   loc
          ts = [rowType $ typeOf a]

toNest :: SOAC -> Maybe LoopNest
toNest (Scan2 cs1 sl [e] [a] _ loc)
  | (L0.Map2 cs2 ml as _ _) <- tupleLambdaBody sl,

    [accparam,arrparam] <- tupleLambdaParams sl,
    [Var accident,Var arrident] <- as,

    [accparam, arrparam] `matches` [accident, arrident] =
      Just $ SWIM cs1 cs2 (e, a) (accident,arrident) ml loc
toNest _ = Nothing

type MapN = (Certificates, TupleLambda, [([Ident], [DeclType])], [Exp], SrcLoc)

mapNfromSOAC :: SOAC -> Maybe MapN
mapNfromSOAC (Map2 cs l as _ loc) =
  let (inner_cs, inner_l, inner_params) = descend l
  in Just (cs++inner_cs, inner_l, inner_params, as, loc)
  where descend l'
          | Just (Map2 inner_cs inner_l inner_as _ _) <-
              fromExp (tupleLambdaBody l'),
            Just ks <- vars inner_as,
            tupleLambdaParams l' `matches` ks =
              let (inner_inner_cs, inner_inner_l, inner_inner_params) = descend inner_l
              in (inner_cs++inner_inner_cs,
                  inner_inner_l,
                  (ks,tupleLambdaReturnType l'):inner_inner_params)
          | otherwise = ([], l', [])

mapNfromSOAC _ = Nothing

mapNtoSOAC :: MapN -> SOAC
mapNtoSOAC (cs, l, [], as, loc) =
  Map2 cs l as (map (rowType . typeOf) as) loc
mapNtoSOAC (cs, l, (ps,rt):pss, as, loc) =
  Map2 cs l' as (map (rowType . typeOf) as) loc
    where l' = TupleLambda { tupleLambdaParams = map toParam ps
                           , tupleLambdaReturnType = rt
                           , tupleLambdaBody = body
                           , tupleLambdaSrcLoc = loc
                           }
          body = toExp $ mapNtoSOAC ([], l, pss, map Var ps, loc)

vars :: [Exp] -> Maybe [Ident]
vars = mapM varExp
  where varExp (Var k) = Just k
        varExp _       = Nothing

matches :: [Parameter] -> [Ident] -> Bool
matches params idds =
  and $ zipWith (==) (map identName params) (map identName idds)
