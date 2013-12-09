module L0C.HOTrans.SOACNest ( SOACNest(..)
                            , inputs
                            , setInputs
                            , certificates
                            , fromSOAC
                            , toSOAC
                            )
  where

import Control.Applicative

import Data.Loc
import Data.Maybe

import L0C.HOTrans.SOAC (SOAC)
import qualified L0C.HOTrans.SOAC as SOAC
import L0C.L0 hiding (Map2, Reduce2, Scan2, Filter2, Redomap2)

type Nesting = ([Ident], [Ident], [DeclType])

data NestBody = Lambda TupleLambda
              | SOAC Nesting SOACNest
                deriving (Show)

bodyToLambda :: NestBody -> TupleLambda
bodyToLambda (Lambda l) = l
bodyToLambda (SOAC (paramIds, bndIds, retTypes) nest) =
  TupleLambda { tupleLambdaSrcLoc = loc
              , tupleLambdaParams = map toParam paramIds
              , tupleLambdaReturnType = retTypes
              , tupleLambdaBody =
                LetPat (TupId (map Id bndIds) loc) (SOAC.toExp $ toSOAC nest)
                (TupLit (map Var bndIds) loc) loc
              }
  where loc = srclocOf nest

lambdaToBody :: TupleLambda -> NestBody
lambdaToBody l = fromMaybe (Lambda l) $ isNesting $ tupleLambdaBody l
  where isNesting (LetPat pat e body _) = do
          soac <- SOAC.fromExp e
          inpks <- vars $ SOAC.inputs soac
          ks <- tuplePatAndLit pat body
          if tupleLambdaParams l `matches` inpks then
            let nesting = (inpks, ks, tupleLambdaReturnType l)
            in Just $ SOAC nesting $ fromSOAC soac
          else Nothing
        isNesting _ = Nothing


data SOACNest = Map2 Certificates NestBody [([Ident], [Ident], [DeclType])] [Exp] SrcLoc
              | Reduce2 Certificates NestBody [([Ident], [Ident], [DeclType])] [Exp] [Exp] SrcLoc
              | Scan2 Certificates NestBody [([Ident], [Ident], [DeclType])] [Exp] [Exp] SrcLoc
              | Filter2 Certificates NestBody [([Ident], [Ident], [DeclType])] [Exp] SrcLoc
              | Redomap2 Certificates TupleLambda NestBody [([Ident], [Ident], [DeclType])] [Exp] [Exp] SrcLoc
                deriving (Show)

instance Located SOACNest where
  locOf (Map2 _ _ _ _ loc) = locOf loc
  locOf (Reduce2 _ _ _ _ _ loc) = locOf loc
  locOf (Scan2 _ _ _ _ _ loc) = locOf loc
  locOf (Filter2 _ _ _ _ loc) = locOf loc
  locOf (Redomap2 _ _ _ _ _ _ loc) = locOf loc

inputs :: SOACNest -> [Exp]
inputs (Map2     _ _ _     arrs _) = arrs
inputs (Reduce2  _ _ _   _ arrs _) = arrs
inputs (Scan2    _ _ _   _ arrs _) = arrs
inputs (Filter2  _ _   _   arrs _) = arrs
inputs (Redomap2 _ _ _ _ _ arrs _) = arrs

setInputs :: [Exp] -> SOACNest -> SOACNest
setInputs arrs (Map2 cs lam params _ loc) =
  Map2 cs lam params arrs loc
setInputs arrs (Reduce2 cs lam params ne _ loc) =
  Reduce2 cs lam params ne arrs loc
setInputs arrs (Scan2 cs lam params ne _ loc) =
  Scan2 cs lam params ne arrs loc
setInputs arrs (Filter2 cs lam params _ loc) =
  Filter2 cs lam params arrs loc
setInputs arrs (Redomap2 cs lam1 lam params ne _ loc) =
  Redomap2 cs lam1 lam params ne arrs loc

-- | Returns the certificates used in a SOACNest.
certificates :: SOACNest -> Certificates
certificates (Map2     cs _     _ _ _) = cs
certificates (Reduce2  cs _   _ _ _ _) = cs
certificates (Scan2    cs _   _ _ _ _) = cs
certificates (Filter2  cs _   _   _ _) = cs
certificates (Redomap2 cs _ _ _ _ _ _) = cs

fromSOAC :: SOAC -> SOACNest
fromSOAC (SOAC.Map2 cs l as loc)
  | Just (Map2 cs2 l2 params _ _, nest) <- nested l =
      Map2 (cs++cs2) l2 (nest:params) as loc
  | otherwise = Map2 cs (lambdaToBody l) [] as loc
fromSOAC (SOAC.Reduce2 cs l es as loc)
  | Just (Reduce2 cs2 l2 params _ _ _, nest) <- nested l =
      Reduce2 (cs++cs2) l2 (nest:params) es as loc
  | otherwise = Reduce2 cs (lambdaToBody l) [] es as loc
fromSOAC (SOAC.Scan2 cs l es as loc)
  | Just (Scan2 cs2 l2 params _ _ _, nest) <- nested l =
      Scan2 (cs++cs2) l2 (nest:params) es as loc
  | otherwise = Scan2 cs (lambdaToBody l) [] es as loc
fromSOAC (SOAC.Filter2 cs l as loc)
  | Just (Filter2 cs2 l2 params _ _, nest) <- nested l =
      Filter2 (cs++cs2) l2 (nest:params) as loc
  | otherwise = Filter2 cs (lambdaToBody l) [] as loc
fromSOAC (SOAC.Redomap2 cs ol l es as loc) =
  -- Never nested, because we need a way to test alpha-equivalence of
  -- the outer combining function.
  Redomap2 cs ol (lambdaToBody l) [] es as loc

nested :: TupleLambda -> Maybe (SOACNest, Nesting)
nested l
  | LetPat (TupId pats _) e (TupLit es _) _ <- -- Is a let-binding...
      tupleLambdaBody l,
    Just tks  <- vars es, map Id tks == pats, -- ...where the body is
                                              -- a tuple literal of
                                              -- the bound variables
    Just soac <- fromSOAC <$> SOAC.fromExp e, -- ...the bindee is a SOAC...
    Just ks   <- vars $ inputs soac, -- ...all of whose inputs are variables...
    tupleLambdaParams l `matches` ks = -- ...and those inputs are the parameters to l!
      Just (soac, (ks, tks, tupleLambdaReturnType l))
  | otherwise = Nothing

toSOAC :: SOACNest -> SOAC
toSOAC (Map2 cs b [] as loc) =
  SOAC.Map2 cs (bodyToLambda b) as loc
toSOAC (Map2 cs l (nest@(ks,_,_):ps) as loc) =
  SOAC.Map2 cs l' as loc
    where l' = subLambda (Map2 [] l ps (map Var ks) loc) nest loc
toSOAC (Reduce2 cs b [] es as loc) =
  SOAC.Reduce2 cs (bodyToLambda b) es as loc
toSOAC (Reduce2 cs l (nest@(ks,_,_):ps) es as loc) =
  SOAC.Reduce2 cs l' es as loc
    where l' = subLambda (Reduce2 [] l ps es (map Var ks) loc) nest loc
toSOAC (Scan2 cs b [] es as loc) =
  SOAC.Scan2 cs (bodyToLambda b) es as loc
toSOAC (Scan2 cs l (nest@(ks,_,_):ps) es as loc) =
  SOAC.Scan2 cs l' es as loc
    where l' = subLambda (Scan2 [] l ps es (map Var ks) loc) nest loc
toSOAC (Filter2 cs b [] as loc) =
  SOAC.Filter2 cs (bodyToLambda b) as loc
toSOAC (Filter2 cs l (nest@(ks,_,_):ps) as loc) =
  SOAC.Filter2 cs l' as loc
    where l' = subLambda (Filter2 [] l ps (map Var ks) loc) nest loc
toSOAC (Redomap2 cs ol b [] es as loc) =
  SOAC.Redomap2 cs ol (bodyToLambda b) es as loc
toSOAC (Redomap2 cs ol l (nest@(ks,_,_):ps) es as loc) =
  SOAC.Redomap2 cs ol l' es as loc
    where l' = subLambda (Redomap2 [] ol l ps es (map Var ks) loc) nest loc

subLambda :: SOACNest -> Nesting -> SrcLoc -> TupleLambda
subLambda s (ks, vs, ts) loc =
  TupleLambda { tupleLambdaReturnType = ts
              , tupleLambdaBody       =
                  LetPat (TupId (map Id vs) loc) (SOAC.toExp $ toSOAC s)
                  (TupLit (map Var vs) loc) loc
              , tupleLambdaSrcLoc     = loc
              , tupleLambdaParams     = map toParam ks
              }

vars :: [Exp] -> Maybe [Ident]
vars = mapM varExp
  where varExp (Var k) = Just k
        varExp _       = Nothing

matches :: [Parameter] -> [Ident] -> Bool
matches params idds =
  and $ zipWith (==) (map identName params) (map identName idds)

tuplePatAndLit :: TupIdent -> Exp -> Maybe [Ident]
tuplePatAndLit (TupId pats _) (TupLit es _)
  | Just ks <- vars es, map Id ks == pats = Just ks
tuplePatAndLit _ _                        = Nothing
