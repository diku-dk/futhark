module L0C.HOTrans.SOACNest ( SOACNest(..)
                            , Combinator(..)
                            , NestBody(..)
                            , bodyToLambda
                            , lambdaToBody
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
              | NewNest Nesting Combinator
                deriving (Show)

bodyToLambda :: NestBody -> TupleLambda
bodyToLambda (Lambda l) = l
bodyToLambda (NewNest (paramIds, bndIds, retTypes) op) =
  TupleLambda { tupleLambdaSrcLoc = loc
              , tupleLambdaParams = map toParam paramIds
              , tupleLambdaReturnType = retTypes
              , tupleLambdaBody =
                LetPat (TupId (map Id bndIds) loc)
                       (SOAC.toExp $ toSOAC $ SOACNest (map SOAC.Var paramIds) op)
                       (TupLit (map Var bndIds) loc)
                       loc
              }
  where loc = srclocOf op

lambdaToBody :: TupleLambda -> NestBody
lambdaToBody l = fromMaybe (Lambda l) $ isNesting $ tupleLambdaBody l
  where isNesting (LetPat pat e body _) = do
          soac <- either (const Nothing) Just $ SOAC.fromExp e
          inpks <- inpVars $ SOAC.inputs soac
          ks <- tuplePatAndLit pat body
          if tupleLambdaParams l `matches` inpks then
            let nesting = (inpks, ks, tupleLambdaReturnType l)
            in Just $ NewNest nesting $ operation $ fromSOAC soac
          else Nothing
        isNesting _ = Nothing

data Combinator = Map2 Certificates NestBody [Nesting] SrcLoc
                | Reduce2 Certificates NestBody [Nesting] [Exp] SrcLoc
                | Scan2 Certificates NestBody [Nesting] [Exp] SrcLoc
                | Filter2 Certificates NestBody [Nesting] SrcLoc
                | Redomap2 Certificates TupleLambda NestBody [Nesting] [Exp] SrcLoc
                 deriving (Show)

instance Located Combinator where
  locOf (Map2 _ _ _ loc) = locOf loc
  locOf (Reduce2 _ _ _ _ loc) = locOf loc
  locOf (Scan2 _ _ _ _ loc) = locOf loc
  locOf (Filter2 _ _ _ loc) = locOf loc
  locOf (Redomap2 _ _ _ _ _ loc) = locOf loc

levels :: Combinator -> [Nesting]
levels (Map2 _ _ ls _) = ls
levels (Reduce2 _ _ ls _ _) = ls
levels (Scan2 _ _ ls _ _) = ls
levels (Filter2 _ _ ls _) = ls
levels (Redomap2 _ _ _ ls _ _) = ls

setLevels :: [Nesting] -> Combinator -> Combinator
setLevels ls (Map2 cs b _ loc) = Map2 cs b ls loc
setLevels ls (Reduce2 cs b _ es loc) = Reduce2 cs b ls es loc
setLevels ls (Scan2 cs b _ es loc) = Scan2 cs b ls es loc
setLevels ls (Filter2 cs b _ loc) = Filter2 cs b ls loc
setLevels ls (Redomap2 cs l b _ es loc) = Redomap2 cs l b ls es loc

data SOACNest = SOACNest { inputs :: [SOAC.Input]
                         , operation :: Combinator
                         }
                deriving (Show)

instance Located SOACNest where
  locOf = locOf . operation

setInputs :: [SOAC.Input] -> SOACNest -> SOACNest
setInputs arrs nest = nest { inputs = arrs }

-- | Returns the certificates used in a SOACNest.
certificates :: SOACNest -> Certificates
certificates (SOACNest _ (Map2     cs _     _ _)) = cs
certificates (SOACNest _ (Reduce2  cs _   _ _ _)) = cs
certificates (SOACNest _ (Scan2    cs _   _ _ _)) = cs
certificates (SOACNest _ (Filter2  cs _   _   _)) = cs
certificates (SOACNest _ (Redomap2 cs _ _ _ _ _)) = cs

fromSOAC :: SOAC -> SOACNest
fromSOAC (SOAC.Map2 cs l as loc)
  | Just (Map2 cs2 l2 params _, nest) <- nested l =
      SOACNest as $ Map2 (cs++cs2) l2 (nest:params) loc
  | otherwise =
      SOACNest as $ Map2 cs (lambdaToBody l) [] loc
fromSOAC (SOAC.Reduce2 cs l es as loc)
  | Just (Reduce2 cs2 l2 params _ _, nest) <- nested l =
      SOACNest as $ Reduce2 (cs++cs2) l2 (nest:params) es loc
  | otherwise =
      SOACNest as $ Reduce2 cs (lambdaToBody l) [] es loc
fromSOAC (SOAC.Scan2 cs l es as loc)
  | Just (Scan2 cs2 l2 params _ _, nest) <- nested l =
      SOACNest as $ Scan2 (cs++cs2) l2 (nest:params) es loc
  | otherwise =
      SOACNest as $ Scan2 cs (lambdaToBody l) [] es loc
fromSOAC (SOAC.Filter2 cs l as loc)
  | Just (Filter2 cs2 l2 params  _, nest) <- nested l =
      SOACNest as $ Filter2 (cs++cs2) l2 (nest:params) loc
  | otherwise =
      SOACNest as $ Filter2 cs (lambdaToBody l) [] loc
fromSOAC (SOAC.Redomap2 cs ol l es as loc) =
  -- Never nested, because we need a way to test alpha-equivalence of
  -- the outer combining function.
  SOACNest as $ Redomap2 cs ol (lambdaToBody l) [] es loc

nested :: TupleLambda -> Maybe (Combinator, Nesting)
nested l
  | LetPat (TupId pats _) e (TupLit es _) _ <- -- Is a let-binding...
      tupleLambdaBody l,
    Just tks  <- vars es, map Id tks == pats, -- ...where the body is
                                              -- a tuple literal of
                                              -- the bound variables
    Right soac <- fromSOAC <$> SOAC.fromExp e, -- ...the bindee is a SOAC...
    Just ks   <- inpVars $ inputs soac, -- ...all of whose inputs are variables...
    tupleLambdaParams l `matches` ks = -- ...and those inputs are the parameters to l!
      Just (operation soac, (ks, tks, tupleLambdaReturnType l))
  | otherwise = Nothing

toSOAC :: SOACNest -> SOAC
toSOAC (SOACNest as comb@(Map2 cs b _ loc)) =
  SOAC.Map2 cs (subLambda b comb) as loc
toSOAC (SOACNest as comb@(Reduce2 cs b _ es loc)) =
  SOAC.Reduce2 cs (subLambda b comb) es as loc
toSOAC (SOACNest as comb@(Scan2 cs b _ es loc)) =
  SOAC.Scan2 cs (subLambda b comb) es as loc
toSOAC (SOACNest as comb@(Filter2 cs b _ loc)) =
  SOAC.Filter2 cs (subLambda b comb) as loc
toSOAC (SOACNest as comb@(Redomap2 cs l b _ es loc)) =
  SOAC.Redomap2 cs l (subLambda b comb) es as loc

subLambda :: NestBody -> Combinator -> TupleLambda
subLambda b comb =
  case levels comb of
    [] -> bodyToLambda b
    ((paramIds, bndIds, retTypes):rest) ->
      TupleLambda { tupleLambdaReturnType = retTypes
                  , tupleLambdaBody       =
                    LetPat (TupId (map Id bndIds) loc)
                           (SOAC.toExp $ toSOAC $
                            SOACNest (map SOAC.Var paramIds)
                                     (rest `setLevels` comb))
                           (TupLit (map Var bndIds) loc) loc
                  , tupleLambdaSrcLoc     = loc
                  , tupleLambdaParams     = map toParam paramIds
                  }
  where loc = srclocOf comb

vars :: [Exp] -> Maybe [Ident]
vars = mapM varExp
  where varExp (Var k) = Just k
        varExp _       = Nothing

inpVars :: [SOAC.Input] -> Maybe [Ident]
inpVars = vars . map SOAC.inputToExp

matches :: [Parameter] -> [Ident] -> Bool
matches params idds =
  and $ zipWith (==) (map identName params) (map identName idds)

tuplePatAndLit :: TupIdent -> Exp -> Maybe [Ident]
tuplePatAndLit (TupId pats _) (TupLit es _)
  | Just ks <- vars es, map Id ks == pats = Just ks
tuplePatAndLit _ _                        = Nothing
