module L0C.HORepresentation.SOACNest
  ( SOACNest (..)
  , Combinator (..)
  , nesting
  , setNesting
  , body
  , setBody
  , params
  , NestBody (..)
  , Nesting (..)
  , bodyToLambda
  , lambdaToBody
  , setInputs
  , certificates
  , fromExp
  , fromSOAC
  , toSOAC
  )
  where

-- Current problems:
--
-- * Some "nests" are detected that really are not.  For example,
-- nested reduces that do not use the same accumulator.  Also look at
-- how they deal with their lambda params.  Also, the inputs to a
-- nested loop should not be used inside the body, or it's not a
-- proper nest!  (I think...)

import Control.Applicative
import Control.Monad

import Data.Loc
import Data.Maybe

import L0C.HORepresentation.SOAC (SOAC)
import qualified L0C.HORepresentation.SOAC as SOAC
import L0C.L0 hiding (MapT, ReduceT, ScanT, FilterT, RedomapT)

data Nesting = Nesting {
    nestingParams     :: [Ident]
  , nestingInputs     :: [SOAC.Input]
  , nestingResult     :: [Ident]
  , nestingReturnType :: [DeclType]
  } deriving (Show)

data NestBody = Lambda TupleLambda
              | NewNest Nesting Combinator
                deriving (Show)

bodyToLambda :: NestBody -> TupleLambda
bodyToLambda (Lambda l) = l
bodyToLambda (NewNest (Nesting paramIds inps bndIds retTypes) op) =
  TupleLambda { tupleLambdaSrcLoc = loc
              , tupleLambdaParams = map toParam paramIds
              , tupleLambdaReturnType = retTypes
              , tupleLambdaBody =
                LetPat (TupId (map Id bndIds) loc)
                       (SOAC.toExp $ toSOAC $ SOACNest inps op)
                       (TupLit (map Var bndIds) loc)
                       loc
              }
  where loc = srclocOf op

lambdaToBody :: TupleLambda -> NestBody
lambdaToBody l = fromMaybe (Lambda l) $ isNesting $ tupleLambdaBody l
  where isNesting (LetPat pat e b _) = do
          soac <- either (const Nothing) Just $ SOAC.fromExp e
          ks <- tuplePatAndLit pat b
          let inps = SOAC.inputs soac
              ps = map fromParam $ tupleLambdaParams l -- XXX: Loses aliasing information.
              nest = Nesting ps inps ks $ tupleLambdaReturnType l
          Just $ NewNest nest (operation $ fromSOAC soac)
        isNesting _ = Nothing

data Combinator = MapT Certificates NestBody [Nesting] SrcLoc
                | ReduceT Certificates NestBody [Nesting] [Exp] SrcLoc
                | ScanT Certificates NestBody [Nesting] [Exp] SrcLoc
                | FilterT Certificates NestBody [Nesting] SrcLoc
                | RedomapT Certificates TupleLambda NestBody [Nesting] [Exp] SrcLoc
                 deriving (Show)

instance Located Combinator where
  locOf (MapT _ _ _ loc) = locOf loc
  locOf (ReduceT _ _ _ _ loc) = locOf loc
  locOf (ScanT _ _ _ _ loc) = locOf loc
  locOf (FilterT _ _ _ loc) = locOf loc
  locOf (RedomapT _ _ _ _ _ loc) = locOf loc

nesting :: Combinator -> [Nesting]
nesting (MapT _ _ ls _) = ls
nesting (ReduceT _ _ ls _ _) = ls
nesting (ScanT _ _ ls _ _) = ls
nesting (FilterT _ _ ls _) = ls
nesting (RedomapT _ _ _ ls _ _) = ls

setNesting :: [Nesting] -> Combinator -> Combinator
setNesting ls (MapT cs b _ loc) = MapT cs b ls loc
setNesting ls (ReduceT cs b _ es loc) = ReduceT cs b ls es loc
setNesting ls (ScanT cs b _ es loc) = ScanT cs b ls es loc
setNesting ls (FilterT cs b _ loc) = FilterT cs b ls loc
setNesting ls (RedomapT cs l b _ es loc) = RedomapT cs l b ls es loc

body :: Combinator -> NestBody
body (MapT _ b _ _) = b
body (ReduceT _ b _ _ _) = b
body (ScanT _ b _ _ _) = b
body (FilterT _ b _ _) = b
body (RedomapT _ _ b _ _ _) = b

setBody :: NestBody -> Combinator -> Combinator
setBody b (MapT cs _ ls loc) = MapT cs b ls loc
setBody b (ReduceT cs _ ls es loc) = ReduceT cs b ls es loc
setBody b (ScanT cs _ ls es loc) = ScanT cs b ls es loc
setBody b (FilterT cs _ ls loc) = FilterT cs b ls loc
setBody b (RedomapT cs l _ ls es loc) = RedomapT cs l b ls es loc

params :: Combinator -> [Parameter]
params comb =
  case nesting comb of
    nest:_ -> map toParam $ nestingParams nest
    []     -> case body comb of
                Lambda l       -> tupleLambdaParams l
                NewNest nest _ -> map toParam $ nestingParams nest

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
certificates (SOACNest _ (MapT     cs _     _ _)) = cs
certificates (SOACNest _ (ReduceT  cs _   _ _ _)) = cs
certificates (SOACNest _ (ScanT    cs _   _ _ _)) = cs
certificates (SOACNest _ (FilterT  cs _   _   _)) = cs
certificates (SOACNest _ (RedomapT cs _ _ _ _ _)) = cs

fromExp :: Exp -> Either SOAC.NotSOAC SOACNest
fromExp = liftM fromSOAC . SOAC.fromExp

fromSOAC :: SOAC -> SOACNest
fromSOAC (SOAC.MapT cs l as loc)
  | Just (MapT cs2 l2 ps _, nest) <- nested l =
      SOACNest as $ MapT (cs++cs2) l2 (nest:ps) loc
  | otherwise =
      SOACNest as $ MapT cs (lambdaToBody l) [] loc
fromSOAC (SOAC.ReduceT cs l es as loc)
  | Just (ReduceT cs2 l2 ps _ _, nest) <- nested l =
      SOACNest as $ ReduceT (cs++cs2) l2 (nest:ps) es loc
  | otherwise =
      SOACNest as $ ReduceT cs (lambdaToBody l) [] es loc
fromSOAC (SOAC.ScanT cs l es as loc)
  | Just (ScanT cs2 l2 ps _ _, nest) <- nested l =
      SOACNest as $ ScanT (cs++cs2) l2 (nest:ps) es loc
  | otherwise =
      SOACNest as $ ScanT cs (lambdaToBody l) [] es loc
fromSOAC (SOAC.FilterT cs l as loc)
  | Just (FilterT cs2 l2 ps  _, nest) <- nested l =
      SOACNest as $ FilterT (cs++cs2) l2 (nest:ps) loc
  | otherwise =
      SOACNest as $ FilterT cs (lambdaToBody l) [] loc
fromSOAC (SOAC.RedomapT cs ol l es as loc) =
  -- Never nested, because we need a way to test alpha-equivalence of
  -- the outer combining function.
  SOACNest as $ RedomapT cs ol (lambdaToBody l) [] es loc

nested :: TupleLambda -> Maybe (Combinator, Nesting)
nested l
  | LetPat (TupId pats _) e (TupLit es _) _ <- -- Is a let-binding...
      tupleLambdaBody l,
    Just tks  <- vars es, map Id tks == pats, -- ...where the body is
                                              -- a tuple literal of
                                              -- the bound variables
    Right soac <- fromSOAC <$> SOAC.fromExp e = -- ...the bindee is a SOAC...
      Just (operation soac,
            case inpVars $ inputs soac of
              Just ks -- ...all of whose inputs are variables...
                | tupleLambdaParams l `matches` ks -> -- ...and those inputs are the parameters to l!
                    Nesting ks Nothing tks $ tupleLambdaReturnType l
              _ -> Nesting (map fromParam $ tupleLambdaParams l) -- ... if they are something else.
                           (Just $ inputs soac) tks (tupleLambdaReturnType l))
  | otherwise = Nothing

toSOAC :: SOACNest -> SOAC
toSOAC (SOACNest as comb@(MapT cs b _ loc)) =
  SOAC.MapT cs (subLambda b comb) as loc
toSOAC (SOACNest as comb@(ReduceT cs b _ es loc)) =
  SOAC.ReduceT cs (subLambda b comb) es as loc
toSOAC (SOACNest as comb@(ScanT cs b _ es loc)) =
  SOAC.ScanT cs (subLambda b comb) es as loc
toSOAC (SOACNest as comb@(FilterT cs b _ loc)) =
  SOAC.FilterT cs (subLambda b comb) as loc
toSOAC (SOACNest as comb@(RedomapT cs l b _ es loc)) =
  SOAC.RedomapT cs l (subLambda b comb) es as loc

subLambda :: NestBody -> Combinator -> TupleLambda
subLambda b comb =
  case nesting comb of
    [] -> bodyToLambda b
    (Nesting paramIds inps bndIds retTypes:rest) ->
      TupleLambda { tupleLambdaReturnType = retTypes
                  , tupleLambdaBody       =
                    LetPat (TupId (map Id bndIds) loc)
                             (SOAC.toExp $ toSOAC $
                              SOACNest inps (rest `setNesting` comb))
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
inpVars = mapM SOAC.inputArray

matches :: [Parameter] -> [Ident] -> Bool
matches ps idds =
  length ps == length idds &&
  and (zipWith (==) (map identName ps) (map identName idds))

tuplePatAndLit :: TupIdent -> Exp -> Maybe [Ident]
tuplePatAndLit (TupId pats _) (TupLit es _)
  | Just ks <- vars es, map Id ks == pats = Just ks
tuplePatAndLit _ _                        = Nothing
