module Futhark.Analysis.HORepresentation.SOACNest
  ( SOACNest (..)
  , Combinator (..)
  , body
  , setBody
  , params
  , returnType
  , NestBody (..)
  , bodyParams
  , Nesting (..)
  , bodyToLambda
  , lambdaToBody
  , setInputs
  , certificates
  , setCertificates
  , combCertificates
  , setCombCertificates
  , fromExp
  , toExp
  , fromSOAC
  , toSOAC
  )
  where

import Control.Applicative
import Control.Monad

import Data.Loc
import Data.Maybe
import qualified Data.HashSet as HS

import Futhark.InternalRep hiding (Map, Reduce, Scan, Filter, Redomap, returnType)
import Futhark.MonadFreshNames
import Futhark.Binder
import Futhark.Analysis.HORepresentation.SOAC (SOAC)
import qualified Futhark.Analysis.HORepresentation.SOAC as SOAC
import Futhark.Substitute

-- Current problems:
--
-- * Some "nests" are detected that really are not.  For example,
-- nested reduces that do not use the same accumulator.  Also look at
-- how they deal with their lambda params.  Also, the inputs to a
-- nested loop should not be used inside the body, or it's not a
-- proper nest!  (I think...)

data Nesting = Nesting {
    nestingParams     :: [Ident]
  , nestingInputs     :: [SOAC.Input]
  , nestingResult     :: [Ident]
  , nestingPostBody   :: Body
  , nestingReturnType :: [ConstType]
  } deriving (Eq, Ord, Show)

data NestBody = Fun Lambda
              | NewNest Nesting Combinator
                deriving (Show)

instance Substitute NestBody where
  substituteNames m (NewNest n comb) =
    let n'    = substituteNames m
                n { nestingParams = nestingParams n' }
        comb' = substituteNames m comb
    in NewNest n' comb'
  substituteNames m (Fun l) =
    Fun l { lambdaBody =
              substituteNames m $ lambdaBody l
          }

bodyParams :: NestBody -> [Param]
bodyParams (Fun l)          = lambdaParams l
bodyParams (NewNest nest _) = map toParam $ nestingParams nest

bodyToLambda :: MonadFreshNames m => NestBody -> m Lambda
bodyToLambda (Fun l) = return l
bodyToLambda (NewNest (Nesting paramIds inps bndIds postBody retTypes) op) = do
  (e,f) <- runBinder' $ SOAC.toExp =<< toSOAC (SOACNest inps op)
  return Lambda { lambdaSrcLoc = loc
                , lambdaParams = map toParam paramIds
                , lambdaReturnType = retTypes
                , lambdaBody = f $ Let bndIds e `insertBinding` postBody
                }
  where loc = srclocOf op

lambdaToBody :: Lambda -> NestBody
lambdaToBody l = fromMaybe (Fun l) $ liftM (uncurry $ flip NewNest) $ nested l

data Combinator = Map Certificates NestBody SrcLoc
                | Reduce Certificates NestBody [SubExp] SrcLoc
                | Scan Certificates NestBody [SubExp] SrcLoc
                | Filter Certificates NestBody SubExp SrcLoc
                | Redomap Certificates Lambda NestBody [SubExp] SrcLoc
                 deriving (Show)

instance Located Combinator where
  locOf (Map _ _ loc) = locOf loc
  locOf (Reduce _ _ _ loc) = locOf loc
  locOf (Scan _ _ _ loc) = locOf loc
  locOf (Filter _ _ _ loc) = locOf loc
  locOf (Redomap _ _ _ _ loc) = locOf loc

instance Substitute Combinator where
  substituteNames m comb =
    substituteNames m (body comb) `setBody` comb

instance Substitute Nesting where
  substituteNames m n =
    n { nestingInputs = map (substituteNames m) $ nestingInputs n }

body :: Combinator -> NestBody
body (Map _ b _) = b
body (Reduce _ b _ _) = b
body (Scan _ b _ _) = b
body (Filter _ b _ _) = b
body (Redomap _ _ b _ _) = b

setBody :: NestBody -> Combinator -> Combinator
setBody b (Map cs _ loc) = Map cs b loc
setBody b (Reduce cs _ es loc) = Reduce cs b es loc
setBody b (Scan cs _ es loc) = Scan cs b es loc
setBody b (Filter cs _ outer_shape loc) = Filter cs b outer_shape loc
setBody b (Redomap cs l _ es loc) = Redomap cs l b es loc

combinatorFirstLoop :: Combinator -> ([Param], [ConstType])
combinatorFirstLoop comb =
  case body comb of
  Fun l          -> (lambdaParams l,
                     lambdaReturnType l)
  NewNest nest _ -> (map toParam $ nestingParams nest,
                     nestingReturnType nest)

params :: Combinator -> [Param]
params = fst . combinatorFirstLoop

returnType :: Combinator -> [ConstType]
returnType = snd . combinatorFirstLoop

data SOACNest = SOACNest { inputs :: [SOAC.Input]
                         , operation :: Combinator
                         }
                deriving (Show)

instance Located SOACNest where
  locOf = locOf . operation

setInputs :: [SOAC.Input] -> SOACNest -> SOACNest
setInputs arrs nest = nest { inputs = arrs }

-- | Returns the certificates used in a 'SOACNest'.  Just wraps
-- 'combCertificates'.
certificates :: SOACNest -> Certificates
certificates = combCertificates . operation

-- | Sets the certificates used in a 'SOACNest'.  Just wraps
-- 'combSetCertificates'.
setCertificates :: Certificates -> SOACNest -> SOACNest
setCertificates cs (SOACNest inp comb) =
  SOACNest inp $ cs `setCombCertificates` comb

-- | Returns the certificates used in a 'Combinator'.
combCertificates :: Combinator -> Certificates
combCertificates (Map     cs _     _) = cs
combCertificates (Reduce  cs _   _ _) = cs
combCertificates (Scan    cs _   _ _) = cs
combCertificates (Filter  cs _   _ _) = cs
combCertificates (Redomap cs _ _ _ _) = cs

-- | Sets the certificates used in a 'Combinator'.
setCombCertificates :: Certificates -> Combinator -> Combinator
setCombCertificates cs (Map     _ bdy     loc) = Map    cs bdy     loc
setCombCertificates cs (Reduce  _ bdy acc loc) = Reduce cs bdy acc loc
setCombCertificates cs (Scan    _ bdy acc loc) = Scan   cs bdy acc loc
setCombCertificates cs (Filter  _ bdy siz loc) = Filter cs bdy siz loc
setCombCertificates cs (Redomap _ fun bdy acc loc) = Redomap cs fun bdy acc loc

fromExp :: Exp -> Either SOAC.NotSOAC SOACNest
fromExp = liftM fromSOAC . SOAC.fromExp

toExp :: SOACNest -> Binder Exp
toExp = SOAC.toExp <=< toSOAC

fromSOAC :: SOAC -> SOACNest
fromSOAC (SOAC.Map cs l as loc) =
  SOACNest as $ Map cs (lambdaToBody l) loc
fromSOAC (SOAC.Reduce cs l args loc) =
  SOACNest (map snd args) $
  Reduce cs (lambdaToBody l) (map fst args) loc
fromSOAC (SOAC.Scan cs l args loc) =
  SOACNest (map snd args) $
  Scan cs (lambdaToBody l) (map fst args) loc
fromSOAC (SOAC.Filter cs l as outer_shape loc) =
  SOACNest as $ Filter cs (lambdaToBody l) outer_shape loc
fromSOAC (SOAC.Redomap cs ol l es as loc) =
  -- Never nested, because we need a way to test alpha-equivalence of
  -- the outer combining function.
  SOACNest as $ Redomap cs ol (lambdaToBody l) es loc

nested :: Lambda -> Maybe (Combinator, Nesting)
nested l
  | Body (Let ids e:bnds) res <- lambdaBody l, -- Is a let-binding...
    Right soac <- fromSOAC <$> SOAC.fromExp e, -- ...the bindee is a SOAC...
    Just postBody <-
      checkPostBody (map fromParam $ lambdaParams l) $ Body bnds res =
      Just (operation soac,
            -- ... FIXME: need more checks here.
            Nesting { nestingParams = map fromParam $ lambdaParams l
                    , nestingInputs = inputs soac
                    , nestingResult = ids
                    , nestingPostBody = postBody
                    , nestingReturnType = lambdaReturnType l
                    })
  | otherwise = Nothing

checkPostBody :: [Ident] -> Body -> Maybe Body
checkPostBody ks b
  | HS.null $ HS.fromList ks `HS.intersection` freeInBody b = Just b
  | otherwise                                               = Nothing

toSOAC :: MonadFreshNames m => SOACNest -> m SOAC
toSOAC (SOACNest as (Map cs b loc)) =
  SOAC.Map cs <$> bodyToLambda b <*> pure as <*> pure loc
toSOAC (SOACNest as (Reduce cs b es loc)) =
  SOAC.Reduce cs <$> bodyToLambda b <*> pure (zip es as) <*> pure loc
toSOAC (SOACNest as (Scan cs b es loc)) =
  SOAC.Scan cs <$> bodyToLambda b <*> pure (zip es as) <*> pure loc
toSOAC (SOACNest as (Filter cs b outer_shape loc)) =
  SOAC.Filter cs <$> bodyToLambda b <*> pure as <*> pure outer_shape <*> pure loc
toSOAC (SOACNest as (Redomap cs l b es loc)) =
  SOAC.Redomap cs l <$> bodyToLambda b <*> pure es <*> pure as <*> pure loc
