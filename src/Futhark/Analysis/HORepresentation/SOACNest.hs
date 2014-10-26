module Futhark.Analysis.HORepresentation.SOACNest
  ( SOACNest (..)
  , Combinator (..)
  , body
  , setBody
  , params
  , returnType
  , NestBody (..)
  , nestBodyParams
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

import Futhark.Representation.AST hiding (Map, Reduce, Scan, Filter, Redomap)
import Futhark.MonadFreshNames
import Futhark.Binder
import Futhark.Tools (instantiateExtTypes)
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

data Nesting lore = Nesting {
    nestingParamNames :: [VName]
  , nestingInputs     :: [SOAC.Input]
  , nestingResult     :: [VName]
  , nestingCertificates :: Certificates
  , nestingReturnType :: [Type]
  } deriving (Eq, Ord, Show)

data NestBody lore = Fun (Lambda lore)
                   | NewNest (Nesting lore) (Combinator lore)
                deriving (Show)

nestBodyParams :: NestBody lore -> [Param]
nestBodyParams (Fun lam) = lambdaParams lam
nestBodyParams (NewNest nesting (Reduce _ _ acc loc)) =
  foldNestParams nesting acc loc
nestBodyParams (NewNest nesting (Scan _ _ acc loc)) =
  foldNestParams nesting acc loc
nestBodyParams (NewNest nesting (Redomap _ _ _ acc loc)) =
  foldNestParams nesting acc loc
nestBodyParams (NewNest nesting comb) =
  foldNestParams nesting [] $ srclocOf comb

foldNestParams :: Nesting lore -> [SubExp] -> SrcLoc -> [Ident]
foldNestParams nesting acc loc =
  zipWith mkParam (nestingParamNames nesting) $
  map subExpType acc ++
  map (rowType . SOAC.inputType) (nestingInputs nesting)
  where mkParam name t = Ident name t loc

instance Substitutable lore => Substitute (NestBody lore) where
  substituteNames m (NewNest n comb) =
    let n'    = substituteNames m n
        comb' = substituteNames m comb
    in NewNest n' comb'
  substituteNames m (Fun l) =
    Fun l { lambdaBody =
              substituteNames m $ lambdaBody l
          }

bodyToLambda :: (Bindable lore, MonadFreshNames m) =>
                [Type] -> NestBody lore -> m (Lambda lore)
bodyToLambda _ (Fun l) = return l
bodyToLambda inpts (NewNest (Nesting paramNames inps bndIds resCerts retTypes) op) = do
  (e,f) <- runBinder' $ SOAC.toExp =<< toSOAC (SOACNest inps op)
  return
    Lambda { lambdaSrcLoc = loc
           , lambdaParams = zipWith mkIdents paramNames $ map rowType inpts
           , lambdaReturnType = retTypes
           , lambdaBody =
             let idents = instantiateExtTypes loc bndIds $
                          resTypeValues $ typeOf e
             in f $ mkBody [mkLet idents e] $
                Result resCerts (map Var idents) loc
           }
  where loc = srclocOf op
        mkIdents name t = Ident name t loc

lambdaToBody :: Bindable lore => Lambda lore -> NestBody lore
lambdaToBody l = fromMaybe (Fun l) $ liftM (uncurry $ flip NewNest) $ nested l

data Combinator lore = Map Certificates (NestBody lore) SrcLoc
                     | Reduce Certificates (NestBody lore) [SubExp] SrcLoc
                     | Scan Certificates (NestBody lore) [SubExp] SrcLoc
                     | Filter Certificates (NestBody lore) SrcLoc
                     | Redomap Certificates (Lambda lore) (NestBody lore) [SubExp] SrcLoc
                 deriving (Show)

instance Located (Combinator lore) where
  locOf (Map _ _ loc) = locOf loc
  locOf (Reduce _ _ _ loc) = locOf loc
  locOf (Scan _ _ _ loc) = locOf loc
  locOf (Filter _ _ loc) = locOf loc
  locOf (Redomap _ _ _ _ loc) = locOf loc

instance Substitutable lore => Substitute (Combinator lore) where
  substituteNames m comb =
    substituteNames m (body comb) `setBody` comb

instance Substitutable lore => Substitute (Nesting lore) where
  substituteNames m n =
    n { nestingInputs = map (substituteNames m) $ nestingInputs n }

body :: Combinator lore -> NestBody lore
body (Map _ b _) = b
body (Reduce _ b _ _) = b
body (Scan _ b _ _) = b
body (Filter _ b _) = b
body (Redomap _ _ b _ _) = b

setBody :: NestBody lore -> Combinator lore -> Combinator lore
setBody b (Map cs _ loc) = Map cs b loc
setBody b (Reduce cs _ es loc) = Reduce cs b es loc
setBody b (Scan cs _ es loc) = Scan cs b es loc
setBody b (Filter cs _ loc) = Filter cs b loc
setBody b (Redomap cs l _ es loc) = Redomap cs l b es loc

combinatorFirstLoop :: Combinator lore -> ([Param], [Type])
combinatorFirstLoop comb =
  (nestBodyParams $ body comb,
   case body comb of
     Fun l          -> lambdaReturnType l
     NewNest nest _ -> nestingReturnType nest)

params :: Combinator lore -> [Param]
params = fst . combinatorFirstLoop

returnType :: Combinator lore -> [Type]
returnType = snd . combinatorFirstLoop

data SOACNest lore = SOACNest { inputs :: [SOAC.Input]
                              , operation :: Combinator lore
                              }
                deriving (Show)

instance Located (SOACNest lore) where
  locOf = locOf . operation

setInputs :: [SOAC.Input] -> SOACNest lore -> SOACNest lore
setInputs arrs nest = nest { inputs = arrs }

-- | Returns the certificates used in a 'SOACNest'.  Just wraps
-- 'combCertificates'.
certificates :: SOACNest lore -> Certificates
certificates = combCertificates . operation

-- | Sets the certificates used in a 'SOACNest'.  Just wraps
-- 'combSetCertificates'.
setCertificates :: Certificates -> SOACNest lore -> SOACNest lore
setCertificates cs (SOACNest inp comb) =
  SOACNest inp $ cs `setCombCertificates` comb

-- | Returns the certificates used in a 'Combinator'.
combCertificates :: Combinator lore -> Certificates
combCertificates (Map     cs _     _) = cs
combCertificates (Reduce  cs _   _ _) = cs
combCertificates (Scan    cs _   _ _) = cs
combCertificates (Filter  cs _     _) = cs
combCertificates (Redomap cs _ _ _ _) = cs

-- | Sets the certificates used in a 'Combinator'.
setCombCertificates :: Certificates -> Combinator lore -> Combinator lore
setCombCertificates cs (Map     _ bdy     loc) = Map    cs bdy     loc
setCombCertificates cs (Reduce  _ bdy acc loc) = Reduce cs bdy acc loc
setCombCertificates cs (Scan    _ bdy acc loc) = Scan   cs bdy acc loc
setCombCertificates cs (Filter  _ bdy     loc) = Filter cs bdy loc
setCombCertificates cs (Redomap _ fun bdy acc loc) = Redomap cs fun bdy acc loc

fromExp :: Bindable lore =>
           Exp lore -> Either SOAC.NotSOAC (SOACNest lore)
fromExp = liftM fromSOAC . SOAC.fromExp

toExp :: Bindable lore => SOACNest lore -> Binder lore (Exp lore)
toExp = SOAC.toExp <=< toSOAC

fromSOAC :: Bindable lore => SOAC lore -> SOACNest lore
fromSOAC (SOAC.Map cs l as loc) =
  SOACNest as $ Map cs (lambdaToBody l) loc
fromSOAC (SOAC.Reduce cs l args loc) =
  SOACNest (map snd args) $
  Reduce cs (lambdaToBody l) (map fst args) loc
fromSOAC (SOAC.Scan cs l args loc) =
  SOACNest (map snd args) $
  Scan cs (lambdaToBody l) (map fst args) loc
fromSOAC (SOAC.Filter cs l as loc) =
  SOACNest as $ Filter cs (lambdaToBody l) loc
fromSOAC (SOAC.Redomap cs ol l es as loc) =
  -- Never nested, because we need a way to test alpha-equivalence of
  -- the outer combining function.
  SOACNest as $ Redomap cs ol (lambdaToBody l) es loc

nested :: Bindable lore => Lambda lore -> Maybe (Combinator lore, Nesting lore)
nested l
  | Body _ [Let pat _ e] res <- lambdaBody l, -- Is a let-binding...
    Right soac <- fromSOAC <$> SOAC.fromExp e, -- ...the bindee is a SOAC...
    resultSubExps res == map Var (patternIdents pat) =
      Just (operation soac,
            -- ... FIXME?: need more checks here.
            Nesting { nestingParamNames = map identName $ lambdaParams l
                    , nestingInputs = inputs soac
                    , nestingResult = patternNames pat
                    , nestingCertificates = resultCertificates res
                    , nestingReturnType = lambdaReturnType l
                    })
  | otherwise = Nothing

toSOAC :: (Bindable lore, MonadFreshNames m) => SOACNest lore -> m (SOAC lore)
toSOAC (SOACNest as (Map cs b loc)) =
  SOAC.Map cs <$> bodyToLambda (map SOAC.inputType as) b <*> pure as <*> pure loc
toSOAC (SOACNest as (Reduce cs b es loc)) =
  SOAC.Reduce cs <$> bodyToLambda (map SOAC.inputType as) b <*> pure (zip es as) <*> pure loc
toSOAC (SOACNest as (Scan cs b es loc)) =
  SOAC.Scan cs <$> bodyToLambda (map SOAC.inputType as) b <*> pure (zip es as) <*> pure loc
toSOAC (SOACNest as (Filter cs b loc)) =
  SOAC.Filter cs <$> bodyToLambda (map SOAC.inputType as) b <*> pure as <*> pure loc
toSOAC (SOACNest as (Redomap cs l b es loc)) =
  SOAC.Redomap cs l <$> bodyToLambda (map SOAC.inputType as) b <*> pure es <*> pure as <*> pure loc
