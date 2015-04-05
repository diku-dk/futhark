module Futhark.Analysis.HORepresentation.SOACNest
  ( SOACNest (..)
  , Combinator (..)
  , body
  , setBody
  , params
  , returnType
  , typeOf
  , NestBody (..)
  , nestBodyParams
  , nestBodyReturnType
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
import Data.Maybe

import Prelude

import Futhark.Representation.AST hiding (Map, Reduce, Scan, Redomap)
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
  , nestingReturnType :: [Type]
  } deriving (Eq, Ord, Show)

data NestBody lore = Fun (Lambda lore)
                   | NewNest (Nesting lore) (Combinator lore)
                deriving (Show)

nestBodyReturnType :: NestBody lore -> [Type]
nestBodyReturnType (Fun lam)           = lambdaReturnType lam
nestBodyReturnType (NewNest nesting _) = nestingReturnType nesting

nestBodyParams :: NestBody lore -> [Param]
nestBodyParams (Fun lam) = lambdaParams lam
nestBodyParams (NewNest nesting (Reduce _ _ acc)) =
  foldNestParams nesting acc
nestBodyParams (NewNest nesting (Scan _ _ acc)) =
  foldNestParams nesting acc
nestBodyParams (NewNest nesting (Redomap _ _ _ acc)) =
  foldNestParams nesting acc
nestBodyParams (NewNest nesting _) =
  foldNestParams nesting []

foldNestParams :: Nesting lore -> [SubExp] -> [Ident]
foldNestParams nesting acc =
  zipWith Ident (nestingParamNames nesting) $
  map subExpType acc ++
  map (rowType . SOAC.inputType) (nestingInputs nesting)

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
bodyToLambda pts (NewNest (Nesting paramNames inps bndIds retTypes) op) = do
  (e,f) <- runBinder' $ SOAC.toExp =<< toSOAC (SOACNest inps op)
  let idents = instantiateExtTypes bndIds $ expExtType e
  bnd <- mkLetNames' (map identName idents) e
  return
    Lambda { lambdaParams = zipWith Ident paramNames pts
           , lambdaReturnType = retTypes
           , lambdaBody = f $ mkBody [bnd] $
                          Result (map Var idents)
           }

lambdaToBody :: Bindable lore => Lambda lore -> NestBody lore
lambdaToBody l = fromMaybe (Fun l) $ liftM (uncurry $ flip NewNest) $ nested l

data Combinator lore = Map Certificates (NestBody lore)
                     | Reduce Certificates (NestBody lore) [SubExp]
                     | Scan Certificates (NestBody lore) [SubExp]
                     | Redomap Certificates (Lambda lore) (NestBody lore) [SubExp]
                 deriving (Show)

instance Substitutable lore => Substitute (Combinator lore) where
  substituteNames m comb =
    substituteNames m (body comb) `setBody` comb

instance Substitutable lore => Substitute (Nesting lore) where
  substituteNames m n =
    n { nestingInputs = map (substituteNames m) $ nestingInputs n }

body :: Combinator lore -> NestBody lore
body (Map _ b) = b
body (Reduce _ b _) = b
body (Scan _ b _) = b
body (Redomap _ _ b _) = b

setBody :: NestBody lore -> Combinator lore -> Combinator lore
setBody b (Map cs _) = Map cs b
setBody b (Reduce cs _ es) = Reduce cs b es
setBody b (Scan cs _ es) = Scan cs b es
setBody b (Redomap cs l _ es) = Redomap cs l b es

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
combCertificates (Map     cs _    ) = cs
combCertificates (Reduce  cs _   _) = cs
combCertificates (Scan    cs _   _) = cs
combCertificates (Redomap cs _ _ _) = cs

-- | Sets the certificates used in a 'Combinator'.
setCombCertificates :: Certificates -> Combinator lore -> Combinator lore
setCombCertificates cs (Map     _ bdy    ) = Map    cs bdy
setCombCertificates cs (Reduce  _ bdy acc) = Reduce cs bdy acc
setCombCertificates cs (Scan    _ bdy acc) = Scan   cs bdy acc
setCombCertificates cs (Redomap _ fun bdy acc) = Redomap cs fun bdy acc

typeOf :: SOACNest lore -> [ExtType]
typeOf (SOACNest inps (Map _ b)) =
  staticShapes [ arrayOf t (Shape [outersize]) (uniqueness t)
               | t <- nestBodyReturnType b ]
  where outersize = arraysSize 0 $ map SOAC.inputType inps
typeOf (SOACNest _ (Reduce _ _ accinit)) =
  staticShapes $ map subExpType accinit
typeOf (SOACNest _ (Redomap _ _ _ accinit)) =
  staticShapes $ map subExpType accinit
typeOf (SOACNest inps (Scan _ _ accinit)) =
  staticShapes [ arrayOf t (Shape [outersize]) (uniqueness t)
               | t <- map subExpType accinit ]
  where outersize = arraysSize 0 $ map SOAC.inputType inps

fromExp :: Bindable lore =>
           Exp lore -> Either SOAC.NotSOAC (SOACNest lore)
fromExp = liftM fromSOAC . SOAC.fromExp

toExp :: Bindable lore => SOACNest lore -> Binder lore (Exp lore)
toExp = SOAC.toExp <=< toSOAC

fromSOAC :: Bindable lore => SOAC lore -> SOACNest lore
fromSOAC (SOAC.Map cs l as) =
  SOACNest as $ Map cs $ lambdaToBody l
fromSOAC (SOAC.Reduce cs l args) =
  SOACNest (map snd args) $
  Reduce cs (lambdaToBody l) (map fst args)
fromSOAC (SOAC.Scan cs l args) =
  SOACNest (map snd args) $
  Scan cs (lambdaToBody l) (map fst args)
fromSOAC (SOAC.Redomap cs ol l es as) =
  -- Never nested, because we need a way to test alpha-equivalence of
  -- the outer combining function.
  SOACNest as $ Redomap cs ol (lambdaToBody l) es

nested :: Bindable lore => Lambda lore -> Maybe (Combinator lore, Nesting lore)
nested l
  | Body _ [Let pat _ e] res <- lambdaBody l, -- Is a let-binding...
    Right soac <- fromSOAC <$> SOAC.fromExp e, -- ...the bindee is a SOAC...
    resultSubExps res == map Var (patternIdents pat) =
      Just (operation soac,
            Nesting { nestingParamNames = map identName $ lambdaParams l
                    , nestingInputs = inputs soac
                    , nestingResult = patternNames pat
                    , nestingReturnType = lambdaReturnType l
                    })
  | otherwise = Nothing

toSOAC :: (Bindable lore, MonadFreshNames m) => SOACNest lore -> m (SOAC lore)
toSOAC (SOACNest as (Map cs b)) =
  SOAC.Map cs <$>
  bodyToLambda (map SOAC.inputRowType as) b <*>
  pure as
toSOAC (SOACNest as (Reduce cs b es)) =
  SOAC.Reduce cs <$>
  bodyToLambda (map subExpType es ++ map SOAC.inputRowType as) b <*>
  pure (zip es as)
toSOAC (SOACNest as (Scan cs b es)) =
  SOAC.Scan cs <$>
  bodyToLambda (map subExpType es ++ map SOAC.inputRowType as) b <*>
  pure (zip es as)
toSOAC (SOACNest as (Redomap cs l b es)) =
  SOAC.Redomap cs l <$>
  bodyToLambda (map subExpType es ++ map SOAC.inputRowType as) b <*>
  pure es <*> pure as
