{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Analysis.HORepresentation.SOACNest
  ( SOACNest (..)
  , TypedSubExp (..)
  , inputFromTypedSubExp
  , Combinator (..)
  , body
  , params
  , returnType
  , typeOf
  , NestBody (..)
  , nestBodyIndex
  , setNestBodyIndex
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

import Futhark.Representation.AST hiding (subExpType, typeOf)
import Futhark.Representation.SOACS.SOAC (StreamForm(..), getStreamAccums)
import qualified Futhark.Representation.SOACS.SOAC as Futhark
import Futhark.MonadFreshNames
import Futhark.Binder
import Futhark.Analysis.HORepresentation.SOAC (SOAC)
import qualified Futhark.Analysis.HORepresentation.SOAC as SOAC
import Futhark.Transform.Substitute

-- Current problems:
--
-- * Some "nests" are detected that really are not.  For example,
-- nested reduces that do not use the same accumulator.  Also look at
-- how they deal with their lambda params.  Also, the inputs to a
-- nested loop should not be used inside the body, or it's not a
-- proper nest!  (I think...)

data Nesting lore = Nesting {
    nestingIndex      :: VName
  , nestingParamNames :: [VName]
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

nestBodyIndex :: NestBody lore -> VName
nestBodyIndex (Fun lam) = lambdaIndex lam
nestBodyIndex (NewNest nesting _) = nestingIndex nesting

setNestBodyIndex :: VName -> NestBody lore -> NestBody lore
setNestBodyIndex index (Fun lam) =
  Fun lam { lambdaIndex = index }
setNestBodyIndex index (NewNest nesting comb) =
  NewNest nesting { nestingIndex = index } comb

nestBodyParams :: Annotations lore => NestBody lore -> [Ident]
nestBodyParams (Fun lam) =
  map paramIdent $ lambdaParams lam
nestBodyParams (NewNest nesting (Reduce _ _ _ _ acc)) =
  foldNestParams nesting $ map subExpType acc
nestBodyParams (NewNest nesting (Scan _ _ _ acc)) =
  foldNestParams nesting $ map subExpType acc
nestBodyParams (NewNest nesting (Redomap _ _ _ _ _ acc)) =
  foldNestParams nesting $ map subExpType acc
nestBodyParams (NewNest nesting _) =
  foldNestParams nesting []

foldNestParams :: Nesting lore -> [Type] -> [Ident]
foldNestParams nesting acc =
  zipWith Ident (nestingParamNames nesting) $
  acc ++
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

bodyToLambda :: (Bindable lore, MonadFreshNames m, LocalScope lore m,
                 Op lore ~ Futhark.SOAC lore) =>
                [Type] -> NestBody lore -> m (Lambda lore)
bodyToLambda _ (Fun l) = return l
bodyToLambda pts (NewNest (Nesting i ps inps bndIds retTypes) op) =
  localScope (scopeOfLParams lparams) $ do
    (e,bnds) <- runBinder $ SOAC.toExp =<< toSOAC (SOACNest inps op)
    bnd <- mkLetNames' bndIds e
    return
      Lambda { lambdaIndex = i
             , lambdaParams = lparams
             , lambdaReturnType = retTypes
             , lambdaBody = mkBody (bnds++[bnd]) $
                            map Var bndIds
             }
  where lparams = [ Param p t | (p, t) <- zip ps pts ]

lambdaToBody :: (LocalScope lore m, Bindable lore,
                Op lore ~ Futhark.SOAC lore) =>
                Lambda lore -> m (NestBody lore)
lambdaToBody l =
  localScope (scopeOfLParams $ lambdaParams l) $
  maybe (Fun l) (uncurry $ flip NewNest) <$> nested l

data TypedSubExp = TypedSubExp { subExpExp :: SubExp
                               , subExpType :: Type
                               }
                   deriving (Show)

inputFromTypedSubExp :: TypedSubExp -> Maybe SOAC.Input
inputFromTypedSubExp (TypedSubExp (Var v) t) = Just $ SOAC.identInput $ Ident v t
inputFromTypedSubExp _                       = Nothing

data Combinator lore = Map Certificates SubExp (NestBody lore)
                     | Reduce Certificates SubExp Commutativity (NestBody lore) [TypedSubExp]
                     | Scan Certificates SubExp (NestBody lore) [TypedSubExp]
                     | Redomap Certificates SubExp Commutativity (Lambda lore) (NestBody lore) [TypedSubExp]
                     | Stream  Certificates SubExp (StreamFormN lore) (Lambda lore)
                      -- Cosmin: I think it might be helpful to make Stream part of a
                      --         nest, although the stream might not be parallel: that
                      --         is because it might enable, for example tiling.
                 deriving (Show)

data StreamFormN lore  = MapLikeN StreamOrd
                       | RedLikeN StreamOrd Commutativity (LambdaT lore) [TypedSubExp]
                       | SequentN [TypedSubExp]
                            deriving (Show)

getStreamAccumsN :: StreamFormN lore -> [TypedSubExp]
getStreamAccumsN (MapLikeN _)          = []
getStreamAccumsN (RedLikeN _ _ _ accs) = accs
getStreamAccumsN (SequentN       accs) = accs


instance Substitutable lore => Substitute (Combinator lore) where
  substituteNames m (Map cs w b) =
    Map cs (substituteNames m w) $ substituteNames m b
  substituteNames m (Reduce cs w comm b ses) =
    Reduce cs (substituteNames m w) comm (substituteNames m b) ses
  substituteNames m (Scan cs w b ses) =
    Scan cs (substituteNames m w) (substituteNames m b) ses
  substituteNames m (Redomap cs w comm l b ses) =
    Redomap cs (substituteNames m w) comm (substituteNames m l) (substituteNames m b) ses
  substituteNames m (Stream cs w (RedLikeN o comm l0 acc) l) =
    Stream cs (substituteNames m w) (RedLikeN o comm (substituteNames m l0) acc) (substituteNames m l)
  substituteNames m (Stream cs w f l) =
    Stream cs (substituteNames m w) f (substituteNames m l)

instance Substitutable lore => Substitute (Nesting lore) where
  substituteNames m n =
    n { nestingInputs = map (substituteNames m) $ nestingInputs n }

body :: Combinator lore -> NestBody lore
body (Map _ _ b) = b
body (Reduce _ _ _ b _) = b
body (Scan _ _ b _) = b
body (Redomap _ _ _ _ b _) = b
body (Stream _ _ _ l) = Fun l

combinatorFirstLoop :: Annotations lore =>
                       Combinator lore -> ([Ident], [Type])
combinatorFirstLoop comb =
  (nestBodyParams $ body comb,
   case body comb of
     Fun l          -> lambdaReturnType l
     NewNest nest _ -> nestingReturnType nest)

params :: Annotations lore =>
          Combinator lore -> [Ident]
params = fst . combinatorFirstLoop

returnType :: Annotations lore =>
              Combinator lore -> [Type]
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
combCertificates (Map     cs _   _    ) = cs
combCertificates (Reduce  cs _ _ _   _) = cs
combCertificates (Scan    cs _   _   _) = cs
combCertificates (Redomap cs _ _ _ _ _) = cs
combCertificates (Stream  cs _   _ _  ) = cs

-- | Sets the certificates used in a 'Combinator'.
setCombCertificates :: Certificates -> Combinator lore -> Combinator lore
setCombCertificates cs (Map     _ w bdy    ) = Map cs w bdy
setCombCertificates cs (Reduce  _ w comm bdy acc) = Reduce cs w comm bdy acc
setCombCertificates cs (Scan    _ w bdy acc) = Scan cs w bdy acc
setCombCertificates cs (Redomap _ w comm fun bdy acc) = Redomap cs w comm fun bdy acc
setCombCertificates cs (Stream  _ w f bdy) = Stream cs w f bdy

typeOf :: Annotations lore =>
          SOACNest lore -> [ExtType]
typeOf (SOACNest _ (Map _ w b)) =
  staticShapes $ map (`arrayOfRow` w) $ nestBodyReturnType b
typeOf (SOACNest _ (Reduce _ _ _ _ accinit)) =
  staticShapes $ map subExpType accinit
typeOf (SOACNest _ (Scan _ w _ accinit)) =
  staticShapes $ map ((`arrayOfRow` w) . subExpType) accinit
typeOf (SOACNest _ (Redomap _ w _ _ b nes)) =
  let allrtps = nestBodyReturnType b
      accrtps = take (length nes) allrtps
      arrrtps = map (`arrayOfRow` w) $ drop (length nes) allrtps
  in  staticShapes $ accrtps ++ arrrtps
typeOf (SOACNest _ (Stream  _ w form lam)) =
  let nes     = getStreamAccumsN form
      allrtps = lambdaReturnType lam
      accrtps = take (length nes) allrtps
      arrrtps = map (`arrayOfRow` w) $ drop (length nes) allrtps
  in  staticShapes $ accrtps ++ arrrtps

fromExp :: (Bindable lore, LocalScope lore f, Monad f,
           Op lore ~ Futhark.SOAC lore) =>
           Exp lore -> f (Either SOAC.NotSOAC (SOACNest lore))
fromExp e = either (return . Left) (fmap Right . fromSOAC) =<< SOAC.fromExp e

toExp :: (Bindable lore, Op lore ~ Futhark.SOAC lore) =>
         SOACNest lore -> Binder lore (Exp lore)
toExp = SOAC.toExp <=< toSOAC

fromSOAC :: (Bindable lore, LocalScope lore m,
             Op lore ~ Futhark.SOAC lore) =>
            SOAC lore -> m (SOACNest lore)
fromSOAC (SOAC.Map cs w l as) =
  SOACNest as <$> Map cs w <$> lambdaToBody l
fromSOAC (SOAC.Reduce cs w comm l args) =
  SOACNest (map snd args) <$>
  (Reduce cs w comm <$> lambdaToBody l <*> pure (accSubExps l $ map fst args))
fromSOAC (SOAC.Scan cs w l args) =
  SOACNest (map snd args) <$>
  (Scan cs w <$> lambdaToBody l <*> pure (accSubExps l $ map fst args))
fromSOAC (SOAC.Redomap cs w comm ol l es as) =
  -- Never nested, because we need a way to test alpha-equivalence of
  -- the outer combining function.
  SOACNest as <$> (Redomap cs w comm ol <$> lambdaToBody l <*>
                   pure (accSubExps l es))
fromSOAC (SOAC.Stream cs w form lam as) = do
  let es  = getStreamAccums form
      tes = zipWith TypedSubExp es $ map paramType $ take (length es) $
                                     drop 1 $ lambdaParams lam
      form' = case form of
                MapLike o           -> MapLikeN o
                Sequential   _      -> SequentN tes
                RedLike o comm l0 _ -> RedLikeN o comm l0 tes
  SOACNest as <$> (Stream cs w <$> return form' <*> return lam)

accSubExps :: Annotations lore =>
              Lambda lore -> [SubExp] -> [TypedSubExp]
accSubExps l args = zipWith TypedSubExp args (map paramType $ lambdaParams l)

nested :: (LocalScope lore m, Monad m, Bindable lore,
          Op lore ~ Futhark.SOAC lore) =>
          Lambda lore -> m (Maybe (Combinator lore, Nesting lore))
nested l
  | Body _ [Let pat _ e] res <- lambdaBody l = do -- Is a let-binding...
    maybesoac <- either (return . Left) (fmap Right . fromSOAC) =<< SOAC.fromExp e
    case maybesoac of
      Right soac -- ...the bindee is a SOAC...
        | res == map Var (patternNames pat) ->
          return $ Just (operation soac,
                         Nesting { nestingIndex = lambdaIndex l
                                 , nestingParamNames = map paramName $ lambdaParams l
                                 , nestingInputs = inputs soac
                                 , nestingResult = patternNames pat
                                 , nestingReturnType = lambdaReturnType l
                                 })
      _ -> pure Nothing
  | otherwise = pure Nothing

toSOAC :: (Bindable lore, MonadFreshNames m, LocalScope lore m,
          Op lore ~ Futhark.SOAC lore) =>
          SOACNest lore -> m (SOAC lore)
toSOAC (SOACNest as (Map cs w b)) =
  SOAC.Map cs w <$>
  bodyToLambda (map SOAC.inputRowType as) b <*>
  pure as
toSOAC (SOACNest as (Reduce cs w comm b es)) =
  SOAC.Reduce cs w comm <$>
  bodyToLambda (map subExpType es ++ map SOAC.inputRowType as) b <*>
  pure (zip (map subExpExp es) as)
toSOAC (SOACNest as (Scan cs w b es)) =
  SOAC.Scan cs w <$>
  bodyToLambda (map subExpType es ++ map SOAC.inputRowType as) b <*>
  pure (zip (map subExpExp es) as)
toSOAC (SOACNest as (Redomap cs w comm l b es)) =
  SOAC.Redomap cs w comm l <$>
  bodyToLambda (map subExpType es ++ map SOAC.inputRowType as) b <*>
  pure (map subExpExp es) <*> pure as
toSOAC (SOACNest as (Stream cs w form lam)) = do
  let tes   = getStreamAccumsN form
      nes   = map subExpExp tes
      form' = case form of
                MapLikeN o           -> MapLike o
                SequentN _           -> Sequential nes
                RedLikeN o comm l0 _ -> RedLike o comm l0 nes
  return $ SOAC.Stream cs w form' lam as
