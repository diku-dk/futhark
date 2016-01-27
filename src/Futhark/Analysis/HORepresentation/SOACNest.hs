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
nestBodyParams (NewNest nesting (Reduce _ _ _ acc)) =
  foldNestParams nesting $ map subExpType acc
nestBodyParams (NewNest nesting (Scan _ _ acc)) =
  foldNestParams nesting $ map subExpType acc
nestBodyParams (NewNest nesting (Redomap _ _ _ _ acc)) =
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

data Combinator lore = Map Certificates (NestBody lore)
                     | Reduce Certificates Commutativity (NestBody lore) [TypedSubExp]
                     | Scan Certificates (NestBody lore) [TypedSubExp]
                     | Redomap Certificates Commutativity (Lambda lore) (NestBody lore) [TypedSubExp]
                     | Stream  Certificates (StreamFormN lore) (Lambda lore) ChunkIntent
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
  substituteNames m (Map cs b) = Map cs $ substituteNames m b
  substituteNames m (Reduce cs comm b ses) = Reduce cs comm (substituteNames m b) ses
  substituteNames m (Scan cs b ses) = Scan cs (substituteNames m b) ses
  substituteNames m (Redomap cs comm l b ses) =
    Redomap cs comm (substituteNames m l) (substituteNames m b) ses
  substituteNames m (Stream  cs (RedLikeN o comm l0 acc) l ii) =
    Stream cs (RedLikeN o comm (substituteNames m l0) acc) (substituteNames m l) ii
  substituteNames m (Stream  cs f l ii) = Stream cs f (substituteNames m l) ii

instance Substitutable lore => Substitute (Nesting lore) where
  substituteNames m n =
    n { nestingInputs = map (substituteNames m) $ nestingInputs n }

body :: Combinator lore -> NestBody lore
body (Map _ b) = b
body (Reduce _ _ b _) = b
body (Scan _ b _) = b
body (Redomap _ _ _ b _) = b
body (Stream  _ _ l _) = Fun l

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
combCertificates (Map     cs   _    ) = cs
combCertificates (Reduce  cs _ _   _) = cs
combCertificates (Scan    cs   _   _) = cs
combCertificates (Redomap cs _ _ _ _) = cs
combCertificates (Stream  cs   _ _ _) = cs

-- | Sets the certificates used in a 'Combinator'.
setCombCertificates :: Certificates -> Combinator lore -> Combinator lore
setCombCertificates cs (Map     _ bdy    ) = Map cs bdy
setCombCertificates cs (Reduce  _ comm bdy acc) = Reduce cs comm bdy acc
setCombCertificates cs (Scan    _ bdy acc) = Scan cs bdy acc
setCombCertificates cs (Redomap _ comm fun bdy acc) = Redomap cs comm fun bdy acc
setCombCertificates cs (Stream  _ f   bdy ii ) = Stream cs f bdy ii

typeOf :: Annotations lore =>
          SOACNest lore -> [ExtType]
typeOf (SOACNest inps (Map _ b)) =
  staticShapes $ map (`arrayOfRow` outersize) $ nestBodyReturnType b
  where outersize = arraysSize 0 $ map SOAC.inputType inps
typeOf (SOACNest _ (Reduce _ _ _ accinit)) =
  staticShapes $ map subExpType accinit
typeOf (SOACNest inps (Scan _ _ accinit)) =
  staticShapes $ map ((`arrayOfRow` outersize) . subExpType) accinit
  where outersize = arraysSize 0 $ map SOAC.inputType inps
typeOf (SOACNest inps (Redomap _ _ _ b nes)) =
  let allrtps = nestBodyReturnType b
      accrtps = take (length nes) allrtps
      arrrtps = map (`arrayOfRow` outersize) $ drop (length nes) allrtps
  in  staticShapes $ accrtps ++ arrrtps
  where outersize = arraysSize 0 $ map SOAC.inputType inps
typeOf (SOACNest inps (Stream  _ form lam _)) =
  let nes     = getStreamAccumsN form
      allrtps = lambdaReturnType lam
      accrtps = take (length nes) allrtps
      arrrtps = map (`arrayOfRow` outersize) $ drop (length nes) allrtps
  in  staticShapes $ accrtps ++ arrrtps
  where outersize = arraysSize 0 $ map SOAC.inputType inps

fromExp :: (Bindable lore, LocalScope lore f, Monad f,
           Op lore ~ Futhark.SOAC lore) =>
           Exp lore -> f (Either SOAC.NotSOAC (SOACNest lore))
fromExp e = either (return . Left) (liftM Right . fromSOAC) =<< SOAC.fromExp e

toExp :: (Bindable lore, Op lore ~ Futhark.SOAC lore) =>
         SOACNest lore -> Binder lore (Exp lore)
toExp = SOAC.toExp <=< toSOAC

fromSOAC :: (Bindable lore, LocalScope lore m,
             Op lore ~ Futhark.SOAC lore) =>
            SOAC lore -> m (SOACNest lore)
fromSOAC (SOAC.Map cs l as) =
  SOACNest as <$> Map cs <$> lambdaToBody l
fromSOAC (SOAC.Reduce cs comm l args) =
  SOACNest (map snd args) <$>
  (Reduce cs comm <$> lambdaToBody l <*> pure (accSubExps l $ map fst args))
fromSOAC (SOAC.Scan cs l args) =
  SOACNest (map snd args) <$>
  (Scan cs <$> lambdaToBody l <*> pure (accSubExps l $ map fst args))
fromSOAC (SOAC.Redomap cs comm ol l es as) =
  -- Never nested, because we need a way to test alpha-equivalence of
  -- the outer combining function.
  SOACNest as <$> (Redomap cs comm ol <$> lambdaToBody l <*>
                   pure (accSubExps l es))
fromSOAC (SOAC.Stream cs form lam ii as) = do
  let es  = getStreamAccums form
      tes = zipWith TypedSubExp es $ map paramType $ take (length es) $
                                     drop 1 $ lambdaParams lam
      form' = case form of
                MapLike o           -> MapLikeN o
                Sequential   _      -> SequentN tes
                RedLike o comm l0 _ -> RedLikeN o comm l0 tes
  SOACNest as <$> (Stream  cs  <$> return form' <*>
                                   return lam   <*> pure ii)

accSubExps :: Annotations lore =>
              Lambda lore -> [SubExp] -> [TypedSubExp]
accSubExps l args = zipWith TypedSubExp args (map paramType $ lambdaParams l)

nested :: (LocalScope lore m, Monad m, Bindable lore,
          Op lore ~ Futhark.SOAC lore) =>
          Lambda lore -> m (Maybe (Combinator lore, Nesting lore))
nested l
  | Body _ [Let pat _ e] res <- lambdaBody l = do -- Is a let-binding...
    maybesoac <- either (return . Left) (liftM Right . fromSOAC) =<< SOAC.fromExp e
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
toSOAC (SOACNest as (Map cs b)) =
  SOAC.Map cs <$>
  bodyToLambda (map SOAC.inputRowType as) b <*>
  pure as
toSOAC (SOACNest as (Reduce cs comm b es)) =
  SOAC.Reduce cs comm <$>
  bodyToLambda (map subExpType es ++ map SOAC.inputRowType as) b <*>
  pure (zip (map subExpExp es) as)
toSOAC (SOACNest as (Scan cs b es)) =
  SOAC.Scan cs <$>
  bodyToLambda (map subExpType es ++ map SOAC.inputRowType as) b <*>
  pure (zip (map subExpExp es) as)
toSOAC (SOACNest as (Redomap cs comm l b es)) =
  SOAC.Redomap cs comm l <$>
  bodyToLambda (map subExpType es ++ map SOAC.inputRowType as) b <*>
  pure (map subExpExp es) <*> pure as
toSOAC (SOACNest as (Stream  cs form lam ii)) = do
  let tes   = getStreamAccumsN form
      nes   = map subExpExp tes
      form' = case form of
                MapLikeN o           -> MapLike o
                SequentN _           -> Sequential nes
                RedLikeN o comm l0 _ -> RedLike o comm l0 nes
  return $ SOAC.Stream cs form' lam ii as
