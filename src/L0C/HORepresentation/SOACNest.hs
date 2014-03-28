module L0C.HORepresentation.SOACNest
  ( SOACNest (..)
  , Combinator (..)
  , nesting
  , setNesting
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
  , fromExp
  , toExp
  , fromSOAC
  , toSOAC
  , inputBindings
  , inputsPerLevel
  )
  where

import Control.Applicative
import Control.Monad

import Data.Loc
import Data.Maybe
import qualified Data.HashSet as HS

import L0C.InternalRep hiding (Map, Reduce, Scan, Filter, Redomap, returnType)
import L0C.MonadFreshNames
import L0C.Tools
import L0C.HORepresentation.SOAC (SOAC)
import qualified L0C.HORepresentation.SOAC as SOAC
import L0C.Substitute

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

data Combinator = Map Certificates NestBody [Nesting] SrcLoc
                | Reduce Certificates NestBody [Nesting] [SubExp] SrcLoc
                | Scan Certificates NestBody [Nesting] [SubExp] SrcLoc
                | Filter Certificates NestBody [Nesting] SubExp SrcLoc
                | Redomap Certificates Lambda NestBody [Nesting] [SubExp] SrcLoc
                 deriving (Show)

instance Located Combinator where
  locOf (Map _ _ _ loc) = locOf loc
  locOf (Reduce _ _ _ _ loc) = locOf loc
  locOf (Scan _ _ _ _ loc) = locOf loc
  locOf (Filter _ _ _ _ loc) = locOf loc
  locOf (Redomap _ _ _ _ _ loc) = locOf loc

instance Substitute Combinator where
  substituteNames m comb =
    substituteNames m (body comb) `setBody`
    (map (substituteNames m) (nesting comb) `setNesting` comb)

instance Substitute Nesting where
  substituteNames m n =
    n { nestingInputs = map (substituteNames m) $ nestingInputs n }

nesting :: Combinator -> [Nesting]
nesting (Map _ _ ls _) = ls
nesting (Reduce _ _ ls _ _) = ls
nesting (Scan _ _ ls _ _) = ls
nesting (Filter _ _ ls _ _) = ls
nesting (Redomap _ _ _ ls _ _) = ls

setNesting :: [Nesting] -> Combinator -> Combinator
setNesting ls (Map cs b _ loc) = Map cs b ls loc
setNesting ls (Reduce cs b _ es loc) = Reduce cs b ls es loc
setNesting ls (Scan cs b _ es loc) = Scan cs b ls es loc
setNesting ls (Filter cs b _ outer_shape loc) = Filter cs b ls outer_shape loc
setNesting ls (Redomap cs l b _ es loc) = Redomap cs l b ls es loc

body :: Combinator -> NestBody
body (Map _ b _ _) = b
body (Reduce _ b _ _ _) = b
body (Scan _ b _ _ _) = b
body (Filter _ b _ _ _) = b
body (Redomap _ _ b _ _ _) = b

setBody :: NestBody -> Combinator -> Combinator
setBody b (Map cs _ ls loc) = Map cs b ls loc
setBody b (Reduce cs _ ls es loc) = Reduce cs b ls es loc
setBody b (Scan cs _ ls es loc) = Scan cs b ls es loc
setBody b (Filter cs _ ls outer_shape loc) = Filter cs b ls outer_shape loc
setBody b (Redomap cs l _ ls es loc) = Redomap cs l b ls es loc

combinatorFirstLoop :: Combinator -> ([Param], [ConstType])
combinatorFirstLoop comb =
  case nesting comb of
      nest:_ -> (map toParam $ nestingParams nest,
                 nestingReturnType nest)
      []     -> case body comb of
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

-- | Returns the certificates used in a SOACNest.
certificates :: SOACNest -> Certificates
certificates (SOACNest _ (Map     cs _     _ _)) = cs
certificates (SOACNest _ (Reduce  cs _   _ _ _)) = cs
certificates (SOACNest _ (Scan    cs _   _ _ _)) = cs
certificates (SOACNest _ (Filter  cs _   _ _ _)) = cs
certificates (SOACNest _ (Redomap cs _ _ _ _ _)) = cs

fromExp :: Exp -> Either SOAC.NotSOAC SOACNest
fromExp = liftM fromSOAC . SOAC.fromExp

toExp :: SOACNest -> Binder Exp
toExp = SOAC.toExp <=< toSOAC

fromSOAC :: SOAC -> SOACNest
fromSOAC (SOAC.Map cs l as loc)
  | Just (Map cs2 l2 ps _, nest) <- nested l =
      SOACNest as $ Map (cs++cs2) l2 (nest:ps) loc
  | otherwise =
      SOACNest as $ Map cs (lambdaToBody l) [] loc
fromSOAC (SOAC.Reduce cs l args loc)
  | Just (Reduce cs2 l2 ps _ _, nest) <- nested l =
      SOACNest (map snd args) $
      Reduce (cs++cs2) l2 (nest:ps) (map fst args) loc
  | otherwise =
      SOACNest (map snd args) $
      Reduce cs (lambdaToBody l) [] (map fst args) loc
fromSOAC (SOAC.Scan cs l args loc)
  | Just (Scan cs2 l2 ps _ _, nest) <- nested l =
      SOACNest (map snd args) $
      Scan (cs++cs2) l2 (nest:ps) (map fst args) loc
  | otherwise =
      SOACNest (map snd args) $
      Scan cs (lambdaToBody l) [] (map fst args) loc
fromSOAC (SOAC.Filter cs l as outer_shape loc)
  | Just (Filter cs2 l2 ps _ _, nest) <- nested l =
      SOACNest as $ Filter (cs++cs2) l2 (nest:ps) outer_shape loc
  | otherwise =
      SOACNest as $ Filter cs (lambdaToBody l) [] outer_shape loc
fromSOAC (SOAC.Redomap cs ol l es as loc) =
  -- Never nested, because we need a way to test alpha-equivalence of
  -- the outer combining function.
  SOACNest as $ Redomap cs ol (lambdaToBody l) [] es loc

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
toSOAC (SOACNest as comb@(Map cs b _ loc)) =
  SOAC.Map cs <$> subLambda b comb <*> pure as <*> pure loc
toSOAC (SOACNest as comb@(Reduce cs b _ es loc)) =
  SOAC.Reduce cs <$> subLambda b comb <*> pure (zip es as) <*> pure loc
toSOAC (SOACNest as comb@(Scan cs b _ es loc)) =
  SOAC.Scan cs <$> subLambda b comb <*> pure (zip es as) <*> pure loc
toSOAC (SOACNest as comb@(Filter cs b _ outer_shape loc)) =
  SOAC.Filter cs <$> subLambda b comb <*> pure as <*> pure outer_shape <*> pure loc
toSOAC (SOACNest as comb@(Redomap cs l b _ es loc)) =
  SOAC.Redomap cs l <$> subLambda b comb <*> pure es <*> pure as <*> pure loc

subLambda :: MonadFreshNames m => NestBody -> Combinator -> m Lambda
subLambda b comb =
  case nesting comb of
    [] -> bodyToLambda b
    (Nesting paramIds inps bndIds postBody retTypes:rest) -> do
      (e,f) <- runBinder' $ SOAC.toExp <=< toSOAC $ SOACNest inps $ rest `setNesting` comb
      return Lambda { lambdaReturnType = retTypes
                    , lambdaBody       = f $ Let bndIds e `insertBinding` postBody
                    , lambdaSrcLoc     = loc
                    , lambdaParams     = map toParam paramIds
                    }
  where loc = srclocOf comb

inputBindings :: SOACNest -> [[Ident]]
inputBindings outernest =
  inputBindings' ps comb
  where ps  = [ inpArr >> Just param
                | (inpArr,param) <- zip inpArrs $ nextInputParams comb]
        inpArrs = map SOAC.inputArray $ inputs outernest
        comb    = operation outernest

inputBindings' :: [Maybe Ident] -> Combinator -> [[Ident]]
inputBindings' ps comb =
  zipWith (++) (map maybeToList ps) $
  case nesting comb of
    [] ->
      case body comb of
        Fun _              ->
          replicate (length ps) []
        NewNest nest comb' ->
          inputBindings' (usedParams ps nest comb') comb'
    nest:nests ->
      let comb' = nests `setNesting` comb
      in inputBindings' (usedParams ps nest comb') comb'

usedParams :: [Maybe Ident] -> Nesting -> Combinator -> [Maybe Ident]
usedParams ps nest comb = nestingInputParam ps nest $ nextInputParams comb

nextInputParams :: Combinator -> [Ident]
nextInputParams comb =
  case nesting comb of
    []        -> case body comb of
                   Fun l          -> map fromParam $ lambdaParams l -- FIXME: remove accumulator params!
                   NewNest nest _ -> nestingParams nest
    nest:_ -> nestingParams nest

nestingInputParam :: [Maybe Ident] -> Nesting -> [Ident] -> [Maybe Ident]
nestingInputParam ps nest nextparams =
  map ((`lookupParamIn` zip inps nextparams)=<<) ps
  where inps = nestingInputs nest
        lookupParamIn = lookup . SOAC.varInput

inputsPerLevel :: SOACNest -> [[SOAC.Input]]
inputsPerLevel = nestedInputs' . operation
  where nestedInputs' comb =
          map nestingInputs (nesting comb) ++
          case body comb of
            Fun _              -> []
            NewNest nest comb' -> nestingInputs nest : nestedInputs' comb'
