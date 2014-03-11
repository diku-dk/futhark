module L0C.HORepresentation.MapNest
  ( Nesting (..)
  , pureNest
  , MapNest (..)
  , params
  , inputs
  , fromSOACNest
  , toSOACNest
  )
where

import Control.Applicative
import Control.Monad

import Data.List
import Data.Loc
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import L0C.NeedNames
import qualified L0C.HORepresentation.SOAC as SOAC
import L0C.HORepresentation.SOACNest (SOACNest)
import qualified L0C.HORepresentation.SOACNest as Nest
import L0C.Substitute
import L0C.InternalRep
import L0C.MonadFreshNames

data Nesting = Nesting {
    nestingParams     :: [Ident]
  , nestingResult     :: [Ident]
  , nestingReturnType :: [ConstType]
  , nestingPostBody   :: Body
  } deriving (Eq, Ord, Show)

pureNest :: Nesting -> Bool
pureNest nest
  | Result _ es _ <- nestingPostBody nest,
    Just vs       <- vars es =
      vs == nestingResult nest
  | otherwise = False

vars :: [SubExp] -> Maybe [Ident]
vars = mapM varExp
  where varExp (Var k) = Just k
        varExp _       = Nothing

data MapNest = MapNest Certificates Nest.NestBody [Nesting] [SOAC.Input] SrcLoc
               deriving (Show)

params :: MapNest -> [Param]
params (MapNest _ body [] _ _)       = Nest.bodyParams body
params (MapNest _ _    (nest:_) _ _) = map toParam $ nestingParams nest

inputs :: MapNest -> [SOAC.Input]
inputs (MapNest _ _ _ inps _) = inps

fromSOACNest :: SOACNest -> NeedNames (Maybe MapNest)
fromSOACNest = fromSOACNest' HS.empty

fromSOACNest' :: HS.HashSet Ident -> SOACNest -> NeedNames (Maybe MapNest)

fromSOACNest' bound (Nest.SOACNest inps (Nest.Map cs body [] loc)) = do
  lam <- lambdaBody <$> Nest.bodyToLambda body
  let boundUsedInBody = HS.toList $ freeInBody lam `HS.intersection` bound
  newParams <- mapM (newIdent' (++"_wasfree")) boundUsedInBody
  let subst = HM.fromList $ zip (map identName boundUsedInBody) (map identName newParams)
      inps' = map (substituteNames subst) inps ++
              map (SOAC.Input [SOAC.Repeat] . SOAC.Var) boundUsedInBody
      body' =
        case body of
          Nest.NewNest n comb ->
            let n'    = substituteNames subst
                        n { Nest.nestingParams = Nest.nestingParams n' ++ newParams }
                comb' = substituteNames subst comb
            in Nest.NewNest n' comb'
          Nest.Fun l ->
            Nest.Fun l { lambdaBody =
                           substituteNames subst $ lambdaBody l
                       , lambdaParams =
                         lambdaParams l ++ map toParam newParams
                       }
  return $ Just $
         if HM.null subst
         then MapNest cs body [] inps loc
         else MapNest cs body' [] inps' loc

fromSOACNest' bound (Nest.SOACNest inps (Nest.Map cs body (n:ns) loc)) = do
  Just mn@(MapNest cs' body' ns' inps' _) <-
    fromSOACNest' bound' (Nest.SOACNest (Nest.nestingInputs n) (Nest.Map cs body ns loc))
  (ps, inps'') <-
    unzip <$> fixInputs (zip (map toParam $ Nest.nestingParams n) inps)
                        (zip (params mn) inps')
  let n' = Nesting {
             nestingParams     = map fromParam ps
           , nestingResult     = Nest.nestingResult n
           , nestingReturnType = Nest.nestingReturnType n
           , nestingPostBody   = Nest.nestingPostBody n
           }
  return $ Just $ MapNest cs' body' (n':ns') inps'' loc
  where bound' = bound `HS.union` HS.fromList (Nest.nestingParams n)

fromSOACNest' _ _ = return Nothing

toSOACNest :: MapNest -> SOACNest
toSOACNest (MapNest cs body ns inps loc) =
  Nest.SOACNest inps $ Nest.Map cs body ns' loc
  where ns' = map soacNesting ns
        soacNesting nest =
          Nest.Nesting {
                  Nest.nestingParams = nestingParams nest
                , Nest.nestingResult = nestingResult nest
                , Nest.nestingReturnType = nestingReturnType nest
                , Nest.nestingInputs =
                  map SOAC.varInput $ nestingParams nest
                , Nest.nestingPostBody = nestingPostBody nest
                }

fixInputs :: [(Param, SOAC.Input)] -> [(Param, SOAC.Input)]
          -> NeedNames [(Param, SOAC.Input)]
fixInputs ourInps childInps =
  reverse . snd <$> foldM inspect (ourInps, []) childInps
  where
    isParam x (y, _) = identName x == identName y

    findParam remPs v
      | ([ourP], remPs') <- partition (isParam v) remPs = Just (ourP, remPs')
      | otherwise                                       = Nothing

    inspect :: ([(Param, SOAC.Input)], [(Param, SOAC.Input)])
            -> (Param, SOAC.Input)
            -> NeedNames ([(Param, SOAC.Input)], [(Param, SOAC.Input)])
    inspect (remPs, newInps) (_, SOAC.Input [] (SOAC.Var v))
      | Just (ourP, remPs') <- findParam remPs v =
          return (remPs', ourP:newInps)

    inspect (remPs, newInps) (param, inp@(SOAC.Input ts ia)) =
      case ia of
        SOAC.Var v | Just ((p,pInp), remPs') <- findParam remPs v ->
          let pInp' = SOAC.transformRows ts pInp
          in return (remPs',
                     (p { identType = undefined $ rowType $ SOAC.inputType pInp' },
                      pInp')
                     : newInps)
        _ -> do
          newParam <- Ident <$> newNameFromString (baseString (identName param) ++ "_rep")
                            <*> pure (SOAC.inputType inp)
                            <*> pure (srclocOf inp)
          return (remPs, (undefined newParam, SOAC.Input (ts++[SOAC.Repeat]) ia) : newInps)
