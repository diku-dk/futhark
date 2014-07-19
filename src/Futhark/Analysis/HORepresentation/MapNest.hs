module Futhark.Analysis.HORepresentation.MapNest
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

import Futhark.NeedNames
import qualified Futhark.Analysis.HORepresentation.SOAC as SOAC
import Futhark.Analysis.HORepresentation.SOACNest (SOACNest)
import qualified Futhark.Analysis.HORepresentation.SOACNest as Nest
import Futhark.Substitute
import Futhark.Representation.Basic
import Futhark.MonadFreshNames

data Nesting = Nesting {
    nestingParams     :: [Ident]
  , nestingResult     :: [Ident]
  , nestingReturnType :: [ConstType]
  , nestingPostBody   :: Body
  } deriving (Eq, Ord, Show)

pureNest :: Nesting -> Bool
pureNest nest
  | Body [] (Result _ es _) <- nestingPostBody nest,
    Just vs       <- vars es =
      sort vs == sort (nestingResult nest)
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

fromSOACNest' bound (Nest.SOACNest inps
                     (Nest.Map cs (Nest.NewNest n body@Nest.Map{}) loc)) = do
  Just mn@(MapNest cs' body' ns' inps' _) <-
    fromSOACNest' bound' (Nest.SOACNest (Nest.nestingInputs n) body)
  (ps, inps'') <-
    unzip <$> fixInputs (zip (map toParam $ Nest.nestingParams n) inps)
                        (zip (params mn) inps')
  let n' = Nesting {
             nestingParams     = map fromParam ps
           , nestingResult     = Nest.nestingResult n
           , nestingReturnType = Nest.nestingReturnType n
           , nestingPostBody   = Nest.nestingPostBody n
           }
  return $ Just $ MapNest (cs++cs') body' (n':ns') inps'' loc
  where bound' = bound `HS.union` HS.fromList (Nest.nestingParams n)

fromSOACNest' bound (Nest.SOACNest inps (Nest.Map cs body loc)) = do
  lam <- lambdaBody <$> Nest.bodyToLambda body
  let boundUsedInBody = HS.toList $ freeInBody lam `HS.intersection` bound
  newParams <- mapM (newIdent' (++"_wasfree")) boundUsedInBody
  let subst = HM.fromList $ zip (map identName boundUsedInBody) (map identName newParams)
      size  = arraysSize 0 $ map SOAC.inputType inps
      inps' = map (substituteNames subst) inps ++
              map (SOAC.addTransform (SOAC.Replicate size) . SOAC.varInput) boundUsedInBody
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

fromSOACNest' _ _ = return Nothing

toSOACNest :: MapNest -> SOACNest
toSOACNest (MapNest cs body [] inps loc) =
  Nest.SOACNest inps $ Nest.Map cs body loc
toSOACNest (MapNest cs body (n:ns) inps loc) =
  let Nest.SOACNest _ body' = toSOACNest $ MapNest cs body ns inps loc
  in Nest.SOACNest inps $ Nest.Map cs (Nest.NewNest n' body') loc
  where n' = soacNesting n
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

    ourSize = arraysSize 0 $ map (SOAC.inputType . snd) ourInps

    findParam remPs v
      | ([ourP], remPs') <- partition (isParam v) remPs = Just (ourP, remPs')
      | otherwise                                       = Nothing

    inspect :: ([(Param, SOAC.Input)], [(Param, SOAC.Input)])
            -> (Param, SOAC.Input)
            -> NeedNames ([(Param, SOAC.Input)], [(Param, SOAC.Input)])
    inspect (remPs, newInps) (_, SOAC.Input ts (SOAC.Var v))
      | SOAC.nullTransforms ts,
        Just (ourP, remPs') <- findParam remPs v =
          return (remPs', ourP:newInps)

    inspect (remPs, newInps) (param, inp@(SOAC.Input ts ia)) =
      case ia of
        SOAC.Var v
          | Just ((p,pInp), remPs') <- findParam remPs v ->
          let pInp'  = SOAC.transformRows ts pInp
              pInpt' = SOAC.inputType pInp'
          in return (remPs',
                     (p { identType = rowType pInpt' `setAliases` () },
                      pInp')
                     : newInps)
          | Just ((p,pInp), _) <- findParam newInps v -> do
          -- The input corresponds to a variable that has already
          -- been used.
          p' <- newIdent' id p
          return (remPs, (p', pInp) : newInps)
        _ -> do
          newParam <- Ident <$> newNameFromString (baseString (identName param) ++ "_rep")
                            <*> pure inpt
                            <*> pure (srclocOf ia)
          return (remPs, (toParam newParam,
                          SOAC.Input (ts SOAC.|> SOAC.Replicate ourSize) ia) : newInps)
      where inpt = SOAC.inputType inp
