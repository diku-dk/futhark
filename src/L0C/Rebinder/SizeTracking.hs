-- |
--
-- Array size tracking for rebinder.
--
module L0C.Rebinder.SizeTracking
  ( ShapeMap
  , ColExps
  , lookup
  , insert
  , sizeRelations
  )
  where

import Data.List hiding (insert, lookup)
import Data.Loc
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.Set as S
import qualified Data.HashSet as HS

import L0C.InternalRep
import qualified L0C.HORepresentation.SOAC as SOAC
import L0C.HORepresentation.SOACNest (SOACNest)
import qualified L0C.HORepresentation.SOACNest as Nest

import Prelude hiding (lookup)

type ColExps = S.Set Exp

data ShapeBinding = DimSizes [ColExps]
                    deriving (Show, Eq)

instance Monoid ShapeBinding where
  mempty = DimSizes []
  DimSizes xs `mappend` DimSizes ys = DimSizes $ merge xs ys
    where merge [] ys' = ys'
          merge xs' [] = xs'
          merge (x:xs') (y:ys') = (x `S.union` y) : merge xs' ys'

type ShapeMap = HM.HashMap Ident ShapeBinding

lookup :: Ident -> ShapeMap -> [ColExps]
lookup idd m = delve HS.empty idd
  where
    delve s k | k `HS.member` s = blank k
              | otherwise =
                case HM.lookup k m of
                  Nothing -> blank k
                  Just (DimSizes colexps) ->
                    map (S.unions . map (recurse (k `HS.insert` s)) . S.toList) colexps

    blank k = replicate (arrayRank $ identType k) S.empty

    recurse :: HS.HashSet Ident -> Exp -> ColExps
    recurse s e@(Size _ i (Var k') _) =
      case drop i $ delve s k' of
        ds:_ | not (S.null ds) -> ds
        _    -> S.singleton e
    recurse _ e = S.singleton e

insert :: Ident -> [Exp] -> ShapeMap -> ShapeMap
insert dest es bnds =
  let es' = map inspect es
  in HM.insertWith (<>) dest (DimSizes es') bnds
  where inspect (Size _ i (Var k) _)
          | xs:_ <- drop i $ lookup k bnds, not (S.null xs) = xs
        inspect e                              = S.singleton e

outputSizeRelations :: [Ident] -> SOACNest -> [[Exp]] -> [(Ident, [Exp])]
outputSizeRelations outputIds nest widths
  | n <- nestingDepth $ Nest.operation nest,
    n > 0 =
      let outputs = zip (map SOAC.varInput outputIds) $ repeat n
      in relationExps (varPairs outputs) widths
  | otherwise = []

nestingDepth :: Nest.Combinator -> Int
nestingDepth comb@(Nest.MapT {}) = length (Nest.nesting comb) + 1
nestingDepth comb@(Nest.ScanT {})
  | Nest.NewNest _ comb2@Nest.MapT {} <- Nest.body comb =
      length (Nest.nesting comb) + length (Nest.nesting comb2) + 2
nestingDepth _ = 0

varPairs :: [(SOAC.Input, Int)] -> [(Ident, Int)]
varPairs = mapMaybe varPair
  where varPair (SOAC.Input [] (SOAC.Var outputId),n) = Just (outputId, n)
        varPair _                                     = Nothing

relationExps :: [(Ident, Int)] -> [[Exp]] -> [(Ident, [Exp])]
relationExps ks widths = map relate ks
  where relate (idd, n) =
          -- FIXME: only uses the first expression for the column.
          (idd, [ e | e:_ <- dropWhile null $ map (filter notThisOne) $ take n widths ])
            where notThisOne (Size _ _ (Var idd2) _) = idd /= idd2
                  notThisOne _                       = True

sizeRelations :: [Ident] -> SOACNest -> [(Ident, [Exp])]
sizeRelations outputIds nest =
  filter (not . null . snd) $
  outputSizeRelations outputIds nest widths ++ relationExps (varPairs depths) widths
  where depths = zip (Nest.inputs nest) $ map length $ Nest.inputBindings nest
        widths = levelSizes nest

levelSizes :: SOACNest -> [[Exp]]
levelSizes nest = merge inputSizes iotaSizes
  where cs = Nest.certificates nest
        loc = srclocOf nest

        merge []     ys     = ys
        merge xs     []     = xs
        merge (x:xs) (y:ys) = (x<>y) : merge xs ys

        inputSizes = transpose $ zipWith mkSizes (Nest.inputs nest) $
                                 Nest.inputBindings nest
          where mkSizes inp bnds
                  | (SOAC.Input [] (SOAC.Var inputArray), idxs)
                    <- SOAC.inputTransposes inp =
                      let numDims = arrayRank $ identType inputArray
                          trns' (k, n) dim = transposeDimension k n dim numDims
                          trns i = foldr trns' i idxs
                      in [ Size cs (trns i) (Var inputArray) loc | i <- [0..length bnds-1] ]
                mkSizes _ _ = []

        iotaSizes = map mkSizes $ Nest.inputs nest : Nest.inputsPerLevel nest
          where mkSizes inps =
                  [ Literal v loc | SOAC.Input [] (SOAC.Iota (Literal v _)) <- inps ]
