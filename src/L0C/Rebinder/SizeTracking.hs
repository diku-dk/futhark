-- |
--
-- Array size tracking for rebinder.
--
module L0C.Rebinder.SizeTracking
  ( ShapeMap
  , ColExps
  , lookup
  , insert )
  where

import Data.List hiding (insert, lookup)
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S

import L0C.L0

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

type ShapeMap = M.Map Ident ShapeBinding

lookup :: Ident -> ShapeMap -> [ColExps]
lookup idd m = {-trace ("lookup\n" ++ textual (identName idd) ++ " " ++ show m) $-} delve S.empty idd
  where
    delve s k | k `S.member` s = blank k
              | otherwise =
                case M.lookup k m of
                  Nothing -> blank k
                  Just (DimSizes colexps) ->
                    map (S.unions . map (recurse (k `S.insert` s)) . S.toList) colexps

    blank k = replicate (arrayDims $ identType k) S.empty

    recurse :: S.Set Ident -> Exp -> ColExps
    recurse s e@(Size _ i (Var k') _) =
      case drop i $ delve s k' of
        ds:_ | not (S.null ds) -> ds
        _    -> S.singleton e
    recurse _ e = S.singleton e

insert :: Ident -> [Exp] -> ShapeMap -> ShapeMap
insert dest es bnds =
  let es' = map inspect es
  in M.insertWith (<>) dest (DimSizes es') bnds
  where inspect (Size _ i (Var k) _)
          | xs:_ <- drop i $ lookup k bnds, not (S.null xs) = xs
        inspect e                              = S.singleton e
