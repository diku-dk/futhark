-- \* Basic utilities for interpreting tuples as records.
module Language.Futhark.Tuple
  ( areTupleFields,
    tupleFields,
    tupleFieldNames,
    sortFields,
  )
where

import Data.Char (isDigit, ord)
import Data.List (sortOn)
import Data.Map qualified as M
import Data.Text qualified as T
import Language.Futhark.Core (Name, nameFromString, nameToText)

-- | Does this record map correspond to a tuple?
areTupleFields :: M.Map Name a -> Maybe [a]
areTupleFields fs =
  let fs' = sortFields fs
   in if (null fs || length fs' > 1)
        && and (zipWith (==) (map fst fs') tupleFieldNames)
        then Just $ map snd fs'
        else Nothing

-- | Construct a record map corresponding to a tuple.
tupleFields :: [a] -> M.Map Name a
tupleFields as = M.fromList $ zip tupleFieldNames as

-- | Increasing field names for a tuple (starts at 0).
tupleFieldNames :: [Name]
tupleFieldNames = map (nameFromString . show) [(0 :: Int) ..]

-- | Sort fields by their name; taking care to sort numeric fields by
-- their numeric value.  This ensures that tuples and tuple-like
-- records match.
sortFields :: M.Map Name a -> [(Name, a)]
sortFields l = map snd $ sortOn fst $ zip (map (fieldish . fst) l') l'
  where
    l' = M.toList l
    onDigit Nothing _ = Nothing
    onDigit (Just d) c
      | isDigit c = Just $ d * 10 + ord c - ord '0'
      | otherwise = Nothing
    fieldish s = maybe (Right s) Left $ T.foldl' onDigit (Just 0) $ nameToText s
