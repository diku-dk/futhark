-- | Checking for missing cases in a match expression.  Based on
-- "Warnings for pattern matching" by Luc Maranget.  We only detect
-- inexhaustiveness here - ideally, we would also like to check for
-- redundant cases.
module Language.Futhark.TypeChecker.Match
  ( unmatched,
    Match,
  )
where

import Data.Bifunctor (first)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Util (maybeHead, nubOrd)
import Futhark.Util.Pretty hiding (group, space)
import Language.Futhark hiding (ExpBase (Constr))

data Constr
  = Constr Name
  | ConstrTuple
  | ConstrRecord [Name]
  | -- | Treated as 0-ary.
    ConstrLit PatLit
  deriving (Eq, Ord, Show)

-- | A representation of the essentials of a pattern.
data Match t
  = MatchWild t
  | MatchConstr Constr [Match t] t
  deriving (Eq, Ord, Show)

matchType :: Match StructType -> StructType
matchType (MatchWild t) = t
matchType (MatchConstr _ _ t) = t

pprMatch :: Int -> Match t -> Doc a
pprMatch _ MatchWild {} = "_"
pprMatch _ (MatchConstr (ConstrLit l) _ _) = pretty l
pprMatch p (MatchConstr (Constr c) ps _) =
  parensIf (not (null ps) && p >= 10) $
    "#" <> pretty c <> mconcat (map ((" " <>) . pprMatch 10) ps)
pprMatch _ (MatchConstr ConstrTuple ps _) =
  parens $ commasep $ map (pprMatch (-1)) ps
pprMatch _ (MatchConstr (ConstrRecord fs) ps _) =
  braces $ commasep $ zipWith ppField fs ps
  where
    ppField name t = pretty (nameToString name) <> equals <> pprMatch (-1) t

instance Pretty (Match t) where
  pretty = pprMatch (-1)

patternToMatch :: Pat StructType -> Match StructType
patternToMatch (Id _ (Info t) _) = MatchWild t
patternToMatch (Wildcard (Info t) _) = MatchWild t
patternToMatch (PatParens p _) = patternToMatch p
patternToMatch (PatAttr _ p _) = patternToMatch p
patternToMatch (PatAscription p _ _) = patternToMatch p
patternToMatch (PatLit l (Info t) _) =
  MatchConstr (ConstrLit l) [] t
patternToMatch p@(TuplePat ps _) =
  MatchConstr ConstrTuple (map patternToMatch ps) $
    patternStructType p
patternToMatch p@(RecordPat fs _) =
  MatchConstr (ConstrRecord fnames) (map patternToMatch ps) $
    patternStructType p
  where
    (fnames, ps) = unzip $ sortFields $ M.fromList $ map (first unLoc) fs
patternToMatch (PatConstr c (Info t) args _) =
  MatchConstr (Constr c) (map patternToMatch args) t

isConstr :: Match t -> Maybe Name
isConstr (MatchConstr (Constr c) _ _) = Just c
isConstr _ = Nothing

isBool :: Match t -> Maybe Bool
isBool (MatchConstr (ConstrLit (PatLitPrim (BoolValue b))) _ _) = Just b
isBool _ = Nothing

complete :: [Match StructType] -> Bool
complete xs
  | Just x <- maybeHead xs,
    Scalar (Sum all_cs) <- matchType x,
    Just xs_cs <- mapM isConstr xs =
      all (`elem` xs_cs) (M.keys all_cs)
  | otherwise =
      all (`elem` fromMaybe [] (mapM isBool xs)) [True, False]
        || all isRecord xs
        || all isTuple xs
  where
    isRecord (MatchConstr ConstrRecord {} _ _) = True
    isRecord _ = False
    isTuple (MatchConstr ConstrTuple _ _) = True
    isTuple _ = False

specialise ::
  [StructType] ->
  Match StructType ->
  [[Match StructType]] ->
  [[Match StructType]]
specialise ats c1 = go
  where
    go ((c2 : row) : ps)
      | Just args <- match c1 c2 =
          (args ++ row) : go ps
      | otherwise =
          go ps
    go _ = []

    match (MatchConstr c1' _ _) (MatchConstr c2' args _)
      | c1' == c2' =
          Just args
      | otherwise =
          Nothing
    match _ MatchWild {} =
      Just $ map MatchWild ats
    match _ _ =
      Nothing

defaultMat :: [[Match t]] -> [[Match t]]
defaultMat = mapMaybe onRow
  where
    onRow (MatchConstr {} : _) = Nothing
    onRow (MatchWild {} : ps) = Just ps
    onRow [] = Nothing -- Should not happen.

findUnmatched :: [[Match StructType]] -> Int -> [[Match ()]]
findUnmatched pmat n
  | ((p : _) : _) <- pmat,
    Just heads <- mapM maybeHead pmat =
      if complete heads
        then completeCase heads
        else incompleteCase (matchType p) heads
  where
    completeCase cs = do
      c <- cs
      let ats = case c of
            MatchConstr _ args _ -> map matchType args
            MatchWild _ -> []
          a_k = length ats
          pmat' = specialise ats c pmat
      u <- findUnmatched pmat' (a_k + n - 1)
      pure $ case c of
        MatchConstr c' _ _ ->
          let (r, p) = splitAt a_k u
           in MatchConstr c' r () : p
        MatchWild _ ->
          MatchWild () : u

    incompleteCase pt cs = do
      u <- findUnmatched (defaultMat pmat) (n - 1)
      if null cs
        then pure $ MatchWild () : u
        else case pt of
          Scalar (Sum all_cs) -> do
            -- Figure out which constructors are missing.
            let sigma = mapMaybe isConstr cs
                notCovered (k, _) = k `notElem` sigma
            (cname, ts) <- filter notCovered $ M.toList all_cs
            pure $ MatchConstr (Constr cname) (map (const (MatchWild ())) ts) () : u
          Scalar (Prim Bool) -> do
            -- Figure out which constants are missing.
            let sigma = mapMaybe isBool cs
            b <- filter (`notElem` sigma) [True, False]
            pure $ MatchConstr (ConstrLit (PatLitPrim (BoolValue b))) [] () : u
          _ -> do
            -- FIXME: this is wrong in the unlikely case where someone
            -- is pattern-matching every single possible number for
            -- some numeric type.  It should be handled more like Bool
            -- above.
            pure $ MatchWild () : u
findUnmatched [] n = [replicate n $ MatchWild ()]
findUnmatched _ _ = []

{-# NOINLINE unmatched #-}

-- | Find the unmatched cases.
unmatched :: [Pat StructType] -> [Match ()]
unmatched orig_ps =
  -- The algorithm may find duplicate example, which we filter away
  -- here.
  nubOrd $
    mapMaybe maybeHead $
      findUnmatched (map (L.singleton . patternToMatch) orig_ps) 1
