module Futhark.Analysis.Refinement.Convert where

import Control.Monad.RWS
import Data.Bifunctor
import Data.List (find)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Debug.Trace
import Futhark.Analysis.Refinement.CNF
import Futhark.Analysis.Refinement.Monad
import Futhark.Analysis.Refinement.Representation
import Futhark.MonadFreshNames
import Futhark.SoP.Refine qualified as SoP
import Futhark.SoP.SoP qualified as SoP
import Futhark.Util.Pretty
import Language.Futhark qualified as E
import Language.Futhark.Primitive qualified as EP

toExp :: (Monad m) => E.Exp -> RefineT m (Maybe Exp)
toExp (E.Attr _ e _) = toExp e
toExp e@(E.AppExp (E.BinOp (op, _) _ (e_x, _) (e_y, _) _) _)
  | E.baseTag (E.qualLeaf op) <= E.maxIntrinsicTag,
    name <- E.baseString $ E.qualLeaf op,
    Just bop <- find ((name ==) . prettyString) [minBound .. maxBound :: E.BinOp] = do
      x <- toExp e_x
      y <- toExp e_y
      case bop of
        E.Plus -> pure $ (~+~) <$> x <*> y
        E.Times -> pure $ (~*~) <$> x <*> y
        E.Minus -> pure $ (~-~) <$> x <*> y
        _ -> error $ show bop
--   | otherwise = do
--       x <- toExp e_x
--       y <- toExp e_y
--       case E.baseString (E.qualLeaf op) of
--         "<+>" -> pure $ (Union . Set . S.fromList) <$> sequence [x, y]
--         "union" -> pure $ (Union . Set . S.fromList) <$> sequence [x, y]
--         "++" -> pure $ Concat <$> x <*> y
--         "without" -> pure $ Without <$> x <*> y
--         s -> error $ s
toExp (E.ArrayLit es _ _) = do
  es' <- mapM toExp es
  pure $ Array <$> sequence es'
toExp (E.Var (E.QualName qs x) _ _) =
  pure $ Just $ Var x
toExp e@(E.AppExp (E.Apply f args _) _)
  | f `isFun` "length",
    [(_, xs)] <- NE.toList args = do
      xs' <- toExp xs
      pure $ Len <$> xs'
  | f `isFun` "elems",
    [(_, n)] <- NE.toList args = do
      n' <- toExp n
      pure $ Elems <$> n'
  | f `isFun` "toSet",
    [(_, n)] <- NE.toList args = do
      n' <- toExp n
      case n' of
        Just (Array es) -> pure $ Just $ Set $ S.fromList es
        _ -> error $ show n'
  | f `isFun` "bool",
    [(_, n)] <- NE.toList args = do
      n' <- toExp n
      pure $ BoolToInt <$> n'
  --   | f `isFun` "union",
  --     [(_, x), (_, y)] <- NE.toList args = do
  --       x' <- (fmap . fmap) termToSet $ toExp x
  --       y' <- (fmap . fmap) termToSet $ toExp y
  --       pure $ (Union . Set . S.fromList) <$> sequence [x', y']
  --   | f `isFun` "sum",
  --     [(_, x)] <- NE.toList args = do
  --       x' <- toExp x
  --       i <- newVName "i"
  --       let set =
  --             fmap (\x'' -> intToTerm 0 ... (Len x'' ~-~ intToTerm 1)) x'
  --       pure $ Sigma (Var i) <$> set <*> ((flip Idx (Var i)) <$> x')
  --   | f `isFun` "without",
  --     [(_, x), (_, y)] <- NE.toList args = do
  --       x' <- (fmap . fmap) termToSet $ toExp x
  --       y' <- (fmap . fmap) termToSet $ toExp y
  --       pure $ Without <$> x' <*> y'
  --   | f `isFun` "iota",
  --     [(_, x)] <- NE.toList args = do
  --       x' <- (fmap . fmap) termToSet $ toExp x
  --       pure $ (intToTerm 0 ...) <$> x'
  | f `isFun` "scan", -- TODO: replication of scan_sum rule, consolidate
    [E.OpSection (E.QualName [] vn) _ _, _, xs] <- map ((\x -> fromMaybe x (E.stripExp x)) . snd) $ NE.toList args,
    "+" <- E.baseString vn = do
      xsm <- toExp xs
      case xsm of
        Nothing -> pure Nothing
        Just (Idx xs' r@(Range from step to)) -> do
          i <- newVName "i"
          j <- newVName "j"
          pure $
            Just $
              Unions (Var i) (Range (intToTerm 0) step (Len r ~-~ intToTerm 1)) (CNFTerm cnfTrue) $
                Sigma
                  (Var j)
                  (Range from step (from ~+~ Var i))
                  (Idx xs' $ Var j)
toExp e@(E.AppExp (E.Range from mstep (E.ToInclusive to) _) _) = do
  from' <- toExp from
  to' <- toExp to
  step' <- case mstep of
    Nothing -> pure $ Just $ SoP $ SoP.int2SoP 1
    Just step -> toExp step
  pure $ Range <$> from' <*> step' <*> to'
toExp (E.Parens e _) = toExp e
toExp (E.Literal pv _)
  | E.SignedValue iv <- pv = pure $ Just $ intToTerm $ EP.valueIntegral iv
  | E.UnsignedValue iv <- pv = pure $ Just $ intToTerm $ EP.valueIntegral iv
toExp (E.IntLit x _ _) = pure $ Just $ SoP $ SoP.int2SoP x
toExp (E.Negate (E.IntLit x _ _) _) = pure $ Just $ SoP $ SoP.negSoP $ SoP.int2SoP x
toExp e@(E.AppExp (E.If c t f _) _) = do
  c' <- toCNF c
  t' <- toExp t
  f' <- toExp f
  pure $ If <$> (CNFTerm <$> c') <*> t' <*> f'
toExp (E.AppExp (E.Index xs [E.DimFix i] _) _) = do
  i' <- toExp i
  xs' <- toExp xs
  pure $ Idx <$> xs' <*> i'
toExp (E.AppExp (E.Index xs [E.DimSlice mstart mend mstep] _) _) = do
  start <- maybe (pure $ Just $ intToTerm 0) toExp mstart
  step <- maybe (pure $ Just $ intToTerm 1) toExp mstep
  xs' <- toExp xs
  end <- maybe (pure $ (\xs -> Len xs ~-~ intToTerm 1) <$> xs') toExp mend
  pure $ Idx <$> xs' <*> (Range <$> start <*> step <*> end)
-- toExp exp@(E.Coerce e te _ _) = do
--   -- fix
--   me_dims <- sequence <$> mapM toExp (E.shapeDims $ E.arrayShape $ E.typeOf e)
--   mexp_dims <- sequence <$> mapM toExp (E.shapeDims $ E.arrayShape $ E.typeOf exp)
--   case (me_dims, mexp_dims) of
--     (Just e_dims, Just exp_dims) -> do
--       zipWithM (\x y -> SoP.addRel $ expToSoP x SoP.:==: expToSoP y) exp_dims e_dims
--       modify $ \senv ->
--         senv
--           { known = known senv ++ zipWith (:==) exp_dims e_dims,
--             known_map =
--               M.unionWith (<>) (known_map senv) (M.fromList (zipWith (\(Var x) y -> (x, [Var x :== y])) exp_dims e_dims))
--           }
--       toExp e
--     _ -> pure Nothing
toExp exp@(E.AppExp (E.LetPat sizes p e body _) _)
  | (E.Named x, _, _) <- E.patternParam p = do
      e' <- toExp e
      body' <- toExp body
      case e' of
        Just e'' -> pure $ SoP.substituteOne (x, e'') <$> body'
        _ ->
          -- pure Nothing
          error $ unlines [prettyString e, show e]
toExp e = error $ prettyString e <> "\n" <> show e

-- pure Nothing --

toCNF :: (Monad m) => E.Exp -> RefineT m (Maybe (CNF Prop))
toCNF (E.Parens e _) = toCNF e
toCNF e@(E.AppExp (E.BinOp (op, _) _ (e_x, _) (e_y, _) _) _)
  | E.baseTag (E.qualLeaf op) <= E.maxIntrinsicTag,
    name <- E.baseString $ E.qualLeaf op,
    Just bop <- find ((name ==) . prettyString) [minBound .. maxBound :: E.BinOp] = do
      x <- toCNF e_x
      y <- toCNF e_y
      case bop of
        E.LogAnd -> do
          pure $ (&&&) <$> x <*> y
        E.LogOr -> do
          pure $ (|||) <$> x <*> y
        _ -> (fmap . fmap) atomCNF $ toProp e
toCNF e = (fmap . fmap) atomCNF $ toProp e

toProp :: (Monad m) => E.Exp -> RefineT m (Maybe Prop)
toProp (E.Parens e _) = toProp e
toProp e@(E.AppExp (E.BinOp (op, _) _ (e_x, _) (e_y, _) _) _)
  | E.baseTag (E.qualLeaf op) <= E.maxIntrinsicTag,
    name <- E.baseString $ E.qualLeaf op,
    Just bop <- find ((name ==) . prettyString) [minBound .. maxBound :: E.BinOp] = do
      x <- toExp e_x
      y <- toExp e_y
      case bop of
        -- E.NotEqual -> pure $ (:/=) <$> x <*> y
        E.Greater -> pure $ (:>) <$> x <*> y
        -- E.Less -> pure $ (:<) <$> x <*> y
        -- E.Geq -> pure $ (:>=) <$> x <*> y
        E.Equal -> pure $ (:==) <$> x <*> y
        _ -> error $ show bop
--   | otherwise = do
--       x <- toExp e_x
--       y <- toExp e_y
--       case E.baseString (E.qualLeaf op) of
--         "subseteq" -> pure $ SubsetEq <$> x <*> y
--         "subeq" -> pure $ SubEq <$> x <*> y
--         _ -> error $ show op
toProp e@(E.AppExp (E.Apply f args _) _)
  --   | f `isFun` "axiom",
  --     [(_, p)] <- NE.toList args = do
  --       p' <- toProp p
  --       pure $ Axiom <$> p'
  | f `isFun` "permutationOf",
    [(_, x), (_, y)] <- NE.toList args = do
      x' <- (fmap . fmap) termToSet $ toExp x
      y' <- (fmap . fmap) termToSet $ toExp y
      pure $ PermutationOf <$> x' <*> y'

--   | f `isFun` "subseteq",
--     [(_, x), (_, y)] <- NE.toList args = do
--       x' <- (fmap . fmap) termToSet $ toExp x
--       y' <- (fmap . fmap) termToSet $ toExp y
--       pure $ SubsetEq <$> x' <*> y'
--   | f `isFun` "forall",
--     [(_, xs), (_, E.Lambda [p] body _ _ _)] <- map (second (\e -> fromMaybe e (E.stripExp e))) (NE.toList args),
--     (E.Named x, _, _) <- E.patternParam p = do
--       i <- newVName "i"
--       xs' <- (fmap . fmap) termToSet $ toExp xs
--       pred' <- (fmap . fmap) (SoP.substituteOne (x, Var i)) $ toProp body
--       pure $ ForAll (Var i) <$> xs' <*> pred'
--   | f `isFun` "foreach",
--     [(_, xs), (_, E.Lambda [p] body _ _ _)] <- map (second (\e -> fromMaybe e (E.stripExp e))) (NE.toList args),
--     (E.Named x, _, _) <- E.patternParam p = do
--       i <- newVName "k"
--       xs' <- (fmap . fmap) termToSet $ toExp xs
--       pred' <- (fmap . fmap) (SoP.substituteOne (x, Var i)) $ toProp body
--       pure $ ForEach (Var i) <$> xs' <*> pred'
toProp e = toExp e

-- A total hack
isFun :: E.Exp -> String -> Bool
isFun (E.Var (E.QualName _ vn) _ _) fname = fname == E.baseString vn
isFun _ _ = False
