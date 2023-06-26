module Futhark.Analysis.Refinement.Rules where

import Control.Applicative
import Control.Monad.RWS
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.Analysis.Refinement.CNF
import Futhark.Analysis.Refinement.Convert
import Futhark.Analysis.Refinement.Match
import Futhark.Analysis.Refinement.Monad
import Futhark.Analysis.Refinement.Relations
import Futhark.Analysis.Refinement.Representation
import Futhark.MonadFreshNames
import Futhark.SoP.Refine
import Futhark.SoP.SoP qualified as SoP
import Futhark.SoP.Util
import Language.Futhark qualified as E

withType :: (Monad m) => E.VName -> (E.PatType -> RefineT m (Maybe a)) -> RefineT m (Maybe a)
withType x f = do
  me <- lookupVName x
  mt <- lookupType x
  case (me, mt) of
    (Just e, _) -> f $ E.typeOf e
    (_, Just t) -> f t
    _ -> pure Nothing

data Rule = Rule
  { from :: Term,
    to :: Subst -> CNFM (Maybe Term)
  }

nope :: CNFM (Maybe a)
nope = pure Nothing

yep :: a -> CNFM (Maybe a)
yep = pure . Just

yepM :: CNFM a -> CNFM (Maybe a)
yepM = fmap Just

rules :: CNFM [(String, Rule)]
rules = do
  h1 <- mkHole
  h2 <- mkHole
  h3 <- mkHole
  h4 <- mkHole
  h5 <- mkHole
  h6 <- mkHole
  h7 <- mkHole
  ctx1 <- mkCHole
  ctx2 <- mkCHole
  let withCtx f subst =
        (fmap . fmap) (replace subst . ctx1) $ f subst
      withCtxAndSubst f subst = withCtx f subst @ subst
      anywhere = ctx1
  pure $
    [ ( "eq",
        Rule
          { from = h1 :== h2,
            to = \s ->
              if (replace s h1 == replace s h2)
                then yep $ Bool True
                else nope
          }
      ),
      ( "permutation_of",
        Rule
          { from = PermutationOf h1 h2,
            to = \s ->
              yepM $
                addGoals
                  [ pure $ (Len h1 :== Len h2) @ s,
                    pure $ (Elems h1 :== Elems h2) @ s
                  ]
          }
      ),
      ( "len",
        Rule
          { from = anywhere $ Len h1,
            to = withCtxAndSubst $ \s ->
              case h1 @ s of
                Var u -> do
                  withType u $ \t ->
                    toExp $ head $ E.shapeDims $ E.arrayShape t
                Set xs -> yep $ intToTerm $ fromIntegral $ length xs
                Array xs -> yep $ intToTerm $ fromIntegral $ length xs
                _ -> nope
          }
      ),
      ( "len_range_unit_step",
        Rule
          { from = anywhere $ Len (Range h1 (intToTerm 1) h2),
            to = withCtxAndSubst $ \_ ->
              yep $ h2 ~-~ h1 ~+~ intToTerm 1
          }
      ),
      ( "elems_range",
        Rule
          { from = anywhere $ Elems (Range h1 h2 h3),
            to = withCtx $ \s ->
              pure $ Just $ Range h1 h2 h3 @ s
          }
      ),
      ( "elems_union",
        Rule
          { from = anywhere $ Elems h1,
            to =
              withCtx $ \s -> do
                i <- newVName "i"
                pure $
                  Just $
                    Unions
                      (Var i)
                      (intToTerm 0 ... Len h1 @ s ~-~ intToTerm 1)
                      (CNFTerm cnfTrue)
                      (Idx h1 @ s $ Var i)
          }
      ),
      ( "map_index",
        Rule
          { from = anywhere $ Idx h1 h2,
            to =
              withCtxAndSubst $ \s ->
                case h1 @ s of
                  Var arr' -> do
                    withBinding arr' $ \e -> do
                      case e of
                        E.AppExp (E.Apply f args _) _
                          | Just fname <- getFun f,
                            "map" `L.isPrefixOf` fname,
                            E.Lambda params body _ _ _ : args' <- map ((\x -> fromMaybe x (E.stripExp x)) . snd) $ NE.toList args -> do
                              let ps = map (\p -> let (E.Named x, _, _) = E.patternParam p in x) params
                              body' <- toExp body
                              argsm'' <- sequence <$> mapM toExp args'
                              case argsm'' of
                                Nothing -> pure Nothing
                                Just args'' ->
                                  let subst = M.fromList $ zip ps (map (\arg -> Idx arg h2) args'')
                                   in pure $ fmap (SoP.substitute subst) body'
                        _ -> pure Nothing
                  _ -> pure Nothing
          }
      ),
      ( "union_if",
        Rule
          { from = anywhere $ Unions h1 h2 h3 (If h4 h5 h6),
            to =
              withCtxAndSubst $ \s ->
                case (h3 @ s, h4 @ s) of
                  (CNFTerm cond, CNFTerm b) ->
                    pure $
                      Just $
                        Unions h1 h2 (CNFTerm $ cond &&& b) h5
                          `Union` Unions h1 h2 (CNFTerm $ cond &&& negateCNF negateProp b) h6
                  _ -> pure Nothing
          }
      ),
      ( "combine_if_sop",
        Rule
          { from = anywhere $ h1 ~+~ (If h2 h3 h4),
            to = withCtxAndSubst $ \_ ->
              pure $ Just $ If h2 (h1 ~+~ h3) (h1 ~+~ h4)
          }
      ),
      ( "split_on_if",
        Rule
          { from =
              ctx1 $ If h1 h2 h3,
            to = \s ->
              if (not . S.null $ fv (h1 @ s) `S.intersection` fv (ctx1 (Bool True) @ s))
                then pure Nothing
                else case (h1 @ s) of
                  CNFTerm c ->
                    Just
                      <$> addGoals
                        [ addInfo c >> (pure $ ctx1 $ h2 @ s),
                          addInfo (negateCNF (negateProp) c)
                            >> (pure $ ctx1 $ h3 @ s)
                        ]
                  _ -> pure Nothing
          }
      ),
      ( "scan_sum",
        Rule
          { from = anywhere $ Idx h1 h2,
            to =
              withCtxAndSubst $ \s ->
                case (h1 @ s, h2 @ s) of
                  (Var arr', i) -> do
                    withBinding arr' $ \e -> do
                      case e of
                        E.AppExp (E.Apply f args _) _
                          | Just "scan" <- getFun f,
                            [E.OpSection (E.QualName [] vn) _ _, _, xs] <- map ((\x -> fromMaybe x (E.stripExp x)) . snd) $ NE.toList args,
                            "+" <- E.baseString vn -> do
                              xsm <- toExp xs
                              case xsm of
                                Nothing -> pure Nothing
                                Just xs' -> do
                                  k <- newVName "j"
                                  yep $
                                    Sigma
                                      (Var k)
                                      (intToTerm 0 ... i)
                                      (Idx xs' $ Var k)
                        _ -> nope
                  _ -> nope
          }
      ),
      ( "union_sigma_bool",
        Rule
          { from =
              ctx1 $
                Unions h1 h2 h3 $
                  ctx2 $
                    Sigma h4 h5 h6,
            to = \s ->
              do
                let [i, range, conds, j, jset, e] = [h1, h2, h3, h4, h5, h6] @ s
                case (conds, range, e, jset) of
                  (CNFTerm conds', Range from step to, BoolToInt (Idx cs j'), Range jstart jstep jend)
                    | [[cs']] <- cnfToLists conds' ->
                        ifM
                          ( andM
                              [ jend ^==^ i,
                                jstart ^==^ intToTerm 0,
                                jstep ^==^ intToTerm 1,
                                from ^==^ intToTerm 0,
                                step ^==^ intToTerm 1,
                                pure $ constCtx (ctx2 @ s),
                                j ^==^ j',
                                Idx cs i ^==^ cs'
                              ]
                          )
                          ( yep $
                              ( ctx1 $
                                  Range
                                    (ctx2 $ intToTerm 1)
                                    (intToTerm 1)
                                    ( ctx2 $
                                        Sigma
                                          j
                                          (Range jstart jstep to)
                                          e
                                    )
                              )
                                @ s
                          )
                          nope
                  _ -> nope
          }
      ),
      ( "var",
        Rule
          { from = anywhere $ h1,
            to =
              withCtxAndSubst $ \s ->
                case h1 @ s of
                  Var x ->
                    withBinding x $ \e ->
                      toExp e
                  _ -> nope
          }
      ),
      ( "split_sigma",
        Rule
          { from = anywhere $ Sigma h1 h2 h3,
            to = withCtxAndSubst $
              \s -> do
                let [i, set, e] = [h1, h2, h3] @ s
                case e of
                  SoP sop
                    | isNothing $ SoP.justSingleTerm sop ->
                        let sums = map (Sigma i set . SoP . uncurry SoP.term2SoP) $ SoP.sopToLists sop
                         in yep $ foldl1 (~+~) sums
                  _ -> nope
          }
      ),
      ( "const_sigma",
        Rule
          { from = anywhere $ Sigma h1 h2 h3,
            to = withCtxAndSubst $
              \s -> do
                let [_, set, e] = [h1, h2, h3] @ s
                case SoP.justConstant $ termToSoP e of
                  Just c ->
                    pure $ (intToTerm c ~*~) <$> setSize set
                  _ -> nope
          }
      ),
      ( "mul_sigma",
        Rule
          { from = anywhere $ Sigma h1 h2 h3,
            to = withCtxAndSubst $
              \s -> do
                let [i, set, e] = [h1, h2, h3] @ s
                case SoP.justSingleTerm $ termToSoP e of
                  Just (t, n)
                    | n /= 1 ->
                        pure $ Just $ intToTerm n ~*~ Sigma i set (SoP $ SoP.term2SoP t 1)
                  _ -> pure Nothing
          }
      ),
      ( "basic_combine_sigma",
        Rule
          { from =
              anywhere $
                Sigma h1 h2 h3 ~-~ Sigma h4 h5 h6,
            to = withCtxAndSubst $
              \s -> do
                let [Var x_i, x_set, x_e, Var y_i, y_set, y_e] =
                      [h1, h2, h3, h4, h5, h6] @ s
                case (x_set, y_set) of
                  (Range x_start x_step x_end, Range y_start y_step y_end) ->
                    ifM
                      ( andM
                          [ x_e ^==^ SoP.substituteOne (y_i, Var x_i) y_e,
                            x_start ^==^ y_start,
                            x_step ^==^ y_step
                          ]
                      )
                      ( yep $
                          ( Sigma (Var x_i) ((y_end ~+~ intToTerm 1) ... x_end) x_e
                          )
                      )
                      nope
                  _ -> nope
          }
      ),
      -- FIX: Needs to actually check the condition on the union
      -- for compatability with the rule.
      ( "i_+_sigma_bool_to_int",
        Rule
          { from =
              ctx1 $
                Unions h1 h2 h3 $
                  ctx2 $
                    h4 ~+~ Sigma h5 h6 h7,
            to =
              \s -> do
                let [u_i, range, cond, vi, y_j, y_set, y_e] = [h1, h2, h3, h4, h5, h6, h7] @ s
                case (cond, flatten vi, range, y_e, y_set) of
                  (CNFTerm cnf, Var i, Range u_min u_step u_end, BoolToInt (Idx arr idx), Range (SoP y_start_sop) y_step y_end)
                    | Just (1, y_start_i, c) <- SoP.justAffine y_start_sop,
                      and
                        [ u_min == intToTerm 0,
                          u_step == intToTerm 1,
                          y_start_i == Var i,
                          u_end == y_end,
                          idx == y_j
                        ] -> do
                        let e_min = Sigma y_j ((SoP.substituteOne (i, intToTerm 0) y_start_i) ... y_end) y_e
                            e_max = y_end
                        yep $
                          (ctx1 $ ctx2 $ (e_min ... e_max)) @ s
                  _ -> nope
          }
      ),
      ( "empty_rset",
        Rule
          { from = ctx1 $ Range h1 h2 h3,
            to =
              \s -> do
                let [from, step, to] = [h1, h2, h3] @ s
                ifM
                  ( localS id $ do
                      mapM_ addToAlgEnv $ knownFromCtx (ctx1 @ s)
                      to ^<^ from
                  )
                  (yep $ (ctx1 $ Set mempty) @ s)
                  nope
          }
      ),
      ( "empty_unions",
        Rule
          { from = anywhere $ Unions h1 h2 h3 h4,
            to = withCtxAndSubst $ \s -> do
              case h2 @ s of
                Set xs
                  | S.null xs -> yep $ Set mempty
                _ -> nope
          }
      ),
      ( "empty_union",
        Rule
          { from = anywhere $ Union h1 h2,
            to = withCtxAndSubst $ \s -> do
              case (h1 @ s, h2 @ s) of
                (Set xs, _)
                  | S.null xs -> yep $ h2
                (_, Set xs)
                  | S.null xs -> yep $ h1
                _ -> nope
          }
      ),
      ( "combine_ranges",
        Rule
          { from = anywhere $ Range h1 h2 h3 `Union` Range h4 h5 h6,
            to = withCtxAndSubst $ \s -> do
              let [from1, step1, to1, from2, step2, to2] = [h1, h2, h3, h4, h5, h6] @ s
              ifM
                ( andM
                    [ from1 ^<=^ from2,
                      from2 ^<=^ (to1 ~+~ intToTerm 1),
                      step1 ^==^ step2
                    ]
                )
                ( yep $ Range from1 step1 to2
                )
                nope
          }
      )
    ]

constCtx :: (Term -> Term) -> Bool
constCtx ctx =
  isJust $ SoP.justConstant $ termToSoP (ctx $ intToTerm 0)

withBinding :: (Monad m) => E.VName -> (E.Exp -> RefineT m (Maybe a)) -> RefineT m (Maybe a)
withBinding x f = do
  me <- lookupVName x
  case me of
    Just e -> f e
    Nothing -> pure Nothing

matchRule :: Rule -> Term -> CNFM (Maybe Term)
matchRule r p = do
  subs <- match (from r) p
  checkMatch subs
  where
    checkMatch [] = pure Nothing
    checkMatch (s : ss) = do
      mt <- to r s
      case mt of
        Just t' -> pure $ Just t'
        Nothing -> checkMatch ss

applyRules :: Term -> CNFM (Maybe (String, Term))
applyRules p = do
  rs <- rules
  applyRules' rs
  where
    applyRules' [] = pure Nothing
    applyRules' ((label, r) : rs) = do
      mp' <- matchRule r p
      case mp' of
        Nothing -> applyRules' rs
        Just p' -> pure $ Just (label, p')

addInfo :: CNF Prop -> CNFM ()
addInfo =
  asum
    . map
      ( \props -> do
          modify (\env -> env {known = known env ++ props})
          mapM_ addToAlgEnv props
      )
    . dnfToLists
    . toDNF

addToAlgEnv :: Term -> CNFM ()
addToAlgEnv (x :> y) = addRel $ termToSoP x SoP.:>: termToSoP y
addToAlgEnv (x :>= y) = addRel $ termToSoP x SoP.:>=: termToSoP y
addToAlgEnv (x :< y) = addRel $ termToSoP x SoP.:<: termToSoP y
addToAlgEnv (x :<= y) = addRel $ termToSoP x SoP.:<=: termToSoP y
addToAlgEnv (Not (x :> y)) = addRel $ termToSoP x SoP.:<=: termToSoP y
addToAlgEnv (Not (x :>= y)) = addRel $ termToSoP x SoP.:<: termToSoP y
addToAlgEnv p = pure ()
