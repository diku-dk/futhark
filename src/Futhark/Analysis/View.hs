module Futhark.Analysis.View (mkViewProg) where

import Data.List qualified as L
import Data.List.NonEmpty()
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe, fromMaybe)
import Futhark.Analysis.View.Representation
import Futhark.Analysis.View.Refine
import Futhark.MonadFreshNames
import Futhark.Util.Pretty
import Futhark.SoP.SoP qualified as SoP
import Language.Futhark.Semantic
import Language.Futhark (VName)
import Language.Futhark qualified as E
import qualified Data.Map as M
import Debug.Trace (traceM, trace)
import qualified Data.Set as S
import Futhark.Analysis.View.Rules
import Language.Futhark.Traversals qualified as T
import Data.Functor.Identity
import Control.Monad.RWS.Strict hiding (Sum)


--------------------------------------------------------------
tracePrettyM :: (Applicative f, Pretty a) => a -> f ()
tracePrettyM = traceM . prettyString
--------------------------------------------------------------

-- mkViewProg :: VNameSource -> [E.Dec] -> Views
-- mkViewProg vns prog = tracePretty $ execViewM (mkViewDecs prog) vns
mkViewProg :: VNameSource -> Imports -> Views
mkViewProg vns prog = execViewM (mkViewImports prog) vns

mkViewImports :: [(ImportName, FileModule)] -> ViewM ()
mkViewImports = mapM_ (mkViewDecs . E.progDecs . fileProg . snd)
-- A program is a list of declarations (DecBase); functions are value bindings
-- (ValBind). Everything is in an AppExp.

mkViewDecs :: [E.Dec] -> ViewM ()
mkViewDecs [] = pure ()
mkViewDecs (E.ValDec vb : rest) = do
  mkViewValBind vb
  mkViewDecs rest
mkViewDecs (_ : ds) = mkViewDecs ds

mkViewValBind :: E.ValBind -> ViewM ()
mkViewValBind (E.ValBind _ vn ret _ _ params body _ _ _) =
  -- mapM_ paramRefs params
  -- forwards body
  case ret of
    Just (E.TERefine _t _goal _) -> do
      -- We don't really care about the goal right now, as
      -- we just want to express the value binding as an index function.
      traceM ("\n====\nmkViewValBind: " <> prettyString vn)
      traceM ("\nTo prove:\n--------\n" <> prettyString ret)
      traceM ("\nWith params\n-----------\n" <> prettyString params)
      traceM ("\nFor body\n--------\n" <> prettyString body <> "\n====\n")
      forwards body
      pure ()
    _ -> pure ()


getFun :: E.Exp -> Maybe String
getFun (E.Var (E.QualName [] vn) _ _) = Just $ E.baseString vn
getFun _ = Nothing

getSize :: E.Exp -> ViewM Exp
getSize (E.Var _ (E.Info {E.unInfo = E.Array _ _ shape _}) _)
  | dim:_ <- E.shapeDims shape =
    toExp dim
getSize (E.ArrayLit [] (E.Info {E.unInfo = E.Array _ _ shape _}) _)
  | dim:_ <- E.shapeDims shape =
    toExp dim
getSize e = error $ "getSize:" <> prettyString e <> "\n" <> show e

stripExp :: E.Exp -> E.Exp
stripExp x = fromMaybe x (E.stripExp x)

forwards :: E.Exp -> ViewM ()
forwards (E.AppExp (E.LetPat _ p e body _) _)
  | (E.Named x, _, _) <- E.patternParam p = do
    traceM (prettyString p <> " = " <> prettyString e)
    newView <- forward e
    -- traceM . show $ newView
    tracePrettyM newView
    newView1 <- simplify newView >>= rewrite >>= normalise
    tracePrettyM newView1
    traceM "🪨 refining"
    newView2 <- refineView newView1 >>= simplify >>= rewrite >>= normalise
    tracePrettyM newView2
    -- newView6 <- substituteViews newView
    -- traceM "🎭 hoisting cases"
    -- newView1 <- hoistCases newView >>= normalise
    -- tracePrettyM newView1
    -- newView2 <- substituteViews newView1
    -- tracePrettyM newView2
    -- traceM "🎭 hoisting cases"
    -- newView3 <- hoistCases newView2 >>= normalise
    -- tracePrettyM newView3
    -- newView4 <- normalise (simplify newView3)
    -- tracePrettyM newView4
    -- newView5 <- rewrite newView4 >>= normalise
    -- tracePrettyM newView5
    -- newView6 <- refineView newView5 >>= normalise
    -- tracePrettyM newView6
    traceM (show newView2)
    traceM "\n"
    insertView x newView2
    forwards body
    pure ()
forwards _ = pure ()


combineIt :: Iterator -> Iterator -> Iterator
combineIt Empty it = it
combineIt it Empty = it
combineIt d1 d2 | d1 == d2 = d1
combineIt _ _ = undefined

combineCases :: (Exp -> Exp -> Exp) -> Cases Exp -> Cases Exp -> Cases Exp
combineCases f (Cases xs) (Cases ys) =
  Cases . NE.fromList $
    [(cx :&& cy, f vx vy) | (cx, vx) <- NE.toList xs, (cy, vy) <- NE.toList ys]

casesToList :: Cases a -> [(a, a)]
casesToList (Cases xs) = NE.toList xs

toView :: Exp -> View
toView e = View Empty (toCases e)

forward :: E.Exp -> ViewM View
forward (E.Parens e _) = forward e
forward (E.Attr _ e _) = forward e
forward (E.Not e _) = do
  View it e' <- forward e
  pure $ View it $ cmapValues (toNNF . Not) e'
-- Leaves.
forward (E.Literal (E.BoolValue x) _) =
  pure . toView $ Bool x
forward (E.IntLit x _ _) =
  pure . toView . SoP $ SoP.int2SoP x
forward (E.Negate (E.IntLit x _ _) _) =
  normalise . toView . SoP $ SoP.negSoP $ SoP.int2SoP x
-- Potential substitions.
forward e@(E.Var (E.QualName _ vn) _ _) = do
  -- Gets previous top-level views.
  -- traceM $ "🪲 Var " <> prettyString e
  views <- gets views
  case M.lookup vn views of
    Just v@(View _ e2) -> do
      traceM ("🪸 substituting " <> prettyString e <> " for " <> prettyString e2)
      pure v
    _ ->
      pure $ View Empty (toCases $ Var vn)
forward e@(E.AppExp (E.Index xs slice _) _)
  | [E.DimFix idx] <- slice, -- XXX support only simple indexing for now
    (E.Var (E.QualName _ vn) _ _) <- stripExp xs = do
      View i e_i <- forward idx
      -- Gets previous top-level views.
      views <- gets views
      case M.lookup vn views of
        Just (View Empty xs') -> do
          traceM ("🪸 substituting " <> prettyString e <> " for " <> prettyString xs')
          normalise $ View i (combineCases Idx xs' e_i)
        Just (View j xs')
          | Just j' <- iteratorName j -> do
          traceM ("🪸 substituting " <> prettyString e <> " for " <> prettyString xs')
          -- Substitute j for each value in e_i, combining cases.
          cs <- sequence $
                  [seq' ((cx :&&) <$> substituteName j' vx cy, substituteName j' vx vy)
                      | (cx, vx) <- casesToList e_i, (cy, vy) <- casesToList xs']
          normalise $ View (combineIt i j) (Cases . NE.fromList $ cs)
          where
            seq' (x, y) = do
              x' <- x
              y' <- y
              pure (x', y')
        _ -> do
          -- TODO this is duplicated below.
          View j xs' <- forward xs
          normalise $ View (combineIt i j) (combineCases Idx xs' e_i)
  | [E.DimFix idx] <- slice = do -- XXX support only simple indexing for now
    -- traceM $ "🪲 Index2 " <> prettyString e
    View i e_i <- forward idx
    View it_xs xs' <- forward xs
    normalise $ View (combineIt i it_xs) (combineCases Idx xs' e_i)
-- Nodes.
forward (E.ArrayLit es _ _) = do
  es' <- mapM forward es
  let arrs = foldr (combineCases f) (toCases $ Array []) (getCases es')
  let it = foldl1 combineIt (getIters es')
  -- traceM ("🪲 array forward: " <> prettyString es')
  -- traceM ("🪲 array combine: " <> prettyString arrs)
  -- traceM ("🪲 array iterators: " <> show it <> "\n")
  normalise $ View it arrs
  where
    getCases [] = []
    getCases (View _ body : xs) = body : getCases xs
    getIters [] = []
    getIters (View it _ : xs) = it : getIters xs
    f y (Array acc) = Array (y : acc)
    f _ _ = error "impossible"
  -- let es' = map toExp es
  -- in  Array <$> sequence es'
forward (E.AppExp (E.LetPat _ (E.Id vn _ _) x y _) _) =
  substituteNameE vn x y >>= forward >>= normalise
forward (E.AppExp (E.BinOp (op, _) _ (e_x, _) (e_y, _) _) _)
  | E.baseTag (E.qualLeaf op) <= E.maxIntrinsicTag,
    name <- E.baseString $ E.qualLeaf op,
    Just bop <- L.find ((name ==) . prettyString) [minBound .. maxBound :: E.BinOp] = do
      View it_x x <- forward e_x
      View it_y y <- forward e_y
      let it = combineIt it_x it_y
      let doOp bopExp = normalise $ View it (combineCases bopExp x y)
      -- traceM $ "binop bop " <> show bop
      -- traceM $ "binop x " <> prettyString x
      -- traceM $ "binop y " <> prettyString y
      -- traceM $ "binop lol " <> prettyString (doOp (:||))
      case bop of
        E.Plus -> doOp (~+~)
        E.Times -> doOp (~*~)
        E.Minus -> doOp (~-~)
        E.Equal -> doOp (:==)
        E.Less -> doOp (:<)
        E.Greater -> doOp (:>)
        E.Leq -> doOp (:<=)
        E.LogAnd -> doOp (:&&)
        E.LogOr -> doOp (:||)
        _ -> error ("forward not implemented for bin op: " <> show bop)
forward (E.AppExp (E.If c t f _) _) = do
  View it_c c' <- forward c
  View it_t t' <- forward t
  View it_f f' <- forward f
  -- Negating `c` means negating the case _values_ of c, keeping the
  -- conditions of any nested if-statements (case conditions) untouched.
  let neg_c' = cmapValues (toNNF . Not) c'
  let cases_t = [(cc :&& cx, vx) | cc <- flattenCases c',
                                   (cx, vx) <- casesToList t']
  let cases_f = [(neg_cc :&& cx, vx) | neg_cc <- flattenCases neg_c',
                                       (cx, vx) <- casesToList f']
  let it = combineIt it_c $ combineIt it_t it_f
  normalise $ View it (Cases . NE.fromList $ cases_t ++ cases_f)
  where
    -- `c` has cases, so the case conditions and values are put in conjunction.
    flattenCases (Cases xs) = NE.toList $ fmap (uncurry (:&&)) xs
forward (E.AppExp (E.Apply f args _) _)
  | Just fname <- getFun f,
    "map" `L.isPrefixOf` fname,
    E.Lambda params body _ _ _ : args' <- getArgs args = do
      -- traceM ("🪲 map body: " <> show body <> "\n")
      -- 0. Create iterator and transform expression to use it
      i <- newNameFromString "i"
      -- TODO Right now we only support variables as arguments.
      -- Add support for any expression by creating views for function
      -- applications. Literals also need to be handled.
      -- After this, maybe don't use mapMaybe in arrs below.
      sz <- getSize (head args')
      -- Make susbtitutions from function arguments to array names.
      let params' = map E.patNames params
      -- TODO params' is a [Set], I assume because we might have
      --   map (\(x, y) -> ...) xys
      -- meaning x needs to be substituted by x[i].0
      let params'' = mconcat $ map S.toList params' -- XXX wrong, see above
      let subst = M.fromList (zip params'' (map (`index` i) args'))
      body' <- substituteNamesE subst body
      -- traceM $ "#### subst: " <> show subst
      -- traceM $ "#### body transformed: " <> show body'
      View it_body body'' <- forward body'
      let it = combineIt (Forall i (Iota sz)) it_body
      normalise $ View it body''
  | Just fname <- getFun f,
    "scan" == fname, -- XXX support only builtin ops for now
    [E.OpSection (E.QualName [] vn) _ _, _ne, xs'] <- getArgs args = do
      sz <- getSize xs'
      i <- newNameFromString "i"
      debugM $ "scan i: " <> prettyString i
      let i' = Var i
      op <-
        case E.baseString vn of
          "+" -> pure (~+~)
          "-" -> pure (~-~)
          "*" -> pure (~*~)
          _ -> error ("toExp not implemented for bin op: " <> show vn)
      -- Note forward on indexed xs.
      View it_xs xs <- forward (index xs' i)
      let base_case = i' :== SoP (SoP.int2SoP 0)
      let e1 = [(base_case :&& cx, vx) | (cx, vx) <- casesToList xs]
      let e2 = [(Not base_case :&& cx, Recurrence `op` vx) | (cx, vx) <- casesToList xs]
      let it = combineIt (Forall i (Iota sz)) it_xs
      normalise $ View it (Cases . NE.fromList $ e1 ++ e2)
  | Just fname <- getFun f,
    "iota" == fname,
    [n] <- getArgs args = do
      view <- forward n
      i <- newNameFromString "i"
      case view of
        View Empty (Cases ((Bool True, m) NE.:| [])) ->
              normalise $ View (Forall i (Iota m)) (toCases $ Var i)
        _ -> undefined -- TODO We've no way to express this yet.
                       -- Have talked with Cosmin about an "outer if" before.
  | Just fname <- getFun f,
    fname == "not",
    [arg] <- getArgs args = do
      View it body <- forward arg
      -- toView . toNNF . Not <$> toExp arg
      normalise $ View it (cmapValues (toNNF . Not) body)
forward e = error $ "forward on " <> show e

-- Strip unused information.
getArgs :: NE.NonEmpty (a, E.Exp) -> [E.Exp]
getArgs = map (stripExp . snd) . NE.toList

toExp :: E.Exp -> ViewM Exp
toExp (E.Var (E.QualName _ x) _ _) =
  pure $ Var x
toExp (E.ArrayLit es _ _) =
  let es' = map toExp es
  in  Array <$> sequence es'
toExp (E.AppExp (E.BinOp (op, _) _ (e_x, _) (e_y, _) _) _)
  | E.baseTag (E.qualLeaf op) <= E.maxIntrinsicTag,
    name <- E.baseString $ E.qualLeaf op,
    Just bop <- L.find ((name ==) . prettyString) [minBound .. maxBound :: E.BinOp] = do
      x <- toExp e_x
      y <- toExp e_y
      case bop of
        E.Plus -> pure $ x ~+~ y
        E.Times -> pure $ x ~*~ y
        E.Minus -> pure $ x ~-~ y
        E.Equal -> pure $ x :== y
        E.Less -> pure $ x :< y
        E.Greater -> pure $ x :> y
        E.Leq -> pure $ x :<= y
        E.LogAnd -> pure $ x :&& y
        E.LogOr -> pure $ x :|| y
        _ -> error ("toExp not implemented for bin op: " <> show bop)
toExp (E.Parens e _) = toExp e
toExp (E.Attr _ e _) = toExp e
toExp (E.IntLit x _ _) = pure $ SoP $ SoP.int2SoP x
toExp e = error ("toExp not implemented for: " <> show e)

index :: E.Exp -> E.VName -> E.Exp
index xs i =
  E.AppExp (E.Index xs [E.DimFix i'] mempty) (E.Info $ E.AppRes (E.typeOf xs) [])
  where
    i' = E.Var (E.QualName [] i) (E.Info . E.Scalar . E.Prim . E.Signed $ E.Int64) mempty

-- Not to be confused with substituteNames lmao.
substituteNamesE :: M.Map E.VName E.Exp -> E.Exp -> ViewM E.Exp
substituteNamesE = onExp
  where
    substituter subst =
      T.ASTMapper
        { T.mapOnExp = onExp subst,
          T.mapOnName = pure,
          T.mapOnStructType = T.astMap (substituter subst),
          T.mapOnPatType = T.astMap (substituter subst),
          T.mapOnStructRetType = T.astMap (substituter subst),
          T.mapOnPatRetType = T.astMap (substituter subst)
        }
      -- T.identityMapper
      --   { T.mapOnExp = onExp subst }
    onExp :: M.Map VName E.Exp -> E.ExpBase E.Info VName -> ViewM (E.ExpBase E.Info VName)
    onExp subst e@(E.Var (E.QualName _ x) _ _) =
      case M.lookup x subst of
        -- Just x' -> trace ("hihi substituting " <> prettyString x <> " for " <> prettyString x') $ pure x'
        -- Nothing -> error $ show e
        Just x' -> pure x'
        Nothing -> pure e
    onExp subst e = T.astMap (substituter subst) e

substituteNameE :: VName -> E.Exp -> E.Exp -> ViewM E.Exp
substituteNameE vn x = substituteNamesE (M.singleton vn x)
