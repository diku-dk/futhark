module Futhark.Analysis.View (mkViewProg) where

import Data.List qualified as L
import Data.List.NonEmpty()
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe, fromMaybe)
import Futhark.Analysis.View.Representation
-- import Futhark.Analysis.View.Refine
-- import Futhark.Analysis.View.Rules
import Futhark.MonadFreshNames
import Futhark.Util.Pretty
import Futhark.SoP.SoP qualified as SoP
import Language.Futhark.Semantic
import Language.Futhark (VName)
import Language.Futhark qualified as E
import qualified Data.Map as M
import Debug.Trace (traceM, trace)
import qualified Data.Set as S
import Language.Futhark.Traversals qualified as T
import Data.Functor.Identity
import Control.Monad.RWS.Strict hiding (Sum)
import Futhark.SoP.SoP (SoP)
import Data.Either (isRight)


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

getSize :: E.Exp -> SoP Term
getSize (E.Var _ (E.Info {E.unInfo = E.Array _ _ shape _}) _)
  | dim:_ <- E.shapeDims shape =
    convertSize dim
getSize (E.ArrayLit [] (E.Info {E.unInfo = E.Array _ _ shape _}) _)
  | dim:_ <- E.shapeDims shape =
    convertSize dim
getSize e = error $ "getSize:" <> prettyString e <> "\n" <> show e

-- Used for converting sizes of function arguments.
convertSize :: E.Exp -> SoP Term
convertSize (E.Var (E.QualName _ x) _ _) = SoP.sym2SoP $ Var x
convertSize (E.Parens e _) = convertSize e
convertSize (E.Attr _ e _) = convertSize e
convertSize (E.IntLit x _ _) = SoP.int2SoP x
convertSize e = error ("convertSize not implemented for: " <> show e)

stripExp :: E.Exp -> E.Exp
stripExp x = fromMaybe x (E.stripExp x)

forwards :: E.Exp -> ViewM ()
forwards (E.AppExp (E.LetPat _ p e body _) _)
  | (E.Named x, _, _) <- E.patternParam p = do
    traceM (prettyString p <> " = " <> prettyString e)
    newView <- forward e
    tracePrettyM newView
    -- newView1 <- rewrite newView
    -- tracePrettyM newView1
    -- traceM "🪨 refining"
    -- newView2 <- refineView newView1 >>= rewrite
    -- tracePrettyM newView2
    -- traceM "\n"
    -- insertView x newView2
    insertView x newView
    forwards body
    pure ()
forwards _ = pure ()


combineIt :: Iterator -> Iterator -> Iterator
combineIt Empty it = it
combineIt it Empty = it
combineIt d1 d2 | d1 == d2 = d1
combineIt _ _ = undefined

combineCases :: (SoP Term -> SoP Term -> SoP Term) -> Cases (SoP Term)-> Cases (SoP Term) -> Cases (SoP Term)
combineCases f (Cases xs) (Cases ys) =
  Cases . NE.fromList $
    [(cx :&& cy, f vx vy) | (cx, vx) <- NE.toList xs, (cy, vy) <- NE.toList ys]

casesToList :: Cases a -> [(BoolExp, a)]
casesToList (Cases xs) = NE.toList xs

toView :: SoP Term -> View
toView e = View Empty (Left $ toCases e)

getBool :: (Show a, Show b) => Either a b -> b
getBool e =
  case e of
    Left _ -> error ("getBool type error on " <> show e)
    Right b -> b

isBoolType :: E.Info E.PatType -> Bool
isBoolType (E.Info {E.unInfo = info}) =
  case info of
    E.Scalar scalarTypeBase -> isBoolType' scalarTypeBase
    E.Array _ _ _ scalarTypeBase -> isBoolType' scalarTypeBase
  where
    isBoolType' (E.Prim E.Bool) = True
    isBoolType' _ = False

normalise = pure -- XXX Remove this!

forward :: E.Exp -> ViewM View
forward (E.Parens e _) = forward e
forward (E.Attr _ e _) = forward e
forward (E.Not e _) = do
  View it e' <- forward e
  pure $ View it $ Right $ cmapValues (toNNF . Not) (getBool e')
-- Leaves.
forward (E.Literal (E.BoolValue x) _) =
  pure $ View Empty (Right . toCases $ Bool x)
forward (E.IntLit x _ _) =
  pure . toView $ SoP.int2SoP x
forward (E.Negate (E.IntLit x _ _) _) =
  normalise . toView . SoP.negSoP $ SoP.int2SoP x
-- Potential substitions.
forward e@(E.Var (E.QualName _ vn) t _) = do
  views <- gets views
  case M.lookup vn views of
    Just v@(View _ e2) -> do
      traceM ("🪸 substituting " <> prettyString e <> " for " <> prettyString e2)
      pure v
    _ ->
      let cs = if isBoolType t then Left . toCases . SoP.sym2SoP $ Var vn
                               else Right . toCases $ BoolVar vn
      in  pure $ View Empty cs
forward (E.AppExp (E.Index xs slice _) _)
  | [E.DimFix idx] <- slice = do -- XXX support only simple indexing for now
      View i idx' <- forward idx
      View j xs' <- forward xs
      let cs = case iteratorName j of
                 Just j' -> -- Substitute j for each value in idx', combining cases.
                   Cases . NE.fromList $
                     [(ci :&& substituteName' j' vi cx, substituteName' j' vi vx)
                       | (ci, vi) <- casesToList idx', (cx, vx) <- casesToList xs']
                 Nothing -> combineCases (\xs i -> Idx xs (expToSoP i)) xs' idx'
      -- If the view of idx is a single point, then the resulting view
      -- alsoshould be a single point (scalar/Empty).
      normalise $ View (if i == Empty then Empty else combineIt i j) cs
-- Nodes.
forward (E.ArrayLit _es _ _) =
  -- TODO support arrays via multi-dim index functions.
  error "forward on array literal"
  -- do
  -- es' <- mapM forward es
  -- let arrs = foldr (combineCases f) (toCases $ Array []) (getCases es')
  -- let it = foldl1 combineIt (getIters es')
  -- normalise $ View it arrs
  -- where
  --   getCases [] = []
  --   getCases (View _ body : xs) = body : getCases xs
  --   getIters [] = []
  --   getIters (View it _ : xs) = it : getIters xs
  --   f y (Array acc) = Array (y : acc)
  --   f _ _ = error "impossible"
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
  | Just "scan" <- getFun f,
    [E.OpSection (E.QualName [] vn) _ _, _ne, xs'] <- getArgs args = do
      sz <- getSize xs'
      i <- newNameFromString "i"
      let i' = Var i
      op <-
        case E.baseString vn of
          "+" -> pure (~+~)
          "-" -> pure (~-~)
          "*" -> pure (~*~)
          _ -> error ("scan not implemented for bin op: " <> show vn)
      -- Note forward on indexed xs.
      View it_xs xs <- forward (index xs' i)
      let base_case = i' :== (SoP.int2SoP 0)
      let e1 = [(base_case :&& cx, vx) | (cx, vx) <- casesToList xs]
      let e2 = [(Not base_case :&& cx, Recurrence `op` vx) | (cx, vx) <- casesToList xs]
      let it = combineIt (Forall i (Iota sz)) it_xs
      normalise $ View it (Cases . NE.fromList $ e1 ++ e2)
  | Just "scatter" <- getFun f,
    [dest_arg, inds_arg, vals_arg] <- getArgs args = do
      -- Scatter in-bounds-monotonic indices.
      --
      -- b has size m
      -- b[k-1] <= b[k] for all k     (e.g., sum of positive ints; can be checked from SoP?)
      -- inds = ∀k ∈ [1, ..., m] .
      --     | c1 => OOB              (c1 may depend on i)
      --     | c2 => b[k-1]           (c2 may depend on i)
      -- dest has size b[m-1]
      -- y = scatter dest inds vals
      -- ___________________________________________________
      -- y = ∀i ∈ ⋃k=1,...,m ([b[k-1], ..., b[k]]) .
      --     | i == inds[k] => vals[k]
      --     | i /= inds[k] => dest[i]
      --
      -- From type checking, we have:
      -- scatter : (dest : [n]t) -> (inds : [m]i64) -> (vals : [m]t) : [n]t
      -- * inds and vals are same size
      -- * dest and result are same size
      inds <- forward inds_arg
      -- get size m
      -- extract b from inds
      -- check monotonicity on b
      -- check that cases match pattern with OOB < 0 or OOB > b[m-1]
      vals <- forward vals_arg
      -- check that iterator matches that of inds
      dest <- forward dest_arg
      -- check dest has size b[m-1]
      undefined
  | Just "iota" <- getFun f,
    [n] <- getArgs args = do
      view <- forward n
      i <- newNameFromString "i"
      case view of
        View Empty (Cases ((Bool True, m) NE.:| [])) ->
              normalise $ View (Forall i (Iota m)) (toCases $ Var i)
        _ -> undefined -- TODO We've no way to express this yet.
                       -- Have talked with Cosmin about an "outer if" before.
  | Just "replicate" <- getFun f,
    [n, x] <- getArgs args = do
      n' <- forward n
      x' <- forward x
      i <- newNameFromString "i"
      case (n', x') of
        (View Empty (Cases ((Bool True, m) NE.:| [])),
         View Empty cases) -> -- XXX support only 1D arrays for now.
              normalise $ View (Forall i (Iota m)) cases
        _ -> undefined -- TODO See iota comment.
  | Just "not" <- getFun f,
    [arg] <- getArgs args = do
      View it body <- forward arg
      normalise $ View it (cmapValues (toNNF . Not) body)
forward e = error $ "forward on " <> show e

-- Strip unused information.
getArgs :: NE.NonEmpty (a, E.Exp) -> [E.Exp]
getArgs = map (stripExp . snd) . NE.toList

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
