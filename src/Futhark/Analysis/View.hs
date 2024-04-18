module Futhark.Analysis.View (mkViewProg) where

import Data.List qualified as L
import Data.List.NonEmpty()
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
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
import Control.Monad.RWS.Strict hiding (Sum)


--------------------------------------------------------------
tracePrettyM :: (Applicative f, Pretty a) => a -> f ()
tracePrettyM = traceM . prettyString
--------------------------------------------------------------

--------------------------------------------------------------
-- Extracting information from E.Exp.
--------------------------------------------------------------
getFun :: E.Exp -> Maybe String
getFun (E.Var (E.QualName [] vn) _ _) = Just $ E.baseString vn
getFun _ = Nothing

getSize :: E.Exp -> Maybe Exp
getSize (E.Var _ (E.Info {E.unInfo = E.Scalar _}) _) =
  Nothing
getSize (E.Var _ (E.Info {E.unInfo = E.Array _ _ shape _}) _)
  | dim:_ <- E.shapeDims shape =
    Just $ convertSize dim
getSize (E.ArrayLit [] (E.Info {E.unInfo = E.Array _ _ shape _}) _)
  | dim:_ <- E.shapeDims shape =
    Just $ convertSize dim
getSize e = error $ "getSize: " <> prettyString e <> "\n" <> show e

-- Used for converting sizes of function arguments.
convertSize :: E.Exp -> Exp
convertSize (E.Var (E.QualName _ x) _ _) = Var x
convertSize (E.Parens e _) = convertSize e
convertSize (E.Attr _ e _) = convertSize e
convertSize (E.IntLit x _ _) = SoP2 $ SoP.int2SoP x
convertSize e = error ("convertSize not implemented for: " <> show e)

-- Strip unused information.
getArgs :: NE.NonEmpty (a, E.Exp) -> [E.Exp]
getArgs = map (stripExp . snd) . NE.toList
  where
    stripExp x = fromMaybe x (E.stripExp x)

--------------------------------------------------------------
-- Refine source program
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


forwards :: E.Exp -> ViewM ()
forwards (E.AppExp (E.LetPat _ p e body _) _)
  | (E.Named x, _, _) <- E.patternParam p = do
    traceM (prettyString p <> " = " <> prettyString e)
    newView <- forward e
    tracePrettyM newView
    traceM (show newView)
    newView1 <- rewrite newView
    tracePrettyM newView1
    traceM "🪨 refining"
    newView2 <- refineView newView1 >>= rewrite
    tracePrettyM newView2
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

-- Substitution rules for indexing. These are the rules
-- shown in the document. (All other substitutions are
-- an implementation detail because we don't have the program
-- in administrative normal form.)
sub :: VName -> View -> View -> View
sub x q@(View (Forall i xD) xs) r@(View (Forall j yD) ys)
  | xD == yD =
  -- Substitution Rule 1 where y indexes x.
    debug ("sub " <> prettyString x <> " for " <> prettyString q <> "\n   in " <> prettyString r) $ View
      (Forall j yD)
      (Cases . NE.fromList $ do
        (xcond, xval) <- casesToList $ substituteName i (Var j) xs
        (ycond, yval) <- casesToList ys
        pure (substituteName x xval ycond :&& xcond,
              substituteName x xval yval))
sub x q@(View Empty xs) r@(View iter_y ys) =
  -- No rule in document (substituting scalar into index function).
  debug ("sub " <> prettyString x <> " for " <> prettyString q <> "\n   in " <> prettyString r) $
    View
      iter_y
      (Cases . NE.fromList $ do
        (xcond, xval) <- casesToList xs
        (ycond, yval) <- casesToList ys
        pure (substituteName x xval ycond :&& xcond,
              substituteName x xval yval))
sub x q r =
  error $ "💀 sub "
          <> prettyString x <> " for "
          <> prettyString q <> "\n   in "
          <> prettyString r

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
  pure . toView . SoP2 $ SoP.int2SoP x
forward (E.Negate (E.IntLit x _ _) _) =
  normalise . toView . SoP2 $ SoP.negSoP $ SoP.int2SoP x
-- Potential substitions.
forward e@(E.Var (E.QualName _ vn) _ _) = do
  views <- gets views
  case M.lookup vn views of
    Just v@(View _ _) -> do
      traceM ("🪸 substituting " <> prettyString e <> " for " <> prettyString v)
      pure v
    _ ->
      case getSize e of
        Just sz -> do
          -- Canonical array representation.
          i <- newNameFromString "i"
          pure $ View (Forall i (Iota sz))
                      (toCases $ Idx (Var vn) (expToSoP (Var i)))
        Nothing ->
          -- Canonical scalar representation.
          pure $ View Empty (toCases $ Var vn)
forward (E.AppExp (E.Index xs' slice _) _)
  | [E.DimFix idx'] <- slice = do -- XXX support only simple indexing for now
      View iter_idx idx <- forward idx'
      View iter_xs xs <- forward xs'
      debugM ("index " <> prettyString xs' <> " by " <> prettyString idx')
      case iteratorName iter_xs of
        Just j -> do
          -- This case can be funky. Treating `a[i]` inside the map lambda
          --   let a = scan (+) 0i64 x
          --   let b = map (\i -> a[i]) (iota n)
          -- yields:
          --   index a by i
          --   a: ∀i₆₁₀₂ ∈ iota n₆₀₆₈ .
          --     | True => Σj₆₁₀₄∈[0, ..., i₆₁₀₂] ((x₆₀₇₀)[j₆₁₀₄])
          --   i: .
          --     | True => i₆₀₉₃
          --   sub i_6102 for .
          --     | True => i₆₀₉₃
          --    in .
          --     | True => Σj₆₁₀₄∈[0, ..., i₆₁₀₂] ((x₆₀₇₀)[j₆₁₀₄])
          --   sub result: .
          --     | True => Σj₆₁₀₄∈[0, ..., i₆₀₉₃] ((x₆₀₇₀)[j₆₁₀₄])
          -- So the result is a scalar because i₆₀₉₃ is a scalar in this context,
          -- because we are inside the body of the map lambda.
          -- (I think this is correct; i₆₀₉₃ is a program variable like x₆₀₇₀.)
          normalise $ sub j (View iter_idx idx) (View iter_idx xs)
        Nothing ->
          error "indexing into a scalar"
-- Nodes.
forward (E.ArrayLit _es _ _) =
  error "forward on array literal"
forward (E.AppExp (E.LetPat _ (E.Id vn _ _) x y _) _) = do
  x' <- forward x
  y' <- forward y
  normalise $ sub vn x' y'
forward (E.AppExp (E.BinOp (op, _) _ (x', _) (y', _) _) _)
  | E.baseTag (E.qualLeaf op) <= E.maxIntrinsicTag,
    name <- E.baseString $ E.qualLeaf op,
    Just bop <- L.find ((name ==) . prettyString) [minBound .. maxBound :: E.BinOp] = do
      View iter_x x <- forward x'
      vy <- forward y'
      a <- newNameFromString "a"
      b <- newNameFromString "b"
      let doOp bopExp = normalise $
                          sub b vy $
                            sub a (View iter_x x) $
                              View iter_x (toCases $ bopExp (Var a) (Var b))
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
  View iter_c c' <- forward c
  vt <- forward t
  vf <- forward f
  -- Negating `c` means negating the case _values_ of c, keeping the
  -- conditions of any nested if-statements (case conditions) untouched.
  cond <- newNameFromString "cond"
  t_branch <- newNameFromString "t_branch"
  f_branch <- newNameFromString "f_branch"
  let y = View iter_c (Cases . NE.fromList $ [(Var cond, Var t_branch),
                                              (Not $ Var cond, Var f_branch)])
  normalise $
    sub f_branch vf $
      sub t_branch vt $
        sub cond (View iter_c c') y
forward (E.AppExp (E.Apply f args _) _)
  | Just fname <- getFun f,
    "map" `L.isPrefixOf` fname,
    E.Lambda params body _ _ _ : args' <- getArgs args = do
      xss <- mapM forward args'
      let View iter_y _ = head xss
      -- TODO use iter_body; likely needed for nested maps?
      View iter_body cases_body <- forward body
      unless (iter_body == iter_y || iter_body == Empty)
             (error $ "map: got incompatible iterator from map lambda body: "
                      <> show iter_body)
      debugM ("map args " <> prettyString xss)
      debugM ("map body " <> prettyString (View iter_body cases_body))
      -- Make susbtitutions from function arguments to array names.
      -- TODO `map E.patNames params` is a [Set], I assume because we might have
      --   map (\(x, y) -> ...) xys
      -- meaning x needs to be substituted by x[i].0
      let paramNames = mconcat $ map (S.toList . E.patNames) params
      --               ^ XXX mconcat is wrong, see above
      let s y (paramName, paramView) = sub paramName paramView y
      normalise $
        foldl s (View iter_y cases_body) (zip paramNames xss)
  | Just "scan" <- getFun f,
    [E.OpSection (E.QualName [] vn) _ _, _ne, xs'] <- getArgs args = do
      View iter_xs xs <- forward xs'
      let Just i = iteratorName iter_xs
      -- TODO should verify that _ne matches op
      op <-
        case E.baseString vn of
          "+" -> pure (~+~)
          "-" -> pure (~-~)
          "*" -> pure (~*~)
          _ -> error ("scan not implemented for bin op: " <> show vn)
      let base_case = Var i :== SoP2 (SoP.int2SoP 0)
      x <- newNameFromString "x"
      let y = View
                iter_xs
                (Cases . NE.fromList $
                  [(base_case, Var x), (Not base_case, Recurrence `op` Var x)])
      normalise $ sub x (View iter_xs xs) y
  | Just "scatter" <- getFun f,
    [dest_arg, inds_arg, vals_arg] <- getArgs args = do
      -- Scatter in-bounds-monotonic indices.
      --
      -- b has at least size m
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
