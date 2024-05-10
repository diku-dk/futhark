module Futhark.Analysis.View (mkIndexFnProg) where

import Data.List qualified as L
import Data.List.NonEmpty(NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Futhark.Analysis.View.Representation
import Futhark.Analysis.View.Monad
import Futhark.Analysis.View.Refine hiding (debugM)
import Futhark.Analysis.View.Rules
import Futhark.Analysis.View.Substitution
import Futhark.MonadFreshNames
import Futhark.Util.Pretty
import Futhark.SoP.SoP qualified as SoP
import Language.Futhark.Semantic
import Language.Futhark qualified as E
import qualified Data.Map as M
import Debug.Trace (traceM, trace)
import qualified Data.Set as S
import Control.Monad.RWS.Strict hiding (Sum)
import Futhark.SoP.Monad (addRange)
import qualified Text.LaTeX.Packages.AMSMath as Math
import Text.LaTeX.Base (textbf, newline)
import Futhark.Analysis.View.Latex


--------------------------------------------------------------
-- Vile and wicked temporary code.
--------------------------------------------------------------
-- This function handles type refinements. Type refinements are on the
-- form (arg: {t | \x -> f x}) where arg is applied to \x -> f x.
handleRefinementTypes :: E.Exp -> IndexFnM ()
handleRefinementTypes (E.Var (E.QualName _ vn)
                             (E.Info {E.unInfo = E.Scalar lol@(E.Refinement _ty ref)}) _) =
  trace ("## hi refinement" <> show lol)
  (handleRefinement ref) vn
handleRefinementTypes _ = pure ()

-- This function takes a type refinement \x -> f x and returns a function
-- that handles this refinement when applied to a name (i.e., arg
-- for (arg: {t | \x -> f x})).
--   For example, the refinement \x -> forall x (>= 0) will return
-- a function that takes a name and adds a range with that name
-- lower bounded by 0 to the environment.
handleRefinement :: E.ExpBase E.Info E.VName -> E.VName -> IndexFnM ()
handleRefinement (E.Lambda [E.Id x _ _] (E.AppExp (E.Apply f args _) _) _ _ _)
  | Just "forall" <- getFun f,
    [E.Var (E.QualName _ x') _ _,
     E.OpSectionRight (E.QualName [] opvn) _ (E.IntLit c _ _) _ _ _] <- getArgs args,
    x == x' = do -- Make sure that lambda arg is applied to forall.
      case E.baseString opvn of
        ">=" ->
          \name ->
            addRange (Var name) (mkRangeLB (SoP.int2SoP c))
        _ -> const undefined
handleRefinement _ = \_ -> pure ()


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

getSize :: E.Exp -> Maybe Term
getSize (E.Var _ (E.Info {E.unInfo = E.Scalar (E.Refinement ty _)}) _) =
  -- TODO why are all refinements scalar?
  sizeOfTypeBase ty
getSize (E.Var _ (E.Info {E.unInfo = ty}) _) = sizeOfTypeBase ty
getSize (E.ArrayLit [] (E.Info {E.unInfo = ty}) _) = sizeOfTypeBase ty
getSize e = error $ "getSize: " <> prettyString e <> "\n" <> show e

sizeOfTypeBase :: E.TypeBase E.Exp as -> Maybe Term
sizeOfTypeBase (E.Array _ _ shape _)
  | dim:_ <- E.shapeDims shape =
    Just $ convertSize dim
  where
    convertSize (E.Var (E.QualName _ x) _ _) = Var x
    convertSize (E.Parens e _) = convertSize e
    convertSize (E.Attr _ e _) = convertSize e
    convertSize (E.IntLit x _ _) = SoP2 $ SoP.int2SoP x
    convertSize e = error ("convertSize not implemented for: " <> show e)
sizeOfTypeBase _ = Nothing


-- Strip unused information.
getArgs :: NE.NonEmpty (a, E.Exp) -> [E.Exp]
getArgs = map (stripExp . snd) . NE.toList
  where
    stripExp x = fromMaybe x (E.stripExp x)

--------------------------------------------------------------
-- Refine source program
--------------------------------------------------------------

-- mkIndexFnProg :: VNameSource -> [E.Dec] -> IndexFns
-- mkIndexFnProg vns prog = tracePretty $ execIndexFnM (mkIndexFnDecs prog) vns
mkIndexFnProg :: VNameSource -> Imports -> (IndexFns, [Log])
mkIndexFnProg vns prog = execIndexFnM (mkIndexFnImports prog) vns

mkIndexFnImports :: [(ImportName, FileModule)] -> IndexFnM ()
mkIndexFnImports = mapM_ (mkIndexFnDecs . E.progDecs . fileProg . snd)
-- A program is a list of declarations (DecBase); functions are value bindings
-- (ValBind). Everything is in an AppExp.

mkIndexFnDecs :: [E.Dec] -> IndexFnM ()
mkIndexFnDecs [] = pure ()
mkIndexFnDecs (E.ValDec vb : rest) = do
  clearIndexFns
  mkIndexFnValBind vb
  mkIndexFnDecs rest
mkIndexFnDecs (_ : ds) = mkIndexFnDecs ds

-- toplevel_indexfns
mkIndexFnValBind :: E.ValBind -> IndexFnM ()
mkIndexFnValBind val@(E.ValBind _ vn ret _ _ _params body _ _ _) = do
  insertTopLevel vn val
  case ret of
    Just (E.TERefine _t _goal _) -> do
      -- We don't really care about the goal right now, as
      -- we just want to express the value binding as an index function.
      traceM ("\n====\nmkIndexFnValBind:\n\n" <> prettyString val)
      traceM ("\nTo prove:\n--------\n" <> prettyString ret <> "\n====\n")
      tell [prettyLaTeX val]
      tell ["To prove:" <> prettyLaTeX ret]
      forwards vn body
      pure ()
    _ -> pure ()


forwards :: E.VName -> E.Exp -> IndexFnM ()
forwards info (E.AppExp (E.LetPat _ p e body _) _)
  | (E.Named x, _, _) <- E.patternParam p = do
    traceM (prettyString p <> " = " <> prettyString e)
    tell [textbf "Forward on " <> Math.math (toLaTeX x) <> toLaTeX e]
    newIndexFn <- forward e
    refineAndBind x newIndexFn
    forwards info body
    pure ()
forwards info (E.AppExp (E.LetPat _ p@(E.TuplePat patterns _) e body _) _) = do
    traceM (prettyString patterns <> " = " <> prettyString e)
    tell [textbf "Forward on " <> Math.math (toLaTeX (S.toList $ E.patNames p)) <> toLaTeX e]
    ys <- unzipT <$> forward e
    forM_ (zip patterns ys) refineAndBind'
    forwards info body
    pure ()
    where
      -- Wrap refineAndBind to discard results otherwise bound to wildcards.
      refineAndBind' (E.Wildcard {}, _) = pure ()
      refineAndBind' (E.Id vn _ _, indexfn) = refineAndBind vn indexfn
      refineAndBind' x = error ("not implemented for " <> show x)
forwards toplevel_vn e = do
  indexfn <- forward e
  refineAndBind toplevel_vn indexfn
  pure ()

-- toplevel_indexfns
refineAndBind :: E.VName -> IndexFn -> IndexFnM ()
refineAndBind vn indexfn = do
  tracePrettyM indexfn
  indexfn' <- rewrite indexfn >>= refineIndexFn >>= rewrite
  tracePrettyM indexfn'
  traceM "\n"
  tell ["resulting in", toLaTeX (vn, indexfn')]
  insertIndexFn vn indexfn'


forward :: E.Exp -> IndexFnM IndexFn
forward (E.Parens e _) = forward e
forward (E.Attr _ e _) = forward e
-- Leaves.
forward (E.Literal (E.BoolValue x) _) =
  normalise . toScalarIndexFn $ Bool x
forward (E.Literal (E.SignedValue (E.Int64Value x)) _) =
  normalise . toScalarIndexFn . SoP2 . SoP.int2SoP $ toInteger x
forward (E.IntLit x _ _) =
  normalise . toScalarIndexFn . SoP2 $ SoP.int2SoP x
forward (E.Negate (E.IntLit x _ _) _) =
  normalise . toScalarIndexFn . SoP2 $ SoP.negSoP $ SoP.int2SoP x
forward e@(E.Var (E.QualName _ vn) _ _) = do
  indexfns <- gets indexfns
  case M.lookup vn indexfns of
    Just indexfn -> do
      traceM ("🌪️🎭 sub " <> prettyString vn <> " for " <> prettyString indexfn)
      pure indexfn
    _ -> do
      debugM ("creating index function for " <> prettyString vn)
      handleRefinementTypes e
      case getSize e of
        Just sz -> do
          -- Canonical array representation.
          i <- newNameFromString "i"
          normalise $ IndexFn (Forall i (Iota sz))
                           (toCases $ Idx (Var vn) (termToSoP (Var i)))
        Nothing ->
          -- Canonical scalar representation.
          normalise $ IndexFn Empty (toCases $ Var vn)
-- Nodes.
forward (E.TupLit es _) = do
  xs <- mapM forward es
  vns <- mapM (\_ -> newNameFromString "xs") xs
  let IndexFn iter1 _ = head xs
  foldM (\acc (vn, x) -> sub vn x acc)
        (IndexFn iter1 (toCases . Tuple $ map Var vns))
        (zip vns xs)
    >>= rewrite
forward (E.AppExp (E.Index xs' slice _) _)
  | [E.DimFix idx'] <- slice = do -- XXX support only simple indexing for now
      IndexFn iter_idx idx <- forward idx'
      IndexFn iter_xs xs <- forward xs'
      case iteratorName iter_xs of
        Just j -> do
          sub j (IndexFn iter_idx idx) (IndexFn iter_idx xs)
            >>= rewrite
        Nothing ->
          error "indexing into a scalar"
forward (E.Not e _) = do
  IndexFn it e' <- forward e
  rewrite $ IndexFn it $ cmapValues Not e'
forward (E.ArrayLit _es _ _) =
  error "forward on array literal"
forward (E.AppExp (E.LetPat _ (E.Id vn _ _) x y _) _) = do
  x' <- forward x
  y' <- forward y
  sub vn x' y' >>= rewrite
forward (E.AppExp (E.BinOp (op', _) _ (x', _) (y', _) _) _)
  | E.baseTag (E.qualLeaf op') <= E.maxIntrinsicTag,
    name <- E.baseString $ E.qualLeaf op',
    Just bop <- L.find ((name ==) . prettyString) [minBound .. maxBound :: E.BinOp] = do
      vx <- forward x'
      let IndexFn iter_x _ = vx
      vy <- forward y'
      a <- newNameFromString "a"
      b <- newNameFromString "b"
      let doOp op = sub a vx (IndexFn iter_x (toCases $ op (Var a) (Var b)))
                      >>= sub b vy
                        >>= rewrite
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
  IndexFn iter_c c' <- forward c
  vt <- forward t
  vf <- forward f
  -- Negating `c` means negating the case _values_ of c, keeping the
  -- conditions of any nested if-statements (case conditions) untouched.
  cond <- newNameFromString "cond"
  t_branch <- newNameFromString "t_branch"
  f_branch <- newNameFromString "f_branch"
  let y = IndexFn iter_c (Cases . NE.fromList $ [(Var cond, Var t_branch),
                                              (Not $ Var cond, Var f_branch)])
  sub cond (IndexFn iter_c c') y
    >>= sub t_branch vt
      >>= sub f_branch vf
        >>= rewrite
forward e | trace ("forward\n  " ++ prettyString e) False =
  -- All calls after this case get traced.
  undefined
forward (E.AppExp (E.Apply f args _) _)
  | Just fname <- getFun f,
    "map" `L.isPrefixOf` fname,
    E.Lambda params body _ _ _ : args' <- getArgs args = do
      xss <- mapM forward args'
      let IndexFn iter_y _ = head xss
      -- TODO use iter_body; likely needed for nested maps?
      IndexFn iter_body cases_body <- forward body
      unless (iter_body == iter_y || iter_body == Empty)
             (error $ "map: got incompatible iterator from map lambda body: "
                      <> show iter_body)
      -- Make susbtitutions from function arguments to array names.
      -- TODO `map E.patNames params` is a [Set], I assume because we might have
      --   map (\(x, y) -> ...) xys
      -- meaning x needs to be substituted by x[i].0
      let paramNames :: [E.VName] = mconcat $ map (S.toList . E.patNames) params
      let xss_flat :: [IndexFn] = mconcat $ map unzipT xss
      let s y (paramName, paramIndexFn) = sub paramName paramIndexFn y >>= simplify
      let y' = IndexFn iter_y cases_body
      tell ["Using map rule ", toLaTeX y']
      foldM s (IndexFn iter_y cases_body) (zip paramNames xss_flat)
        >>= rewrite
  | Just "scan" <- getFun f,
    [E.OpSection (E.QualName [] vn) _ _, _ne, xs'] <- getArgs args = do
      IndexFn iter_xs xs <- forward xs'
      let Just i = iteratorName iter_xs
      -- TODO should verify that _ne matches op
      op <-
        case E.baseString vn of
          "+" -> pure (~+~)
          "-" -> pure (~-~)
          "*" -> pure (~*~)
          _ -> error ("scan not implemented for bin op: " <> show vn)
      let base_case = Var i :== SoP2 (SoP.int2SoP 0)
      x <- newNameFromString "a"
      let y = IndexFn
                iter_xs
                (Cases . NE.fromList $
                  [(base_case, Var x), (Not base_case, Recurrence `op` Var x)])
      tell ["Using scan rule ", toLaTeX y]
      sub x (IndexFn iter_xs xs) y
        >>= rewrite
  | Just "scan" <- getFun f,
    [E.Lambda params body' _ _ _, _ne, xs'] <- getArgs args,
    [paramNames_x, paramNames_y] <- map (S.toList . E.patNames) params = do
      xs <- forward xs'
      body <- forward body'
      -- Our semantics are sequential, so we can just decide that the scan
      -- is a scanl with x always being the accumulator and y always being
      -- an element of the array being scanned over.
      let s y (paramName, paramIndexFn) = sub paramName paramIndexFn y
                                            >>= simplify
      vn <- newNameFromString "body"
      y <- sub vn body (IndexFn (getIterator xs) (toCases (Var vn))) >>= simplify
      tell ["Using scan rule", toLaTeX y]
      foldM s y (zip paramNames_y (unzipT xs))
        >>= \y' -> foldM s y' (zip paramNames_x (repeat (toScalarIndexFn Recurrence)))
          >>= rewrite
  | Just fname <- getFun f,
    "zip" `L.isPrefixOf` fname = do
      xss <- mapM forward (getArgs args)
      vns <- mapM (\_ -> newNameFromString "xs") xss
      let IndexFn iter1 _ = head xss
      let y = IndexFn iter1 (toCases . Tuple $ map Var vns)
      tell ["Using zip rule ", toLaTeX y]
      rewrite =<<
        foldM (\acc (vn, xs) -> sub vn xs acc) y (zip vns xss)
  | Just fname <- getFun f,
    "unzip" `L.isPrefixOf` fname,
    [xs'] <- getArgs args = do
      -- XXX unzip is a no-op.
      tell ["Using unzip rule" <> newline]
      forward xs' >>= rewrite
  | Just "scatter" <- getFun f,
    [dest_arg, inds_arg, vals_arg] <- getArgs args = do
      -- Scatter in-bounds-monotonic indices.
      --
      -- b has size at least m
      -- b[k-1] <= b[k] for all k     (e.g., sum of positive ints; can be checked from SoP?)
      -- inds = ∀k ∈ [0, ..., m) .
      --     | c  => b[k]             (c may depend on k)
      --     | ¬c => OOB
      -- dest has size b[m-1]         (to ensure conclusion covers all of dest)
      -- b[0] is 0
      -- OOB < 0 or OOB >= b[m-1]
      -- y = scatter dest inds vals
      -- ___________________________________________________
      -- y = ∀i ∈ ⊎k=iota m [b[k], ..., b[k+1]] .
      --     | i == inds[k] => vals[k]
      --     | i /= inds[k] => dest[i]
      --
      -- Note that c and ¬c are not in the conclusion for later pattern matching.
      -- Leaving them out is safe, because i == inds[k] only if inds[k] == b[k],
      -- which implies c. The latter case is just a negation of the first and
      -- so ¬c also disappears.
      --
      -- From type checking, we have:
      -- scatter : (dest : [n]t) -> (inds : [m]i64) -> (vals : [m]t) : [n]t
      -- * inds and vals are same size
      -- * dest and result are same size
      IndexFn iter_inds inds <- forward inds_arg
      -- let Forall i (Iota m) = iter_inds -- TODO don't do unsafe matching.
      let Cases ((c, x) :| [(neg_c, y)]) = inds
      unless (c == (toNNF . Not $ neg_c)) (error "this should never happen")
      vals_fn <- forward vals_arg
      IndexFn iter_dest dest <- forward dest_arg
      -- The size of dest is the final value that the iterator takes on
      -- since b is monotonically increasing. (Later we check that
      -- sz_dest == b[m-1] to actually confirm that there's a correspondence.)
      let Just sz_dest = iteratorEnd iter_dest -- TODO unsafe
      debugM ("sz_dest " <> prettyString sz_dest)
      let inds_fn = IndexFn iter_inds inds
      -- Check that exactly one branch is OOB---and determine which.
      oob_test <- getOOB (SoP.int2SoP 0) (termToSoP sz_dest) inds_fn
      case oob_test of
        Just ((_, oob), (cond_b, b)) -> do
          -- check monotonicity on b
          isMonotonic <- checkMonotonic iter_inds (cond_b, b)
          -- TODO allow monotonically decreasing by just reversing b
          unless isMonotonic (error "couldn't prove that scatter indices are monotonically increasing")
          -- check that cases match pattern with OOB < 0 or OOB > b[m-1]
          -- check that iterator matches that of inds
          -- check dest has size b[m-1]
          let Forall k (Iota m) = iter_inds
          i <- newNameFromString "i"
          vals_k <- newNameFromString "vals_k"
          dest_i <- newNameFromString "dest_i"
          -- Purely aesthetic renaming of k to ensure that "k" is printed
          -- (and not, e.g., "i").
          k' <- newNameFromString "k"
          let b' = substituteName k (Var k') b
          -- Leave out cond_b; see comment.
          let cond = Var i :== b' -- :&& cond_b
          debugM ("cond_b " <> show cond_b)
          let y = IndexFn (Forall i (Cat k' m b'))
                          (Cases . NE.fromList $ [(cond, Var vals_k),
                                                  (Not cond, Var dest_i)])
          tell ["Using Scatter in-bounds-monotonic indices rule ", toLaTeX y]
          -- TODO ^ should probably substitute b in using sub rather than using it
          -- directly.
          let dest_fn = IndexFn iter_dest dest
          sub vals_k vals_fn y
            >>= sub dest_i dest_fn
              >>= rewrite
        Nothing -> error "🤡 unhandled scatter"
  | Just "iota" <- getFun f,
    [n] <- getArgs args = do
      indexfn <- forward n
      i <- newNameFromString "i"
      case indexfn of
        IndexFn Empty (Cases ((Bool True, m) :| [])) ->
              rewrite $ IndexFn (Forall i (Iota m)) (toCases $ Var i)
        _ -> undefined -- TODO We've no way to express this yet.
                       -- Have talked with Cosmin about an "outer if" before.
  | Just "replicate" <- getFun f,
    [n, x] <- getArgs args = do
      n' <- forward n
      x' <- forward x
      i <- newNameFromString "i"
      case (n', x') of
        (IndexFn Empty (Cases ((Bool True, m) :| [])),
         IndexFn Empty cases) -> -- XXX support only 1D arrays for now.
              simplify $ IndexFn (Forall i (Iota m)) cases
        _ -> undefined -- TODO See iota comment.
  | Just "not" <- getFun f,
    [arg] <- getArgs args = do
      IndexFn it body <- forward arg
      rewrite $ IndexFn it (cmapValues (toNNF . Not) body)
  | Just g <- getFun f,
    args' <- getArgs args = do
      -- Handle references to user-defined top-level function definitions.
      toplevel <- gets toplevel
      case M.lookup g toplevel of
        Just val -> do
          xs <- mapM forward args'
          let argnames =
                S.toList . mconcat . map E.patNames $ E.valBindParams val
          when (length argnames /= length xs) (error "must be fully applied")
          traceM ("🪅 inlining " <> prettyString (E.valBindName val))
          debugM ("adding indexfns\n  " <> prettyString (zip argnames xs))
          forM_ (zip argnames xs) (uncurry insertIndexFn)
          forward (E.valBindBody val)
        _ -> do
          error $ "forward on unhandled function " <> prettyString g
forward e = error $ "forward on " <> show e

-- Check that exactly one branch is OOB---and determine which.
-- Returns Just (OOB, non-OOB), if decidable, else Nothing.
getOOB :: SoP.SoP Term
  -> SoP.SoP Term
  -> IndexFn
  -> IndexFnM (Maybe ((Term, Term), (Term, Term)))
getOOB lower_bound upper_bound (IndexFn iter cases)
  | Cases ((c, x) :| [(neg_c, y)]) <- cases,
    c == (toNNF . Not $ neg_c) = do
      let test = cmapValues (\v -> SoP2 (termToSoP v) :< SoP2 lower_bound
                                   :|| SoP2 (termToSoP v) :>= SoP2 upper_bound)
                            cases
      IndexFn _ (Cases res) <- refineIndexFn (IndexFn iter test)
      -- Swap (x, y) so that OOB is first.
      let res' = case res of
                   ((cx, Bool True) :| [(cy, y')]) | y' /= Bool True ->
                     Just ((cx, x), (cy, y))
                   ((cx, x') :| [(cy, Bool True)]) | x' /= Bool True ->
                     Just ((cy, y), (cx, x))
                   _ -> Nothing
      pure res'
getOOB _ _ _ = pure Nothing
