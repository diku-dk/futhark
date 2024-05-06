module Futhark.Analysis.View (mkIndexFnProg) where

import Data.List qualified as L
import Data.List.NonEmpty(NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, mapMaybe, catMaybes, fromJust)
import Futhark.Analysis.View.Representation
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
import Futhark.SoP.SoP (justConstant)
import Language.Futhark.Primitive (allIntTypes, PrimType (IntType))
import Futhark.SoP.Monad (addRange)


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
mkIndexFnProg :: VNameSource -> Imports -> IndexFns
mkIndexFnProg vns prog = execIndexFnM (mkIndexFnImports prog) vns

mkIndexFnImports :: [(ImportName, FileModule)] -> IndexFnM ()
mkIndexFnImports = mapM_ (mkIndexFnDecs . E.progDecs . fileProg . snd)
-- A program is a list of declarations (DecBase); functions are value bindings
-- (ValBind). Everything is in an AppExp.

mkIndexFnDecs :: [E.Dec] -> IndexFnM ()
mkIndexFnDecs [] = pure ()
mkIndexFnDecs (E.ValDec vb : rest) = do
  mkIndexFnValBind vb
  mkIndexFnDecs rest
mkIndexFnDecs (_ : ds) = mkIndexFnDecs ds

mkIndexFnValBind :: E.ValBind -> IndexFnM ()
mkIndexFnValBind (E.ValBind _ vn ret _ _ params body _ _ _) =
  -- mapM_ paramRefs params
  -- forwards body
  case ret of
    Just (E.TERefine _t _goal _) -> do
      -- We don't really care about the goal right now, as
      -- we just want to express the value binding as an index function.
      traceM ("\n====\nmkIndexFnValBind: " <> prettyString vn)
      traceM ("\nTo prove:\n--------\n" <> prettyString ret)
      traceM ("\nWith params\n-----------\n" <> prettyString params)
      traceM ("\nFor body\n--------\n" <> prettyString body <> "\n====\n")
      forwards body
      pure ()
    _ -> pure ()



forwards :: E.Exp -> IndexFnM ()
forwards (E.AppExp (E.LetPat _ p e body _) _)
  | (E.Named x, _, _) <- E.patternParam p = do
    traceM (prettyString p <> " = " <> prettyString e)
    newIndexFn <- forward e
    postProcess x newIndexFn
    forwards body
    pure ()
forwards (E.AppExp (E.LetPat _ ps@(E.TuplePat {}) e body _) _)
  | names <- S.toList $ E.patNames ps = do
    ys <- unzipT <$> forward e
    forM_ (zip names ys) (uncurry postProcess)
    forwards body
    pure ()
    -- trace ("###" <> prettyString yss) $ pure ()
forwards _ = pure ()

postProcess :: E.VName -> IndexFn -> IndexFnM ()
postProcess vn indexfn = do
  tracePrettyM indexfn
  traceM "🪨 refining"
  indexfn' <- rewrite indexfn >>= refineIndexFn >>= rewrite
  tracePrettyM indexfn'
  traceM "\n"
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
    Just v@(IndexFn _ _) -> do
      traceM ("🪸 substituting " <> prettyString e <> " for " <> prettyString v)
      pure v
    _ -> do
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
  rewrite $
    foldl (\acc (vn, x) -> sub vn x acc)
          (IndexFn iter1 (toCases . Tuple $ map Var vns))
          (zip vns xs)
forward (E.AppExp (E.Index xs' slice _) _)
  | [E.DimFix idx'] <- slice = do -- XXX support only simple indexing for now
      IndexFn iter_idx idx <- forward idx'
      IndexFn iter_xs xs <- forward xs'
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
          rewrite $ sub j (IndexFn iter_idx idx) (IndexFn iter_idx xs)
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
  rewrite $ sub vn x' y'
forward (E.AppExp (E.BinOp (op', _) _ (x', _) (y', _) _) _)
  | E.baseTag (E.qualLeaf op') <= E.maxIntrinsicTag,
    name <- E.baseString $ E.qualLeaf op',
    Just bop <- L.find ((name ==) . prettyString) [minBound .. maxBound :: E.BinOp] = do
      IndexFn iter_x x <- forward x'
      vy <- forward y'
      a <- newNameFromString "a"
      b <- newNameFromString "b"
      let doOp op = rewrite $
                          sub b vy $
                            sub a (IndexFn iter_x x) $
                              IndexFn iter_x (toCases $ op (Var a) (Var b))
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
  rewrite $
    sub f_branch vf $
      sub t_branch vt $
        sub cond (IndexFn iter_c c') y
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
      debugM ("map args " <> prettyString xss)
      debugM ("map body " <> prettyString (IndexFn iter_body cases_body))
      -- Make susbtitutions from function arguments to array names.
      -- TODO `map E.patNames params` is a [Set], I assume because we might have
      --   map (\(x, y) -> ...) xys
      -- meaning x needs to be substituted by x[i].0
      let paramNames = mconcat $ map (S.toList . E.patNames) params
      --               ^ XXX mconcat is wrong, see above
      let s y (paramName, paramIndexFn) = sub paramName paramIndexFn y
      rewrite $
        foldl s (IndexFn iter_y cases_body) (zip paramNames xss)
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
      x <- newNameFromString "x"
      let y = IndexFn
                iter_xs
                (Cases . NE.fromList $
                  [(base_case, Var x), (Not base_case, Recurrence `op` Var x)])
      rewrite $ sub x (IndexFn iter_xs xs) y
  | Just "scan" <- getFun f,
    -- [E.OpSection (E.QualName [] vn) _ _, _ne, xs'] <- getArgs args = do
    [E.Lambda params body' _ _ _, _ne, xs'] <- getArgs args,
    [paramNames_x, paramNames_y] <- map (S.toList . E.patNames) params = do
      -- check if params are tuple, then xs must be unzipped before forward (so never hit zip case)
      xs <- forward xs'
      body <- forward body'
      let paramNames = [paramNames_x, paramNames_y]
      debugM ("paramIndexFns " <> prettyString xs)
      debugM ("paramNames " <> prettyString paramNames)
      debugM ("unzipT xs " <> prettyString (unzipT xs))
      debugM ("unzipT body " <> prettyString (unzipT body))
      ff <- mapM normalise $ unzipT xs
      debugM ("unzipT xs normalised " <> prettyString ff)
      gg <- mapM normalise $ unzipT body
      debugM ("unzipT body normalised " <> prettyString gg)
      -- let zip4 as bs cs ds = zipWith (\(a,b) (c,d) -> (a,b,c,d)) (zip as bs) (zip cs ds)
      -- y_unzipped <- forM (zip4 (unzipT xs) (unzipT body) paramNames_x paramNames_y)
      --                    (\(x, b, vnx, vny) -> do
      --                       vn <- newNameFromString "body"
      --                       pure $
      --                         sub vnx (toScalarIndexFn Recurrence) $
      --                           sub vny x $
      --                             sub vn b $
      --                               IndexFn (getIterator xs) (toCases (Var vn))
      --                    )
      -- debugM ("y_unzipped head " <> prettyString (head y_unzipped))
      -- debugM ("y_unzipped last " <> prettyString (last y_unzipped))
      -- If case values are identifcal, drop conditions?
      -- .      | y_flag₆₀₈₉ ⇒  x_flag₆₀₈₅ || y_flag₆₀₈₉
      --        | ¬(y_flag₆₀₈₉) ⇒  x_flag₆₀₈₅ || y_flag₆₀₈₉,
      -- rewrite $ head y_unzipped

      -- Our semantics are sequential here, so we can just decide that the scan
      -- is a scanl with x always being the accumulator and y always being
      -- an element of the array being scanned over.
      let s y (paramName, paramIndexFn) = sub paramName paramIndexFn y
      vn <- newNameFromString "body"
      let y = sub vn body $ IndexFn (getIterator xs) (toCases (Var vn))
      debugM ("y " <> prettyString y)
      let y1 = foldl s y (zip paramNames_y (unzipT xs))
      debugM ("y " <> prettyString y)
      let y2 = foldl s y1 (zip paramNames_x (repeat (toScalarIndexFn Recurrence)))
      -- y_unzipped <- mapM rewrite $ unzipT y2
      -- debugM ("y_unzipped head " <> prettyString (head y_unzipped))
      -- debugM ("y_unzipped last " <> prettyString (last y_unzipped))
      rewrite y2
      -- case paramNames_x of
      --   [x1, x2] ->
      --     trace (prettyString body) undefined
      --   _ -> undefined
      -- pure $ IndexFn Empty (toCases . SoP2 $ SoP.int2SoP 0)
  | Just fname <- getFun f,
    "zip" `L.isPrefixOf` fname = do
      xss <- mapM forward (getArgs args)
      vns <- mapM (\_ -> newNameFromString "xs") xss
      let IndexFn iter1 _ = head xss
      rewrite $
        foldl (\acc (vn, xs) -> sub vn xs acc)
              (IndexFn iter1 (toCases . Tuple $ map Var vns))
              (zip vns xss)
  | Just fname <- getFun f,
    "unzip" `L.isPrefixOf` fname,
    [xs'] <- getArgs args = do
      -- XXX unzip is a no-op.
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
      -- y = ∀i ∈ ⊎k=1,...,m-1 [b[k-1], ..., b[k]] .
      --     | i == inds[k] && c  => vals[k]   (c may depend on k)
      --     | i /= inds[k] || ¬c => dest[i]
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
          let cond = Var i :== b' :&& cond_b
          debugM ("cond_b " <> show cond_b)
          let y = IndexFn (Forall i (Cat k' m b'))
                          (Cases . NE.fromList $ [(cond, Var vals_k),
                                                  (Not cond, Var dest_i)])
          -- TODO ^ should probably substitute b in using sub rather than using it
          -- directly.
          let dest_fn = IndexFn iter_dest dest
          rewrite $
            sub dest_i dest_fn $
              sub vals_k vals_fn y
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
      let test =
           cmapValues (\v -> SoP2 (termToSoP v) :< SoP2 lower_bound :|| SoP2 (termToSoP v) :>= SoP2 upper_bound)
                      cases
      -- debugM $ "test reflexivity " <> prettyString (map (\(_,v) -> (v, v == v)) (casesToList cases))
      -- debugM $ "test eq bounds " <> prettyString (map (\(_,v) -> (v,SoP2 upper_bound, v == SoP2 upper_bound)) (casesToList cases))
      -- debugM $ "test eq bounds termToSoP " <> prettyString (map (\(_,v) -> (v,SoP2 upper_bound, termToSoP v == upper_bound)) (casesToList cases))
      IndexFn _ (Cases res) <- refineIndexFn (IndexFn iter test)
      -- Swap (x, y) so that OOB is first.
      let res' = case res of
                   ((cx, Bool True) :| [(cy, y')]) | y' /= Bool True ->
                     Just ((cx, x), (cy, y))
                   ((cx, x') :| [(cy, Bool True)]) | x' /= Bool True ->
                     Just ((cy, y), (cx, x))
                   _ -> Nothing
      debugM ("getOOB " <> prettyString (IndexFn iter (Cases res)))
      debugM ("getOOB res: " <> prettyString res')
      pure res'
getOOB _ _ _ = pure Nothing
