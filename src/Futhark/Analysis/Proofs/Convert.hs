module Futhark.Analysis.Proofs.Convert where

import Control.Applicative ((<|>))
import Control.Monad (foldM, forM, forM_, msum, unless, void, when, (<=<))
import Control.Monad.RWS (gets)
import Data.Bifunctor
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Debug.Trace (traceM)
import Futhark.Analysis.Proofs.AlgebraBridge (addRelIterator, algebraContext, toAlgebra, ($==))
import Futhark.Analysis.Proofs.AlgebraPC.Symbol qualified as Algebra
import Futhark.Analysis.Proofs.IndexFn (Cases (Cases), Domain (..), IndexFn (..), Iterator (..), cases, casesToList, catVar, justSingleCase, unzipT)
import Futhark.Analysis.Proofs.IndexFnPlus (domainEnd, domainStart, intervalEnd, repIndexFn)
import Futhark.Analysis.Proofs.Monad
import Futhark.Analysis.Proofs.Query (Answer (..), MonoDir (..), Query (..), allCases, askQ, isUnknown, isYes)
import Futhark.Analysis.Proofs.Rewrite (rewrite, rewriteWithoutRules)
import Futhark.Analysis.Proofs.Substitute ((@))
import Futhark.Analysis.Proofs.Symbol (Symbol (..), neg, sop2Symbol)
import Futhark.Analysis.Proofs.Unify (Replacement, Substitution (mapping), mkRep, renamesM, rep, unify)
import Futhark.Analysis.Proofs.Util (prettyBinding, prettyBinding')
import Futhark.MonadFreshNames (VNameSource, newVName)
import Futhark.SoP.Monad (addEquiv, addProperty, addRange, addUntrans, mkRangeLB)
import Futhark.SoP.SoP (SoP, int2SoP, justSym, mapSymSoP_, negSoP, sym2SoP, (.+.), (.-.), (~*~), (~+~), (~-~))
import Futhark.Util.Pretty (prettyString)
import Language.Futhark qualified as E
import Language.Futhark.Semantic (FileModule (fileProg), ImportName, Imports)

--------------------------------------------------------------
-- Extracting information from E.Exp.
--------------------------------------------------------------
getFun :: E.Exp -> Maybe String
getFun (E.Var (E.QualName [] vn) _ _) = Just $ E.baseString vn
getFun _ = Nothing

getSize :: E.Exp -> Maybe (SoP Symbol)
getSize (E.Var _ (E.Info {E.unInfo = ty}) _) = sizeOfTypeBase ty
getSize (E.ArrayLit [] (E.Info {E.unInfo = ty}) _) = sizeOfTypeBase ty
getSize e = error $ "getSize: " <> prettyString e <> "\n" <> show e

sizeOfTypeBase :: E.TypeBase E.Exp as -> Maybe (SoP Symbol)
sizeOfTypeBase (E.Scalar (E.Refinement ty _)) =
  sizeOfTypeBase ty
sizeOfTypeBase (E.Scalar (E.Arrow _ _ _ _ return_type)) =
  sizeOfTypeBase (E.retType return_type)
sizeOfTypeBase (E.Array _ shape _)
  | dim : _ <- E.shapeDims shape =
      Just $ convertSize dim
  where
    convertSize (E.Var (E.QualName _ x) _ _) = sym2SoP $ Var x
    convertSize (E.Parens e _) = convertSize e
    convertSize (E.Attr _ e _) = convertSize e
    convertSize (E.IntLit x _ _) = int2SoP x
    convertSize e = error ("convertSize not implemented for: " <> show e)
sizeOfTypeBase _ = Nothing

typeIsBool :: E.TypeBase E.Exp as -> Bool
typeIsBool (E.Scalar (E.Prim E.Bool)) = True
typeIsBool _ = False

-- Strip unused information.
getArgs :: NE.NonEmpty (a, E.Exp) -> [E.Exp]
getArgs = map (stripExp . snd) . NE.toList
  where
    stripExp x = fromMaybe x (E.stripExp x)

--------------------------------------------------------------
-- Construct index function for source program
--------------------------------------------------------------
mkIndexFnProg :: VNameSource -> Imports -> M.Map E.VName IndexFn
mkIndexFnProg vns prog = snd $ runIndexFnM (mkIndexFnImports prog) vns

mkIndexFnImports :: [(ImportName, FileModule)] -> IndexFnM ()
mkIndexFnImports = mapM_ (mkIndexFnDecs . E.progDecs . fileProg . snd)

-- A program is a list of declarations (DecBase); functions are value bindings
-- (ValBind). Everything is in an AppExp.

mkIndexFnDecs :: [E.Dec] -> IndexFnM ()
mkIndexFnDecs [] = pure ()
mkIndexFnDecs (E.ValDec vb : rest) = do
  _ <- mkIndexFnValBind vb
  mkIndexFnDecs rest
mkIndexFnDecs (_ : ds) = mkIndexFnDecs ds

-- toplevel_indexfns
mkIndexFnValBind :: E.ValBind -> IndexFnM IndexFn
mkIndexFnValBind val@(E.ValBind _ vn _ret _ _ params body _ _ _) = do
  clearAlgEnv
  whenDebug . traceM $ "\n\n==== mkIndexFnValBind:\n\n" <> prettyString val
  forM_ params addTypeRefinement
  forM_ params addBooleanNames
  indexfn <- forward body >>= rewrite >>= bindfn vn
  insertTopLevel vn (params, indexfn)
  _ <- algebraContext indexfn $ do
    alg <- gets algenv
    whenDebug . traceM $ "Algebra context for indexfn:\n" <> prettyString alg
  pure indexfn

bindfn :: E.VName -> IndexFn -> IndexFnM IndexFn
bindfn vn indexfn = do
  insertIndexFn vn indexfn
  whenDebug (traceM $ prettyBinding vn indexfn <> "\n")
  -- tell ["resulting in", toLaTeX (vn, indexfn')]
  pure indexfn

singleCase :: a -> Cases Symbol a
singleCase e = cases [(Bool True, e)]

fromScalar :: SoP Symbol -> IndexFn
fromScalar e = IndexFn Empty (singleCase e)

forward :: E.Exp -> IndexFnM IndexFn
forward (E.Parens e _) = forward e
forward (E.Attr _ e _) = forward e
forward (E.AppExp (E.LetPat _ (E.Id vn _ _) x in_body _) _) = do
  -- tell [textbf "Forward on " <> Math.math (toLaTeX vn) <> toLaTeX x]
  whenDebug . traceM $ "Forward on " <> prettyString vn
  (bindfn vn =<< forward x) >> forward in_body
forward (E.AppExp (E.LetPat _ (E.TuplePat patterns _) x body _) _) = do
  -- tell [textbf "Forward on " <> Math.math (toLaTeX (S.toList $ E.patNames p)) <> toLaTeX x]
  whenDebug . traceM $ "Forward on " <> prettyString patterns
  xs <- unzipT <$> forward x
  forM_ (zip patterns xs) bindfnOrDiscard
  forward body
  where
    bindfnOrDiscard (E.Wildcard {}, _) = pure ()
    bindfnOrDiscard (E.Id vn _ _, indexfn) = void (bindfn vn indexfn)
    bindfnOrDiscard e = error ("not implemented for " <> show e)
forward (E.Literal (E.BoolValue x) _) =
  pure . fromScalar . sym2SoP $ Bool x
forward (E.Literal (E.SignedValue (E.Int64Value x)) _) =
  pure . fromScalar . int2SoP $ toInteger x
forward (E.IntLit x _ _) =
  pure . fromScalar $ int2SoP x
forward (E.Negate x _) = do
  -- Numeric negation.
  fn <- forward x
  pure $
    IndexFn
      { iterator = iterator fn,
        body = cases [(p, negSoP v) | (p, v) <- casesToList (body fn)]
      }
forward e@(E.Var (E.QualName _ vn) _ _) = do
  indexfns <- gets indexfns
  case M.lookup vn indexfns of
    Just indexfn -> do
      pure indexfn
    _ -> do
      case getSize e of
        Just sz -> do
          -- Canonical array representation.
          i <- newVName "i"
          pure $
            IndexFn
              (Forall i (Iota sz))
              (singleCase . sym2SoP $ Idx (Var vn) (sym2SoP $ Var i))
        Nothing ->
          -- Canonical scalar representation.
          pure $ IndexFn Empty (singleCase . sym2SoP $ Var vn)
forward (E.TupLit xs _) = do
  fns <- mapM forward xs
  vns <- forM fns (\_ -> newVName "f")
  let IndexFn it1 _ = head fns -- TODO probably want to grab most complex iterator here.
  let y = IndexFn it1 (cases [(Bool True, sym2SoP . Tuple $ map (sym2SoP . Var) vns)])
  substParams y (zip vns fns)
forward (E.AppExp (E.Index xs' slice _) _)
  | [E.DimFix idx'] <- slice = do
      idx@(IndexFn idx_iter _) <- forward idx'
      xs <- forward xs'
      debugPrettyM "idx" idx
      debugPrettyM "xs" xs
      -- If xs has a Cat iterator, this indexing can only be done
      -- if we can express k in terms of i.
      capture_avoiding_rep <-
        case (xs, justSingleCase idx) of
          (IndexFn (Forall _ (Cat k m b)) _, Just idx1) -> do
            s1 <- solve_for k b idx1
            s2 <- solve_for k (intervalEnd $ Cat k m b) idx1
            pure $ maybe mempty (mkRep k) (s1 <|> s2)
          _ ->
            pure mempty
      unless (null capture_avoiding_rep) $
        debugPrettyM "capture_avoiding_rep" capture_avoiding_rep
      let ys = repIndexFn capture_avoiding_rep xs
      unless (null capture_avoiding_rep) $ debugPrettyM "xs repped" ys
      i <- newVName "i"
      x <- newVName "x"
      debugT' "idx res" $
        substParams
          (IndexFn idx_iter (singleCase . sym2SoP $ Idx (Var x) (sym2SoP $ Var i)))
          [(i, idx), (x, ys)]
  where
    -- Solve for k in x(k) = y.
    solve_for k x y = do
      k_hole <- newVName "k_hole"
      let x_holed = rep (M.singleton k $ sym2SoP (Hole k_hole)) x
      s :: Maybe (Replacement Symbol) <- fmap mapping <$> unify x_holed y
      pure $ s >>= (M.!? k_hole)
forward (E.Not e _) = do
  IndexFn it e' <- forward e
  rewrite $ IndexFn it $ cmapValues (mapSymSoP_ neg) e'
forward (E.AppExp (E.BinOp (op', _) _ (x', _) (y', _) _) _)
  | E.baseTag (E.qualLeaf op') <= E.maxIntrinsicTag,
    name <- E.baseString $ E.qualLeaf op',
    Just bop <- L.find ((name ==) . prettyString) [minBound .. maxBound :: E.BinOp] = do
      vx <- forward x'
      vy <- forward y'
      a <- newVName "a"
      b <- newVName "b"
      let doOp op =
            substParams
              (IndexFn Empty (singleCase $ op (Var a) (Var b)))
              [(a, vx), (b, vy)]
      case bop of
        E.Plus -> doOp (~+~)
        E.Times -> doOp (~*~)
        E.Minus -> doOp (~-~)
        E.Equal -> doOp (~==~)
        E.Less -> doOp (~<~)
        E.Greater -> doOp (~>~)
        E.Leq -> doOp (~<=~)
        E.LogAnd -> doOp (~&&~)
        E.LogOr -> doOp (~||~)
        _ -> error ("forward not implemented for bin op: " <> show bop)
forward (E.AppExp (E.If c t f _) _) = do
  IndexFn iter_c c' <- forward c
  vt <- forward t
  vf <- forward f
  -- Negating `c` means negating the case _values_ of c, keeping the
  -- conditions of any nested if-statements (case conditions) untouched.
  cond <- newVName "if-condition"
  t_branch <- newVName "t_branch"
  f_branch <- newVName "f_branch"
  let y =
        IndexFn
          iter_c
          ( cases
              [ (Var cond, sym2SoP $ Var t_branch),
                (neg $ Var cond, sym2SoP $ Var f_branch)
              ]
          )
  substParams y [(cond, IndexFn iter_c c'), (t_branch, vt), (f_branch, vf)]
forward expr@(E.AppExp (E.Apply f args _) _)
  | Just fname <- getFun f,
    "map" `L.isPrefixOf` fname,
    E.Lambda params body _ _ _ : args' <- getArgs args = do
      -- tell ["Using map rule ", toLaTeX y']
      arrs <- concatMap unzipT <$> mapM forward args'
      iter <- bindLambdaBodyParams (zip (concatMap E.patNames params) arrs)
      body_fn <- forward body
      case body_fn of
        IndexFn Empty body_cases -> rewrite $ IndexFn iter body_cases
        _ -> error "Non-scalar body."
  | Just fname <- getFun f,
    "map" `L.isPrefixOf` fname = do
      -- No need to handle map non-lambda yet as program can just be rewritten.
      error $
        "forward on map with non-lambda function arg: "
          <> prettyString expr
          <> ". Eta-expand your program."
  | Just "replicate" <- getFun f,
    [n, x] <- getArgs args = do
      n' <- forward n
      x' <- forward x
      i <- newVName "i"
      case (n', x') of
        ( IndexFn Empty (Cases ((Bool True, m) NE.:| [])),
          IndexFn Empty body
          ) ->
            -- XXX support only 1D arrays for now.
            rewrite $ IndexFn (Forall i (Iota m)) body
        _ -> undefined -- TODO See iota comment.
  | Just "iota" <- getFun f,
    [n] <- getArgs args = do
      indexfn <- forward n
      i <- newVName "i"
      case indexfn of
        IndexFn Empty (Cases ((Bool True, m) NE.:| [])) ->
          rewrite $ IndexFn (Forall i (Iota m)) (singleCase . sym2SoP $ Var i)
        _ -> undefined -- TODO We've no way to express this yet.
        -- Have talked with Cosmin about an "outer if" before.
  | Just fname <- getFun f,
    "zip" `L.isPrefixOf` fname = do
      xss <- mapM forward (getArgs args)
      vns <- mapM (\_ -> newVName "xs") xss
      let IndexFn it1 _ = head xss -- TODO probably want to grab most complex iterator here.
      let y = IndexFn it1 (cases [(Bool True, sym2SoP . Tuple $ map (sym2SoP . Var) vns)])
      substParams y (zip vns xss)
  | Just fname <- getFun f,
    "unzip" `L.isPrefixOf` fname,
    [xs'] <- getArgs args =
      -- XXX unzip is a no-op.
      forward xs'
  | Just "scan" <- getFun f,
    [E.OpSection (E.QualName [] vn) _ _, _ne, xs'] <- getArgs args = do
      -- Scan with basic operator.
      IndexFn iter_xs xs <- forward xs'
      let i = case iter_xs of
            (Forall i' _) -> i'
            Empty -> error "scan array is empty?"
      -- TODO should we verify that _ne matches op?
      op <-
        case E.baseString vn of
          "+" -> pure (~+~)
          "-" -> pure (~-~)
          "*" -> pure (~*~)
          _ -> error ("scan not implemented for bin op: " <> show vn)
      let base_case = sym2SoP (Var i) :== int2SoP 0
      x <- newVName "a"
      let y =
            IndexFn
              iter_xs
              ( cases
                  [ (base_case, sym2SoP (Idx (Var x) (sVar i))),
                    (neg base_case, Recurrence `op` Idx (Var x) (sVar i))
                  ]
              )
      -- tell ["Using scan rule ", toLaTeX y]
      y @ (x, IndexFn iter_xs xs)
        >>= rewrite
  | Just "scan" <- getFun f,
    [E.Lambda params lam_body _ _ _, _ne, lam_xs] <- getArgs args,
    [paramNames_acc, paramNames_x] <- map E.patNames params = do
      -- We pick the first argument of the lambda to be the accumulator
      -- and the second argument to be an element of the input array.
      -- (The lambda is associative, so we are free to pick.)
      iter <- bindLambdaBodyParams . zip paramNames_x . unzipT =<< forward lam_xs
      let accToRecurrence = M.fromList (map (,sym2SoP Recurrence) paramNames_acc)
      body_fn <- repIndexFn accToRecurrence <$> forward lam_body
      case body_fn of
        IndexFn Empty body_cases -> rewrite $ IndexFn iter body_cases
        _ -> error "Non-scalar body."
  | Just "scatter" <- getFun f,
    [dest_arg, inds_arg, vals_arg] <- getArgs args = do
      -- Scatter in-bounds-monotonic indices.
      --
      -- y = scatter dest inds vals
      -- where
      --   inds = ∀k ∈ [0, ..., m-1] .
      --       | seg(k+1) - seg(k) > 0  => seg(k)
      --       | seg(k+1) - seg(k) <= 0 => OOB
      --   seg(0) is 0
      --   seg(k) is monotonically increasing
      --   dest has size seg(m) - 1         (to ensure conclusion covers all of dest)
      --   OOB < 0 or OOB >= seg(m) - 1
      -- ___________________________________________________
      -- y = ∀i ∈ ⊎k=iota m [seg(k), ..., seg(k+1) - 1] .
      --     | i == inds[k] => vals[k]
      --     | i /= inds[k] => dest[i]
      --
      -- by the semantics of scatter, equivalent to
      -- y = ∀i ∈ ⊎k=iota m [seg(k), ..., seg(k+1) - 1] .
      --     | i == seg(k) => vals[k]
      --     | i /= seg(k) => dest[i]
      --
      -- (i == seg(k) implies seg(k+1) - seg(k) > 0, since otherwise
      --  the interval [seg(k), ..., seg(k+1) - 1] is empty and i could
      --  not be equal to seg(k).)
      -- TODO find a nicer way to express this index function.
      --
      --
      -- From type checking, we have:
      -- scatter : (dest : [n]t) -> (inds : [m]i64) -> (vals : [m]t) : [n]t
      -- \* inds and vals are same size
      -- \* dest and result are same size

      ----------------------------
      -- WIP More general rule:
      --
      --   inds = ∀k ∈ [0, ..., m-1] .
      --       | p(k) => seg(k)
      --       | p(k) => OOB
      --   seg(0) is 0
      --   seg(k) is monotonically increasing
      --   dest has size seg(m) - 1         (to ensure conclusion covers all of dest)
      --   OOB < 0 or OOB >= seg(m) - 1
      -- ___________________________________________________
      -- y = ∀i ∈ ⊎k=iota m [seg(k), ..., seg(k+1) - 1] .
      --     | i == inds[k] => vals[k]
      --     | i /= inds[k] => dest[i]
      -- y = ∀i ∈ ⊎k=iota m [seg(k), ..., seg(k+1) - 1] .
      --     | i == seg(k) ^ p(k) => vals[k]
      --     | i == OOB ^ not p(k) => vals[k]
      --     | i /= seg(k) ^ p(k) => dest[i]
      --     | i /= OOB ^ not p(k) => dest[i]
      --
      -- by OOB < 0, we know that i == OOB is false and i /= OOB is true:
      -- y = ∀i ∈ ⊎k=iota m [seg(k), ..., seg(k+1) - 1] .
      --     | i == seg(k) ^ p(k) => vals[k]
      --     | False ^ not p(k) => vals[k]
      --     | i /= seg(k) ^ p(k) => dest[i]
      --     | True ^ not p(k) => dest[i]
      --
      -- y = ∀i ∈ ⊎k=iota m [seg(k), ..., seg(k+1) - 1] .
      --     | i == seg(k) ^ p(k) => vals[k]
      --     | i /= seg(k) ^ p(k) => dest[i]
      --     | not p(k) => dest[i]
      --
      -- y = ∀i ∈ ⊎k=iota m [seg(k), ..., seg(k+1) - 1] .
      --     | i == seg(k) ^ p(k) => vals[k]
      --     | i /= seg(k) ^ p(k) || True ^ not p(k) => dest[i]
      --
      -- y = ∀i ∈ ⊎k=iota m [seg(k), ..., seg(k+1) - 1] .
      --     | i == seg(k) ^ p(k) => vals[k]
      --     | i /= seg(k) => dest[i]
      dest <- forward dest_arg >>= rewrite
      inds <- forward inds_arg >>= rewrite
      vals <- forward vals_arg >>= rewrite
      -- 1. Check that inds is on the right form.
      vn_k <- newVName "k"
      vn_m <- newVName "m"
      vn_p0 <- newVName "p0"
      vn_f0 <- newVName "f0"
      vn_p1 <- newVName "p1"
      vn_f1 <- newVName "f1"
      let inds_template =
            IndexFn
              { iterator = Forall vn_k (Iota $ sym2SoP $ Hole vn_m),
                body =
                  cases
                    [ (Hole vn_p0, sym2SoP $ Hole vn_f0),
                      (Hole vn_p1, sym2SoP $ Hole vn_f1)
                    ]
              }
      s <- fromMaybe (error "unhandled scatter") <$> unify inds_template inds
      -- Safe to do this now:
      let IndexFn inds_iter@(Forall k (Iota m)) _ = inds
      -- Determine which is OOB and which is e1.
      let isOOB ub = CaseCheck (\c -> c :< int2SoP 0 :|| (mapping s M.! ub) :<= c)
      (case_idx_seg, vn_p_seg, vn_f_seg) <- do
        case0_is_OOB <- askQ (isOOB vn_f1) inds 0
        case case0_is_OOB of
          Yes -> pure (1, vn_p1, vn_f1)
          Unknown -> do
            case1_is_OOB <- askQ (isOOB vn_f0) inds 1
            case case1_is_OOB of
              Yes -> pure (0, vn_p0, vn_f0)
              Unknown -> error "scatter: unable to determine OOB branch"
      let p_seg = sop2Symbol $ mapping s M.! vn_p_seg
      let f_seg = mapping s M.! vn_f_seg
      -- Check that p_seg = f_seg(k+1) - f_seg(k) > 0.
      algebraContext inds $ do
        addRelIterator inds_iter
        seg_delta <- rewrite $ rep (mkRep k (sVar k .+. int2SoP 1)) f_seg .-. f_seg
        p_desired_form :: Maybe (Substitution Symbol) <- unify p_seg (seg_delta :> int2SoP 0)
        unless (isJust p_desired_form) $ error "p is not on desired form"
      -- Check that seg(0) = 0 and that seg is monotonically increasing.
      let x `at_k` i = rep (mkRep k i) x
      let zero :: SoP Symbol = int2SoP 0
      eq0 <- askQ (CaseCheck (\seg -> seg `at_k` zero :== int2SoP 0)) inds case_idx_seg
      when (isUnknown eq0) $ error "scatter: unable to determine segment start"
      -- Check that seg is monotonically increasing.
      mono <- askQ (CaseIsMonotonic Inc) inds case_idx_seg
      when (isUnknown mono) $ error "scatter: unable to show segment monotonicity"
      -- Check that the proposed end of segments seg(m) - 1 equals the size of dest.
      -- (Note that has to hold outside the context of inds, so we cannot assume p_seg.)
      let IndexFn (Forall _ dom_dest) _ = dest
      let dest_size = domainEnd dom_dest
      domain_covered <- f_seg `at_k` m .-. int2SoP 1 $== dest_size
      when (isUnknown domain_covered) $
        error "scatter: segments do not cover iterator domain"
      -- y = ∀i ∈ ⊎k=iota m [seg(k), ..., seg(k+1) - 1] .
      --     | i == seg(k) ^ p(k) => vals[k]
      --     | i /= seg(k) || not p(k) => dest[i]
      i <- newVName "i"
      dest_hole <- newVName "dest_hole"
      vals_hole <- newVName "vals_hole"
      let p = sVar i :== f_seg
      let fn =
            IndexFn
              { iterator = Forall i (Cat k m f_seg),
                body =
                  cases
                    [ (p, sym2SoP $ Apply (Var vals_hole) [sVar k]),
                      (neg p, sym2SoP $ Apply (Var dest_hole) [sVar i])
                    ]
              }
      substParams fn [(vals_hole, vals), (dest_hole, dest)]
  -- Applying other functions, for instance, user-defined ones.
  | (E.Var (E.QualName [] g) info _) <- f,
    args' <- getArgs args,
    E.Scalar (E.Arrow _ _ _ _ (E.RetType _ return_type)) <- E.unInfo info = do
      toplevel <- gets toplevel
      case M.lookup g toplevel of
        Just (pats, indexfn) -> do
          -- g is a previously analyzed top-level function definiton.
          whenDebug . traceM $ "✨ Using index fn " <> prettyBinding' g indexfn
          let param_names = mconcat . map E.patNames $ pats
          let param_sizes = map (getVName <=< sizeOfTypeBase . E.patternType) pats
          -- The arguments that the parameters are to be replaced for.
          arg_fns <- mapM forward args'
          arg_sizes <- mapM sizeOfDomain arg_fns
          when (length param_names /= length arg_fns) (error "must be fully applied")
          -- Size paramters must be replaced as well.
          unless (map isJust param_sizes == map isJust arg_sizes) (error "sizes don't align")
          let size_rep = M.fromList $ catMaybes $ zipMaybes param_sizes arg_sizes
          -- Check that preconditions are satisfied.
          preconditions <- mapM getPrecondition pats
          when (isJust $ msum preconditions) $
            whenDebug . traceM $
              "Checking preconditions for " <> prettyString g
          forM_ (zip3 pats preconditions arg_fns) $ \(pat, pre, fn) -> do
            ans <- case pre of
              Nothing -> pure Yes
              Just check -> check (size_rep, zip param_names arg_fns)
            unless (isYes ans) . error $
              "Precondition on " <> prettyString pat <> " not satisfied for " <> prettyString fn
          -- The resulting index fn will be fully applied, so we can rewrite recurrences here.
          -- (Which speeds up things by eliminating cases.)
          substParams (repIndexFn size_rep indexfn) (zip param_names arg_fns)
            >>= rewrite
          where
            getVName x | Just (Var vn) <- justSym x = Just vn
            getVName _ = Nothing

            sizeOfDomain (IndexFn Empty _) = pure Nothing
            sizeOfDomain (IndexFn (Forall _ d) _) =
              Just <$> rewrite (domainEnd d .-. domainStart d .+. int2SoP 1)

            zipMaybes = zipWith (liftA2 (,))
        Nothing -> do
          -- g is a free variable in this expression (probably a parameter
          -- to the top-level function currently being analyzed).
          arg_fns <- mapM forward args'
          arg_names <- forM arg_fns (const $ newVName "x")
          iter <-
            case sizeOfTypeBase return_type of
              Just sz -> do
                -- Function returns an array.
                i <- newVName "i"
                pure $ Forall i (Iota sz)
              Nothing -> do
                pure Empty
          let g_fn =
                IndexFn
                  { iterator = iter,
                    body =
                      singleCase . sym2SoP $
                        Apply (Var g) (map (sym2SoP . Var) arg_names)
                  }
          debugPrettyM "g_fn:" g_fn
          debugPrettyM "g:" g
          when (typeIsBool return_type) $ addProperty (Algebra.Var g) Algebra.Boolean
          substParams g_fn (zip arg_names arg_fns)
forward e = error $ "forward on " <> show e <> "\nPretty: " <> prettyString e

substParams :: (Foldable t) => IndexFn -> t (E.VName, IndexFn) -> IndexFnM IndexFn
substParams = foldM substParam
  where
    -- We want to simplify, but avoid rewriting recurrences during
    -- paramter-substitution.
    substParam fn (paramName, paramIndexFn) =
      (fn @ (paramName, paramIndexFn)) >>= rewriteWithoutRules

cmap :: ((a, b) -> (c, d)) -> Cases a b -> Cases c d
cmap f (Cases xs) = Cases (fmap f xs)

cmapValues :: (b -> c) -> Cases a b -> Cases a c
cmapValues f = cmap (second f)

sVar :: E.VName -> SoP Symbol
sVar = sym2SoP . Var

-- TODO eh bad
(~==~) :: Symbol -> Symbol -> SoP Symbol
x ~==~ y = sym2SoP $ sym2SoP x :== sym2SoP y

(~<~) :: Symbol -> Symbol -> SoP Symbol
x ~<~ y = sym2SoP $ sym2SoP x :< sym2SoP y

(~>~) :: Symbol -> Symbol -> SoP Symbol
x ~>~ y = sym2SoP $ sym2SoP x :> sym2SoP y

(~<=~) :: Symbol -> Symbol -> SoP Symbol
x ~<=~ y = sym2SoP $ sym2SoP x :<= sym2SoP y

(~&&~) :: Symbol -> Symbol -> SoP Symbol
x ~&&~ y = sym2SoP $ x :&& y

(~||~) :: Symbol -> Symbol -> SoP Symbol
x ~||~ y = sym2SoP $ x :|| y

--------------------------------------------------------------
-- Handling refinement types.
--------------------------------------------------------------
type CheckContext = (Replacement Symbol, [(E.VName, IndexFn)])

type Check = CheckContext -> IndexFnM Answer

type Effect = IndexFnM ()

getPrecondition :: E.PatBase E.Info E.VName (E.TypeBase dim u) -> IndexFnM (Maybe Check)
getPrecondition = fmap (fmap fst) . getRefinement

getRefinement :: E.PatBase E.Info E.VName (E.TypeBase dim u) -> IndexFnM (Maybe (Check, Effect))
getRefinement (E.PatParens pat _) = getRefinement pat
getRefinement (E.PatAscription pat _ _) = getRefinement pat
getRefinement (E.Id param (E.Info {E.unInfo = E.Scalar (E.Refinement _ty ref)}) _) = do
  whenDebug . traceM $ "Getting type refinement" <> prettyString (param, ref)
  Just <$> mkRef ref
  where
    mkRef ((E.AppExp (E.Apply f args _) _))
      | Just "elementwise" <- getFun f,
        [E.OpSectionRight (E.QualName [] vn_op) _ y _ _ _] <- getArgs args = do
          let op = case E.baseString vn_op of
                ">=" -> (:>=)
                _ -> undefined
          y' <- getScalar <$> (forward y >>= rewrite)
          let check = mkCheck $ toScalarFn . sym2SoP $ sym2SoP (Var param) `op` y'
          let effect = do
                hole <- sym2SoP . Hole <$> newVName "h"
                alg_vn <- newVName (E.baseString param <> "ª")
                addUntrans (Algebra.Var alg_vn) (Idx (Var param) hole)
                addRange (Algebra.Var alg_vn) . mkRangeLB =<< toAlgebra y'
          pure (check, effect)
      | Just "equals" <- getFun f,
        [y] <- getArgs args = do
          y' <- getScalar <$> (forward y >>= rewrite)
          let check = mkCheck $ toScalarFn . sym2SoP $ sym2SoP (Var param) :== y'
          let effect = addEquiv (Algebra.Var param) =<< toAlgebra y'
          pure (check, effect)
    mkRef x = error $ "Unhandled refinement predicate " <> show x

    mkCheck check_fn (size_rep, param_subst) = do
      check <- repIndexFn size_rep <$> substParams check_fn param_subst
      allCases (askQ (CaseCheck sop2Symbol)) check

    getScalar (IndexFn Empty cs) | [(Bool True, x)] <- casesToList cs = x
    getScalar _ = error "getScalar on non-scalar index function"

    toScalarFn x = IndexFn Empty (cases [(Bool True, x)])
getRefinement _ = pure Nothing

-- This function adds the effects of type refinements to the environment
-- without checking that they hold.
-- Use this function on the parameters of top-level definitions, where
-- refinements are pre-requisites assumed to be true.
addTypeRefinement :: E.PatBase E.Info E.VName E.ParamType -> IndexFnM ()
addTypeRefinement pat = do
  ref <- getRefinement pat
  case ref of
    Just (_, effect) -> effect
    _ -> pure ()

addBooleanNames :: E.PatBase E.Info E.VName E.ParamType -> IndexFnM ()
addBooleanNames (E.PatParens pat _) = addBooleanNames pat
addBooleanNames (E.PatAscription pat _ _) = addBooleanNames pat
addBooleanNames (E.Id param (E.Info {E.unInfo = E.Array _ _ t}) _) = do
  when (typeIsBool $ E.Scalar t) $ addProperty (Algebra.Var param) Algebra.Boolean
addBooleanNames (E.Id param (E.Info {E.unInfo = t}) _) = do
  when (typeIsBool t) $ addProperty (Algebra.Var param) Algebra.Boolean
addBooleanNames _ = pure ()

-- Binds names of scalar parameters to scalar values of corresponding
-- index functions. Assumes that domains are equivalent across index
-- functions. Returns the most "complex" iterator over these domains.
-- For example, this would transform the lambda body of the following
--   map (\x y z -> x + y + z) xs ys zs
-- into
--   map (\i -> xs[i] + ys[i] + zs[i]) (indices xs)
-- where xs is the index function with the most "complex" iterator.
bindLambdaBodyParams :: [(E.VName, IndexFn)] -> IndexFnM Iterator
bindLambdaBodyParams params = do
  -- Make sure all Cat k bound in iterators are identical by renaming.
  fns <- renamesM (map snd params)
  let iter@(Forall i _) = maximum (map iterator fns)
  forM_ (zip (map fst params) fns) $ \(paramName, fn) -> do
    debugPrettyM (prettyString paramName) fn
    vn <- newVName "tmp_fn"
    IndexFn tmp_iter cs <-
      IndexFn iter (singleCase . sym2SoP $ Idx (Var vn) (sVar i)) @ (vn, fn)
    -- Renaming k bound in `tmp_iter` to k bound in `iter`.
    let k_rep =
          fromMaybe mempty $ mkRep <$> catVar tmp_iter <*> (sVar <$> catVar iter)
    unless (null k_rep) $ debugPrettyM2 "k_rep" k_rep
    insertIndexFn paramName (repIndexFn k_rep $ IndexFn Empty cs)
  pure iter
