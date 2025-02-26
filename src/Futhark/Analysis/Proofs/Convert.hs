module Futhark.Analysis.Proofs.Convert (mkIndexFnProg, mkIndexFnValBind) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, foldM_, forM, forM_, unless, void, when, zipWithM_)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT), hoistMaybe)
import Data.Bifunctor
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing)
import Data.Set qualified as S
import Debug.Trace (traceM)
import Futhark.Analysis.Proofs.AlgebraBridge (addRelIterator, addRelSymbol, algebraContext, assume, paramToAlgebra, toAlgebra, ($==), ($>=))
import Futhark.Analysis.Proofs.AlgebraPC.Symbol qualified as Algebra
import Futhark.Analysis.Proofs.IndexFn (Cases (Cases), Domain (..), IndexFn (..), Iterator (..), cases, casesToList, flattenCases, justSingleCase)
import Futhark.Analysis.Proofs.IndexFnPlus (domainEnd, domainStart, repCases, repIndexFn)
import Futhark.Analysis.Proofs.Monad
import Futhark.Analysis.Proofs.Query (Answer (..), Property (..), Query (..), askQ, askRefinement, askRefinements, isUnknown, isYes, prove)
import Futhark.Analysis.Proofs.Rewrite (rewrite, rewriteWithoutRules)
import Futhark.Analysis.Proofs.Substitute (subst, (@))
import Futhark.Analysis.Proofs.Symbol (Symbol (..), neg, sop2Symbol)
import Futhark.Analysis.Proofs.Unify (Replacement, Substitution (mapping), mkRep, renamesM, rep, unify)
import Futhark.Analysis.Proofs.Util (prettyBinding, prettyBinding')
import Futhark.MonadFreshNames (VNameSource, newName, newVName)
import Futhark.SoP.Monad (addEquiv, addProperty)
import Futhark.SoP.Refine (addRel)
import Futhark.SoP.SoP (Rel (..), SoP, int2SoP, justSym, mapSymSoP, negSoP, sym2SoP, (.+.), (.-.), (~*~), (~+~), (~-~))
import Futhark.Util.Pretty (prettyString)
import Language.Futhark qualified as E
import Language.Futhark.Semantic (FileModule (fileProg), ImportName, Imports)

--------------------------------------------------------------
-- Extracting information from E.Exp.
--------------------------------------------------------------
refinementPrelude :: S.Set String
refinementPrelude =
  S.fromList
    [ "injective",
      "injectiveOn",
      "and"
      -- "segments"
    ]

justVName :: E.Exp -> Maybe E.VName
justVName (E.Var (E.QualName [] vn) _ _) = Just vn
justVName _ = Nothing

getFun :: E.Exp -> Maybe String
getFun e = E.baseString <$> justVName e

getSize :: E.Exp -> IndexFnM (Maybe (SoP Symbol))
getSize (E.Var _ (E.Info {E.unInfo = (E.Scalar (E.Record _))}) loc) =
  errorMsg loc "Record-type variables must be unpacked."
getSize (E.Var _ (E.Info {E.unInfo = ty}) _) = sizeOfTypeBase ty
getSize (E.ArrayLit [] (E.Info {E.unInfo = ty}) _) = sizeOfTypeBase ty
getSize e = error $ "getSize: " <> prettyString e <> "\n" <> show e

sizeOfTypeBase :: E.TypeBase E.Size as -> IndexFnM (Maybe (SoP Symbol))
sizeOfTypeBase (E.Scalar (E.Refinement ty _)) =
  sizeOfTypeBase ty
sizeOfTypeBase (E.Scalar (E.Arrow _ _ _ _ return_type)) =
  sizeOfTypeBase (E.retType return_type)
sizeOfTypeBase (E.Array _ shape _)
  | dim : _ <- E.shapeDims shape = do
      -- FIXME Only supporting one dimensional arrays.
      ds <- forward dim
      case ds of
        [f_d]
          | Just d <- justSingleCase f_d ->
              pure $ Just d
        [_] ->
          -- FIXME Pretty sure all branches would have the same size?
          -- So could we just pick one?
          error "sizeOfTypBase on multiple case function"
        _ ->
          error "Currently only has support for one dimensional arrays."
sizeOfTypeBase (E.Scalar (E.Record _)) =
  error "Run E.patternMap first"
sizeOfTypeBase _ = pure Nothing

typeIsBool :: E.TypeBase E.Exp as -> Bool
typeIsBool (E.Scalar (E.Prim E.Bool)) = True
typeIsBool _ = False

-- Strip unused information.
getArgs :: NE.NonEmpty (a, E.Exp) -> [E.Exp]
getArgs = map (stripExp . snd) . NE.toList
  where
    stripExp x = fromMaybe x (E.stripExp x)

-- Like patternMap, but doesn't discard information about wildcards.
patternMapAligned :: E.Pat t -> [(Maybe E.VName, t)]
patternMapAligned = map f . patIdentsAligned
  where
    f (v, E.Info t) = (v, t)

    patIdentsAligned (E.Id v t _) = [(Just v, t)]
    patIdentsAligned (E.PatParens p _) = patIdentsAligned p
    patIdentsAligned (E.TuplePat pats _) = foldMap patIdentsAligned pats
    patIdentsAligned (E.RecordPat fs _) = foldMap (patIdentsAligned . snd) fs
    patIdentsAligned (E.Wildcard t _) = [(Nothing, t)]
    patIdentsAligned (E.PatAscription p _ _) = patIdentsAligned p
    patIdentsAligned (E.PatLit _ t _) = [(Nothing, t)]
    patIdentsAligned (E.PatConstr _ _ ps _) = foldMap patIdentsAligned ps
    patIdentsAligned (E.PatAttr _ p _) = patIdentsAligned p

--------------------------------------------------------------
-- Construct index function for source program
--------------------------------------------------------------
mkIndexFnProg :: VNameSource -> Imports -> M.Map E.VName [IndexFn]
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

hasRefinement :: E.TypeExp d vn -> Bool
hasRefinement (E.TEParens te _) =
  hasRefinement te
hasRefinement (E.TERefine {}) = True
hasRefinement (E.TETuple tes _) =
  any hasRefinement tes
hasRefinement _ = False

mkIndexFnValBind :: E.ValBind -> IndexFnM [IndexFn]
mkIndexFnValBind val@(E.ValBind _ vn (Just ret) _ _ params body _ _ val_loc)
  | hasRefinement ret = do
      clearAlgEnv
      printM 1 $
        emphString ("Analyzing " <> prettyString (E.locText (E.srclocOf val_loc)))
          <> prettyString val
      forM_ params addTypeRefinement
      forM_ params addBooleanNames
      forM_ params addSizeVariables
      indexfns <- forward body >>= mapM rewrite >>= bindfn vn
      insertTopLevel vn (params, indexfns)
      checkRefinement indexfns ret
      pure indexfns
  where
    checkRefinement indexfns (E.TEParens te _) =
      checkRefinement indexfns te
    checkRefinement indexfns te@(E.TERefine _ (E.Lambda lam_params lam_body _ _ _) loc) = do
      printM 1 . warningString $
        "Checking post-condition:\n" <> prettyStr te
      let param_names = map fst $ mconcat $ map patternMapAligned lam_params
      forM_ (zip param_names indexfns) $ \(nm, fn) ->
        when (isJust nm) . void $ bindfn (fromJust nm) [fn]
      postconds <- forward lam_body >>= mapM rewrite
      printM 1 $
        "Post-conditions after substituting in results:\n  "
          <> prettyStr postconds
      answer <- askRefinements postconds
      case answer of
        Yes -> do
          printM 1 (E.baseString vn <> " ∎\n\n")
          pure ()
        Unknown ->
          errorMsg loc $ "Failed to show refinement: " <> prettyString te
    checkRefinement _ (E.TERefine _ _ loc) = do
      errorMsg loc "Only lambda post-conditions are currently supported."
    checkRefinement indexfns (E.TETuple tes _)
      | length indexfns == length tes = do
          zipWithM_ checkRefinement (map (: []) indexfns) tes
      | otherwise =
          undefined
    checkRefinement _ _ = pure ()
mkIndexFnValBind (E.ValBind _ vn _ _ _ params body _ _ _) = do
  insertTopLevelDef vn (params, body)
  pure []

bindfn :: E.VName -> [IndexFn] -> IndexFnM [IndexFn]
bindfn = bindfn_ 1

bindfn_ :: Int -> E.VName -> [IndexFn] -> IndexFnM [IndexFn]
bindfn_ level vn indexfns = do
  insertIndexFn vn indexfns
  printM level $ prettyBinding vn indexfns
  pure indexfns

singleCase :: a -> Cases Symbol a
singleCase e = cases [(Bool True, e)]

fromScalar :: SoP Symbol -> [IndexFn]
fromScalar e = [IndexFn Empty (singleCase e)]

forward :: E.Exp -> IndexFnM [IndexFn]
forward (E.Parens e _) = forward e
forward (E.Attr _ e _) = forward e
forward (E.AppExp (E.LetPat _ (E.Id vn _ _) x in_body _) _) = do
  -- tell [textbf "Forward on " <> Math.math (toLaTeX vn) <> toLaTeX x]
  whenDebug . traceM $ "Forward on " <> prettyString vn
  (bindfn vn =<< forward x) >> forward in_body
forward (E.AppExp (E.LetPat _ (E.TuplePat patterns _) x body _) _) = do
  -- tell [textbf "Forward on " <> Math.math (toLaTeX (S.toList $ E.patNames p)) <> toLaTeX x]
  whenDebug . traceM $ "Forward on " <> prettyString patterns
  xs <- forward x
  forM_ (zip (mconcat $ map patternMapAligned patterns) xs) bindfnOrDiscard
  forward body
  where
    bindfnOrDiscard ((Nothing, _), _) = pure ()
    bindfnOrDiscard ((Just vn, _), indexfn) = void (bindfn vn [indexfn])
forward (E.Literal (E.BoolValue x) _) =
  pure . fromScalar . sym2SoP $ Bool x
forward (E.Literal (E.SignedValue (E.Int64Value x)) _) =
  pure . fromScalar . int2SoP $ toInteger x
forward (E.IntLit x _ _) =
  pure . fromScalar $ int2SoP x
forward (E.Negate x _) = do
  -- Numeric negation.
  fns <- forward x
  forM fns $ \fn -> do
    pure $
      IndexFn
        { iterator = iterator fn,
          body = cases [(p, negSoP v) | (p, v) <- casesToList (body fn)]
        }
forward e@(E.Var (E.QualName _ vn) _ _) = do
  bindings <- getIndexFns
  case M.lookup vn bindings of
    Just indexfns -> do
      pure indexfns
    _ -> do
      size <- getSize e
      case size of
        Just sz -> do
          -- Canonical array representation.
          i <- newVName "i"
          pure
            [ IndexFn
                { iterator = Forall i (Iota sz),
                  body = singleCase . sym2SoP $ Idx (Var vn) (sym2SoP $ Var i)
                }
            ]
        Nothing ->
          -- Canonical scalar representation.
          pure [IndexFn Empty (singleCase . sym2SoP $ Var vn)]
forward (E.TupLit xs _) = do
  mconcat <$> mapM forward xs
forward (E.AppExp (E.Index e_xs slice _loc) _)
  | [E.DimFix e_idx] <- slice = do
      f_xss <- forward e_xs
      f_idxs <- forward e_idx
      forM (zip f_xss f_idxs) $ \(f_xs, f_idx) -> do
        xs <- case justVName e_xs of
          Just vn -> newName vn
          Nothing -> newVName "I_xs"
        idx <- newVName "I_idx"

        -- We don't use substParams on f_xs because the substitution
        -- might fail here (e.g., if we are inside a map lambda).
        insertIndexFn xs [f_xs]
        -- Lift idx.
        unless (iterator f_idx == Empty) $ error "E.Index: internal error"
        i <- newVName "i"
        insertIndexFn idx [IndexFn (Forall i (Iota $ int2SoP 1)) (body f_idx)]
        subst
          (IndexFn Empty $ singleCase . sym2SoP $ Idx (Var xs) (sym2SoP $ Idx (Var idx) (int2SoP 0)))
forward (E.Not e _) = do
  fns <- forward e
  forM fns $ \fn -> do
    rewrite $
      IndexFn (iterator fn) $
        cmapValues (mapSymSoP (sym2SoP . neg)) (body fn)
forward (E.AppExp (E.BinOp (op', _) _ (x', _) (y', _) _) _)
  | E.baseTag (E.qualLeaf op') <= E.maxIntrinsicTag,
    name <- E.baseString $ E.qualLeaf op',
    Just bop <- L.find ((name ==) . prettyString) [minBound .. maxBound :: E.BinOp] = do
      vxs <- forward x'
      vys <- forward y'
      forM (zip vxs vys) $ \(vx, vy) -> do
        a <- newVName "I_a"
        b <- newVName "I_b"
        let doOp op =
              substParams
                (IndexFn Empty (singleCase $ op (Var a) (Var b)))
                [(a, vx), (b, vy)]
        case bop of
          E.Plus -> doOp (~+~)
          E.Times -> doOp (~*~)
          E.Minus -> doOp (~-~)
          E.Equal -> doOp (~==~)
          E.NotEqual -> doOp (~/=~)
          E.Less -> doOp (~<~)
          E.Greater -> doOp (~>~)
          E.Leq -> doOp (~<=~)
          E.Geq -> doOp (~>=~)
          E.LogAnd -> doOp (~&&~)
          E.LogOr -> doOp (~||~)
          _ -> error ("forward not implemented for bin op: " <> show bop)
forward (E.AppExp (E.If e_c e_t e_f _) _) = do
  cs <- forward e_c
  let f_c = case cs of
        [v] -> v
        _ -> error "If on tuple?"
  unless (iterator f_c == Empty) $ error "Condition in if-statement is an array?"

  -- Create boolean expression for true and false branches.
  c_t_branch <-
    rewrite $
      foldl1 (:||) [p :&& sop2Symbol q | (p, q) <- casesToList $ body f_c]
  c_f_branch <- rewrite (neg c_t_branch)

  ts <- rollbackAlgEnv $ do
    assume c_t_branch
    forward e_t
  fs <- rollbackAlgEnv $ do
    assume c_f_branch
    forward e_f

  cond <- newVName "if-condition"
  t_branch <- newVName "t_branch"
  f_branch <- newVName "f_branch"
  let fn_if =
        IndexFn
          (iterator f_c)
          ( cases
              [ (Var cond, sym2SoP $ Var t_branch),
                (neg $ Var cond, sym2SoP $ Var f_branch)
              ]
          )
  forM (zip ts fs) $ \(t, f) -> do
    substParams fn_if [(cond, f_c), (t_branch, t), (f_branch, f)]
      >>= rewrite
forward (E.Lambda _ _ _ _ loc) =
  errorMsg loc "Cannot create index function for unapplied lambda."
forward expr@(E.AppExp (E.Apply f args loc) _)
  | Just fname <- getFun f,
    "map" `L.isPrefixOf` fname,
    E.Lambda params lam_body _ _ _ : _args <- getArgs args,
    Just arrays <- NE.nonEmpty (NE.tail args) = do
      (aligned_args, _aligned_sizes) <- zipArgs loc params arrays
      iter <- bindLambdaBodyParams (mconcat aligned_args)
      bodies <- forward lam_body

      forM bodies $ \body_fn -> do
        subst (IndexFn iter (body body_fn))
          >>= rewrite
  | Just fname <- getFun f,
    "map" `L.isPrefixOf` fname = do
      -- No need to handle map non-lambda yet as program can just be rewritten.
      etaExpandErrorMsg loc (head $ getArgs args)
  | Just "replicate" <- getFun f,
    [e_n, e_x] <- getArgs args = do
      ns <- forward e_n
      xs <- forward e_x
      forM (zip ns xs) $ \(n, x) -> do
        i <- newVName "i"
        case x of
          IndexFn Empty body -> do
            case n of
              IndexFn Empty cs -> do
                m <- rewrite $ flattenCases cs
                rewrite $ IndexFn (Forall i (Iota m)) body
              _ ->
                errorMsg loc "type error"
          _ -> do
            errorMsg loc "Multi-dimensional arrays not supported yet."
  | Just "iota" <- getFun f,
    [e_n] <- getArgs args = do
      ns <- forward e_n
      forM ns $ \n -> do
        i <- newVName "i"
        case n of
          IndexFn Empty cs -> do
            m <- rewrite $ flattenCases cs
            rewrite $ IndexFn (Forall i (Iota m)) (singleCase . sym2SoP $ Var i)
          _ ->
            errorMsg loc "type error"
  | Just fname <- getFun f,
    "zip" `L.isPrefixOf` fname = do
      mconcat <$> mapM forward (getArgs args)
  | Just fname <- getFun f,
    "unzip" `L.isPrefixOf` fname,
    [xs'] <- getArgs args =
      -- XXX unzip is a no-op.
      forward xs'
  | Just "scan" <- getFun f,
    [E.OpSection (E.QualName [] vn) _ _, _ne, xs'] <- getArgs args = do
      -- Scan with basic operator.
      fns <- forward xs'
      forM fns $ \fn -> do
        let i = case iterator fn of
              (Forall i' _) -> i'
              Empty -> error "scan array is empty?"
        -- TODO should we verify that _ne matches op?
        op <-
          case E.baseString vn of
            "+" -> pure (~+~)
            "-" -> pure (~-~)
            "*" -> pure (~*~)
            "&&" -> pure (~&&~)
            _ -> error ("scan not implemented for bin op: " <> show vn)
        let base_case = sym2SoP (Var i) :== int2SoP 0
        x <- newVName "a"
        let y =
              IndexFn
                (iterator fn)
                ( cases
                    [ (base_case, sym2SoP (Idx (Var x) (sVar i))),
                      (neg base_case, Recurrence `op` Idx (Var x) (sVar i))
                    ]
                )
        -- tell ["Using scan rule ", toLaTeX y]
        y @ (x, fn)
          >>= rewrite
  | Just "scan" <- getFun f,
    [E.Lambda params lam_body _ _ _, _ne, _xs] <- getArgs args,
    xs <- NE.fromList [NE.last args],
    [pat_acc, pat_x] <- params = do
      -- We pick the first argument of the lambda to be the accumulator
      -- and the second argument to be an element of the input array.
      -- (The lambda is associative, so we are free to pick.)
      (aligned_args, _) <- zipArgs loc [pat_x] xs

      iter <- bindLambdaBodyParams (mconcat aligned_args)
      let accToRec = M.fromList (map (,sym2SoP Recurrence) $ E.patNames pat_acc)
      bodies <- map (repIndexFn accToRec) <$> forward lam_body

      forM bodies $ \body_fn -> do
        subst (IndexFn iter (body body_fn))
          >>= rewrite
  | Just "scatter" <- getFun f,
    [e_dest, e_inds, e_vals] <- getArgs args = do
      -- `scatter dest is vs` calculates the equivalent of this imperative code:
      --
      -- ```
      -- for k in 0 .. length is:
      --   i = is[k]
      --   if i >= 0 && i < length dest:
      --     dest[i] = vs[k]
      -- ```
      --
      -- Scatter generic:
      --   - The indices are integers, so they are ordered.
      --   - Let P be the permutation that sorts indices in ascending order.
      --   - Then ... TODO
      --
      dests <- forward e_dest >>= mapM rewrite
      indss <- forward e_inds >>= mapM rewrite
      valss <- forward e_vals >>= mapM rewrite

      forM (zip3 dests indss valss) $ \(dest, inds, vals) -> do
        -- 1. Scatter in-bounds-monotonic indices:
        rule1 <- runMaybeT $ scatterMono dest inds vals
        rule2 <- runMaybeT $ scatterPerm dest inds vals e_inds
        maybe
          (errorMsg loc "Failed to infer index function for scatter.")
          pure
          (rule1 <|> rule2)
  | Just "sized" <- getFun f,
    [_, e] <- getArgs args = do
      -- No-op.
      forward e
  | Just vn <- getFun f,
    vn `S.member` refinementPrelude = do
      forwardRefPrelude loc expr vn args
  -- Applying other functions, for instance, user-defined ones.
  | (E.Var (E.QualName [] g) info loc') <- f,
    E.Scalar (E.Arrow _ _ _ _ (E.RetType _ return_type)) <- E.unInfo info = do
      toplevel <- getTopLevelIndexFns

      -- A top-level definition with an index function.
      case M.lookup g toplevel of
        Just (pats, indexfns) -> do
          printM 1337 $ "✨ Using index fn " <> prettyBinding' g indexfns
          (actual_args, _, size_rep) <- handleArgs loc' g pats

          forM indexfns $ \fn -> do
            substParams (repIndexFn size_rep fn) (mconcat actual_args)
              >>= rewrite
        Nothing -> do
          defs <- getTopLevelDefs

          -- A top-level definition without an index function.
          case M.lookup g defs of
            -- NOTE This "inlines" the definition of top-level definition,
            -- as opposed to the treatment for "top-level index functions"
            -- where the args are substituted into the previously analyzed
            -- index function. (Less work, but more likely to fail.
            -- For example, substituting into a Sum fails in some cases.)
            Just (pats, e) -> do
              printM 1337 $ "✨ Using top-level def " <> prettyString g
              (actual_args, actual_sizes, _) <- handleArgs loc' g pats

              forM_ (mconcat actual_sizes) $ \(n, sz) -> do
                addEquiv (Algebra.Var n) =<< toAlgebra sz
              forM_ (mconcat actual_args) $ \(vn, fn) ->
                void $ bindfn_ 1337 vn [fn]
              forward e
            Nothing -> do
              -- g is a free variable in this expression (probably a parameter
              -- to the top-level function currently being analyzed).
              arg_fns <- mconcat <$> mapM forward (getArgs args)
              size <- sizeOfTypeBase return_type
              arg_names <- forM arg_fns (const $ newVName "x")
              iter <- case size of
                Just sz ->
                  flip Forall (Iota sz) <$> newVName "i"
                Nothing ->
                  pure Empty
              when (typeIsBool return_type) $ addProperty (Algebra.Var g) Algebra.Boolean

              let g_fn =
                    IndexFn
                      { iterator = iter,
                        body =
                          singleCase . sym2SoP $
                            Apply (Var g) (map (sym2SoP . Var) arg_names)
                      }
              fn <- substParams g_fn (zip arg_names arg_fns)
              pure [fn]
  where
    handleArgs loc' g pats = do
      (actual_args, actual_sizes) <- zipArgs loc' pats args
      let size_rep = M.fromList $ mconcat actual_sizes
      whenDebug . traceM $
        "Size variable replacement " <> prettyString size_rep

      checkPreconditions actual_args size_rep

      pure (actual_args, actual_sizes, size_rep)
      where
        checkPreconditions actual_args size_rep = do
          foldM_
            ( \args_in_scope (pat, arg) -> do
                let scope = args_in_scope <> arg
                checkPatPrecondition scope pat size_rep
                pure scope
            )
            []
            (zip pats actual_args)

        checkPatPrecondition scope pat size_rep = do
          cond <- getPrecondition pat
          ans <- case cond of
            Nothing -> pure Yes
            Just check -> do
              whenDebug . traceM $
                "Checking precondition " <> prettyString pat <> " for " <> prettyString g
              check (size_rep, scope)
          unless (isYes ans) . errorMsg loc $
            "Failed to show precondition " <> prettyString pat <> " in context: " <> prettyString scope
forward e = error $ "forward on " <> show e <> "\nPretty: " <> prettyString e

-- Align parameters and arguments. Each parameter is a pattern.
-- A pattern unpacks to a list of (optional) names with type information.
-- An argument is an expression, which `forward` will, correspondingly,
-- return a list of index functions for.
-- Patterns and arguments must align---otherwise an error is raised.
zipArgs ::
  E.SrcLoc ->
  [E.Pat E.ParamType] ->
  NE.NonEmpty (a, E.Exp) ->
  IndexFnM ([[(E.VName, IndexFn)]], [[(E.VName, SoP Symbol)]])
zipArgs loc formal_args actual_args = do
  let pats = map patternMapAligned formal_args
  args <- mapM forward (getArgs actual_args)

  unless (length pats == length args) $
    errorMsg loc "Functions must be fully applied. Maybe you want to eta-expand?"
  unless (map length pats == map length args) $
    errorMsg loc "Internal error: actual argument does not match parameter pattern."

  -- Discard unused parameters such as wildcards while maintaining alignment.
  let aligned_args = do
        (pat, arg) <- zip pats args
        pure . catMaybes $ zipWith (\vn fn -> (,fn) <$> vn) (map fst pat) arg

  -- When applying bound functions size parameters must be replaced as well.
  -- (E.g., if we are zipping index functions with use of a top-level definition.)
  aligned_sizes <- forM (zip pats args) $ \(pat, arg) -> do
    let types = map snd pat
    size_names <- mapM (fmap (>>= getVName) . sizeOfTypeBase) types
    arg_sizes <- mapM sizeOfDomain arg
    -- Assert that if there is a size parameter, then we have a size to bind it to.
    when (any (\(vn, sz) -> isJust vn && isNothing sz) (zip size_names arg_sizes)) $
      errorMsg loc "Internal error: sizes don't align."
    pure $ catMaybes $ zipMaybes size_names arg_sizes

  pure (aligned_args, aligned_sizes)
  where
    getVName x | Just (Var vn) <- justSym x = Just vn
    getVName _ = Nothing

    sizeOfDomain (IndexFn Empty _) = pure Nothing
    sizeOfDomain (IndexFn (Forall _ d) _) =
      Just <$> rewrite (domainEnd d .-. domainStart d .+. int2SoP 1)

    zipMaybes = zipWith (liftA2 (,))

substParams :: (Foldable t) => IndexFn -> t (E.VName, IndexFn) -> IndexFnM IndexFn
substParams = foldM substParam
  where
    -- We want to simplify, but avoid rewriting recurrences during
    -- paramter-substitution.
    substParam fn (paramName, paramIndexFn) =
      (fn @ (paramName, paramIndexFn)) >>= rewriteWithoutRules

-- Scatter permutation indices:
--   - If `is` is a permutation of (0 .. length dst), then the
--     inverse of `is` exists and the scatter is equivalent to a gather:
--     ```
--     for i in 0 .. length dest:
--         k = is^(-1)(i)
--         dest[i] = vs[k]
--     ```
--   - Rule:
--     (`is` is a permutation of (0 .. length dst))
--     ___________________________________________________
--     y = ∀i ∈ 0 .. length dest . vs[is^(-1)(i)]
scatterPerm :: IndexFn -> IndexFn -> IndexFn -> E.Exp -> MaybeT IndexFnM IndexFn
scatterPerm (IndexFn (Forall _ dom_dest) _) inds vals e_inds = do
  dest_size <- lift $ rewrite $ domainEnd dom_dest
  perm <- lift $ prove (PermutationOfZeroTo dest_size) inds
  case perm of
    Unknown -> fail "scatterPerm: no match"
    Yes -> do
      vn_inds <- warningInds

      -- No out-of-bounds and no duplicate indices.
      vn_vals <- newVName "vals"
      i <- newVName "i"
      vn_inv <- newVName (E.baseString vn_inds <> "⁻¹")

      let inv_ind = Idx (Var vn_inv) (sym2SoP (Var i))
      lift $
        IndexFn
          { iterator = Forall i (Iota dest_size),
            body = cases [(Bool True, sym2SoP $ Idx (Var vn_vals) (sym2SoP inv_ind))]
          }
          @ (vn_vals, vals)
  where
    warningInds
      | Just vn <- justVName e_inds = pure vn
      | otherwise = do
          warningMsg (E.locOf e_inds) $
            "You might want to bind scattered indices to a name to aid"
              <> " index function inference: "
              <> prettyString e_inds
          fail ""
scatterPerm _ _ _ _ = fail ""

-- Scatter in-bounds-monotonic indices:
--   - If `is` is a monotonically increasing sequence of values
--     that starts at 0 and ends at length dest-1, we can express
--     the scatter as an index function by the following rule:
--
--     is = ∀k ∈ [0, ..., m-1] .
--         | seg(k+1) - seg(k) > 0  => seg(k)
--         | seg(k+1) - seg(k) <= 0 => OOB
--     seg(0) is 0
--     seg(k) is monotonically increasing
--     dest has size seg(m) - 1         (to ensure conclusion covers all of dest)
--     OOB < 0 or OOB >= seg(m) - 1
--     _________________________________________________
--     y = ∀i ∈ ⊎k=iota m [seg(k), ..., seg(k+1) - 1] .
--         | i == seg(k) => vals[k]
--         | i /= seg(k) => dest[i]
--
--   - By the semantics of scatter, the conclusion is:
--     y = ∀i ∈ ⊎k=iota m [seg(k), ..., seg(k+1) - 1] .
--         | i == is[k] => vals[k]
--         | i /= is[k] => dest[i]
--     Substituting is[k]
--     y = ∀i ∈ ⊎k=iota m [seg(k), ..., seg(k+1) - 1] .
--         | i == seg(k) ^ seg(k+1) - seg(k) > 0 => vals[k]
--         | i == seg(k) ^ seg(k+1) - seg(k) <= 0 => vals[k]
--         | i /= seg(k) => dest[i]
--     Since i ∈ [seg(k), ..., seg(k+1) - 1], we have
--     y = ∀i ∈ ⊎k=iota m [seg(k), ..., seg(k+1) - 1] .
--         | i == seg(k) ^ True => vals[k]
--         | i == seg(k) ^ False => vals[k]
--         | i /= seg(k) => dest[i]
--     which simplifies to what we wanted to show.
scatterMono :: IndexFn -> IndexFn -> IndexFn -> MaybeT IndexFnM IndexFn
scatterMono dest@(IndexFn (Forall _ dom_dest) _) inds@(IndexFn inds_iter@(Forall k (Iota m)) _) vals = do
  dest_size <- lift $ rewrite $ domainEnd dom_dest
  -- Match rule.
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
  s <- hoistMaybe =<< lift (unify inds_template inds)
  -- Determine which is OOB and which is e1.
  let isOOB ub = CaseCheck (\c -> c :< int2SoP 0 :|| (mapping s M.! ub) :<= c)
  (vn_p_seg, vn_f_seg) <- do
    case0_is_OOB <- lift $ askQ (isOOB vn_f1) inds 0
    case case0_is_OOB of
      Yes -> pure (vn_p1, vn_f1)
      Unknown -> do
        case1_is_OOB <- lift $ askQ (isOOB vn_f0) inds 1
        case case1_is_OOB of
          Yes -> pure (vn_p0, vn_f0)
          Unknown -> failM "scatterMono: unable to determine OOB branch"
  let p_seg = sop2Symbol $ mapping s M.! vn_p_seg
  let f_seg = mapping s M.! vn_f_seg
  -- Check that p_seg = f_seg(k+1) - f_seg(k) > 0.
  s_p :: Maybe (Substitution Symbol) <- lift $ algebraContext inds $ do
    addRelIterator inds_iter
    seg_delta <- rewrite $ rep (mkRep k (sVar k .+. int2SoP 1)) f_seg .-. f_seg
    unify p_seg (seg_delta :> int2SoP 0)
  when (isNothing s_p) (failM "scatterMono: predicate not on desired form")
  -- Check that seg is monotonically increasing. (Essentially checking
  -- that OOB branch is never taken in inds.)
  mono <- lift $ algebraContext inds $ do
    addRelIterator inds_iter
    seg_delta <- rewrite $ rep (mkRep k (sVar k .+. int2SoP 1)) f_seg .-. f_seg
    seg_delta $>= int2SoP 0
  when (isUnknown mono) (failM "scatterMono: unable to show monotonicity")
  -- Check that seg(0) = 0.
  -- (Not using CaseCheck as it has to hold outside case predicate.)
  let x `at_k` i = rep (mkRep k i) x
  let zero :: SoP Symbol = int2SoP 0
  eq0 <- lift $ f_seg `at_k` zero $== int2SoP 0
  when (isUnknown eq0) (failM "scatterMono: unable to determine segment start")

  -- Check that the proposed end of segments seg(m) - 1 equals the size of dest.
  -- (Note that has to hold outside the context of inds, so we cannot assume p_seg.)
  domain_covered <- lift $ f_seg `at_k` m .-. int2SoP 1 $== dest_size
  when (isUnknown domain_covered) $
    fail "scatter: segments do not cover iterator domain"

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
  lift $ substParams fn [(vals_hole, vals), (dest_hole, dest)]
  where
    failM msg = do
      printM 1338 msg
      fail msg
scatterMono _ _ _ = fail ""

cmap :: ((a, b) -> (c, d)) -> Cases a b -> Cases c d
cmap f (Cases xs) = Cases (fmap f xs)

cmapValues :: (b -> c) -> Cases a b -> Cases a c
cmapValues f = cmap (second f)

sVar :: E.VName -> SoP Symbol
sVar = sym2SoP . Var

-- TODO eh bad
(~==~) :: Symbol -> Symbol -> SoP Symbol
x ~==~ y = sym2SoP $ sym2SoP x :== sym2SoP y

(~/=~) :: Symbol -> Symbol -> SoP Symbol
x ~/=~ y = sym2SoP $ sym2SoP x :/= sym2SoP y

(~<~) :: Symbol -> Symbol -> SoP Symbol
x ~<~ y = sym2SoP $ sym2SoP x :< sym2SoP y

(~>~) :: Symbol -> Symbol -> SoP Symbol
x ~>~ y = sym2SoP $ sym2SoP x :> sym2SoP y

(~<=~) :: Symbol -> Symbol -> SoP Symbol
x ~<=~ y = sym2SoP $ sym2SoP x :<= sym2SoP y

(~>=~) :: Symbol -> Symbol -> SoP Symbol
x ~>=~ y = sym2SoP $ sym2SoP x :>= sym2SoP y

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
getRefinement (E.Id param (E.Info {E.unInfo = info}) _loc)
  | E.Array _ _ (E.Refinement _ty ref) <- info = do
      whenDebug . traceM $ "Getting (array) type refinement " <> prettyString (param, ref)
      hole <- sym2SoP . Hole <$> newVName "h"
      Just <$> mkRef ((`Idx` hole) . Var) ref
  | E.Scalar (E.Refinement _ty ref) <- info = do
      whenDebug . traceM $ "Getting type refinement " <> prettyString (param, ref)
      Just <$> mkRef Var ref
  where
    mkRef wrap (E.OpSectionRight (E.QualName [] vn_op) _ e_y _ _ _) = do
      let rel = case E.baseString vn_op of
            ">" -> (:>)
            ">=" -> (:>=)
            "<" -> (:<)
            "<=" -> (:<=)
            "==" -> (:==)
            "!=" -> (:/=)
            _ -> undefined
      ys <- forwardRefinementExp e_y
      -- Create check as an index function whose cases contain the refinement.
      let check =
            mkCheck $
              IndexFn Empty . cases $
                map (second (sym2SoP . (sym2SoP (Var param) `rel`))) (casesToList ys)
      let effect = do
            -- (We allow Holes in wrap and toAlgebra cannot be called on symbols with Holes.)
            alg_vn <- paramToAlgebra param wrap
            y <- rewrite $ flattenCases ys
            addRelSymbol $ sym2SoP (Var alg_vn) `rel` y
      pure (check, effect)
    mkRef wrap (E.Lambda lam_params lam_body _ _ _) = do
      let param_names = map fst $ mconcat $ map patternMapAligned lam_params
      case param_names of
        [lam_param] -> do
          body <- forwardRefinementExp lam_body
          let ref = case lam_param of
                Just vn -> repCases (mkRep vn $ wrap param) body
                Nothing -> body
          let check = mkCheck $ IndexFn Empty ref
          let effect = do
                y <- rewrite $ flattenCases ref
                addRelSymbol (sop2Symbol y)
          pure (check, effect)
        _ ->
          error "Impossible: Refinements have type t -> bool."
    mkRef _ x = error $ "Unhandled refinement predicate " <> show x

    forwardRefinementExp e = do
      fns <- forward e
      case fns of
        [fn] -> body <$> rewrite fn
        _ -> error "Impossible: Refinements have return type bool."

    -- Check that all branches of check_fn evaluate to true
    -- when substituting in param_subst.
    mkCheck check_fn (size_rep, args_in_scope) = do
      -- Substitute parameters in scope.
      -- The refinement of a parameter can use previous parameters:
      --   (x : []i64) (n : {i64 | (== sum x))
      -- NOTE unpacked record-types are disallowed:
      --   (x : {(t1, t2) | \(a, b) -> ...})
      check <- substParams (repIndexFn size_rep check_fn) args_in_scope
      askRefinement check
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

-- Lowerbounds size variables by 0.
addSizeVariables :: E.PatBase E.Info E.VName E.ParamType -> IndexFnM ()
addSizeVariables (E.PatParens pat _) = addSizeVariables pat
addSizeVariables (E.PatAscription pat _ _) = addSizeVariables pat
addSizeVariables (E.Id _ (E.Info {E.unInfo = E.Array _ shp _}) _) = do
  mapM_ addSize (E.shapeDims shp)
  where
    addSize (E.Var (E.QualName _ d) _ _) = do
      alg_d <- toAlgebra (sym2SoP $ Var d)
      addRel (alg_d :>=: int2SoP 0)
    addSize _ = pure ()
addSizeVariables (E.Id param (E.Info {E.unInfo = t}) _) = do
  when (typeIsBool t) $ addProperty (Algebra.Var param) Algebra.Boolean
addSizeVariables _ = pure ()

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
  forM_ (zip (map fst params) fns) $ \(paramName, f_xs) -> do
    vn <- newVName ("I_lam_" <> E.baseString paramName)
    insertIndexFn vn [f_xs]
    insertIndexFn
      paramName
      [IndexFn Empty $ singleCase . sym2SoP $ Idx (Var vn) (sVar i)]
  pure iter

errorMsg :: (E.Located a1) => a1 -> [Char] -> a2
errorMsg loc msg =
  error $
    "Error at " <> prettyString (E.locText (E.srclocOf loc)) <> ": " <> msg

etaExpandErrorMsg :: E.SrcLoc -> E.Exp -> a
etaExpandErrorMsg loc fn =
  errorMsg loc $
    "Only anonymous functions may be passed as argument. Perhaps you want to eta-expand: "
      <> prettyString fn

warningMsg :: (Monad m, E.Located a) => a -> String -> m ()
warningMsg loc msg = do
  printM 1 . warningString $
    prettyString (E.locText (E.srclocOf loc)) <> ": " <> msg

forwardRefPrelude :: E.SrcLoc -> E.Exp -> String -> NE.NonEmpty (a4, E.Exp) -> IndexFnM [IndexFn]
forwardRefPrelude loc e f args = do
  -- case f of
  -- "segments"
  --   | E.Lambda lam_params lam_body _ _ _ : _ <- getArgs args,
  --     Just lam_args <- NE.nonEmpty (NE.tail args) -> do
  --       debugPrettyM "segments" f
  --       -- No-op.
  --       (aligned_args, _) <- zipArgs loc lam_params lam_args
  --       case mconcat aligned_args of
  --         [(vn_seg, fn)] -> do
  --           seg <- getSegment fn
  --           case seg of
  --             Just (seg_start, seg_end) -> do
  --               -- bind lambda body params to segment
  --               -- forward on lam_body
  --               debugPrettyM "segment:" seg
  --               iter <- bindLambdaBodyParams (mconcat aligned_args)
  --               fns <- quantifiedBy iter $ forward lam_body

  --               forM fns $ \body_fn ->
  --                 if iterator body_fn == Empty
  --                   then rewrite $ IndexFn iter (body body_fn)
  --                   else error "scan: Non-scalar body."

  --               undefined
  --             Nothing ->
  --               errorMsg loc "not segmented"
  --         _ ->
  --           undefined
  -- "segments" ->
  --   etaExpandErrorMsg loc (head $ getArgs args)
  -- _ -> do
  (effect, xss) <- parsePrelude f args
  forM xss $ \xs -> do
    fromPreludeEffect xs $ effect xs
  where
    -- inPrelude :: E.Exp -> Bool
    -- inPrelude (E.Var (E.QualName _ vn) _ _) =
    --   E.baseString vn `S.member` refinementPrelude
    -- inPrelude (E.AppExp (E.Apply g _ _) _)
    --   | Just vn <- getFun g =
    --       vn `S.member` refinementPrelude
    -- inPrelude _ = False

    fromPreludeEffect fn m = do
      ans <- m
      case ans of
        Yes ->
          pure $ IndexFn Empty (cases [(Bool True, int2SoP 1)])
        _ ->
          errorMsg loc $
            "Failed to show: "
              <> prettyString e
              <> "\nIndex function:\n"
              <> prettyString fn

-- getSegment (IndexFn (Forall _ dom@(Cat _ _ b)) _) = do
--   end <- rewrite $ intervalEnd dom
--   pure (Just (b, end))
-- getSegment _ =
--   pure Nothing

parsePrelude :: String -> NE.NonEmpty (a, E.Exp) -> IndexFnM (IndexFn -> IndexFnM Answer, [IndexFn])
parsePrelude f args =
  case f of
    "injectiveOn" | [e_rng, e_xs] <- getArgs args -> do
      rng <- forward e_rng
      case rng of
        [IndexFn Empty cs_start, IndexFn Empty cs_end] -> do
          xss <- forward e_xs
          let start = flattenCases cs_start
          let end = flattenCases cs_end
          pure (prove (InjectiveOn start end), xss)
        _ ->
          undefined
    "and" | [e_xs] <- getArgs args -> do
      -- No-op: The argument e_xs is a boolean array; each branch will
      -- be checked in refinements.
      -- XXX but we lose information about iterator at check site, hm...
      xss <- forward e_xs
      pure (askRefinement, xss)
    _ ->
      undefined
