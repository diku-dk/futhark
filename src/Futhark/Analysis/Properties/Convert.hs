{-# LANGUAGE LambdaCase #-}

module Futhark.Analysis.Properties.Convert (mkIndexFnProg, mkIndexFnValBind) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, foldM_, forM, forM_, unless, void, when, zipWithM, (<=<))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Data.Set qualified as S
import Debug.Trace (trace)
import Futhark.Analysis.Properties.AlgebraBridge (algebraContext, fromAlgebra, simplify, toAlgebra)
import Futhark.Analysis.Properties.AlgebraBridge.Util
import Futhark.Analysis.Properties.AlgebraPC.Symbol qualified as Algebra
import Futhark.Analysis.Properties.Flatten (unflatten)
import Futhark.Analysis.Properties.IndexFn
import Futhark.Analysis.Properties.IndexFnPlus (dimSize, domainEnd, domainStart, index, intervalEnd, repCases, repIndexFn)
import Futhark.Analysis.Properties.Monad
import Futhark.Analysis.Properties.Property (MonDir (..), askRng, cloneProperty)
import Futhark.Analysis.Properties.Property qualified as Property
import Futhark.Analysis.Properties.Query
import Futhark.Analysis.Properties.Rewrite (rewrite, rewriteWithoutRules)
import Futhark.Analysis.Properties.Substitute (subst, (@))
import Futhark.Analysis.Properties.Symbol (Symbol (..), justVar, neg, sop2Symbol)
import Futhark.Analysis.Properties.SymbolPlus (repProperty)
import Futhark.Analysis.Properties.Unify
import Futhark.Analysis.Properties.Util
import Futhark.MonadFreshNames (VNameSource, newNameFromString, newVName)
import Futhark.SoP.Monad (addEquiv, addProperty, getProperties)
import Futhark.SoP.Refine (addRel)
import Futhark.SoP.SoP (Rel (..), SoP, int2SoP, justConstant, justSym, mapSymSoP, negSoP, sym2SoP, (.*.), (.+.), (.-.), (./.), (~*~), (~+~), (~-~))
import Futhark.Util.Pretty (Pretty)
import Language.Futhark qualified as E
import Language.Futhark.Semantic (FileModule (fileProg), ImportName, Imports)

propertyPrelude :: S.Set String
propertyPrelude =
  S.fromList
    [ "Monotonic",
      "Equiv",
      "Range",
      "Disjoint",
      "Injective",
      "InjectiveRCD",
      "BijectiveRCD",
      "FiltPartInv",
      "FiltPart",
      "FiltPartInv2",
      "FiltPart2",
      "and"
    ]

newIndexFn :: (Pretty u) => E.VName -> E.TypeBase E.Exp u -> IndexFnM [IndexFn]
newIndexFn _ (E.Scalar (E.Arrow {})) =
  -- Don't create index functions for function arguments---they're
  -- handled specially in `forward`. If we create an index function,
  -- `Substitute.subst` will attempt to substitute applications of this
  -- function, but its domain is undefined/scalar so the arguments
  -- will mismatch.
  pure []
newIndexFn vn t = do
  -- x :: [n](t, (t, t))
  --  -->
  --  [ for i < n . x.0(i),
  --    for i < n . x.1.0(i),
  --    for i < n . x.1.1(i)]
  array_shape <- shapeOf t
  printM 2 $ "### newIndexFn t " <> prettyStr t
  printM 2 $ "### tuple projections " <> prettyStr vn <> prettyStr (toTupleOfArrays t)
  fs <- mapM (createUninterpretedIndexFn array_shape <=< dots vn) (toTupleOfArrays t)
  printM 2 $ "### fs " <> prettyStr vn <> prettyStr fs
  insertIndexFn vn fs
  pure fs
  where
    name `dots` [] =
      -- Not a tuple; don't touch vn.
      pure name
    name `dots` proj =
      newNameFromString (E.baseString name <> concatMap (('.' :) . show) proj)

createUninterpretedIndexFn :: [SoP Symbol] -> E.VName -> IndexFnM IndexFn
createUninterpretedIndexFn [] vn =
  -- Uninterpreted scalar.
  pure $ IndexFn [] (singleCase $ sVar vn)
createUninterpretedIndexFn array_shape vn = do
  -- Uinterpreted array/tuple.
  ids <- mapM newVName (take (length array_shape) $ cycle ["i", "j", "k"])
  pure $
    IndexFn
      { shape = zipWith (\sz i -> [Forall i (Iota sz)]) array_shape ids,
        body = singleCase . sym2SoP $ Apply (Var vn) (map sVar ids)
      }

{-
    Construct index function for source program
-}
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

mkIndexFnValBind :: E.ValBind -> IndexFnM [IndexFn]
mkIndexFnValBind val@(E.ValBind _ vn (Just te) _ type_params params body _ _ val_loc)
  | hasRefinement te = do
      clearAlgEnv
      setOutputNames []
      printM 1 $
        emphString ("Analyzing " <> prettyStr (E.locText (E.srclocOf val_loc)))
          <> prettyStr val
      forM_ formal_args (uncurry newIndexFn)
      forM_ params addPreconditions
      forM_ params addBooleanNames
      forM_ size_vars addSizeVariable
      indexfns <- forward body >>= mapM rewrite >>= bindfn vn
      checkPostcondition vn indexfns te
      insertTopLevel vn (mkApplyIndexFn vn size_vars params te indexfns)
      out <- mapM (changeScope (S.fromList $ size_vars <> arg_vars)) indexfns
      printM 1 $ prettyBinding vn out
      pure out
  where
    formal_args = concatMap E.patternMap params
    arg_vars = map fst formal_args
    size_vars =
      mapMaybe
        ( \case
            E.TypeParamDim vn_dim _ -> Just vn_dim
            E.TypeParamType {} -> Nothing
        )
        type_params
    -- Adds the effect of a precondition without checking that it holds.
    addPreconditions pat = do
      printM 1 $ "+ Adding precondition on " <> prettyStr pat
      ref <- getRefinement pat
      forM_ ref $ \(_, effect) -> effect emptyCheckContext
      printAlgEnv 3
mkIndexFnValBind (E.ValBind _ vn _ _ _ params body _ _ _) = do
  insertTopLevel vn (mkApplyDef vn params body)
  pure []

bindfn :: E.VName -> [IndexFn] -> IndexFnM [IndexFn]
bindfn = bindfn_ 1

bindfn_ :: Int -> E.VName -> [IndexFn] -> IndexFnM [IndexFn]
bindfn_ level vn indexfns = do
  insertIndexFn vn indexfns
  printM level $ prettyBinding vn indexfns
  pure indexfns

checkPostcondition :: E.VName -> [IndexFn] -> E.TypeExp E.Exp E.VName -> IndexFnM ()
checkPostcondition vn indexfns te = do
  case getTERefine te of
    [e@(E.Lambda lam_params lam_body _ _ _)] -> do
      actual_names <- getOutputNames
      let param_names = map fst $ mconcat $ map patternMapAligned lam_params
      -- Propagate properties on the final let-body expression
      -- to the postcondition lambda parameters
      -- WARN Assumes that the final let-body is in ANF: let ... in (x,...,z).
      when (length actual_names == length param_names) $ do
        let aligned = catMaybes $ zipWith (\x y -> (x,) <$> (sym2SoP . Var <$> y)) actual_names param_names
        let name_rep = mkRepFromList aligned

        forM_ aligned $ \(actual, _) -> do
          props <- (M.!? Algebra.Var actual) <$> getProperties
          case S.toList <$> props of
            Just ps ->
              forM_ ps $ (addRelSymbol . Prop . repProperty name_rep) <=< fromAlgebra
            Nothing -> pure ()

      -- Substitute lam_params for indexfns and perform checks.
      printM 1 $ warningString "Checking postcondition: " <> prettyStr te
      forM_ (zip param_names indexfns) $ \(nm, fn) ->
        when (isJust nm) . void $ bindfn (fromJust nm) [fn]
      postconds <- forward lam_body >>= mapM rewrite
      printM 1 $
        "Postconditions after substituting in results:\n  "
          <> prettyStr postconds

      answer <- askRefinements postconds
      case answer of
        Yes -> do
          printM 1 (E.baseString vn <> greenString " OK\n\n")
          pure ()
        Unknown ->
          error . errorMsg (E.locOf e) $
            "Failed to show postcondition:\n" <> prettyStr e <> "\n" <> prettyStr postconds
    _ -> error "Tuples of postconditions not implemented yet."

-- Used to ensure that returns of top-level definitions are either expressed
-- only over the formal arguments or uninterpreted.
--
-- Restricts `f`'s free variables to be in `newScope`. Returns
--  * `f`, if `f` only references variables in `newScope`.
--  * A fresh uninterpreted function, otherwise.
-- Preserves f's shape, if this only depends on in-scope variables, regardless.
changeScope :: S.Set E.VName -> IndexFn -> IndexFnM IndexFn
changeScope newScope f
  | fv f `S.isSubsetOf` newScope = pure f
  | fv (shape f) `S.isSubsetOf` newScope = do
      g <- mkUinterpreted f
      pure $ g {shape = shape f}
  | otherwise = mkUinterpreted f

uninterpretedName :: String
uninterpretedName = "<f>"

mkUinterpreted :: IndexFn -> IndexFnM IndexFn
mkUinterpreted f = do
  new_shape <- forM (shape f) . mapM $ \(Forall i _) ->
    Forall i . Iota . sym2SoP . Var <$> newNameFromString "<d>"
  x <- newNameFromString uninterpretedName
  pure $
    IndexFn
      { shape = new_shape,
        body = singleCase (sym2SoP $ Apply (Var x) (indexVars f))
      }

bodyIsUinterpreted :: IndexFn -> Bool
bodyIsUinterpreted f
  | Just (Apply (Var x) _) <- justSym =<< justSingleCase f =
      E.baseString x == uninterpretedName
  | otherwise = False

-- Apply a top-level definition with index function(s).
-- g, size_vars, pats, postcond, indexfns: The top-level definition.
-- loc, args: The source program apply.
mkApplyIndexFn ::
  E.VName ->
  [E.VName] ->
  [E.Pat E.ParamType] ->
  E.TypeExp E.Exp E.VName ->
  [IndexFn] ->
  E.SrcLoc ->
  EArgs ->
  IndexFnM (ApplyEffect, [IndexFn])
mkApplyIndexFn g size_vars pats postcond indexfns loc args = do
  printM 5 $ "✨ Using index fn " <> prettyStr g

  -- NOTE on why changeScope is called here.
  --
  -- 1. `g` may be uninterpreted when exiting the scope of its body.
  -- 2. Uninterpreted functions are of the form
  --          g = [for i < e . x(i), ...]
  -- 3. When `g` is applied to `args`, the arguments are not captured
  -- in the above uninterpreted function(s).
  -- 4. Therefore, two applications of `g` with different arguments are equal
  -- over the index domain: i = j => x(i) = x(j).
  --
  -- To fix this, the uninterpreted function name (`x`) must be generated
  -- afresh at _each application site_.
  --
  indexfns_scoped <- mapM (changeScope scope) indexfns

  (actual_args, actual_sizes) <- zipArgs loc pats args

  -- Assert that application is in ANF; rename formal arguments to actual
  -- arguments to check preconditions.
  let name_rep = renamingRep (mconcat actual_args) args
  size_rep <- checkPreconditions loc g pats (actual_args, actual_sizes) name_rep

  fs <- forM indexfns_scoped $ \fn -> do
    substParams (repIndexFn size_rep fn) (mconcat actual_args)
      >>= fmap (repIndexFn name_rep) . rewrite

  unless (all (\f -> fv f `S.disjoint` scope) fs) $
    error "mkApplyIndexFn: bound variable(s) captured."

  pure (mkEffectFromTypeExp size_rep (mconcat actual_args), fs)
  where
    scope = S.fromList $ size_vars <> (fst <$> concatMap E.patternMap pats)

    mkEffectFromTypeExp size_rep actual_args vns =
      forM_ (getTERefine postcond) $ \ref_exp -> do
        -- Bounds checking is disabled here because the bounds in the refinement
        -- of te was already checked previously (when creating the top level
        -- index funciton used here).
        -- We don't check bounds again because they might rely on a context
        -- that is now out of scope.
        (_check, effect) <- withoutBoundsChecks (mkRef vns ref_exp)
        effect (size_rep, actual_args, renamingRep actual_args args)

-- Apply a top-level definition without index function(s) (no postcondition).
mkApplyDef ::
  E.VName ->
  [E.Pat E.ParamType] ->
  E.Exp ->
  E.SrcLoc ->
  EArgs ->
  IndexFnM (ApplyEffect, [IndexFn])
mkApplyDef g pats e loc args = do
  -- NOTE This "inlines" the definition of the top-level definition, as opposed
  -- to the treatment for "top-level index functions" where the args are
  -- substituted into the previously analyzed index function.
  printM 5 $ "✨ Using top-level def " <> prettyStr g
  (actual_args, actual_sizes) <- zipArgs loc pats args
  let name_rep = renamingRep (mconcat actual_args) args
  _ <- checkPreconditions loc g pats (actual_args, actual_sizes) name_rep

  forM_ (mconcat actual_sizes) $ \(n, sz) -> do
    addEquiv (Algebra.Var n) =<< toAlgebra sz
  forM_ (mconcat actual_args) $ \(vn, fn) ->
    void $ bindfn_ 1337 vn [fn]
  (const $ pure (),) <$> forward e

-- Assert ANF to handle name substitutions in Properties.
--
-- HINT This limitation only exists because FiltPart Y X needs to substitute
-- the name X for the corresponding argument, for instance, in
--   def filter X p : \Y -> FiltPart Y X p _ = ...
--   def f ... =
--     let ys = filter p x
--     --> FiltPart ys x p _
-- and we want x to be meaningful to the user (i.e., to not bind X to fresh X'
-- and X' to x).
renamingRep :: (ReplacementBuilder (SoP Symbol) u) => [(E.VName, b)] -> NE.NonEmpty (a, E.Exp) -> Replacement u
renamingRep actual_args actual_arg_exprs =
  let arg_vns = checkANF (getArgs actual_arg_exprs)
   in mkRepFromList
        (zipWith (\k v -> (fst k, sym2SoP $ Var v)) actual_args arg_vns)
  where
    checkANF [] = []
    checkANF (arg : as) =
      case justVName arg of
        Just vn -> vn : checkANF as
        _ ->
          error . errorMsg (E.locOf arg) $
            "Limitation: Application of top-level definitions"
              <> " with postconditions must be in A-normal form: "
              <> prettyStr arg

-- Puts parameters in scope one-by-one before checking preconditions;
-- the refinement of a parameter can use previous parameters:
--   (x : []i64) (n : {i64 | (== sum x))
checkPreconditions :: (Pretty u, Pretty (E.Shape dim)) => E.SrcLoc -> E.VName -> [E.PatBase E.Info E.VName (E.TypeBase dim u)] -> ([[(E.VName, IndexFn)]], [[(E.VName, SoP Symbol)]]) -> M.Map E.VName (SoP Symbol) -> IndexFnM (M.Map E.VName (SoP Symbol))
checkPreconditions loc g pats (actual_args, actual_sizes) name_rep = do
  let size_rep = M.fromList $ mconcat actual_sizes
  foldM_
    ( \args_in_scope (pat, arg) -> do
        let scope = args_in_scope <> arg
        checkPatPrecondition scope pat size_rep
        pure scope
    )
    []
    (zip pats actual_args)

  pure size_rep
  where
    checkPatPrecondition scope pat size_rep = do
      conds <- getPrecondition pat
      answers <- forM conds $ \check -> do
        printM 1 $
          "Checking precondition " <> prettyStr pat <> " for " <> prettyStr g
        check (size_rep, scope, name_rep)
      unless (all isYes answers) $ do
        printAlgEnv 3
        error . errorMsg loc $
          "Failed to show precondition "
            <> prettyStr pat
            <> " in context: "
            <> prettyStr scope

{-
    Construct index function for source expression.
-}

forward :: E.Exp -> IndexFnM [IndexFn]
forward (E.Parens e _) = forward e
forward (E.Attr _ e _) = forward e
forward (E.AppExp e@(E.LetFun {}) _) =
  error $
    errorMsg (E.locOf e) "Only top-level function definitions are supported."
forward (E.AppExp (E.LetPat _ (E.Id vn _ _) x in_body _) _) = do
  trackNamesOfFinalLetBody in_body
  fs <- forwardLetEffects [Just vn] x
  bindfn vn fs >> forward in_body
forward (E.AppExp (E.LetPat _ (E.TuplePat patterns _) x body _) _) = do
  trackNamesOfFinalLetBody body
  let bound_names = map fst $ mconcat $ map patternMapAligned patterns
  fs <- forwardLetEffects bound_names x
  forM_ (zip (mconcat $ map patternMapAligned patterns) fs) bindfnOrDiscard
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
  map negf <$> forward x
  where
    negf f = f {body = cases [(p, negSoP v) | (p, v) <- casesToList (body f)]}
forward e@(E.Var (E.QualName _ vn) _ _) = do
  bindings <- getIndexFns
  case M.lookup vn bindings of
    Just indexfns -> pure indexfns
    _ -> newIndexFn vn (E.typeOf e)
forward (E.TupLit xs _) = do
  mconcat <$> mapM forward xs
forward expr@(E.AppExp (E.Index e_xs slice loc) _)
  -- Indexing on the form e_1[e_2].
  | [E.DimFix e_idx] <- slice = do
      f_xss <- forward e_xs
      f_idxs <- forward e_idx
      forM (zip f_xss f_idxs) $ \(f_xs, f_idx) -> do
        unless (rank f_xs == 1) $
          error "Not implemented yet: implicit indexing dimensions. Use `:`."

        checkBounds expr f_xs [Just f_idx]
        xs <- case justVName e_xs of
          Just vn -> pure vn
          Nothing -> do
            vn <- newVName "#xs"
            insertIndexFn vn [f_xs]
            pure vn

        -- We don't use substParams on f_xs because the substitution
        -- might fail here (e.g., if we are inside a map lambda).
        -- Lift idx.
        unless (null $ shape f_idx) $ error "E.Index: internal error"
        subst
          IndexFn
            { shape = [],
              body = cmapValues (\e -> sym2SoP $ Apply (Var xs) [e]) (body f_idx)
            }
  -- Indexing x[e_1, :, e_2] (multi-dim, no implicit dims, : allowed).
  | Just xs <- justVName e_xs,
    all supportedDim slice = do
      f_xss <- forward e_xs
      let [f_xs] = f_xss
      unless (rank f_xs == length slice) . error $
        errorMsg loc "Not implemented yet."

      slice' <- mapM forwardDim slice
      checkBounds expr f_xs slice'
      (new_dims, idxs) <- fmap unzip . forM (zip (shape f_xs) slice') $
        \cases
          -- Single value taken from dimension.
          (_, Just f_idx) -> do
            idx <- newVName "#idx"
            i <- newVName "i"
            insertIndexFn idx [f_idx {shape = [[Forall i (Iota $ int2SoP 1)]]}]
            pure (Nothing, sym2SoP (Apply (Var idx) [int2SoP 0]))
          -- All values taken from dimension (using `:`).
          ([d_xs], Nothing) -> do
            pure (Just [d_xs], sym2SoP (Var (boundVar d_xs)))
          (_, Nothing) -> error "Not implemented yet indexing flattened dim."
      (: [])
        <$> subst
          IndexFn
            { shape = catMaybes new_dims,
              body = singleCase . sym2SoP $ Apply (Var xs) idxs
            }
  | otherwise = do
      error $ "Not implemented yet: " <> prettyStr expr
  where
    supportedDim E.DimFix {} = True
    supportedDim (E.DimSlice Nothing Nothing Nothing) = True
    supportedDim _ = False

    forwardDim (E.DimFix e) = do
      fs <- forward e
      let [f] = fs
      unless (null $ shape f) $ error "E.Index: internal error"
      pure $ Just f
    forwardDim (E.DimSlice {}) = pure Nothing
forward (E.Not e _) = do
  mapM negf =<< forward e
  where
    negf f =
      rewrite $ f {body = cmapValues (mapSymSoP (sym2SoP . neg)) (body f)}
forward (E.AppExp (E.BinOp (vn_op, _) _ (_, _) (_, _) _) _)
  | "++" <- E.baseString $ E.qualLeaf vn_op = do
      error "Limitation: Must bind (++) result to a name."
forward (E.AppExp (E.BinOp (op', _) _ (x', _) (y', _) _) _)
  | E.baseTag (E.qualLeaf op') <= E.maxIntrinsicTag,
    name <- E.baseString $ E.qualLeaf op',
    Just bop <- L.find ((name ==) . prettyStr) [minBound .. maxBound :: E.BinOp] = do
      vxs <- forward x'
      vys <- forward y'
      forM (zip vxs vys) $ \(vx, vy) -> do
        a <- newVName "#a"
        b <- newVName "#b"
        let doOp op =
              substParams
                (IndexFn [] (singleCase $ op (Var a) (Var b)))
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
          E.ShiftL -> doOp (\x y -> sym2SoP x .*. sym2SoP (Pow 2 $ sym2SoP y))
          E.Divide
            | Just x_sop <- justSingleCase vx,
              Just y_sop <- justSingleCase vy ->
                case x_sop ./. y_sop of
                  Just z -> pure $ IndexFn [] (singleCase z)
                  Nothing
                    -- i^x / i = i^(x - 1)
                    | Just (Pow i x) <- justSym x_sop,
                      Just j <- justConstant y_sop,
                      i == j -> do
                        ok <- x $>= int2SoP 1
                        case ok of
                          Yes -> pure . IndexFn [] . singleCase . sym2SoP $ Pow i (x .-. int2SoP 1)
                          Unknown -> doOp uninterpretedOp
                    -- i^x / i^y = i^(x - y)
                    | Just (Pow i x) <- justSym x_sop,
                      Just (Pow j y) <- justSym y_sop,
                      i == j -> do
                        ok <- x $>= y
                        case ok of
                          Yes -> pure $ IndexFn [] $ singleCase $ sym2SoP $ Pow i $ x .-. y
                          Unknown -> doOp uninterpretedOp
                    | otherwise -> doOp uninterpretedOp
            | otherwise -> doOp uninterpretedOp
          _ -> error ("forward not implemented for bin op: " <> show bop)
  where
    uninterpretedOp a b = sym2SoP $ Apply (Var $ E.qualLeaf op') [sym2SoP a, sym2SoP b]
forward (E.AppExp (E.If e_c e_t e_f _) _) = do
  cs <- forward e_c
  let f_c = case cs of
        [v] -> v
        _ -> error "If on tuple?"
  unless (null $ shape f_c) $ error "Condition in if-statement is an array?"

  -- Create boolean expression for true and false branches.
  c_t_branch <-
    rewrite $
      foldl1 (:||) [p :&& sop2Symbol q | (p, q) <- guards f_c]
  c_f_branch <- rewrite (neg c_t_branch)

  ts <- rollbackAlgEnv $ do
    assume c_t_branch
    forward e_t
  fs <- rollbackAlgEnv $ do
    assume c_f_branch
    forward e_f

  unless (map rank ts == map rank fs) $ error "Branch types differ?"
  -- TODO support branches of same rank (type), but different dimensions.
  -- (E.g., use "outer" guards or bool-multiplication tricks on domain sizes.)
  let eq x y = isJust <$> (unify x y :: IndexFnM (Maybe (Substitution Symbol)))
  dims_eq <- and . mconcat <$> zipWithM (zipWithM eq) (concatMap shape ts) (concatMap shape fs)
  unless dims_eq $ error "Branches with different dimensions not supported yet."

  c <- newVName "if-condition"
  t_branch <- newVName "t_branch"
  f_branch <- newVName "f_branch"
  let gs = cases [(Var c, sVar t_branch), (neg $ Var c, sVar f_branch)]
  forM (zip ts fs) $ \(t, f) -> do
    substParams (IndexFn (shape t) gs) [(c, f_c), (t_branch, t), (f_branch, f)]
      >>= rewrite
forward (E.Lambda _ _ _ _ loc) =
  error $ errorMsg loc "Cannot create index function for unapplied lambda."
forward expr@(E.AppExp (E.Apply e_f args loc) appres)
  | Just fname <- getFun e_f,
    "map" `L.isPrefixOf` fname,
    E.Lambda params lam_body _ _ _ : _args <- getArgs args,
    Just arrays <- NE.nonEmpty (NE.tail args) = do
      (outer_dim, aligned_args) <- zipArgsSOAC loc params arrays
      bindLambdaBodyParams (mconcat aligned_args)
      bodies <- rollbackAlgEnv $ do
        addRelDim outer_dim
        forward lam_body

      forM bodies $ \f_body ->
        subst (f_body {shape = outer_dim : shape f_body}) >>= rewrite
  | Just fname <- getFun e_f,
    "map" `L.isPrefixOf` fname = do
      -- No need to handle map non-lambda yet as program can just be rewritten.
      error . errorMsg loc $
        "Eta-expand map's first argument: " <> prettyStr (head $ getArgs args)
  | Just "replicate" <- getFun e_f,
    [e_n, e_x] <- getArgs args = do
      ns <- forward e_n
      unless (length ns == 1) . error $ errorMsg loc "type error"
      let n = head ns
      xs <- forward e_x
      forM xs $ \x -> do
        i <- newVName "i"
        unless (rank n == 0) . error $ errorMsg loc "type error"
        m <- rewrite $ flattenCases (body n)
        rewrite $ IndexFn ([Forall i (Iota m)] : shape x) (body x)
  | Just "iota" <- getFun e_f,
    [e_n] <- getArgs args = do
      ns <- forward e_n
      forM ns $ \n -> do
        i <- newVName "i"
        case n of
          IndexFn [] cs -> do
            m <- rewrite $ flattenCases cs
            rewrite $ IndexFn [[Forall i (Iota m)]] (singleCase $ sVar i)
          _ ->
            error $ errorMsg loc "type error"
  | Just "length" <- getFun e_f,
    [e_arg] <- getArgs args = do
      fs <- forward e_arg
      forM fs $ \f -> do
        pure $ IndexFn [] (singleCase $ dimSize $ head $ shape f)
  | Just fname <- getFun e_f,
    "zip" `L.isPrefixOf` fname = do
      mconcat <$> mapM forward (getArgs args)
  | Just fname <- getFun e_f,
    "unzip" `L.isPrefixOf` fname,
    [xs'] <- getArgs args =
      -- XXX unzip is a no-op.
      forward xs'
  | Just "flatten" <- getFun e_f,
    [e_x] <- getArgs args = do
      fs <- forward e_x
      forM fs $ \f ->
        case shape f of
          ds | length ds <= 1 -> error $ "Flatten on less-than-2d array." <> prettyStr f
          [Forall i (Iota n)] : [Forall j (Iota m)] : shp -> do
            pure $ f {shape = [Forall i (Iota n), Forall j (Iota m)] : shp}
          _ -> error "Not implemented yet."
  | Just "scan" <- getFun e_f,
    [E.OpSection (E.QualName [] vn) _ _, _ne, xs'] <- getArgs args = do
      -- Scan with basic operator.
      -- TODO stop using this case and require all scans to eta expand...
      fns <- forward xs'
      forM fns $ \fn -> do
        let i = case shape fn of
              [] -> error "scan array is empty?"
              [[Forall i' _]] -> i'
              _ -> error "Not implemented yet"
        -- TODO should we verify that _ne matches op?
        op <-
          case E.baseString vn of
            "+" -> pure (~+~)
            "-" -> pure (~-~)
            "*" -> pure (~*~)
            "&&" -> pure (~&&~)
            _ -> error ("scan not implemented for bin op: " <> show vn)
        let base_case = sVar i :== int2SoP 0
        x <- newVName "a"
        let y =
              IndexFn
                (shape fn)
                ( cases
                    [ (base_case, sym2SoP (Apply (Var x) [sVar i])),
                      (neg base_case, Recurrence `op` Apply (Var x) [sVar i])
                    ]
                )
        -- tell ["Using scan rule ", toLaTeX y]
        y @ (x, fn)
          >>= rewrite
  | Just "scan" <- getFun e_f,
    [E.Lambda params lam_body _ _ _, ne, _xs] <- getArgs args,
    xs <- NE.fromList [NE.last args],
    [pat_acc, pat_x] <- params = do
      -- The first argument of the lambda is the accumulator
      -- and the second argument is an element of the input array.
      (outer_dim, aligned_args) <- zipArgsSOAC loc [pat_x] xs

      bindLambdaBodyParams (mconcat aligned_args)
      let acc_vns = E.patNames pat_acc
      bodies <- rollbackAlgEnv $ do
        addRelDim outer_dim
        forward lam_body

      neutrals <- forward ne

      forM (zip3 bodies acc_vns neutrals) $ \(f_body, acc, f_ne) -> do
        let f_rec = repIndexFn (mkRep acc (sym2SoP Recurrence)) f_body
        f_base <- f_body @ (acc, f_ne)
        base_case <- newVName "#base_case"
        rec_case <- newVName "#rec_case"
        f_scan <-
          ( IndexFn
              { shape = outer_dim : shape f_body,
                body =
                  cases
                    [ (index outer_dim :== int2SoP 0, sVar base_case),
                      (index outer_dim :/= int2SoP 0, sVar rec_case)
                    ]
              }
            )
            `substParams` [(base_case, f_base), (rec_case, f_rec)]
        subst f_scan >>= rewrite
  | Just "scatter" <- getFun e_f,
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
        safety <- scatterSafe dest (e_inds, inds) vals
        f <- case safety of
          Unknown -> do
            printM 10 "scatter: unable to show safety"
            pure Nothing
          Yes ->
            runMaybeT $
              scatterSc1 dest (e_inds, inds) vals
                <|> scatterSc2 dest inds vals
                <|> scatterSc3 dest
        maybe
          (error $ errorMsg loc "Failed to infer index function for scatter.")
          pure
          f
  | Just "hist" <- getFun e_f,
    Just hist_vn <- justVName e_f,
    [_, _, e_k, _, _] <- getArgs args = do
      -- TODO remove this. It's only here to infer the size of the output hist.
      f_ks <- forward e_k
      case f_ks of
        [IndexFn [] cs] | [(Bool True, k)] <- casesToList cs -> do
          arg_fns <- mconcat <$> mapM forward (getArgs args) -- redundant forward on e_k again.
          i <- newNameFromString "i"
          arg_names <- forM arg_fns (const $ newVName "x")
          let g_fn =
                IndexFn
                  { shape = [[Forall i (Iota k)]],
                    body =
                      singleCase . sym2SoP $
                        Apply (Var hist_vn) (map sVar arg_names)
                  }
          fn <- substParams g_fn (zip arg_names arg_fns)
          pure [fn]
        _ ->
          undefined
  | Just "sized" <- getFun e_f,
    [_, e] <- getArgs args = do
      -- No-op.
      forward e
  | Just "Assume" <- getFun e_f,
    [e] <- getArgs args = do
      forward e
        >>= mapM
          ( \f ->
              rewrite $ f {body = cmapValues (mapSymSoP (sym2SoP . Assume)) (body f)}
          )
  | Just vn <- getFun e_f,
    vn `S.member` propertyPrelude = do
      (: []) <$> forwardPropertyPrelude vn args
  -- Applying other functions, for instance, user-defined ones.
  | otherwise = do
      y <- forwardApplyDef expr
      case y of
        Just (_effect, fns) -> pure fns
        Nothing -> do
          g <- lookupUninterpreted e_f
          -- We treat g as an uninterpreted function.
          printM 1 $ warningMsg loc ("g: " <> prettyStr g)
          arg_fns <- mconcat <$> mapM forward (getArgs args)
          let return_type = E.appResType (E.unInfo appres)
          size <- shapeOf return_type
          arg_names <- forM arg_fns (const $ newVName "x")
          iter <- case size of
            [] ->
              pure []
            [sz] -> do
              i <- newVName "i"
              pure [[Forall i (Iota sz)]]
            _ ->
              error "multi-dim not implemented yet"
          when (typeIsBool return_type) $ addProperty (Algebra.Var g) Property.Boolean

          let g_fn =
                IndexFn
                  { shape = iter,
                    body =
                      singleCase . sym2SoP $
                        Apply (Var g) (map sVar arg_names)
                  }
          fn <- substParams g_fn (zip arg_names arg_fns)
          pure [fn]
forward (E.AppExp (E.Loop _sz _init_pat _init form e_body _loc) _) = do
  fs <- rollbackAlgEnv $ do
    case form of
      (E.For ident e_sz) -> do
        let i = E.identName ident
        sizes <- map justSingleCase <$> forward e_sz
        forM_ sizes . mapM_ $ \sz ->
          assume (int2SoP 0 :<= sVar i :&& sVar i :< sz)
      (E.While e_condition) -> do
        conds <- map justSingleCase <$> forward e_condition
        forM_ conds . mapM_ $ assume . sop2Symbol
      _ -> error "not implemented"
    forward e_body
  -- Create uninterpreted functions that vary like the actual loop body's
  -- result. (If we just create scalars we can prove injectivity erroneously
  -- etc.)
  let mkUntransFun f = do
        vn <- newVName "untranslatable_loop"
        pure $ f {body = singleCase . sym2SoP $ Apply (Var vn) (map (sVar . boundVar) (concat $ shape f))}
  mapM mkUntransFun fs
forward (E.Coerce e _ _ _) = do
  -- No-op; I've only seen coercions that are hints for array sizes.
  forward e
forward e = do
  -- XXX Need to know the size of e to create uninterpreted functions;
  -- otherwise verification breaks (we can prove lots of array properties
  -- erroneously on a single element/scalar array).
  error $ "forward on unimplemented source expression: " <> prettyStr e

-- Propagate postcondition effects, such as adding properties.
-- Fires binding applications of top-level defs to names.
forwardLetEffects :: [Maybe E.VName] -> E.Exp -> IndexFnM [IndexFn]
forwardLetEffects [Just _] e@(E.Var (E.QualName _ _) _ loc) = do
  printM 2 . warningMsg loc $
    "Warning: Aliasing " <> prettyStr e <> " strips property information."
  forward e
forwardLetEffects [Just vn] (E.AppExp (E.BinOp (vn_op, _) _ (e_x, _) (e_y, _) _) _)
  | "++" <- E.baseString $ E.qualLeaf vn_op = do
      fs <- forward e_x
      gs <- forward e_y
      unless (length fs == length gs) (error "Internal error.")
      forM (zip fs gs) $ \(f, g) -> do
        insertConcat vn [f, g]
        when (any (\case (Forall _ (Cat {})) -> True; _ -> False) $ concat (shape f) ++ concat (shape g)) $
          error "Internal error (get rid of Cat domain)."
        shapes_are_compatible <- shapeCompatible (shape f) (shape g)
        -- The operation type-checked, so this error should never hit.
        unless shapes_are_compatible (error "Not implemented yet.")
        -- Concatenate leading dimension.
        let (Forall i (Iota n) : dim) : trailing_dims = shape f
        let Forall _ (Iota m) = head (head (shape g))
        let new_shape = (Forall i (Iota (n .+. m)) : dim) : trailing_dims
        -- Create guards that toggle each array. We unflatten f and g for the
        -- substitution, so that we can index it without first converting to
        -- flat indexing.
        let select_x = sVar i :< n
        x <- newNameFromString "#x"
        y <- newNameFromString "#y"
        let indices = map (sym2SoP . Var . boundVar) (concat new_shape)
        let i1 = head indices .-. n
        IndexFn
          { shape = new_shape,
            body =
              cases
                [ (select_x, sym2SoP $ Apply (Var x) indices),
                  (neg select_x, sym2SoP $ Apply (Var y) (i1 : drop 1 indices))
                ]
          }
          `substParams` [(x, unflatten f), (y, unflatten g)]
  where
    -- (++) : [n]t -> [m]t -> [n+m]t
    -- The first dimension in both arrays may differ (i.e., n and m), remaining
    -- dimensions have to be identical (type t). If the first dimension is
    -- flattened in the index function representation (i.e., has multiple
    -- iterators), then the first iterator of that dimension may differ, and
    -- the others have to be identical to preserve the flattened structure.
    -- (TODO handle the case where we have inferred that one dimension has been
    -- flattened in one function, but not in the other, in which case the other
    -- should simply adopt that.)
    shapeCompatible (x : xs) (y : ys) =
      (drop 1 x : xs) `unifiesWith` (drop 1 y : ys)
    shapeCompatible _ _ = undefined
forwardLetEffects [Just vn] e@(E.AppExp (E.Apply e_f args _) _)
  | Just "flatten" <- getFun e_f,
    [E.Var (E.QualName _ x) _ _] <- getArgs args = do
      p <- fromMaybe mempty <$> (getProperties <&> (M.!? Algebra.Var x))
      f <- forward e
      mapM_ addProp (S.filter propagatesOverFlatten p)
      pure f
  where
    addProp = addProperty (Algebra.Var vn) . cloneProperty vn

    propagatesOverFlatten p = case p of
      Property.Monotonic {} -> True
      Property.Rng {} -> True
      Property.Injective {} -> True
      Property.BijectiveRCD {} -> True
      Property.FiltPartInv {} -> True
      Property.FiltPart {} -> True
      _ -> False
forwardLetEffects [Just h] e@(E.AppExp (E.Apply e_f args _) _)
  | Just "reduce_by_index" <- getFun e_f,
    [e_dest, e_op, _e_ne, _e_is, e_xs] <- getArgs args,
    E.OpSection vn_op _ _ <- e_op,
    E.baseTag (E.qualLeaf vn_op) <= E.maxIntrinsicTag,
    name <- E.baseString $ E.qualLeaf vn_op,
    Just E.Plus <- L.find ((name ==) . prettyStr) [minBound .. maxBound :: E.BinOp] = do
      f_xs <- forward e_xs
      f_dest <- forward e_dest
      unless (length f_xs == length f_dest) $ error "not implemented yet"
      forM (zip f_dest f_xs) $ \(y, x) -> do
        case (justConstant =<< justSingleCase x, shape x) of
          (Just 1, [d]) -> do
            addRelSymbol (Prop $ Property.Rng h (Just $ int2SoP 0, Just $ dimSize d .+. int2SoP 1))
            printAlgEnv 5
          _ -> pure ()
        pure $
          y
            { body =
                singleCase . sym2SoP $
                  Apply (Var h) (map (sVar . boundVar) (mconcat $ shape y))
            }
  | Just "reduce_by_index" <- getFun e_f,
    [dest, e_op, e_ne, _is, _xs] <- getArgs args,
    E.Lambda params lam_body _ _ _ <- e_op,
    xs <- NE.last args,
    [pat_acc, pat_x] <- params = do
      -- "min/max/id" rule: if each branch in the body of e_op returns a variable,
      -- inherit the union of the variables' ranges.
      --
      -- For the purpose of inferring ranges, it doesn't matter that there's an
      -- accumulator because we don't allow e_op to actually do any arithmetic
      -- anyway---only to return one of the params (possibly in a conditional).
      (outer_dim, aligned_args) <- zipArgsSOAC (E.locOf e) [pat_acc, pat_x] (NE.fromList [xs, xs])
      bindLambdaBodyParams (mconcat aligned_args)
      ops <- rollbackAlgEnv $ do
        addRelDim outer_dim
        forward lam_body
      neutrals <- forward e_ne

      bins <- forward dest
      unless (length bins == length neutrals) $ error "not implemented yet"

      -- Infer ranges.
      forM (zip3 bins ops neutrals) $ \(bin, op, ne) -> do
        case mapM ((justVar <=< justSym) . snd) $ guards op of
          Just vns -> do
            alg_ranges <- rollbackAlgEnv $ do
              -- Add outer_dim range as property in case vns is just the iterator.
              mapM_
                ( \d -> do
                    n <- toAlgebra (domainEnd (formula d) .+. int2SoP 1)
                    addProperty (Algebra.Var $ boundVar d) (Property.Rng (boundVar d) (Just $ int2SoP 0, Just n))
                )
                outer_dim
              sequence <$> mapM (askRng . Algebra.Var) vns
            ranges :: Maybe [Property.Property Symbol] <- traverse (mapM fromAlgebra) alg_ranges
            -- We don't know whether ne would be a lower bound or upper bound,
            -- so we make sure that the ranges include ne, in order to disregard
            -- ne's range.
            forM_ ranges $ \rs -> do
              ans <-
                allM $
                  map
                    ( \(Property.Rng _ (a, b)) -> do
                        let lb c = maybe (Bool True) (:<= c) a
                        let ub c = maybe (Bool True) (c :<=) b
                        askQ (CaseCheck (\c -> lb c :&& ub c)) ne
                    )
                    rs
              when (isYes ans) $
                forM_ rs (addRelSymbol . Prop . cloneProperty h)
          Nothing -> pure ()
        pure $
          IndexFn
            { shape = shape bin,
              body =
                singleCase . sym2SoP $
                  Apply (Var h) (map (sVar . boundVar) outer_dim)
            }
forwardLetEffects bound_names x = do
  y <- forwardApplyDef x
  case y of
    Just (effect, fns) -> do
      printM 3 $ "Propating effects on " <> prettyStr bound_names
      -- Apply effect to unusable fresh name if it's a wildcard `let _ = ...`.
      bound_names' <- forM bound_names $ \case
        Just vn -> pure vn
        Nothing -> newNameFromString "_wildcard"
      effect bound_names'
      printAlgEnv 3
      -- Rename uninterpreted functions to match bound names
      -- because effects get applied to the bound name.
      -- Transforms
      --   indices |-> for i < n . <f>(i)
      --   Range indices (0, m)
      -- into
      --   indices |-> for i < n . indices(i)
      --   Range indices (0, m)
      pure $
        zipWith
          ( \vn f ->
              if bodyIsUinterpreted f
                then f {body = singleCase (sym2SoP $ Apply (Var vn) (indexVars f))}
                else f
          )
          bound_names'
          fns
    Nothing -> forward x

-- Applying top-level definitions of functions.
-- Returns nothing if the function is not defined at top-level.
--
-- BUG: this allows capturing the names of formal arguments of the top-level
-- definition being applied. (We should call changeScope on the resulting list
-- of index functions, but we don't know the names of all variables in scope
-- here.) This is a minor bug.
forwardApplyDef :: E.Exp -> IndexFnM (Maybe (ApplyEffect, [IndexFn]))
forwardApplyDef (E.AppExp (E.Apply f args loc) _)
  | (E.Var (E.QualName [] g) info _loc) <- f,
    E.Scalar (E.Arrow {}) <- E.unInfo info = do
      t <- getTopLevel
      case M.lookup g t of
        Just apply -> Just <$> apply loc args
        Nothing -> pure Nothing
forwardApplyDef _ = pure Nothing

-- TODO make it return a list of functions just to remove the many undefined cases
-- (it should never happen in practice as these functions are type checked).
forwardPropertyPrelude :: String -> NE.NonEmpty (a, E.Exp) -> IndexFnM IndexFn
forwardPropertyPrelude f args =
  case f of
    "Monotonic"
      | [E.OpSection (E.QualName [] vn_op) _ _, e_X] <- getArgs args,
        Just x <- justVName e_X,
        Just dir <- opToMonDir (E.baseString vn_op) -> do
          printM 1 $ prettyStr (IndexFn [] $ cases [(Bool True, pr $ Property.Monotonic x dir)])
          pure (IndexFn [] $ cases [(Bool True, pr $ Property.Monotonic x dir)])
      where
        opToMonDir ">" = pure DecS
        opToMonDir ">=" = pure Dec
        opToMonDir "<" = pure IncS
        opToMonDir "<=" = pure Inc
        opToMonDir _ = Nothing
    "Equiv"
      | [e_X, e_y] <- getArgs args,
        Just x <- justVName e_X -> do
          y <- forward e_y
          case y of
            [IndexFn [] g_a] ->
              fmap (IndexFn []) . simplify . cases $ do
                (p_a, a) <- casesToList g_a
                pure (p_a, pr $ Property.Equiv x a)
            _ ->
              undefined
    "Range"
      | [e_X, e_rng] <- getArgs args,
        Just x <- justVName e_X -> do
          f_rng <- forward e_rng
          case f_rng of
            [IndexFn [] g_a, IndexFn [] g_b] ->
              fmap (IndexFn []) . simplify . cases $ do
                (p_a, a) <- casesToList g_a
                (p_b, b) <- casesToList g_b
                pure (p_a :&& p_b, pr $ Property.Rng x (inf2Nothing a, inf2Nothing b))
            _ ->
              undefined
    "Disjoint"
      | [e_preds] <- getArgs args,
        Just (i, lam) <- parsePredicate e_preds -> do
          -- TODO add domain to this property to get with bounds checking
          -- below?
          f_preds <- withoutBoundsChecks (forward lam)
          case mapM (justSym <=< justSingleCase) f_preds of
            Just ps | length ps > 1 -> do
              let preds = map (Property.Predicate i) ps
              pure (IndexFn [] $ cases [(Bool True, pr $ Property.UserFacingDisjoint preds)])
            _ -> undefined
    "Injective"
      | [e_X] <- getArgs args,
        Just x <- justVName e_X -> do
          pure $
            IndexFn
              { shape = [],
                body = cases [(Bool True, pr $ Property.Injective x Nothing)]
              }
    "InjectiveRCD"
      | [e_X, e_RCD] <- getArgs args,
        Just x <- justVName e_X -> do
          f_RCD <- forward e_RCD
          case f_RCD of
            [IndexFn [] g_a, IndexFn [] g_b] ->
              fmap (IndexFn []) . simplify . cases $ do
                (p_a, a) <- casesToList g_a
                (p_b, b) <- casesToList g_b
                pure (p_a :&& p_b, pr $ Property.Injective x $ Just (a, b))
            _ ->
              undefined
    "BijectiveRCD"
      | [e_X, e_RCD, e_ImgRCD] <- getArgs args,
        Just x <- justVName e_X -> do
          f_RCD <- forward e_RCD
          f_ImgRCD <- forward e_ImgRCD
          case f_RCD <> f_ImgRCD of
            [ IndexFn [] g_a,
              IndexFn [] g_b,
              IndexFn [] g_c,
              IndexFn [] g_d
              ] ->
                fmap (IndexFn []) . simplify . cases $ do
                  (p_a, a) <- casesToList g_a
                  (p_b, b) <- casesToList g_b
                  (p_c, c) <- casesToList g_c
                  (p_d, d) <- casesToList g_d
                  pure (p_a :&& p_b :&& p_c :&& p_d, pr $ Property.BijectiveRCD x (a, b) (c, d))
            _ ->
              undefined
    "FiltPartInv2" -> forwardPropertyPrelude "FiltPartInv" args
    "FiltPartInv"
      | e_X : e_filt : e_parts <- getArgs args,
        Just (param_filt, lam_filt) <- parsePredicate e_filt,
        Just parts <- mapM parsePredicate e_parts,
        Just x <- justVName e_X -> do
          propArgs <- commonFiltPart x (param_filt, lam_filt) parts
          fmap (IndexFn []) . simplify . cases $ do
            (c, pf, pps) <- propArgs
            pure (c, pr $ Property.FiltPartInv x pf pps)
    "FiltPart"
      | e_Y : e_X : e_filt : e_parts <- getArgs args,
        Just (param_filt, lam_filt) <- parsePredicate e_filt,
        Just parts <- mapM parsePredicate e_parts,
        Just y <- justVName e_Y,
        Just x <- justVName e_X -> rollbackAlgEnv $ do
          -- HINT make sure in \i -> pf, i gets the iterator of e_Y not e_X.
          propArgs <- commonFiltPart x (param_filt, lam_filt) parts
          fmap (IndexFn []) . simplify . cases $ do
            (c, pf, pps) <- propArgs
            pure (c, pr $ Property.FiltPart y x pf pps)
    "FiltPart2" -> forwardPropertyPrelude "FiltPart" args
    "and" | [e_xs] <- getArgs args -> do
      -- No-op: The argument e_xs is a boolean array; each branch will
      -- be checked in refinements.
      -- XXX but we lose information about iterator at check site, hm...
      xss <- forward e_xs
      case xss of
        [xs] -> pure xs
        _ -> undefined
    _ -> do
      error $
        "Properties must be in A-normal form and not use wildcards: " <> prettyStr f <> " " <> prettyStr (NE.map snd args)
  where
    pr = sym2SoP . Prop

    parsePredicate (E.Lambda params lam _ _ _)
      | [[(Just param, _)]] <- map patternMapAligned params =
          Just (param, lam)
    parsePredicate _ = Nothing

    inf2Nothing x | Just (Var vn) <- justSym x, E.baseString vn == "inf" = Nothing
    inf2Nothing x = Just x

    -- Map filter and partition lambdas over indices of X to infer their
    -- index functions.
    -- substitutions (e.g., sizes). Then extract the inferred cases to create
    -- `Property.Predicate`s which is the internal representation of the lambdas.
    -- Returns combined
    commonFiltPart x (param_filt, lam_filt) parts = do
      -- (Note that this essentially just uses the inferred size of X;
      -- f_X could simply be the trivial function: for i < n . true => X[i].)
      res <- lookupIndexFn x
      case res of
        Just [f_X] | [[Forall i _]] <- shape f_X -> rollbackAlgEnv $ do
          let idx = IndexFn [] (cases [(Bool True, sVar i)])
          bindLambdaBodyParams $ (param_filt, idx) : map ((,idx) . fst) parts
          addRelShape (shape f_X)
          f_filt <- forward lam_filt >>= subst . IndexFn (shape f_X) . body . head
          f_parts <- forM parts $ \(_, lam_part) ->
            forward lam_part >>= subst . IndexFn (shape f_X) . body . head

          let g_parts = [g | f_part <- f_parts, g <- guards f_part]
          unless (all ((== Bool True) . fst) g_parts) $
            error "Predicates with if-statements not implemented yet."

          pure $ do
            (p_filt, filt) <- guards f_filt
            pure
              ( p_filt,
                Property.Predicate i (sop2Symbol filt),
                [ Property.Predicate i (sop2Symbol part)
                  | (Bool True, part) <- g_parts
                ]
              )
        _ -> do
          error "Applying property to name bound to tuple?"

scatterSs1 :: IndexFn -> (E.Exp, IndexFn) -> IndexFnM Answer
scatterSs1 (IndexFn ([Forall _ d_xs] : _) _) (e_is, is) = do
  dest_size <- rewrite $ domainEnd d_xs
  case justVName e_is of
    Just vn_is -> do
      prove (Property.Injective vn_is $ Just (int2SoP 0, dest_size))
        `orM` prove (Property.Injective vn_is Nothing)
    Nothing ->
      proveFn (PInjective $ Just (int2SoP 0, dest_size)) is
scatterSs1 _ _ = pure Unknown

scatterSs2 :: IndexFn -> Answer
scatterSs2 vs =
  answerFromBool $ all ((`S.notMember` fv (body vs)) . boundVar) (mconcat $ shape vs)

-- TODO implement (extract vn for values and lookup Range property).
scatterSs3 :: (Applicative f) => p -> f Answer
scatterSs3 _ = pure Unknown

scatterSafe :: IndexFn -> (E.Exp, IndexFn) -> IndexFn -> IndexFnM Answer
scatterSafe xs is vs = scatterSs1 xs is `orM` pure (scatterSs2 vs) `orM` scatterSs3 vs

-- TODO also look up in env to see if there is a `Bij is Y Z` property with Z <= (0, dest_size) <= Y.
scatterSc1 :: IndexFn -> (E.Exp, IndexFn) -> IndexFn -> MaybeT IndexFnM IndexFn
scatterSc1 xs@(IndexFn ([Forall i dom_dest] : _) _) (e_is, is) vs
  | rank is == 1,
    rank xs == rank vs = do
      dest_size <- lift $ rewrite $ domainEnd dom_dest
      vn_inds <- warningInds
      perm <- lift $ prove (Property.BijectiveRCD vn_inds (int2SoP 0, dest_size) (int2SoP 0, dest_size))
      case perm of
        Unknown -> failMsg ""
        Yes -> do
          -- `inds` is invertible on the whole domain.
          vn_vs <- newVName "#vs"
          vn_inv <- newVName (E.baseString vn_inds <> "⁻¹")

          lift $ addInvAlias vn_inv vn_inds
          lift $ addRelSymbol (Prop $ Property.BijectiveRCD vn_inv (int2SoP 0, dest_size) (int2SoP 0, dest_size))
          -- TODO make bijective cover injective also!
          lift $ addRelSymbol (Prop $ Property.Injective vn_inv $ Just (int2SoP 0, dest_size))
          -- TODO add these ranges when needed using the property table.
          -- Here we add them as a special case because we know is^(-1) will
          -- be used for indirect indexing.
          lift $ addRelSymbol (Prop $ Property.Rng vn_inv (Just $ int2SoP 0, Just dest_size))

          -- let inv_i = Apply (Var vn_inv) (sVar . boundVar <$> shape xs)
          let inv_i = sym2SoP (Apply (Var vn_inv) [sVar i])
          let inner_dims = map index (drop 1 (shape xs))
          lift $
            xs {body = singleCase (sym2SoP (Apply (Var vn_vs) $ inv_i : inner_dims))}
              @ (vn_vs, vs)
  where
    warningInds
      | Just vn <- justVName e_is = pure vn
      | otherwise = do
          printM 1 . warningMsg (E.locOf e_is) $
            "You might want to bind scattered indices to a name to aid"
              <> " index function inference: "
              <> prettyStr e_is
          fail ""
scatterSc1 _ _ _ = fail ""

scatterSc2 :: IndexFn -> IndexFn -> IndexFn -> MaybeT IndexFnM IndexFn
scatterSc2 xs@(IndexFn [[Forall _ d_xs]] _) is@(IndexFn [[Forall k (Iota m)]] _) vs = do
  n <- lift $ rewrite $ domainEnd d_xs .+. int2SoP 1
  -- Check that we can show whether each case is in domain of xs or not.
  let in_dom_xs y = int2SoP 0 :<= y :&& y :< n
  in_bounds <- lift $ mapM (queryCase (CaseCheck in_dom_xs) is) [0 .. length (guards is) - 1]
  sanity_check <-
    lift . allM $
      zipWith
        (\j ans -> pure ans `orM` queryCase (CaseCheck (neg . in_dom_xs)) is j)
        [0 ..]
        in_bounds
  printM 3 $ "answers " <> prettyStr in_bounds
  printM 3 $ "sanity  " <> prettyStr sanity_check
  printM 3 $ "scatterSc2: is " <> prettyStr is
  -- Sort branches so that indices in the domain of xs come before out-of-bounds indices.
  let gs = L.sortOn fst $ zip in_bounds (guards is)
  printM 3 $ "scatterSc2: gs " <> prettyStr (map snd gs)
  case gs of
    [(Unknown, _), (Yes, (c, e))] -> do
      Yes <- lift $ algebraContext is $ do
        addRelShape (shape is)
        neg c =>? (e :>= rep (mkRep k (sVar k .+. int2SoP 1)) e)
      Yes <- lift $ algebraContext is $ do
        addRelShape (shape is)
        k' <- newNameFromString "k"
        k +< k'
        c =>? (e :<= rep (mkRep k (sVar k')) e)

      -- TODO relax this one (requires support for union of index functions to handle initial linear segment)
      Yes <- lift $ do
        Bool True =>? (rep (mkRep k (int2SoP 0 :: SoP Symbol)) e :== int2SoP 0)
      -- TODO relax this one (requires support for union of index functions to handle final linear segment)
      Yes <- lift $ do
        Bool True =>? (rep (mkRep k m) e .-. int2SoP 1 :== n .-. int2SoP 1)

      -- Attempt to simplify c away.
      c' <- lift $ algebraContext is $ do
        addRelShape (shape is)
        let c'' = case c of
              Apply {} -> sym2SoP c :> int2SoP 0 -- (c is application of a boolean function.)
              _ -> c
        let ans1 = (rep (mkRep k (sVar k .+. int2SoP 1)) e .-. e :> int2SoP 0) =>? c''
        let ans2 = (n .-. rep (mkRep k m) e :> int2SoP 0) =>? sop2Symbol (rep (mkRep k m) c'')
        ans <- ans1 `andM` ans2
        case ans of
          Yes -> pure (Bool True)
          Unknown -> pure c

      i <- newVName "i"
      hole_xs <- newVName "hole_xs"
      hole_vs <- newVName "hole_vs"
      let p = sVar i :== e :&& c'
      let f =
            IndexFn
              { shape = [[Forall i (Cat k m e)]],
                body =
                  cases
                    [ (p, sym2SoP $ Apply (Var hole_vs) [sVar k]),
                      (neg p, sym2SoP $ Apply (Var hole_xs) [sVar i])
                    ]
              }
      lift $ substParams f [(hole_vs, vs), (hole_xs, xs)]
    _ -> failMsg "scatterSc2: unable to determine OOB branch"
scatterSc2 _ _ _ = fail ""

-- Scatter fallback: result is uninterpreted, but safe.
scatterSc3 :: IndexFn -> MaybeT IndexFnM IndexFn
scatterSc3 (IndexFn [[Forall i dom_dest]] _) = do
  uninterpreted <- newNameFromString "safe_scatter"
  lift . pure $
    IndexFn
      { shape = [[Forall i dom_dest]],
        body = cases [(Bool True, sym2SoP $ Apply (Var uninterpreted) [sVar i])]
      }
scatterSc3 _ = fail ""

{-
    Utilities.
-}

justVName :: E.Exp -> Maybe E.VName
justVName (E.Var (E.QualName [] vn) _ _) = Just vn
justVName _ = Nothing

getFun :: E.Exp -> Maybe String
getFun e = E.baseString <$> justVName e

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

getTERefine :: E.TypeExp E.Exp E.VName -> [E.Exp]
getTERefine (E.TEParens te _) = getTERefine te
getTERefine (E.TETuple tes _) = do
  te <- tes
  getTERefine te
getTERefine (E.TERefine _ e@(E.Lambda {}) _) =
  pure e
getTERefine (E.TERefine _ _ loc) =
  error $ errorMsg loc "Only lambda postconditions are currently supported."
getTERefine _ = []

hasRefinement :: E.TypeExp E.Exp E.VName -> Bool
hasRefinement = not . null . getTERefine

shapeOf :: E.TypeBase E.Exp as -> IndexFnM [SoP Symbol]
shapeOf (E.Scalar (E.Refinement t _)) = shapeOf t
shapeOf (E.Scalar _) = pure mempty
shapeOf (E.Array _ shp _) = do
  shapeToSoP shp
  where
    shapeToSoP s = do
      dims <- mapM forward (E.shapeDims s)
      unless (all ((<= 1) . length) dims) $ error "Size of array dimension is a tuple?"
      pure $ map getScalar (mconcat dims)

    getScalar (IndexFn [] cs) | [(Bool True, x)] <- casesToList cs = x
    getScalar f = error ("getScalar on " <> prettyStr f)

-- Flatten array of tuples into list of projections, giving the structure
-- for a tuple of arrays representation.
toTupleOfArrays :: E.TypeBase dim u -> [[Integer]]
toTupleOfArrays (E.Array _ _ ty) = toTupleOfArrays (E.Scalar ty)
toTupleOfArrays (E.Scalar ty) = project ty
  where
    project :: E.ScalarTypeBase dim u -> [[Integer]]
    project (E.Record ts) = do
      (i, t) <- zip [0 ..] (M.elems ts)
      map (i :) (project (unwrapScalar t))
    project _ = pure mempty

    unwrapScalar E.Array {} = error "Arrays of structures is not supported."
    unwrapScalar (E.Scalar t) = t

newtype Tree a = Node [Tree a]
  deriving (Show)

-- HACK this lets us propagate any properties on the output names
-- to the postcondition. For example, in the below we want to
-- know that y' = y (the name, because y' = indexfn of y).
--
-- def f x : {\y' -> property y} =
--   let y = ...
--   let z = funWithPostCond y
--   in y
--
-- A more elegant solution would change the return type of forward...
trackNamesOfFinalLetBody :: E.Exp -> IndexFnM ()
trackNamesOfFinalLetBody (E.TupLit es _)
  | Just vns <- mapM (justVName . stripCoerce) es =
      setOutputNames vns
trackNamesOfFinalLetBody e
  | Just vn <- justVName (stripCoerce e) = setOutputNames [vn]
  | otherwise = pure ()

stripCoerce :: E.Exp -> E.Exp
stripCoerce (E.Coerce e _ _ _) = e
stripCoerce e = e

-- Binds names to index functions.
bindLambdaBodyParams :: [(E.VName, IndexFn)] -> IndexFnM ()
bindLambdaBodyParams = mapM_ (\(vn, f) -> insertIndexFn vn [f])

-- Like zipArgs, but for zipping parameters of SOAC's lambda with its
-- array arguments:
--   * the outer dimension of each argument is dropped (each parameters'
--     rank is one less than its corresponding array's rank)
--   * a new common outer dimension is returned (corresponding to the
--     most complex outer dimension of the arrays)
zipArgsSOAC ::
  (E.Located a) =>
  a ->
  [E.Pat E.ParamType] ->
  NE.NonEmpty (b, E.Exp) ->
  IndexFnM ([Iterator], [[(E.VName, IndexFn)]])
zipArgsSOAC loc formal_args actual_args = do
  -- Renaming makes sure all Cat k bound in iterators are identical, so that
  -- a new common outer iterator can be used.
  args <- renameSameL (mapM . mapM) =<< mapM forward (getArgs actual_args)
  let new_outer_dim = maximum $ map (head . shape) (mconcat args)
  -- Transform each arg to use the new common outer iterator.
  args' <- forM args . mapM $ \f -> do
    vn <- newVName "#f"
    let new_shape = new_outer_dim : tail (shape f)
    IndexFn
      { shape = new_shape,
        body = singleCase . sym2SoP $ Apply (Var vn) (map index new_shape)
      }
      @ (vn, f)
  -- Substitutions may have renamed Cat `k`s; do a common rename again.
  args'' <- renameSameL (mapM . mapM) args'
  let new_outer_dim' = head (shape (head (head args'')))
  -- Drop the new outer dim, aligning arguments with parameters.
  (aligned_args, _) <-
    zipArgs' loc formal_args (map (map dropOuterDim) args'')
  pure (new_outer_dim', aligned_args)
  where
    dropOuterDim f = f {shape = drop 1 (shape f)}

-- Align parameter patterns with their arguments---or raise an error.
-- A parameter pattern reduces to a list of (optional) names with type information.
-- An argument is a source expression, which `forward` will return a list
-- of index functions for.
-- For each pattern and argument, the two lists must correspond.
zipArgs ::
  (E.Located a) =>
  a ->
  [E.Pat E.ParamType] ->
  NE.NonEmpty (b, E.Exp) ->
  IndexFnM ([[(E.VName, IndexFn)]], [[(E.VName, SoP Symbol)]])
zipArgs loc formal_args actual_args =
  zipArgs' loc formal_args =<< mapM forward (getArgs actual_args)

zipArgs' ::
  (E.Located a) =>
  a ->
  [E.Pat E.ParamType] ->
  [[IndexFn]] ->
  IndexFnM ([[(E.VName, IndexFn)]], [[(E.VName, SoP Symbol)]])
zipArgs' loc formal_args actual_args = do
  let pats = map patternMapAligned formal_args
  -- Each name in the pattern may partially deconstruct tuple arguments.
  -- Our arguments are always fully deconstructed, so I only allow full
  -- deconstructions in the source code for now. TODO implement partial
  -- deconstruction using `alignWithPattern`, biggest question is
  -- how to bind one name to multiple index functions in the functions
  -- that use zipArgs's output (e.g., substParams).
  unless (length pats == length actual_args) . error $
    errorMsg loc "Functions must be fully applied. Maybe you want to eta-expand?"
  unless (map length pats == map length actual_args) . error $
    errorMsg loc "Internal error: actual argument does not match parameter pattern."

  -- Discard unused parameters such as wildcards while maintaining alignment.
  let aligned_args = do
        (pat, arg) <- zip pats actual_args
        pure . catMaybes $ zipWith (\vn fn -> (,fn) <$> vn) (map fst pat) arg

  -- When applying top-level functions size parameters must be replaced as well.
  aligned_sizes <-
    forM (zip pats actual_args) $ \(pat, arg) -> do
      fmap mconcat . forM (zip (map snd pat) arg) $ \(pat_type, f) -> do
        array_shape <- shapeOf pat_type
        unless (length array_shape == rank f) . error $
          errorMsg loc "Internal error: parameter and argument sizes do not align."
        let size_vars = map getVName array_shape
        sizes <- mapM (rewrite . dimSize) (shape f)
        pure . catMaybes $ zipWith (\sz -> fmap (,sz)) sizes size_vars

  pure (aligned_args, aligned_sizes)
  where
    getVName x
      | Just (Var vn) <- justSym x = Just vn
      | otherwise = Nothing

substParams :: (Foldable t) => IndexFn -> t (E.VName, IndexFn) -> IndexFnM IndexFn
substParams = foldM substParam
  where
    -- We want to simplify, but avoid rewriting recurrences during
    -- paramter-substitution.
    substParam fn (paramName, paramIndexFn) =
      (fn @ (paramName, paramIndexFn)) >>= rewriteWithoutRules

failMsg :: (MonadFail m) => String -> m b
failMsg msg = do
  printM 3 msg
  fail msg

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

{-
    Handling pre- and postconditions.
-}
type CheckContext = (Replacement Symbol, [(E.VName, IndexFn)], Replacement Symbol)

emptyCheckContext :: CheckContext
emptyCheckContext = (mempty, mempty, mempty)

type Check = CheckContext -> IndexFnM Answer

type Effect = CheckContext -> IndexFnM ()

-- Extract the Check to verify a formal argument's precondition, if it exists.
getPrecondition :: E.PatBase E.Info E.VName (E.TypeBase dim u) -> IndexFnM [Check]
getPrecondition = fmap (fmap fst) . getRefinement

-- Extract the Check to verify, and the Effect of, a formal argument's refinement, if it exists.
getRefinement :: E.PatBase E.Info E.VName (E.TypeBase dim u) -> IndexFnM [(Check, Effect)]
getRefinement (E.PatParens pat _) = getRefinement pat
getRefinement (E.PatAscription pat _ _) = getRefinement pat
getRefinement (E.Id param (E.Info {E.unInfo = info}) _loc) = getRefinementFromType [param] info
getRefinement _ = pure []

-- Associates any refinement in `ty` with the name `vn`.
getRefinementFromType :: [E.VName] -> E.TypeBase dim u -> IndexFnM [(Check, Effect)]
getRefinementFromType vns ty = case ty of
  E.Array _ _ (E.Refinement _ ref) -> do
    pure <$> mkRef vns ref
  E.Scalar (E.Refinement _ ref) ->
    pure <$> mkRef vns ref
  _ -> pure []

mkRef :: [E.VName] -> E.Exp -> IndexFnM (Check, Effect)
mkRef vns refexp = case refexp of
  E.Lambda lam_params lam_body _ _ loc -> do
    let param_names = map fst $ mconcat $ map patternMapAligned lam_params
    ref <- forwardRefinementExp lam_body
    unless (length vns >= length (catMaybes param_names)) . error $
      errorMsg loc ("Internal error: mkRef: " <> prettyStr refexp)
    let ref' = foldl repParam ref (zip param_names vns)
    let check = inContext askRefinement ref'
    let effect =
          inContext
            ( \f -> do
                y <- rewrite $ flattenCases (body f)
                addRelSymbol (sop2Symbol y)
            )
            ref'
    pure (check, effect)
  x -> error $ "Unhandled refinement predicate " <> show x
  where
    forwardRefinementExp e = do
      fns <- forward e
      case fns of
        [fn] -> body <$> rewrite fn
        _ -> error "Impossible: Refinements have return type bool."

    repParam g (Just param_name, vn) = repCases (mkRep param_name $ sym2SoP (Var vn)) g
    repParam g (Nothing, _) = g

    -- This wraps a Check or an Effect, making sure that parameters/names/sizes
    -- are substituted correctly at the evaluation site.
    -- (renaming_rep used only by postcondition effects; see forwardApplyDef).
    inContext f e (size_rep, args, renaming_rep) = do
      fn <- substParams (repIndexFn size_rep (IndexFn [] e)) args
      f (repIndexFn renaming_rep fn)

-- Tags formal arguments that are booleans or arrays of booleans as such.
addBooleanNames :: E.PatBase E.Info E.VName E.ParamType -> IndexFnM ()
addBooleanNames (E.PatParens pat _) = addBooleanNames pat
addBooleanNames (E.PatAscription pat _ _) = addBooleanNames pat
addBooleanNames (E.Id param (E.Info {E.unInfo = E.Array _ _ t}) _) = do
  when (typeIsBool $ E.Scalar t) $ addProperty (Algebra.Var param) Property.Boolean
addBooleanNames (E.Id param (E.Info {E.unInfo = t}) _) = do
  when (typeIsBool t) $ addProperty (Algebra.Var param) Property.Boolean
addBooleanNames _ = pure ()

-- Automatically refines size variables to be non-negative.
addSizeVariable :: E.VName -> IndexFnM ()
addSizeVariable d = do
  alg_d <- toAlgebra (sym2SoP $ Var d)
  addRel (alg_d :>=: int2SoP 0)
  addRelSymbol (Prop (Property.Rng d (Just (int2SoP 0), Nothing)))

{-
    Bounds checking.
-}
checkBounds :: E.Exp -> IndexFn -> [Maybe IndexFn] -> IndexFnM ()
checkBounds _ (IndexFn [] _) _ =
  error "E.Index: Indexing into scalar"
checkBounds e f_xs idxs =
  whenBoundsChecking $ do
    forM_ (zip (shape f_xs) idxs) checkIndexInDomain
    printM 1 . locMsg (E.locOf e) $ prettyStr e <> greenString " OK"
  where
    checkIndexInDomain (_, Nothing) = pure ()
    checkIndexInDomain ([Forall _ d], Just f_idx) =
      algebraContext f_idx $ do
        bounds <- getBounds d
        void . foreachCase f_idx $ \n -> do
          forM bounds $ \bound -> do
            c <- isYes <$> queryCase (CaseCheck bound) f_idx n
            unless c $ emitFailure n bound f_idx
    checkIndexInDomain (_dims, Just _) = undefined

    getBounds d = do
      d_start <- rewrite $ domainStart d
      d_end <- rewrite $ domainEnd d
      pure $ case d of
        Cat _ _ b ->
          [ \idx -> b :<= idx :|| d_start :<= idx,
            \idx -> idx :<= intervalEnd d :|| idx :<= d_end
          ]
        Iota _ ->
          [ (d_start :<=),
            (:<= d_end)
          ]

    emitFailure n bound f_idx =
      let (p_idx, e_idx) = getCase n $ body f_idx
       in error . errorMsg (E.locOf e) $
            "Unsafe indexing: "
              <> prettyStr e
              <> " (failed to show: "
              <> prettyStr p_idx
              <> " => "
              <> prettyStr (bound e_idx)
              <> ")."
