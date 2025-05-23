{-# LANGUAGE LambdaCase #-}

module Futhark.Analysis.Properties.Convert (mkIndexFnProg, mkIndexFnValBind) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, foldM_, forM, forM_, unless, void, when, zipWithM, (<=<))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Bifunctor
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.Set qualified as S
import Futhark.Analysis.Properties.AlgebraBridge (algebraContext, fromAlgebra, paramToAlgebra, simplify, toAlgebra)
import Futhark.Analysis.Properties.AlgebraBridge.Util
import Futhark.Analysis.Properties.AlgebraPC.Symbol qualified as Algebra
import Futhark.Analysis.Properties.Flatten (flatten2d)
import Futhark.Analysis.Properties.IndexFn
import Futhark.Analysis.Properties.IndexFnPlus (domainEnd, domainStart, intervalEnd, repCases, repIndexFn)
import Futhark.Analysis.Properties.Monad
import Futhark.Analysis.Properties.Property (MonDir (..))
import Futhark.Analysis.Properties.Property qualified as Property
import Futhark.Analysis.Properties.Query
import Futhark.Analysis.Properties.Rewrite (rewrite, rewriteWithoutRules)
import Futhark.Analysis.Properties.Substitute (subst, (@))
import Futhark.Analysis.Properties.Symbol (Symbol (..), neg, sop2Symbol)
import Futhark.Analysis.Properties.SymbolPlus (repProperty)
import Futhark.Analysis.Properties.Unify
import Futhark.Analysis.Properties.Util
import Futhark.MonadFreshNames (VNameSource, newNameFromString, newVName)
import Futhark.SoP.Monad (addEquiv, addProperty, getProperties)
import Futhark.SoP.Refine (addRel)
import Futhark.SoP.SoP (Rel (..), SoP, int2SoP, justSym, mapSymSoP, negSoP, sym2SoP, (.+.), (.-.), (~*~), (~+~), (~-~))
import Language.Futhark qualified as E
import Language.Futhark.Semantic (FileModule (fileProg), ImportName, Imports)

propertyPrelude :: S.Set String
propertyPrelude =
  S.fromList
    [ "Monotonic",
      "Range",
      "Injective",
      "InjectiveRCD",
      "BijectiveRCD",
      "FiltPartInv",
      "FiltPart",
      "FiltPartInv2",
      "FiltPart2",
      "and"
    ]

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
mkIndexFnValBind val@(E.ValBind _ vn (Just te) _ _ params body _ _ val_loc)
  | hasRefinement te = do
      clearAlgEnv
      setOutputNames []
      printM 1 $
        emphString ("Analyzing " <> prettyStr (E.locText (E.srclocOf val_loc)))
          <> prettyStr val
      forM_ params addPreconditions
      forM_ params addBooleanNames
      forM_ params addSizeVariables
      indexfns <- forward body >>= mapM rewrite >>= bindfn vn
      checkPostcondition vn indexfns te
      insertTopLevel vn (params, indexfns, te)
      pure indexfns
  where
    -- Adds the effect of a precondition without checking that it holds.
    addPreconditions pat = do
      printM 1 $ "+ Adding precondition on " <> prettyStr pat
      ref <- getRefinement pat
      forM_ ref $ \(_, effect) -> effect emptyCheckContext
      printAlgEnv 3
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

checkPostcondition :: E.VName -> [IndexFn] -> E.TypeExp E.Exp E.VName -> IndexFnM ()
checkPostcondition vn indexfns te = do
  case getTERefine te of
    [e@(E.Lambda lam_params lam_body _ _ _)] -> do
      actual_names <- getOutputNames
      let param_names = map fst $ mconcat $ map patternMapAligned lam_params
      -- Propagate properties on the final let-body expression
      -- to the postcondition lambda parameterslambda parameters
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

{-
    Construct index function for source expression.
-}

forward :: E.Exp -> IndexFnM [IndexFn]
forward (E.Parens e _) = forward e
forward (E.Attr _ e _) = forward e
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
    Just indexfns -> do
      pure indexfns
    _ -> do
      dims <- shapeOfTypeBase (E.typeOf e)
      f <- case dims of
        [] ->
          -- Canonical scalar representation.
          pure [IndexFn [] (singleCase $ sVar vn)]
        shape -> do
          -- Canonical array representation.
          ids <-
            zipWithM (\_ i -> newVName i) shape $
              mconcat (repeat ["i", "j", "k"])
          pure
            [ IndexFn
                { shape = zipWith (\sz i -> Forall i (Iota sz)) shape ids,
                  body = singleCase . sym2SoP $ Apply (Var vn) (map sVar ids)
                }
            ]
      insertIndexFn vn f
      pure f
forward (E.TupLit xs _) = do
  mconcat <$> mapM forward xs
forward expr@(E.AppExp (E.Index e_xs slice _loc) _)
  | [E.DimFix e_idx] <- slice = do
      f_xss <- forward e_xs
      f_idxs <- forward e_idx
      forM (zip f_xss f_idxs) $ \(f_xs, f_idx) -> do
        checkBounds expr f_xs f_idx
        xs <- case justVName e_xs of
          Just vn -> pure vn
          Nothing -> do
            vn <- newVName "#xs"
            insertIndexFn vn [f_xs]
            pure vn
        idx <- newVName "#idx"

        -- We don't use substParams on f_xs because the substitution
        -- might fail here (e.g., if we are inside a map lambda).
        -- Lift idx.
        unless (null $ shape f_idx) $ error "E.Index: internal error"
        i <- newVName "i"
        insertIndexFn idx [IndexFn [Forall i (Iota $ int2SoP 1)] (body f_idx)]
        subst
          (IndexFn [] $ singleCase . sym2SoP $ Apply (Var xs) [sym2SoP (Apply (Var idx) [int2SoP 0])])
forward (E.Not e _) = do
  mapM negf =<< forward e
  where
    negf f =
      rewrite $ f {body = cmapValues (mapSymSoP (sym2SoP . neg)) (body f)}
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
          _ -> error ("forward not implemented for bin op: " <> show bop)
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
  dims_eq <- and <$> zipWithM eq (concatMap shape ts) (concatMap shape fs)
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
forward expr@(E.AppExp (E.Apply e_f args loc) _)
  | Just fname <- getFun e_f,
    "map" `L.isPrefixOf` fname,
    E.Lambda params lam_body _ _ _ : _args <- getArgs args,
    Just arrays <- NE.nonEmpty (NE.tail args) = do
      (outer_dim, aligned_args) <- zipArgsSOAC loc params arrays
      bindLambdaBodyParams (mconcat aligned_args)
      bodies <- rollbackAlgEnv $ do
        addRelIterator outer_dim
        forward lam_body

      forM bodies $ \f_body ->
        subst (f_body {shape = outer_dim : shape f_body}) >>= rewrite
  | Just fname <- getFun e_f,
    "map" `L.isPrefixOf` fname = do
      -- No need to handle map non-lambda yet as program can just be rewritten.
      error . errorMsg loc $
        "map takes lambda as first argument. Perhaps you want to eta-expand: "
          <> prettyStr (head $ getArgs args)
  | Just "replicate" <- getFun e_f,
    [e_n, e_x] <- getArgs args = do
      ns <- forward e_n
      xs <- forward e_x
      forM (zip ns xs) $ \(n, x) -> do
        i <- newVName "i"
        unless (rank n == 0) . error $ errorMsg loc "type error"
        m <- rewrite $ flattenCases (body n)
        rewrite $ IndexFn (Forall i (Iota m) : shape x) (body x)
  | Just "iota" <- getFun e_f,
    [e_n] <- getArgs args = do
      ns <- forward e_n
      forM ns $ \n -> do
        i <- newVName "i"
        case n of
          IndexFn [] cs -> do
            m <- rewrite $ flattenCases cs
            rewrite $ IndexFn [Forall i (Iota m)] (singleCase $ sVar i)
          _ ->
            error $ errorMsg loc "type error"
  | Just fname <- getFun e_f,
    "zip" `L.isPrefixOf` fname = do
      mconcat <$> mapM forward (getArgs args)
  | Just fname <- getFun e_f,
    "unzip" `L.isPrefixOf` fname,
    [xs'] <- getArgs args =
      -- XXX unzip is a no-op.
      forward xs'
  | Just fname <- getFun e_f,
    "flatten" `L.isPrefixOf` fname,
    [e_x] <- getArgs args = do
      fs <- forward e_x
      forM fs $ \f ->
        case shape f of
          ds | length ds <= 1 -> error $ "Flatten on less-than-2d array." <> prettyStr f
          Forall k (Iota m) : Forall j (Iota n) : shp -> do
            -- HACK j === (i' mod n), but we don't have modulo, so we convert to Cat domain.
            -- TODO use the multi-dim theory instead.
            k' <- newNameFromString "k"
            (_, flat_dim) <- flatten2d k' m n
            let Forall i (Cat _ _ e_k) = flat_dim
            let j' = sVar i .-. e_k
            pure $
              f
                { shape = flat_dim : shp,
                  body = repCases (addRep j j' $ mkRep k (sVar k')) (body f)
                }
          -- ALTERNATIVE: using II(i) with domain Iota (m * n).
          -- k' <- newNameFromString "k"
          -- let b i = i .*. n
          -- let flat_dim = Cat k' m (b (sVar k'))
          -- -- Also add II array; if this flattened array is substituted into a
          -- -- top-level definition it will be needed during substitution:
          -- --   def f flat_x =
          -- --     ...
          -- --   def g x =
          -- --     f (flatten x)
          -- -- because flat_x[i] gets substituted for, e.g., x(II(i), i - II(i) * n).
          -- i <- newNameFromString "i"
          -- let f_II = IndexFn [Forall i flat_dim] (cases [(Bool True, sym2SoP (Var k))])
          -- (vn_II, _) <- lookupII flat_dim f_II
          -- addRelSymbol (Prop (Property.Monotonic vn_II Inc))
          -- let ii = sym2SoP (Apply (Var vn_II) [sVar i])
          -- let j' = sVar i .-. b ii
          -- pure $
          --   f
          --     { shape = Forall i (Iota $ m .*. n) : shp,
          --       body = repCases (addRep j j' $ mkRep k ii) (body f)
          --     }
          _ -> error "Not implemented yet."
  | Just "scan" <- getFun e_f,
    [E.OpSection (E.QualName [] vn) _ _, _ne, xs'] <- getArgs args = do
      -- Scan with basic operator.
      fns <- forward xs'
      forM fns $ \fn -> do
        let i = case shape fn of
              [] -> error "scan array is empty?"
              [Forall i' _] -> i'
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
    [E.Lambda params lam_body _ _ _, _ne, _xs] <- getArgs args,
    xs <- NE.fromList [NE.last args],
    [pat_acc, pat_x] <- params = do
      -- We pick the first argument of the lambda to be the accumulator
      -- and the second argument to be an element of the input array.
      -- (The lambda is associative, so we are free to pick.)
      (outer_dim, aligned_args) <- zipArgsSOAC loc [pat_x] xs

      bindLambdaBodyParams (mconcat aligned_args)
      let accToRec = M.fromList (map (,sym2SoP Recurrence) $ E.patNames pat_acc)
      bodies <- rollbackAlgEnv $ do
        addRelIterator outer_dim
        map (repIndexFn accToRec) <$> forward lam_body

      forM bodies $ \f_body ->
        subst (f_body {shape = outer_dim : shape f_body}) >>= rewrite
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
        -- 1. Scatter in-bounds-monotonic indices:
        rule1 <- runMaybeT $ scatterSc1 dest (e_inds, inds) vals
        rule2 <- runMaybeT $ scatterSc2 dest (e_inds, inds) vals
        rule3 <- runMaybeT $ scatterSc3 dest (e_inds, inds) vals
        maybe
          (error $ errorMsg loc "Failed to infer index function for scatter.")
          pure
          (rule1 <|> rule2 <|> rule3)
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
                  { shape = [Forall i (Iota k)],
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
  | Just vn <- getFun e_f,
    vn `S.member` propertyPrelude = do
      (: []) <$> forwardPropertyPrelude vn args
  -- Applying other functions, for instance, user-defined ones.
  | (E.Var (E.QualName [] g) info _) <- e_f,
    E.Scalar (E.Arrow _ _ _ _ (E.RetType _ return_type)) <- E.unInfo info = do
      toplevel_fns <- getTopLevelIndexFns
      defs <- getTopLevelDefs
      case forwardApplyDef toplevel_fns defs expr of
        Just effect_and_fs ->
          -- g is a top-level definition.
          snd <$> effect_and_fs
        Nothing -> do
          -- g is a free variable in this expression (probably a parameter
          -- to the top-level function currently being analyzed).
          --
          -- We treat it as an uninterpreted function. Function congruence
          -- lets us check equality on g regardless:
          --   forall i,j . i = j => g(i) = g(j).
          arg_fns <- mconcat <$> mapM forward (getArgs args)
          size <- shapeOfTypeBase return_type
          arg_names <- forM arg_fns (const $ newVName "x")
          iter <- case size of
            [] ->
              pure []
            [sz] ->
              (: []) . flip Forall (Iota sz) <$> newVName "i"
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
forward e@(E.AppExp (E.Loop _sz _init_pat _init (E.For ident e_sz) e_body loc) _) = do
  let i = E.identName ident
  f_szs <- forward e_sz
  case map justSingleCase f_szs of
    [Just sz] -> do
      addRelSymbol (Prop $ Property.Rng i (int2SoP 0, sz))
      printM 1 $
        warningMsg loc "Analyzing loop body, but resulting index function will be uninterpreted."
      _ <- forward e_body
      vn <- newVName $ "untrans(" <> prettyStr e <> ")"
      pure [IndexFn [] (cases [(Bool True, sVar vn)])]
    _ -> error "not implemented yet"
forward e@(E.AppExp (E.Loop {}) _) = error $ "new loop who dis? " <> prettyStr e
forward (E.Coerce e _ _ _) = do
  -- No-op; I've only seen coercions that are hints for array sizes.
  forward e
forward e = do
  printM 1337 $ warningString "forward on unimplemented source expression: " <> prettyStr e <> "\n" <> show e
  vn <- newVName $ "untrans(" <> prettyStr e <> ")"
  pure [IndexFn [] (cases [(Bool True, sVar vn)])]

-- Propagate postcondition effects, such as adding properties.
-- Fires binding applications of top-level defs to names.
forwardLetEffects :: [Maybe E.VName] -> E.Exp -> IndexFnM [IndexFn]
forwardLetEffects [Just _] e@(E.Var (E.QualName _ _) _ loc) = do
  printM 0 . warningMsg loc $
    "Warning: Aliasing " <> prettyStr e <> " strips property information."
  forward e
forwardLetEffects bound_names x = do
  toplevel_fns <- getTopLevelIndexFns
  defs <- getTopLevelDefs
  case forwardApplyDef toplevel_fns defs x of
    Just effect_and_fns -> do
      (effect, fns) <- effect_and_fns
      bound_names' <- forM bound_names $ \case
        Just vn -> pure vn
        Nothing -> newNameFromString "_" -- HACK Apply effects to unusable wildcard.
      effect bound_names'
      printM 3 $ "Propating effects on " <> prettyStr bound_names'
      printAlgEnv 3
      pure fns
    Nothing -> forward x

-- Applying top-level definitions of functions.
-- Returns nothing if the function is not defined at top-level.
forwardApplyDef ::
  M.Map E.VName ([E.PatBase E.Info E.VName (E.TypeBase E.Size E.Diet)], [IndexFn], E.TypeExp E.Exp E.VName) ->
  M.Map E.VName ([E.PatBase E.Info E.VName (E.TypeBase E.Size E.Diet)], E.Exp) ->
  E.Exp ->
  Maybe (IndexFnM ([E.VName] -> IndexFnM (), [IndexFn]))
forwardApplyDef toplevel_fns defs (E.AppExp (E.Apply f args loc) _)
  | (E.Var (E.QualName [] g) info loc') <- f,
    E.Scalar (E.Arrow {}) <- E.unInfo info,
    Just (pats, indexfns, te) <- M.lookup g toplevel_fns = Just $ do
      -- A top-level definition with an index function.
      printM 5 $ "✨ Using index fn " <> prettyStr g

      (actual_args, actual_sizes) <- zipArgs loc' pats args

      -- Application is in ANF; rename formal arguments to actual arguments
      -- to check preconditions.
      let name_rep = renamingRep (mconcat actual_args)
      size_rep <- checkPreconditions g pats (actual_args, actual_sizes) name_rep

      fs <- forM indexfns $ \fn -> do
        substParams (repIndexFn size_rep fn) (mconcat actual_args)
          >>= rewrite
      pure (mkEffectFromTypeExp te size_rep (mconcat actual_args), fs)
  | (E.Var (E.QualName [] g) info loc') <- f,
    E.Scalar (E.Arrow {}) <- E.unInfo info,
    Just (pats, e) <- M.lookup g defs = Just $ do
      -- A top-level definition without an index function (no postcondition).
      --
      -- NOTE This "inlines" the definition of top-level definition,
      -- as opposed to the treatment for "top-level index functions"
      -- where the args are substituted into the previously analyzed
      -- index function. (Less work, but more likely to fail.
      -- For example, substituting into a Sum fails in some cases.)
      printM 5 $ "✨ Using top-level def " <> prettyStr g
      (actual_args, actual_sizes) <- zipArgs loc' pats args
      let name_rep = renamingRep (mconcat actual_args)
      _ <- checkPreconditions g pats (actual_args, actual_sizes) name_rep

      forM_ (mconcat actual_sizes) $ \(n, sz) -> do
        addEquiv (Algebra.Var n) =<< toAlgebra sz
      forM_ (mconcat actual_args) $ \(vn, fn) ->
        void $ bindfn_ 1337 vn [fn]
      (const $ pure (),) <$> forward e
  where
    -- Assume ANF to handle name substitutions in Properties.
    --
    -- HINT This limitation only exists because FiltPart Y X needs to substitute
    -- the name X for the corresponding argument, for instance, in
    --   def filter X p : \Y -> FiltPart Y X p _ = ...
    --   def f ... =
    --     let ys = filter p x
    --     --> FiltPart ys x p _
    -- and we want x to be meaningful to the user (i.e., to not bind X to fresh X'
    -- and X' to x).
    checkANF [] = []
    checkANF (arg : as) =
      case justVName arg of
        Just vn -> vn : checkANF as
        _ ->
          error . errorMsg (E.locOf arg) $
            "Limitation: Application of top-level definitions"
              <> " with postconditions must be in A-normal form: "
              <> prettyStr arg

    renamingRep actual_args =
      let arg_vns = checkANF (getArgs args)
       in mkRepFromList
            (zipWith (\k v -> (fst k, sym2SoP $ Var v)) actual_args arg_vns)

    mkEffectFromTypeExp te size_rep actual_args vns =
      forM_ (getTERefine te) $ \ref_exp -> do
        -- Bounds checking is disabled because the bounds in the refinement
        -- of te was already checked previously (when creating the
        -- top level index funciton used here).
        -- We don't simply check bounds again because they might rely on a
        -- context that is now out of scope.
        (_check, effect) <- withoutBoundsChecks (mkRef vns Var ref_exp)
        effect (size_rep, actual_args, renamingRep actual_args)

    -- Puts parameters in scope one-by-one before checking preconditions;
    -- the refinement of a parameter can use previous parameters:
    --   (x : []i64) (n : {i64 | (== sum x))
    checkPreconditions g pats (actual_args, actual_sizes) name_rep = do
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
        -- TODO could probably be deduplicated by merging with mkEffectFromTypeExp.
        checkPatPrecondition scope pat size_rep = do
          conds <- getPrecondition pat
          answers <- forM conds $ \check -> do
            printM 1 $
              "Checking precondition " <> prettyStr pat <> " for " <> prettyStr g
            check (size_rep, scope, name_rep)
          unless (all isYes answers) . error . errorMsg loc $
            "Failed to show precondition " <> prettyStr pat <> " in context: " <> prettyStr scope
forwardApplyDef _ _ _ = Nothing

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
    "Range"
      | [e_X, e_rng] <- getArgs args,
        Just x <- justVName e_X -> do
          f_rng <- forward e_rng
          case f_rng of
            [IndexFn [] g_a, IndexFn [] g_b] ->
              fmap (IndexFn []) . simplify . cases $ do
                (p_a, a) <- casesToList g_a
                (p_b, b) <- casesToList g_b
                pure (p_a :&& p_b, pr $ Property.Rng x (a, b))
            _ ->
              undefined
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
        Just [f_X] | [Forall i _] <- shape f_X -> rollbackAlgEnv $ do
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
scatterSs1 (IndexFn (Forall _ d_xs : _) _) (e_is, is) = do
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
  answerFromBool $ all ((`S.notMember` fv (body vs)) . boundVar) (shape vs)

-- TODO implement (extract vn for values and lookup Range property).
scatterSs3 :: (Applicative f) => p -> f Answer
scatterSs3 _ = pure Unknown

scatterSafe :: IndexFn -> (E.Exp, IndexFn) -> IndexFn -> IndexFnM Answer
scatterSafe xs is vs = scatterSs1 xs is `orM` pure (scatterSs2 vs) `orM` scatterSs3 vs

-- TODO also look up in env to see if there is a `Bij is Y Z` property with Z <= (0, dest_size) <= Y.
scatterSc1 :: IndexFn -> (E.Exp, IndexFn) -> IndexFn -> MaybeT IndexFnM IndexFn
scatterSc1 xs@(IndexFn (Forall i dom_dest : _) _) (e_is, is) vs
  | rank is == 1,
    rank xs == rank vs = do
      safe <- lift $ scatterSafe xs (e_is, is) vs
      when (isUnknown safe) (failMsg "scatterSc1: unable to show safety")
      dest_size <- lift $ rewrite $ domainEnd dom_dest
      printM 1337 $ "scatterSc1: dest_size " <> prettyStr dest_size
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
          hole <- sym2SoP . Hole <$> newVName "h"
          let wrap = (`Apply` [hole]) . Var
          alg_vn <- lift $ paramToAlgebra vn_inv wrap
          lift $ addRelSymbol (Prop $ Property.Rng alg_vn (int2SoP 0, dest_size))

          -- let inv_i = Apply (Var vn_inv) (sVar . boundVar <$> shape xs)
          let inv_i = sym2SoP (Apply (Var vn_inv) [sVar i])
          let inner_dims = drop 1 (sVar . boundVar <$> shape xs)
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

scatterSc2 :: IndexFn -> (E.Exp, IndexFn) -> IndexFn -> MaybeT IndexFnM IndexFn
scatterSc2 xs@(IndexFn [Forall _ d_xs] _) (e_is, is@(IndexFn [Forall k (Iota m)] _)) vs = do
  safe <- lift $ scatterSafe xs (e_is, is) vs
  when (isUnknown safe) (failMsg "scatterSc2: unable to show safety")
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
  printM 1337 $ "answers " <> prettyStr in_bounds
  printM 1337 $ "sanity  " <> prettyStr sanity_check
  printM 1337 $ "scatterSc2: is " <> prettyStr is
  -- Sort branches so that indices in the domain of xs come before out-of-bounds indices.
  let gs = L.sortOn fst $ zip in_bounds (guards is)
  printM 1337 $ "scatterSc2: gs " <> prettyStr (map snd gs)
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
              { shape = [Forall i (Cat k m e)],
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
scatterSc3 :: IndexFn -> (E.Exp, IndexFn) -> IndexFn -> MaybeT IndexFnM IndexFn
scatterSc3 xs@(IndexFn [Forall i dom_dest] _) (e_is, is) vs = do
  safe <- lift $ scatterSafe xs (e_is, is) vs
  when (isUnknown safe) (failMsg "scatterSc3: unable to show safety")
  uninterpreted <- newNameFromString "safe_scatter"
  lift . pure $
    IndexFn
      { shape = [Forall i dom_dest],
        body = cases [(Bool True, sym2SoP $ Apply (Var uninterpreted) [sVar i])]
      }
scatterSc3 _ _ _ = fail ""

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

shapeOfTypeBase :: E.TypeBase E.Exp as -> IndexFnM [SoP Symbol]
shapeOfTypeBase e = do
  dims <- mapM forward (E.shapeDims (E.arrayShape e))
  unless (all ((<= 1) . length) dims) $ error "Size of array dimension is a tuple?"
  pure $ map getScalar (mconcat dims)
  where
    getScalar (IndexFn [] cs) | [(Bool True, x)] <- casesToList cs = x
    getScalar f = error ("getScalar on " <> prettyStr f)

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

-- Given names of lambda parameters and rank-`n` index functions whose
-- outer dimension is being iterated over (e.g., in a map or scan),
-- bind those names to corresponding index functions of rank `n - 1`.
--
-- Assumes that outer dimensions are equivalent across index functions.
-- Returns the most "complex" iterator over these domains.
-- For example, this would transform the lambda body of the following
--   map (\x y z -> x + y + z) xs ys zs
-- into
--   map (\i -> xs[i] + ys[i] + zs[i]) (indices xs)
-- where xs is the index function with the most "complex" iterator.
-- For multiple dimensions, the most complex iterator is chosen for
-- each dimension. And the parameters are not scalar; the outer
-- dimension is just dropped.
bindLambdaBodyParams :: [(E.VName, IndexFn)] -> IndexFnM ()
bindLambdaBodyParams = mapM_ (\(vn, f) -> insertIndexFn vn [f])

-- Like zipArgs, but for zipping parameters of SOAC's lambda with its
-- array arguments:
--   * the outer dimension of each argument is dropped (each parameters'
--     rank is one less than its corresponding array's rank)
--   * a new common outer dimension is returned (corresponding to the
--     most complex outer dimension of the arrays)
zipArgsSOAC ::
  E.SrcLoc ->
  [E.Pat E.ParamType] ->
  NE.NonEmpty (a, E.Exp) ->
  IndexFnM (Iterator, [[(E.VName, IndexFn)]])
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
        body = singleCase . sym2SoP $ Apply (Var vn) (iters new_shape)
      }
      @ (vn, f)
  -- Substitutions may have renamed Cat `k`s; do a common rename again.
  args'' <- renameSameL (mapM . mapM) args'
  let new_outer_dim' = head (shape (head (head args'')))
  -- Drop the new outer dim, aligning arguments with parameters.
  (aligned_args, _) <- zipArgs' loc formal_args (map (map dropOuterDim) args'')
  pure (new_outer_dim', aligned_args)
  where
    dropOuterDim f = f {shape = drop 1 (shape f)}
    iters = map (sVar . boundVar)

-- Align parameter patterns with their arguments---or raise an error.
-- A parameter pattern reduces to a list of (optional) names with type information.
-- An argument is a source expression, which `forward` will return a list
-- of index functions for.
-- For each pattern and argument, the two lists must correspond.
zipArgs ::
  E.SrcLoc ->
  [E.Pat E.ParamType] ->
  NE.NonEmpty (a, E.Exp) ->
  IndexFnM ([[(E.VName, IndexFn)]], [[(E.VName, SoP Symbol)]])
zipArgs loc formal_args actual_args =
  zipArgs' loc formal_args =<< mapM forward (getArgs actual_args)

zipArgs' ::
  E.SrcLoc ->
  [E.Pat E.ParamType] ->
  [[IndexFn]] ->
  IndexFnM ([[(E.VName, IndexFn)]], [[(E.VName, SoP Symbol)]])
zipArgs' loc formal_args actual_args = do
  let pats = map patternMapAligned formal_args
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
        type_shape <- shapeOfTypeBase pat_type
        unless (length type_shape == rank f) . error $
          errorMsg loc "Internal error: parameter and argument sizes do not align."
        let size_vars = map getVName type_shape
        sizes <- mapM (domainSize . formula) (shape f)
        pure . catMaybes $ zipWith (\sz -> fmap (,sz)) sizes size_vars

  pure (aligned_args, aligned_sizes)
  where
    getVName x
      | Just (Var vn) <- justSym x = Just vn
      | otherwise = Nothing

    domainSize d = rewrite (domainEnd d .-. domainStart d .+. int2SoP 1)

substParams :: (Foldable t) => IndexFn -> t (E.VName, IndexFn) -> IndexFnM IndexFn
substParams = foldM substParam
  where
    -- We want to simplify, but avoid rewriting recurrences during
    -- paramter-substitution.
    substParam fn (paramName, paramIndexFn) =
      (fn @ (paramName, paramIndexFn)) >>= rewriteWithoutRules

failMsg :: (MonadFail m) => String -> m b
failMsg msg = do
  printM 1337 msg
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
    hole <- sym2SoP . Hole <$> newVName "h"
    pure <$> mkRef vns ((`Apply` [hole]) . Var) ref
  E.Scalar (E.Refinement _ ref) ->
    pure <$> mkRef vns Var ref
  _ -> pure []

mkRef :: [E.VName] -> (E.VName -> Symbol) -> E.Exp -> IndexFnM (Check, Effect)
mkRef vns wrap refexp = case refexp of
  E.OpSectionRight (E.QualName [] vn_op) _ e_y _ _ _
    | [name] <- vns -> do
        let rel = fromJust $ parseOpVName vn_op
        g <- forwardRefinementExp e_y
        -- Create check as an index function whose cases contain the refinement.
        let check =
              inContext
                askRefinement
                (cases $ map (second (sym2SoP . (sVar name `rel`))) (casesToList g))
        let effect =
              inContext
                ( \f -> do
                    -- (We allow Holes in wrap and toAlgebra cannot be called on symbols with Holes.)
                    alg_vn <- paramToAlgebra name wrap
                    y <- rewrite $ flattenCases (body f)
                    addRelSymbol $ sVar alg_vn `rel` y
                )
                g
        pure (check, effect)
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
  when (typeIsBool t) $ addProperty (Algebra.Var param) Property.Boolean
addSizeVariables _ = pure ()

parseOpVName :: E.VName -> Maybe (SoP Symbol -> SoP Symbol -> Symbol)
parseOpVName vn =
  case E.baseString vn of
    ">" -> Just (:>)
    ">=" -> Just (:>=)
    "<" -> Just (:<)
    "<=" -> Just (:<=)
    "==" -> Just (:==)
    "!=" -> Just (:/=)
    "+<" -> Just (\x y -> int2SoP 0 :<= x :&& x :< y)
    _ -> Nothing

{-
    Bounds checking.
-}
checkBounds :: E.Exp -> IndexFn -> IndexFn -> IndexFnM ()
checkBounds _ (IndexFn [] _) _ =
  error "E.Index: Indexing into scalar"
checkBounds e f_xs@(IndexFn [Forall _ df] _) f_idx = algebraContext f_idx $ do
  c <- getCheckBounds
  when c $ do
    df_start <- rewrite $ domainStart df
    df_end <- rewrite $ domainEnd df
    case df of
      Cat _ _ b -> do
        doCheck (\idx -> b :<= idx :|| df_start :<= idx)
        doCheck (\idx -> idx :<= intervalEnd df :|| idx :<= df_end)
      Iota _ -> do
        doCheck (df_start :<=)
        doCheck (:<= df_end)
    printM 1 . locMsg (E.locOf e) $ prettyStr e <> greenString " OK"
  where
    doCheck :: (SoP Symbol -> Symbol) -> IndexFnM ()
    doCheck bound = do
      _ <- foreachCase f_idx $ \n -> do
        c <- isYes <$> queryCase (CaseCheck bound) f_idx n
        unless c $ do
          printExtraDebugInfo n
          let (p_idx, e_idx) = getCase n $ body f_idx
          error . errorMsg (E.locOf e) $
            "Unsafe indexing: "
              <> prettyStr e
              <> " (failed to show: "
              <> prettyStr p_idx
              <> " => "
              <> prettyStr (bound e_idx)
              <> ")."
      pure ()
      where
        -- TODO remove this.
        printExtraDebugInfo n = do
          env <- getAlgEnv
          printM 100 $
            "Failed bounds-checking:"
              <> "\nf_xs:"
              <> prettyStr f_xs
              <> "\nf_idx: "
              <> prettyStr f_idx
              <> "\nCASE f_idx: "
              <> show n
              <> "\nUnder AlgEnv:"
              <> prettyStr env
