module Futhark.Analysis.Proofs.Convert where

import Control.Monad (foldM, forM, unless, when)
import Control.Monad.RWS (gets)
import Data.Bifunctor
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust, fromJust)
import Debug.Trace (traceM)
import Futhark.Analysis.Proofs.IndexFn (Cases (Cases), Domain (..), IndexFn (..), Iterator (..), cases)
import Futhark.Analysis.Proofs.Monad
import Futhark.Analysis.Proofs.IndexFnPlus (subst, domainEnd)
import Futhark.Analysis.Proofs.Rewrite (rewrite)
import Futhark.Analysis.Proofs.Symbol (Symbol (..), neg, sop2Symbol)
import Futhark.Analysis.Proofs.Util (prettyBinding)
import Futhark.MonadFreshNames (VNameSource, newVName)
import Futhark.SoP.SoP (SoP, int2SoP, mapSymSoP_, negSoP, sym2SoP, (~+~), (~*~), (~-~), justSym, (.-.))
import Futhark.Util.Pretty (prettyString)
import Language.Futhark qualified as E
import Language.Futhark.Semantic (FileModule (fileProg), ImportName, Imports)
import Futhark.Analysis.Proofs.Unify (unify, Substitution (mapping), mkRep, rep)
import Futhark.SoP.Monad (addProperty)
import qualified Futhark.Analysis.Proofs.AlgebraPC.Symbol as Algebra
import Futhark.Analysis.Proofs.AlgebraBridge.Translate (algebraContext)
import Futhark.Analysis.Proofs.Query (Query(..), askQ, Answer (..), isUnknown, MonoDir (..))
import Futhark.Analysis.Proofs.AlgebraBridge (($==))

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
-- sizeOfTypeBase (E.Scalar (E.Refinement ty _)) =
--   -- TODO why are all refinements scalar?
--   sizeOfTypeBase ty
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

funPrimTypeIsBool :: E.TypeBase E.Exp as -> Bool
funPrimTypeIsBool (E.Scalar (E.Arrow _ _ _ _ return_type)) =
  funPrimTypeIsBool (E.retType return_type)
funPrimTypeIsBool (E.Scalar (E.Prim E.Bool)) = True
funPrimTypeIsBool _ = False

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
mkIndexFnValBind val@(E.ValBind _ vn _ret _ _ _params body _ _ _) = do
  clearAlgEnv
  debugPrettyM "\n====\nmkIndexFnValBind:\n\n" val
  indexfn <- forward body >>= refineAndBind vn
  -- insertTopLevel vn (params, indexfn)
  _ <- algebraContext indexfn $ do
      alg <- gets algenv
      debugPrettyM "Algebra context for indexfn:\n" alg
  pure indexfn

refineAndBind :: E.VName -> IndexFn -> IndexFnM IndexFn
refineAndBind vn indexfn = do
  indexfn' <- rewrite indexfn
  insertIndexFn vn indexfn'
  whenDebug (traceM $ prettyBinding vn indexfn')
  -- tell ["resulting in", toLaTeX (vn, indexfn')]
  pure indexfn'

singleCase :: a -> Cases Symbol a
singleCase e = cases [(Bool True, e)]

fromScalar :: SoP Symbol -> IndexFn
fromScalar e = IndexFn Empty (singleCase e)

forward :: E.Exp -> IndexFnM IndexFn
forward (E.Parens e _) = forward e
forward (E.Attr _ e _) = forward e
-- Let-bindings.
forward (E.AppExp (E.LetPat _ (E.Id vn _ _) x body _) _) = do
  -- tell [textbf "Forward on " <> Math.math (toLaTeX vn) <> toLaTeX x]
  _ <- refineAndBind vn =<< forward x
  forward body
-- Tuples left unhandled for now.
-- forward (E.AppExp (E.LetPat _ p@(E.TuplePat patterns _) x body _) _) = do
--     -- tell [textbf "Forward on " <> Math.math (toLaTeX (S.toList $ E.patNames p)) <> toLaTeX x]
--     xs <- unzipT <$> forward x
--     forM_ (zip patterns xs) refineAndBind'
--     forward body
--     where
--       -- Wrap refineAndBind to discard results otherwise bound to wildcards.
--       refineAndBind' (E.Wildcard {}, _) = pure ()
--       refineAndBind' (E.Id vn _ _, indexfn) =
--         void (refineAndBind vn indexfn)
--       refineAndBind' e = error ("not implemented for " <> show e)
-- Leaves.
forward (E.Literal (E.BoolValue x) _) =
  pure . fromScalar . sym2SoP $ Bool x
forward (E.Literal (E.SignedValue (E.Int64Value x)) _) =
  pure . fromScalar . int2SoP $ toInteger x
forward (E.IntLit x _ _) =
  pure . fromScalar $ int2SoP x
forward (E.Negate (E.IntLit x _ _) _) =
  pure . fromScalar . negSoP $ int2SoP x
forward e@(E.Var (E.QualName _ vn) _ _) = do
  indexfns <- gets indexfns
  case M.lookup vn indexfns of
    Just indexfn -> do
      pure indexfn
    _ -> do
      -- TODO handle refinement types
      -- handleRefinementTypes e
      case getSize e of
        Just sz -> do
          -- Canonical array representation.
          i <- newVName "i"
          rewrite $
            IndexFn
              (Forall i (Iota sz))
              (singleCase . sym2SoP $ Idx (Var vn) (sym2SoP $ Var i))
        Nothing ->
          -- Canonical scalar representation.
          rewrite $ IndexFn Empty (singleCase . sym2SoP $ Var vn)
-- Nodes.
-- TODO handle tuples later.
-- forward (E.TupLit es _) = do
--   xs <- mapM forward es
--   vns <- mapM (\_ -> newVName "xs") xs
--   let IndexFn iter1 _ = head xs
--   foldM (\acc (vn, x) -> sub vn x acc)
--         (IndexFn iter1 (toCases . Tuple $ map Var vns))
--         (zip vns xs)
--     >>= rewrite
forward (E.AppExp (E.Index xs' slice _) _)
  | [E.DimFix idx'] <- slice = do
      -- XXX support only simple indexing for now
      IndexFn iter_idx idx <- forward idx'
      IndexFn iter_xs xs <- forward xs'
      case iter_xs of
        Forall j _ -> do
          subst j (IndexFn iter_idx idx) (IndexFn iter_idx xs)
        _ ->
          error "indexing into a scalar"
forward (E.Not e _) = do
  IndexFn it e' <- forward e
  rewrite $ IndexFn it $ cmapValues (mapSymSoP_ neg) e'
forward (E.AppExp (E.BinOp (op', _) _ (x', _) (y', _) _) _)
  | E.baseTag (E.qualLeaf op') <= E.maxIntrinsicTag,
    name <- E.baseString $ E.qualLeaf op',
    Just bop <- L.find ((name ==) . prettyString) [minBound .. maxBound :: E.BinOp] = do
      vx <- forward x'
      let IndexFn iter_x _ = vx
      vy <- forward y'
      a <- newVName "a"
      b <- newVName "b"
      let doOp op =
            subst a vx (IndexFn iter_x (singleCase $ op (Var a) (Var b)))
              >>= subst b vy
              >>= rewrite
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
  cond <- newVName "cond"
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
  subst cond (IndexFn iter_c c') y
    >>= subst t_branch vt
    >>= subst f_branch vf
    >>= rewrite
-- forward e | trace ("forward\n  " ++ prettyString e) False =
--   -- All calls after this case get traced.
--   undefined
forward expr@(E.AppExp (E.Apply f args _) _)
  | Just fname <- getFun f,
    "map" `L.isPrefixOf` fname,
    E.Lambda params body _ _ _ : args' <- getArgs args = do
      xss <- mapM forward args'
      debugPrettyM "map args:" xss
      let IndexFn iter_first_arg _ = head xss
      -- TODO use iter_body; likely needed for nested maps?
      IndexFn iter_body cases_body <- forward body
      unless
        (iter_body == iter_first_arg || iter_body == Empty)
        ( error $
            "map internal error: iter_body != iter_first_arg"
              <> show iter_body
              <> show iter_first_arg
        )
      -- Make susbtitutions from function arguments to array names.
      let paramNames :: [E.VName] = concatMap E.patNames params
      -- TODO handle tupled values by splitting them into separate index functions
      -- let xss_flat :: [IndexFn] = mconcat $ map unzipT xss
      let xss_flat = xss
      let y' = IndexFn iter_first_arg cases_body
      -- tell ["Using map rule ", toLaTeX y']
      foldM substParams y' (zip paramNames xss_flat)
        >>= rewrite
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
  | Just "scan" <- getFun f,
    [E.OpSection (E.QualName [] vn) _ _, _ne, xs'] <- getArgs args = do
      -- Scan with basic operator.
      IndexFn iter_xs xs <- forward xs'
      let i = case iter_xs of
            (Forall i' _) -> i'
            Empty -> error "scan array is empty?"
      -- TODO should verify that _ne matches op
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
                  [(base_case, sym2SoP (Idx (Var x) (sVar i))),
                   (neg base_case, Recurrence `op` Idx (Var x) (sVar i))]
              )
      -- tell ["Using scan rule ", toLaTeX y]
      subst x (IndexFn iter_xs xs) y
        >>= rewrite
  | Just "scatter" <- getFun f,
    [dest_arg, inds_arg, vals_arg] <- getArgs args = do
    -- Scatter in-bounds-monotonic indices.
    --
    -- y = scatter dest inds vals
    -- where
    --   inds = ∀k ∈ [0, ..., m-1] .
    --       | p(k)  => seg(k)
    --       | ¬p(k) => OOB
    --   seg(0) is 0
    --   seg(k) is monotonically increasing
    --   dest has size seg(m) - 1         (to ensure conclusion covers all of dest)
    --   OOB < 0 or OOB >= seg(m) - 1
    -- ___________________________________________________
    -- y = ∀i ∈ ⊎k=iota m [seg(k), ..., seg(k+1) - 1] .
    --     | i == inds[k] => vals[k]
    --     | i /= inds[k] => dest[i]
    --
    -- equivalent to
    --
    -- y = ∀i ∈ ⊎k=iota m [seg(k), ..., seg(k+1) - 1] .
    --     | p(k) => vals[k]
    --     | ¬p(k) => dest[i]
    --
    -- From type checking, we have:
    -- scatter : (dest : [n]t) -> (inds : [m]i64) -> (vals : [m]t) : [n]t
    -- * inds and vals are same size
    -- * dest and result are same size
    debugM "scatter"
    dest <- forward dest_arg
    inds <- forward inds_arg
    vals <- forward vals_arg
    debugPrettyM "dest" dest
    debugPrettyM "inds" inds
    debugPrettyM "vals" vals
    -- 1. Check that inds is on the right form.
    vn_k <- newVName "k"
    vn_m <- newVName "m"
    vn_p0 <- newVName "p0"
    vn_f0 <- newVName "f0"
    vn_p1 <- newVName "p1"
    vn_f1 <- newVName "f1"
    let inds_template =
          IndexFn {
            iterator = Forall vn_k (Iota $ sym2SoP $ Hole vn_m),
            body = cases [(Hole vn_p0, sym2SoP $ Hole vn_f0),
                          (Hole vn_p1, sym2SoP $ Hole vn_f1)]
            }
    s <- fromMaybe (error "unhandled scatter") <$> unify inds_template inds
    debugPrettyM "s" s
    -- Safe to do this now:
    let IndexFn (Forall k (Iota m)) _ = inds
    -- Determine which is OOB and which is e1.
    let isOOB ub = CaseTransform (\c -> c :< int2SoP 0 :|| (mapping s M.! ub) :<= c)
    (case_idx_seg, p_seg, f_seg) <- do
      case0_is_OOB <- askQ (isOOB vn_f1) inds 0
      case case0_is_OOB of
        Yes -> pure (1, vn_p1, vn_f1)
        Unknown -> do
          case1_is_OOB <- askQ (isOOB vn_f0) inds 1
          case case1_is_OOB of
            Yes -> pure (0, vn_p0, vn_f0)
            Unknown -> error "scatter: unable to determine OOB branch"
    -- Check that seg(0) = 0 and that seg is monotonically increasing.
    let x `at_k` i = rep (mkRep k i) x
    let zero :: SoP Symbol = int2SoP 0
    eq0 <- askQ (CaseTransform (\seg -> seg `at_k` zero :== int2SoP 0)) inds case_idx_seg
    when (isUnknown eq0) $ error "scatter: unable to determine segment start"
    debugPrettyM "seg(0) = 0" eq0
    -- Check that seg is monotonically increasing.
    mono <- askQ (CaseIsMonotonic Inc) inds case_idx_seg
    debugPrettyM "mono" mono
    when (isUnknown mono) $ error "scatter: unable to show segment monotonicity"
    -- ^ TODO need to put pre-requisite on shape that shape >= 0 for mk_flag to go through here.
    -- Check that the proposed end of segments seg(m) - 1 equals the size of dest.
    -- (Note that has to hold outside the context of inds, so we cannot assume p_seg.)
    let IndexFn (Forall _ dom_dest) _ = dest
    let dest_size = domainEnd dom_dest
    let seg = mapping s M.! f_seg
    domain_covered <- seg `at_k` m .-. int2SoP 1 $== dest_size
    debugPrettyM "domain_covered" domain_covered
    -- y = ∀i ∈ (⊎k=[0,...,m-1] [seg(k), ..., seg(k+1) - 1]) .
    --     | p(k) => vals[k]
    --     | ¬p(k) => dest[i]
    i <- newVName "i"
    dest_vn <- newVName "dest_vn"
    vals_vn <- newVName "vals_vn"
    let p = sop2Symbol $ mapping s M.! p_seg
    let fn = IndexFn {
      iterator = Forall i (Cat k m seg),
      body = cases [(p, sym2SoP $ Apply (Var vals_vn) [sVar k]),
                    (neg p, sym2SoP $ Apply (Var dest_vn) [sVar i])]
    }
    subst vals_vn vals fn
      >>= subst dest_vn dest
      >>= rewrite
  -- Applying other functions, for instance, user-defined ones.
  | (E.Var (E.QualName [] g) info _) <- f,
    args' <- getArgs args,
    E.Scalar (E.Arrow _ _ _ _ (E.RetType _ return_type)) <- E.unInfo info = do
      toplevel <- gets toplevel
      case M.lookup g toplevel of
        Just (_param_names, _param_sizes, _indexfn) ->
          -- g is a previously analyzed user-defined top-level function.
          error "use of top-level defs not implemented yet"
        Nothing -> do
          -- g is a free variable in this expression (probably a parameter
          -- to the top-level function currently being analyzed).
          params <- mapM forward args'
          param_names <- forM params (const $ newVName "x")
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
                        Apply (Var g) (map (sym2SoP . Var) param_names)
                  }
          debugPrettyM "g_fn:" g_fn
          debugPrettyM2 "g:" g
          let booltype = funPrimTypeIsBool return_type
          when booltype $ addProperty (Algebra.Var g) Algebra.Boolean
          foldM substParams g_fn (zip param_names params)
            >>= rewrite
forward e = error $ "forward on " <> show e

substParams :: IndexFn -> (E.VName, IndexFn) -> IndexFnM IndexFn
substParams y (paramName, paramIndexFn) =
  subst paramName paramIndexFn y >>= rewrite

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
