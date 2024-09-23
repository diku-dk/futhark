module Futhark.Analysis.Proofs.Convert where

import Control.Monad (foldM, unless)
import Control.Monad.RWS
import Data.Bifunctor
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Futhark.Analysis.Proofs.IndexFn (Cases (Cases), Domain (..), IndexFn (..), IndexFnM, Iterator (..), VEnv (..), cases, clearAlgEnv, debugM, insertIndexFn, runIndexFnM, debugPrettyM)
import Futhark.Analysis.Proofs.IndexFnPlus (subst)
import Futhark.Analysis.Proofs.Rewrite (rewrite)
import Futhark.Analysis.Proofs.Symbol (Symbol (..))
import Futhark.Analysis.Proofs.Util (prettyBinding)
import Futhark.MonadFreshNames (VNameSource, newNameFromString)
import Futhark.SoP.SoP (SoP, int2SoP, mapSymSoP_, negSoP, sym2SoP, (.*.), (.+.), (.-.))
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
-- sizeOfTypeBase (E.Scalar (E.Refinement ty _)) =
--   -- TODO why are all refinements scalar?
--   sizeOfTypeBase ty
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
mkIndexFnValBind :: E.ValBind -> IndexFnM (Maybe IndexFn)
mkIndexFnValBind val@(E.ValBind _ vn ret _ _ params body _ _ _) = do
  clearAlgEnv
  debugM ("\n====\nmkIndexFnValBind:\n\n" <> prettyString val)
  indexfn <- forward body >>= refineAndBind vn
  -- insertTopLevel vn (params, indexfn)
  algenv <- gets algenv
  debugM ("AlgEnv\n" <> prettyString algenv)
  pure (Just indexfn)

refineAndBind :: E.VName -> IndexFn -> IndexFnM IndexFn
refineAndBind vn indexfn = do
  indexfn' <- rewrite indexfn
  insertIndexFn vn indexfn'
  debugM (prettyBinding vn indexfn')
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
forward (E.AppExp (E.LetPat _ p@(E.Id vn _ _) x body _) _) = do
  -- debugM (prettyString p <> " = " <> prettyString x)
  -- tell [textbf "Forward on " <> Math.math (toLaTeX vn) <> toLaTeX x]
  _ <- refineAndBind vn =<< forward x
  forward body
-- Tuples left unhandled for now.
-- forward (E.AppExp (E.LetPat _ p@(E.TuplePat patterns _) x body _) _) = do
--     debugM (prettyString patterns <> " = " <> prettyString x)
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
      -- debugM ("using index function " <> prettyString vn <> " = " <> prettyString indexfn)
      pure indexfn
    _ -> do
      -- debugM ("creating index function for " <> prettyString vn)
      -- TODO handle refinement types
      -- handleRefinementTypes e
      case getSize e of
        Just sz -> do
          -- Canonical array representation.
          i <- newNameFromString "i"
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
--   vns <- mapM (\_ -> newNameFromString "xs") xs
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
  rewrite $ IndexFn it $ cmapValues (mapSymSoP_ Not) e'
forward (E.AppExp (E.BinOp (op', _) _ (x', _) (y', _) _) _)
  | E.baseTag (E.qualLeaf op') <= E.maxIntrinsicTag,
    name <- E.baseString $ E.qualLeaf op',
    Just bop <- L.find ((name ==) . prettyString) [minBound .. maxBound :: E.BinOp] = do
      vx <- forward x'
      let IndexFn iter_x _ = vx
      vy <- forward y'
      a <- newNameFromString "a"
      b <- newNameFromString "b"
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
  cond <- newNameFromString "cond"
  t_branch <- newNameFromString "t_branch"
  f_branch <- newNameFromString "f_branch"
  let y =
        IndexFn
          iter_c
          ( cases
              [ (Var cond, sym2SoP $ Var t_branch),
                (Not $ Var cond, sym2SoP $ Var f_branch)
              ]
          )
  subst cond (IndexFn iter_c c') y
    >>= subst t_branch vt
    >>= subst f_branch vf
    >>= rewrite
-- forward e | trace ("forward\n  " ++ prettyString e) False =
--   -- All calls after this case get traced.
--   undefined
forward (E.AppExp (E.Apply f args _) _)
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
      -- TODO `map E.patNames params` is a [Set], I assume because we might have
      --   map (\(x, y) -> ...) xys
      -- meaning x needs to be substituted by x[i].0
      let paramNames :: [E.VName] = concatMap E.patNames params
      -- TODO handle tupled values by splitting them into separate index functions
      -- let xss_flat :: [IndexFn] = mconcat $ map unzipT xss
      let xss_flat = xss
      let y' = IndexFn iter_first_arg cases_body
      debugPrettyM "map template:" y'
      -- tell ["Using map rule ", toLaTeX y']
      res <- foldM substParams y' (zip paramNames xss_flat)
      debugPrettyM "map substituted:" res
      rewrite res
      -- foldM substParams y' (zip paramNames xss_flat)
      --   >>= rewrite
  | Just "replicate" <- getFun f,
    [n, x] <- getArgs args = do
      n' <- forward n
      x' <- forward x
      i <- newNameFromString "i"
      case (n', x') of
        ( IndexFn Empty (Cases ((Bool True, m) NE.:| [])),
          IndexFn Empty body
          ) ->
            -- XXX support only 1D arrays for now.
            rewrite $ IndexFn (Forall i (Iota m)) body
        _ -> undefined -- TODO See iota comment.
  -- Scan with basic operator.
  | Just "scan" <- getFun f,
    [E.OpSection (E.QualName [] vn) _ _, _ne, xs'] <- getArgs args = do
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
      x <- newNameFromString "a"
      let y = IndexFn
                iter_xs
                (cases
                  [(base_case, sym2SoP (Var x)), (Not base_case, Recurrence `op` Var x)])
      -- tell ["Using scan rule ", toLaTeX y]
      subst x (IndexFn iter_xs xs) y
        >>= rewrite

forward e = error $ "forward on " <> show e

substParams :: IndexFn -> (E.VName, IndexFn) -> IndexFnM IndexFn
substParams y (paramName, paramIndexFn) =
  subst paramName paramIndexFn y >>= rewrite

cmap :: ((a, b) -> (c, d)) -> Cases a b -> Cases c d
cmap f (Cases xs) = Cases (fmap f xs)

cmapValues :: (b -> c) -> Cases a b -> Cases a c
cmapValues f = cmap (second f)

(~-~) :: Symbol -> Symbol -> SoP Symbol
x ~-~ y = sym2SoP x .-. sym2SoP y

(~+~) :: Symbol -> Symbol -> SoP Symbol
x ~+~ y = sym2SoP x .+. sym2SoP y

(~*~) :: Symbol -> Symbol -> SoP Symbol
x ~*~ y = sym2SoP x .*. sym2SoP y

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
